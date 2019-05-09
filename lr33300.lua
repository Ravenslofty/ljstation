local bit = require("bit")
local ffi = require("ffi")

local interpret = require("lr33300_interpret")

local band, bor = bit.band, bit.bor
local lshift, rshift = bit.lshift, bit.rshift

local lr33300 = {}

local module = {
    rom = nil,
    mem = nil,
    program_counter = nil,
    instructions_executed = nil,
    gpr = nil,
    cp0r = nil,
}

local instance = {__index = module}

local function load_bios(filename)
    local file = io.open(filename)
    local data = file:read("*all")
    file:close()
    local bios = ffi.new("uint8_t[512*1024]", data)
    return bios
end

local function translate_address(address)
    local mask = {
        -- KUSEG
        [0] = 0xFFFFFFFF,
        [1] = 0xFFFFFFFF,
        [2] = 0xFFFFFFFF,
        [3] = 0xFFFFFFFF,
        -- KSEG0
        [4] = 0x7FFFFFFF,
        -- KSEG1
        [5] = 0x1FFFFFFF,
        -- KSEG2
        [6] = 0xFFFFFFFF,
        [7] = 0xFFFFFFFF,
    }

    local region = band(rshift(tonumber(address), 29), 7)
    return band(address, mask[region])
end

local function read(self, address, size)
    assert(tonumber(address), "lr33300:read: address is not a number")
    assert(band(address, 3) == 0, "lr33300:read: address is not 4-byte aligned")

    address = translate_address(address)

    if address >= 0x00000000 and address < 0x00200000 then
        assert(self.mem ~= nil)
        assert(size == 4, "non-word RAM reads not implemented")
        local data = bor(
            self.mem[address],
            lshift(self.mem[address+1], 8),
            lshift(self.mem[address+2], 16),
            lshift(self.mem[address+3], 24))
        return data
    elseif address >= 0x1FC00000 and address < 0x1FC80000 then
        assert(self.rom ~= nil)
        assert(size == 4, "non-word BIOS reads not implemented")
        address = address - 0x1FC00000
        local data = bor(
            self.rom[address],
            lshift(self.rom[address+1], 8),
            lshift(self.rom[address+2], 16),
            lshift(self.rom[address+3], 24))
        return data
    else
        assert(false, "read of size " .. tostring(size) .. " of address 0x" .. bit.tohex(address) .. " is not recognised")
    end       
end

local function advance(self)
    self.instructions_executed = self.instructions_executed + 1
end

local function write(self, address, size, data)
    assert(tonumber(address), "lr33300:write: address is not a number")
    assert(tonumber(data), "lr33300:write: data is not a number")
    assert(band(address, size-1) == 0, "lr33300:write: address is not aligned")

    address = translate_address(address)

    if address == 0x1F801000 then
        assert(data == 0x1F000000, "lr33300:write: expansion 1 base address reassigned")
    elseif address == 0x1F801004 then
        assert(data == 0x1F802000, "lr33300:write: expansion 2 base address reassigned")
    elseif address >= 0x1F801008 and address <= 0x1F801024 then
        print("ignoring write to MEM_CONTROL address", bit.tohex(address))
    elseif address == 0x1F801060 then
        print("ignoring write to RAM_SIZE")
    elseif address >= 0x1F801C00 and address <= 0x1FC01E80 then
        print("ignoring write to SPU")
    elseif address >= 0x1FC00000 and address <= 0x1FC08000 then
        print("ignoring write to BIOS ROM")
    elseif address == bit.tobit(0xFFFE0130) then
        print("ignoring write to CACHE_CONTROL")
    elseif band(self.cp0r[12], lshift(1, 16)) ~= 0 then
        print("ignoring write while caches are isolated")
    elseif address >= 0x00000000 and address <= 0x00200000 then
        address = address - 0x00000000
        self.mem[address] = band(data, 0xFF)
        self.mem[address+1] = band(rshift(data, 8), 0xFF)
        self.mem[address+2] = band(rshift(data, 16), 0xFF)
        self.mem[address+3] = band(rshift(data, 24), 0xFF)
    else
        print("C0_Status", self.cp0r[12])
        print()
        assert(false, 
            "write of size " .. tostring(size) .. " of data 0x" .. bit.tohex(data) .. " to address 0x" .. bit.tohex(address) .. " is not recognised")
    end
end

function decode(self, program_counter)
    assert(self ~= nil)
    assert(tonumber(program_counter))
   
    if self.traces[program_counter] then
        return self.traces[program_counter]
    end

    print("decoding ", bit.tohex(program_counter))

    local keep_decoding = true
    local branch_delay_slot = false
    local s = table.concat({[[
-- x]], bit.tohex(program_counter), [[

local self = ...
local bit = require("bit")
local ffi = require("ffi")
local band, bor = bit.band, bit.bor
local lshift = bit.lshift
return function()
    ]]})
    local decoder = interpret.new(program_counter)

    while keep_decoding do
        local instruction = self:read(decoder.program_counter, 4)
        assert(tonumber(instruction), "lr33300:decode: read returned non-number")
        local ok, decoded = decoder:decode(instruction)
        --print(decoded)
        
        s = s .. decoded

        keep_decoding = ok and not branch_delay_slot
        branch_delay_slot = decoder.branch_target ~= nil
    end

    s = s .. table.concat({
        decoder:writeback(),
        "\nio.write(\"PC: ",
        bit.tohex(program_counter),
        "\\n\")\n",
        decoder:jump_to_next(),
        "end\n"
    })

    local f = io.open("blocks/" .. bit.tohex(program_counter) .. ".lua", "w")
    f:write(s)
    f:close()

    local func = assert(loadstring(s))(self)

    self.traces[program_counter] = func

    return self.traces[program_counter]
end

function lr33300.new(bios_filename)
    assert(type(bios_filename) == "string")

    local t = {
        rom = load_bios(bios_filename),
        mem = ffi.new("uint8_t[2*1024*1024]"),
        program_counter = 0xBFC00000,
        instructions_executed = 0,

        gpr = ffi.new("int32_t[32]"),
        cp0r = ffi.new("int32_t[32]"),

        read = read,
        write = write,
        advance = advance,
        traces = {}
    }

    for i=0,31 do
        t.gpr[i] = 0
        t.cp0r[i] = 0
    end

    local meta = { __index = decode }

    return setmetatable(t, meta)
end

return lr33300
