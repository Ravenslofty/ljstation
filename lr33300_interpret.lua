-- ljstation - a PlayStation emulator written in Lua.
-- Copyright (C) 2019 Dan Ravensloft.
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

local bit = require("bit")

local band, bor = bit.band, bit.bor
local lshift, rshift, arshift = bit.lshift, bit.rshift, bit.arshift

local lr33300_interpret = {}

local module = {
    program_counter = nil,
    gpr_declared = nil,
    gpr_needs_writeback = nil,
    branch_target = nil,
    conditional_branch = false,
    exception = false,
}

local instance = { __index = module }

local function register_name(reg)
    assert(tonumber(reg))
    assert(reg >= 0 and reg <= 31)

    local name = {
        "zero", "at", "v0", "v1", "a0", "a1", "a2", "a3",
        "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7",
        "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7",
        "t8", "t9", "k0", "k1", "gp", "sp", "fp", "ra"
    }

    return name[reg+1]
end

local function cop0_register_name(reg)
    assert(tonumber(reg))
    assert(reg >= 0 and reg <= 31)

    local name = {
        "c0_0", "c0_1", "c0_2", "c0_bpc", "c0_4", "c0_bda", "c0_jumpdest", "c0_dcic",
        "c0_badvaddr", "c0_bdam", "c0_10", "c0_bpcm", "c0_status", "c0_cause", "c0_epc", "c0_prid",
        "c0_16", "c0_17", "c0_18", "c0_19", "c0_20", "c0_21", "c0_22", "c0_23",
        "c0_24", "c0_25", "c0_26", "c0_27", "c0_28", "c0_29", "c0_30", "c0_31"
    }

    return name[reg+1]
end

local function declare_source(self, register)
    assert(type(register) == "number")

    if self.gpr_declared[register] then
        return ""
    end
    
    self.gpr_declared[register] = true

    if register ~= 0 then
        local name = register_name(register)
        return "local " .. name .. " = self.gpr[" .. register .. "]\n"
    end

    return "local zero = 0\n"
end

local function declare_destination(self, register)
    assert(type(register) == "number")

    if register == 0 then
        return "local _ = "
    end

    local prefix = self.gpr_declared[register] and "" or "local "

    self.gpr_declared[register] = true
    self.gpr_needs_writeback[register] = true

    local name = register_name(register)
    
    return prefix .. name .. " = "
end

local function declare_cop0_source(self, register)
    assert(type(register) == "number")

    if self.cp0r_declared[register] then
        return ""
    end
    
    self.cp0r_declared[register] = true

    local name = cop0_register_name(register)
    return "local " .. name .. " = self.cp0r[" .. register .. "]\n"
end

local function declare_cop0_destination(self, register)
    assert(type(register) == "number")

    -- TODO: detect and ignore writes to read-only registers.

    local prefix = self.cp0r_declared[register] and "" or "local "

    self.cp0r_declared[register] = true
    self.cp0r_needs_writeback[register] = true

    local name = cop0_register_name(register)
    return prefix .. name .. " = "
end

local function construct_immediate(imm3, imm2, imm1)
    return bor(lshift(imm3, 11), lshift(imm2, 6), imm1)
end

local function branch_target_address(self, target3, target2, target1)
    local addr = bor(lshift(target3, 11), lshift(target2, 6), target1)
    addr = arshift(lshift(addr, 16), 14) -- Sign extend from 16 to 18 bits.
    return addr + self.program_counter
end

local function shift_immediate(self, _, _, second_source, destination, shift_amount, function_field)
    -- The 0x01 bit signifies an arithmetic shift, which shifts in the sign-bit instead of zeroes.
    local arithmetic_shift = bit.band(function_field, 0x01) ~= 0
    
    -- The 0x02 bit signifies a right shift, equivalent to a division by two.
    local right_shift = bit.band(function_field, 0x02) ~= 0

    assert(not(arithmetic_shift and not needs_shift_offset), "reserved instruction: arithmetic left shift")

    local operation = table.concat({
        arithmetic_shift and "a" or "",
        right_shift and "r" or "l",
        "shift"
    })

    local op = {
        -- Operands
        declare_source(self, second_source),
        -- Shift
        declare_destination(self, destination),
        operation,
        "(",
        second_source,
        ", ",
        tostring(shift_amount),
        ")\n",
    }

    return true, table.concat(op)
end

local function jump_register(self, _, target, _, link_register, _, function_field)
    -- The 0x01 bit specifies if the return address should be placed in link_register.
    local linked_branch = bit.band(function_field, 0x01) ~= 0

    local s = table.concat({
        declare_source(self, target),
        "-- jr $",
        register_name(target),
        "\n"
    })

    self.branch_target = register_name(target)
    
    if linked_branch then
        return true, table.concat({
            s,
            declare_destination(self, target),
            tostring(self.program_counter + 8),
            "\n"
        })
    else
        return true, s
    end

end

local function shift_variable(self, _, first_source, second_source, destination, _, function_field)
    -- The 0x01 bit signifies an arithmetic shift, which shifts in the sign-bit instead of zeroes.
    local arithmetic_shift = bit.band(function_field, 0x01) ~= 0
    
    -- The 0x02 bit signifies a right shift, equivalent to a division by two.
    local right_shift = bit.band(function_field, 0x02) ~= 0

    local operation = table.concat({
        arithmetic_shift and "a" or "",
        right_shift and "r" or "l",
        "shift"
    })

    local op = {
        -- Operands
        declare_source(self, first_source),
        declare_source(self, second_source),
        declare_destination(self, destination),
        -- Shift
        op_table[function_field],
        "(",
        second_source,
        ", band(",
        first_source,
        ",",
        tostring(mask),
        "))\n",
    }

    return true, table.concat(op)
end

local function addsub_register(self, _, first_source, second_source, destination, _, function_field)
    -- The 0x01 bit signifies whether to perform overflow checks on the result.
    local op_is_signed = band(function_field, 0x01) == 0

    -- The 0x02 bit signifies whether to perform addition or subtraction.
    local op_is_subtraction = band(function_field, 0x02) ~= 0

    local operation = op_is_subtraction and "-" or "+"

    local op = {
        -- Operands
        declare_source(self, first_source),
        declare_source(self, second_source),
        -- Operate
        declare_destination(self, destination),
        register_name(first_source),
        " ",
        operation,
        " ",
        register_name(second_source),
        "\n"
    }

    if op_is_signed then
        -- TODO: Overflow checking.
    end

    return true, table.concat(op)
end

local function bitop_register(self, _, first_source, second_source, destination, _, function_field)
    -- Haven't found a way to break the instruction encoding down; the instruction encoding probably
    -- selects a logical function unit inside the ALU.

    local op_table = {
        [0x24] = "band",     -- AND
        [0x25] = "bor",      -- OR
        [0x26] = "bxor",     -- XOR
        [0x27] = "bnot(bor(" -- NOR
    }

    local end_bracket = {
        [0x24] = "", -- AND
        [0x25] = "", -- OR
        [0x26] = "", -- XOR
        [0x27] = ")" -- NOR
    }

    local op = {
        -- Operands
        declare_source(self, first_source),
        declare_source(self, second_source),
        -- Operate
        declare_destination(self, destination),
        op_table[function_field],
        "(",
        first_source,
        ", ",
        second_source,
        ")",
        end_bracket[function_field],
        "\n"
    }

    return true, table.concat(op)
end

local function set_if_less_than(self, _, first_source, second_source, destination, _, function_field)
    -- The 0x01 bit specifies whether to perform signed or unsigned comparison.
    local comparison_is_unsigned = bit.band(function_field, 0x01) ~= 0

    local compare_type = comparison_is_unsigned and "uint32_t" or "int32_t"

    local op = {
        -- Operands
        declare_source(self, first_source),
        declare_source(self, second_source),
        declare_destination(self, destination), 
        -- Operate
        "(ffi.cast(\"",
        compare_type,
        "\", ",
        register_name(first_source),
        ") < ffi.cast(\"",
        compare_type,
        "\", ",
        register_name(second_source),
        ")) and 1 or 0\n",
    }

    return true, table.concat(op)
end

local special_table = {
    [0] = shift_immediate,      -- SLL
    [2] = shift_immediate,      -- SRL
    [3] = shift_immediate,      -- SRA
    [8] = jump_register,        -- JR
    [33] = addsub_register,     -- ADDU
    [36] = bitop_register,      -- AND
    [37] = bitop_register,      -- OR
    [38] = bitop_register,      -- XOR
    [39] = bitop_register,      -- NOR
    [42] = set_if_less_than,    -- SLT
    [43] = set_if_less_than,    -- SLTU
}

local function jump(self, op, imm5, imm4, imm3, imm2, imm1)
    -- The 0x01 bit encodes whether this is a linked branch.
    local linked_branch = band(op, 0x01) ~= 0

    local imm = bor(imm1, lshift(imm2, 6), lshift(imm3, 11), lshift(imm4, 16), lshift(imm5, 21))
    imm = bor(band(self.program_counter, 0xF0000000), lshift(imm, 2))

    self.branch_target = "0x" .. bit.tohex(imm)

    if linked_branch then
        local s = table.concat({
            declare_destination(self, 31),
            "bit.tobit(0x",
            bit.tohex(self.program_counter + 8),
            ")\n"
        })
        return true, s
    else
        return true, ""
    end
end

local function equality_branch(self, opcode, first_source, second_source, target3, target2, target1)
    -- The 0x01 bit inverts the check. 
    local invert_comparison = band(opcode, 0x01) ~= 0

    local operation = invert_comparison and " ~= " or " == "

    local op = {
        -- Operands
        declare_source(self, first_source),
        declare_source(self, second_source),
        -- Compare
        "local branch_condition = (",
        register_name(first_source),
        operation,
        register_name(second_source),
        ")\n"
    }

    self.branch_target = "0x" .. bit.tohex(branch_target_address(self, target3, target2, target1))
    self.conditional_branch = true

    return true, table.concat(op)
end

local function add_constant(self, opcode, source, destination, imm3, imm2, imm1)
    -- The 0x01 bit signifies whether to perform overflow checks on the result.
    local op_is_signed = band(opcode, 0x01) == 0

    local imm = construct_immediate(imm3, imm2, imm1)

    local op = {
        -- Operands
        declare_source(self, source),
        -- Operate
        declare_destination(self, destination),
        register_name(source),
        " + 0x",
        bit.tohex(imm),
        "\n"
    }

    if op_is_signed then
        -- TODO: Overflow checking.
    end

    return true, table.concat(op)
end

local function bitop_constant(self, opcode, source, destination, imm3, imm2, imm1)
    -- Haven't found a way to break the instruction encoding down; the instruction encoding probably
    -- selects a logical function unit inside the ALU.

    local op_table = {
        [0x0C] = "band",    -- ANDI
        [0x0D] = "bor",     -- ORI
        [0x0E] = "bxor",    -- XORI
    }

    local imm = construct_immediate(imm3, imm2, imm1)

    local op = {
        -- Operands
        declare_source(self, source),
        -- Operate
        declare_destination(self, destination),
        op_table[opcode],
        "(",
        register_name(source),
        ", 0x",
        bit.tohex(imm),
        ")\n"
    }

    return true, table.concat(op)
end

local function load_upper(self, _, _, destination, imm3, imm2, imm1)
    local imm = construct_immediate(imm3, imm2, imm1)
    imm = lshift(imm, 16)

    local op = {
        -- Operands
        declare_destination(self, destination),
        -- Operate
        "0x",
        bit.tohex(imm),
        "\n"
    }

    return true, table.concat(op)
end

local function load_word(self, op, base, destination, imm3, imm2, imm1)
    local imm = construct_immediate(imm3, imm2, imm1)
    imm = arshift(lshift(imm, 16), 16)

    local op = {
        -- Operands
        declare_source(self, base),
        -- Load
        declare_destination(self, destination),
        "self:read(",
        register_name(base),
        " + 0x",
        bit.tohex(imm),
        ", 4)\n"
    }

    return true, table.concat(op)
end

local function store(self, op, base, source, imm3, imm2, imm1)
    local size = (op - 40) + 1

    local imm = construct_immediate(imm3, imm2, imm1)
    imm = arshift(lshift(imm, 16), 16)

    local op = {
        -- Operands
        declare_source(self, base),
        declare_source(self, source),
        -- Store
        self:cop0_writeback(),
        "self:write(",
        register_name(base),
        " + 0x",
        bit.tohex(imm),
        ", ",
        tostring(size),
        ", ",
        register_name(source),
        ")\n"
    }

    return true, table.concat(op)
end

local general_table = {
    [2] = jump,            -- J
    [3] = jump,            -- JAL
    [4] = equality_branch, -- BEQ
    [5] = equality_branch, -- BNE
    [8] = add_constant,    -- ADDI
    [9] = add_constant,    -- ADDIU
    [12] = bitop_constant, -- ANDI
    [13] = bitop_constant, -- ORI
    [14] = bitop_constant, -- XORI
    [15] = load_upper,     -- LUI
    [35] = load_word,      -- LW
    [40] = store,          -- SB
    [41] = store,          -- SH
    [43] = store,          -- SW
}

local function move_to_coprocessor0(self, _, _, source, destination, _, _)
    local op = {
        -- Operands
        declare_source(self, source),
        declare_cop0_destination(self, destination),
        -- Assign
        register_name(source),
        "\n"
    }

    return true, table.concat(op)
end

local cop0_table = {
    [4] = move_to_coprocessor0,     -- MTC0
}

local decode_table = {
    [0] = { 0, 0x3F, special_table },   -- [SPECIAL]
    [2] = { 26, 0x3F, general_table },  -- J
    [3] = { 26, 0x3F, general_table },  -- JAL
    [4] = { 26, 0x3F, general_table },  -- BEQ
    [5] = { 26, 0x3F, general_table },  -- BNE
    [8] = { 26, 0x3F, general_table },  -- ADDI
    [9] = { 26, 0x3F, general_table },  -- ADDIU
    [12] = { 26, 0x3F, general_table }, -- ANDI
    [13] = { 26, 0x3F, general_table }, -- ORI
    [14] = { 26, 0x3F, general_table }, -- XORI
    [15] = { 26, 0x3F, general_table }, -- LUI
    [16] = { 21, 0x1F, cop0_table },    -- [COP0]
    [35] = { 26, 0x3F, general_table }, -- LW
    [40] = { 26, 0x3F, general_table }, -- SB
    [41] = { 26, 0x3F, general_table }, -- SH
    [43] = { 26, 0x3F, general_table }, -- SW
}

function module:decode(instruction)
    assert(tonumber(instruction), "lr33300_interpret:decode: instruction is not a number")
    
    local op = rshift(instruction, 26)
    local rs = band(rshift(instruction, 21), 0x1F)
    local rt = band(rshift(instruction, 16), 0x1F)
    local rd = band(rshift(instruction, 11), 0x1F)
    local sa = band(rshift(instruction, 6), 0x1F)
    local fn = band(instruction, 0x3F)

    if decode_table[op] == nil then
        assert(decode_table[op] ~= nil, 
            "lr33300_interpret:decode: reserved instruction: op = " .. op .. " fn = " .. fn)
    end

    local shift = decode_table[op][1]
    local mask = decode_table[op][2]
    local index = band(rshift(instruction, shift), mask)
    local emitter = decode_table[op][3][index]

    if emitter == nil then
        assert(emitter ~= nil,
            "lr33300_interpret:decode: couldn't find emitter for op " .. op .. ", fn " .. fn)
    end

    self.program_counter = self.program_counter + 4

    local ok, s = true, ""
    if instruction ~= 0 then
        ok, s = emitter(self, op, rs, rt, rd, sa, fn)
    end

    s = s .. "self:advance()\n"
    return ok, s
end

function module:cop0_writeback()
    s = ""
    for i=0,31 do
        if self.cp0r_needs_writeback[i] then
            s = s .. table.concat({
                "self.cp0r[",
                tostring(i),
                "] = ",
                cop0_register_name(i),
                "\n"
            })
            self.cp0r_needs_writeback[i] = false
        end
    end
    return s
end

function module:writeback()
    s = ""
    for i=0,31 do
        if self.gpr_needs_writeback[i] then
            s = s .. table.concat({
                "self.gpr[",
                tostring(i),
                "] = ",
                register_name(i),
                "\n"
            })
            self.gpr_needs_writeback[i] = false
        end
    end
    return s .. self:cop0_writeback()
end

function module:jump_to_next()
    if self.conditional_branch then
        return [[
        if branch_condition then
            return self[]] .. self.branch_target .. [[](self)
        else
            return self[0x]] .. bit.tohex(self.program_counter + 4) .. [[](self)
        end
        ]]
    else
        return "return self[" .. self.branch_target .. "](self)\n"
    end
end

function lr33300_interpret.new(program_counter)
    local t = {
        program_counter = program_counter,
        gpr_declared = {},
        gpr_needs_writeback = {},
        cp0r_declared = {},
        cp0r_needs_writeback = {},
        branch_target = nil,
        conditional_branch = false,
    }

    for i=0,31 do
        t.gpr_declared[i] = false
        t.gpr_needs_writeback[i] = false
        t.cp0r_declared[i] = false
        t.cp0r_needs_writeback[i] = false
    end

    return setmetatable(t, instance)
end

return lr33300_interpret
