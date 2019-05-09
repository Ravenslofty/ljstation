local lr33300 = require("lr33300")

DEBUG = true

local cpu = lr33300.new("SCPH1001.BIN")
cpu[0xBFC00000]()
local status, err = pcall(cpu[0xBFC00000])
print(err)
print(cpu.instructions_executed, "instructions executed")
