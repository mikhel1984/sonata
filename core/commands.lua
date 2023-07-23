--[[		sonata/core/commands.lua

--- Commands logic.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2023.

	module 'commands'
--]]


--	LOCAL

local Test = nil
local loadStr = loadstring or load
-- File to save logs
local LOGNAME = 'log.note'


--	MODULE

local commands = {}
local cmdInfo = {}


--- Show command description.
--  @param k Command name.
cmdInfo._print = function (k)
  local info = cmdInfo[k]
  if info then
    print(string.format("%s\t%s\t- %s", k, info[2], info[1]))
  else
    print("Unknown key: ", k)
  end
end


--- Show list of commands
commands.help = function (args, env)
  if cmdInfo[args[2]] then
    cmdInfo._print(args[2])
  else
    -- combine
    local group = {}
    for k, _ in pairs(commands) do
      local tp = cmdInfo[k][3]
      if tp then
        group[tp] = group[tp] or {}
        table.insert(group[tp], k)
      else
        cmdInfo._print(k)  -- basic command
      end
    end
    -- rest
    cmdInfo._print('N')
    for t, lst in pairs(group) do
      print(string.format("--\t%s", t))
      for _, v in ipairs(lst) do cmdInfo._print(v) end
    end
  end
end
cmdInfo.help = {"Show this help", ""}


-- Save session to log
commands.log = function (args, env)
  if args[2] == 'on' then
    if not env.log then
      env.log = io.open(LOGNAME, 'a')
      if not env.log then env.evaluate.printErr("Can't open log file") end
      local d = os.date('*t')
      env.log:write(
        string.format('\n--\tSession\n-- %d-%d-%d %d:%d\n\n',
          d.day, d.month, d.year, d.hour, d.min))
    end
  elseif args[2] == 'off' then
    if env.log then
      env.log:close()
      env.log = nil
    end
  else
    env.evaluate.printErr('Unexpected argument!')
  end
end
cmdInfo.log = {"Turn on/off logging", "on/off"}


-- Print list of blocks
commands.ls = function (args, env)
  for i = 1, #env.notes do
    local s = ''
    for line in string.gmatch(env.notes[i], '[^\n\r]+') do
      if string.find(line, "[^%s]+") then s = line; break end
    end
    io.write(string.format("%d   %s\n", i, s))
  end
end
cmdInfo.ls = {"Show list of blocks for execution", "", "Note-files"}


-- Go to line
cmdInfo.N = {"Go to N-th block", ""}


-- Add 'note' file to the list
commands.o = function (args, env)
  local blk = env.evaluate._toBlocks(args[2])
  if blk then
    for _, v in ipairs(blk) do
      table.insert(env.notes, v)
    end
    io.write("Name: '", args[2], "'\tBlocks: ", #blk, "\n")
  else
    env.evaluate.printErr("Can't open file "..args[2])
  end
end
cmdInfo.o = {"Open note-file", "filename", "Note-files"}


-- Clear notes
commands.rm = function (args, env)
  env.notes = {}
  env.queue = {}
  env.index = 1
end
cmdInfo.rm = {"Clear list of notes", "", "Note-files"}


-- Average time
commands.time = function (args, env)
  Test = Test or require('core.test')
  if args[2] then
    local fn, err = loadStr('return '..args[2])
    if fn then
      print(string.format('%.4f ms', Test.time(fn())))
    else
      env.evaluate.printErr(err)
    end
  else
    env.evaluate.printErr("Unexpected argument!")
  end
end
cmdInfo.time = {"Estimate average time", "func", "Debug"}


-- Trace function
commands.trace = function (args, env)
  Test = Test or require('core.test')
  if args[2] then
    local fn, err = loadStr('return '..args[2])
    if fn then
      print(Test.profile(fn()))
    else
      env.evaluate.printErr(err)
    end
  else
    env.evaluate.printErr("Unexpected argument!")
  end
end
cmdInfo.trace = {"Profiling for the function", "func", "Debug"}


-- Quit the program.
commands.q = function (args, env)
  env.evaluate.exit()
end
cmdInfo.q = {"Quit", ""}


return commands

