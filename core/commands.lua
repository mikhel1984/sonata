--[[		sonata/core/commands.lua

--- Commands logic.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2025.

	module 'commands'
--]]


--	LOCAL

local Test = nil
local loadStr = loadstring or load
-- File to save logs
local LOGNAME = 'log.note'


--- Get instruction how to set global variable.
--  @param m Module name.
--  @param k Key name.
--  @param n Global variable name.
--  @return string for evaluation.
local function set_template (m, k, n)
  return string.format(
    "%s = (type(%s.%s) == 'function') and function(...) return %s.%s(%s, ...) end or %s.%s",
     n,          m, k,                                         m, k, m,              m,  k)
end


--	MODULE

local commands = {}
local cmdInfo = {}


--- Show command description.
--  @param k Command name.
cmdInfo._print = function (k)
  local info = cmdInfo[k]
  if info then
    io.write(string.format("%s\t%s\t- %s\n", k, info[2], About:get(info[1])))
  else
    io.write("Unknown key: ", k, "\n")
  end
end


--- Clear global variables in environment
--  @param args List {command, arg_string}
--  @param env Table with environment references.
commands.clear = function (args, env)
  local vs = {}
  for s in string.gmatch(args[2], "[^, ]+") do vs[#vs+1] = s end
  -- prepare
  local input = ""
  if vs[1] == '*' then
    input = 'for k in pairs(_ENV) do _ENV[k] = nil end'
  else
    for i, v in ipairs(vs) do vs[i] = string.format('_ENV["%s"] = nil', v) end
    input = table.concat(vs, '; ')
  end
  -- execute
  coroutine.resume(env.co, input)
end
cmdInfo.clear = {'cmd_clear', "*|v1,v2.."}


--- Show list of commands
--  @param args List {command, arg_string}
--  @param env Table with environment references.
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
      io.write(string.format("--\t%s\n", t))
      for _, v in ipairs(lst) do cmdInfo._print(v) end
    end
  end
end
cmdInfo.help = {'cmd_help', "[cmd]"}


-- Save session to log
--  @param args List {command, arg_string}
--  @param env Table with environment references.
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
cmdInfo.log = {'cmd_log', "on/off"}


-- Print list of blocks
--  @param args List {command, arg_string}
--  @param env Table with environment references.
commands.ls = function (args, env)
  for i = 1, #env.notes do
    local s = ''
    for line in string.gmatch(env.notes[i], '[^\n\r]+') do
      if string.find(line, "[^%s]+") then s = line; break end
    end
    io.write(string.format("%d   %s\n", i, s))
  end
end
cmdInfo.ls = {'cmd_ls', "", "Note-files"}


-- Go to line
cmdInfo.N = {'cmd_N', "", "Note-files"}


-- Add 'note' file to the list
--  @param args List {command, arg_string}
--  @param env Table with environment references.
commands.o = function (args, env)
  local blk = env.evaluate.toBlocks(args[2])
  if blk then
    for _, v in ipairs(blk) do
      table.insert(env.notes, v)
    end
    io.write("Name: '", args[2], "'\tBlocks: ", #blk, "\n")
  else
    env.evaluate.printErr("Can't open file "..args[2])
  end
end
cmdInfo.o = {'cmd_o', "filename", "Note-files"}


-- Quit the program.
--  @param args List {command, arg_string}
--  @param env Table with environment references.
commands.q = function (args, env) os.exit() end 
cmdInfo.q = {'cmd_q', ""}


-- Clear notes
--  @param args List {command, arg_string}
--  @param env Table with environment references.
commands.rm = function (args, env)
  env.notes = {}
  env.queue = {}
  env.index = 1
end
cmdInfo.rm = {'cmd_rm', "", "Note-files"}


--- Set short alias for module methods.
--  @param args List {command, arg_string}
--  @param env Table with environment references.
commands.set = function (args, env)
  local grp = {}
  if #args < 2 then return end
  for s in string.gmatch(args[2], "[^,]+") do
    local g = {}
    for token in string.gmatch(s, "[^ ]+") do g[#g+1] = token end
    grp[#grp+1] = g
  end
  local tbl = table.remove(grp[1], 1)
  local cs = {}
  for i, v in ipairs(grp) do
    if not (#v == 1 or #v == 3 and v[2] == 'as') then
      env.evaluate.printErr('Wrong format!')
      return
    end
    local nm = v[3] or v[1]
    if string.find(nm, '.', 1, true) then
      env.evaluate.printErr('Wrong format!')
      return
    end
    grp[i] = set_template(tbl, v[1], nm)
  end
  local input = table.concat(grp, '; ')
  coroutine.resume(env.co, input)
end
cmdInfo.set = {'cmd_set', "Module f1 [as v1], f2"}


-- TODO remove?
commands.shell = function (args, env)
  local f = assert(io.popen(args[2]))
  local res = f:read('a')
  f:close()
  io.write(res)
end
cmdInfo.shell = {"cmd_shell", "cmd"}


--- Show the given notes.
--  @param args List {command, arg_string}
--  @param env Table with environment references.
commands.show = function (args, env)
  local ind = args[2] and tonumber(args[2]) or env.index
  if ind < 0 then ind = #env.notes + ind end
  if 0 < ind and ind <= #env.notes then
    io.write(env.notes[ind], '\n')
  end
end
cmdInfo.show = {'cmd_show', "[N]", "Note-files"}


--- Average time
--  @param args List {command, arg_string}
--  @param env Table with environment references.
commands.time = function (args, env)
  Test = Test or require('core.test')
  if args[2] then
    local fn, err = loadStr('return '..args[2])
    if fn then
      io.write(string.format('%.4f ms\n', Test.time(fn())))
    else
      env.evaluate.printErr(err)
    end
  else
    env.evaluate.printErr("Unexpected argument!")
  end
end
cmdInfo.time = {'cmd_time', "func", "Debug"}


--- Trace function
--  @param args List {command, arg_string}
--  @param env Table with environment references.
commands.trace = function (args, env)
  Test = Test or require('core.test')
  if args[2] then
    local fn, err = loadStr('return '..args[2])
    if fn then
      io.write(tostring(Test.profile(fn())), "\n")
    else
      env.evaluate.printErr(err)
    end
  else
    env.evaluate.printErr("Unexpected argument!")
  end
end
cmdInfo.trace = {'cmd_trace', "func", "Debug"}


--- Set output to other window (temporary file).
--  @param args List {command, arg_string}
--  @param env Table with environment references.
commands.w = function (args, env)
  -- prepare file
  if not env.pipe then
    local fname = env.evaluate._pipeFile()
    env.pipe = io.open(fname, 'a+')
  end
  -- exec and write
  local _, status, res = coroutine.resume(env.co, args[2] or '')
  if type(res) ~= 'string' then
    env.evaluate.printErr('Expected string or number, got ' .. type(res))
  elseif status == env.evaluate.EV_ERR then
    env.evaluate.printErr(res)
  else
    env.pipe:write(res, '\n\n')
    env.pipe:flush()
  end
end
cmdInfo.w = {"cmd_w", "expr"}


return commands

