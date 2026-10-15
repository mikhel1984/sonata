--[[		sonata/core/commands.lua

--- Commands logic.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2026.

	module 'commands'
--]]


--	LOCAL

local _ext = {
  base = require("core.io_base"),
  -- notes = require("core.io_notes"),
  -- test = require("core.test"),
}

-- aliases
local _loadStr = loadstring or load
local _extBase = _ext.base


--- Call note fragment evaluation.
--  Post processing method.
--  @param state Executor state.
--  @param env Thread environment.
local function _evalNotes (state, env)
  if state.input == "" and state.notes:ready() then
    local block, index = state.notes:getNext()
    local res = state.notes:evalBlock(block, state, env)
    res[#res+1] = string.format("\t[ %d / %d ]", index, state.notes:size())
    coroutine.yield(_extBase.EV_INFO, table.concat(res, "\n"))
  end
end


--- Save text for log file.
--  Post processing method.
--  @param state Executor state.
--  @param env Thread environment.
local function _writeLog (state, env)
  -- input
  state.log:write(state.input, "\n")
  -- result
  if state.status == _extBase.EV_ERR then
    state.log:write("--[[ ERROR ]]\n")
  elseif state.output then
    local txt = _extBase.simpleString(state.output)
    state.log:write("--[[ ", txt, " ]]\n")
  end
end


--- Initialize log writing.
--  Post processing method.
--  @param state Executor state.
--  @param env Thread environment.
local function _initLog (state, env)
  state.postProc.log = _writeLog
  state.preProc.log = nil
end


--  MODULE

-- File to save logs
local LOGNAME = "log.note"
-- commands
local commands = {}
-- Tags
local cmdInfo = {}


--- Make copy of available commands, exclude names from the argument list.
--  @param ... Commands to hide.
--  @return list of commands.
local function _init (...)
  local res = {}
  -- copy initial function list
  for k, v in pairs(commands) do res[k] = v end
  -- remove the given elements
  for _, k in ipairs {...} do res[k] = nil end
  return res
end


--- Prepare command description.
--  @param k Command name.
--  @return description line.
local function _line (k)
  local info, txt = cmdInfo[k], nil
  if info then
    return string.format("%s\t%s\t- %s", k, info[2], About:get(info[1]))
  end
  return "Unknown key: "..k
end


--- Clear global variables in environment
--  @param expr Command agrument.
--  @param state Executor state.
--  @param env Thread environment.
--  @return status and result.
commands.clear = function (expr, state, env)
  -- parse
  local vs = {}
  for s in string.gmatch(expr, "[^, ]+") do vs[#vs+1] = s end
  -- apply
  if vs[1] == "*" then
    for k in pairs(env) do env[k] = nil end
  else
    for _, v in ipairs(vs) do env[v] = nil end
  end
  return _extBase.EV_RES, "Done"
end
cmdInfo.clear = {"cmd_clear", "*|v1,v2.."}


--- Get information about functions.
--  @param expr Command agrument.
--  @param state Executor state.
--  @param env Thread environment.
--  @return status and result.
commands.help = function (expr, state, env)
  local acc = {}
  if cmdInfo[expr] then
    acc[1] = _line(expr)
  else
    -- combine
    local group = {}
    for k, _ in pairs(state.commands) do
      local tp = cmdInfo[k][3]
      if tp then
        group[tp] = group[tp] or {}
        table.insert(group[tp], k)
      else
        acc[#acc+1] = _line(k)  -- basic command
      end
    end
    -- rest
    acc[#acc+1] = _line("N")
    for t, lst in pairs(group) do
      acc[#acc+1] = "--\t"..t
      for _, v in ipairs(lst) do acc[#acc+1] = _line(v) end
    end
  end
  return _extBase.EV_RES, table.concat(acc, "\n")
end
cmdInfo.help = {"cmd_help", "[cmd]"}


--- Change current language.
--  @param expr Command agrument.
--  @param state Executor state.
--  @param env Thread environment.
--  @return status and result.
commands.lang = function (expr, state, env)
  -- get file
  if expr == "en" then
    expr = "nil"
  else
    expr = string.match(expr, "(%S*)")..".lua"
    if not About:localization(expr) then
      return _extBase.EV_ERR, "File not found"
    end
    expr = string.format('"%s"', expr)
  end
  -- update 'help' function
  local cmd = string.format([[
    help = function(v)
      v = v or 'main'
      if v == '*' then
        return About:makeFull(use, %s)
      else
        return About:findObject(v, use, %s) or Sonata.formatted(SonataHelp.objectInfo(v))
      end
    end]],
    expr, expr)
  local fn, err = _loadStr(cmd, nil, "t", env)
  if err then
    return _extBase.EV_ERR, err
  end
  local ok, val = pcall(fn)
  if not ok then
    return _extBase.EV_ERR, val
  end
  return _extBase.EV_RES,  "Load "..(expr == "nil" and "en" or expr)
end
cmdInfo.lang = {"cmd_lang", "name"}


--- Save session to log.
--  @param expr Command agrument.
--  @param state Executor state.
--  @param env Thread environment.
--  @return status and result.
commands.log = function (expr, state, env)
  local txt = nil
  -- start
  if expr == "on" then
    if not state.log then
      state.log = io.open(LOGNAME, "a")
      if not state.log then
        return _extBase.EV_ERR, "Can't open log file"
      end
      local d = os.date("*t")
      state.log:write(
        string.format("\n--\tSession\n-- %d-%d-%d %d:%d\n\n",
          d.day, d.month, d.year, d.hour, d.min))
      txt = "Open "..LOGNAME
      state.preProc.log = _initLog
    end
  -- stop
  elseif expr == "off" then
    if state.log then
      state.log:close()
      state.log = nil
      state.postProc.log = nil
      txt = "Close "..LOGNAME
    end
  else
    return _extBase.EV_ERR, "Expected 'on'/'off', found "..expr
  end
  return _extBase.EV_RES, txt
end
cmdInfo.log = {"cmd_log", "on/off"}


-- Go to line
cmdInfo.N = {"cmd_N", "", "Note-files"}


--- Add 'note' file to the list.
--  @param expr Command agrument.
--  @param state Executor state.
--  @param env Thread environment.
--  @return status and result.
commands.o = function (expr, state, env)
  -- load library
  if not state.notes then
    _ext.notes = _ext.notes or require("core.io_notes")
    local cmds = _init("log", "rm")
    state.notes = _ext.notes.init(cmds)
  end
  -- load file
  local res, n0 = nil, state.notes:size()
  if state.notes:readFile(expr) then
    state.postProc.notes = _evalNotes
    res = string.format("Name: %s\tBlocks %d", expr, state.notes:size() - n0)
  else
    res = "Unable to open file "..expr
  end
  return _extBase.EV_RES, res
end
cmdInfo.o = {"cmd_o", "filename", "Note-files"}


--- Quit the program.
--  @param expr Command agrument.
--  @param state Executor state.
--  @param env Thread environment.
--  @return status and result.
commands.q = function (expr, state, env)
  state.run = false
  return _extBase.EV_RES, nil
end
cmdInfo.q = {"cmd_q", ""}


--- Clear notes.
--  @param expr Command agrument.
--  @param state Executor state.
--  @param env Thread environment.
--  @return status and result.
commands.rm = function (expr, state, env)
  if state.notes then
    state.notes:clear()
  end
  return _extBase.EV_RES, nil
end
cmdInfo.rm = {"cmd_rm", "", "Note-files"}


--- Print list of blocks.
--  @param expr Command agrument.
--  @param state Executor state.
--  @param env Thread environment.
--  @return status and result.
commands.ls = function (expr, state, env)
  local res
  if not state.notes or state.notes:size() == 0 then
    res = "No notes"
  else
    res = table.concat(state.notes:heads(), "\n")
  end
  return _extBase.EV_RES, res
end
cmdInfo.ls = {"cmd_ls", "", "Note-files"}


--- Show the given notes.
--  @param expr Command agrument.
--  @param state Executor state.
--  @param env Thread environment.
--  @return status and result.
commands.show = function (expr, state, env)
  return _extBase.EV_RES,
    state.notes and state.notes:text(expr and tonumber(expr))
end
cmdInfo.show = {"cmd_show", "[N]", "Note-files"}


--- Average time of execution.
--  @param expr Command agrument.
--  @param state Executor state.
--  @param env Thread environment.
--  @return status and result.
commands.time = function (expr, state, env)
  _ext.test = _ext.test or require("core.test")
  -- make function
  local fn, err = _loadStr("return "..expr, nil, "t", env)
  if err then
    return _extBase.EV_ERR, err
  end
  -- eval
  local ok, val = pcall(_ext.test.time, fn)
  if not ok then
    return _extBase.EV_ERR, val
  end
  return _extBase.EV_RES, string.format("%.4f ms", val)
end
cmdInfo.time = {"cmd_time", "func", "Debug"}


--- Trace function call.
--  @param expr Command agrument.
--  @param state Executor state.
--  @param env Thread environment.
--  @return status and result.
commands.trace = function (expr, state, env)
  _ext.test = _ext.test or require("core.test")
  -- make function
  local fn, err = _loadStr("return "..expr, nil, "t", env)
  if err then
    return _extBase.EV_ERR, err
  end
  -- eval
  local ok, val = pcall(_ext.test.profile, fn)
  if not ok then
    return _extBase.EV_ERR, val
  end
  return _extBase.EV_RES, tostring(val)
end
cmdInfo.trace = {"cmd_trace", "func", "Debug"}


--- Show list of variables.
--  @param expr Command agrument.
--  @param state Executor state.
--  @param env Thread environment.
--  @return status and result.
commands.var = function (expr, state, env)
  local acc = {}
  for k, v in pairs(env) do
    -- make text
    local tp = type(v)
    if tp == "table" then tp = v.type or tp end
    local val = string.match(tostring(v), "^([^\r\n]*)")
    if #val > 30 then  -- strip
      val = string.sub(val, 1, 30).."..."
    end
    -- show
    acc[#acc+1] = string.format("%s\t<%s>\t%s", tostring(k), tp, val)
  end
  return _extBase.EV_RES, table.concat(acc, "\n")
end
cmdInfo.var = {"cmd_var", ""}


--- Set output to other window (temporary file).
--  @param expr Command agrument.
--  @param state Executor state.
--  @param env Thread environment.
--  @return status and result.
commands.w = function (expr, state, env)
  -- prepare file
  if not state.pipe then
    local fname = _extBase.pipeFile()
    state.pipe = io.open(fname, "a+")
  end
  -- evaluate input
  state.pipe:write(">> ", expr, "\n")
  local co = coroutine.create(_extBase.doEval)
  local ok, status, res = coroutine.resume(co, expr, env)
  -- process answers of a user
  while coroutine.status(co) ~= "dead" do
    if status == _extBase.EV_ASK and res[2] then
      state.pipe:write(_extBase.simpleString(res[2]), "\n")
      state.pipe:flush()
      table.remove(res)
    end
    local answer = coroutine.yield(status, res)
    ok, status, res = coroutine.resume(co, answer)
  end
  -- result
  if status then
    state.pipe:write(_extBase.simpleString(res), "\n")
    status, res = _extBase.EV_RES, nil
  else
    state.pipe:write("<< ERROR\n")
    status = _extBase.EV_ERR
  end
  state.pipe:flush()
  return status, res
end
cmdInfo.w = {"cmd_w", "expr"}


return {
  init = _init,
}

--=========================================

