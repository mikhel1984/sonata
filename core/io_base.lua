--[[		sonata/core/io_base.lua

--- Basic functions for evaluation loop.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2026.

	module 'io_base'
--]]


--	LOCAL

-- String evaluation method
local _loadStr = loadstring or load


--- Check if the number is integer.
--  @param x Number to check.
--  @return true in case of integer.
local _isint = math.tointeger or function (x)
  if type(x) ~= "number" then return false end
  local v, p = math.modf(x)
  return p == 0.0
end


-- Marker for formatted output.
local mt_sonatainfo = {}


--  MODULE

-- Module functions and variables.
local io_base = {
-- status
EV_RES = 1,   -- found result
EV_CMD = 2,   -- continue input
EV_ASK = 3,   -- print question
EV_WARN = 4,  -- show warning
EV_INFO = 5,  -- show notification
EV_ERR = 6,   -- error
-- string formats
FORMAT_V1 = "#v_1",
FORMAT_V2 = "#v_2",
FORMAT_CLR = "#v_clr",
}


-- Default format lines
local _txtCodes = {
  [io_base.FORMAT_V1] = "",
  [io_base.FORMAT_V2] = "",
  [io_base.FORMAT_CLR] = ""
}


--- Content dependent result demonstration.
--  @param res Result of evaluation.
--  @return string of formatted table.
local function _doShowResult (res)
  local mt = getmetatable(res)
  if mt == mt_sonatainfo then
    return res
  elseif type(res) == "table" and not (mt and mt.__tostring) then
    return io_base.doShowTable(res)
  else
    return tostring(res)
  end
end


--- Print table, ask about continuation if need.
--  @param t Table to show.
--  @return table elements.
io_base.doShowTable = function (t)
  local N, out = 10, {"{ "}
  -- dialog
  local function continue(n, res)
    local txt = tostring(n) .. ": continue? (y/n) "
    txt = io_base.ask(txt, res)
    return string.lower(txt) == "y"
  end
  -- list elements
  for i, v in ipairs(t) do
    out[#out+1] = tostring(v); out[#out+1] = ", "
    if i % N == 0 then
      out[#out+1] = "\n"
      local data = table.concat(out)
      out = {"\n"}
      if not continue(i, data) then
        return "\n}"
      end
    end
  end
  -- dictionary elements
  local count, len = 0, #t
  for k, v in pairs(t) do
    if not (_isint(k) and 0 < k and k <= len) then
      local sk = tostring(k)
      if string.sub(sk, 1, 1) ~= "_" then
        out[#out+1] = string.format(
          #out > 1 and "\n  %s = %s," or "%s = %s,", sk, tostring(v))
        count = count + 1
        if count % N == 0 then
          local data = table.concat(out)
          out = {"\n"}
          if not continue(count, data) then break end
        end
      end
    end
  end
  out[#out+1] = "}"
  return table.concat(out)
end


--- Emulate "print" function with coroutines.
--  @param ... Elements to show.
io_base.doPrint = function (...)
  local t = {...}
  for i, v in ipairs(t) do t[i] = tostring(v) end
  coroutine.yield(io_base.EV_INFO, table.concat(t, "\t"))
end


--- Mark information about formatting.
--  @param t Table to print.
--  @return Table with marker.
io_base.formatted = function (t) return setmetatable(t, mt_sonatainfo) end


--- Check if the table is mt_sonatainfo list.
--  @param v Object.
--  @return true if mt_sonatainfo list is found.
io_base.isformatted = function (v) return getmetatable(v) == mt_sonatainfo end


--- Allow functions to ask question.
--  @param question Question to user, print as invite string.
--  @param res Current result if any.
--  @return user response.
io_base.ask = function (question, res)
  return coroutine.yield(io_base.EV_ASK, {question, res})
end


--- Show warning notification.
--  @param txt Warning message.
io_base.warning = function (txt) coroutine.yield(io_base.EV_WARN, txt) end


--- Show any information.
--  @param txt Text to show.
io_base.info = function (txt) coroutine.yield(io_base.EV_INFO, txt) end


--- Check command input, evaluate expression.
--  @param input Expression to evaluate.
--  @param commands List of available commands.
--  @param state Executor state.
--  @param env Thread environment.
--  @return flag of success and result.
io_base.doCommand = function (input, commands, state, env)
  -- parse input
  local name, expr = nil, nil
  if string.find(input, "^%s*:") then
    name, expr = string.match(input, "(%-?%w+)%s*(.*)")
  end
  -- try to execute
  local fn = commands[name]
  local ok, res = false, nil
  if name and not fn then
    ok, res = false, "Unknown command: "..name
  elseif fn then
    ok, res = fn(expr, state, env)
  else
    ok, res = io_base.doEval(input, env)
  end
  return ok and io_base.EV_RES or io_base.EV_ERR, res
end


--- Compile and evaluate input expression.
--  @param input String to compile.
--  @param env Thread environment.
--  @return flag of success and result.
io_base.doEval = function (input, env)
  -- either 'return expr'
  local fn, err = _loadStr("return "..input, nil, "t", env)
  if err then
    -- or 'expr'
    fn, err = _loadStr(input, nil, "t", env)
  end
  local solve, res = false, err
  -- get result
  if not err then
    solve, res = pcall(fn)
  end
  return solve, res
end


--- Read user input untill full command is obtained.
--  Lines could be separated with \ or \\.
--  @param input Initial input string.
--  @param env Thread environment.
io_base.doRead = function (input, env)
  local partCmd, amp = string.match(input, "(.-)(\\*)%s*$")
  local cmd = {partCmd or input}
  local multiline = #amp > 1
  -- the rest
  while multiline or #amp == 1 do
    input = coroutine.yield(io_base.EV_CMD)
    partCmd, amp = string.match(input, "(.-)(\\*)%s*$")
    if #amp > 1 then
      multiline = true   -- on
    elseif #input == 0 then
      multiline = false  -- off
    end
    cmd[#cmd+1] = partCmd or input
  end
  local s = table.concat(cmd, "\n")
  -- f"..."
  s = string.gsub(s, 'f%b""', io_base.stringFormat)
  return s
end


--- Core of read-eval-print loop.
--  Works inside thread communicates through 'yield'.
--  It takes input string and returns status of evaluation and result.
--  @param commands List of available commands.
--  @param state Executor state.
--  @param env Thread environment.
io_base.doThread = function (commands, state, env)
  local status, res = io_base.EV_RES, nil
  while true do
    -- pre processing
    for _, fn in pairs(state.preProc) do fn(state, env) end
    -- main logic
    local input = coroutine.yield(status, res)
    local cmd = io_base.doRead(input, env)
    state.input = cmd
    status, res = io_base.doCommand(cmd, commands, state, env)
    state.status = status
    -- hide output in the case of ";"
    if res ~= nil and status == io_base.EV_RES and string.find(cmd, ";%s-$") then
      res = nil
    end
    state.output = res
    -- print table
    if res ~= nil and status == io_base.EV_RES then
      env.ANS = res  -- set "global" variable
      res = _doShowResult(res)
    end
    -- post processing
    for _, fn in pairs(state.postProc) do fn(state, env) end
  end
end


--- Create and run evaluation thread, process output status.
--  @param reader Function fn(invite, env) -> user_input.
--  @param writer Function fn(text, status) that sends output to user.
--  @param commands List of available commands.
--  @param invite List of 2 invite strings.
--  @param preprec Function fn(state, env) that runs in the begining of a loop.
--  @param postproc Function fn(state, env) that runs in the end of a loop.
io_base.mainLoop = function (reader, writer, commands, invite, preproc, postproc)
  -- evaluation state
  local state = {
    run=true, input="", output="", status=io_base.EV_RES,
    commands=commands, preProc={main=preproc}, postProc={main=postproc}}
  -- environment
  local _ENV = setmetatable({}, {__index=_G})
  if _VERSION == "Lua 5.1" then setfenv(1, _ENV) end
  -- thread
  local co = coroutine.create(io_base.doThread)
  coroutine.resume(co, commands, state, _ENV)
  -- invite string
  local txt = invite[1]

  while state.run do
    -- get use input when txt is not nil
    local ok, status, res = coroutine.resume(co, txt and reader(txt, _ENV))
    txt = invite[1]
    if not ok then
      -- should not be here
      writer("Program error", io_base.EV_ERR)
      print(status)  -- for debug
      break
    elseif status == io_base.EV_RES then
      -- found solution
      if res ~= nil then
        writer(res)
      end
    elseif status == io_base.EV_CMD then
      -- expected next line
      txt = invite[2]
    elseif status == io_base.EV_ASK then
      -- show question
      if res[2] then
        writer(res[2])
      end
      txt = res[1]
    elseif status == io_base.EV_ERR then
      -- evaluation error
      writer(res, status)
    elseif status == io_base.EV_WARN then
      -- show warning
      writer(res, status)
      txt = nil
    elseif status == io_base.EV_INFO then
      -- show info
      writer(res)
      txt = nil
    end
  end
end


--- Get name of a file that works as a "pipe".
--  @return name string.
io_base.pipeFile = function ()
  local tmp = os.tmpname()
  local n = string.find(string.reverse(tmp), "[/\\]")
  local prefix = n and string.sub(tmp, 1, #tmp-n+1) or ""
  return prefix .. "sonata-pipe"
end


--- Translate evaluation output to string.
--  @param res Found result.
--  @return string representation.
io_base.simpleString = function (res)
  if getmetatable(res) == mt_sonatainfo then
    for i, v in ipairs(res) do res[i] = _txtCodes[v] or v end
    return table.concat(res)
  else
    return tostring(res)
  end
end


--- Process expressions "{}" in string.
--  @param s Source string.
--  @return equivalent "string.format" expression.
io_base.stringFormat = function (s)
  local acc = {}
  s = string.sub(s, 2)  -- remove "f"
  s = string.gsub(s, "%b{}",
    function (x)
      local format, arg
      local a, b = string.match(x, "(.-):(%.%d+f)")
      if a then
        format, arg = "%"..b, string.sub(a, 2)
      else
        format, arg = "%s", string.format("tostring(%s)", string.sub(x, 2, -2))
      end
      acc[#acc+1] = arg
      return format
    end)
  return string.format("string.format(%s, %s)", s, table.concat(acc, ", "))
end


return io_base

--===================================================

