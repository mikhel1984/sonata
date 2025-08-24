--[[		sonata/core/evaluate.lua

--- REPL implementation.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2025.

	module 'evaluate'
--]]


--	LOCAL

-- List of commands
local Cmds = require('core.commands')
-- String evaluation method
local loadStr = loadstring or load
-- Highlight "header" in a "note" file
local NOTE_TEMPL = SonataHelp.CBOLD..'\t%1'..SonataHelp.CNBOLD


-- Format marker
local mt_sonatainfo = {}


--- Check if the variable is integer.
--  @param x Object.
--  @return true if integer.
local _isint = math.tointeger or function (x)
  if type(x) ~= 'number' then return false end
  local v, p = math.modf(x)
  return p == 0.0
end


--- Correct note index.
--  @param n Index value, positive or negative.
--  @param env Environment table.
--  @return Corrected index or nil.
local function _wrapIndex (n, env)
  local lim = #env.notes
  if n < 0 then n = lim + n + 1 end
  if n > 0 and n <= lim then return n end
  return nil
end


--- Set position for the next block in 'note' file.
--  @param args List of arguments.
--  @param env Evaluation parameters.
local function _goTo (args, env)
  local num = tonumber(args[1])
  if not num then
    env.evaluate.printErr("Unknown command "..(args[1] or ""))
    return nil
  end
  env.index = 1
  env.queue = {}
  if #env.notes == 0 then return end
  -- TODO to integer
  local nextInd = _wrapIndex(num, env)
  if not nextInd then return end
  env.index = nextInd
  env.read = false
  table.insert(env.queue, nextInd)
  -- rest
  for p, d in string.gmatch(args[2], "([,:])%s*(%-?%d+)") do
    if not (p and d) then break end
    nextInd = _wrapIndex(tonumber(d), env)
    if not nextInd then break end
    if p == ',' then
      table.insert(env.queue, nextInd)
    else
      for i = env.queue[#env.queue]+1, nextInd, 1 do
        table.insert(env.queue, i)
      end
    end
  end
  table.remove(env.queue, 1)
end


--- Parse string to get command parameters.
--  @param str Input string.
--  @return Table with commands (if any).
local function _getCmd (str)
  if string.find(str, "^%s*:") then
    return { string.match(str, "(%w+)%s*(.*)") }
  elseif string.find(str, "^s*!") then
    return { 'shell', string.match(str, "(%w+.*)") }
  end
  return nil
end


--	MODULE

local evaluate = {

-- current version
MAJOR_V = 1,
MINOR_V = 1,

-- status
EV_RES = 1,   -- found result
EV_CMD = 0,   -- continue expected
EV_ERR = -1,  -- error
EV_ASK = 2,   -- print question
EV_WRN = 3,
EV_INF = 4,
-- string formats
FORMAT_V1 = '#v_1',
FORMAT_V2 = '#v_2',
FORMAT_CLR = '#v_clr',
-- Variants of invite
INV_MAIN = SonataHelp.CMAIN..'## '..SonataHelp.CRESET,
INV_CONT = SonataHelp.CMAIN..'.. '..SonataHelp.CRESET,
INV_NOTE = SonataHelp.CMAIN..'>>> '..SonataHelp.CRESET,
-- common vars
alias = {},
_modules = {},
}


-- Format to representation mapping
local txtCodes = {
  [evaluate.FORMAT_V1] = SonataHelp.CHELP,
  [evaluate.FORMAT_V2] = SonataHelp.CHELP .. SonataHelp.CBOLD,
  [evaluate.FORMAT_CLR] = SonataHelp.CRESET,
}


--- Check if the table is mt_sonatainfo list.
--  @param v Object.
--  @return true if mt_sonatainfo list is found.
evaluate.islist = function (v)
  return type(v) == 'table' and getmetatable(v) == mt_sonatainfo
end


--- Evaluate string of Lua code.
--  The function should work in coroutine.
--  It takes code line and return status and evaluation result.
local function _evalCode()
  local state, cmd = evaluate.EV_RES, ''
  local multiline, res = false, nil
  local _ENV = setmetatable({}, {__index=_G})
  if _VERSION == 'Lua 5.1' then setfenv(1, _ENV) end
  evaluate.set_local_env(_ENV)  -- for 'readline' completion
  evaluate.IN_COROUTINE = true  -- set marker
  while true do
    -- next line of code
    local input = coroutine.yield(state, res)
    cmd = (state == evaluate.EV_CMD) and cmd or ''
    -- check if multi input
    local partCmd, amp = string.match(input, "(.-)(\\*)%s*$")
    if #amp > 1 then
      multiline = true   -- on
    elseif #input == 0 then
      multiline = false  -- off
    end
    -- exec or wait
    if multiline or #amp == 1 then
      cmd = string.format("%s%s\n", cmd, partCmd or input)
      state, res = evaluate.EV_CMD, nil
    else
      cmd = cmd..(partCmd or input)
      -- 'parse'
      local fn, err = loadStr('return '..cmd, nil, 't', _ENV)  -- either 'return expr'
      if err then
        fn, err = loadStr(cmd, nil, 't', _ENV)                 -- or 'expr'
      end
      -- get result
      if err then
        state, res = evaluate.EV_ERR, err
      else
        local ok, ans = pcall(fn)
        state, res = ok and evaluate.EV_RES or evaluate.EV_ERR, nil
        if ans ~= nil and not string.find(cmd, ';%s-$') then
          res = evaluate.islist(ans) and ans or evaluate._extPrint(ans)
          -- save result to global var
          if ok then ANS = ans end
        end
      end
    end
  end
end


--- Show result and choose the next invite string.
--  @param status Status of evaluation.
--  @param res Result of evaluation.
--  @param env List of environment parameters.
--  @return Invite string.
local function _showAndNext(status, res, env)
  if status == evaluate.EV_RES then
    -- finish evaluation
    if res ~= nil then
      local out = evaluate.islist(res) and evaluate._toText(res) or res
      io.write(tostring(out), "\n")
      if env.log then env.log:write('--[[ ', out, ' ]]\n') end
    end
  elseif status == evaluate.EV_CMD then
    -- continue input
    return evaluate.INV_CONT
  elseif status == evaluate.EV_ERR then
    -- error found
    evaluate.printErr(res)
    if env.log then env.log:write('--[[ ERROR ]]\n') end
  elseif status == evaluate.EV_ASK then
    -- system question
    if res[2] then
      io.write(res[2], "\n")
      if env.log then env.log:write('--[[ ', res[2], ' ]]\n') end
    end
    return res[1]
  elseif status == evaluate.EV_WRN then
    -- interpreter warning
    io.write(SonataHelp.CHELP, 'WARNING ', res, '\n')
    env.read, env.info = false, true
  elseif status == evaluate.EV_INF then
    -- print text
    io.write(res, '\n')
    env.read, env.info = false, true
  end
  return evaluate.INV_MAIN
end


--- Evaluate block of text.
--  @param co Coroutine object.
--  @param txt Block of text.
local function _evalBlock (co, env)
  local txt = env.notes[env.index]
  for line in string.gmatch(txt, '([^\n]+)\r?\n?') do
    if string.find(line, '^%s*%-%-') then
      -- highlight line comments
      line = string.gsub(line, '\t(.+)', NOTE_TEMPL)
      line = string.format(
        "%s%s%s\n", SonataHelp.CHELP, line, SonataHelp.CRESET)
      io.write(line)
    else
      -- print line and evaluate
      io.write(evaluate.INV_NOTE, line, '\n')
      repeat
        local _, status, res = coroutine.resume(co, line)
        _showAndNext(status, res, {})
      until status ~= evaluate.EV_INF and status ~= evaluate.EV_WRN
    end
  end
  io.write(SonataHelp.CMAIN,
    '\t[ ', env.index, ' / ', #env.notes, ' ]', SonataHelp.CRESET, '\n')
  if #env.queue > 0 then
    env.index = table.remove(env.queue, 1)
  else
    env.index = env.index+1
  end
end


evaluate._pipeFile = function ()
  local tmp = os.tmpname()
  local n = string.find(string.reverse(tmp), '[/\\]')
  local prefix = n and string.sub(tmp, 1, #tmp-n+1) or ''
  return prefix .. 'sonata-pipe'
end


--- Emulate 'print' behavior
evaluate._simpPrint = function (...)
  local t = {...}
  for i, v in ipairs(t) do t[i] = tostring(v) end
  coroutine.yield(evaluate.EV_INF, table.concat(t, '\t'))
end


--- Extended print function
--  @return string representation
evaluate._extPrint = function (v)
  local mt = getmetatable(v)
  if type(v) == 'table' and not (mt and mt.__tostring) then
    return evaluate._showTable(v)
  else
    return tostring(v)
  end
end


--- Show elements of the table.
--  @param t Table to print.
evaluate._showTable = function (t)
  local N, out = 10, {'{ '}
  -- dialog
  local function continue(n, res)
    local txt = tostring(n) .. ': continue? (y/n) '
    txt = evaluate.ask(txt, res)
    return string.lower(txt) == 'y'
  end
  -- list elements
  for i, v in ipairs(t) do
    out[#out+1] = tostring(v); out[#out+1] = ', '
    if i % N == 0 then
      out[#out+1] = '\n'
      local data = table.concat(out)
      out = {'\n'}
      if not continue(i, data) then
        return '\n}'
      end
    end
  end
  -- hash table elements
  local count, len = 0, #t
  for k, v in pairs(t) do
    if not (_isint(k) and 0 < k and k <= len) then
      local sk = tostring(k)
      if string.sub(sk, 1, 1) ~= '_' then
        out[#out+1] = string.format(
          #out > 1 and '\n  %s = %s,' or '%s = %s,', sk, tostring(v))
        count = count + 1
        if count % N == 0 then
          local data = table.concat(out)
          out = {'\n'}
          if not continue(count, data) then break end
        end
      end
    end
  end
  out[#out+1] = '}'
  return table.concat(out)
end


--- Get string from list of elements.
--  @param lst List of strings and commands.
--  @return Text for visualization.
evaluate._toText = function (lst)
  for i, v in ipairs(lst) do
    lst[i] = txtCodes[v] or v
  end
  return table.concat(lst)
end


--- Allow functions to ask user.
--  @param question Question to user, print as invite string.
--  @param res Current result if any.
--  @return user input.
evaluate.ask = function (question, res)
  return coroutine.yield(evaluate.EV_ASK, {question, res})
end


--- Execute single input line.
--  @param cmd Input expression.
--  @return result of execution or nil.
evaluate.evalLine = function (cmd)
  local fn, err = loadStr('return '..cmd)
  if err then
    fn, err = loadStr(cmd)
  end
  if not err then
    local ok, ans = pcall(fn)
    if ok and ans then
      return evaluate.islist(ans) and ans or evaluate._extPrint(ans)
    end
  end
end


--- Sonata evaluation loop.
--  @param noteList Table with text blocks.
evaluate.repl = function (noteList, reader)
  local invite = evaluate.INV_MAIN
  local co = evaluate.evalThread()
  local env = {notes=noteList or {}, index=1,
    read = true, info=false, queue={}, evaluate=evaluate, co=co}
  -- update print
  if not evaluate.oO then
    evaluate.oO, print = print, evaluate._simpPrint
  end
  while true do
    local input = env.read and reader(invite) or ''
    local cmd = (invite ~= evaluate.INV_CONT) and _getCmd(input)
    if cmd then
      -- execute command
      local fn = Cmds[cmd[1]] or _goTo
      fn(cmd, env)
    elseif #input > 0 or env.info or invite == evaluate.INV_CONT then
      -- evaluate input
      env.read, env.info = true, false
      if env.log then env.log:write(input, '\n') end
      local _, status, res = coroutine.resume(co, input)
      invite = _showAndNext(status, res, env)
    elseif env.index <= #env.notes then
      -- evaluate notes
      _evalBlock(co, env)
      env.read = true
    elseif noteList then
      break
    end
  end
end


--- Get coroutine for evaluation.
--  @return coroutine object.
evaluate.evalThread = function ()
  local co = coroutine.create(_evalCode)
  coroutine.resume(co)
  return co
end


--- Make title string.
--  @return title string.
evaluate.getTitle = function ()
  return string.format(
[[   # #      --=====  so/\/ata  =====--     # #
    # #         --==== %d.%02d ====--        # #
]], evaluate.MAJOR_V, evaluate.MINOR_V)
end


--- Mark information about formatting.
--  @param t Table to print.
--  @return Table with marker.
evaluate.info = function (t)
  return setmetatable(t, mt_sonatainfo)
end


--- Print formatted error message.
--  @param msg Message string.
evaluate.printErr = function (msg)
  io.write(
    string.format("%sERROR %s%s", SonataHelp.CERROR, msg, SonataHelp.CRESET),
    "\n")
end


--- Set environment table reference.
evaluate.set_local_env = function () end


--- Send warning to user.
--  @param txt Text to print.
evaluate.warning = function (txt)
  txt = SonataHelp.CHELP..txt..SonataHelp.CRESET
  if evaluate.IN_COROUTINE then
    coroutine.yield(evaluate.EV_WRN, txt)
  else print(txt) end
end


--- Read file, split into text blocks.
--  Separator is the "pause" word.
--  @param fname File name.
--  @return table with the text blocks.
evaluate.toBlocks = function (fname)
  local f = io.open(fname, 'r')
  if not f then return nil end
  local txt = f:read('*a'); f:close()
  -- remove long comments
  txt = string.gsub(txt, '%-%-%[(=*)%[.-%]%1%]', '')
  local init, res = 1, {}
  while true do
    local i1, i2 = string.find(txt, '%-%-%s-[Pp][Aa][Uu][Ss][Ee].-\n?', init)
    if i1 then
      res[#res+1] = string.sub(txt, init, i1 - 1)
      init = i2 + 1
    else
      break
    end
  end
  local last = string.sub(txt, init, #txt)
  if string.find(last, "%w?") then
    res[#res+1] = last
  end
  return res
end


return evaluate

--=================================
