--[[		sonata/core/evaluate.lua

--- REPL implementation.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2023.

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


--- Check if the table is mt_sonatainfo list.
--  @param v Object.
--  @return true if mt_sonatainfo list is found.
local function islist(v)
  return type(v) == 'table' and getmetatable(v) == mt_sonatainfo
end


--- Correct note index.
--  @param n Index value, positive or negative.
--  @param env Environment table.
--  @return Corrected index or nil.
local function wrapIndex (n, env)
  local lim = #env.notes
  if n < 0 then n = lim + n + 1 end
  if n > 0 and n <= lim then return n end
  return nil
end


--- Set position for the next block in 'note' file.
--  @param args List of arguments.
--  @param env Evaluation parameters.
local function goTo (args, env)
  local num = tonumber(args[1])
  if not num then
    env.evaluate.printErr("Unknown command "..args[1])
  end
  env.index = 1
  env.queue = {}
  if #env.notes == 0 then return end
  -- TODO to integer
  local nextInd = wrapIndex(num, env)
  if not nextInd then return end
  env.index = nextInd
  env.read = false
  table.insert(env.queue, nextInd)
  -- rest
  for p, d in string.gmatch(args[2], "([,:])%s*(%-?%d+)") do
    if not (p and d) then break end
    nextInd = wrapIndex(tonumber(d), env)
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
local function getCmd (str)
  if string.find(str, "^%s*:") then
    return { string.match(str, "(%w+)%s*(.*)") }
  end
  return nil
end


-- local function balance(s)
--   local t = {}
--   string.gsub(s, '([(){}])', function (x) t[x] = (t[x] or 0) + 1 end)
--   local d1 = (t['('] or 0) - (t[')'] or 0)
--   local d2 = (t['{'] or 0) - (t['}'] or 0)
--   return (d1 < 0 or d2 < 0) and -1 or (d1 > 0 or d2 > 0) and 1 or 0
-- end


--	MODULE

local evaluate = {

-- current version
version = '0.9.38',

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

--- Evaluate string of Lua code.
--  The function should work in coroutine.
--  It takes code line and return status and evaluation result.
local function evalCode()
  local state, cmd, res = evaluate.EV_RES, '', nil
  local IN_COROUTINE = true  -- set marker
  while true do
    -- next line of code
    local input = coroutine.yield(state, res)
    cmd = (state == evaluate.EV_CMD) and cmd or ''
    -- check if multiline
    local partCmd = string.match(input, "(.*)\\%s*$")
    if partCmd == nil then
      cmd = cmd..input
      -- 'parse'
      local fn, err = loadStr('return '..cmd)  -- either 'return expr'
      if err then
        fn, err = loadStr(cmd)                 -- or 'expr'
      end
      -- get result
      if err then
        state, res = evaluate.EV_ERR, err
      else
        local ok, ans = pcall(fn)
        state, res = ok and evaluate.EV_RES or evaluate.EV_ERR, nil
        if ans ~= nil then
          res = islist(ans) and ans or tostring(ans)
          _ans = ans  -- set global var
        end
      end
    else
      cmd = string.format("%s%s\n", cmd, partCmd)
      state, res = evaluate.EV_CMD, nil
    end
  end
end


--- Show result and choose the next invite string.
--  @param status Status of evaluation.
--  @param res Result of evaluation.
--  @return Invite string.
local function showAndNext(status, res, env)
  if status == evaluate.EV_RES then
    -- finish evaluation
    if res ~= nil then
      local out = islist(res) and evaluate._toText(res) or res
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
    io.write(SonataHelp.CHELP, 'Sonata: ', res, '\n')
    env.read, env.info = false, true
  elseif status == evaluate.EV_INF then
    io.write(res, '\n')
    env.read, env.info = false, true
  end
  return evaluate.INV_MAIN
end


--- Evaluate block of text.
--  @param co Coroutine object.
--  @param txt Block of text.
evaluate._evalBlock = function (co, env)
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
      local _, status, res = coroutine.resume(co, line)
      showAndNext(status, res, {})
      if status == evaluate.EV_ERR then break end
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


--- Redefined print function
evaluate._newPrint = function (...)
  local out = {}
  for i, v in ipairs({...}) do
    local mt = getmetatable(v)
    if type(v) == 'table' and not (mt and mt.__tostring) then
      -- show table
      out[#out+1] = evaluate._showTable(v)
    else
      -- show element
      local s = tostring(v)
      out[#out+1] = s
      out[#out+1] = (string.find(s, '\n.+') ~= false) and '\t' or '\n'
    end
  end
  local res = table.concat(out)
  coroutine.yield(evaluate.EV_INF, res)
end


--- Show elements of the table.
--  @param t Table to print.
evaluate._showTable = function (t)
  local N, nums, out = 10, {}, {'{ '}
  -- dialog
  local function continue(n, res)
    local txt = tostring(n) .. ' continue? (y/n) '
    txt = evaluate.ask(txt, res)
    return string.lower(txt) == 'y'
  end
  -- list elements
  for i, v in ipairs(t) do
    out[#out+1] = tostring(v); out[#out+1] = ', '
    nums[i] = true
    if i % N == 0 then
      out[#out+1] = '\n'
      local data = table.concat(out)
      out = {'\n'}
      if not continue(i, data) then break end
    end
  end
  -- hash table elements
  local count = 0
  for k, v in pairs(t) do
    if not nums[k] then
      out[#out+1] = string.format('\n  %s = %s,', tostring(k), tostring(v))
      count = count + 1
      if count % N == 0 then
        local data = table.concat(out)
        out = {'\n'}
        if not continue(count, data) then break end
      end
    end
  end
  out[#out+1] = '}\n'
  return table.concat(out)
end


--- Read file, split into text blocks.
--  Separator is the "pause" word.
--  @param fname File name.
--  @return table with the text blocks.
evaluate._toBlocks = function (fname)
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


--- Sonata evaluation loop.
--  @param noteList Table with text blocks.
evaluate.repl = function (noteList)
  local invite = evaluate.INV_MAIN
  local env = {notes=noteList or {}, index=1,
    read = true, info=false, queue={}, evaluate=evaluate}
  local co = evaluate.evalThread()
  -- update print
  if not evaluate._oldPrint then
    evaluate._oldPrint, print = print, evaluate._newPrint
  end
  while true do
    local input = ''
    if env.read then
      io.write(invite)
      input = io.read()
    end
    local cmd = getCmd(input)
    if cmd then
      local fn = Cmds[cmd[1]] or goTo
      fn(cmd, env)
    elseif #input > 0 or env.info then
      env.read, env.info = true, false
      if env.log then env.log:write(input, '\n') end
      local _, status, res = coroutine.resume(co, input)
      invite = showAndNext(status, res, env)
    elseif env.index <= #env.notes then
      evaluate._evalBlock(co, env)
      env.read = true
    elseif noteList then
      break
    end
  end
end


--- Get coroutine for evaluation.
--  @return coroutine object.
evaluate.evalThread = function ()
  local co = coroutine.create(evalCode)
  coroutine.resume(co)
  return co
end


--- Show message and exit the program.
evaluate.exit = function ()
  io.write(SonataHelp.CMAIN,
    "\n             --======= Bye! =======--\n", SonataHelp.CRESET, "\n")
  os.exit()
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
    string.format("%sERROR: %s%s", SonataHelp.CERROR, msg, SonataHelp.CRESET),
    "\n")
end


--- Send warning to user.
--  @param txt Text to print.
evaluate.warning = function (txt)
  txt = SonataHelp.CHELP..txt..SonataHelp.CRESET
  if IN_COROUTINE then
    coroutine.yield(evaluate.EV_WRN, txt)
  else print(txt) end
end


return evaluate

--=================================
