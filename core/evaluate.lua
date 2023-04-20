--[[		sonata/core/evaluate.lua

--- Evaluate user commands.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2023.

	module 'evaluate'
--]]


--	LOCAL

-- String evaluation method
local loadStr = (_VERSION < 'Lua 5.3') and loadstring or load


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


--- Print formatted error message.
--  @param msg Message string.
local function print_err (msg)
  print(
    string.format("%sERROR: %s%s", SonataHelp.CERROR, msg, SonataHelp.CRESET))
end


--- Set position for the next block in 'note' file.
--  @param arg List of arguments.
--  @param env Evaluation parameters.
local function goTo (arg, env)
  local num = tonumber(arg[1])
  if not num then
    print_err("Not a number")
  end
  -- TODO to integer
  local lim = #env.notes
  if lim > 0 then
    env.index = (num < 0) and 1 or (num > lim) and lim or num
    env.read = false
  else
    env.index = 1
  end
end


--- Parse string to get command parameters.
--  @param str Input string.
--  @return Table with commands (if any).
local function getCmd (str)
  local res = {}
  if string.find(str, "^%s*:") then
    for w in string.gmatch(str, "%w+") do res[#res+1] = w end
  end
  return res
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
-- status
EV_RES = 1,   -- found result
EV_CMD = 0,   -- continue expected
EV_ERR = -1,  -- error
-- string formats
FORMAT_V1 = '#v1',
FORMAT_V2 = '#v2',
-- log file name
LOGNAME = 'log.note',
-- Variants of invite
INV_MAIN = SonataHelp.CMAIN..'dp: '..SonataHelp.CRESET,
INV_CONT = SonataHelp.CMAIN..'..: '..SonataHelp.CRESET,
INV_NOTE = SonataHelp.CMAIN..'>>> '..SonataHelp.CRESET,
}


local commands = {

--- Quit the program.
q = function (arg, env)
  evaluate.exit()  
end,

--- Print list of blocks
ls = function (arg, env)
  for i = 1, #env.notes do
    local s = ''
    for line in string.gmatch(env.notes[i], '[^\n\r]+') do
      if string.find(line, "[^%s]+") then s = line; break end
    end
    io.write(string.format("%d   %s\n", i, s))
  end
end,


}


--- Evaluate string of Lua code.
--  The function should work in coroutine.
--  It takes code line and return status and evaluation result.
local function evalCode()
  local state, cmd, res = evaluate.EV_RES, '', nil
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
local function showAndNext(status, res)
  if status == evaluate.EV_RES then
    if res ~= nil then
      print(islist(res) and evaluate._toText(res) or res)
    end
    _ans = res
  elseif status == evaluate.EV_CMD then
    return evaluate.INV_CONT
  elseif status == evaluate.EV_ERR then
    print_err(res)
  end
  return evaluate.INV_MAIN
end


--- Print next block title.
--  @param blks Table with blocks of text.
--  @param n Next block index.
--evaluate._dotNext = function (blks, n)
--  local s = ''
--  if n > #blks then
--    s = 'quit'
--  else
--    for line in string.gmatch(blks[n], '([^\n]+)\r?\n?') do
--      if string.find(line, "[^%s]+") then
--        s = line
--        break
--      end
--    end
--  end
--  io.write(string.format("%d %s\n", n, s))
--end


--- Evaluate block of text.
--  @param co Coroutine object.
--  @param txt Block of text.
evaluate._evalBlock = function (co, txt)
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
      showAndNext(status, res)
      if status == evaluate.EV_ERR then break end
    end
  end
end


--- Read file, split into text blocks. 
--  Separator is the "pause" word.
--  @param fname File name.
--  @return table with the text blocks.
evaluate._toBlocks = function (fname)
  local f = assert(io.open(fname, 'r'))
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
  local res, i = {}, 1
  while i <= #lst do
    local v = lst[i]
    if v == evaluate.FORMAT_V1 then
      i = i + 1
      res[#res+1] = string.format("%s%s%s", 
        SonataHelp.CHELP, lst[i], SonataHelp.CRESET)
    elseif v == evaluate.FORMAT_V2 then
      i = i + 1
      res[#res+1] = string.format("%s%s%s%s",
        SonataHelp.CHELP, SonataHelp.CBOLD, lst[i], SonataHelp.CRESET)
    else
      res[#res+1] = v
    end
    i = i + 1
  end
  return table.concat(res)
end


--- Sonata evaluation loop.
--  @param noteList Table with text blocks.
evaluate.cli = function (noteList)
  local invite = evaluate.INV_MAIN
  local env = {notes=noteList or {}, index=1, read = true}
  local co = evaluate.evalThread()
  while true do
    local input = ''
    if env.read then
      io.write(invite)
      input = io.read()
    end
    local cmd = getCmd(input)
    if #cmd > 0 then
      local fn = commands[cmd[1]] or goTo
      fn(cmd, env)
    elseif #input > 0 then
      local _, status, res = coroutine.resume(co, input)
      invite = showAndNext(status, res)
    elseif env.index <= #env.notes then
      evaluate._evalBlock(co, env.notes[env.index])
      env.index = env.index + 1
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
  print(SonataHelp.CMAIN..
    "\n             --======= Bye! =======--\n"..SonataHelp.CRESET)
  os.exit()
end


--- Evaluate one expression.
--  @param ev Environment.
--  @param cmd Command.
--  @param useLog Flag to write log.
--  @return Result of evaluation.
--evaluate.eval = function (ev, cmd, useLog)
--  local res = evaluate._eval(ev, cmd)
--  -- logging
--  if useLog and ev._logFile then
--    ev._logFile:write(cmd, '\n')
--    -- TODO: Update it
--    -- if status == evaluate.EV_RES and ev._ans then
--    --   ev._logFile:write('--[[ ', ev._ans, ' ]]\n\n')
--    -- elseif status == evaluate.EV_ERR then
--    --   ev._logFile:write('--[[ ERROR ]]\n\n')
--    -- end
--  end
--  return res
--end


--- Mark information about formatting.
--  @param t Table to print.
--  @return Table with marker.
evaluate.info = function (t)
  return setmetatable(t, mt_sonatainfo)
end


return evaluate

--=================================
--TODO fix logs
--TODO open/add/clear notes
