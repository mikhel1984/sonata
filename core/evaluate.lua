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
local Test = nil

-- Highlight "header" in a "note" file
local NOTE_TEMPL = SonataHelp.CBOLD..'\t%1'..SonataHelp.CNBOLD
-- File to save logs
local LOGNAME = 'log.note'


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
local function printErr (msg)
  print(
    string.format("%sERROR: %s%s", SonataHelp.CERROR, msg, SonataHelp.CRESET))
end


--- Set position for the next block in 'note' file.
--  @param args List of arguments.
--  @param env Evaluation parameters.
local function goTo (args, env)
  local num = tonumber(args[1])
  if not num then
    printErr("Not a number")
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
  if string.find(str, "^%s*:") then
    local cmd, args = string.match(str, "(%w+)%s*(.*)")
    return {cmd, args}
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
-- status
EV_RES = 1,   -- found result
EV_CMD = 0,   -- continue expected
EV_ERR = -1,  -- error
EV_ASK = 2,   -- print question
EV_WRN = 3,
-- string formats
FORMAT_V1 = '#v_1',
FORMAT_V2 = '#v_2',
FORMAT_CLR = '#v_clr',
-- Variants of invite
INV_MAIN = SonataHelp.CMAIN..'## '..SonataHelp.CRESET,
INV_CONT = SonataHelp.CMAIN..'.. '..SonataHelp.CRESET,
INV_NOTE = SonataHelp.CMAIN..'>>> '..SonataHelp.CRESET,
}


-- Format to representation mapping
local txtCodes = {
  [evaluate.FORMAT_V1] = SonataHelp.CHELP,
  [evaluate.FORMAT_V2] = SonataHelp.CHELP .. SonataHelp.CBOLD,
  [evaluate.FORMAT_CLR] = SonataHelp.CRESET,
}


local cmdInfo = {}
local commands = {

--- Quit the program.
q = function (args, env)
  evaluate.exit()
end,

--- Print list of blocks
ls = function (args, env)
  for i = 1, #env.notes do
    local s = ''
    for line in string.gmatch(env.notes[i], '[^\n\r]+') do
      if string.find(line, "[^%s]+") then s = line; break end
    end
    io.write(string.format("%d   %s\n", i, s))
  end
end,

--- Open 'note' file
o = function (args, env)
  local blk = evaluate._toBlocks(args[2])
  if blk then
    env.notes = blk
    env.index = 1
    io.write("Name: '", args[2], "'\tBlocks: ", #blk, "\n")
  else
    printErr("Can't open file "..args[2])
  end
end,

--- Save session to log
log = function (args, env)
  if args[2] == 'on' then
    if not env.log then
      env.log = io.open(LOGNAME, 'a')
      if not env.log then printErr("Can't open log file") end
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
    printErr('Unexpected argument!')
  end
end,

-- Trace function
trace = function (args, env)
  local a = args[2]
  Test = Test or require('core.test')
  if a then
    local fn, err = loadStr('return '..a)
    if fn then
      print(Test.profile(fn()))
    else
      printErr(err)
    end
  else
    printErr("Unexpected argument!")
  end
end

}  -- commands


--- Show list of commands
commands.help = function (args, env)
  if cmdInfo[args[2]] then
    cmdInfo._print(args[2])
  else
    for k, _ in pairs(commands) do
      cmdInfo._print(k)
    end
    cmdInfo._print('number')
  end
end


-- Command description
cmdInfo.q = {"Quit"}
cmdInfo.ls = {"Show list of blocks for execution"}
cmdInfo.o = {"Open note-file", "filename"}
cmdInfo.log = {"Turn on/off logging", "on/off"}
cmdInfo.help = {"Show this help"}
cmdInfo.number = {"Go to N-th block"}
cmdInfo.trace = {"Profiling for the function", "func"}


--- Show command description.
--  @param k Command name.
cmdInfo._print = function (k) 
  local info = cmdInfo[k]
  if info then
    print(string.format("  %s %s - %s", k, info[2] or '', info[1]))
  else 
    print("Unknown key: ", k)
  end
end


--- Evaluate string of Lua code.
--  The function should work in coroutine.
--  It takes code line and return status and evaluation result.
local function evalCode()
  local state, cmd, res = evaluate.EV_RES, '', nil
  local in_coroutine = true  -- set marker
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
      print(out)
      if env.log then env.log:write('--[[ ', out, ' ]]\n') end
    end
  elseif status == evaluate.EV_CMD then
    -- continue input
    return evaluate.INV_CONT
  elseif status == evaluate.EV_ERR then
    -- error found
    printErr(res)
    if env.log then env.log:write('--[[ ERROR ]]\n') end
  elseif status == evaluate.EV_ASK then
    -- system question
    if res[2] then
      print(res[2])
      if env.log then env.log:write('--[[ ', res[2], ' ]]\n') end
    end
    return res[1]
  elseif status == evaluate.EV_WRN then
    io.write('Sonata: ', res, '\n')
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
  env.index = env.index+1
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
evaluate.cli = function (noteList)
  local invite = evaluate.INV_MAIN
  local env = {notes=noteList or {}, index=1, 
    read = true, info=false}
  local co = evaluate.evalThread()
  while true do
    local input = ''
    if env.read then
      io.write(invite)
      input = io.read()
    end
    local cmd = getCmd(input)
    if cmd then
      local fn = commands[cmd[1]] or goTo
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
  print(SonataHelp.CMAIN..
    "\n             --======= Bye! =======--\n"..SonataHelp.CRESET)
  os.exit()
end


--- Mark information about formatting.
--  @param t Table to print.
--  @return Table with marker.
evaluate.info = function (t)
  return setmetatable(t, mt_sonatainfo)
end


--- Send info/warning to user.
--  @param txt Text to print.
evaluate.say = function (txt)
  if in_coroutine then
    coroutine.yield(evaluate.EV_WRN, txt)
  else print(txt) end
end


return evaluate

--=================================
