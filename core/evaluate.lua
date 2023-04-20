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
-- 
INV_MAIN = SonataHelp.CMAIN..'dp: '..SonataHelp.CRESET,
INV_CONT = SonataHelp.CMAIN..'..: '..SonataHelp.CRESET,
INV_NOTE = SonataHelp.CMAIN..'>>> '..SonataHelp.CRESET,

}


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


--- Process command string.
--  @param ev Accumulated string.
--  @param nextCmd New string with Lua expression.
--  @return Status of processing and rest of command.
--evaluate._eval = function (ev, nextCmd)
--  -- reset state
--  if ev._st ~= evaluate.EV_CMD then evaluate.reset(ev) end
--  -- check if multiline
--  local partCmd = string.match(nextCmd, "(.*)\\%s*")
--  if partCmd ~= nil then
--    -- expected next line
--    return evaluate._update(
--      ev, evaluate.EV_CMD, string.format("%s%s\n", ev._cmd, partCmd))
--  end
--  local cmd = ev._cmd..nextCmd
--  -- 'parse'
--  local fn, err = loadStr('return '..cmd)  -- either 'return expr'
--  if err then
--    fn, err = loadStr(cmd)                 -- or 'expr'
--  end
--  -- get result
--  if err then
--    return evaluate._update(ev, evaluate.EV_ERR, cmd, err)
--  else
--    local ok, res = pcall(fn)
--    return evaluate._update(
--      ev, ok and evaluate.EV_RES or evaluate.EV_ERR, cmd, res)
--  end
--end

local function evalCode()
  local state, res = evaluate.EV_RES, nil
  local cmd = ''
  while true do
    -- next line of code
    local input = coroutine.yield(state, res)
    cmd = (state == evaluate.EV_CMD) and cmd or ''
    -- check if multiline
    local partCmd = string.match(input, "(.*)\\%s*")
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


--- Evaluate block of text.
--  @param ev Evaluation environment.
--  @param txt Block of text.
--  @param full True when need to show comments.
--  @param templ Header template string.
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


--- Read-Evaluate-Write circle as a Lua program.
--  Call 'quit' to exit this function.
--  @param ev Evaluation environment.
--evaluate.cli = function (ev)
--  local invA = SonataHelp.CMAIN..'dp: '..SonataHelp.CRESET
--  local invB = SonataHelp.CMAIN..'..: '..SonataHelp.CRESET
--  evaluate.cliLoop(ev, invA, invB)
--  if ev._logFile then ev._logFile:close() end
--  evaluate.exit()
--end

local commands = {

q = function (arg, env)
  evaluate.exit()  
end,

ls = function (arg, env)
  for i = 1, #env.notes do
    local s = ''
    for line in string.gmatch(env.notes[i], '([^\n]+)\r?\n?') do
      if string.find(line, "[^%s]+") then
        s = line
        break
      end
    end
    io.write(string.format("%d   %s\n", i, s))
  end
end,


}

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

local function getCmd (str)
  local res = {}
  if string.find(str, "^%s*:") then
    for w in string.gmatch(str, "%w+") do res[#res+1] = w end
  end
  return res
end

evaluate.cli = function (noteList)
  local invite = evaluate.INV_MAIN
  local env = {notes=noteList or {}, index=1, read = true}
  local co = coroutine.create(evalCode)
  coroutine.resume(co)
  while true do
    local input = ''
    if env.read then
      io.write(invite)
      input = io.read()
    end
    local cmd = getCmd(input)
    if #cmd > 0 then
      -- pocess command
      --for i, v in ipairs(cmd) do print(v) end
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


--- Evaluate 'note'-file.
--  @param ev Evaluate.
--  @param fname Script file name.
--  @param full Flag to work in interactive mode.
--evaluate.note = function (ev, fname, full)
--  full = (full ~= false)
--  local templ = SonataHelp.CBOLD..'\t%1'..SonataHelp.CNBOLD
--  local invA, invB = '?> ', '>> '
--  -- read
--  local f = assert(io.open(fname, 'r'))
--  local txt = f:read('*a'); f:close()
--  txt = string.gsub(txt, '%-%-%[(=*)%[.-%]%1%]', '')  -- remove long comments
--  local block = evaluate._blocks(txt)
--  if full then io.write("Name: '", fname, "'\tBlocks: ", #block, "\n") end
--  local n = evaluate._userInput(ev, invA, invB, block, full, 0)
--  while n <= #block do
--    evaluate._evalBlock(ev, block[n], full, templ)
--    io.write(
--      SonataHelp.CMAIN,
--      '\t[ ', n, ' / ', #block, ' ]', SonataHelp.CRESET, '\n')
--    -- user commands
--    n = evaluate._userInput(ev, invA, invB, block, full, n)
--  end
--end

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


return evaluate

--=================================
--TODO remove invB
