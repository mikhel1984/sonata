--[[		sonata/core/evaluate.lua

--- Evaluate user commands.

--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2021.

	module 'evaluate'
--]]

--	LOCAL

local Ver = {}
if _VERSION < 'Lua 5.3' then
  Ver.loadStr = loadstring
else 
  Ver.loadStr = load
end

local _update = function (ev, st, cmd, ans)
  ev._st = st
  ev._cmd = cmd
  ev._ans = (ans ~= nil) and tostring(ans) or nil
  return st
end

local print_err = function (msg)
  print(string.format("%sERROR: %s%s", SonataHelp.CERROR, msg, SonataHelp.CRESET))
end

--	MODULE

local evaluate = {
-- status
EV_RES = 1,   -- found result
EV_CMD = 0,   -- continue expected 
EV_ERR = -1,  -- error
EV_QUIT = -2, -- command for quit
-- information types
INFO_USE_LIST = 101,
-- log file name
LOGNAME = 'log.note',
-- state and result
_cmd = "",    -- last request
_st  = 1,     -- last status
}

evaluate.reset = function (ev)
  ev._cmd = ""
  ev._ans = nil
  ev._st  = evaluate.EV_RES
end

--- Process command string.
--  @param cmd String with Lua expression.
--  @return Status of processing and rest of command.
evaluate._eval_ = function (ev, nextCmd)
  if nextCmd == 'quit' then 
    return _update(ev, evaluate.EV_QUIT, "")
  end
  -- reset state
  if ev._st ~= evaluate.EV_CMD then evaluate.reset(ev) end
  -- check if multiline
  local partCmd = string.match(nextCmd, "(.*)\\%s*")
  if partCmd ~= nil then
    -- expected next line 
    return _update(ev, evaluate.EV_CMD, string.format("%s%s\n", ev._cmd, partCmd))
  end
  local cmd = ev._cmd..nextCmd
  -- 'parse'
  local fn, err = Ver.loadStr('return '..cmd)  -- either 'return expr'
  if err then
    fn, err = Ver.loadStr(cmd)                 -- or 'expr'
  end
  -- get result
  if err then
    return _update(ev, evaluate.EV_ERR, cmd, err)
  else
    local ok, res = pcall(fn)
    return _update(ev, ok and evaluate.EV_RES or evaluate.EV_ERR, cmd, res)
  end
end

evaluate.eval = function (ev, cmd, useLog)
  local res = evaluate._eval_(ev, cmd) 
  -- logging
  if useLog and ev._logFile_ then
    ev._logFile_:write(newLine,'\n')
    if status == evaluate.EV_RES and ev._ans then 
      ev._logFile_:write('--[[ ', ev._ans, ' ]]\n\n') 
    elseif status == evaluate.EV_ERR then
      ev._logFile_:write('--[[ ERROR ]]\n\n')
    end
  end
  return res
end

evaluate.cli_loop = function (ev, invA, invB, isNote) 
  local invite = invA
  -- start dialog
  while true do
    io.write(invite)
    -- command processing
    local newLine = io.read()
    if isNote and newLine == "" then break end
    local status = evaluate.eval(ev, newLine, not isNote)
    if status == evaluate.EV_RES then
      if ev._ans ~= nil then print(ev._ans) end
      invite = invA
    elseif status == evaluate.EV_CMD then
      invite = invB
    elseif status == evaluate.EV_ERR then
      print_err(ev._ans)
      invite = invA
    else -- status == evaluate.EV_QUIT
      return evaluate.EV_QUIT
    end
  end
end

--- Read-Evaluate-Write circle as a Lua program.
--  Call 'quit' to exit this function.
evaluate.cli = function (ev)
  local invA, invB = SonataHelp.CMAIN..'dp: '..SonataHelp.CRESET, SonataHelp.CMAIN..'..: '..SonataHelp.CRESET
  evaluate.cli_loop(ev, invA, invB, false)
  if ev._logFile_ then ev._logFile_:close() end
  evaluate.exit()
end

--- Evaluate 'note'-file.
--  @param fname Script file name.
evaluate.note = function (ev, fname, full)
  full = (full ~= false)
  local templ = SonataHelp.CBOLD..'\t%1'..SonataHelp.CNBOLD
  local invA, invB = '?> ', '>> '
  -- read lines
  if full then io.write("Run file ", fname, "\n") end
  -- read
  local f = assert(io.open(fname, 'r'))
  local txt = f:read('*a'); f:close()
  txt = string.gsub(txt, '%-%-%[(=*)%[.-%]%1%]', '')  -- remove long comments
  for line in string.gmatch(txt, '([^\n]+)\r?\n?') do
    if string.find(line, '^%s*%-%-%s*PAUSE') then
      if full then
        if evaluate.cli_loop(ev, invA, invB, true) == evaluate.EV_QUIT then
          break
        end
      end
    elseif string.find(line, '^%s*%-%-') then
      -- highlight line comments
      if full then
        line = string.gsub(line, '\t(.+)', templ)
        line = string.format("%s%s%s\n", SonataHelp.CHELP, line, SonataHelp.CRESET)
        io.write(line)
      end
    else
      -- print line and evaluate
      io.write(SonataHelp.CMAIN, '@ ', SonataHelp.CRESET, line, '\n')
      local status = evaluate.eval(ev, line, false)
      if status == evaluate.EV_RES then
        if ev._ans ~= nil then print(ev._ans) end
      elseif status == evaluate.EV_CMD then
        -- skip
      else -- evaluate.EV_ERR 
        print_err(ev._ans)
        break
      end
    end
  end
end

evaluate.exit = function () 
  print(SonataHelp.CMAIN.."\n             --======= Bye! =======--\n"..SonataHelp.CRESET) 
  os.exit() 
end

return evaluate
