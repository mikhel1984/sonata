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

--	MODULE

local evaluate = {
-- status
EV_RES = 1,   -- found result
EV_CMD = 0,   -- continue expected 
EV_ERR = -1,  -- error
EV_QUIT = -2, -- command for quit
-- log file name
LOGNAME = 'log.note',
-- state and result
_cmd = "",    -- last request
--_ans = "",    -- last response
_st  = 1,     -- last status
}

evaluate.reset = function (ev)
  ev._cmd = ""
  ev._ans = nil
  ev._st  = evaluate.EV_RES
end

evaluate._update = function (ev, st, cmd, ans)
  ev._st = st
  ev._cmd = cmd
  ev._ans = ans
  return st
end

--- Process command string.
--  @param cmd String with Lua expression.
--  @return Status of processing and rest of command.
evaluate.eval = function (ev, nextCmd)
  if nextCmd == 'quit' then 
    return evaluate._update(ev, evaluate.EV_QUIT, "", "")
  end
  -- reset state
  if ev._st ~= evaluate.EV_CMD then evaluate.reset(ev) end
  -- check if multiline
  local partCmd = string.match(nextCmd, "(.*)\\%s*")
  if partCmd ~= nil then
    -- expected next line 
    return evaluate._update(ev, evaluate.EV_CMD, string.format("%s%s\n", ev._cmd, partCmd), "")
  end
  local cmd = ev._cmd..nextCmd
  -- 'parse'
  local fn, err = Ver.loadStr('return '..cmd)  -- either 'return expr'
  if err then
    fn, err = Ver.loadStr(cmd)                 -- or 'expr'
  end
  -- get result
  if err then
    return evaluate._update(ev, evaluate.EV_ERR, cmd, err)
  else
    local ok, res = pcall(fn)
    return evaluate._update(ev, ok and evaluate.EV_RES or evaluate.EV_ERR, cmd, res)
  end
end

--- Read-Evaluate-Write circle as a Lua program.
--  Call 'quit' to exit this function.
evaluate.cli = function (ev)
  local invA, invB = Sonata_help.CMAIN..'dp: '..Sonata_help.CRESET, Sonata_help.CMAIN..'..: '..Sonata_help.CRESET
  local invite = invA
  local ERROR = Sonata_help.CERROR.."ERROR: "
  -- start dialog
  while true do
    io.write(invite)
    -- command processing
    local newLine = io.read()
    local status = evaluate.eval(ev, newLine)
    if status == evaluate.EV_RES then
      if ev._ans ~= nil then print(ev._ans) end
      invite = invA
    elseif status == evaluate.EV_CMD then
      invite = invB
    elseif status == evaluate.EV_ERR then
      print(ERROR, ev._ans, Sonata_help.CRESET)
      invite = invA; cmd = ""
    else -- status == evaluate.EV_QUIT
      break
    end
    -- logging
    if ev._logFile_ then
      ev._logFile_:write(newLine,'\n')
      if status == evaluate.EV_RES and ev._ans then 
        ev._logFile_:write('--[[ ', ev._ans, ' ]]\n\n') 
      elseif status == evaluate.EV_ERR then
        ev._logFile_:write('--[[ ERROR ]]\n\n')
      end
    end
  end
  if ev._logFile_ then ev._logFile_:close() end
  evaluate.exit()
end

--- Evaluate 'note'-file.
--  @param fname Script file name.
evaluate.note = function (ev, fname, full)
  full = (full ~= false)
  local ERROR = Sonata_help.CERROR.."ERROR: "
  local templ = Sonata_help.CBOLD..'\t%1'..Sonata_help.CNBOLD
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
        -- call dialog
        local lquit = false
        local invite = invA
        while true do
          io.write(invite)
          local newCmd = io.read()
          if newCmd == "" then break end  -- continue file evaluation
          local status = evaluate.eval(ev, newCmd)
          if status == evaluate.EV_RES then
            if ev._ans ~= nil then print(ev._ans) end
            invite = invA
          elseif status == evaluate.EV_CMD then
            invite = invB
          elseif status == evaluate.EV_ERR then
            print(ERROR, ev._ans, Sonata_help.CRESET)
            invite = invA
          else --  evaluate.EV_QUIT
            lquit = true
            break
          end
        end
        if lquit then break end
      end
    elseif string.find(line, '^%s*%-%-') then
      -- highlight line comments
      if full then
        line = string.gsub(line, '\t(.+)', templ)
        line = string.format("%s%s%s\n", Sonata_help.CHELP, line, Sonata_help.CRESET)
        io.write(line)
      end
    else
      -- print line and evaluate
      io.write(Sonata_help.CMAIN, '@ ', Sonata_help.CRESET, line, '\n')
      local status = evaluate.eval(ev, line)
      if status == evaluate.EV_RES then
        if ev._ans ~= nil then print(ev._ans) end
      elseif status == evaluate.EV_CMD then
        -- skip
      else -- evaluate.EV_ERR 
        print(ERROR, ev._ans, Sonata_help.CRESET)
        break
      end
    end
  end
end

evaluate.exit = function () 
  print(Sonata_help.CMAIN.."\n             --======= Bye! =======--\n"..Sonata_help.CRESET) 
  os.exit() 
end

return evaluate
