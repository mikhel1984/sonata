--[[		sonata/core/evaluate.lua

--- Evaluate user commands.

--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2021.

	module 'evaluate'
--]]

--	LOCAL

local EV_QUIT, EV_ERROR, EV_CMD, EV_RES = 1, 2, 3, 4


local Ver = {}
if _VERSION < 'Lua 5.3' then
  Ver.loadStr = loadstring
else 
  Ver.loadStr = load
end

--	MODULE

local evaluate = {}

--- Process command string.
--  @param cmd String with Lua expression.
--  @return Status of processing and rest of command.
evaluate.eval = function (cmd, nextCmd)
  if nextCmd == 'quit' then return EV_QUIT end
  -- check if multiline
  local partCmd = string.match(nextCmd, "(.*)\\%s*")
  if partCmd ~= nil then
    -- expected next line 
    return EV_CMD, string.format("%s%s\n", cmd, partCmd)
  end
  cmd = cmd..nextCmd
  -- 'parse'
  local fn, err = Ver.loadStr('return '..cmd)  -- either 'return expr'
  if err then
    fn, err = Ver.loadStr(cmd)                 -- or 'expr'
  end
  -- get result
  if err then
    return EV_ERROR, err
  else
    local ok, res = pcall(fn)
    if ok then 
      _ans = res       -- save last result
       return EV_RES, res
    else
      -- evaluation error
      return EV_ERROR, res
    end
  end
end

--- Read-Evaluate-Write circle as a Lua program.
--  Call 'quit' to exit this function.
evaluate.cli = function (main)
  local invA, invB = Sonata_help.CMAIN..'dp: '..Sonata_help.CRESET, Sonata_help.CMAIN..'..: '..Sonata_help.CRESET
  local invite, cmd = invA, ""
  local ERROR = Sonata_help.CERROR.."ERROR: "
  -- start dialog
  while true do
    io.write(invite)
    -- command processing
    local newLine = io.read()
    local status, res = evaluate.eval(cmd, newLine)
    if status == EV_RES then
      if res ~= nil then print(res) end
      invite = invA; cmd = ""
    elseif status == EV_CMD then
      invite = invB; cmd = res
    elseif status == EV_ERROR then
      print(ERROR, res, Sonata_help.CRESET)
      invite = invA; cmd = ""
    else -- status == EV_QUIT
      break
    end
    -- logging
    if main._logFile_ then
      main._logFile_:write(newLine,'\n')
      if status == EV_RES and res then 
        main._logFile_:write('--[[ ', res, ' ]]\n\n') 
      elseif status == EV_ERROR then
        main._logFile_:write('--[[ ERROR ]]\n\n')
      end
    end
  end
  if main._logFile_ then main._logFile_:close() end
  evaluate.exit()
end

--- Evaluate 'note'-file.
--  @param fname Script file name.
evaluate.note = function (fname, full)
  full = (full ~= false)
  local ERROR = Sonata_help.CERROR.."ERROR: "
  local cmd = ""
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
        local lcmd, lquit = "", false
        local invite = invA
        while true do
          io.write(invite)
          local newCmd = io.read()
          if newCmd == "" then break end  -- continue file evaluation
          local status, res = evaluate.eval(lcmd, newCmd)
          if status == EV_RES then
            if res ~= nil then print(res) end
            invite = invA; lcmd = ""
          elseif status == EV_CMD then
            invite = invB; lcmd = res
          elseif status == EV_ERROR then
            print(ERROR, res, Sonata_help.CRESET)
            invite = invA; lcmd = ""
          else --  EV_QUIT
            lquit = true
            break
          end
        end
        if lquit then break end
      end
    elseif string.find(line, '^%s*%-%-') then
      if full then
        -- highlight line comments
        line = string.gsub(line, '\t(.+)', templ)
        line = string.format("%s%s%s\n", Sonata_help.CHELP, line, Sonata_help.CRESET)
        io.write(line)
      end
    else
      -- print line and evaluate
      io.write(Sonata_help.CMAIN, '@ ', Sonata_help.CRESET, line, '\n')
      local status, res = evaluate.eval(cmd, line)
      if status == EV_RES then
        if res ~= nil then print(res) end
        cmd = ""
      elseif status == EV_CMD then
        cmd = res
      else -- EV_ERROR 
        print(ERROR, res, Sonata_help.CRESET)
        break
      end
    end
  end
end

evaluate.exit = function () print(Sonata_help.CMAIN.."\n             --======= Bye! =======--\n"..Sonata_help.CRESET); os.exit() end

return evaluate
