--[[		sonata/core/test.lua 

--- Unit test system.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2022.

	module 'test'
--]]

--[=[
	Rules for the test writing.

  * Tests should be encapsulated in --[[TEST ... ]], i.e. in multi string comments with signs '!!'.

  * Each block of test code must be separated with at leas 1 empty line.

  * If the test have a result, it must be saved in the variable 'ans'. Write the right value after arrow '-->' to make checking. 
    In the case of float point number use '--n>' to define the number of digital signs n (0-9) for comparison.

  * Test can have no result. In this case '-->' can be omitted.

  * To perform tests call for each module name
    test.module(file1)
    test.module(file2)
    etc.
    
  * Test summary includes information about the number of the passed and failed tests, average time per test unit (in milliseconds), memory size. Call 
    test.summary()
    to show all the results.

  * The results and the error messages are saved to the 'test.log' file. 
]=]

--	LOCAL

local DELIM = '\r?\n[%s%c]*\n'
local LOG_NAME = 'test.log'
local CODE_TEMPLATE = '%-%-%[(=*)%[TEST(.-)%]%1%]'
local TEST_TEMPLATE = '(.*)%-%-(%d?)>(.*)'
local TOL = {['0']=1,['1']=1E1,['2']=1E2,['3']=1E3,['4']=1E4,['5']=1E5,['6']=1E6,['7']=1E7,['8']=1E8,['9']=1E9}

local loadStr = (_VERSION < 'Lua 5.3') and loadstring or load

local help = require('core.help')

--	MODULE

local test = {
-- test results
results = {},
}

--- Extract test code from file.
--  @param str File text.
--  @return Strings with unit tests.
test._getCode_ = function (str)
  local _,q = string.match(str, CODE_TEMPLATE) 
  return q
end

--- Prepare test code preview.
--  @param str Source string.
--  @param res Boolean result of execution.
--  @param time Time of execution, ms.
--  @return String with test results.
test._markTest_ = function (str, res, time)
  local MIN, FULL = 30, 34
  local s = string.match(str, '%C+')
  s = string.match(s, '^(.-)%s*$')
  if #s > MIN then s = string.sub(s, 1, MIN) end
  local rest = string.rep('.', (FULL-#s))
  return string.format('%s%s%s | %.3f |', s, rest, (res and 'Succeed' or 'Failed'), time)
end

--- Save string to the file and simultaneously print to the screen.
--  @param str String for saving.
test._print_ = function (str)
  print(str)
  test.log:write(str,'\n')
end

--- Execute tests listed in module.
--  @param fname Lua file name.
test.module = function (fname)
  -- read file
  local text = assert(help.readAll(fname), "Can't open file '"..fname.."'")
  test.log = test.log or io.open(LOG_NAME, 'w')
  -- write head
  test._print_('\n\tModule: ' .. fname)
  -- get test
  text = test._getCode_(text)
  if not text or #text == 0 then return end  -- no tests
  local succeed, failed = 0, 0
  local fulltime = 0
  -- parse
  for block in test.split(text, DELIM) do
    if string.find(block, '%s') then 
      local q,e,a = string.match(block, TEST_TEMPLATE)
      q = q or block   -- question
      a = a or ''      -- answer
      local arrow, time
      -- evaluate
      local status, err = pcall(function ()
        local fq = loadStr(q)
        -- estimate duration
        time = os.clock(); fq(); time = (os.clock() - time)*1000
        -- check result
        if #a > 0 then
          fq = loadStr('return '..a)
          arrow = fq()
          if e == '' then
            return ans == arrow
          else 
            -- approximately equal
            local tol = TOL[e]
            if tol then 
              return tol*math.abs(ans-arrow) < 1
            else
              io.write('Unexpected symbol ', e, '\n')
              return false
            end
          end 
        else
          return true
        end
      end)
      local res = status and err
      test._print_(test._markTest_(q,res,time)) 
      if not status then
        test.log:write(err,'\n')
      elseif not err then
        test.log:write(tostring(ans),' IS NOT ',tostring(arrow),' !!!\n')
      end
      if not string.find(block, 'require') then -- 'require' takes too mach time
        if res then succeed = succeed + 1 else failed = failed + 1 end
        fulltime = fulltime + time
      end
    end
  end
  test.results[fname] = {succeed, failed, fulltime}
  test.log:flush()
end

--- Combine all results.
test.summary = function ()
  test._print_(string.format('\n%-25s%-10s%-10s%-10s', 'Module', 'Succeed', 'Failed', 'Av.time'))
  for k,v in pairs(test.results) do
    test._print_(string.format('%-27s%-10d%-9d%-10.3f', k, v[1], v[2], v[3]/(v[1]+v[2])))
  end
  print(string.format('Memory in use: %.1f kB', collectgarbage('count')))
end

--- Remove old results
test.clear = function ()
  test.results = {}
end

--- Split string into substrings based on the given delimiter.
--  @param str Initial string.
--  @param delim Delimiter string.
--  @return Iterator over substrings.
test.split = function (str, delim)
  local i,j,k = 1,1,0
  -- return iterator
  return function ()
    if not str or i > #str then 
      return nil 
    end
    j,k = string.find(str, delim, k+1)
    local res
    if j then
      res = string.sub(str, i, j-1)
      i = k+1
    else  -- no more delimiters
      res = string.sub(str, i)
      i = #str+1
    end
    return res
  end
end

--============ Diagnostic methods =================

--- Find function name
--  @param dbg Structure with debug info.
--  @return String with function name.
test._getName_ = function (dbg)
  if dbg.what == 'C' then return dbg.name end
  local lc = string.format("[%s]:%d", dbg.short_src, dbg.linedefined)
  if dbg.what ~= "main" and dbg.namewhat ~= "" then
    return string.format("%s (%s)", lc, dbg.name)
  else
    return lc
  end
end

--- Count internal calls inside function.
--  Based on example from "Programming in Lua" by Roberto Ierusalimschy.
--  @param fn Function to check.
--  @param ... List of arguments.
test.profile = function (fn,...)
  -- prepare storage
  local counters, names = {}, {}
  local function hook()
    local f = debug.getinfo(2, "f").func
    local count = counters[f]
    if count == nil then 
      counters[f] = 1
      names[f] = debug.getinfo(2, "Sn")
    else 
      counters[f] = count+1
    end
  end
  -- run
  debug.sethook(hook, "c")   -- turn on
  fn(...)
  debug.sethook()         -- turn off
  -- process results
  local stat = {}
  for f, c in pairs(counters) do stat[#stat+1] = {test._getName_(names[f]), c} end
  table.sort(stat, function (a,b) return a[2] > b[2] end)
  -- show results
  for _, res in ipairs(stat) do print(res[1], res[2]) end
end

--- Estimate the function execution time.
--  @param fn Function to execute. After it write the list of arguments.
--  @return Average time in msec.
test.time = function (fn,...)
  local n, sum, t = 10, 0, 0
  for i = 1,n do
    t = os.clock(); fn(...); t = os.clock() - t
    sum = sum + t
  end
  return sum * 1000/ n
end

return test

--=========================
