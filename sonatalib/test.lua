--[[      sonatalib/test.lua 

--- Unit test system.
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonatalib</a> collection, 2017-2019.

            module 'test'
--]]

--[=[
	Rules for writing test.

   * Tests should be encapsulated in --[[TEST ... ]], i.e. in multi string comments with signs '!!'.

   * Each block of test code must be separated with at leas 1 empty line.

   * Each expression in block must occupy only one line.

   * If test have result, it must be saved in variable 'ans'. To check it write the right value after arrow '-->'. 
     In case of float point number use '--n>' to define number number of equal digital signs n (0-9).

   * Test can have no any results. In this case '-->' can be omitted.

   * To perform tests call 
       'lua sonata.lua --test [module_name]' or 'lua sonata.lua -t [module_name]'
     For example
       'lua sonata.lua --test array'
     If module name is not written tests will be executed for all modules.

   * Test summary includes information about number of passed and failed tests, average time per test unit (in milliseconds), memory size.
     
   * The results are saved to the 'test.log' file. It contains error messages as well (don't displayed on the screen).
]=]

--	LOCAL

local Ver = require "sonatalib.versions"

local DELIM = '%c[%s%c]+'    -- empty strings
local LOG_NAME = 'test.log'
local CODE_TEMPLATE = '%-%-%[(=*)%[TEST(.-)%]%1%]'
local TEST_TEMPLATE = '(.*)%-%-(%d?)>(.*)'
local TOL_LST = {['0']=1,['1']=1E1,['2']=1E2,['3']=1E3,['4']=1E4,['5']=1E5,['6']=1E6,['7']=1E7,['8']=1E8,['9']=1E9}

--	MODULE

local test = {
-- test results
results = {},
-- files functions
lc_files = require('sonatalib.files'),
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
   if #s > MIN then s = string.sub(s, MIN) end
   local rest = string.rep('.', (FULL-#s))
   return string.format('%s%s%s | %.3f |', s, rest, (res and 'Succeed' or 'Failed'), time)
end

--- Save string to the file and simultaneously print to the screen.
--  @param str String for saving.
test.print = function (str)
   print(str)
   test.log:write(str,'\n')
end

--- Execute tests listed in module.
--  @param fname Lua file name.
test.module = function (fname)
   -- read file
   local text = assert(test.lc_files.read(fname), "Can't open file '"..fname.."'")
   test.log = test.log or io.open(LOG_NAME, 'w')
   -- write head
   test.print('\n\tModule: ' .. fname)
   -- get test
   text = test._getCode_(text)
   if not text or #text == 0 then return end   -- no tests
   local succeed, failed = 0, 0
   local fulltime = 0
   -- parse
   for block in test.lc_files.split(text, DELIM) do
      if string.find(block, '%s') then 
         local q,e,a = string.match(block, TEST_TEMPLATE)
         q = q or block    -- question
         a = a or ''       -- answer
         local arrow, time
         -- evaluate
         local status, err = pcall(function ()
            local fq = Ver.loadStr(q)
	    time = os.clock(); fq(); time = (os.clock() - time)*1000
	    if #a > 0 then
	       local fa = Ver.loadStr('return '..a)
	       arrow = fa()
               if e == '' then
                  return ans == arrow
               else 
                  -- approximately equal
                  local tol = TOL_LST[e]
                  if tol then 
                     return tol*math.abs(ans-arrow) < 1
                  else
                     print('Unexpected symbol '..e)
                     return false
                  end
               end
	    else
	       return true
	    end
         end)
         local res = status and err
         test.print(test._markTest_(q,res,time)) 
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
   test.print(string.format('\n%-25s%-10s%-10s%-10s%s', 'Module', 'Succeed', 'Failed', 'Av.time', 'Done'))
   for k,v in pairs(test.results) do
      test.print(string.format('%-27s%-10d%-9d%-10.3f%s', k, v[1], v[2], v[3]/(v[1]+v[2]), (v[2]==0 and 'v' or '-')))
   end
   print(string.format('Memory in use: %.1f kB', collectgarbage('count')))
end

--- Check function execution time.
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

--- Count internal calls inside function.
--  Based on example from "Programming in Lua" by Roberto Ierusalimschy.
--  @param fn Function to check.
--  @param ... List of arguments.
test.profile = function (fn,...)
   -- prepare storage
   local counters = {}
   local names = {}
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
   debug.sethook(hook, "c")    -- turn on
   fn(...)
   debug.sethook()             -- turn off

   -- process results
   local stat = {}
   for f, c in pairs(counters) do stat[#stat+1] = {main._getName_(names[f]), c} end
   table.sort(stat, function (a,b) return a[2] > b[2] end)

   -- show results
   for _, res in ipairs(stat) do print(res[1], res[2]) end
end

return test

--=========================
--TODO: Add file with tests for combination of different classes
