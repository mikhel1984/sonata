--[[      liblc/test.lua 

--- Unit test system.
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

            module 'test'
--]]

--[=[
	Rules for writing test.

   * Tests should be encapsulated in --[[!! ... ]], i.e. in multi string comments with signs '!!'.

   * Each block of test code must be separated with at leas 1 empty line.

   * If test have result, it must be saved in variable 'ans'. To check it write the right value after arrow '-->'. 
     In case of float point number sign '--~' can be used for not strong equality.

   * Test can have no any results. In this case '-->' can be omitted.

   * To perform tests call 
       'lua calc.lua -test [module_name]'
     For example
       'lua calc.lua -test array'
     If module name is not written tests will be executed for all modules.

   * Test summary includes information about number of passed and failed tests, average time per test unit (in milliseconds), memory size.
     
   * The results are saved to the 'test.log' file. It contains error messages as well (don't displayed on the screen).
]=]

--	LOCAL

local Ver = require "liblc.versions"

local DELIM = '%c[%s%c]+'    -- empty strings
local LOG_NAME = 'test.log'
local TOL = 0.001
local CODE_TEMPLATE = '%-%-%[(=*)%[!!(.-)%]%1%]'
local TEST_TEMPLATE = '(.*)%-%-([>~])(.*)'

--	MODULE

local test = {
-- test results
results = {},
-- files functions
lc_files = require('liblc.files'),
}

--- Extract test code from file.
--    @param str File text.
--    @return Unit tests.
test._getCode = function (str)
   local _,q = string.match(str, CODE_TEMPLATE) 
   return q
end

--- Prepare test code preview.
--    @param str Source string.
--    @param res Boolean result of execution.
--    @param time Time of execution, ms.
test._markTest = function (str, res, time)
   local MIN, FULL = 30, 34
   local s = string.match(str, '%C+')
   s = string.match(s, '^(.-)%s*$')
   if #s > MIN then s = string.sub(s, MIN) end
   local rest = string.rep('.', (FULL-#s))
   return string.format('%s%s%s | %.3f |', s, rest, (res and 'Succeed' or 'Failed'), time)
end

--- Save string to the file and also print to the screen.
--    @param str String for saving.
test.print = function (str)
   print(str)
   test.log:write(str,'\n')
end

--- Try to execute tests listed in module.
--    @param fname Lua file name.
test.module = function (fname)
   -- read file
   local text = assert(test.lc_files.read(fname), "Can't open file '"..fname.."'")
   test.log = test.log or io.open(LOG_NAME, 'w')
   -- write head
   test.print('\n\tModule: ' .. fname)
   -- get test
   text = test._getCode(text)
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
	       return (e == '~') and (math.abs(ans-arrow) <= TOL*math.abs(ans)) or (ans == arrow)
	    else
	       return true
	    end
         end)
         local res = status and err
         test.print(test._markTest(q,res,time)) 
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
   test.print(string.format('\n%-20s%-10s%-10s%-10s%s', 'Module', 'Succeed', 'Failed', 'Av.time', 'Done'))
   for k,v in pairs(test.results) do
      test.print(string.format('%-22s%-10d%-9d%-10.3f%s', k, v[1], v[2], v[3]/(v[1]+v[2]), (v[2]==0 and 'v' or '-')))
   end
   print(string.format('Memory in use: %.1f kB', collectgarbage('count')))
end

--- Check function execution time.
--    @param fn Function to execute. After it write the list of arguments.
--    @return Average time in msec.
test.time = function (fn,...)
   local n, sum, t = 10, 0, 0
   for i = 1,n do
      t = os.clock(); fn(...); t = os.clock() - t
      sum = sum + t
   end
   return sum * 1000/ n
end


return test

