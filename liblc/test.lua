--[[      liblc/test.lua 

--- Unit test system.
--  @author <a href="mailto:vpsys@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

            module 'test'
--]]

--[=[
	Rules for writing test.

   * Tests should be encapsulated in --[[!! ... ]], i.e. in multi string comments with signs '!!'.

   * Each block of test code must be separated with at leas 1 empty line.

   * If test have result, it must be saved in variable 'ans'. To check it write the right value after arrow '-->'. 
     In case of float point number sign '--~' can be used for not strong equality.

   * Test can have no any results. This way print nothing after '-->' (or don't print arrow itself).

   * To perform tests call 
     'lua calc.lua -test [module_name]'
     for example
     'lua calc.lua -test array'
   If module name is not written tests will be performed for all modules.

   * Test summary includes information about number of passed and failed tests, average time per test unit (in milliseconds), memory size.
]=]

local delim = '%c[%s%c]+'    -- empty strings
local logname = 'test.log'
local tol = 0.001

-------------------------------------------- 
-- @class table
-- @name test
-- @field results Results for each module.
-- @field log Log file name.

local test = {}
-- test results
test.results = {}

test.lc_files = require('liblc.files')

--- Extract test code from file.
--    <i>Private function.</i>
--    @param str File text.
--    @return Unit tests.
test.getcode = function (str)
   local p = '%-%-%[(=*)%[!!(.-)%]%1%]'
   local _,q = string.match(str, p) 
   return q
end

--- Prepare test code preview.
--    <i>Private function.</i>
--    @param str Source string.
--    @param res Boolean result of execution.
--    @param time Time of execution, ms.
local function marktest(str, res, time)
   local min, full = 30, 34
   local s = string.match(str, '%C+')
   s = string.match(s, '^(.-)%s*$')
   if #s > min then s = string.sub(s, min) end
   local rest = string.rep('.', (full-#s))
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
   test.log = test.log or io.open(logname, 'w')
   -- write head
   test.print('\n\tModule: ' .. fname)
   -- get test
   text = test.getcode(text)
   if not text or #text == 0 then return end   -- no tests
   local succeed, failed = 0, 0
   local fulltime = 0
   -- parse
   for block in test.lc_files.split(text, delim) do
      if not string.find(block, '%s') then goto endfor end
      local q,e,a = string.match(block,'(.*)%-%-([>~])(.*)')
      q = q or block    -- question
      a = a or ''       -- answer
      local arrow, time
      -- evaluate
      local status, err = pcall(function ()
         local fq = load(q)
	 time = os.clock(); fq(); time = (os.clock() - time)*1000
	 if #a > 0 then
	    local fa = load('return '..a)
	    arrow = fa()
	    return (e == '~') and (math.abs(ans-arrow) <= tol*math.abs(ans)) or (ans == arrow)
	 else
	    return true
	 end
      end)
      local res = status and err
      test.print(marktest(q,res,time)) 
      if not status then
         test.log:write(err,'\n')
      elseif not err then
         test.log:write(tostring(ans),' IS NOT ',tostring(arrow),' !!!\n')
      end
      if string.find(block, 'require') then goto endfor end  -- 'require' takes too mach time
      if res then succeed = succeed + 1 else failed = failed + 1 end
      fulltime = fulltime + time
      ::endfor::
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

