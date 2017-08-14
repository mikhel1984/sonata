--[[      liblc/test.lua 

--- Unit test system.
--  <i>This file is a part of 
--  <a href="https://github.com/mikhel1984/lc">liblc</a>
--  collection.</i>
--  @copyright 2017, Stanislav Mikhel

            module 'test'
--]]

--[=[
	Rules for writing test.

   * Tests should be encapsulated in --[[!! ... ]], i.e. in multistring comments with signs '!!'.

   * Each block of test code must be separated with at leas 1 empty line.

   * If test have result, it must be saved in variable 'ans'. To check it write the right value after arrow '-->'. 
     In case of float point number sign '--~' can be used for not strong equality.

   * Test can have no any results. This way print nothing after '-->' (or don't print arrow itself).

   * To perform tests call 
     'lua calc.lua -test [module_name]'
     for example
     'lua calc.lua -test array'
   If module name is not written tests will be performed for all modules.

   * Test summary includes information about number of passed and failed tests, average time per test unit (in msec), memory size.
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

--- Extract test code from file.
--    <i>Private function.</i>
--    @param str File text.
--    @return Unit tests.
local function getcode(str)
   local p = '%-%-%[(=*)%[!!(.-)%]%1%]'
   local _,q = string.match(str, p) 
   return q
end

--- Divide string into the list of substrings using delimeter.
--    <i>Private function.</i>
--    @param str Source string.
--    @param delim Delimeter.
--    @return Table of substrings.
local function split(str, delim)
   local i,j,k = 1,1,0
   return function ()
      if not str or i >= #str then return nil end
      local res
      j,k = string.find(str, delim, k+1)
      if j then
         res = string.sub(str, i, j-1)
	 i = k+1
      else
         res = string.sub(str, i)
	 i = #str
      end
      return res
   end
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
   return string.format('%s%s%s | %.3f |', s, rest, (res and 'Succed' or 'Failed'), time)
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
   local f = io.open(fname, 'r')
   if not f then print("Can't open file '"..fname.."'"); return end
   local text = f:read('*a')
   f:close()
   test.log = test.log or io.open(logname, 'w')
   -- write head
   test.print('\n\tModule: ' .. fname)
   -- get test
   text = getcode(text)
   if not text or #text == 0 then return end   -- no tests
   local succeed, failed = 0, 0
   local fulltime, first = 0
   -- parse
   for block in split(text, delim) do
      local q,e,a = string.match(block,'(.*)%-%-([>~])(.*)')
      q = q or block    -- question
      a = a or ''       -- answer
      local arrow, time
      -- evaluate
      local status, err = pcall(function ()
         local fq = load(q)
	 time = os.clock(); fq(); time = (os.clock() - time)*1000
	 first = first or time
	 if #a > 0 then
	    local fa = load('return '..a)
	    arrow = fa()
	    return (e == '~') and (math.abs(ans-arrow) <= tol*math.abs(ans)) or (ans == arrow)
	 else
	    return true
	 end
      end)
      local res = status and err
      if res then succeed = succeed + 1 else failed = failed + 1 end
      test.print(marktest(q,res,time)) 
      if not status then
         test.log:write(err,'\n')
      elseif not err then
         test.log:write(tostring(ans),' IS NOT ',tostring(arrow),' !!!\n')
      end
      fulltime = fulltime + time
   end
   fulltime = fulltime - (first or 0)
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

