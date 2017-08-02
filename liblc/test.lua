

local test = {}

local delim = '%c[%s%c]+'    -- empty strings
local logname = 'test.log'
local tol = 0.001

local function getcode(str)
   local p = '%-%-%[(=*)%[!!(.-)%]%1%]'
   local _,q = string.match(str, p) 
   return q
end

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

local function marktest(str, res, time)
   local min, full = 30, 34
   local s = string.match(str, '%C+')
   s = string.match(s, '^(.-)%s*$')
   if #s > min then s = string.sub(s, min) end
   local rest = string.rep('.', (full-#s))
   return string.format('%s%s%s\t%.3f', s, rest, (res and 'Succed' or 'Failed'), time)
end

test.print = function (str)
   print(str)
   test.log:write(str,'\n')
end

test.results = {}

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
	 --first = first or time
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
   test.results[fname] = {succeed, failed, fulltime}
   --local final = string.format("%d tests: %d - succeed, %d - failed", succeed+failed, succeed, failed)
   --test.print(final)
   test.log:flush()
end

test.summary = function ()
   test.print(string.format('\n%-20s%-10s%-10s%-10s%s', 'Module', 'Succeed', 'Failed', 'Av.time', 'Done'))
   for k,v in pairs(test.results) do
      test.print(string.format('%-22s%-10d%-9d%-10.3f%s', k, v[1], v[2], v[3]/(v[1]+v[2]), (v[2]==0 and 'V' or '-')))
   end
end

-- check execution time of function
test.time = function (fn,...)
   local n, sum, t = 10, 0, 0
   for i = 1,n do
      t = os.clock(); fn(...); t = os.clock() - t
      sum = sum + t
   end
   return sum / n
end


return test




