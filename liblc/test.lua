

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

local function marktest(str, res)
   local min = 20
   local s = string.match(str, '%C+')
   s = string.match(s, '^(.-)%s*$')
   if #s > min then s = string.sub(s, min) end
   return string.format('  %s...\t%s', s, (res and 'Succesfull' or 'Failed'))
end

test.print = function (str)
   print(str)
   test.log:write(str,'\n')
end

test.module = function (fname)
   -- read file
   local f = io.open(fname, 'r')
   if not f then print("Can't open file '"..fname.."'"); return end
   local text = f:read('*a')
   f:close()
   test.log = test.log or io.open(logname, 'w')
   -- write head
   test.print('\nModule: ' .. fname)
   -- get test
   text = getcode(text)
   local succesfull, failed = 0, 0
   -- parse
   for block in split(text, delim) do
      local q,e,a = string.match(block,'(.*)%-%-([>~])(.*)')
      q = q or block    -- question
      a = a or ''       -- answer
      local arrow
      -- evaluate
      local status, err = pcall(function ()
         local fq = load(q)()
	 if #a > 0 then
	    local fa = load('return '..a)
	    arrow = fa()
	    return (e == '~') and (math.abs(ans-arrow) <= tol*math.abs(ans)) or (ans == arrow)
	 else
	    return true
	 end
      end)
      local res = status and err
      if res then succesfull = succesfull + 1 else failed = failed + 1 end
      test.print(marktest(q,res)) 
      if not status then
         test.log:write(err,'\n')
      elseif not err then
         test.log:write(tostring(ans),' IS NOT ',tostring(arrow),' !!!\n')
      end
   end
   local final = string.format("%d tests: %d - succesfull, %d - failed", succesfull+failed, succesfull, failed)
   test.print(final)
   test.log:flush()
end

return test




