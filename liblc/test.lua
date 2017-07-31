

local test = {}

local delim = '%c[%s%c]+'    -- empty strings
local logname = 'test.log'

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

test.module = function (fname)
   -- read file
   local f = io.open(fname, 'r')
   if not f then print("Can't open file '"..fname.."'"); return end
   local text = f:read('*a')
   f:close()
   test.log = test.log or io.open(logname, 'w')
   -- write head
   local head = '\nModule: ' .. fname
   print(head)
   test.log:write(head,'\n')
   -- get test
   text = getcode(text)
   local succesfull, failed = 0, 0
   -- parse
   for block in split(text, delim) do
      local q,a = string.match(block,'(.*)%-%->(.*)')
      q = q or block    -- question
      a = a or ''       -- answer
      local arrow
      -- evaluate
      local status, err = pcall(function ()
         local fq = load(q)()
	 if #a > 0 then
	    local fa = load('return '..a)
	    arrow = fa()
	    return ans == arrow
	 else
	    return true
	 end
      end)
      local res = status and err
      if res then succesfull = succesfull + 1 else failed = failed + 1 end
      local mark = marktest(q,res)
      print(mark)
      test.log:write(mark,'\n')
      if not status then
         test.log:write(err,'\n')
      elseif not err then
         test.log:write(tostring(ans),'\t',tostring(arrow),'\n')
      end
   end
   local final = string.format("%d test done: %d - succesfull, %d - failed", succesfull+failed, succesfull, failed)
   print(final)
   test.log:write(final,'\n')
   test.log:flush()
end

return test




