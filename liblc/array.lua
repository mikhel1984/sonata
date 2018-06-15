--[[      liblc/array.lua 

--- Manipulations with arrays of elements.
--  Arrays are sparse as long as possible.
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

            module 'array'
--]]

------------------- Tests -------------------
--[[!!
Arr = require 'liblc.array'

-- empty array
a = Arr {2,3,4}                       
ans = a:get{1,2,1}                     --> nil

-- set values
a:set({1,2,1},4)
ans = a:get{1,2,1}                     --> 4

-- random array
b = Arr.rand {5,2,1}
-- number of elements
ans = #b                               --> 10

ans = b:copy()                         --> b

-- compare sizes
ans = b:isequal(Arr.rand{5,2,1})       --> true

-- arithmetical operations
c = b + b
ans = c:get{1,1,1}                     --> 2*b:get{1,1,1}

-- get sub array
g = a:sub({1,1,1},{-1,-1,2})         
ans = g:isequal(Arr{2,3,2})            --> true

-- concatenate along the 3-rd axes
d = Arr.concat(b,b,3)
ans = d:dim()[3]                       --> 2

-- apply functions of 2 arguments
-- to get new array
e = Arr.apply(b,b, function (x,y) return x*y end)
ans = e:get{1,1,1}                     --> (b:get{1,1,1})^2

-- apply function of 1 argument
-- to get new array
f = b:map(function (x) return 10*x end)
ans = f:get{1,1,1}                     --> b:get{1,1,1}*10

-- simple print
print(a)

-- print slices for axis 2 and 3
print(d:fullstring(2,3))
]]

-- 	LOCAL

-- Compatibility with previous versions
local Ver = require "liblc.versions"

-- Check object type.
local function isarray(t) return type(t) == 'table' and t.isarray end

-- Prepare list of coefficients
local function getk (s)
   local k = {1}
   for i = 2, #s do k[i] = s[i-1]*k[i-1] end
   return k
end

-- Info
local help = lc_version and (require "liblc.help") or {new=function () return {} end}

-- 	MODULE

local array = {
type='array', isarray=true,
-- elementary function definition
_add_ = function (x,y) return x+y end,
_sub_ = function (x,y) return x-y end,
_unm_ = function (x) return -x end,
_mul_ = function (x,y) return x*y end,
_div_ = function (x,y) return x/y end,
_pow_ = function (x,y) return x^y end,
}
-- Submatrix
array.__index = array
-- Description
array.about = help:new("Manipulations with arrays of elements.")

-- Create new object, set metatable.
array.new = function (self, s)
   return setmetatable({size=s, k=getk(s)}, self)
end

-- Check index correctness.
array._icorrect = function (arr, ind)
   if #arr.size ~= #ind then return false end
   for i = 1,#ind do 
      if ind[i] > arr.size[i] or ind[i] < 1 then return false end
   end
   return true
end

-- Transform index into single dimension representation.
array._iconvert = function (arr, ind)
   local res, k = ind[1], arr.k
   for i = 2,#ind do 
      res = res + (ind[i]-1) * k[i]
   end
   return res
end

-- Maximum number of elements in array.
array._capacity = function (arr)
   local S,K = arr.size, arr.k
   return S[#S] * K[#K]
end

--- Get array element.
--    @param arr Array object.
--    @param ind Index of the element (table).
--    @return Element or error if index is wrong.
array.get = function (arr, ind)
   assert(array._icorrect(arr, ind), "Wrong index!")
   return arr[array._iconvert(arr, ind)]
end
array.about[array.get] = {"get(arr,ind)", "Get array element. Index is a table.", }

--- Set new value.
--    @param arr Array object.
--    @param ind Element index (table).
--    @param val New value.
array.set = function (arr, ind, val)
   assert(array._icorrect(arr, ind), "Wrong index!")
   arr[array._iconvert(arr,ind)] = val 
end
array.about[array.set] = {"set(arr,ind,val)", "Set value to the array. Index is a table.", }

--- Get array copy.
--    @param arr Array object.
--    @return Deep copy of the array.
array.copy = function (arr)
   local cp = array:new(Ver.move(arr.size, 1, #arr.size, 1, {}))          -- copy size, create new array
   return Ver.move(arr, 1, array._capacity(arr), 1, cp)                   -- copy array elements
end
array.about[array.copy] = {"copy(arr)", "Get copy of the array.", help.OTHER}

--- Compare array size.
--    @param a1 First array object.
--    @param a2 Second array object.
--    @return <code>true</code> if size is the same.
array.isequal = function (a1, a2)
   if isarray(a1) and isarray(a2) and #a1.size == #a2.size then
      for i = 1, #a1.size do
         if a1.size[i] ~= a2.size[i] then return false end
      end
      return true
   end
   return false
end
array.about[array.isequal] = {"isequal(a1,a2)", "Check size equality.", help.OTHER}

--- Apply function of 2 arguments.
--    @param a1 First array object.
--    @param a2 Second array object.
--    @param fn Function with 2 arguments.
--    @return New array where each element is result of the function evaluation fn(a1[i],a2[i]).
array.apply = function (a1, a2, fn)
   assert(array.isequal(a1,a2), "Not compatible arrays!")
   local res, v = array:new(Ver.move(a1.size, 1, #a1.size, 1, {}))    -- prepare empty copy
   for i = 1, array._capacity(a1) do 
      v = fn(a1[i] or 0, a2[i] or 0)             -- elements can be empty
      if v ~= 0 then res[i] = v end              -- save nonzero values
   end
   return res
end
array.about[array.apply] = {"apply(a1,a2,fn)", "Apply function of 2 arguments. Return new array.", help.OTHER}

--- Apply function of 1 argument.
--    @param a Array object.
--    @param fn Function with 1 argument.
--    @return New array where each element is result of the function evaluation fn(a[i]).
array.map = function (a, fn)
   local res, v = array:new(Ver.move(a.size, 1, #a.size, 1, {}))
   for i = 1, array._capacity(a) do
      v = fn(a[i] or 0)                          -- elements can be empty
      if v ~= 0 then res[i] = v end              -- save nonzero values
   end
   return res
end
array.about[array.map] = {"map(a,fn)", "Apply function of 1 argument. Return new array.", help.OTHER}

-- a1 + a2
array.__add = function (a1, a2)
   return array.apply(a1, a2, array._add_)
end

-- a1 - a2
array.__sub = function (a1, a2)
   return array.apply(a1, a2, array._sub_)
end

-- -a
array.__unm = function (a)
   return array.map(a, array._unm_)
end

-- a1 * a2
array.__mul = function (a1, a2)
   return array.apply(a1, a2, array._mul_)
end

-- a1 / a2
array.__div = function (a1, a2)
   return array.apply(a1, a2, array._div_)
end

-- a1 ^ a2
array.__pow = function (a1, a2)
   return array.apply(a1, a2, array._pow_)
end

array.arithmetic = 'arithmetic'
array.about[array.arithmetic] = {array.arithmetic, "a+b, a-b, a*b, a/b, -a, a^b", }

--- Random array generator.
--    @param s Size table.
--    @return Array of the given size with random numbers from 0 to 1.
array.rand = function (s)
   local arr = array:new(s)
   for i = 1, array._capacity(arr) do arr[i] = math.random() end
   return arr
end
array.about[array.rand] = {"rand(size)", "Return array with random numbers between 0 and 1.", }

-- a1 == a2
array.__eq = function (a1, a2)
   if array.isequal(a1,a2) then
      for i = 1, array._capacity(a1) do
	 if (a1[i] or 0) ~= (a2[i] or 0) then return false end
      end
      return true
   end
   return false
end

array.comparison = 'comparison'
array.about[array.comparison] = {array.comparison, "a == b, a ~= b", }

--- Get array dimension.
--    @param arr Array object.
--    @return Table of size.
array.dim = function (arr)
   return Ver.move(arr.size, 1, #arr.size, 1, {})
end
array.about[array.dim] = {"dim(arr)", "Return size of array.", }

--- Get part of array between two indexes.
--    @param arr Array object.
--    @param ind1 Index of the lower bound.
--    @param ind2 Index of the upper bound.
--    @return Array of elements restricted by the indexes.
array.sub = function (arr, ind1, ind2)
   -- negative index means the last element
   for i = 1, #ind2 do 
      if ind2[i] < 0 then ind2[i] = ind2[i] + arr.size[i] + 1 end 
   end
   assert(array._icorrect(arr, ind1) and array._icorrect(arr, ind2), "Wrong index!")
   -- prepare tables
   local newsize, ind = {}, {}
   for i = 1, #ind1 do 
      newsize[i] = ind2[i] - ind1[i] + 1 
      ind[i]     = 0
   end 
   local res = array:new(newsize)
   -- fill
   local K, S = res.k, res.size
   for count = 1, array._capacity(res) do
      -- calculate new temporary index
      for i = 1, #ind do 
         local tmp = math.modf(count,K[i]) % S[i]
	 ind2[i] = ind1[i] + tmp
	 ind[i] = tmp + 1
      end
      res[array._iconvert(res,ind)] = arr[array._iconvert(arr,ind2)]
   end
   return res
end
array.about[array.sub] = {"sub(arr,ind1,ind2)", "Return subarray restricted by 2 indexes.", }

--- Concatenate 2 arrays along given axes.
--    @param arr1 First array object.
--    @param arr2 Second array object.
--    @return New concatenated array.
array.concat = function (arr1, arr2, axe)
   -- check size
   assert(axe > 0 and axe <= #arr1.size, "Wrong axe!")
   for i = 1, #arr1.size do 
     if not (arr1.size[i] == arr2.size[i] or i == axe) then error("Different size!") end
   end
   -- prepare new size
   local newsize = Ver.move(arr1.size, 1, #arr1.size, 1, {})
   newsize[axe] = newsize[axe] + arr2.size[axe]
   -- combine
   local res, ind1, ind2 = array:new(newsize), Ver.move(newsize, 1, #newsize, 1, {}), Ver.move(newsize, 1, #newsize, 1, {})
   local edge = arr1.size[axe]
   local K, S = res.k, res.size
   for count = 1, array._capacity(res) do
      -- prepare index
      for i = 1, #ind1 do
         ind1[i] = math.modf(count, K[i]) % S[i] + 1
	 ind2[i] = ind1[i]
      end
      -- get value
      local second = ind1[axe] > edge
      ind2[axe] = second and (ind2[axe]-edge) or ind2[axe]
      res[array._iconvert(res,ind1)] = second and arr2[array._iconvert(arr2,ind2)] or arr1[array._iconvert(arr1,ind2)]
   end
   return res 
end
array.about[array.concat] = {"concat(a1,a2,axe)", "Array concatenation along given axe.", }

--- Method #
--    @param arr Array object.
--    @return Number of elements.
array.__len = function (arr)
   return array._capacity(arr)
end

-- String representation.
array.__tostring = function (arr)
   return 'Array ' .. table.concat(arr.size, 'x')
end

--- Get array slice.
--    Show sequence of 2D matrices with array elements.
--    @param arr Array object.
--    @param r Number of axes for representation as rows.
--    @param c Number of axes for representation as columns.
--    @return String with all array elements slice by slice.
array.fullstring = function (arr, r, c)
   local res = {}
   local S = arr.size
   -- 1 dimensional array
   if #S == 1 then
      for i = 1, S[1] do res[i] = arr[i] or 0 end
      return table.concat(res, ' ')
   elseif #S == 2 then
      r = r or 1
      c = c or 2
   end
   assert(r > 0 and r <= #S and c > 0 and c <= #S and r ~= c, "Wrong indexes!")
   -- get 2D layer as string
   local function layer (ind)
            local row = {}
	    for i = 1, S[r] do
	       ind[r] = i
	       -- get row
	       local col = {}
	       for j = 1, S[c] do
	          ind[c]  = j
		  col[#col+1] = arr[array._iconvert(arr, ind)] or 0
	       end
	       -- save
	       row[#row+1] = table.concat(col, ' ')
	    end
	    return table.concat(row, '\n')
         end
   -- prepare index calculation
   local bound, current, extent = {}, {}, {}
   local prod = 1
   for i = 1, #S do
      if i ~= r and i ~= c then 
         bound[#bound+1] = S[i] 
	 current[#current+1] = 0 
	 extent[#extent+1] = prod
	 prod = prod * S[i]
      end
   end
   local next_index = Ver.move(S, 1, #S, 1, {})

   for counter = 0, prod-1 do
      -- find coefficients
      for i = #current,1,-1 do current[i] = math.modf(counter,extent[i]) % bound[i] end
      -- write
      local k = #current
      for i = #next_index, 1, -1 do
         if     i == r then next_index[i] = 'R'
	 elseif i == c then next_index[i] = 'C'
	 else
	    next_index[i] = current[k]+1
	    k = k-1
	 end
      end
      --print(next_index[1],next_index[2],next_index[3])
      -- get strings
      res[#res+1] = '{' .. table.concat(next_index,',') .. '}' 
      res[#res+1] = layer(next_index)
      res[#res+1] = ''
   end
   return table.concat(res, '\n') 
end
array.about[array.fullstring] = {"fullstring(arr,r,c)", "Represent array as sequence of matrices, where r and c are numbers of axes.", help.OTHER}

-- Constructor
setmetatable(array, {__call = function (self, v) 
   -- check correctness
   assert(type(v) == 'table', "Table is expected!")
   for i = 1,#v do assert(v[i] > 0 and Ver.isinteger(v[i]), "Positive integer is expected!") end
   -- build
   return array:new(v) 
end})
array.Arr = 'Arr'
array.about[array.Arr] = {"Arr(size)", "Create empty array with given size, represented as a table.", help.NEW}

--- Array serialization.
--    @param obj Array object.
--    @return String, suitable for exchange.
array.serialize = function (obj)
   local s = {}
   s[#s+1] = 'size={' .. table.concat(obj.size, ',') .. '}'
   s[#s+1] = 'k={' .. table.concat(obj.k, ',') .. '}'
   for i = 1, array._capacity(obj) do
      if obj[i] then
         s[#s+1] = string.format("[%d]=%s", i, (type(obj[i]) == 'string' and "'"..obj[i].."'" or obj[i]))
      end
   end
   s[#s+1] = "metatablename='Arr'"
   s[#s+1] = "modulename='array'"
   return string.format("{%s}", table.concat(s, ','))
end
array.about[array.serialize] = {"serialize(obj)", "String representation of array internal structure.", help.OTHER}

--- Iterator across the array.
--    @param arr Array object.
--    @return Index of the next array element and the element itself, <code>nil</code> at the end.
array.next = function (arr)
   local a = array:new(Ver.move(arr.size, 1, #arr.size, 1, {})) -- copy size
   local count, S, K, len = 0, a.size, a.k, array._capacity(a)

   return function ()
             if count == len then return nil, nil end
	     local res = {}
	     for i = 1, #S do res[i] = math.modf(count,K[i]) % S[i]+1 end
	     count = count + 1
	     return res, arr[array._iconvert(arr,res)]
          end
end
array.about[array.next] = {"next(arr)", "Return iterator along all indexes.", help.OTHER}

-- Free memory if need
if not lc_version then array.about = nil end

return array
