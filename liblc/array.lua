------------  array.lua ----------------
--
-- Manipulations with arrays of elements.
--
-- This file is a part of liblc collection. 
-- Stanislav Mikhel, 2017.
----------------------------------------

-------------------- Tests -------------------
--[[!!
Arr = require 'liblc.array'
-- empty array
a = Arr {2,3,4}                       
ans = a:get{1,2,1}                     --> nil

a:set({1,2,1},4)
ans = a:get{1,2,1}                     --> 4

-- random array
b = Arr.rand {5,2,1}
-- number of elements
ans = #b                               --> 10

ans = b:copy()                         --> b

ans = b:isequal(Arr.rand{5,2,1})       --> true

-- arithmetical operations
c = b + b
ans = c:get{1,1,1}                     --> 2*b:get{1,1,1}

-- get subarray
g = a:sub({1,1,1},{-1,-1,2})         
ans = g:isequal(Arr{2,3,2})            --> true

-- concatenate along the 3-rd axes
d = Arr.concat(b,b,3)
ans = d:dim()[3]                       --> 2

e = Arr.apply(b,b, function (x,y) return x*y end)
ans = e:get{1,1,1}                     --> (b:get{1,1,1})^2

f = b:map(function (x) return 10*x end)
ans = f:get{1,1,1}                     --> b:get{1,1,1}*10

ans = tostring(a)                      --> 'array 2x3x4'

print(d:fullstring(2,3))
]]
---------------------------------------------

local array = {}
array.__index = array

array.type = 'array'

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
array.about = help:new("Manipulations with arrays of elements")

-- check object type
local function isarray(t)
   return type(t) == 'table' and t.type == array.type 
end

-- prepare list of coefficients
local function getk (s)
   local k = {1}
   for i = 2, #s do k[i] = s[i-1]*k[i-1] end
   return k
end

-- constructor
function array:new(s)
   assert(type(s) == 'table', "Table is expected!")
   for i = 1,#s do assert(s[i] > 0 and math.tointeger(s[i]), "Positive integer is expected!") end
   local o = {size = s, k = getk(s)}
   setmetatable(o, self)
   return o
end

-- check index correctness
local function iscorrect(arr, ind)
   if #arr.size ~= #ind then return false end
   for i = 1,#ind do 
      if ind[i] > arr.size[i] or ind[i] < 1 then return false end
   end
   return true
end

-- transform index into single dimention representation
local function index(arr, ind)
   local res = ind[1]
   for i = 2,#ind do 
      res = res + (ind[i]-1) * arr.k[i]
   end
   return res
end

-- number of elements
local function capacity(arr)
   return arr.size[#arr.size] * arr.k[#arr.k]
end

-- get element
array.get = function (arr, ind)
   assert(iscorrect(arr, ind), "Wrong index for given array!")
   return arr[index(arr, ind)]
end
array.about[array.get] = {"get(arr,ind)", "Get array element. Index is a table.", help.BASE}

-- set value
array.set = function (arr, ind, val)
   assert(iscorrect(arr, ind), "Wrong index for given array!")
   arr[index(arr,ind)] = val 
end
array.about[array.set] = {"set(arr,ind,val)", "Set value to the array. Index is a table.", help.BASE}

-- get array copy
array.copy = function (arr)
   assert(isarray(arr), "Not an array!")
   local cp = array:new(table.move(arr.size, 1, #arr.size, 1, {}))
   return table.move(arr, 1, capacity(arr), 1, cp)
end
array.about[array.copy] = {"copy(arr)", "Get copy of the array.", help.OTHER}

-- check array size
array.isequal = function (a1, a2)
   if not (isarray(a1) and isarray(a2)) then return false end
   if #a1.size ~= #a2.size then return false end
   for i = 1, #a1.size do
      if a1.size[i] ~= a2.size[i] then return false end
   end
   return true
end
array.about[array.isequal] = {"isequal(a1,a2)", "Check size equality.", help.OTHER}

-- apply function of 2 arguments
array.apply = function (a1, a2, fn)
   assert(array.isequal(a1,a2), "Not compatible arrays!")
   local res, v = array:new(table.move(a1.size, 1, #a1.size, 1, {}))
   for i = 1, capacity(a1) do 
      v = fn(a1[i] or 0, a2[i] or 0)
      if v ~= 0 then res[i] = v end
   end
   return res
end
array.about[array.apply] = {"apply(a1,a2,fn)", "Apply function of 2 arguments. Return new array.", help.OTHER}

-- apply function of 1 argument
array.map = function (a, fn)
   local res, v = array:new(table.move(a.size, 1, #a.size, 1, {}))
   for i = 1, capacity(a) do
      v = fn(a[i] or 0)
      if v ~= 0 then res[i] = v end
   end
   return res
end
array.about[array.map] = {"map(a,fn)", "Apply function of 1 argument. Return new array.", help.OTHER}

-- a1 + a2
array.__add = function (a1, a2)
   return array.apply(a1, a2, function (x,y) return x+y end)
end

-- a1 - a2
array.__sub = function (a1, a2)
   return array.apply(a1, a2, function (x,y) return x-y end)
end

-- -a
array.__unm = function (a)
   return array.map(a, function (x) return -x end)
end

-- a1 * a2
array.__mul = function (a1, a2)
   return array.apply(a1, a2, function (x,y) return x*y end)
end

-- a1 / a2
array.__div = function (a1, a2)
   return array.apply(a1, a2, function (x,y) return x/y end)
end

array.__pow = function (a1, a2)
   return array.apply(a1, a2, math.pow)
end

array.arithmetic = 'arithmetic'
array.about[array.arithmetic] = {array.arithmetic, "a+b, a-b, a*b, a/b, -a, a^b", help.BASE}

-- random array
array.rand = function (s)
   local arr = array:new(s)
   for i = 1, capacity(arr) do arr[i] = math.random() end
   return arr
end
array.about[array.rand] = {"rand(size)", "Return array with random numbers.", help.BASE}

-- check array equality
array.__eq = function (a1, a2)
   if not array.isequal(a1,a2) then return false end
   local v1, v2
   for i = 1, capacity(a1) do
      v1, v2 = a1[i] or 0, a2[i] or 0
      if v1 ~= v2 then return false end
   end
   return true
end

array.comparation = 'comparation'
array.about[array.comparation] = {array.comparation, "a == b, a ~= b", help.BASE}

-- get array dimentions
array.dim = function (arr)
   return table.move(arr.size, 1, #arr.size, 1, {})
end
array.about[array.dim] = {"dim(arr)", "Return size of array.", help.BASE}

-- get part of array
array.sub = function (arr, ind1, ind2)
   for i = 1, #ind2 do if ind2[i] < 0 then ind2[i] = ind2[i] + arr.size[i] + 1 end end
   assert(iscorrect(arr, ind1) and iscorrect(arr, ind2), "Wrong index!")
   -- prepare tables
   local newsize, ind = {}, {}
   for i = 1, #ind1 do 
      newsize[i] = ind2[i] - ind1[i] + 1 
      ind[i] = 0
   end 
   local res = array:new(newsize)
   -- fill
   for count = 1, capacity(res) do
      for i = 1, #ind do 
         ind[i] = (count // res.k[i]) % res.size[i]
	 ind2[i] = ind1[i] + ind[i]
	 ind[i] = ind[i]+1
      end
      res[index(res,ind)] = arr[index(arr,ind2)]
   end
   return res
end
array.about[array.sub] = {"sub(arr,ind1,ind2)", "Return subarray restricted by 2 indexes.", help.BASE}

-- concatenate 2 arrays
array.concat = function (arr1, arr2, axe)
   assert(axe > 0 and axe <= #arr1.size, "Wrong axe!")
   for i = 1, #arr1.size do assert(arr1.size[i] == arr2.size[i] or i == axe, "Different size!") end

   local newsize = table.move(arr1.size, 1, #arr1.size, 1, {})
   newsize[axe] = newsize[axe] + arr2.size[axe]

   local res, ind1, ind2 = array:new(newsize), table.move(newsize, 1, #newsize, 1, {}), table.move(newsize, 1, #newsize, 1, {})
   local edge = arr1.size[axe]
   for count = 1, capacity(res) do
      for i = 1, #ind1 do
         ind1[i] = (count // res.k[i]) % res.size[i] + 1
	 ind2[i] = ind1[i]
      end
      local second = ind1[axe] > edge
      ind2[axe] = second and (ind2[axe]-edge) or ind2[axe]
      res[index(res,ind1)] = second and arr2[index(arr2,ind2)] or arr1[index(arr1,ind2)]
   end
   return res 
end
array.about[array.concat] = {"concat(a1,a2,axe)", "Array concatenation along given axe.", help.BASE}

-- get number of elements
array.__len = function (arr)
   return capacity(arr)
end

-- string representation
array.__tostring = function (arr)
   return 'array ' .. table.concat(arr.size, 'x')
end

array.fullstring = function (arr, r, c)
   local res = {}
   
   -- 1D array
   if #arr.size == 1 then
      for i = 1, arr.size[1] do res[i] = arr[i] or 0 end
      return table.concat(res, ' ')
   elseif #arr.size == 2 then
      r = r or 1
      c = c or 2
   end

   assert(r > 0 and r <= #arr.size and c > 0 and c <= #arr.size and r ~= c, "Wrong indexes!")

   -- get 2D layer as string
   local function layer (ind)
            local row = {}
	    for i = 1, arr.size[r] do
	       ind[r] = i
	       -- get row
	       local col = {}
	       for j = 1, arr.size[c] do
	          ind[c]  = j
		  col[#col+1] = arr[index(arr, ind)] or 0
	       end
	       -- save
	       row[#row+1] = table.concat(col, ' ')
	    end
	    return table.concat(row, '\n')
         end

   -- prepare index calculation
   local bound, current, extent = {}, {}, {}
   local prod = 1
   for i = 1, #arr.size do
      if i ~= r and i ~= c then 
         bound[#bound+1] = arr.size[i]; 
	 current[#current+1] = 0 
	 extent[#extent+1] = prod
	 prod = prod * arr.size[i]
      end
   end
   local next_index = table.move(arr.size, 1, #arr.size, 1, {})

   for counter = 0, prod-1 do
      -- find coefficients
      for i = #current,1,-1 do current[i] = (counter // extent[i]) % bound[i] end
      -- write
      local k = #current
      for i = #next_index, 1, -1 do
         if i == r then next_index[i] = 'R'
	 elseif i == c then next_index[i] = 'C'
	 else
	    next_index[i] = current[k]+1
	    k = k-1
	 end
      end
      -- get strings
      res[#res+1] = '{' .. table.concat(next_index,',') .. '}' 
      res[#res+1] = layer(next_index)
      res[#res+1] = ''
   end
   return table.concat(res, '\n') 
end
array.about[array.fullstring] = {"fullstring(arr,r,c)", "Represent array as sequence of matrixes, where r and c are numbers of axes", help.OTHER}

-- constructor
setmetatable(array, {__call = function (self, v) return array:new(v) end})
array.Arr = 'Arr'
array.about[array.Arr] = {"Arr(size)", "Create empty array with given size, represented as a table.", help.NEW}

-- serialization
array.serialize = function (obj)
   local s = {}
   s[#s+1] = 'size={' .. table.concat(obj.size, ',') .. '}'
   s[#s+1] = 'k={' .. table.concat(obj.k, ',') .. '}'
   for i = 1, capacity(obj) do
      if obj[i] then
         s[#s+1] = string.format("[%d]=%s", i, (type(obj[i]) == 'string' and "'"..obj[i].."'" or obj[i]))
      end
   end
   s[#s+1] = "metatablename='Arr'"
   s[#s+1] = "modulename='array'"
   return string.format("{%s}", table.concat(s, ','))
end
array.about[array.serialize] = {"serialize(obj)", "String representation of array internal structure.", help.OTHER}

array.getnext = function (arr)
   local a = array:new(table.move(arr.size, 1, #arr.size, 1, {})) -- copy size
   local count = 0

   return function ()
             if count == capacity(a) then return nil end
	     local res = {}
	     for i = 1, #a.size do res[i] = (count // a.k[i]) % a.size[i]+1 end
	     count = count + 1
	     return res
          end
end
array.about[array.getnext] = {"getnext(arr)", "Return iterator along all indexes", help.OTHER}

return array
