

local array = {}
array.__index = array

array.type = 'array'

-- check object type
local function isarray(t)
   return type(t) == 'table' and t.type == array.type 
end

-- constructor
function array:new(s)
   assert(type(s) == 'table', "Table is expected!")
   for i = 1,#s do assert(s[i] > 0 and math.tointeger(s[i]), "Positive integer is expected!") end
   local o = {size = s}
   setmetatable(o, self)
   return o
end

-- check index correctness
local function iscorrect(arr, ind)
   if #arr.size ~= #ind then return false end
   for i = 1,#ind do 
      if ind[i] > arr.size[i] or ind[i] < -arr.size[i] or ind[i] == 0 then return false end
   end
   return true
end

-- transform index into single dimention representation
local function index(arr, ind)
   local res, prod = ind[1], arr.size[1]
   for i = 2,#ind do 
      res = res + (ind[i]-1) * prod
      prod = prod * arr.size[i]
   end
   return res
end

-- get element
array.get = function (arr, ind)
   assert(iscorrect(arr, ind), "Wrong index for given array!")
   return arr[index(arr, ind)]
end

-- set value
array.set = function (arr, ind, val)
   assert(iscorrect(arr, ind), "Wrong index for given array!")
   arr[index(arr,ind)] = val 
end

-- get array copy
array.copy = function (arr)
   assert(isarray(arr), "Not an array!")
   local cp = array:new(table.move(arr.size, 1, #arr.size, 1, {}))
   return table.move(arr, 1, #arr, 1, cp)
end

-- check array size
array.isequal = function (a1, a2)
   if a1.type ~= a2.type then return false end
   if #a1.size ~= #a2.size then return false end
   for i = 1, #a1.size do
      if a1.size[i] ~= a2.size[i] then return false end
   end
   return true
end

-- check array equality
array.__eq = function (a1, a2)
   if not array.isequal(a1,a2) then return false end
   local v1, v2
   for i = 1, #a1 do
      v1, v2 = a1[i] or 0, a2[i] or 0
      if v1 ~= v2 then return false end
   end
   return true
end

-- get array dimentions
array.dim = function (arr)
   local res = {}
   table.move(arr.size, 1, #arr.size, 1, res)
   return res
end

-- get number of elements
array.__len = function (arr)
   local res = arr.size[1]
   for i = 2,#arr.size do res = res * arr.size[i] end
   return res
end

-- string representation
array.__tostring = function (arr)
   return 'array ' .. table.concat(arr.size, 'x')
end

-----------------------------
--           test
----------------------------

a = array:new {5, 4, 3}
b = a:copy()

print(b)

--[[
for i = 1,2 do
   for j = 1,3 do
      for k = 1,4 do
         print(i,j,k,index(a, {i,j,k}))
      end
   end
end
]]

