

local array = {}
array.__index = array

array.type = 'array'

-- check object type
local function isarray(t)
   return type(t) == 'table' and t.type == array.type 
end

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
      if ind[i] > arr.size[i] or ind[i] < -arr.size[i] or ind[i] == 0 then return false end
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

local function capacity(arr)
   local prod = arr.size[1]
   for i = 2, #arr.size do prod = prod * arr.size[i] end
   return prod
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
   return table.move(arr, 1, capacity(arr), 1, cp)
end

-- check array size
array.isequal = function (a1, a2)
   if not (isarray(a1) and isarray(a2)) then return false end
   if #a1.size ~= #a2.size then return false end
   for i = 1, #a1.size do
      if a1.size[i] ~= a2.size[i] then return false end
   end
   return true
end

array.apply = function (a1, a2, fn)
   assert(array.isequal(a1,a2), "Not compatible arrays!")
   local res, v = array:new(table.move(a1.size, 1, #a1.size, 1, {}))
   for i = 1, capacity(a1) do 
      v = fn(a1[i] or 0, a2[i] or 0)
      if v ~= 0 then res[i] = v end
   end
   return res
end

array.map = function (a, fn)
   local res, v = array:new(table.move(a.size, 1, #a.size, 1, {}))
   for i = 1, capacity(a) do
      v = fn(a[i] or 0)
      if v ~= 0 then res[i] = v end
   end
   return res
end

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

-- random array
array.rand = function (s)
   local arr = array:new(s)
   for i = 1, capacity(arr) do arr[i] = math.random() end
   return arr
end

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

-- get array dimentions
array.dim = function (arr)
   local res = {}
   table.move(arr.size, 1, #arr.size, 1, res)
   return res
end

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

-----------------------------
--           test
----------------------------

--a = array:new {2, 4, 3}
--b = a:copy()
a = array.rand {2,3,4}
b = array.rand {2,3,4}

--[[
d = a + b
d = a - b
d = a * b
d = a / b
d = -d

print(d:fullstring(1,2))
]]

--print(b)

--[[
for i = 1,2 do
   for j = 1,4 do
      for k = 1,3 do
         print(i,j,k,index(a, {i,j,k}))
      end
   end
end
]]

