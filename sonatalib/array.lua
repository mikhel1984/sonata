--[[      sonatalib/array.lua 

--- Manipulations with arrays of elements.
--  Arrays are sparse as long as possible.
--
--  Object structure:                     </br>
--  <code>{a1...an, b1...bn, c1...cn, ...,</br> 
--  array_size, index_coefficients}</code></br>
--  i.e. all elements of the array are written sequentially, row by row. Position of one element is calculated as
--  <code>C1*n1+C2*n2+...+Ck*nk</code>
--  where <code>{n1,n2,...nk}<code> - index, <code>C1...Ck</code> - size based coefficients.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonatalib</a> collection, 2017-2019.

            module 'array'
--]]

------------------- Tests -------------------
--[[TEST

-- import 'array'
Arr = require 'sonatalib.array'

-- empty array
a = Arr {2,3,4}                       
ans = a:get{1,2,1}                     --> nil

-- set values
a:set({1,2,1},4)
ans = a:get{1,2,1}                     --> 4

-- random array
b = Arr.rand {5,2,1}
-- max number of elements
ans = #b                               --> 10

ans = b:copy()                         --> b

-- compare sizes
ans = b:isEqual(Arr.rand{5,2,1})       --> true

-- arithmetical operations
c = b + b
ans = c:get{1,1,1}                     --> 2*b:get{1,1,1}

-- get sub array
g = a:sub({1,1,1},{-1,-1,2})         
ans = g:isEqual(Arr{2,3,2})            --> true

-- concatenate along the 3-rd axes
d = Arr.concat(b,b,3)
-- size for given dimension
ans = d:dim()[3]                       --> 2

-- apply function of several arguments
a1 = Arr.rand{2,2}
a2 = Arr.rand{2,2}
a3 = Arr.rand{2,2}
fn = function (x,y,z) return x*y+z end
a4 = Arr.apply(fn, a1,a2,a3)
ans = a4:get{1,2}                      --> a1:get{1,2}*a2:get{1,2}+a3:get{1,2}


-- apply function of 1 argument
-- to get new array
f = b:map(function (x) return 10*x end)
ans = f:get{1,1,1}                     --> b:get{1,1,1}*10

-- simple print
print(a)

-- iterate over array 
for ind, val in a4:next() do io.write('{',ind[1],',',ind[2],'}\t',val, '\n') end

--]]

-- 	LOCAL

-- Compatibility with previous versions
local Ver = require "sonatalib.versions"

--- Check object type.
--  @param t Object.
--  @return True if the object is array.
local function isarray(t) return type(t) == 'table' and t.isarray end

--	INFO

local help = LC_DIALOG and (require "sonatalib.help") or {new=function () return {} end}

-- 	MODULE

local array = {
-- mark
type='array', isarray=true,
-- elementary function definition
_add_ = function (x,y) return x+y end,
_sub_ = function (x,y) return x-y end,
_unm_ = function (x) return -x end,
_mul_ = function (x,y) return x*y end,
_div_ = function (x,y) return x/y end,
_pow_ = function (x,y) return x^y end,
-- description
about = help:new("Manipulations with arrays of elements. Indices have form of tables. Indexation from 1.")
}
-- metamethods
array.__index = array

--- Create new object, set metatable.
--  @param self Pointer to object.
--  @param tSize Table with array size.
--  @return Empty array.
array.new = function (self, tSize)
   -- prepare list of coefficients
   local k = {1}
   for i = 2, #tSize do k[i] = tSize[i-1]*k[i-1] end
   -- return new object
   return setmetatable({size=tSize, k=k}, self)
end

--- Check index correctness.
--  @param A Array.
--  @param tInd Index to check.
--  @return True if the index is correct for current array.
array._isIndex_ = function (A, tInd)
   if #A.size ~= #tInd then return false end
   for i = 1,#tInd do 
      if tInd[i] > A.size[i] or tInd[i] < 1 then return false end
   end
   return true
end

--- Transform index into single dimension representation.
--  @param A Array.
--  @param tInd Element index.
--  @return Position of the element in the array table.
array._indexConvert_ = function (A, tInd)
   local res, k = tInd[1], A.k
   for i = 2,#tInd do 
      res = res + (tInd[i]-1) * k[i]
   end
   return res
end

--- Maximum number of elements in the array.
--  The same as #A.
--  @param A Array.
--  @return "Volume" of the array.
array.capacity = function (A)
   local S,K = A.size, A.k
   return S[#S] * K[#K]
end
array.about[array.capacity] = {"capacity(A)", "Maximal number of elements in the array. The same as #A.", help.OTHER}

--- Get array element.
--  @param A Array object.
--  @param tInd Index of the element (table).
--  @return Element or error if index is wrong.
array.get = function (A, tInd)
   if not array._isIndex_(A, tInd) then error("Wrong index!") end
   return A[array._indexConvert_(A, tInd)]
end
array.about[array.get] = {"get(A,tInd)", "Get array element."}

--- Set new value.
--  @param A Array object.
--  @param tInd Element index (table).
--  @param val New value.
array.set = function (A, tInd, val)
   if not array._isIndex_(A, tInd) then error("Wrong index!") end
   A[array._indexConvert_(A,tInd)] = val 
end
array.about[array.set] = {"set(A,tInd,val)", "Set value to the array."}

--- Get array copy.
--  @param A Array object.
--  @return Deep copy of the array.
array.copy = function (A)
   local cp = array:new(Ver.move(A.size, 1, #A.size, 1, {}))          -- copy size, create new array
   return Ver.move(A, 1, array.capacity(A), 1, cp)                   -- copy array elements
end
array.about[array.copy] = {"copy(A)", "Get copy of the array.", help.OTHER}

--- Compare array size.
--  @param A1 First array object.
--  @param A2 Second array object.
--  @return True if size is the same.
array.isEqual = function (A1, A2)
   if isarray(A1) and isarray(A2) and #A1.size == #A2.size then
      -- compare size
      for i = 1, #A1.size do
         if A1.size[i] ~= A2.size[i] then return false end
      end
      return true
   end
   return false
end
array.about[array.isEqual] = {"isEqual(A1,A2)", "Check size equality.", help.OTHER}

--- Apply function of 2 arguments.
--  @param func Function with 2 arguments.
--  @param A1 First array object.
--  @param A2 Second array object.
--  @return New array where each element is result of the function evaluation func(A1[i],A2[i]).
array._apply2_ = function (func, A1, A2)
   if not array.isEqual(A1,A2) then error("Not compatible arrays!") end
   local res, v = array:new(Ver.move(A1.size, 1, #A1.size, 1, {}))    -- prepare empty copy
   for i = 1, array.capacity(A1) do 
      v = func(A1[i] or 0, A2[i] or 0)                                -- elements can be empty
      if v ~= 0 then res[i] = v end                                   -- save nonzero values
   end
   return res
end

--- Apply function of several arguments.
--  @param func Function with 2 arguments.
--  @param ... List of arrays.
--  @return New array where each element is result of the function evaluation.
array.apply = function (func, ...)
   local arg = {...}
   -- check arguments
   local eq = array.isEqual
   for i = 2,#arg do
      if not eq(arg[i],arg[i-1]) then error("Not compatible arrays!") end
   end
   -- prepare new
   local tmp, v = arg[1], {}
   local res = array:new(Ver.move(tmp.size, 1, #tmp.size, 1, {}))
   local upack = Ver.unpack
   for i = 1, array.capacity(tmp) do
      -- collect
      for k = 1,#arg do v[k] = arg[k][i] or 0 end
      -- evaluate
      local p = func(upack(v))
      if p ~= 0 then res[i] = p end
   end
   return res
end
array.about[array.apply] = {"apply(func, ...)", "Apply function of several arguments. Return new array.", help.OTHER}

--- Apply function of 1 argument.
--  @param A Array object.
--  @param func Function with 1 argument.
--  @return New array where each element is result of the function evaluation func(a[i]).
array.map = function (A, func)
   local res, v = array:new(Ver.move(A.size, 1, #A.size, 1, {}))
   for i = 1, array.capacity(A) do
      v = func(A[i] or 0)                            -- elements can be empty
      if v ~= 0 then res[i] = v end                  -- save nonzero values
   end
   return res
end
array.about[array.map] = {"map(A,func)", "Apply function of 1 argument. Return new array.", help.OTHER}

--- A1 + A2
--  @param A1 First array.
--  @param A2 Second array.
--  @return Array with element wise sum.
array.__add = function (A1, A2) return array._apply2_(array._add_, A1, A2) end

--- A1 - A2
--  @param A1 First array.
--  @param A2 Second array.
--  @return Array with element wise difference.
array.__sub = function (A1, A2) return array._apply2_(array._sub_, A1, A2) end

--- -A
--  @param A Array.
--  @return Array with inverted sign.
array.__unm = function (A) return array.map(A, array._unm_) end

--- A1 * A2
--  @param A1 First array.
--  @param A2 Second array.
--  @return Array with element wise product.
array.__mul = function (A1, A2) return array._apply2_(array._mul_, A1, A2) end

--- A1 / A2
--  @param A1 First array.
--  @param A2 Second array.
--  @return Array with element wise ratio.
array.__div = function (A1, A2) return array._apply2_(array._div_, A1, A2) end

--- A1 ^ A2
--  @param A1 First array.
--  @param A2 Second array.
--  @return Array with element wise power.
array.__pow = function (A1, A2) return array._apply2_(array._pow_, A1, A2) end

array.arithmetic = 'arithmetic'
array.about[array.arithmetic] = {array.arithmetic, "a+b, a-b, a*b, a/b, -a, a^b", help.META}

--- Random array generator.
--  @param tSize Size table.
--  @return Array of the given size with random numbers from 0 to 1.
array.rand = function (tSize)
   local arr = array:new(tSize)
   local rnd = math.random
   for i = 1, array.capacity(arr) do arr[i] = rnd() end
   return arr
end
array.about[array.rand] = {"rand(tSize)", "Return array with random numbers between 0 and 1.", help.NEW}

--- a1 == A2
--  @param A1 First array.
--  @param A2 Second array.
--  @return True if all elements are equal.
array.__eq = function (A1, A2)
   if array.isEqual(A1,A2) then
      -- compare element wise
      for i = 1, array.capacity(A1) do
	 if (A1[i] or 0) ~= (A2[i] or 0) then return false end
      end
      return true
   end
   return false
end

array.comparison = 'comparison'
array.about[array.comparison] = {array.comparison, "a == b, a ~= b", help.META}

--- Get array dimension.
--  @param A Array object.
--  @return Table with array size.
array.dim = function (A) return Ver.move(A.size, 1, #A.size, 1, {}) end
array.about[array.dim] = {"dim(A)", "Return size of array."}

--- Get part of array between two indexes.
--  @param A Array object.
--  @param tInd1 Index of the lower bound.
--  @param tInd2 Index of the upper bound.
--  @return Array of elements restricted by the indices.
array.sub = function (A, tInd1, tInd2)
   -- negative index means the last element
   for i = 1, #tInd2 do 
      if tInd2[i] < 0 then tInd2[i] = tInd2[i] + A.size[i] + 1 end 
   end
   if not (array._isIndex_(A, tInd1) and array._isIndex_(A, tInd2)) then error("Wrong index!") end
   -- prepare tables
   local newsize, ind = {}, {}
   for i = 1, #tInd1 do 
      newsize[i] = tInd2[i] - tInd1[i] + 1 
      ind[i]     = 0
   end 
   local res = array:new(newsize)
   -- fill
   local K, S = res.k, res.size
   local conv = array._indexConvert_
   for count = 1, array.capacity(res) do
      -- calculate new temporary index
      for i = 1, #ind do 
         local tmp = math.modf(count/K[i]) % S[i]
	 tInd2[i] = tInd1[i] + tmp
	 ind[i] = tmp + 1
      end
      res[conv(res,ind)] = A[conv(A,tInd2)]
   end
   return res
end
array.about[array.sub] = {"sub(A,tInd1,tInd2)", "Return sub array restricted by 2 indexes."}

--- Concatenate 2 arrays along given axes.
--  @param A1 First array.
--  @param A2 Second array.
--  @param nAxis Axis number.
--  @return New concatenated array.
array.concat = function (A1, A2, nAxis)
   -- check size
   if not (nAxis > 0 and nAxis <= #A1.size) then error("Wrong axis!") end
   for i = 1, #A1.size do 
     if not (A1.size[i] == A2.size[i] or i == nAxis) then error("Different size!") end
   end
   -- prepare new size
   local newsize = Ver.move(A1.size, 1, #A1.size, 1, {})
   newsize[nAxis] = newsize[nAxis] + A2.size[nAxis]
   -- combine
   local res, ind1, ind2 = array:new(newsize), {}, {}
   local edge = A1.size[nAxis]
   local K, S = res.k, res.size
   local conv = array._indexConvert_
   for count = 1, array.capacity(res) do
      -- prepare index
      for i = 1, #newsize do
         ind1[i] = math.modf(count/K[i]) % S[i] + 1
	 ind2[i] = ind1[i]
      end
      -- get value
      local second = ind1[nAxis] > edge
      ind2[nAxis] = second and (ind2[nAxis]-edge) or ind2[nAxis]
      res[conv(res,ind1)] = second and A2[conv(A2,ind2)] or A1[conv(A1,ind2)]
   end
   return res 
end
array.about[array.concat] = {"concat(A1,A2,nAxis)", "Array concatenation along the given axis."}

--- Method #
--  @param A Array object.
--  @return Number of elements.
array.__len = array.capacity

--- String representation.
--  @param A Array.
--  @return String representation of the array.
array.__tostring = function (A) return 'Array ' .. table.concat(A.size, 'x') end

-- Constructor
setmetatable(array, {__call = function (self, tSize) 
   -- check correctness
   assert(type(tSize) == 'table', "Table is expected!")
   for i = 1,#tSize do assert(tSize[i] > 0 and Ver.isInteger(tSize[i]), "Positive integer is expected!") end
   -- build
   return array:new(tSize) 
end})
array.Arr = 'Arr'
array.about[array.Arr] = {"Arr(tSize)", "Create empty array with given size.", help.NEW}

--- Iterator across the array.
--  @param A Array object.
--  @return Iterator which calculate index of the next array element and the element itself, <code>nil</code> at the end.
array.next = function (A)
   local a = array:new(Ver.move(A.size, 1, #A.size, 1, {})) -- copy size
   local count, S, K, len = 0, a.size, a.k, array.capacity(a)

   return function ()
             if count == len then return nil, nil end
	     local res = {}
	     for i = 1, #S do res[i] = math.modf(count/K[i]) % S[i]+1 end
	     count = count + 1
	     return res, A[array._indexConvert_(A,res)]
          end
end
array.about[array.next] = {"next(A)", "Return iterator along all indexes.", help.OTHER}

-- Free memory if need
if not LC_DIALOG then array.about = nil end

return array

--=======================
--TODO: slice to matrix (square table)
