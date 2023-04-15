--[[		sonata/lib/array.lua

--- Manipulations with arrays of elements.
--  Arrays are sparse as long as possible.
--
--  Object structure:</br>
--  <code>{a1...an, b1...bn, c1...cn, ..., </br>
--  array_size, index_coefficients}</code></br>
--  i.e. all elements of the array are written sequentially, column by column.
--  Position of one element is calculated as
--  <code>C1*n1+C2*n2+...+Ck*nk</code>
--  where <code>{n1, n2,...nk}</code> - index, <code>C1...Ck</code> - size based
--  coefficients.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2023.

	module 'array'
--]]

------------------- Tests -------------------
--[[TEST

-- use 'array'
Arr = require 'lib.array'

-- empty array
a = Arr {2,3,4}
ans = a:get{1,2,1}            --> nil

-- set values
a:set({1,2,1},4)
ans = a:get{1,2,1}            --> 4

-- random array
rnd = function () return math.random() end
b = Arr{5,2,1}:map(rnd)
-- max number of elements
ans = #b                      --> 10

-- compare sizes
ans = b:isEqual(Arr{5,2,1})   --> true

-- compare elements
ans = (b == b:copy())         --> true

-- get sub array
g = a:sub({1,1,1},{-1,-1,2})
ans = g:isEqual(Arr{2,3,2})   --> true

-- apply function with indeces
eye = function (x,t) return (t[1]==t[2] and t[1]==t[3]) and 1 or 0 end
f = Arr{3,3,3}:map(eye)
ans = f:get{2,2,2}            --> 1

-- apply function of several arguments
a1 = Arr{2,2}:map(rnd)
a2 = Arr{2,2}:map(rnd)
a3 = Arr{2,2}:map(rnd)
-- 'lazy' function definition
a4 = Arr:zip('x1*x2+x3', a1,a2,a3)
ans = a4:get{1,2}             --> a1:get{1,2}*a2:get{1,2}+a3:get{1,2}

-- iterate over array
g = Arr {2,2}
g:set({1,1}, 1)
g:set({2,1}, 2)
g:set({1,2}, 3)
g:set({2,2}, 4)
-- show
for ind, val in g:ipairs() do
  io.write('{',ind[1],',',ind[2],'}\t',val, '\n')
end

-- simple iteration
n = g:capacity()   -- number of elements
for i = 1,n do
  io.write(i,':',g[i],'\n')
end

-- concatenate along the 3-rd axes
d = b:concat(b,3)
-- size for given dimension
ans = d:dim()[3]              --> 2

-- simple print
print(a)

--]]

--	LOCAL

-- Compatibility with previous versions
local Ver = require("lib.utils")
local Utils = Ver.utils
Ver = Ver.versions


--- Check object type.
--  @param t Object.
--  @return True if the object is array.
local function isarray(v) return type(v) == 'table' and v.isarray end


--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Manipulations with arrays of elements. Indices have form of tables. Indexation from 1."
}

--	MODULE

local array = { type='array', isarray=true }


--- a1 == A2
--  @param A1 First array.
--  @param A2 Second array.
--  @return True if all elements are equal.
array.__eq = function (A1, A2)
  if array.isEqual(A1, A2) then
    -- compare element wise
    for i = 1, array.capacity(A1) do
      if (A1[i] or 0) ~= (A2[i] or 0) then return false end
    end
    return true
  end
  return false
end


-- metamethods
array.__index = array


--- Method #
--  @param A Array object.
--  @return Number of elements.
array.__len = array.capacity


--- String representation.
--  @param A Array.
--  @return String representation of the array.
array.__tostring = function (A) return 'array '..table.concat(A.size, 'x') end


array.comparison = 'comparison'
about[array.comparison] = {array.comparison, "a == b, a ~= b", help.META}


--- Get index from position number.
--  @param A Array object.
--  @param N position.
--  @return Index table.
array._index = function (A, N)
  local index = {}
  local S, K = A.size, A.k
  for i = 1, #S do
    index[i] = math.modf(N / K[i]) % S[i]+1
  end
  return index
end


--- Check index correctness.
--  @param A Array.
--  @param tInd Index to check.
--  @return True if the index is correct for the current array.
array._isIndex = function (A, tInd)
  if #A.size ~= #tInd then return false end
  for i = 1, #tInd do
    if tInd[i] > A.size[i] or tInd[i] < 1 then return false end
  end
  return true
end


--- Create new object, set metatable.
--  @param self Pointer to object.
--  @param tSize Table with array size.
--  @return Empty array.
array._new = function (self, tSize)
  -- prepare list of coefficients
  local k = {1}
  for i = 2, #tSize do k[i] = tSize[i-1]*k[i-1] end
  -- return new object
  return setmetatable({size=tSize, k=k}, self)
end


--- Transform index into single dimension representation.
--  @param A Array.
--  @param tInd Element index.
--  @return Position of the element in the array table.
array._pos = function (A, tInd)
  local res, k = tInd[1], A.k
  for i = 2, #tInd do
    res = res + (tInd[i]-1) * k[i]
  end
  return res
end


--- Maximum number of elements in the array.
--  The same as #A.
--  @param A Array.
--  @return "Volume" of the array.
array.capacity = function (A)
  local S, K = A.size, A.k
  return S[#S] * K[#K]
end
about[array.capacity] = {"A:capacity() --> int",
  "Maximal number of elements in the array. The same as #A.", help.OTHER}


--- Concatenate 2 arrays along given axes.
--  @param A1 First array.
--  @param A2 Second array.
--  @param iAxis Axis number.
--  @return New concatenated array.
array.concat = function (A1, A2, iAxis)
  -- check size
  if not (iAxis > 0 and iAxis <= #A1.size) then error("Wrong axis!") end
  for i = 1, #A1.size do
    if not (A1.size[i] == A2.size[i] or i == iAxis) then
      error("Different size!")
    end
  end
  -- prepare new size
  local newsize = Ver.move(A1.size, 1, #A1.size, 1, {})
  newsize[iAxis] = newsize[iAxis] + A2.size[iAxis]
  -- combine
  local res, ind1, ind2 = array:_new(newsize), {}, {}
  local edge = A1.size[iAxis]
  local K, S = res.k, res.size
  local conv = array._pos
  for count = 1, array.capacity(res) do
    -- prepare index
    for i = 1, #newsize do
      ind1[i] = math.modf(count/K[i]) % S[i] + 1
      ind2[i] = ind1[i]
    end
    -- get value
    local second = (ind1[iAxis] > edge)
    ind2[iAxis] = second and (ind2[iAxis]-edge) or ind2[iAxis]
    res[conv(res, ind1)] = second and A2[conv(A2, ind2)] or A1[conv(A1, ind2)]
  end
  return res
end
about[array.concat] = {"A:concat(A2, axis_N) --> A3",
  "Concatenate along the given array along the given axis."}


--- Get array copy.
--  @param A Array object.
--  @return Deep copy of the array.
array.copy = function (A)
  -- copy size, create new array
  local cp = array:_new(Ver.move(A.size, 1, #A.size, 1, {}))
  -- copy array elements
  return Ver.move(A, 1, array.capacity(A), 1, cp)
end
about[array.copy] = {"A:copy() --> cpy_A", "Get copy of the array.", help.OTHER}


--- Get array dimension.
--  @param A Array object.
--  @return Table with array size.
array.dim = function (A) return Ver.move(A.size, 1, #A.size, 1, {}) end
about[array.dim] = {"A:dim() --> int", "Return size of the array."}


--- Get array element.
--  @param A Array object.
--  @param tInd Index of the element (table).
--  @return Element or error if index is wrong.
array.get = function (A, tInd)
  if not array._isIndex(A, tInd) then error("Wrong index!") end
  return A[array._pos(A, tInd)]
end
about[array.get] = {"A:get(ind_t) --> var", "Get array element."}


--- Iterator across the array.
--  @param A Array object.
--  @return Iterator which calculate index of the next array element and the element itself, nil at the end.
array.ipairs = function (A)
  local count, len = 0, array.capacity(A)
  return function ()
    if count == len then return nil, nil end
    local index = array._index(A, count)
    count = count + 1
    return index, A[count]
  end
end
about[array.ipairs] = {"A:ipairs() --> iter_fn", "Return iterator along all indexes.",
  help.OTHER}


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
about[array.isEqual] = {"A:isEqual(A2) --> bool", "Check size equality.", help.OTHER}


--- Apply function of 1 argument.
--  @param A Array object.
--  @param fn Function of value and (optional) index.
--  @return New array where each element is result of the function evaluation fn(a[i]).
array.map = function (A, fn)
  local res, v = array:_new(Ver.move(A.size, 1, #A.size, 1, {})), nil
  for i = 1, array.capacity(A) do
    v = fn(A[i], array._index(A, i-1))
    res[i] = v
  end
  return res
end
about[array.map] = {"A:map(fn) --> out_A", "Apply function fn(x,[ind]) to all elements, return new array.", help.OTHER}


--- Set new value.
--  @param A Array object.
--  @param tInd Element index (table).
--  @param v New value.
array.set = function (A, tInd, v)
  if not array._isIndex(A, tInd) then error("Wrong index!") end
  A[array._pos(A, tInd)] = v
end
about[array.set] = {"A:set(ind_t, var) --> nil", "Set value to the array."}


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
  if not (array._isIndex(A, tInd1) and array._isIndex(A, tInd2)) then
    error("Wrong index!")
  end
  -- prepare tables
  local newsize, ind = {}, {}
  for i = 1, #tInd1 do
    newsize[i] = tInd2[i] - tInd1[i] + 1
    ind[i]     = 0
  end
  local res = array:_new(newsize)
  -- fill
  local K, S = res.k, res.size
  local conv = array._pos
  for count = 1, array.capacity(res) do
    -- calculate new temporary index
    for i = 1, #ind do
      local tmp = math.modf(count/K[i]) % S[i]
      tInd2[i] = tInd1[i] + tmp
      ind[i] = tmp + 1
    end
    res[conv(res, ind)] = A[conv(A, tInd2)]
  end
  return res
end
about[array.sub] = {"A:sub(ind1_t, ind2_t) --> range_A",
  "Return sub array restricted by 2 indexes."}


--- Apply function of several arguments.
--  @param self Do nothing.
--  @param fn Function with N arguments or expression.
--  @param ... List of N arrays.
--  @return New array where each element is result of the function evaluation.
array.zip = function (self, fn, ...)
  local arg = {...}
  if type(fn) == 'string' then fn = Utils.Fn(fn, #arg) end
  -- check arguments
  local eq, a1 = array.isEqual, arg[1]
  for i = 2, #arg do
    if not eq(arg[i], a1) then error("Not compatible arrays!") end
  end
  -- prepare new
  local v, upack = {}, Ver.unpack
  local res = array:_new(Ver.move(a1.size, 1, #a1.size, 1, {}))
  for i = 1, array.capacity(a1) do
    -- collect
    for k = 1, #arg do v[k] = arg[k][i] end
    -- evaluate
    res[i] = fn(upack(v))
  end
  return res
end
about[array.zip] = {":zip(fn, ...) --> A",
  "Apply function of several arguments. Return new array.", help.STATIC}


-- Constructor
setmetatable(array, {__call = function (self, tSize)
  -- check correctness
  assert(type(tSize) == 'table', "Table is expected!")
  for i = 1, #tSize do
    assert(tSize[i] > 0 and Ver.isInteger(tSize[i]),
           "Positive integer is expected!")
  end
  -- build
  return array:_new(tSize)
end})
about[array] = {" {size1_N, [size2_N, ..]} --> new_A",
  "Create empty array with the given size.", help.STATIC}


-- Comment to remove descriptions
array.about = about

return array

--=======================
-- TODO: slice to matrix (square table)
-- TODO: make row-by-row indexation
