--[[      sonatalib/array.lua 

--- Manipulations with arrays of elements.
--  Arrays are sparse as long as possible.
--
--  Object structure:                     </br>
--  <code>{a1...an, b1...bn, c1...cn, ...,</br> 
--  array_size, index_coefficients}</code></br>
--  i.e. all elements of the array are writen sequentially, row by row. Position of one element is calculated as
--  <code>C1*n1+C2*n2+...+Ck*nk</code>
--  where <code>{n1,n2,...nk}<code> - index, <code>C1...Ck</code> - size based coefficients.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">sonatalib</a> collection, 2017-2018.

            module 'array'
--]]

------------------- Tests -------------------
--[[!!
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
-- number of elements
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
ans = d:dim()[3]                       --> 2

-- apply functions of 2 arguments
-- to get new array
e = Arr.apply2(function (x,y) return x*y end, b, b)
ans = e:get{1,1,1}                     --> (b:get{1,1,1})^2

-- apply function of 1 argument
-- to get new array
f = b:map(function (x) return 10*x end)
ans = f:get{1,1,1}                     --> b:get{1,1,1}*10

-- simple print
print(a)

-- print slices for axis 2 and 3
print(d:fullString(2,3))
]]

-- 	LOCAL

-- Compatibility with previous versions
local Ver = require "sonatalib.versions"

--- Check object type.
--  @param t Object.
--  @return True if the object is array.
local function isarray(t) return type(t) == 'table' and t.isarray end

--	INFO

local help = lc_version and (require "sonatalib.help") or {new=function () return {} end}

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
about = help:new("Manipulations with arrays of elements.")
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
--  @param A Array.
--  @return "Volume" of the array.
array._capacity_ = function (A)
   local S,K = A.size, A.k
   return S[#S] * K[#K]
end

--- Get array element.
--  @param A Array object.
--  @param tInd Index of the element (table).
--  @return Element or error if index is wrong.
array.get = function (A, tInd)
   if not array._isIndex_(A, tInd) then error("Wrong index!") end
   return A[array._indexConvert_(A, tInd)]
end
array.about[array.get] = {"get(A,tInd)", "Get array element. Index is a table."}

--- Set new value.
--  @param A Array object.
--  @param tInd Element index (table).
--  @param val New value.
array.set = function (A, tInd, val)
   if not array._isIndex_(A, tInd) then error("Wrong index!") end
   A[array._indexConvert_(A,tInd)] = val 
end
array.about[array.set] = {"set(A,tInd,val)", "Set value to the array. Index is a table."}

--- Get array copy.
--  @param A Array object.
--  @return Deep copy of the array.
array.copy = function (A)
   local cp = array:new(Ver.move(A.size, 1, #A.size, 1, {}))          -- copy size, create new array
   return Ver.move(A, 1, array._capacity_(A), 1, cp)                   -- copy array elements
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
array.apply2 = function (func, A1, A2)
   if not array.isEqual(A1,A2) then error("Not compatible arrays!") end
   local res, v = array:new(Ver.move(A1.size, 1, #A1.size, 1, {}))    -- prepare empty copy
   for i = 1, array._capacity_(A1) do 
      v = func(A1[i] or 0, A2[i] or 0)                                -- elements can be empty
      if v ~= 0 then res[i] = v end                                   -- save nonzero values
   end
   return res
end
array.about[array.apply2] = {"apply2(func,A1,A2)", "Apply function of 2 arguments. Return new array.", help.OTHER}

array.apply = function (func, ...)
   arg = {...}
   -- check arguments
   for i = 2,#arg do
      if not array.isEqual(arg[i],arg[i-1]) then error("Not compatible arrays!") end
   end
   -- prepare new
   local tmp, v = arg[0]
   local res = array:new(Ver.move(tmp.size, 1, #tmp.size, 1, {}))
   for i = 1, array._capacity_(tmp) do
      -- collect
      for k = 1,#arg do v[k] = arg[k][i] or 0 end
      -- evaluate
      local p = func(Ver.unpack(v))
      if p ~= 0 then res[i] = p end
   end
end

--- Apply function of 1 argument.
--  @param A Array object.
--  @param func Function with 1 argument.
--  @return New array where each element is result of the function evaluation func(a[i]).
array.map = function (A, func)
   local res, v = array:new(Ver.move(A.size, 1, #A.size, 1, {}))
   for i = 1, array._capacity_(A) do
      v = func(A[i] or 0)                            -- elements can be empty
      if v ~= 0 then res[i] = v end                  -- save nonzero values
   end
   return res
end
array.about[array.map] = {"map(A,func)", "Apply function of 1 argument. Return new array.", help.OTHER}

--- A1 + A2
--  @param A1 First array.
--  @param A2 Second array.
--  @return Array with elementwise sum.
array.__add = function (A1, A2)
   return array.apply2(array._add_, A1, A2)
end

--- A1 - A2
--  @param A1 First array.
--  @param A2 Second array.
--  @return Array with elementwise difference.
array.__sub = function (A1, A2)
   return array.apply2(array._sub_, A1, A2)
end

--- -A
--  @param A Array.
--  @return Array with inverted sign.
array.__unm = function (A)
   return array.map(A, array._unm_)
end

--- A1 * A2
--  @param A1 First array.
--  @param A2 Second array.
--  @return Array with element wise product.
array.__mul = function (A1, A2)
   return array.apply2(array._mul_, A1, A2)
end

--- A1 / A2
--  @param A1 First array.
--  @param A2 Second array.
--  @return Array with element wise ratio.
array.__div = function (A1, A2)
   return array.apply2(array._div_, A1, A2)
end

--- A1 ^ A2
--  @param A1 First array.
--  @param A2 Second array.
--  @return Array with element wise power.
array.__pow = function (A1, A2)
   return array.apply2(array._pow_, A1, A2)
end

array.arithmetic = 'arithmetic'
array.about[array.arithmetic] = {array.arithmetic, "a+b, a-b, a*b, a/b, -a, a^b", help.META}

--- Random array generator.
--  @param tSize Size table.
--  @return Array of the given size with random numbers from 0 to 1.
array.rand = function (tSize)
   local arr = array:new(tSize)
   for i = 1, array._capacity_(arr) do arr[i] = math.random() end
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
      for i = 1, array._capacity_(A1) do
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
array.dim = function (A)
   return Ver.move(A.size, 1, #A.size, 1, {})
end
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
   for count = 1, array._capacity_(res) do
      -- calculate new temporary index
      for i = 1, #ind do 
         local tmp = math.modf(count/K[i]) % S[i]
	 tInd2[i] = tInd1[i] + tmp
	 ind[i] = tmp + 1
      end
      res[array._indexConvert_(res,ind)] = A[array._indexConvert_(A,tInd2)]
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
   for count = 1, array._capacity_(res) do
      -- prepare index
      for i = 1, #newsize do
         ind1[i] = math.modf(count/K[i]) % S[i] + 1
	 ind2[i] = ind1[i]
      end
      -- get value
      local second = ind1[nAxis] > edge
      ind2[nAxis] = second and (ind2[nAxis]-edge) or ind2[nAxis]
      res[array._indexConvert_(res,ind1)] = second and A2[array._indexConvert_(A2,ind2)] or A1[array._indexConvert_(A1,ind2)]
   end
   --for i = 1, array._capacity_(arr2) do print(i, arr2[i]) end
   return res 
end
array.about[array.concat] = {"concat(A1,A2,nAxis)", "Array concatenation along the given axis."}

--- Method #
--  @param A Array object.
--  @return Number of elements.
array.__len = function (A)
   return array._capacity_(A)
end
array.about['#array'] = {"#array", "Return maximum number of elements.", help.META}

--- String representation.
--  @param A Array.
--  @return String representation of the array.
array.__tostring = function (A)
   return 'Array ' .. table.concat(A.size, 'x')
end

--- Get array slice.
--  Show sequence of 2D matrices with array elements.
--  @param A Array object.
--  @param nRow Number of axes for representation as rows.
--  @param nCol Number of axes for representation as columns.
--  @return String with all array elements slice by slice.
array.fullString = function (A, nRow, nCol)
   local res = {}
   local S = A.size
   -- 1 dimensional array
   if #S == 1 then
      for i = 1, S[1] do res[i] = A[i] or 0 end
      return table.concat(res, ' ')
   elseif #S == 2 then
      nRow = nRow or 1
      nCol = nCol or 2
   end
   assert(nRow > 0 and nRow <= #S and nCol > 0 and nCol <= #S and nRow ~= nCol, "Wrong indexes!")
   -- get 2D layer as string
   local function layer (ind)
            local row, col = {}, {}
	    for i = 1, S[nRow] do
	       ind[nRow] = i
	       -- get row
	       for j = 1, S[nCol] do
	          ind[nCol]  = j
		  col[j] = A[array._indexConvert_(A, ind)] or 0
	       end
	       -- save
	       row[i] = table.concat(col, ' ')
	    end
	    return table.concat(row, '\n')
         end
   -- prepare index calculation
   local bound, current, extent = {}, {}, {}
   local prod = 1
   for i = 1, #S do
      if i ~= nRow and i ~= nCol then 
         bound[#bound+1] = S[i] 
	 current[#current+1] = 0 
	 extent[#extent+1] = prod
	 prod = prod * S[i]
      end
   end
   local next_index = Ver.move(S, 1, #S, 1, {})

   for counter = 0, prod-1 do
      -- find coefficients
      for i = #current,1,-1 do current[i] = math.modf(counter/extent[i]) % bound[i] end
      -- write
      local k = #current
      for i = #next_index, 1, -1 do
         if     i == nRow then next_index[i] = 'R'
	 elseif i == nCol then next_index[i] = 'C'
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
array.about[array.fullString] = {"fullString(A,nRow,nCol)", "Represent array as sequence of matrices, where r and c are numbers of axes.", help.OTHER}

-- Constructor
setmetatable(array, {__call = function (self, tSize) 
   -- check correctness
   assert(type(tSize) == 'table', "Table is expected!")
   for i = 1,#tSize do assert(tSize[i] > 0 and Ver.isInteger(tSize[i]), "Positive integer is expected!") end
   -- build
   return array:new(tSize) 
end})
array.Arr = 'Arr'
array.about[array.Arr] = {"Arr(tSize)", "Create empty array with given size, represented as a table.", help.NEW}

--[[
-- Array serialization.
array.serialize = function (A)
   local s = {}
   s[#s+1] = 'size={' .. table.concat(A.size, ',') .. '}'
   s[#s+1] = 'k={' .. table.concat(A.k, ',') .. '}'
   for i = 1, array._capacity_(A) do
      if A[i] then
         s[#s+1] = string.format("[%d]=%s", i, (type(A[i]) == 'string' and "'"..A[i].."'" or A[i]))
      end
   end
   s[#s+1] = "metatablename='Arr'"
   s[#s+1] = "modulename='array'"
   return string.format("{%s}", table.concat(s, ','))
end
array.about[array.serialize] = {"serialize(A)", "String representation of array internal structure.", help.OTHER}
]]

--- Iterator across the array.
--  @param A Array object.
--  @return Iterator which calculate index of the next array element and the element itself, <code>nil</code> at the end.
array.next = function (A)
   local a = array:new(Ver.move(A.size, 1, #A.size, 1, {})) -- copy size
   local count, S, K, len = 0, a.size, a.k, array._capacity_(a)

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
if not lc_version then array.about = nil end

return array
