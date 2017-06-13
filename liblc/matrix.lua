--[[     matrix.lua
Matrix operations.

------------- Examples ---------------

Mat = require 'liblc.matrix'

a = Mat({1,2},{3,4})             --> [1,2; 3,4]
b = Mat({5,6},{7,8})             --> [5,6; 7,8]

In functions such as 'get', 'set', 'sub' indexation 
starts from 0.

a(0,0)                           --> 1
b:set(9, 1,0)                    --> [5,6; 9,8]
a:transpose()                    --> [1,3; 2,4]
#a                               --> 2,2

a + b                            --> [6,8; 10,12]
b - a                            --> [4,4; 4,4]
a * b
a / b
a ^ 2 

a:det()
a:inv()
c = a:copy()                     --> [1,2; 3,4]

Mat.eye(2)                       --> [1,0; 0,1]
Mat.zeros(2,3)                   --> [0,0,0; 0,0,0]
Mat.ones(2,3, 4)                 --> [4,4,4; 4,4,4]

a .. b                           --> [1,2,5,6; 3,4,7,8]
a // b                           --> [1,2; 3,4; 5,6; 7,8]
a:sub(0,1,1,-1)                  --> [3,4]

a:map(function(x) return x^2 end) --> [1,4; 9,16]

This file is a part of liblc collection. 
Stanislav Mikhel, 2017.
]]

local matrix = {}
matrix.__index = matrix

matrix.type = 'matrix'

local help = require "liblc.help"
matrix.about = help:new("Matrix operations. Indexation from 0!")

-- test object type
local function ismatrix(m) return type(m) == 'table' and m.type == matrix.type end

-- initialization of matrix with given size
function matrix:init(r, c, m)
   assert(r > 0 and c > 0, "Wrong matrix size")
   m = m or {}
   m.cols, m.rows = c, r
   setmetatable(m, self)
   return m
end

-- create new matrix from list of tables
function matrix.new(...)
   local args = {...}
   local cols, rows = 0, #args
   for i = 1, rows do
      assert(type(args[i]) == 'table', "Row must be a table!")
      cols = (cols < #args[i]) and #args[i] or cols
   end
   return matrix:init(rows, cols, args)
end

-- check correctness of element index
local function checkindex(m, r, c)
   assert(ismatrix(m), "Matrix is expected")
   if c == nil then
      if m.cols == 1 then 
         r, c = r, 0
      elseif m.rows == 1 then 
         r, c = 0, r
      end
   end
   assert(c < m.cols and c > -m.cols, "Column number must be less then " .. m.cols)
   assert(r < m.rows and r > -m.rows, "Row number must be less then " .. m.rows)
   r = (r < 0) and (m.rows+r+1) or (r+1)
   c = (c < 0) and (m.cols+c+1) or (c+1)
   return r, c
end

-- return 0 in element is empty or its value
local function getval(m, r, c)
   local v = m[r]
   if v then 
      return v[c] or 0
   else
      return 0
   end
end

-- set value if need
local function setval(m, r, c, v)
   if (m[r] and m[r][c]) or v ~= 0 then 
      m[r] = m[r] or {}
      m[r][c] = v
   end
end

-- set product of element to coefficient
local function kprod(k, m)
   local res = matrix:init(m.rows, m.cols)
   for r = 1, m.rows do
      for c = 1, m.cols do
      	 setval(res,r,c, k*getval(m,r,c))
      end
   end
   return res
end

-- transform matrix to upper triangle
local function gaussdown(m)
   local A = 1
   for k = 1, m.rows do
      -- look for nonzero element
      local i = k+1
      while getval(m,k,k) == 0 and i <= m.rows do
         if getval(m,i,k) ~= 0 then m[i],m[k],A = m[k],m[i],-A end
	 i = i+1
      end
      local coef = getval(m,k,k)
      A = A * coef
      if coef ~= 0 then
         -- normalization
	 coef = 1/coef
         for c = k, m.cols do setval(m,k,c, getval(m,k,c)*coef) end
         -- substraction
         for r = (k+1), m.rows do
            local v = getval(m, r, k)
	    if v ~= 0 then
               for c = k, m.cols do setval(m,r,c, getval(m,r,c)-v*getval(m,k,c)) end
	    end
         end
      end
   end
   return m, A
end

-- transform triangle matrix to identity matrix
local function gaussup(m)
   for k = m.rows-1, 1, -1 do
      local v = getval(m, k, k+1)
      if v ~= 0 then
         for c = k+1, m.cols do setval(m,k,c, getval(m,k,c)-v*getval(m,k+1,c)) end 
      end
   end
   return m
end

-- return matrix element
matrix.get = function (m, r, c)
   r, c = checkindex(m, r, c)
   return getval(m, r, c)
end
matrix.about[matrix.get] = {"get(m,raw,col)", "Return matrix element", help.BASE}

-- simplify call of matrix.get()
matrix.__call = function (m,r,c) return matrix.get(m,r,c) end

-- set value of matrix
matrix.set = function (m, val, r, c)
   r, c = checkindex(m, r, c)
   m[r] = m[r] or {}
   m[r][c] = val
end
matrix.about[matrix.set] = {"set(m,val,raw,col)", "Set value of matrix", help.BASE} 

-- transpose matrix
matrix.transpose = function (m)
   local res = matrix:init(m.cols, m.rows)
   for r = 1, m.rows do
      for c = 1, m.cols do
	 setval(res,c,r, getval(m,r,c))
      end
   end
   return res
end
matrix.about[matrix.transpose] = {"transpose(m)", "Return matrnix transpose", help.BASE}

-- auxiliary function for addition/substraction
local function sum(a,b,sign)
   assert(a.cols == b.cols and a.rows == b.rows, "Wrong matrix size")
   local res = matrix:init(a.rows, a.cols)
   for r = 1, res.rows do
      for c = 1, res.cols do
         local sum = (sign == '-') and (getval(a,r,c)-getval(b,r,c)) or (getval(a,r,c)+getval(b,r,c))
	 setval(res,r,c, sum)
      end
   end
   return res
end

-- a + b
matrix.__add = function (a,b)
   return sum(a, b)
end

-- a - b
matrix.__sub = function (a,b)
   return sum(a, b, '-')
end

-- -a
matrix.__unm = function (a)
   local tmp = matrix:init(a.rows, a.cols)
   return sum(tmp, a, '-')
end

--[[
-- return size
matrix.size = function (m)
   assert(ismatrix(m), "Matrix is expected")
   return m.rows, m.cols
end
matrix.about[matrix.size] = {"size(m)", "Return number or rows and columns. Can be called with '#'", help.BASE}
]]

matrix.map = function (m, fn) 
   local res = matrix:init(m.rows, m.cols)
   for r = 1, res.rows do
      for c = 1, res.cols do setval(res,r,c, fn(getval(m,r,c))) end
   end
   return res
end
matrix.about[matrix.map] = {"map(m,func)", "Apply the given function to all elements, return new matrix", help.OTHER}

-- redefine size call
matrix.__len = function (m)
   return m.rows, m.cols
end

-- create copy of matrix
matrix.copy = function (m)
   assert(ismatrix(m), "Matrix is expected!")
   local res = matrix:init(m.rows, m.cols)
   for r = 1, res.rows do
      for c = 1, res.cols do setval(res,r,c, getval(m,r,c)) end
   end
   return res
end
matrix.about[matrix.copy] = {"copy(m)", "Return copy of matrix", help.OTHER}

-- a * b
matrix.__mul = function (a,b)
   assert(ismatrix(a) or ismatrix(b), "Wrong argemnt type")
   if not ismatrix(a) then return kprod(a, b) end
   if not ismatrix(b) then return kprod(b, a) end
   assert(a.cols == b.rows, "Impossible to get product: different size")
   local res = matrix:init(a.rows, b.cols)
   for r = 1, res.rows do
      for c = 1, res.cols do
         local sum = 0
	 for i = 1, a.cols do sum = sum + getval(a,r,i)*getval(b,i,c) end
	 setval(res,r,c, sum)
      end
   end
   return res
end

-- a / b
matrix.__div = function (a,b)
   assert(ismatrix(a) or ismatrix(b), "Wrong argemnt type")
   if not ismatrix(b) then return kprod(1/b, a) end
   return a * matrix.inv(b)
end

-- a ^ n
matrix.__pow = function (a,n)
   n = assert(math.tointeger(n), "Integer is expected!")
   assert(a.rows == a.cols, "Square matrix is expected!")
   local res, acc = matrix.eye(a.rows), matrix.copy(a)
   while n > 0 do
      if n%2 == 1 then res = res * acc end
      acc = acc * acc
      n = n // 2
   end
   return res
end
matrix.about["arithmetic"] = {"arithmetic", "a+b, a-b, a*b, a/b, a^b, -a", help.BASE}

-- a == b
matrix.__eq = function (a,b)
   if not (ismatrix(a) and ismatrix(b)) then return false end
   if a.rows ~= b.rows or a.cols ~= b.cols then return false end
   for r = 1, a.rows do
      for c = 1, a.cols do
         if getval(a,r,c) ~= getval(b,r,c) then return false end
      end
   end
   return true
end
matrix.about["comp"] = {"comparation", "a==b, a~=b", help.BASE}


-- determinant
matrix.det = function (m)
   assert(m.rows == m.cols, "Square matrix is expected!")
   local _, K = gaussdown(matrix.copy(m))
   return K
end
matrix.about[matrix.det] = {"det(m)", "Calculate determinant", help.BASE}

-- inverse matrix
matrix.inv = function (m)
   assert(m.rows == m.cols, "Square matrix is expected!")
   local con, det = matrix.concat(m, matrix.eye(m.cols),'h'), 0
   con, det = matrix.rref(con)
   return (det ~= 0) and matrix.sub(con, 0,-1, m.cols, -1) or matrix.ones(m.rows,m.rows,math.huge)  -- indexation from 0
end
matrix.about[matrix.inv] = {"inv(m)", "Return inverse matrix", help.BASE}

-- calculate system of equations using Gauss method
matrix.rref = function (m)
   local tr, d = gaussdown(matrix.copy(m))
   return gaussup(tr), d
end
matrix.about[matrix.rref] = {"rref(m)", "Perform transformations using Gauss method. Return also determinant.", help.BASE}

-- create vector
matrix.vector = function (...)
   local v, res = {...}, {}
   for i = 1, #v do res[i] = {v[i]} end
   return matrix:init(#v, 1, res)
end
matrix.about[matrix.vector] = {"vector(...)", "Create vector from list of numbers", help.BASE}

-- matrix of 0
matrix.zeros = function (rows, cols)
   cols = cols or rows
   return matrix:init(rows, cols)
end
matrix.about[matrix.vector] = {"zeros(rows[,cols])", "Create matrix from zeros", help.OTHER}

-- matrix of 1
matrix.ones = function (rows, cols, val)
   cols = cols or rows
   val = val or 1
   local m = matrix:init(rows, cols)
   for r = 1, rows do
      m[r] = {}
      for c = 1, cols do m[r][c] = val end
   end
   return m
end
matrix.about[matrix.ones] = {"ones(rows[,cols[,val]])", "Create matrix of given numbers (default is 1).", help.OTHER}

-- matrix with random values
matrix.rand = function (rows, cols)
   cols = cols or rows
   local m = matrix:init(rows, cols)
   for r = 1, rows do
      m[r] = {}
      for c = 1, cols do
         m[r][c] = math.random()
      end
   end
   return m
end
matrix.about[matrix.rand] = {"rand(rows[,cols])", "Create matrix with random numbers from 0 to 1", help.OTHER}

-- identity matrix
matrix.eye = function (rows, cols)
   cols = cols or rows
   local m = matrix:init(rows, cols)
   for i = 1, math.min(rows, cols) do setval(m,i,i, 1) end
   return m
end
matrix.about[matrix.eye] = {"eye(rows[,cols])", "Create identity matrix", help.OTHER}

-- get submatrix
matrix.sub = function (m, r1, r2, c1, c2)
   r1, c1 = checkindex(m, r1, c1)
   r2, c2 = checkindex(m, r2, c2)
   local res = matrix:init(r2-r1+1, c2-c1+1)
   local i, j = 1, 1
   for r = r1, r2 do
      for c = c1, c2 do
	 setval(res,i,j, getval(m,r,c))
	 j = j+1
      end
      j = 1
      i = i+1
   end
   return res
end
matrix.about[matrix.sub] = {"sub(m, r1, r2, c1, c2)", "Return submatrix with rows [r1;r2] and columns [c1;c2]", help.OTHER}

-- perform concatenation
matrix.concat = function (a, b, dir)
   local res = nil
   if dir == 'h' then
      assert(a.rows == b.rows, "Different number of rows")
      res = matrix:init(a.rows, a.cols+b.cols)
   elseif dir == 'v' then
      assert(a.cols == b.cols, "Different number of columns")
      res = matrix:init(a.rows+b.rows, a.cols)
   else
      error("Unexpected type of concatenation")
   end
   for r = 1, res.rows do
      for c = 1, res.cols do
         local src = (r <= a.rows and c <= a.cols) and a or b
         local i = (r <= a.rows) and r or (r - a.rows)
	 local j = (c <= a.cols) and c or (c - a.cols)
	 setval(res,r,c, getval(src,i,j))
      end
   end
   return res
end
matrix.about[matrix.concat] = {"concat(m1, m2, dir)", "Concatenate two matrix, dir='h' - in horizontal direction, dir='v' - in vertical\nUse m1 .. m2 for horizontal concatenation and m1 // m2 for vertical", help.OTHER}

-- horizontal concatenation
matrix.__concat = function (a,b)
   return matrix.concat(a,b,'h')
end

-- vertical concatenation
matrix.__idiv = function (a,b)
   return matrix.concat(a,b,'v')
end

-- strig representation
matrix.__tostring = function (m)
   local srow = {}
   for r = 1, m.rows do
      local scol = {}
      for c = 1, m.cols do
         table.insert(scol, getval(m, r, c))
      end
      table.insert(srow, table.concat(scol, "  "))
   end
   return table.concat(srow, "\n")
end

-- constructor call
setmetatable(matrix, {__call = function (self,...) return matrix.new(...) end})
matrix.about[help.NEW] = {"Mat(...)", "Create matrix from list of strings (tables)", help.NEW}

return matrix
