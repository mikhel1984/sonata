--[[     matrix.lua
Matrix operations.

------------- Examples ---------------

Mat = require 'liblc.matrix'

a = Mat({1,2},{3,4})             --> [1,2; 3,4]
b = Mat({5,6},{7,8})             --> [5,6; 7,8]

Indexation from 1!

a(1,1)                           --> 1
b:set(9, 1,1)                    --> [5,6; 9,8]
a:transpose() or a:T()           --> [1,3; 2,4]
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
a:sub(1,1,2,-1)                  --> [3,4]

a:map(function(x) return x^2 end) --> [1,4; 9,16]

This file is a part of liblc collection. 
Stanislav Mikhel, 2017.
]]

local matrix = {}
matrix.__index = matrix

matrix.type = 'matrix'

local help = require "liblc.help"
matrix.about = help:new("Matrix operations. The matrixes are spares by default.")

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
         r, c = r, 1
      elseif m.rows == 1 then 
         r, c = 1, r
      end
   end
   assert(c <= m.cols and c >= -m.cols, "Column number must be not more then " .. m.cols)
   assert(r <= m.rows and r >= -m.rows, "Row number must be not more then " .. m.rows)
   assert(r ~= 0 and c ~= 0, "Indexation from 1!")
   r = (r < 0) and (m.rows+r+1) or r
   c = (c < 0) and (m.cols+c+1) or c
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
matrix.about[matrix.get] = {"get(m,row,col)", "Return matrix element.", help.BASE}

-- simplify call of matrix.get()
matrix.__call = function (m,r,c) return matrix.get(m,r,c) end

-- set value of matrix
matrix.set = function (m, val, r, c)
   r, c = checkindex(m, r, c)
   m[r] = m[r] or {}
   m[r][c] = val
end
matrix.about[matrix.set] = {"set(m,val,row,col)", "Set value of matrix.", help.BASE} 

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
matrix.about[matrix.transpose] = {"transpose(m)", "Return matrix transpose. Shorten form is T().", help.BASE}

matrix.T = matrix.transpose

-- auxiliary function for addition/substraction
local function sum(a,b,sign)
   assert(a.cols == b.cols and a.rows == b.rows, "Wrong matrix size!")
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

-- return size
matrix.size = function (m)
   assert(ismatrix(m), "Matrix is expected")
   return m.rows, m.cols
end
matrix.about[matrix.size] = {"size(m)", "Return number or rows and columns. Can be called with '#'.", help.BASE}

matrix.map = function (m, fn) 
   local res = matrix:init(m.rows, m.cols)
   for r = 1, res.rows do
      for c = 1, res.cols do setval(res,r,c, fn(getval(m,r,c))) end
   end
   return res
end
matrix.about[matrix.map] = {"map(m,fn)", "Apply the given function to all elements, return new matrix.", help.OTHER}

matrix.map_ex = function (m, fn)
   local res = matrix:init(m.rows, m.cols)
   for r = 1, res.rows do
      for c = 1, res.cols do setval(res,r,c, fn(r,c,getval(m,r,c))) end
   end
   return res
end
matrix.about[matrix.map_ex] = {"map_ex(m,fn)", "Applay function fn(row,col,val) to all elements, return new matrix.", help.OTHER}

matrix.__len = matrix.size

-- create copy of matrix
matrix.copy = function (m)
   assert(ismatrix(m), "Matrix is expected!")
   local res = matrix:init(m.rows, m.cols)
   for r = 1, res.rows do
      for c = 1, res.cols do setval(res,r,c, getval(m,r,c)) end
   end
   return res
end
matrix.about[matrix.copy] = {"copy(m)", "Return copy of matrix.", help.OTHER}

-- a * b
matrix.__mul = function (a,b)
   assert(ismatrix(a) or ismatrix(b), "Wrong argemnt type")
   if not ismatrix(a) then return kprod(a, b) end
   if not ismatrix(b) then return kprod(b, a) end
   assert(a.cols == b.rows, "Impossible to get product: different size!")
   local res = matrix:init(a.rows, b.cols)
   for r = 1, res.rows do
      for c = 1, res.cols do
         local sum = 0
	 for i = 1, a.cols do sum = sum + getval(a,r,i)*getval(b,i,c) end
	 setval(res,r,c, sum)
      end
   end
   return (res.cols == 1 and res.rows == 1) and getval(res, 1, 1) or res
end

-- a / b
matrix.__div = function (a,b)
   assert(ismatrix(a) or ismatrix(b), "Wrong argemnt type!")
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

matrix.arithmetic = 'arithmetic'
matrix.about[matrix.arithmetic] = {matrix.arithmetic, "a+b, a-b, a*b, a/b, a^b, -a", help.BASE}

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

matrix.comparation = 'comparation'
matrix.about[matrix.comparation] = {matrix.comparation, "a==b, a~=b", help.BASE}


-- determinant
matrix.det = function (m)
   assert(m.rows == m.cols, "Square matrix is expected!")
   local _, K = gaussdown(matrix.copy(m))
   return K
end
matrix.about[matrix.det] = {"det(m)", "Calculate determinant.", help.BASE}

-- inverse matrix
matrix.inv = function (m)
   assert(m.rows == m.cols, "Square matrix is expected!")
   local con, det = matrix.rref(m, matrix.eye(m.cols))
   return (det ~= 0) and matrix.sub(con, 1,-1, m.cols+1, -1) or matrix.ones(m.rows,m.rows,math.huge) 
end
matrix.about[matrix.inv] = {"inv(m)", "Return inverse matrix.", help.BASE}

-- calculate system of equations using Gauss method
matrix.rref = function (A,b)
   local tr, d = gaussdown(matrix.concat(A,b,'h'))
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
matrix.about[matrix.sub] = {"sub(m,r1,r2,c1,c2)", "Return submatrix with rows [r1;r2] and columns [c1;c2]", help.OTHER}

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
matrix.about[matrix.concat] = {"concat(m1,m2,dir)", 
                               "Concatenate two matrix, dir='h' - in horizontal direction, dir='v' - in vertical\nUse m1 .. m2 for horizontal concatenation and m1 // m2 for vertical", 
			       help.OTHER}

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

--> SVD - "standard" algorithm implementation
--  BUT: some of singular values are negative :(

-- Householder transformation
local function householder (x, k)
   local n, sum = x.rows, 0
   local u = matrix.zeros(n, 1)
   for r = k,n do sum = sum + getval(x, r, 1)^2 end
   local tmp = getval(x,k,1)
   setval(u,k,1, tmp + math.sqrt(sum)*(tmp < 0 and -1 or (tmp > 0 and 1 or 0)))  
   for r = k+1,n do setval(u,r,1, getval(x,r,1)) end
   local ut = matrix.transpose(u)
   return matrix.eye(n,n) - (2 / (ut * u)) * (u * ut)
end

-- bidiagonalization
local function bidiag_reduction(A)
   local m,n = A.rows, A.cols
   local B = matrix.copy(A)
   local U, V = matrix.eye(m), matrix.eye(n)
   local M = math.min(m,n)
   for k = 1, M do
      local H1 = householder(B:sub(1,-1, k,k), k)  
      B = H1 * B
      U = U * matrix.transpose(H1)
      if k < (M-1) then
         local H2 = householder(matrix.transpose(B:sub(k, k, 1, -1)), k+1)
	 H2 = matrix.transpose(H2)
	 B = B * H2
	 V = V * H2
      end
   end
   return U, B, V
end

-- Given's rotation
local function rot(f,g)
   --> return cos, sin; r is omited
   if f == 0 then
      return 0, 1
   elseif math.abs(f) > math.abs(g) then
      local t = g / f
      local t1 = math.sqrt(1 + t*t)
      return 1/t1, t/t1
   else
      local t = f / g
      local t1 = math.sqrt(1 + t*t)
      return t/t1, 1/t1
   end
end

-- QR-type sweeps
local function qr_sweep(B)
   local m,n = B.rows, B.cols
   local U,V = matrix.eye(m), matrix.eye(n)
   local M = math.min(m,n)
   local threshould = function (x) return math.abs(x) > 1e-13 and x or 0 end   
   for k = 1, M-1 do
      -- first
      local c,s = rot(getval(B,k,k), getval(B,k,k+1))
      local Q = matrix.eye(n)
      setval(Q, k,   k,  c); setval(Q, k,   k+1, s)
      setval(Q, k+1, k, -s); setval(Q, k+1, k+1, c)
      Q = matrix.transpose(Q)
      B = B * Q
      V = V * Q
      B = matrix.map(B, threshould)
      -- second
      c,s = rot(getval(B,k,k), getval(B,k+1,k))
      Q = matrix.eye(m)
      setval(Q, k,   k,  c); setval(Q, k,   k+1, s)
      setval(Q, k+1, k, -s); setval(Q, k+1, k+1, c)
      B = Q * B
      U = U * matrix.transpose(Q)
      B = matrix.map(B, threshould)
   end
   -- add diagonal to zero matrix
   local I = matrix.map_ex(matrix.zeros(m,n),
                            function (r,c,x) return (c == (r+1) and c <= M and r <= M) and 1 or 0 end)
   -- error - norm
   local E = 0
   for r = 1, m do
      for c = 1, n do E = E + math.abs(getval(I,r,c)*getval(B,r,c)) end
   end
   return U, B, V, E
end

-- "standard" SVD calculation
matrix.svd = function (A)
   local transp = (A.rows < A.cols)
   local B = transp and A:transpose() or A:copy()
   local U1, U2, U3, V1, V2, V3
   U1, B, V1 = bidiag_reduction(B);
   U2 = matrix.eye(matrix.size(U1))
   V2 = matrix.eye(matrix.size(V1))
   local E = math.huge
   while E > 1e-8 do
      U3, B, V3, E = qr_sweep(B)
      U2 = U2 * U3
      V2 = V2 * V3
   end
   U1, V1 = U1*U2, V1*V2
   if transp then 
      U1, B, V1 = V1, B:transpose(), U1
   end
   return U1, B, V1
end
matrix.about[matrix.svd] = {"svd(M)", "Singular value decomposition of the matrix M.", help.OTHER}

-- pseudoinverse matrix
matrix.pinv = function (M)
   local u,s,v = matrix.svd(M)
   s = matrix.map_ex(s, function (r,c,x) return (r == c and math.abs(x) > 1e-8) and (1/x) or 0 end)
   return v * s:transpose() * u:transpose()
end
matrix.about[matrix.pinv] = {"pinv(M)", "Calculates pseudoinverse matrix using SVD", help.OTHER}

-- quick pseudoinverse matrix
-- based on "Fast computation of Moore-Penrose inverse matrices" by Pierre Courrieu
matrix.pinv2 = function (M)
   local m,n,transp = M.rows, M.cols, false
   local A, Mt = nil, M:transpose()
   if m < n then 
      A, n, transp = M * Mt, m, true
   else
      A = Mt * M
   end
   local tol, v = math.huge, 0
   for i = 1, A.rows do v = getval(A,i,i); tol = math.min(tol, (v > 0 and v or math.huge)) end
   tol = tol * 1e-9
   local L, r = matrix.zeros(A:size()), 0
   for k = 1, n do
      r = r + 1
      local B = A:sub(k,n,k,k)
      if r > 1 then 
         local tmp = L:sub(k,n,1,r-1) * L:sub(k,k,1,r-1):transpose() 
	 tmp = ismatrix(tmp) and tmp or matrix.new({tmp})         -- product can return a number
         B = B - tmp
      end
      for i = k, n do setval(L,i,r, getval(B,i-k+1,1)) end   -- copy B to L
      tmp = getval(L,k,r)
      if tmp > tol then
         setval(L,k,r, math.sqrt(tmp))
         tmp = getval(L,k,r)
	 if k < n then
	    for i = k+1, n do setval(L, i, r, getval(L,i,r)/tmp) end
	 end
      else
         r = r - 1
      end
   end
   L = L:sub(1,-1, 1, (r > 0 and r or 1))
   local Lt = L:transpose()
   local K = matrix.inv(Lt * L)
   if transp then
      return Mt * L * K * K * Lt
   end
   return L * K * K * Lt * Mt
end
matrix.about[matrix.pinv2] = {"pinv2(M)", "More quick function for pseudoinverse matrix calculation", help.OTHER}

-- constructor call
setmetatable(matrix, {__call = function (self,...) return matrix.new(...) end})
matrix.Mat = 'Mat'
matrix.about[matrix.Mat] = {"Mat(...)", "Create matrix from list of strings (tables)", help.NEW}

-- matrix serialization
matrix.serialize = function (obj)
   local s = {}
   s[#s+1] = "cols=" .. obj.cols
   s[#s+1] = "rows=" .. obj.rows
   for r = 1, obj.rows do
      local row = obj[r]
      if row and #row > 0 then
	 local tmp = {}
	 for c = 1, obj.cols do
	    if row[c] then tmp[#tmp+1] = string.format("[%d]=%a", c, row[c]) end
	 end
	 s[#s+1] = string.format("[%d]={%s}", r, table.concat(tmp, ','))
      end
   end
   s[#s+1] = "metatablename='Mat'"
   s[#s+1] = "modulename='matrix'"
   return string.format("{%s}", table.concat(s, ','))
end
matrix.about[matrix.serialize] = {"serialize(obj)", "Save matrix internal representation", help.OTHER}

return matrix
