--[[      sonatalib/matrix.lua 

--- Matrix operations. Indexation from 1.
--
--  Object structure:                           </br>
--  <code>{{x11...x1n},{x21...x2n}...{xm1...xmn}</br>
--  rows=m, cols=n} </code></br>
--  Internal elements can be empty tables, but anyway operation X[i][j] will return 0.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">sonatalib</a> collection, 2017-2018.

            module 'matrix'
--]]

-------------------- Tests -------------------
--[[TEST

-- import 'matrix'
Mat = require 'sonatalib.matrix'

-- define matrix objects
a = Mat {{1,2},{3,4}}             
b = Mat {{5,6},{7,8}}  
-- call in typical way           
ans = a[2][2]                    --> 4 

b[1][1] = 5
-- transpose
c = a:T()
-- use () as alias for get()
ans = c(1,-1)                    --> 3

-- matrix rows and columns
_, ans = a:size()                --> 2

-- arithmetical operations
ans = a + b                      --> Mat {{6,8},{10,12}}

ans = b - a                      --> Mat {{4,4},{4,4}}

ans = a * b                      --> Mat {{19,22},{43,50}}

ans = a / b 
-- determinant
ans = ans:det()                  --~ 1

-- multiply to scalar
ans = 2 * a                      --> Mat {{2,4},{6,8}}

-- add scalar (to all elements)
ans = a - 1                      --> Mat {{0,1},{2,3}}

ans = a ^ 2                      --> Mat {{7,10},{15,22}} 

-- determinant
ans = a:det()                    --> -2

-- inverse matrix
e = a:inv()
ans = e(2,1)                     --> 1.5

-- another call of inversion
e = a^-1
ans = e(2,1)                     --> 1.5

-- object copy
f = a:copy()
ans = (f == a)                   --> true

-- element-wise comparison
ans = (a == b)                   --> false

-- identity matrix
ans = Mat.eye(2)                 --> Mat {{1,0},{0,1}}

-- matrix argument
ans = Mat.eye(a)                 --> Mat {{1,0},{0,1}}

-- matrix of zeros
ans = Mat.zeros(2,1)             --> Mat {{0},{0}}

-- matrix of constants = 4
ans = Mat.ones(2,3,4)            --> Mat {{4,4,4},{4,4,4}}

-- matrix of constants = 1
ans = Mat.ones(a,3)              --> Mat {{3,3},{3,3}}

-- define rule to fill
-- result matrix is 'dense'
fn = function (i,j) return i == j and 1 or 0 end
ans = Mat.fill(2,3,fn)           --> Mat {{1,0,0},{0,1,0}}

-- horizontal concatenation
ans = a .. b                     --> Mat {{1,2,5,6},{3,4,7,8}}

-- vertical concatenation
-- (a // b - for short)
ans = a:concat(b,'v')            --> Mat {{1,2},{3,4},{5,6},{7,8}}

-- to triangular form
print(Mat.triang(a .. b))

-- apply function of 1 argument
ans = a:map(function (x) return x^2 end)          --> Mat {{1,4},{9,16}}

-- apply function which depends on index too
ans = a:map(function (x,r,c) return x-r-c end) --> Mat {{-1,-1},{-0,-0}}

-- apply function to matrices
-- element-wise
fn = function (x,y,z) return x*y+z end
aa = Mat.apply(fn, b,b,b) 
ans = aa[1][1]                    --> 30

-- use Gauss transform to solve equation
ans = Mat.rref(a, Mat {{5},{11}}) --> Mat {{1,0,1},{0,1,2}}

-- create vector
ans = Mat.V {1,2,3}              --> Mat {{1},{2},{3}}

-- get submatrix
g = Mat {{1,2,3},{4,5,6},{7,8,9}}
ans = g({2,-1},{2,3})           --> Mat {{5,6},{8,9}}

-- square norm along rows
gsq = g:sqNorm('r') 
ans = gsq[1][1]                 --> 14

-- eualedian norm
ans = Mat.V({1,2,3}):norm()     --~ math.sqrt(14)

-- random matrix
h = Mat.rand(3,2)
print(h)

-- pseudo inverse matrix
m = Mat {{1,2},{3,4},{5,6}}
n = m:pinv()
ans = n(2,2)                    --~ 0.333

-- copy as Lua table
-- without methametods
k = Mat.eye(3)
k = k:table()
ans = k[2][1]                   --> 0

-- back to matrix
-- (import table)
k = Mat(k)
k = k:sparse()
ans = rawget(k[2],1)             --> nil

-- make diagonal matrix
ans = Mat.diag({1,2,3})         --> Mat {{1,0,0},{0,2,0},{0,0,3}}

-- shifted diagonal
ans = g:diag(1)                 --> Mat {{2},{6}}

-- cross-product of 2 vectors
x1 = Mat {{1,2,3}}
x2 = Mat {{4,5,6}}
ans = Mat.cross(x1,x2)          --> Mat {{-3},{6},{-3}}

-- dot product of 2 vectors
ans = Mat.dot(x1,x2)            --> 32

-- LU transform
l,u,p = b:lu()
ans = l[2][1]                   --~ 0.714

-- Cholesky decomposition
m = Mat {{3,1},{1,3}}
m = m:Cholesky()
ans = m[2][2]                   --~ 1.633

-- matrix trace
ans = a:tr()                    --> 5

-- extract first row
m = a({},1)
-- vector doesn't need in 2 indices
ans = m(1)                      --> 1

-- extract last column
-- index can be negative 
m = a(-1,{})
ans = m:get(2)                  --> 4

-- apply summation to each row
ans = a:sum()                   --> Mat {{3},{7}}

-- apply product to each column
-- initial value is 1
ans = a:reduce(function (x,y) return x*y end, 'c', 1) --> Mat {{3,8}}

-- get rank
ans = Mat.ones(2,3):rank()      --> 1
--]]

--	LOCAL

-- to accelerate calculations
local fn_sum = function (x,y) return x+y end
local fn_sub = function (x,y) return x-y end
local fn_unm = function (x) return -x end

-- compatibility
local Ver = require "sonatalib.versions"

-- Metatable for new rows.
local access = {
   -- 0 instead nil
   __index = function () return 0 end,
   -- comment this function in order to work a little bit faster, but in this case matrix can become dense
   __newindex = function (t,k,v) if v ~= 0 then rawset(t,k,v) end end,
}

--- Check object type.
--  @param m Object to check.
--  @return True if the object is 'matrix'.
local function ismatrix(m) return type(m) == 'table' and m.ismatrix end

--- Correct range if possible.
--  @param ind Positive or negative index value.
--  @param nRange Available range of indexes. 
--  @return Corrected index or nil.
local function toRange(ind,nRange)
   if ind < 0 then ind = ind + nRange + 1 end
   if ind <= 0 or ind > nRange then return nil end
   return ind
end

local TRANSFORM = 'transform'

--	INFO
local help = lc_version and (require "sonatalib.help") or {new=function () return {} end}

--	MODULE

local matrix = {
-- mark object
type = 'matrix', ismatrix = true,
-- description
about = help:new("Matrix operations. The matrices are spares by default."),
}

--- Metametod for access to elements.
--  @param t Table (object).
--  @param k Key.
--  @return New matrix row or desired method.
matrix.__index = function (t,k) 
   if type(k) == 'number' then
      -- new element
      local tmp = setmetatable({}, access)
      t[k] = tmp
      return tmp
   else
      -- matrix methods
      return matrix[k]
   end
end

--- Initialization of matrix with given size.
--  @param nR Number of rows.
--  @param nC Number of columns.
--  @param M Table for initialization.
--  @return Matrix object.
matrix.init = function (self, nR, nC, M)
   if nR <= 0 or nC <= 0 then error("Wrong matrix size!") end
   M.cols, M.rows = nC, nR
   return setmetatable(M, self)
end

--- Create new matrix from list of tables.
--  @param M Table, where each sub table is a raw of matrix.
--  @return Matrix object.
matrix.new = function (M)
   M = M or {}
   local cols, rows = 0, #M
   for i = 1, rows do
      if not type(M[i]) == 'table' then error('Row must be a table!') end
      cols = (cols < #M[i]) and #M[i] or cols
      setmetatable(M[i], access)
   end
   return matrix:init(rows, cols, M)
end

--- Set product of element to coefficient.
--  @param k Coefficient.
--  @param m Matrix.
--  @return Result of production.
matrix._kProd_ = function (k, M)
   local res = matrix:init(M.rows, M.cols, {})
   for r = 1, M.rows do
      local resr, mr = res[r], M[r]
      for c = 1, M.cols do resr[c] = k*mr[c] end
   end
   return res
end

--- Transform matrix to upper triangle (in-place).
--  @param M Initial matrix.
--  @return Upper triangulated matrix and determinant.
matrix._GaussDown_ = function (M)
   local A = 1
   for k = 1, M.rows do
      -- look for nonzero element
      local i = k+1
      while M[k][k] == 0 and i <= M.rows do
         if M[i][k] ~= 0 then M[i],M[k],A = M[k],M[i],-A end
	 i = i+1
      end
      local coef = M[k][k]
      A = A * coef
      if coef ~= 0 then
         -- normalization
	 coef = 1/coef
	 local mk = M[k]
         for c = k, M.cols do mk[c] = mk[c]*coef end
         -- subtraction
         for r = (k+1), M.rows do
	    local mr = M[r]
            local v = mr[k]
	    if v ~= 0 then
               for c = k, M.cols do mr[c] = mr[c]-v*mk[c] end
	    end -- if
         end -- for
      end -- if
   end -- for
   return M, A
end

--- Transform triangle matrix to identity matrix (in-place).
--  @param M Initial matrix
--  @return Matrix with diagonal zeros.
matrix._GaussUp_ = function (M)
   for k = M.rows, 1, -1 do
      local mk = M[k]
      for r = k-1,1,-1 do
         local mr = M[r]
         local v = mr[k]
         if v ~= 0 then
            for c = k, M.cols do mr[c] = mr[c]-v*mk[c] end 
         end -- if
      end -- for
   end -- for
   return M
end

--- Matrix triangulation.
--  @param M Initial matrix.
--  @return Triangulated matrix.
matrix.triang = function (M) return matrix._GaussDown_(matrix.copy(M)) end
matrix.about[matrix.triang] = {'triang(M)', 'Matrix triangulation produced by Gaussian elimination.', TRANSFORM}

--- Matrix rank.
--  @param M Initial matrix.
--  @return Value of rank.
matrix.rank = function (M)
   local mat,i = matrix.triang(M),1
   while i <= mat.rows do
      local mati = mat[i]
      if not mati then break end
      -- find nonzero element
      local zeros = true
      for j = i,mat.cols do
         if mati[j] ~= 0 then zeros = false; break end
      end
      if zeros then break end
      i = i+1
   end
   return i-1
end
matrix.about[matrix.rank] = {"rank(M)", "Find rank of the matrix."}

--- Get element or sub matrix. 
--  In case of sub matrix each index should be a table of 2 or 3 elements: [begin,end[,step]].
--  @param M Source matrix.
--  @param a Raw index or array of indexes.
--  @param b Column index or array of indexes. In case of vector can be omitted.
--  @return Element or sub array or nil in case of error.
matrix.get = function (M,a,b)
   if not b then
      -- vector call
      if M.rows == 1 then b,a = a,1 else b = 1 end
   end
   local numa = (type(a) == 'number')
   local numb = (type(b) == 'number')
   -- check range
   if numa then a = toRange(a, M.rows) end
   if numb then b = toRange(b, M.cols) end
   if not (a and b) then return nil end

   -- both are numbers
   if numa and numb then return M[a][b] end

   -- expected table
   if numa then
      a = {a,a,1}
   else
      a[1] = toRange(a[1] or 1, M.rows)
      a[2] = toRange(a[2] or M.rows, M.rows)
      a[3] = a[3] or 1
      if not (a[1] and a[2] and (a[2]-a[1])/a[3] >= 0) then return nil end
   end
   if numb then
      b = {b,b,1}
   else
      b[1] = toRange(b[1] or 1, M.cols)
      b[2] = toRange(b[2] or M.cols, M.cols)
      b[3] = b[3] or 1
      if not (b[1] and b[2] and (b[2]-b[1])/b[3] >= 0) then return nil end
   end

   -- fill matrix
   local res = matrix:init(math.floor((a[2]-a[1])/a[3])+1, math.floor((b[2]-b[1])/b[3])+1, {})
   local i = 0
   for r = a[1],a[2],a[3] do
      i = i+1
      local resi, mr = res[i], M[r]
      local j = 0
      for c = b[1],b[2],b[3] do
         j = j+1
         resi[j] = mr[c]
      end
   end
   return res
end


-- simplify call of matrix.get()
matrix.__call = function (m,r,c) return matrix.get(m,r,c) end

--- Transpose matrix.
--  Can be called as T().
--  @param M Initial matrix.
--  @return Transposed matrix.
matrix.transpose = function (M)
   local res = matrix:init(M.cols, M.rows, {})
   for r = 1, M.rows do
      local mr = M[r]
      for c = 1, M.cols do res[c][r] = mr[c] end
   end
   return res
end
matrix.about[matrix.transpose] = {"transpose(M)", "Return matrix transpose. Shorten form is T().", TRANSFORM}
matrix.T = matrix.transpose


--- M1 + M2
--  @param M1 First matrix or number.
--  @param M2 Second matrix or number.
--  @return Summ matrix.
matrix.__add = function (M1,M2)
   M1 = ismatrix(M1) and M1 or matrix.ones(M2.rows, M2.cols, M1)
   M2 = ismatrix(M2) and M2 or matrix.ones(M1.rows, M1.cols, M2)
   return matrix._apply2_(fn_sum,M1,M2)
end

--- M1 - M2
--  @param M1 First matrix or number.
--  @param M2 Second matrix or number.
--  @return Difference matrix.
matrix.__sub = function (M1,M2)
   M1 = ismatrix(M1) and M1 or matrix.ones(M2.rows, M2.cols, M1)
   M2 = ismatrix(M2) and M2 or matrix.ones(M1.rows, M1.cols, M2)
   return matrix._apply2_(fn_sub,M1,M2)
end

--- - M
--  @param M Matrix object.
--  @return Matrix where each element has opposite sign.
matrix.__unm = function (a) return matrix.map(a,fn_unm) end

--- Get matrix size.
--  @param M Matrix.
--  @return Number of rows and columns.
matrix.size = function (M) return M.rows, M.cols end
matrix.about[matrix.size] = {"size(M)", "Return number or rows and columns."}

--- Apply function to each element.
--  @param M Source matrix.
--  @param fn Desired function.
--  @return Matrix where each element is obtained based on desired function.
matrix.map = function (M, fn) 
   local res = matrix:init(M.rows, M.cols, {})
   for r = 1, res.rows do
      local resr, mr = res[r], M[r]
      for c = 1, res.cols do resr[c] = fn(mr[c],r,c) end
   end
   return res
end
matrix.about[matrix.map] = {"map(M,fn)", "Apply the given function to all elements, return new matrix. Function can be in form f(x) or f(x,row,col).", TRANSFORM}

--- Apply function to each pair elements of given matrices.
--  @param M1 First matrix.
--  @param M2 Second matrix.
--  @param fn Function from two arguments f(v1,v2).
--  @return Result of function evaluation.
matrix._apply2_ = function (fn, M1, M2)
   if (M1.rows~=M2.rows or M1.cols~=M2.cols) then error("Different matrix size!") end
   local res = matrix:init(M1.rows,M1.cols,{})
   for r = 1,res.rows do
      local resr, m1r, m2r = res[r], M1[r], M2[r]
      for c = 1,res.cols do resr[c] = fn(m1r[c], m2r[c]) end
   end
   return res
end

--- Apply function element-wise to matrices.
--  @param fn Function of several arguments.
--  @param ... List of matrices.
--  @return New found matrix.
matrix.apply = function (fn, ...)
   local arg = {...}
   local rows, cols = arg[1].rows, arg[1].cols
   -- check size
   for i = 2,#arg do
      if arg[i].rows ~= rows or arg[i].cols ~= cols then error("Different size!") end
   end
   local res, v = matrix:init(rows, cols, {}), {}
   -- evaluate
   for r = 1,res.rows do
      for c = 1,res.cols do
         -- collect
	 for k = 1,#arg do v[k] = arg[k][r][c] end
	 -- calc
	 res[r][c] = fn(Ver.unpack(v))
      end
   end
   return res
end
matrix.about[matrix.apply] = {'apply(fn,M1,M2,...)','Apply function to the given matrices element-wise.', TRANSFORM}

--- Create copy of matrix.
--  @param M Source matrix.
--  @return Deep copy.
matrix.copy = function (M)
   local res = matrix:init(M.rows,M.cols,{})
   for r = 1,M.rows do
      local resr, mr = res[r], M[r]
      for c = 1,M.cols do resr[c] = mr[c] end
   end
   return res
end
matrix.about[matrix.copy] = {"copy(M)", "Return copy of matrix.", help.OTHER}

--- M1 * M2
--  @param M1 First matrix or number.
--  @param M2 Second matrix or number.
--  @return Result of multiplication.
matrix.__mul = function (M1,M2)
   if not ismatrix(M1) then return matrix._kProd_(M1, M2) end
   if not ismatrix(M2) then return matrix._kProd_(M2, M1) end
   if (M1.cols ~= M2.rows) then error("Impossible to get product: different size!") end
   local res = matrix:init(M1.rows, M2.cols,{})
   local resCols, m1Cols = res.cols, M1.cols
   for r = 1, res.rows do
      local ar, resr = M1[r], res[r]
      for c = 1, resCols do
         local sum = 0
	 for i = 1, m1Cols do sum = sum + ar[i]*M2[i][c] end
	 resr[c] = sum
      end
   end
   return (res.cols == 1 and res.rows == 1) and res[1][1] or res
end

--- M1 / M2
--  @param M1 First matrix or number.
--  @param M2 Second matrix or number.
--  @return Matrix of ratio.
matrix.__div = function (M1,M2)
   if not ismatrix(M2) then return matrix._kProd_(1/M2, M1) end
   return matrix.__mul(M1, matrix.inv(M2))
end

--- M ^ n
--  @param M Square matrix.
--  @param n Natural power or -1.
--  @return Power of the matrix.
matrix.__pow = function (M,n)
   n = assert(Ver.toInteger(n), "Integer is expected!")
   if (M.rows ~= M.cols) then error("Square matrix is expected!") end
   if n == -1 then return matrix.inv(M) end
   local res, acc = matrix.eye(M.rows), matrix.copy(M)
   while n > 0 do
      if n%2 == 1 then res = matrix.__mul(res, acc) end
      n = math.modf(n*0.5)
      if n > 0 then acc = matrix.__mul(acc, acc) end
   end
   return res
end

matrix.arithmetic = 'arithmetic'
matrix.about[matrix.arithmetic] = {matrix.arithmetic, "a+b, a-b, a*b, a/b, a^b, -a", help.META}

--- M1 == M2
--  @param M1 First matrix.
--  @param M2 Second matrix.
--  @return True if all elements are the same.
matrix.__eq = function (M1,M2)
   if not (ismatrix(M1) and ismatrix(M2)) then return false end
   if M1.rows ~= M2.rows or M1.cols ~= M2.cols then return false end
   for r = 1, M1.rows do
      local ar, br = M1[r], M2[r]
      for c = 1, M1.cols do
         if ar[c] ~= br[c] then return false end
      end
   end
   return true
end

matrix.comparison = 'comparison'
matrix.about[matrix.comparison] = {matrix.comparison, "a==b, a~=b", help.META}

--- Find determinant.
--  @param M Initial matrix.
--  @return Determinant.
matrix.det = function (M)
   if (M.rows ~= M.cols) then error("Square matrix is expected!") end
   local _, K = matrix._GaussDown_(matrix.copy(M))
   return K
end
matrix.about[matrix.det] = {"det(M)", "Calculate determinant."}

--- Inverse matrix.
--  @param M Initial matrix.
--  @return Result of inversion.
matrix.inv = function (M)
   if (M.rows ~= M.cols) then error("Square matrix is expected!") end
   local size = M.cols
   -- prepare matrix
   local res, det = matrix.copy(M)
   -- add "tail"
   for i = 1,size do
      res[i][i+size] = 1
   end
   res.cols = 2*size
   res, det = matrix._GaussDown_(res)
   if det == 0 then 
      return matrix.ones(size,size, math.huge)
   end
   res = matrix._GaussUp_(res)
   -- move result
   for r = 1,size do
      local resr = res[r]
      for c = 1,size do
         local p = size+c
	 resr[c], resr[p] = resr[p], nil
      end
   end
   res.cols = size
   return res
end
matrix.about[matrix.inv] = {"inv(M)", "Return inverse matrix.", TRANSFORM}

--- Solve system of equations using Gauss method.
--  @param A Matrix of coefficients.
--  @param b Free coefficients (matrix of vector).
--  @return Solution and determinant.
matrix.rref = function (A,b)
   local tr, d = matrix._GaussDown_(matrix.concat(A,b,'h'))
   return matrix._GaussUp_(tr), d
end
matrix.about[matrix.rref] = {"rref(A,b)", "Perform transformations using Gauss method. Return also determinant."}

--- Create vector.
--  Simplified vector constructor. 
--  Can be called as V(...).
--  @param t Table with vector elements.
--  @return Vector form of matrix.
matrix.vector = function (t)
   local res = {0,0,0}          -- prepare some memory
   for i = 1, #t do res[i] = {t[i]} end
   return matrix:init(#t, 1, res)
end
matrix.about[matrix.vector] = {"vector({...})", "Create vector from list of numbers. The same as V().", help.NEW}
matrix.V = matrix.vector

--- Create matrix of zeros.
--  @param nR Number of rows.
--  @param nC Number of columns. Can be omitted in case of square matrix.
--  @return Sparse matrix.
matrix.zeros = function (nR, nC)
   if ismatrix(nR) then nR,nC = nR.rows, nR.cols end   -- input is a matrix
   nC = nC or nR                                       -- input is a number
   return matrix:init(nR, nC, {})
end
matrix.about[matrix.vector] = {"zeros(rows[,cols])", "Create matrix from zeros.", help.NEW}

--- Create dense matrix using given rule.
--  @param rows Number of rows.
--  @param cols Number of columns.
--  @param fn Function which depends on element index.
matrix.fill = function (rows, cols, fn)
   local m = matrix:init(rows, cols, {})
   for r = 1, rows do
      local mr = {}
      for c = 1,cols do mr[c] = fn(r,c) end
      m[r] = mr
   end
   return m
end
matrix.about[matrix.fill] = {"fill(rows,cols,fn)", "Create matrix, using function fn(r,c).", help.OTHER}

--- Matrix of constants.
--  @param rows Number of rows.
--  @param cols Number of columns. Can be omitted in case of square matrix.
--  @param val Value to set. Default is 1.
--  @return New matrix.
matrix.ones = function (rows, cols, val)
   if ismatrix(rows) then rows,cols,val = rows.rows, rows.cols, cols end
   return matrix.fill(rows, cols or rows, function () return val or 1 end)
end
matrix.about[matrix.ones] = {"ones(rows[,cols[,val]])", "Create matrix of given numbers (default is 1).", help.NEW}

--- Matrix with random values.
--  @param rows Number of rows.
--  @param cols Number of columns. Can be omitted in case of square matrix.
--  @return New matrix.
matrix.rand = function (rows, cols)
   if ismatrix(rows) then rows,cols = rows.rows, rows.cols end
   return matrix.fill(rows, cols or rows, function () return math.random() end)
end
matrix.about[matrix.rand] = {"rand(rows[,cols])", "Create matrix with random numbers from 0 to 1.", help.NEW}

--- Identity matrix.
--  @param rows Number of rows.
--  @param cols Number of columns. Can be omitted in case of square matrix.
--  @param val Diagonal value, default is 1.
--  @return Diagonal matrix with ones.
matrix.eye = function (rows, cols, val)
   if ismatrix(rows) then 
      val = cols or 1
      rows,cols = rows.rows, rows.cols 
   end
   cols = cols or rows
   val = val or 1
   local m = matrix:init(rows, cols, {})
   for i = 1, math.min(rows, cols) do m[i][i] = val end
   return m
end
matrix.about[matrix.eye] = {"eye(rows[,cols[,init]])", "Create identity matrix. Diagonal value (init) can be defined.", help.NEW}

--- Matrix concatenation.
--  Horizontal concatenation can be performed with <code>..</code>, vertical - <code>//</code>.
--  @param M1 First matrix.
--  @param M2 Second matrix.
--  @param dir Direction of concatenation ('h' for horizontal, 'v' for vertical).
--  @return Concatenated matrix.
matrix.concat = function (M1, M2, dir)
   local res = nil
   if dir == 'h' then
      if (M1.rows ~= M2.rows) then error("Different number of rows") end
      res = matrix:init(M1.rows, M1.cols+M2.cols, {})
   elseif dir == 'v' then
      if (M1.cols ~= M2.cols) then error("Different number of columns") end
      res = matrix:init(M1.rows+M2.rows, M1.cols, {})
   else
      error("Unexpected type of concatenation")
   end
   for r = 1, res.rows do
      local resr = res[r]
      for c = 1, res.cols do
         local src = (r <= M1.rows and c <= M1.cols) and M1 or M2
         local i = (r <= M1.rows) and r or (r - M1.rows)
	 local j = (c <= M1.cols) and c or (c - M1.cols)
	 resr[c] = src[i][j]
      end
   end
   return res
end
matrix.about[matrix.concat] = {"concat(M1,M2,dir)", 
                               "Concatenate two matrix, dir='h' - in horizontal direction, dir='v' - in vertical\nUse M1 .. M2 for horizontal concatenation and M1 // M2 for vertical.", 
			       TRANSFORM}

--- Horizontal concatenation
--  @param M1 First matrix.
--  @param M2 Second matrix.
--  @return Concatenated matrix.
matrix.__concat = function (M1,M2) return matrix.concat(M1,M2,'h') end

--- vertical concatenation
--  @param M1 First matrix.
--  @param M2 Second matrix.
--  @return Concatenated matrix.
matrix.__idiv = function (M1,M2) return matrix.concat(M1,M2,'v') end

--- String representation.
--  @param M Matrix.
--  @return String.
matrix.__tostring = function (M)
   local srow = {}
   for r = 1, M.rows do
      local scol, mr = {}, M[r]
      for c = 1, M.cols do
         local tmp = mr[c]
         table.insert(scol, type(tmp) == 'number' and string.format('%.3f',tmp) or tostring(mr[c]))
      end
      table.insert(srow, table.concat(scol, "  "))
   end
   return table.concat(srow, "\n")
end

--- Get trace of the matrix.
--  @param m Source matrix.
--  @return Sum of elements of the main diagonal.
matrix.tr = function (M)
   local sum = 0
   for i = 1,math.min(M.rows,M.cols) do sum = sum + M[i][i] end
   return sum
end
matrix.about[matrix.tr] = {"tr(M)", "Get trace of the matrix.", help.OTHER}

--[=[
--> SVD - "standard" algorithm implementation
--  BUT: some of singular values are negative :(

-- Householder transformation.
--    <i>Private function.</i>
--    @param x Matrix.
--    @param k Index.
--    @return Result of transformation.
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

-- Bidiagonalization.
--    <i>Private function.</i>
--    @param A Source matrix.
--    @return U, B, V
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

-- Given's rotation.
--    <i>Private function.</i>
--    @return cos, sin
local function rot(f,g)
   --> return cos, sin; r is omitted
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

-- QR-type sweeps.
--    <i>Private function.</i>
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
   local I = matrix.mapEx(matrix.zeros(m,n),
                            function (r,c,x) return (c == (r+1) and c <= M and r <= M) and 1 or 0 end)
   -- error - norm
   local E = 0
   for r = 1, m do
      for c = 1, n do E = E + math.abs(getval(I,r,c)*getval(B,r,c)) end
   end
   return U, B, V, E
end

-- "Standard" SVD calculation.
--    Warning: B get negative elements...
--    @param A original matrix.
--    @return U,B,V
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

-- Pseudo inverse matrix using SVD.
--    @param M Initial matrix.
--    @return Pseudo inverse matrix.
matrix.pinv = function (M)
   local u,s,v = matrix.svd(M)
   s = matrix.mapEx(s, function (r,c,x) return (r == c and math.abs(x) > 1e-8) and (1/x) or 0 end)
   return v * s:transpose() * u:transpose()
end
matrix.about[matrix.pinv] = {"pinv(M)", "Calculates pseudo inverse matrix using SVD.", help.OTHER}
]=]

--- Quick pseudo inverse matrix.
--  Based on "Fast computation of Moore-Penrose inverse matrices" paper by Pierre Courrieu.
--  @param M Initial matrix.
--  @return Pseudo inverse matrix.
matrix.pinv = function (M)
   local m,n,transp = M.rows, M.cols, false
   local Mt, A = M:transpose()
   if m < n then 
      A, n, transp = matrix.__mul(M, Mt), m, true
   else
      A = matrix.__mul(Mt, M)
   end
   local tol = math.huge
   for i = 1, A.rows do 
      local v = A[i][i]
      if type(v) == 'table' and v.iscomplex then v = v:abs() end
      tol = math.min(tol, (v > 0 and v or math.huge)) 
   end
   tol = tol * 1e-9
   local L, r, tmp = matrix.zeros(A:size()), 0
   for k = 1, n do
      r = r + 1
      local B = A:get({k,n},k)
      if r > 1 then 
         tmp = L:get({k,n},{1,r-1}) * L:get(k,{1,r-1}):transpose() 
	 tmp = ismatrix(tmp) and tmp or matrix:init(1,1,{{tmp}})         -- product can return a number
         B = B - tmp
      end
      for i = k, n do L[i][r] = B[i-k+1][1] end   -- copy B to L
      tmp = L[k][r]
      local iscomplex = (type(tmp)=='table') and tmp.iscomplex
      if iscomplex and tmp:abs() > tol then
         L[k][r] = tmp:sqrt()
         tmp = L[k][r]
         for i = k+1, n do L[i][r] = L[i][r]/tmp end
      elseif not iscomplex and tmp > tol then
         L[k][r] = math.sqrt(tmp)
         tmp = L[k][r]
         for i = k+1, n do L[i][r] = L[i][r]/tmp end
      else
         r = r - 1
      end
   end
   L.cols = (r > 0) and r or 1
   local Lt = L:transpose()
   local K = matrix.inv(Lt * L)
   if transp then
      return Mt * L * K * K * Lt
   end
   return L * K * K * Lt * Mt
end
matrix.about[matrix.pinv] = {"pinv(M)", "Pseudo inverse matrix calculation.", TRANSFORM}

--- Matrix to table.
--  @param M Source matrix.
--  @return Table without metametods.
matrix.table = function (M)
   local res = {}
   for r = 1,M.rows do
      local resr, mr = {}, M[r]
      for c = 1,M.cols do resr[c] = mr[c] end
      res[r] = resr
   end
   return res
end
matrix.about[matrix.table] = {"table(M)", "Convert to simple Lua table.", help.OTHER}

--- Remove zeros.
--  @param M Source matrix.
--  @return Sparse matrix.
matrix.sparse = function (M)
   local res = matrix:init(M.rows, M.cols, {})
   for r = 1, res.rows do
      local resr, mr = res[r], M[r]
      for c = 1, res.cols do resr[c] = mr[c] end
   end
   return res
end
matrix.about[matrix.sparse] = {"sparse(M)", "Return sparse matrix.", help.OTHER}

--- Get diagonal vector or create matrix with given elements.
--  @param M Matrix, vector or table with numbers.
--  @param n Diagonal index. Default is 0.
--  @return Vector of matrix.
matrix.diag = function (M,n)
   n = n or 0
   local k,res = (n < 0 and -n or n)
   if ismatrix(M) then
      if M.rows == 1 or M.cols == 1 then
         res = {}
	 for i = 1,math.max(M.rows,M.cols) do res[i] = M:get(i) end
	 return matrix.diag(res,n)
      else
	 local z = (n < 0) and math.min(M.rows-k,M.cols) or math.min(M.cols-k,M.rows) 
	 if z <= 0 then error("Wrong shift!") end
	 res = matrix:init(z,1, {})
	 for i = 1,z do res[i][1] = M[(n<0 and i+k or i)][(n<0 and i or i+k)] end
      end
   else
      res = matrix:init(#M+k,#M+k, {})
      for i = 1,#M do res[(n<0 and i+k or i)][(n<0 and i or i+k)] = M[i] end
   end
   return res
end
matrix.about[matrix.diag] = {'diag(M[,n])','Get diagonal of the matrix or create new matrix which diagonal elements are given. n is the diagonal index.', help.NEW}

--- V1 x V2
--  @param V1 3-element vector.
--  @param V2 3-element vector.
--  @return Cross product.
matrix.cross = function (V1,V2)
   if (V1.rows*V1.cols ~= 3 or V2.rows*V2.cols ~= 3) then error("Vector with 3 elements is expected!") end
   local x1,y1,z1 = V1:get(1), V1:get(2), V1:get(3)
   local x2,y2,z2 = V2:get(1), V2:get(2), V2:get(3)
   return matrix:init(3,1, {{y1*z2-z1*y2},{z1*x2-x1*z2},{x1*y2-y1*x2}})
end
matrix.about[matrix.cross] = {'cross(V1,V2)','Cross product or two 3-element vectors.'}

--- V1 . V2
--  @param V1 3-element vector.
--  @param V2 3-element vector.
--  @return Scalar product.
matrix.dot = function (V1,V2)
   if (V1.rows*V1.cols ~= 3 or V2.rows*V2.cols ~= 3) then error("Vector with 3 elements is expected!") end
   local x1,y1,z1 = V1:get(1), V1:get(2), V1:get(3)
   local x2,y2,z2 = V2:get(1), V2:get(2), V2:get(3)
   return x1*x2+y1*y2+z1*z2
end
matrix.about[matrix.dot] = {'dot(V1,V2)', 'Scalar product of two 3-element vectors'}

--- Auxiliary function for working with complex numbers.
--  @param a Number.
--  @return Absolute value.
matrix._fabs_ = function (a)
   return (type(a) == 'table' and a.iscomplex) and a:abs() or math.abs(a)
end

--- Prepare LU transformation for other functions.
--  @param M Initial square matrix.
--  @return "Compressed" LU, indexes, number of permutations
matrix._luPrepare_ = function (M)
   if M.rows ~= M.cols then error("Square matrix is expected!") end
   local a = matrix.copy(M)
   local vv = {}
   -- get scaling information
   for r = 1,a.rows do
      local big,abig,v = 0,0
      local ar = a[r]
      for c = 1,a.cols do 
         v = matrix._fabs_(ar[c])
	 if v > abig then
	    big = ar[c]
	    abig = v
	 end
      end
      vv[r] = 1.0/big
   end
   -- Crout's method
   local rmax, dum
   local TINY, d = 1e-20, 0
   local index = {}
   for c = 1,a.cols do
      for r = 1,c-1 do
         local ar = a[r]
         local sum = ar[c]
	 for k = 1,r-1 do sum = sum - ar[k]*a[k][c] end
	 ar[c] = sum
      end
      local big = 0                         -- largest pivot element
      for r=c,a.rows do
         local ar = a[r]
         local sum = ar[c]
	 for k = 1,c-1 do sum = sum - ar[k]*a[k][c] end
	 ar[c] = sum
	 sum = matrix._fabs_(sum)
	 dum = vv[r]*sum
	 if matrix._fabs_(dum) >= matrix._fabs_(big) then big = dum; rmax = r end
      end
      local ac = a[c]
      if c ~= rmax then
         local armax = a[rmax]
         -- interchange rows
         for k = 1,a.rows do
            dum = armax[k]
	    armax[k] = ac[k]
	    ac[k] = dum
	 end
	 d = d+1
	 vv[rmax] = vv[c]
      end
      index[c] = rmax
      if ac[c] == 0 then ac[c] = TINY end
      -- divide by pivot element
      if c ~= a.cols then 
         dum = 1.0 / ac[c]
	 for r = c+1,a.rows do a[r][c] = dum*a[r][c] end
      end
   end
   return a, index, d
end

--- LU transform
--  @param M Initial square matrix.
--  @return L matrix, U matrix, permutations
matrix.lu = function (M)
   local a,_,d = matrix._luPrepare_(M)
   local p = matrix.eye(M.rows,M.cols)
   while d > 0 do
      local tmp = p[1]; Ver.move(p,2,p.rows,1); p[p.rows] = tmp   -- shift
      d = d-1
   end
   return matrix.map(a, function (M,r,c) return (r==c) and 1.0 or (r>c and M or 0) end),   -- lower
          matrix.map(a, function (M,r,c) return r <= c and M or 0 end),                    -- upper
	  p                                                                                   -- permutations
end
matrix.about[matrix.lu] = {"lu(M)", "LU decomposition for the matrix. Return L,U and P matrices.", TRANSFORM}

--- Cholesky decomposition.
--  @param M Positive definite symmetric matrix.
--  @return Lower part of the decomposition.
matrix.Cholesky = function (M)
   if M.rows ~= M.cols then error("Square matrix is expected!") end
   local a,p = matrix.copy(M), {}
   -- calculate new values
   for r = 1,a.rows do
      local ar = a[r]
      for c = r,a.cols do
         local sum = ar[c]
	 for k = r-1,1,-1 do sum = sum - ar[k]*a[c][k] end
	 if r == c then
	    assert(sum >= 0, 'The matrix is not positive definite!')
	    p[r] = math.sqrt(sum)
	 else
	    a[c][r] = sum/p[r]
	 end
      end
   end
   -- insert zeros and the main diagonal elements
   for r = 1,a.rows do
      local ar = a[r]
      for c = r+1,a.cols do ar[c] = 0 end
      ar[r] = p[r]
   end
   return a
end
matrix.about[matrix.Cholesky] = {"Cholesky(M)", "Cholesky decomposition of positive definite symmetric matrix.", TRANSFORM}

--- Apply function to all elements along given direction.
--  @param M Initial matrix.
--  @param fn Function of 2 arguments.
--  @param dir Direction of evaluations (optional).
--  @param init Initial value (optional).
--  @return Reduced matrix.
matrix.reduce = function (M,fn,dir,init)
   dir = dir or 'r'
   init = init or 0
   local res
   if dir == 'r' then
      res = matrix:init(M.rows,1, {})
      for r = 1,M.rows do
         local s, mr = init, M[r]
	 for c = 1,M.cols do s = fn(s,mr[c]) end
	 res[r][1] = s
      end
   elseif dir == 'c' then
      res = matrix:init(1,M.cols, {})
      for c = 1,M.cols do
         local s = init
	 for r = 1,M.rows do s = fn(s,M[r][c]) end
	 res[1][c] = s
      end
   else
      error("Only 'r'(ows) or 'c'(olomns) are expected!")
   end
   return res
end
matrix.about[matrix.reduce] = {"reduce(M,fn,dir,init)","Evaluate s=fn(s,x) along rows (dir='r') or columns (dir='c'), where s0=init.",help.OTHER}

--- Get sum of all elements.
--  @param M Initial matrix.
--  @param dir Direction (optional).
--  @return Sum along 'r'ows or 'c'olumns
matrix.sum = function (M,dir) return matrix.reduce(M, fn_sum, dir, 0) end
matrix.about[matrix.sum] = {"sum(M,dir)", "Find sum of elements along given direction ('r' or 'c')."}

--- Get euclidean norm for each column/row.
--  @param m Initial matrix.
--  @param dir Direction (optional).
--  @return Norm along rows or columns.
matrix.sqNorm = function (M,dir)
   return matrix.reduce(M, function (a,b) return a+b^2 end, dir, 0)
end
matrix.about[matrix.sqNorm] = {"sqNorm(M,dir)", "Calculate square norm along given direction."}

--- Euclidean norm of the matrix at whole.
--  @param M Current matrix.
--  @return Norm value.
matrix.norm = function (M)
   local sum = 0
   for r = 1,M.rows do
      local mr = M[r]
      for c = 1,M.cols do
         sum = sum+(mr[c])^2
      end
   end
   return math.sqrt(sum)
end
matrix.about[matrix.norm] = {"norm(M)", "Euclidean norm."}

-- constructor call
setmetatable(matrix, {__call = function (self,m) return matrix.new(m) end})
matrix.Mat = 'Mat'
matrix.about[matrix.Mat] = {"Mat(...)", "Create matrix from list of strings (tables).", help.NEW}

--[[
--- Matrix serialization.
--    @param obj Matrix object.
--    @return String, suitable for exchange.
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
matrix.about[matrix.serialize] = {"serialize(obj)", "Save matrix internal representation.", help.OTHER}
]]

--- Function for execution during the module import.
matrix.onImport = function ()
   local _rand = rand or math.random
   rand = function (a,...) return a and matrix.rand(a,...) or _rand(a,...) end
   local _abs = abs or math.abs
   abs = function (a) return ismatrix(a) and matrix.map(a,_abs) or _abs(a) end
   local _sqrt = sqrt or math.sqrt
   sqrt = function (a) return ismatrix(a) and matrix.map(a,_sqrt) or _sqrt(a) end
   local _exp = exp or math.exp
   exp = function (a) return ismatrix(a) and matrix.map(a,_exp) or _exp(a) end
   local _sin = sin or math.sin
   sin = function (a) return ismatrix(a) and matrix.map(a,_sin) or _sin(a) end
   local _cos = cos or math.cos
   cos = function (a) return ismatrix(a) and matrix.map(a,_cos) or _cos(a) end
   local _tan = tan or math.tan
   tan = function (a) return ismatrix(a) and matrix.map(a,_tan) or _tan(a) end
end

-- free memory if need
if not lc_version then matrix.about = nil end

return matrix

--=========================
--TODO: Fix sign in SVD transform
--TODO: change matrix print
