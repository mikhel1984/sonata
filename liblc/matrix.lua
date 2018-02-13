--[[      liblc/matrix.lua 

--- Matrix operations. Indexation from 1.
--  @author <a href="mailto:vpsys@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

            module 'matrix'
--]]

-------------------- Tests -------------------
--[[!!
Mat = require 'liblc.matrix'

a = Mat {{1,2},{3,4}}             
b = Mat {{5,6},{7,8}}             
ans = a(2,2)                     --> 4 

b:set(1,1)(9)
ans = b(1,1)                     --> 9

b:set(1,1)(5)
c = a:T()
ans = c(1,2)                     --> 3

_, ans = a:size()                --> 2

ans = a + b                      --> Mat {{6,8},{10,12}}

ans = b - a                      --> Mat {{4,4},{4,4}}

ans = a * b                      --> Mat {{19,22},{43,50}}

ans = a / b 
ans = ans:det()                  --~ 1

ans = 2 * a                      --> Mat {{2,4},{6,8}}

ans = a - 1                      --> Mat {{0,1},{2,3}}

ans = a ^ 2                      --> Mat {{7,10},{15,22}} 

ans = a:det()                    --> -2

e = a:inv()
ans = e(2,1)                     --> 1.5

e = a^-1
ans = e(2,1)                     --> 1.5

f = a:copy()
ans = (f == a)                   --> true

ans = (a == b)                   --> false

ans = Mat.eye(2)                 --> Mat {{1,0},{0,1}}

ans = Mat.zeros(2,1)             --> Mat {{0},{0}}

ans = Mat.ones(2,3,4)            --> Mat {{4,4,4},{4,4,4}}

ans = Mat.ones(a,3)              --> Mat {{3,3},{3,3}}

ans = a .. b                     --> Mat {{1,2,5,6},{3,4,7,8}}

ans = a // b                     --> Mat {{1,2},{3,4},{5,6},{7,8}}

ans = a:map(function (x) return x^2 end)          --> Mat {{1,4},{9,16}}

ans = a:map_ex(function (x,r,c) return x-r-c end) --> Mat {{-1,-3},{-2,-4}}

ans = Mat.rref(a, Mat {{5},{11}}) --> Mat {{1,0,1},{0,1,2}}

ans = Mat.V {1,2,3}              --> Mat {{1},{2},{3}}

g = Mat {{1,2,3},{4,5,6},{7,8,9}}
ans = g:sub(2,-1,2,3)           --> Mat {{5,6},{8,9}}

h = Mat.rand(3,2)
print(h)

m = Mat {{1,2},{3,4},{5,6}}
n = m:pinv()
ans = n(2,2)                    --~ 0.333

k = Mat.eye(3)
k = k:dense()
ans = k[2][1]                   --> 0

k = k:sparse()
ans = k[2][1]                   --> nil

ans = Mat.diag({1,2,3})         --> Mat {{1,0,0},{0,2,0},{0,0,3}}

ans = g:diag(1)                 --> Mat {{2},{6}}

x1 = Mat {{1,2,3}}
x2 = Mat {{4,5,6}}
ans = Mat.cross(x1,x2)          --> Mat {{-3},{6},{-3}}

ans = Mat.dot(x1,x2)            --> 32

l,u,p = b:lu()
ans = l[2][1]                   --~ 0.714

m = Mat {{3,1},{1,3}}
m = m:cholesky()
ans = m[2][2]                   --~ 1.633

ans = a:tr()                    --> 5

m = a:row(1)
ans = m:get(1)                  --> 1

m = a:col(-1)
ans = m:get(2)                  --> 4

ans = a:sum()                   --> Mat {{3},{7}}

ans = a:reduce(function (x,y) return x*y end, 'c', 1) --> Mat {{3,8}}
]]

-------------------------------------------- 
-- @class table
-- @name matrix
-- @field type Define object type string.
-- @field about Description of functions.

local matrix = {}
-- mark object
matrix.type = 'matrix'
matrix.ismatrix = true
-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
matrix.about = help:new("Matrix operations. The matrices are spares by default.")

-- access to the elements
local get0 = function () return 0 end

matrix.__index = function (t,k) 
   if type(k) == 'number' then
      local tmp = setmetatable({}, {__index = get0})
      t[k] = tmp
      return tmp
   else
      return matrix[k]
   end
end
   

--- Check object type.
--    <i>Private function.</i>
--    @param m Object for checking.
--    @return True if table is a matrix.
local function ismatrix(m) return type(m) == 'table' and m.ismatrix end

--- Initialization of matrix with given size.
--    @param r Number of rows.
--    @param c Number of columns.
--    @param m Table for initialization.
--    @return Matrix object.
function matrix:init(r, c, m)
   assert(r > 0 and c > 0, "Wrong matrix size")
   m = m or {}
   m.cols, m.rows = c, r
   setmetatable(m, self)
   return m
end

--- Create new matrix from list of tables.
--    Arguments are rows represented as tables.
--    @return Matrix object.
function matrix.new(m)
   m = m or {}
   local cols, rows = 0, #m
   --local equal = true
   for i = 1, rows do
      assert(type(m[i]) == 'table', "Row must be a table!")
      cols = (cols < #m[i]) and #m[i] or cols
    --  if i > 1 then equal = equal and #m[i] == #m[i-1] end
   end
   --m.isdense = equal
   return matrix:init(rows, cols, m)
end

--- Check correctness of element index.
--    Used only for user defined index. Can be negative.
--    If <code>c</code> is omitted then <code>r</code> is index in vector.
--    <i>Private function.</i>
--    @param m Matrix.
--    @param r Row number.
--    @param c Col number.
--    @return Corrected value of row and col.
local function checkindex(m, r, c)
   assert(ismatrix(m), "Matrix is expected")
   if not c then
      if m.cols == 1 then 
         c = 1
      elseif m.rows == 1 then 
         r, c = 1, r
      end
   end
   -- check range
   assert(c <= m.cols and c >= -m.cols, "Column number must be not more then " .. m.cols)
   assert(r <= m.rows and r >= -m.rows, "Row number must be not more then " .. m.rows)
   assert(r ~= 0 and c ~= 0, "Indexation from 1!")
   r = (r < 0) and (m.rows+r+1) or r
   c = (c < 0) and (m.cols+c+1) or c
   return r, c
end

--- Return 0 in element is empty or its value.
--    <i>Private function.</i>
--    @param m Matrix.
--    @param r Row number.
--    @param c Column number.
--    @return Element value.
--[[
local function getval(m, r, c)
   --return m[r] and m[r][c] or 0
   return m[r][c]
end]]

--- Set value if need.
--    <i>Private function.</i>
--    @param m Matrix.
--    @param r Row number.
--    @param c Column number.
--    @param v New value.
local function setval(m, r, c, v)
   --[[if m.isdense then 
      m[r][c] = v
   else]] 
   --[[
   if (m[r] and m[r][c]) or v ~= 0 then 
      m[r] = m[r] or {}
      m[r][c] = v
   end
   ]]
   m[r][c] = v
end

--- Set product of element to coefficient.
--    <i>Private function.</i>
--    @param k Coefficient.
--    @param m Matrix.
--    @return Result of production.
local function kprod(k, m)
   local res = matrix:init(m.rows, m.cols)
   for r = 1, m.rows do
      for c = 1, m.cols do setval(res,r,c, k*m[r][c]) end
   end
   return res
end

--- Transform matrix to upper triangle.
--    @param m Initial matrix.
--    @return Upper triangulated matrix and determinant.
local function gaussdown(m)
   local A = 1
   for k = 1, m.rows do
      -- look for nonzero element
      local i = k+1
      while m[k][k] == 0 and i <= m.rows do
         if m[i][k] ~= 0 then m[i],m[k],A = m[k],m[i],-A end
	 i = i+1
      end
      local coef = m[k][k]
      A = A * coef
      if coef ~= 0 then
         -- normalization
	 coef = 1/coef
         for c = k, m.cols do setval(m,k,c, m[k][c]*coef) end
         -- subtraction
         for r = (k+1), m.rows do
            local v = m[r][k]
	    if v ~= 0 then
               for c = k, m.cols do setval(m,r,c, m[r][c]-v*m[k][c]) end
	    end -- if
         end -- for r
      end -- if
   end -- for k
   return m, A
end

--- Transform triangle matrix to identity matrix.
--    @param Initial matrix
--    @return Matrix with diagonal zeros.
local function gaussup(m)
   for k = m.rows, 1, -1 do
      for r = k-1,1,-1 do
         local v = m[r][k]
         if v ~= 0 then
            for c = k, m.cols do setval(m,r,c, m[r][c]-v*m[k][c]) end 
         end -- if
      end -- for r
   end -- for k
   return m
end

--- Matrix triangularization.
--    @param m Initial matrix.
--    @return Triangularized matrix.
matrix.triang = function (m)
   local res = matrix.copy(m)
   return gaussdown(res)
end
matrix.about[matrix.triang] = {'triang(m)', 'Matrix triangularization produced by Gaussian elimination.', help.OTHER}

matrix.rank = function (m)
   local mat,i = matrix.triang(m),1
   while i <= mat.rows do
      if not mat[i] then break end
      local zeros = true
      for j = 1,mat.cols do
         if mat[i][j] ~= 0 then zeros = false; break end
      end
      if zeros then break end
      i = i+1
   end
   return i-1
end


--- Get matrix element.
--    Can be called with ().
--    @param m Matrix.
--    @param r Row number.
--    @param c Column number.
--    @return Element value.
matrix.get = function (m, r, c)
   r, c = checkindex(m, r, c)
   return m[r][c]
end
matrix.about[matrix.get] = {"get(m,row,col)", "Return matrix element.", }

-- simplify call of matrix.get()
matrix.__call = function (m,r,c) return matrix.get(m,r,c) end

--- Set value of matrix element.
--    @param m Matrix.
--    @param val New value.
--    @param r Row number.
--    @param c Column number.
--[[
matrix.set = function (m, val, r, c)
   r, c = checkindex(m, r, c)
   m[r] = m[r] or {}
   m[r][c] = val
end
]]
matrix.set = function (m,r,c)
   r, c = checkindex(m, r, c)
   return function (val)
         --    m[r] = m[r] or {}
	     m[r][c] = val
          end
end
matrix.about[matrix.set] = {"set(m,row,col)(val)", "Set value of matrix element.", } 

--- Transpose matrix.
--    Can be called as T().
--    @param m Initial matrix.
--    @return Transposed matrix.
matrix.transpose = function (m)
   local res = matrix:init(m.cols, m.rows)
   for r = 1, m.rows do
      for c = 1, m.cols do setval(res,c,r, m[r][c]) end
   end
   return res
end
matrix.about[matrix.transpose] = {"transpose(m)", "Return matrix transpose. Shorten form is T().", }
matrix.T = matrix.transpose

-- to accelerate calculations
local fn_sum = function (x,y) return x+y end
local fn_sub = function (x,y) return x-y end
local fn_unm = function (x) return -x end

--- a + b
--    @param a First matrix.
--    @param b Second matrix.
--    @return Sum of the given matrices.
matrix.__add = function (a,b)
   a = ismatrix(a) and a or matrix.ones(b.rows, b.cols, a)
   b = ismatrix(b) and b or matrix.ones(a.rows, a.cols, b)
   return matrix.apply(a,b,fn_sum)
end

--- a - b
--    @param a First matrix.
--    @param b Second matrix.
--    @return Subtraction of the given matrices.
matrix.__sub = function (a,b)
   a = ismatrix(a) and a or matrix.ones(b.rows, b.cols, a)
   b = ismatrix(b) and b or matrix.ones(a.rows, a.cols, b)
   return matrix.apply(a,b,fn_sub)
end

--- -a
--    @param a Initial matrix.
--    @return Negative value of matrix.
matrix.__unm = function (a)
   return matrix.map(a,fn_unm)
end

--- Get matrix size.
--    @param m Matrix to check.
--    @return Number of rows and columns.
matrix.size = function (m)
   return m.rows, m.cols
end
matrix.about[matrix.size] = {"size(m)", "Return number or rows and columns. Can be called with '#'.", }

--- Apply function to each element.
--    @param m Source matrix.
--    @param fn Function f(x).
--    @return Result of function evaluation.
matrix.map = function (m, fn) 
   local res = matrix:init(m.rows, m.cols)
   --res.isdense = m.isdense
   for r = 1, res.rows do
      --if res.isdense then res[r] = {} end
      for c = 1, res.cols do setval(res,r,c, fn(m[r][c])) end
   end
   return res
end
matrix.about[matrix.map] = {"map(m,fn)", "Apply the given function to all elements, return new matrix.", help.OTHER}

--- Apply function to each element, use row and col values.
--    @param m Source matrix.
--    @param fn Function f(r,c,x).
--    @return Result of function evaluation.
matrix.map_ex = function (m, fn)
   local res = matrix:init(m.rows, m.cols)
   --res.isdense = m.isdense
   for r = 1, res.rows do
      --if res.isdense then res[r] = {} end
      for c = 1, res.cols do setval(res,r,c, fn(r,c,m[r][c])) end
   end
   return res
end
matrix.about[matrix.map_ex] = {"map_ex(m,fn)", "Apply function fn(row,col,val) to all elements, return new matrix.", help.OTHER}

--- Apply function to each pair elements of given matrices.
--    @param m1 First matrix.
--    @param m2 Second matrix.
--    @param fn Function from two arguments f(v1,v2).
--    @return Result of function evaluation.
matrix.apply = function (m1, m2, fn)
   assert(m1.rows==m2.rows and m1.cols==m2.cols, "Different matrix size!")
   local res = matrix:init(m1.rows,m1.cols)
   --res.isdense = m1.isdense and m2.isdense
   for r = 1,res.rows do
      --if res.isdense then res[r] = {} end
      for c = 1,res.cols do setval(res,r,c, fn(m1[r][c], m2[r][c])) end
   end
   return res
end
matrix.about[matrix.apply] = {"apply(m1,m2,fn)", "Apply fu(v1,v2) to each element of matrices m1 and m2.", help.OTHER}

--- Create copy of matrix.
--    @param m Source matrix.
--    @return Deep copy.
matrix.copy = function (m)
   return matrix.map(m, function (x) return x end)
end
matrix.about[matrix.copy] = {"copy(m)", "Return copy of matrix.", help.OTHER}

--- a * b
--    @param a First matrix.
--    @param b Second matrix.
--    @return Multiplication of the given matrices.
matrix.__mul = function (a,b)
   if not ismatrix(a) then return kprod(a, b) end
   if not ismatrix(b) then return kprod(b, a) end
   assert(a.cols == b.rows, "Impossible to get product: different size!")
   local res = matrix:init(a.rows, b.cols)
   for r = 1, res.rows do
      for c = 1, res.cols do
         local sum = 0
	 for i = 1, a.cols do sum = sum + a[r][i]*b[i][c] end
	 setval(res,r,c, sum)
      end
   end
   return (res.cols == 1 and res.rows == 1) and res[1][1] or res
end

--- a / b
--    @param a First matrix.
--    @param b Second matrix.
--    @return Ratio of the given matrices.
matrix.__div = function (a,b)
   if not ismatrix(b) then return kprod(1/b, a) end
   return a * matrix.inv(b)
end

--- a ^ n
--    @param a Matrix.
--    @param n Positive integer power.
--    @return New matrix.
matrix.__pow = function (a,n)
   n = assert(math.tointeger(n), "Integer is expected!")
   assert(a.rows == a.cols, "Square matrix is expected!")
   if n == -1 then return matrix.inv(a) end
   local res, acc = matrix.eye(a.rows), matrix.copy(a)
   while n > 0 do
      if n%2 == 1 then res = res * acc end
      n = n // 2
      if n > 0 then acc = acc * acc end
   end
   return res
end

matrix.arithmetic = 'arithmetic'
matrix.about[matrix.arithmetic] = {matrix.arithmetic, "a+b, a-b, a*b, a/b, a^b, -a", }

--- a == b
--    @param a First matrix.
--    @param b Second matrix.
--    @return <code>true</code> if all elements are equal.
matrix.__eq = function (a,b)
   if not (ismatrix(a) and ismatrix(b)) then return false end
   if a.rows ~= b.rows or a.cols ~= b.cols then return false end
   for r = 1, a.rows do
      for c = 1, a.cols do
         if a[r][c] ~= b[r][c] then return false end
      end
   end
   return true
end

matrix.comparison = 'comparison'
matrix.about[matrix.comparison] = {matrix.comparison, "a==b, a~=b", }


--- Find determinant.
--    @param m Initial matrix.
--    @return Determinant.
matrix.det = function (m)
   assert(m.rows == m.cols, "Square matrix is expected!")
   local _, K = gaussdown(matrix.copy(m))
   return K
end
matrix.about[matrix.det] = {"det(m)", "Calculate determinant.", }

--- Inverse matrix.
--    @param m Initial matrix.
--    @return Result of inversion.
matrix.inv = function (m)
   assert(m.rows == m.cols, "Square matrix is expected!")
   local con, det = matrix.rref(m, matrix.eye(m.cols))
   return (det ~= 0) and matrix.sub(con, 1,-1, m.cols+1, -1) or matrix.ones(m.rows,m.rows,math.huge) 
end
matrix.about[matrix.inv] = {"inv(m)", "Return inverse matrix.", }

--- Solve system of equations using Gauss method.
--    @param A Matrix of coefficients.
--    @param b Free coefficients (matrix of vector).
--    @return Solution and determinant.
matrix.rref = function (A,b)
   local tr, d = gaussdown(matrix.concat(A,b,'h'))
   return gaussup(tr), d
end
matrix.about[matrix.rref] = {"rref(A,b)", "Perform transformations using Gauss method. Return also determinant.", }

--- Create vector.
--    Simplified vector constructor. 
--    Arguments are list of vector elements.
--    Can be called as V(...).
--    @return Vector form of matrix.
matrix.vector = function (v)
   local res = {}
   for i = 1, #v do res[i] = {v[i]} end
   return matrix:init(#v, 1, res)
end
matrix.about[matrix.vector] = {"vector(...)", "Create vector from list of numbers. The same as V().", }
matrix.V = matrix.vector

--- Create matrix of zeros.
--    @param rows Number of rows.
--    @param cols Number of columns. Can be omitted in case of square matrix.
--    @return Sparse matrix.
matrix.zeros = function (rows, cols)
   if ismatrix(rows) then rows,cols = rows.rows, rows.cols end
   cols = cols or rows
   return matrix:init(rows, cols)
end
matrix.about[matrix.vector] = {"zeros(rows[,cols])", "Create matrix from zeros.", help.OTHER}

--- Create dense matrix using given rule.
--    @param rows Number of rows.
--    @param cols Number of columns.
--    @param fn Function which depend on element index.
matrix.fill = function (rows, cols, fn)
   assert(rows > 0 and cols > 0, "Wrong matrix size!")
   local m = matrix:init(rows, cols)
   for r = 1, rows do
      m[r] = {}
      for c = 1,cols do m[r][c] = fn(r,c) end
   end
   --m.isdense = true
   return m
end
matrix.about[matrix.fill] = {"fill(rows,cols,fn)", "Create matrix, using function fn(r,c).", help.OTHER}

--- Matrix of constants.
--    @param rows Number of rows.
--    @param cols Number of columns. Can be omitted in case of square matrix.
--    @param val Value to set. Default is 1.
--    @return New matrix.
matrix.ones = function (rows, cols, val)
   if ismatrix(rows) then rows,cols,val = rows.rows, rows.cols, cols end
   return matrix.fill(rows, cols or rows, function (r,c) return val or 1 end)
end
matrix.about[matrix.ones] = {"ones(rows[,cols[,val]])", "Create matrix of given numbers (default is 1).", help.OTHER}

--- Matrix with random values.
--    @param rows Number of rows.
--    @param cols Number of columns. Can be omitted in case of square matrix.
--    @return New matrix.
matrix.rand = function (rows, cols)
   if ismatrix(rows) then rows,cols = rows.rows, rows.cols end
   return matrix.fill(rows, cols or rows, function (r,c) return math.random() end)
end
matrix.about[matrix.rand] = {"rand(rows[,cols])", "Create matrix with random numbers from 0 to 1.", help.OTHER}

--- Identity matrix.
--    @param rows Number of rows.
--    @param cols Number of columns. Can be omitted in case of square matrix.
--    @return Diagonal matrix with ones.
matrix.eye = function (rows, cols)
   if ismatrix(rows) then rows,cols = rows.rows, rows.cols end
   cols = cols or rows
   local m = matrix:init(rows, cols)
   for i = 1, math.min(rows, cols) do setval(m,i,i, 1) end
   return m
end
matrix.about[matrix.eye] = {"eye(rows[,cols])", "Create identity matrix.", help.OTHER}

--- Get sub matrix.
--    @param m Initial matrix.
--    @param r1 Lower row index.
--    @param r2 Upper row index.
--    @param c1 Lower column number.
--    @param upper column number.
matrix.sub = function (m, r1, r2, c1, c2)
   r1, c1 = checkindex(m, r1, c1)
   r2, c2 = checkindex(m, r2, c2)
   local res = matrix:init(r2-r1+1, c2-c1+1)
   local i, j = 1, 1
   for r = r1, r2 do
      for c = c1, c2 do
	 setval(res,i,j, m[r][c])
	 j = j+1
      end
      i, j = i+1, 1
   end
   return res
end
matrix.about[matrix.sub] = {"sub(m,r1,r2,c1,c2)", "Return sub matrix with rows [r1;r2] and columns [c1;c2].", help.OTHER}

--- Matrix concatenation.
--    Horizontal concatenation can be performed with <code>..</code>, vertical - <code>//</code>.
--    @param a First matrix.
--    @param b Second matrix.
--    @param dib Direction of concatenation ('h' for horizontal, 'v' for vertical).
--    @return Concatenated matrix.
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
	 setval(res,r,c, src[i][j])
      end
   end
   return res
end
matrix.about[matrix.concat] = {"concat(m1,m2,dir)", 
                               "Concatenate two matrix, dir='h' - in horizontal direction, dir='v' - in vertical\nUse m1 .. m2 for horizontal concatenation and m1 // m2 for vertical.", 
			       help.OTHER}

-- horizontal concatenation
matrix.__concat = function (a,b) return matrix.concat(a,b,'h') end

-- vertical concatenation
matrix.__idiv = function (a,b) return matrix.concat(a,b,'v') end

--- String representation.
--    @param m Matrix.
--    @return String.
matrix.__tostring = function (m)
   local srow = {}
   for r = 1, m.rows do
      local scol = {}
      for c = 1, m.cols do
         table.insert(scol, tostring(m[r][c]))
      end
      table.insert(srow, table.concat(scol, "  "))
   end
   return table.concat(srow, "\n")
end

--- Get trace of the matrix.
--    @param m Source matrix.
--    @return Sum of elements of the main diagonal.
matrix.tr = function (m)
   local sum = 0
   for i = 1,math.min(m.rows,m.cols) do sum = sum + m[i][i] end
   return sum
end
matrix.about[matrix.tr] = {"tr(m)", "Get trace of the matrix.", help.OTHER}

--- Get matrix row.
--    @param m Source matrix.
--    @param k Row index.
--    @return Row of the matrix.
matrix.row = function (m,k)
   assert(k >= -m.rows and k <= m.rows and k ~= 0, 'Wrong row number!')
   if k < 0 then k = m.rows+k+1 end
   local r = m[k] or {}
   return matrix:init(1,m.cols,{r})
end
matrix.about[matrix.row] = {"row(m,k)", "Return k-th row of the given matrix.", help.OTHER}

--- Get matrix column.
--    @param m Source matrix.
--    @param k Column index.
--    @return Column of the matrix.
matrix.col = function (m,k)
   assert(k >= -m.cols and k <= m.cols and k ~= 0, 'Wrong column number!')
   if k < 0 then k = m.cols+k+1 end
   local acc = {}
   for r = 1,m.rows do acc[r] = {m[r][k]} end
   return matrix:init(m.rows,1,acc)
end
matrix.about[matrix.col] = {"col(m,k)", "Return k-th column of the given matrix.", help.OTHER}

--[=[
--> SVD - "standard" algorithm implementation
--  BUT: some of singular values are negative :(

--- Householder transformation.
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

--- Bidiagonalization.
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

--- Given's rotation.
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

--- QR-type sweeps.
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
   local I = matrix.map_ex(matrix.zeros(m,n),
                            function (r,c,x) return (c == (r+1) and c <= M and r <= M) and 1 or 0 end)
   -- error - norm
   local E = 0
   for r = 1, m do
      for c = 1, n do E = E + math.abs(getval(I,r,c)*getval(B,r,c)) end
   end
   return U, B, V, E
end

--- "Standard" SVD calculation.
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

--- Pseudo inverse matrix using SVD.
--    @param M Initial matrix.
--    @return Pseudo inverse matrix.
matrix.pinv = function (M)
   local u,s,v = matrix.svd(M)
   s = matrix.map_ex(s, function (r,c,x) return (r == c and math.abs(x) > 1e-8) and (1/x) or 0 end)
   return v * s:transpose() * u:transpose()
end
matrix.about[matrix.pinv] = {"pinv(M)", "Calculates pseudo inverse matrix using SVD.", help.OTHER}
]=]

--- Quick pseudo inverse matrix.
--    Based on "Fast computation of Moore-Penrose inverse matrices" paper by Pierre Courrieu.
--    @param M Initial matrix.
--    @return Pseudo inverse matrix.
matrix.pinv = function (M)
   local m,n,transp = M.rows, M.cols, false
   local A, Mt = nil, M:transpose()
   if m < n then 
      A, n, transp = M * Mt, m, true
   else
      A = Mt * M
   end
   local tol, v = math.huge, 0
   for i = 1, A.rows do 
      v = A[i][i];
      if type(v) == 'table' and v.iscomplex then v = v:abs() end
      tol = math.min(tol, (v > 0 and v or math.huge)) 
   end
   tol = tol * 1e-9
   local L, r = matrix.zeros(A:size()), 0
   for k = 1, n do
      r = r + 1
      local B = A:sub(k,n,k,k)
      if r > 1 then 
         local tmp = L:sub(k,n,1,r-1) * L:sub(k,k,1,r-1):transpose() 
	 tmp = ismatrix(tmp) and tmp or matrix.new {{tmp}}         -- product can return a number
         B = B - tmp
      end
      for i = k, n do setval(L,i,r, B[i-k+1][1]) end   -- copy B to L
      tmp = L[k][r]
      local iscomplex = (type(tmp)=='table') and tmp.iscomplex
      if iscomplex and tmp:abs() > tol then
         setval(L,k,r, tmp:sqrt())
         tmp = L[k][r]
	 if k < n then
	    for i = k+1, n do setval(L, i, r, L[i][r]/tmp) end
	 end
      elseif not iscomplex and tmp > tol then
         setval(L,k,r, math.sqrt(tmp))
         tmp = L[k][r]
	 if k < n then
	    for i = k+1, n do setval(L, i, r, L[i][r]/tmp) end
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
matrix.about[matrix.pinv] = {"pinv(M)", "Pseudo inverse matrix calculation.", help.OTHER}

--- Represent matrix in explicit (dense) form.
--    @param m Source matrix.
--    @return Dense matrix.
matrix.dense = function (m)
   local res = matrix:init(m.rows, m.cols)
   for r = 1,m.rows do
      res[r] = {}
      for c = 1,m.cols do res[r][c] = m[r][c] end
   end
   --res.isdense = true
   return res
end
matrix.about[matrix.dense] = {"dense(m)", "Return dense matrix.", help.OTHER}

--- Return sparse matrix, if possible.
--    @param m Source matrix.
--    @return Sparse matrix.
matrix.sparse = function (m)
   local res = matrix:init(m.rows, m.cols)
   for r = 1, res.rows do
      for c = 1, res.cols do setval(res,r,c, m[r][c]) end
   end
   return res
end
matrix.about[matrix.sparse] = {"sparse(m)", "Return sparse matrix.", help.OTHER}

--- Get diagonal vector or create matrix with given elements.
--    @param m Matrix, vector or table with numbers.
--    @param n Diagonal index. Default is 0.
--    @return Vector of matrix.
matrix.diag = function (m,n)
   n = n or 0
   local k,res = (n < 0 and -n or n)
   if ismatrix(m) then
      if m.rows == 1 or m.cols == 1 then
         res = {}
	 for i = 1,math.max(m.rows,m.cols) do res[i] = m:get(i) end
	 return matrix.diag(res,n)
      else
	 local z = (n < 0) and math.min(m.rows-k,m.cols) or math.min(m.cols-k,m.rows) 
	 assert(z > 0, "Wrong shift!")
	 res = matrix:init(z,1)
	 for i = 1,z do setval(res, i,1, m[(n<0 and i+k or i)][(n<0 and i or i+k)]) end
      end
   else
      res = matrix:init(#m+k,#m+k)
      for i = 1,#m do setval(res, (n<0 and i+k or i), (n<0 and i or i+k), m[i]) end
   end
   return res
end
matrix.about[matrix.diag] = {'diag(M[,n])','Get diagonal of the matrix or create new matrix which diagonal elements are given. n is the diagonal index.', help.OTHER}

--- a x b
--    @param a 3-element vector.
--    @param b 3-element vector.
--    @return Cross product.
matrix.cross = function (a,b)
   assert(a.rows*a.cols == 3 and b.rows*b.cols == 3, "Vector with 3 elements is expected!")
   local x1,y1,z1 = a:get(1), a:get(2), a:get(3)
   local x2,y2,z2 = b:get(1), b:get(2), b:get(3)
   return matrix.new {{y1*z2-z1*y2},{z1*x2-x1*z2},{x1*y2-y1*x2}}
end
matrix.about[matrix.cross] = {'cross(a,b)','Cross product or two 3-element vectors.', }

--- a . b
--    @param a 3-element vector.
--    @param b 3-element vector.
--    @return Scalar product.
matrix.dot = function (a,b)
   assert(a.rows*a.cols == 3 and b.rows*b.cols == 3, "Vector with 3 elements is expected!")
   local x1,y1,z1 = a:get(1), a:get(2), a:get(3)
   local x2,y2,z2 = b:get(1), b:get(2), b:get(3)
   return x1*x2+y1*y2+z1*z2
end
matrix.about[matrix.dot] = {'dot(a,b)', 'Scalar product of two 3-element vectors', }

local function fabs(m)
   return (type(m) == 'table' and m.iscomplex) and m:abs() or math.abs(m)
end

--- Prepare LU transformation for other functions.
--    @param m Initial square matrix.
--    @return "Compressed" LU, indexes, number of permutations
matrix.luprepare = function (m)
   assert(m.rows == m.cols, "Square matrix is expected!")
   local a = matrix.copy(m)
   local vv = {}
   -- get scaling information
   for r = 1,a.rows do
      local big,abig,v = 0,0
      for c = 1,a.cols do 
         v = fabs(a[r][c])
	 if v > abig then
	    big = a[r][c]
	    abig = v
	 end
      end
      vv[#vv+1] = 1.0/big
   end
   -- Crout's method
   local rmax, dum
   local TINY, d = 1e-20, 0
   --local index = matrix:init(m.rows,1)
   local index = {}
   for c = 1,a.cols do
      for r = 1,c-1 do
         local sum = a[r][c]
	 for k = 1,r-1 do sum = sum - a[r][k]*a[k][c] end
	 setval(a,r,c, sum)
      end
      local big = 0                         -- largest pivot element
      for r=c,a.rows do
         local sum = a[r][c]
	 for k = 1,c-1 do sum = sum - a[r][k]*a[k][c] end
	 setval(a,r,c, sum)
	 sum = fabs(sum)
	 dum = vv[r]*sum
	 if fabs(dum) >= fabs(big) then big = dum; rmax = r end
      end
      if c ~= rmax then
         -- interchange rows
         for k = 1,a.rows do
            dum = a[rmax][k]
	    setval(a,rmax,k, a[c][k])
	    setval(a,c,k, dum)
	 end
	 d = d+1
	 vv[rmax] = vv[c]
      end
      --setval(index,c,1, rmax)
      index[c] = rmax
      if a[c][c] == 0 then setval(a,c,c, TINY) end
      -- divide by pivot element
      if c ~= a.cols then 
         dum = 1.0 / a[c][c]
	 for r = c+1,a.rows do setval(a,r,c, dum*a[r][c]) end
      end
   end
   return a, index, d
end

--- LU transform
--    @param m Initial square matrix.
--    @return L matrix, U matrix, permutations
matrix.lu = function (m)
   local a,_,d = matrix.luprepare(m)
   local p = matrix.eye(m.rows,m.cols)
   while d > 0 do
      local tmp = p[1]; table.move(p,2,p.rows,1); p[p.rows] = tmp   -- shift
      d = d-1
   end
   return matrix.map_ex(a, function (r,c,m) return (r==c) and 1.0 or (r>c and m or 0) end),   -- lower
          matrix.map_ex(a, function (r,c,m) return r <= c and m or 0 end),                    -- upper
	  p                                                                                   -- permutations
end
matrix.about[matrix.lu] = {"lu(m)", "LU decomposition for the matrix. Return L,U and P matrices.", }

--- Cholesky decomposition.
--    @param m Positive definite symmetric matrix.
--    @return Lower part of the decomposition.
matrix.cholesky = function (m)
   assert(m.rows == m.cols, "Square matrix is expected!")
   local a,p = matrix.copy(m), {}
   -- calculate new values
   for r = 1,a.rows do
      for c = r,a.cols do
         local sum = a[r][c]
	 for k = r-1,1,-1 do sum = sum - a[r][k]*a[c][k] end
	 if r == c then
	    assert(sum >= 0, 'The matrix is not positive definite!')
	    p[r] = math.sqrt(sum)
	 else
	    setval(a,c,r, sum/p[r])
	 end
      end
   end
   -- insert zeros and the main diagonal elements
   for r = 1,a.rows do
      for c = r+1,a.cols do setval(a,r,c, 0) end
      setval(a,r,r, p[r])
   end
   return a
end
matrix.about[matrix.cholesky] = {"cholesky(m)", "Cholesky decomposition of positive definite symmetric matrix.", help.OTHER}

--- Apply function to all elements along given direction.
--    @param m Initial matrix.
--    @param fn Function of 2 arguments.
--    @param dir Direction of evaluations (optional).
--    @param init Initial value (optional).
--    @return Reduced matrix.
matrix.reduce = function (m,fn,dir,init)
   dir = dir or 'r'
   init = init or 0
   local res
   if dir == 'r' then
      res = matrix:init(m.rows,1)
      for r = 1,m.rows do
         local s = init
	 for c = 1,m.cols do s = fn(s,m[r][c]) end
	 setval(res,r,1, s)
      end
   elseif dir == 'c' then
      res = matrix:init(1,m.cols)
      for c = 1,m.cols do
         local s = init
	 for r = 1,m.rows do s = fn(s,m[r][c]) end
	 setval(res,1,c, s)
      end
   else
      error("Only 'r'(ows) or 'c'(olomns) are expected!")
   end
   return res
end
matrix.about[matrix.reduce] = {"reduce(m,fn,dir,init)","Evaluate s=fn(s,x) along rows (dir='r') or columns (dir='c'), where s0=init.",help.OTHER}

--- Get summ of all elements.
--    @param m Initial matrix.
--    @param dir Direction (optional).
--    @return Sum along 'r'ows or 'c'olumns
matrix.sum = function (m,dir) return matrix.reduce(m, fn_sum, dir, 0) end
matrix.about[matrix.sum] = {"sum(m,dir)", "Find sum of elements along given direction ('r' or 'c')."}

matrix.sqnorm = function (m,dir)
   return matrix.reduce(m, function (a,b) return a+b^2 end, dir, 0)
end
matrix.about[matrix.sqnorm] = {"sqnorm(m,dir)", "Calculate square norm along given direction."}

matrix.norm = function (m)
   local sum = 0
   for r = 1,m.rows do
      for c = 1,m.cols do
         sum = sum+(m[r][c])^2
      end
   end
   return math.sqrt(sum)
end

-- constructor call
setmetatable(matrix, {__call = function (self,m) return matrix.new(m) end})
matrix.Mat = 'Mat'
matrix.about[matrix.Mat] = {"Mat(...)", "Create matrix from list of strings (tables).", help.NEW}

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

-- free memory if need
if not lc_version then matrix.about = nil end

return matrix

--=========================
--TODO: Fix sign in SVD transform
--TODO: rank
