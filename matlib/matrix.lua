--[[		sonata/lib/matrix.lua

--- Matrix operations. Indexation from 1.
--
--  Object structure: </br>
--  <code>{{x11...x1n}, {x21...x2n}...{xm1...xmn}</br>
--  _rows=m, _cols=n} </code></br>
--  Internal elements can be empty tables, but anyway
--  operation X[i][j] will return 0.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2023.

	module 'matrix'
--]]


-------------------- Tests -------------------
--[[TEST_IT

-- use 'matrix'
Mat = require 'matlib.matrix'

-- define matrix objects
a = Mat {{1,2},{3,4}}
b = Mat {{5,6},{7,8}}
-- call in typical way
ans = a[2][2]                 -->  4

b[1][1] = 5

-- transpose
c = a:T()
-- 'smart' getter
ans = c(1,-1)                 -->  3

-- matrix columns and rows
ans = a:cols()                -->  2

ans = a:rows()                -->  2

-- arithmetical operations
ans = a + b                   -->  Mat {{6,8},{10,12}}

ans = b - a                   -->  Mat {{4,4},{4,4}}

ans = a * b                   -->  Mat {{19,22},{43,50}}

ans = a / b
-- determinant
ans = ans:det()              --2>  1

-- multiply to scalar
ans = 2 * a                   -->  Mat {{2,4},{6,8}}

-- add scalar (to all elements)
ans = a - 1                   -->  Mat {{0,1},{2,3}}

ans = a ^ 2                   -->  Mat {{7,10},{15,22}}

-- determinant
ans = a:det()                 -->  -2

-- inverse matrix
e = a:inv()
ans = e[2][1]                 -->  1.5

-- another call of inversion
e = a^-1
ans = e(2,1)                  -->  1.5

-- object copy
-- (it doesn't copy zeros)
f = a:copy()
ans = (f == a)                -->  true

-- element-wise comparison
ans = (a == b)                -->  false

-- identity matrix
ans = Mat:eye(2)              -->  Mat {{1,0},
                                        {0,1}}

-- matrix argument
ans = Mat:eye(a)              -->  Mat {{1,0},
                                        {0,1}}

-- matrix of zeros
ans = Mat:zeros(2,1)          -->  Mat {{0},{0}}

-- matrix of constants = 4
ans = Mat:fill(2,3,4)         -->  Mat {{4,4,4},
                                        {4,4,4}}

-- horizontal concatenation
ans = a .. b                  -->  Mat {{1,2,5,6},
                                        {3,4,7,8}}

-- vertical concatenation
-- (a // b - for short)
ans = a:concat(b,'v')         -->  Mat {{1,2},{3,4},{5,6},{7,8}}

-- apply function of 1 argument
ans = a:map(function (x) return x^2 end)       -->  Mat {{1,4},{9,16}}

-- apply function which depends on index too
ans = a:map(function (x,r,c) return x-r-c end) -->  Mat {{-1,-1},{-0,-0}}

-- apply function to matrices
-- element-wise
fn = function (x,y,z) return x*y+z end
aa = Mat:zip(fn, b,b,b)
ans = aa[1][1]                -->  30

-- use Gauss transform to solve equation
ans = (a .. Mat:V{5,11}):rref()            -->  Mat {{1,0,1},
                                                     {0,1,2}}

-- create vector
ans = Mat:V {1,2,3}           -->  Mat {{1},{2},{3}}

-- get submatrix
g = Mat {
  {1,2,3},
  {4,5,6},
  {7,8,9}
}
-- same as g({2,-1},{2,3})
ans = g({2,-1},{2,3})         -->  Mat {{5,6},
                                        {8,9}}

-- insert elements
gg = Mat:eye(3)
gg({1,2},{1,2}).data = a
ans = gg({1,2},{1,2})   -->  a

-- euclidean norm
ans = Mat:V({1,2,3}):norm()  --3>  math.sqrt(14)

-- random matrix
rnd = function () return math.random() end
h = Mat:zeros(2,3):map(rnd)
print(h)

-- round elements
noize = function (v) return v + math.random()*1E-8 end
hh = a:map(noize)
hh:round(3)
ans = hh                      -->  a

-- pseudo inverse matrix
m = Mat {
  {1,2},
  {3,4},
  {5,6}
}
n = m:pinv()
ans = n(2,2)                 --3>  0.333

-- copy as Lua table
-- (without methametods)
k = Mat:eye(3)
k = k:table()
ans = k[2][1]                 -->  0

-- make diagonal matrix
ans = Mat:D({1,2,3})          -->  Mat {{1,0,0},
                                        {0,2,0},
                                        {0,0,3}}

-- get diagonal
ans = g:diag()                -->  Mat {{1},{5},{9}}

-- cross-product of 2 vectors
x1 = Mat {{1,2,3}}
x2 = Mat {{4,5,6}}
-- to vectors
v1, v2 = x1:vec(), x2:vec()
ans = v1:cross(v2)  -->  Mat {{-3},{6},{-3}}

-- dot product of 2 vectors
ans = v1:dot(v2)              -->  32

-- LU decomposition
l,u,p = b:lu()
ans = l[2][1]                --3>  0.714

-- QR decomposition
q,r = m:qr()
ans = (q*r)[2][2]            --3>  m[2][2]

ans = q:det()                --3>  1.0

-- SVD decomposition
u,s,v = m:svd()
ans = (u*s*v:T())[1][1]      --3>  m[1][1]

-- Cholesky decomposition
m = Mat {{3,1},{1,3}}
m = m:chol()
ans = m[2][2]                --3>  1.633

-- matrix trace
ans = a:tr()                  -->  5

-- extract first row
m = a({},1)
-- vector doesn't need in 2 indices
ans = m(1)                    -->  1

-- extract last column
-- index can be negative
m = a(-1,{})
ans = m(2)                    -->  4

-- get rank
ans = Mat:fill(2,3):rank()    -->  1

-- change size
tmp = Mat{
  {1,2},
  {3,4},
  {5,6}
}
ans = tmp:reshape(2,3)        -->  Mat {{1,2,3},
                                        {4,5,6}}

--]]


--	LOCAL

-- compatibility
local Ver = require("matlib.utils")
local Cnorm, Cfloat = Ver.cross.norm, Ver.cross.float
local Utils = Ver.utils
Ver = Ver.versions

-- Matrix transformations
local tf = require("matlib.matrix_tf")

--- 0 instead nil
local mt_access = { __index = function () return 0 end }

--- Metatable without any operations
local mt_container = {}


--- Simplify object when possible.
--  @param M Matrix.
--  @return Number in the case of single element.
local function nummat(M)
  return M._rows == 1 and M._cols == 1 and M[1][1] or M
end


--- Correct range if possible.
--  @param i Positive or negative index value.
--  @param iRange Available range of indexes.
--  @return Corrected index or nil.
local function toRange(i, iRange)
  if i < 0 then i = i + iRange + 1 end
  return i > 0 and i <= iRange and i or nil
end


--- Add new row to matrix
--  @param t Table (matrix).
--  @param i Index.
--  @return Matrix row.
local function addRow(t, i)
  local row = setmetatable({}, mt_access)
  t[i] = row
  return row
end

local inform = Sonata and Sonata.warning or print

local TRANSFORM = 'transform'
local VECTOR = 'vector'


--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Matrix operations. The matrices are spares by default."
}


--	MODULE

local matrix = {
  type = 'matrix', ismatrix=true,
  -- parameters
  ALIGH_WIDTH = 8,  -- number of columns to aligh width
  CONDITION_NUM = nil,  -- set limit for notification
}


--- Check object type.
--  @param v Object to check.
--  @return True if the object is 'matrix'.
local function ismatrix(v) return getmetatable(v) == matrix end


local function ismatrixex(v) 
  return ismatrix(v) or tf.isref(v)
end


--- M1 + M2
--  @param M1 First matrix or number.
--  @param M2 Second matrix or number.
--  @return Sum matrix.
matrix.__add = function (M1, M2)
  M1 = ismatrixex(M1) and M1 or matrix:fill(
    M2._rows, M2._cols, getmetatable(M1) == mt_container and M1[1] or M1)
  M2 = ismatrixex(M2) and M2 or matrix:fill(
    M1._rows, M1._cols, getmetatable(M2) == mt_container and M2[1] or M2)
  if (M1._rows~=M2._rows or M1._cols~=M2._cols) then
    error("Different matrix size!")
  end
  local res, Mcols = {}, M1._cols
  for r = 1, M1._rows do
    local rr, m1r, m2r = {}, M1[r], M2[r]
    for c = 1, Mcols do rr[c] = m1r[c] + m2r[c] end
    res[r] = rr
  end
  return matrix._init(#res, Mcols, res)
end


--- Simplify call of vectors and range.
--  @param vR Row number or range.
--  @param vC Column number or range (optional).
--  @return Matrix element or submatrix.
matrix.__call = function (self, vR, vC)
  if not vC then
    if self._cols == 1 then 
      return self[vR][1]
    elseif self._rows == 1 then
      return self[1][vR]
    else
      error 'Not a vector'
    end
  end
  local rows, num = {}, false
  if type(vR) == 'number' then
    rows[1] = toRange(vR, self._rows)
    num = true
  else  -- table
    local r1 = toRange(vR[1] or 1, self._rows)
    local rn = toRange(vR[2] or self._rows, self._rows)
    for r = r1, rn, (vR[3] or 1) do rows[#rows+1] = r end
  end
  local cols = {}
  if type(vC) == 'number' then
    cols[1] = toRange(vC, self._cols)
    -- get element
    if num then return self[rows[1]][cols[1]] end
  else  -- table
    local c1 = toRange(vC[1] or 1, self._cols)
    local cn = toRange(vC[2] or self._cols, self._cols)
    for c = c1, cn, (vC[3] or 1) do cols[#cols+1] = c end
  end
  return tf.make_range(self, rows, cols)
end


--- Horizontal concatenation
--  @param M1 First matrix.
--  @param M2 Second matrix.
--  @return Concatenated matrix.
matrix.__concat = function (M1, M2) return matrix.concat(M1, M2, 'h') end


--- M1 / M2
--  @param M1 First matrix or number.
--  @param M2 Second matrix or number.
--  @return Matrix of ratio.
matrix.__div = function (M1, M2)
  if not ismatrixex(M2) then return matrix._kProd(1/M2, M1) end
  return matrix.__mul(M1, matrix.inv(M2))
end


--- M1 == M2
--  @param M1 First matrix.
--  @param M2 Second matrix.
--  @return True if all elements are the same.
matrix.__eq = function (M1, M2)
  if not (ismatrixex(M1) and ismatrixex(M2)) then return false end
  if M1._rows ~= M2._rows or M1._cols ~= M2._cols then return false end
  for r = 1, M1._rows do
    local ar, br = M1[r], M2[r]
    for c = 1, M1._cols do
      if ar[c] ~= br[c] then return false end
    end
  end
  return true
end


--- Vertical concatenation.
--  @param M1 First matrix.
--  @param M2 Second matrix.
--  @return Concatenated matrix.
matrix.__idiv = function (M1, M2) return matrix.concat(M1, M2, 'v') end


--- Metametod for access to elements.
--  @param v Key.
--  @return New matrix row or desired method.
matrix.__index = function (self, v)
  return matrix[v] or (type(v)=='number' and addRow(self, v))
end


--- M1 * M2
--  @param M1 First matrix or number.
--  @param M2 Second matrix or number.
--  @return Result of multiplication.
matrix.__mul = function (M1, M2)
  if not ismatrixex(M1) then return matrix._kProd(M1, M2) end
  if not ismatrixex(M2) then return matrix._kProd(M2, M1) end
  if (M1._cols ~= M2._rows) then
    error("Impossible to get product: different size!")
  end
  local res = {}
  local resCols, m1Cols = M2._cols, M1._cols
  for r = 1, M1._rows do
    local ar, rr = M1[r], {}
    for c = 1, resCols do
      local sum = 0
      for i = 1, m1Cols do sum = sum + ar[i]*M2[i][c] end
      rr[c] = sum
    end
    res[r] = rr
  end
  return nummat(matrix._init(#res, resCols, res))
end


--- M ^ n
--  @param n Natural power or -1.
--  @return Power of the matrix.
matrix.__pow = function (self, N)
  N = assert(Ver.toInteger(N), "Integer is expected!")
  if (self._rows ~= self._cols) then error("Square matrix is expected!") end
  if N == -1 then return matrix.inv(self) end
  local res, acc = matrix:eye(self._rows), matrix.copy(self)
  local mul = matrix.__mul
  while N > 0 do
    if N%2 == 1 then res = mul(res, acc) end
    N = math.modf(N*0.5)
    if N > 0 then acc = mul(acc, acc) end
  end
  return res
end


--- M1 - M2
--  @param M1 First matrix or number.
--  @param M2 Second matrix or number.
--  @return Difference matrix.
matrix.__sub = function (M1, M2)
  M1 = ismatrixex(M1) and M1 or matrix:fill(
    M2._rows, M2._cols, getmetatable(M1) == mt_container and M1[1] or M1)
  M2 = ismatrixex(M2) and M2 or matrix:fill(
    M1._rows, M1._cols, getmetatable(M2) == mt_container and M2[1] or M2)
  if (M1._rows~=M2._rows or M1._cols~=M2._cols) then
    error("Different matrix size!")
  end
  local res, Mcols = {}, M1._cols
  for r = 1, M1._rows do
    local rr, m1r, m2r = {}, M1[r], M2[r]
    for c = 1, Mcols do rr[c] = m1r[c] - m2r[c] end
    res[r] = rr
  end
  return matrix._init(#res, Mcols, res)
end


--- String representation.
--  @return String.
matrix.__tostring = function (self)
  local rows = {}
  -- to strings
  for r = 1, self._rows do
    local row, src = {}, self[r]
    for c = 1, self._cols do
      local tmp = src[c]
      row[c] = (type(tmp) == 'number') and Utils.numstr(tmp) or tostring(tmp)
    end
    rows[r] = row
  end
  -- combine
  if self._rows > 1 and self._cols > 1 and self._cols <= matrix.ALIGH_WIDTH then
    Utils.align(rows, true)
  end
  for i, row in ipairs(rows) do rows[i] = table.concat(row, "  ") end
  return table.concat(rows, '\n')
end


--- - M
--  @return Matrix where each element has opposite sign.
matrix.__unm = function (self)
  local res, Mcols = {}, self._cols
  for r = 1, self._rows do
    local rr, mr = {}, self[r]
    for c = 1, Mcols do rr[c] = -mr[c] end
    res[r] = rr
  end
  return matrix._init(#res, Mcols, res)
end


about['_ar'] = {"arithmetic: a+b, a-b, a*b, a/b, a^b, -a", nil, help.META}
about['_cmp'] = {"comparison: a==b, a~=b", nil, help.META}


--- Initialization of matrix with given size.
--  @param iR Number of rows.
--  @param iC Number of columns.
--  @param t Table for initialization.
--  @return Matrix object.
matrix._init = function (iR, iC, t)
  if iR <= 0 or iC <= 0 then error("Wrong matrix size!") end
  t._cols, t._rows = iC, iR
  return setmetatable(t, matrix)
end


--- Set product of element to coefficient.
--  @param d Coefficient.
--  @param M Matrix.
--  @return Result of multiplication.
matrix._kProd = function (d, M)
  if getmetatable(d) == mt_container then d = d[1] end
  local res, Mcols = {}, M._cols
  for r = 1, M._rows do
    local rr, mr = {}, M[r]
    for c = 1, Mcols do rr[c] = d*mr[c] end
    res[r] = rr
  end
  return matrix._init(#res, Mcols, res)
end


--- Create new matrix from list of tables.
--  @param t Table, where each sub table is a raw of matrix.
--  @return Matrix object.
matrix._new = function (self, t)
  local cols, rows = 0, #t
  for i, v in ipairs(t) do
    if not type(v) == 'table' then error('Row must be a table!') end
    cols = (cols < #v) and #v or cols
    setmetatable(v, mt_access)
  end
  return matrix._init(rows, cols, t)
end


matrix._strip = function (M, tol)
  for r = 1, M._rows do
    local mr = M[r]
    for c = 1, M._cols do
      mr[c] = Utils.strip(mr[c], tol)
    end
  end
end

--- Bidiagonalization.
--  Find such U, B, V that U*B*V:T() = M and
--  B is upper bidiagonal, U and V are ortogonal.
--  @param M Source matrix.
--  @return U, B, V
matrix.bidiag = function (M)
  local m, n, B = M._rows, M._cols, M
  local U, V = matrix:eye(m), matrix:eye(n)
  local w = math.min(m, n)
  for k = 1, w do
    -- set zero to column elements
    local H1 = tf.householder(B({1, m}, {k}), k)
    U, B = U * H1:H(), H1 * B
    if k < (w - 1) then
      local H2 = tf.householder(B({k}, {1, n}):T(), k+1):H()
      B, V = B * H2, V * H2    -- H2 is transposed!
    end
  end
  return U, B, V
end
about[matrix.bidiag] = {"M:bidiag() --> U_M, B_M, V_M",
  "Bidiagonalization of matrix, return U, B, V.", TRANSFORM}


--- Cholesky decomposition.
--  @param M Positive definite symmetric matrix.
--  @return Lower part of the decomposition.
matrix.chol = function (M)
  if M._rows ~= M._cols then error("Square matrix is expected!") end
  local a, p = matrix.copy(M), {}
  -- calculate new values
  for r = 1, a._rows do
    local ar = a[r]
    for c = r, a._cols do
      local sum = ar[c]
      for k = r-1, 1, -1 do sum = sum - ar[k]*a[c][k] end
      if r == c then
        assert(sum >= 0, 'The matrix is not positive definite!')
        p[r] = math.sqrt(sum)
      else
        a[c][r] = sum/p[r]
      end
    end
  end
  -- insert zeros and the main diagonal elements
  for r = 1, a._rows do
    local ar = a[r]
    for c = r+1, a._cols do ar[c] = 0 end
    ar[r] = p[r]
  end
  return a
end
about[matrix.chol] = {"M:chol() --> lower_M",
  "Cholesky decomposition of positive definite symmetric matrix.", TRANSFORM}


--- Get number of columns.
--  @param M Matrix.
--  @return Number of columns.
matrix.cols = function (M) return M._cols end
about[matrix.cols] = {"M:cols() --> N",
  "Get number of columns."}


--- Matrix concatenation.
--  Horizontal concatenation can be performed with
--  <code>..</code>, vertical - <code>//</code>.
--  @param M1 First matrix.
--  @param M2 Second matrix.
--  @param dir Direction of concatenation ('h' for horizontal, 'v' for vertical).
--  @return Concatenated matrix.
matrix.concat = function (M1, M2, sDir)
  local res = nil
  if sDir == 'h' then
    if (M1._rows ~= M2._rows) then error("Different number of rows") end
    res = matrix._init(M1._rows, M1._cols+M2._cols, {})
  elseif sDir == 'v' then
    if (M1._cols ~= M2._cols) then error("Different number of columns") end
    res = matrix._init(M1._rows+M2._rows, M1._cols, {})
  else
    error("Unexpected type of concatenation")
  end
  for r = 1, res._rows do
    local resr = res[r]
    for c = 1, res._cols do
      local src = (r <= M1._rows and c <= M1._cols) and M1 or M2
      local i = (r <= M1._rows) and r or (r - M1._rows)
      local j = (c <= M1._cols) and c or (c - M1._cols)
      resr[c] = src[i][j]
    end
  end
  return res
end
about[matrix.concat] = {"M:concat(M2, dir_s) --> comb_M",
  "Concatenate two matrices, dir='h' - in horizontal direction, dir='v' - in vertical\nUse M1 .. M2 for horizontal concatenation and M1 // M2 for vertical.",
  TRANSFORM}


--- Create copy of matrix.
--  @param M Source matrix.
--  @return Deep copy.
matrix.copy = function (M)
  local res, Mcols = {}, M._cols
  for r = 1, M._rows do
    local rr, mr = {}, M[r]
    for c = 1, Mcols do rr[c] = mr[c] end
    res[r] = rr
  end
  return matrix._init(#res, Mcols, res)
end
about[matrix.copy] = {"M:copy() --> cpy_M",
  "Return copy of matrix.", help.OTHER}


-- Cross product.
matrix.cross = tf.vec_access.cross
about[matrix.cross] = {'V:cross(V2) --> V3',
  'Cross product or two 3-element vectors.', VECTOR}


--- Find determinant.
--  @param M Initial matrix.
--  @return Determinant.
matrix.det = function (M)
  if (M._rows ~= M._cols) then error("Square matrix is expected!") end
  local fn = tf.detList[M._rows]
  if fn then return fn(M) end
  -- in other cases
  local _, K = tf.gaussDown(matrix.copy(M))
  return K
end
about[matrix.det] = {"M:det() --> num", "Calculate determinant."}


--- Get diagonal vector.
--  @param M Matrix.
--  @param v Diagonal elements (optional).
--  @return Vector of matrix.
matrix.diag = function (M)
  local res = {}
  for i = 1, math.min(M._rows, M._cols) do
    res[i] = {M[i][i]}
  end
  return matrix._init(#res, 1, res)
end
about[matrix.diag] = {'M:diag() --> V', 'Get diagonal of the matrix.'}


--- Create matrix with given diagonal elements.
--  @param v List of elements.
matrix.D = function (self, v)
  local vec = ismatrixex(v)
  if vec and (v._rows == 1 or v._cols == 1) or type(v) == 'table' then
    local n = vec and v._rows * v._cols or #v
    local res = matrix._init(n, n, {})
    for i = 1, n do res[i][i] = vec and v(i) or v[i] end
    return res
  end
  return nil
end
about[matrix.D] = {':D(list_v) --> M',
  'Create new matrix with the given diagonal elements.',
  help.NEW}


-- Scalar product.
matrix.dot = tf.vec_access.dot
about[matrix.dot] = {'V:dot(V2) --> num',
  'Scalar product of two vectors.', VECTOR}


--- Find eigenvectors and eigenvalues.
--  @param M Square matrix.
--  @return Matrix with eigenvectors, matrix with eigenvalues in diagonal.
matrix.eig = function (M)
  assert(M._rows == M._cols)
  matrix.ext_poly = matrix.ext_poly or require("matlib.polynomial")
  local p = matrix.ext_poly:char(M)
  local root = p:roots()
  local P, lam = matrix:zeros(M._rows), matrix:zeros(M._rows)
  for j = 1, #root do
    local v = tf.findEigenvector(M, root[j], 1E-4)
    -- save
    for i = 1, M._rows do P[i][j] = v[i][1] end
    lam[j][j] = root[j]
  end
  return P, lam
end
about[matrix.eig] = {'M:eig() --> vectors_M, values_M',
  'Find matrices of eigenvectors and eigenvalues.'}


--- Identity matrix.
--  @param rows Number of rows.
--  @param cols Number of columns. Can be omitted in case of square matrix.
--  @return Diagonal matrix with ones.
matrix.eye = function (self, iR, iC)
  if ismatrixex(iR) then
    iR, iC = iR._rows, iR._cols
  else
    iC = iC or iR
  end
  local m = matrix._init(iR, iC, {})
  for i = 1, math.min(iR, iC) do m[i][i] = 1 end
  return m
end
about[matrix.eye] = {":eye(row_N, col_N=row_N) --> M",
  "Create identity matrix.", help.NEW}


--- Fill matrix with some value.
--  @param iR Number of rows.
--  @param iC Number of columns.
--  @param val Value to set. Default is 1.
--  @return New matrix.
matrix.fill = function (self, iR, iC, val)
  assert(iR > 0 and iC > 0)
  val = val or 1
  local res = {}
  for r = 1, iR do
    local mr = {}
    for c = 1, iC do mr[c] = val end
    res[r] = mr
  end
  return matrix._init(iR, iC, res)
end
about[matrix.fill] = {":fill(row_N, col_N, val=1) --> M",
  "Create matrix of given numbers (default is 1).", help.NEW}


--- Kronecker product.
--  @param M1 First matrix.
--  @param M2 Second matrix.
--  @return matrix, obtained with Kronecker product.
matrix.kron = function (M1, M2)
  local res = matrix._init(M1._rows*M2._rows, M1._cols*M2._cols, {})
  for i = 1, M1._rows do
    for j = 1, M1._cols do
      local v = M1[i][j]
      if v ~= 0 then
        local rr, cc = (i-1)*M2._rows, (j-1)*M2._cols
        for p = 1, M2._rows do
          local mp = M2[p]
          local resr = res[rr + p]
          for q = 1, M2._cols do resr[cc + q] = v * mp[q] end
        end
      end
    end
  end
  return res
end
about[matrix.kron] = {"M:kron(M2) --> M3", "Find Kronecker product."}


--- Kronecker sum.
--  @param M1 First matrix.
--  @param M2 Second matrix.
--  @return matrix, obtained with Kronecker sum.
matrix.kronSum = function (M1, M2)
  if M1._rows ~= M1._cols or M2._rows ~= M2._cols then
    error('Square matrices expected')
  end
  return matrix.kron(M1, matrix:eye(M2)) + matrix.kron(matrix:eye(M1), M2)
end
about[matrix.kronSum] = {"M:kronSum(M2) --> M3", "Find Kronecker sum."}


--- Round matrix elements in place.
--  @param M Matrix object.
--  @param N Number of digits.
matrix.round = function(M, N)
  N = N or 6
  local tol = 10^(-N)
  for r = 1, M._rows do
    local mr = M[r]
    for c = 1, M._cols do
      local v = mr[c]
      if type(v) == 'number' then
        mr[c] = Utils.round(v, tol)
      elseif v.iscomplex then
        v = v:round(N)
        mr[c] = (v:im() == 0) and v:re() or v
      elseif Cnorm(v) < tol then
        mr[c] = 0
      end
    end
  end
end
about[matrix.round] = {"M:round(N=6)",
  "Round matrix elements in place.", help.OTHER}


--- Conjugate transpose.
--  @param M Initial matrix.
--  @return Conjugate transformed matrix object.
matrix.H = function (M) return tf.make_t(M, true) end
about[matrix.H] = {"M:H() --> conj_M",
  "Return conjugabe transpose. ", TRANSFORM}


--- Inverse matrix.
--  @param M Initial matrix.
--  @return Result of inversion.
matrix.inv = function (M)
  if M._rows ~= M._cols then error("Square matrix is expected!") end
  local size = M._cols
  -- check simple cases
  local fn = tf.invList[size]
  if fn then
    local det = tf.detList[size](M)
    return (det ~= 0) and matrix._kProd(1/det, fn(M))
                       or matrix:fill(size, size, math.huge)
  end
  -- prepare matrix
  local res, det = matrix.copy(M), nil
  -- add "tail"
  for i = 1, size do
    local resi = res[i]
    setmetatable(resi, mt_access)
    resi[i+size] = 1
  end
  res._cols = 2*size
  res, det = tf.gaussDown(res)
  if det == 0 then
    return matrix:fill(size, size, math.huge)
  end
  res = tf.gaussUp(res)
  -- move result
  for r = 1, size do
    local resr = res[r]
    for c = 1, size do
      local p = size+c
      resr[c], resr[p] = resr[p], nil
    end
  end
  res._cols = size
  if matrix.CONDITION_NUM then
    local cn = matrix.norm(M) * matrix.norm(res)
    if cn >= matrix.CONDITION_NUM then
      inform("Condition number is "..tostring(cn))
    end
  end
  return res
end
about[matrix.inv] = {"M:inv() --> inv_M", "Return inverse matrix.", TRANSFORM}


-- "In the game of life the strong survive..." (Scorpions) ;)
--  board - matrix with 'ones' as live cells
matrix.life = function (board)
  local src = board
  local gen = 0
  -- make decision about current cell
  local function islive (r, c)
    local n = src[r-1][c-1] + src[r][c-1] + src[r+1][c-1] + src[r-1][c]
      + src[r+1][c] + src[r-1][c+1] + src[r][c+1] + src[r+1][c+1]
    return (n==3 or n==2 and src[r][c]==1) and 1 or 0
  end
  -- evaluate
  repeat
    local new = matrix:zeros(board)   -- empty matrix of the same size
    gen = gen+1
    -- update
    for r = 1, board._rows do
      for c = 1, board._cols do
        new[r][c] = gen > 1 and islive(r, c) or src[r][c] ~= 0 and 1 or 0
      end
    end
    if gen > 1 and new == src then
      local msg = '~~ No more steps ~~'
      return Sonata and Sonata.IN_COROUTINE and msg or print(msg)
    end
    src = new
    local req = string.format('step %d continue? (y/n) ', gen)
    local resp
    if Sonata and Sonata.IN_COROUTINE then
      resp = Sonata.ask(req, new:stars())
    else
      print(new:stars())
      io.write(req)
      resp = io.read()
    end
  until 'n' == resp
end


--- LU transform
--  @param M Initial square matrix.
--  @return L matrix, U matrix, permutations
matrix.lu = function (M)
  local a, _, d = tf.luPrepare(M)
  local p = matrix:eye(M._rows, M._cols)
  local move = Ver.move
  while d > 0 do
    local tmp = p[1]; move(p, 2, p._rows, 1); p[p._rows] = tmp  -- shift
    d = d-1
  end
  return
    -- lower
    matrix.map(a,
      function (M, r, c) return (r==c) and 1.0 or (r>c and M or 0) end),
    -- upper
    matrix.map(a, function (M, r, c) return r <= c and M or 0 end),
    p  -- permutations
end
about[matrix.lu] = {"M:lu() --> L_M, U_M, perm_M",
  "LU decomposition for the matrix. Return L,U and P matrices.", TRANSFORM}


--- Apply function to each element.
--  @param M Source matrix.
--  @param fn Desired function.
--  @return Matrix where each element is obtained based on desired function.
matrix.map = function (M, fn)
  local res, Mcols = {}, M._cols
  for r = 1, M._rows do
    local rr, mr = {}, M[r]
    for c = 1, Mcols do rr[c] = fn(mr[c], r, c) end
    res[r] = rr
  end
  return matrix._init(#res, Mcols, res)
end
about[matrix.map] = {"M:map(fn) --> found_M",
  "Apply the given function to all elements, return new matrix. Function can be in form f(x) or f(x,row,col).",
  TRANSFORM}


--- Find minor for the matrix element.
--  @param M Source matrix.
--  @param ir Row index.
--  @param ic Column index.
--  @return minor matrix.
matrix.minor = function (M, ir, ic)
  assert(M._rows == M._cols)
  if ir > 0 and ic > 0 and ir <= M._rows and ic <= M._cols then
    return tf.firstMinor(tf.firstMinorSub(M, ir, ic))
  else
    -- determinant via minors
    return tf.firstMinor(M)
  end
end
about[matrix.minor] = {"M:minor(row_N, col_N) --> minor_M",
  "Find minor for the matrix element."}


--- Euclidean norm of the matrix at whole.
--  @param M Current matrix.
--  @return Norm value.
matrix.norm = function (M)
  local sum = 0
  for r = 1, M._rows do
    local mr = M[r]
    for c = 1, M._cols do
      sum = sum + Cnorm(mr[c])^2
    end
  end
  return math.sqrt(sum)
end
about[matrix.norm] = {"M:norm() --> num", "Euclidean norm."}


--- Quick pseudo inverse matrix.
--  Based on "Fast computation of Moore-Penrose inverse matrices"
--  paper by Pierre Courrieu.
--  @param M Initial matrix.
--  @return Pseudo inverse matrix.
matrix.pinv = function (M)
  local m, n, transp = M._rows, M._cols, false
  local Mt, A = M:T(), nil
  if m < n then
    A, n, transp = matrix.__mul(M, Mt), m, true
  else
    A = matrix.__mul(Mt, M)
  end
  local tol = math.huge
  for i = 1, A._rows do
    local v = Cnorm(A[i][i])
    tol = math.min(tol, (v > 0 and v or math.huge))
  end
  tol = tol * 1e-9
  local L, r, tmp = matrix:zeros(A._rows, A._cols), 0, nil
  for k = 1, n do
    r = r + 1
    local B = A({k, n}, {k})
    if r > 1 then
      tmp = L({k, n}, {1, r-1}) * L({k}, {1, r-1}):T()
      -- product can be scalar
      tmp = ismatrixex(tmp) and tmp or matrix._init(1, 1, {{tmp}})
      B = B - tmp
    end
    for i = k, n do L[i][r] = B[i-k+1][1] end  -- copy B to L
    tmp = L[k][r]
    local iscomplex = (type(tmp)=='table') and tmp.iscomplex
    if iscomplex and tmp:abs() > tol then
      tmp = tmp:sqrt()
      L[k][r] = tmp
      for i = k+1, n do L[i][r] = L[i][r]/tmp end
    elseif not iscomplex and tmp > tol then
      tmp = math.sqrt(Cfloat(tmp))
      L[k][r] = tmp
      for i = k+1, n do L[i][r] = L[i][r]/tmp end
    else
      r = r - 1
    end
  end
  L._cols = (r > 0) and r or 1
  local Lt = L:T()
  local K = matrix.inv(Lt * L)
  if transp then
    return Mt * L * K * K * Lt
  end
  return L * K * K * Lt * Mt
end
about[matrix.pinv] = {"M:pinv() --> inv_M",
  "Pseudo inverse matrix calculation.", TRANSFORM}


--- QR transformation
--  @param M Matrix, #rows >= #cols.
--  @return Q and R matrices.
matrix.qr = function (M)
  local m, n = M._rows, M._cols
  if n > m then error("Wrong matrix size") end
  local Q = matrix:eye(m)
  local R = matrix.copy(M)
  local v = matrix._init(m, 1, {})
  for j = 1, math.min(m-1, n) do
    -- housholder transformation
    -- get vector
    local k = 1
    for i = j, m do
      v[k][1] = R[i][j]
      k = k + 1
    end
    v._rows = k - 1
    -- prepare v and v:T()
    local v11 = v[1][1]
    local v11abs = Cnorm(v11)
    local s = (v11abs > 0) and (v11 / v11abs) or 1
    v[1][1] = v11 + s * v:norm()
    local vnorm = v:norm()
    for r = 1, v._rows do v[r][1] = v[r][1] / vnorm end
    local vt = v:H():copy()
    for r = 1, v._rows do v[r][1] = 2 * v[r][1] end
    -- update R
    local vvr = v * (vt * R({j, m}, {j, n}))
    local j1 = j - 1
    for r = j, m do
      for c = j, n do R[r][c] = R[r][c] - vvr[r-j1][c-j1] end
    end
    -- update Q
    local qvv = (Q({1, m}, {j, m}) * v) * vt
    for r = 1, m do
      for c = j, m do Q[r][c] = Q[r][c] - qvv[r][c-j1] end
    end
  end
  -- set zeros
  for c = 1, n do
    for r = c+1, m do R[r][c] = 0 end
  end
  return Q, R
end
about[matrix.qr] = {"M:qr() --> Q_M, R_M",
  "QR decomposition of the matrix.", TRANSFORM}


--- Matrix rank.
--  @param M Initial matrix.
--  @return Value of rank.
matrix.rank = function (M)
  local mat = tf.gaussDown(matrix.copy(M))
  local i = 1
  while i <= mat._rows do
    local mati = mat[i]
    if not mati then break end
    -- find nonzero element
    local zeros = true
    for j = i, mat._cols do
      if mati[j] ~= 0 then
        zeros = false
        break
      end
    end
    if zeros then break end
    i = i+1
  end
  return i-1
end
about[matrix.rank] = {"M:rank() --> N", "Find rank of the matrix."}


--- Change matrix size.
--  @param M Source matrix.
--  @param iRows New number of rows.
--  @param iCols New number of columns.
--  @return Matrix with new size.
matrix.reshape = function (M, iRows, iCols)
  iRows = iRows or (M._rows*M._cols)
  iCols = iCols or 1
  local res = matrix._init(iRows, iCols, {})
  local newR, newC = 1, 1   -- temporary indices
  for r = 1, M._rows do
    local Mr = M[r]
    for c = 1, M._cols do
      res[newR][newC] = Mr[c]
      newC = newC+1
      if newC > iCols then
        newC = 1
        newR = newR+1
      end
    end
    if newR > iRows then break end
  end
  return res
end
about[matrix.reshape] = {"M:reshape(row_N=size, col_N=1) --> upd_M",
  "Get matrix with changed size.", help.OTHER}


--- Get number or rows.
--  @param M Matrix.
--  @return Number of rows.
matrix.rows = function (M) return M._rows end
about[matrix.rows] = {"M:rows() --> N", "Get number of rows."}


--- Solve system of equations using Gauss method.
--  @param M Matrix representation for system of equations.
--  @return Transformed matrix.
matrix.rref = function (M)
  return tf.gaussUp(tf.gaussDown(M))
end
about[matrix.rref] = {"M:rref() --> upd_M",
  "Perform transformations using Gauss method."}


--- Visualize matrix elements
--  @param fn Condition function, returns true/false.
--  @return String with stars when condition is true.
matrix.stars = function (self, fn)
  fn = fn or function (x) return x ~= 0 end
  local acc, row = {}, {}
  for r = 1, self._rows do
    local mr = self[r]
    for c = 1, self._cols do
      row[c] = fn(mr[c]) and '*' or ' '
    end
    row[self._cols+1] = '|'
    acc[r] = table.concat(row)
  end
  return table.concat(acc, '\n')
end
about[matrix.stars] = {"M:star(cond_fn) --> str",
  "Print star when condition for the current elemen is true.", help.OTHER}


--- Singular value decomposition for a matrix.
--  Find U, S, V such that M = U*S*V' and
--  U, V are orthonormal, S is diagonal matrix.
--  @param M Source matrix.
--  @return Matrices U, S, V.
matrix.svd = function (M)
  local transpose = M._rows < M._cols
  if transpose then M = M:T() end
  -- main steps
  local U1, B, V1 = matrix.bidiag(M)
  local U2, V2 = matrix:eye(U1), matrix:eye(V1)
  local E, U3, V3 = math.huge, nil, nil
  while E > 1E-8 do
    U3, B, V3, E = tf.qrSweep(B)
    U2, V2 = U2 * U3, V2 * V3
  end
  U1, V1 = U1 * U2, V1 * V2
  if transpose then
    U1, B, V1 = V1, B:T(), U1
  end
  -- remove zeros
  local B1 = matrix:zeros(B._rows, B._cols)
  for i = 1, V1._rows do
    local s = B[i][i]
    if s < 0 then
      s = -s  -- correct sign (TODO try to avoid it)
      --  and column elements
      for j = 1, V1._rows do V1[j][i] = -V1[j][i] end
    end
    B1[i][i] = s
  end
  return U1, B1, V1
end
about[matrix.svd] = {"M:svd() --> U_M, S_M, V_M",
  "Singular value decomposition, return U, S, V.", TRANSFORM}


--- Matrix to table.
--  @param M Source matrix.
--  @return Table without metametods.
matrix.table = function (M)
  local res = {}
  -- simplify 'vector' representation
  if M._rows == 1 then
    for c = 1, M._cols do res[c] = M[1][c] end
  elseif M._cols == 1 then
    for r = 1, M._rows do res[r] = M[r][1] end
  -- full matrix
  else
    for r = 1, M._rows do
      local resr, mr = {}, M[r]
      for c = 1, M._cols do resr[c] = mr[c] end
      res[r] = resr
    end
  end
  return res
end
about[matrix.table] = {"M:table() --> tbl",
  "Convert to simple Lua table.", help.OTHER}


--- Get trace of the matrix.
--  @param m Source matrix.
--  @return Sum of elements of the main diagonal.
matrix.tr = function (M)
  local sum = 0
  for i = 1, math.min(M._rows, M._cols) do sum = sum + M[i][i] end
  return sum
end
about[matrix.tr] = {"M:tr() --> sum", "Get trace of the matrix.", help.OTHER}


--- Transpose matrix.
--  @param M Initial matrix.
--  @return Transposed matrix reference.
matrix.T = function (M) return tf.make_t(M) end
about[matrix.T] = {"M:T() --> transpose_M",
  "Return matrix transpose.", TRANSFORM}


--- Create column vector.
--  Simplified vector constructor.
--  @param t Table with vector elements.
--  @return Vector form of matrix.
matrix.V = function (self, t) return tf.make_vec(t) end
about[matrix.V] = {":V {...} --> new_V",
  "Create vector from list of numbers.", help.NEW}


matrix.vec = function (self) return tf.make_vec_access(self) end

--- Stack columns into the single vector.
--  @param M Source matrix.
--  @return column vector.
matrix.vectorize = function (M)
  local res = {}
  for c = 1, M._cols do
    for r = 1, M._rows do
      res[#res+1] = {M[r][c]}  -- TODO save nonzero only?
    end
  end
  return matrix._init(#res, 1, res)
end
about[matrix.vectorize] = {"M:vectorize() --> V",
  "Create vector as a stack of columns.", TRANSFORM}


--- Create matrix of zeros.
--  @param nR Number of rows.
--  @param nC Number of columns. Can be omitted in case of square matrix.
--  @return Sparse matrix.
matrix.zeros = function (self, iR, iC)
  if ismatrixex(iR) then iR, iC = iR._rows, iR._cols end  -- input is a matrix
  iC = iC or iR                          -- input is a number
  return matrix._init(iR, iC, {})
end
about[matrix.zeros] = {":zeros(row_N, col_N=row_N) --> M",
  "Create matrix of zeros.", help.NEW}


--- Apply function element-wise to matrices.
--  @param fn Function of N arguments.
--  @param ... List of N matrices.
--  @return New matrix.
matrix.zip = function (self, fn, ...)
  local arg = {...}
  local rows, cols = arg[1]._rows, arg[1]._cols
  -- check size
  for i = 2, #arg do
    if arg[i]._rows ~= rows or arg[i]._cols ~= cols then
      error("Different size!")
    end
  end
  local res, v = {}, {}
  -- evaluate
  local upack = Ver.unpack
  for r = 1, rows do
    local rr = {}
    for c = 1, cols do
      -- collect
      for k = 1, #arg do v[k] = arg[k][r][c] end
      -- calc
      rr[c] = fn(upack(v))
    end
    res[r] = rr
  end
  return matrix._init(rows, cols, res)
end
about[matrix.zip] = {':zip(fn, M1, M2,..) --> res_M',
  'Apply function to the given matrices element-wise.', TRANSFORM}


-- constructor call
setmetatable(matrix, {__call = matrix._new})
about[matrix] = {" {row1_t, row2_t,..} --> new_M",
  "Create matrix from list of strings (tables).", help.NEW}


tf.init_ref(matrix)


-- Comment to remove descriptions
matrix.about = about


return matrix

--=========================
--TODO: check SVD with complex numbers
--TODO: change matrix print
--TODO: matrix from list and size
--TODO: Fix eigenvectors for complex eigenvalues.
--TODO: single Householder algorithm
