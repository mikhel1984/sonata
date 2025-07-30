--[[		sonata/lib/matrix.lua

--- Matrix operations. Indexation from 1.
--
--  Object structure: </br>
--  <code>{{x11...x1n}, {x21...x2n}...{xm1...xmn}</br>
--  _rows=m, _cols=n} </code></br>
--  Internal elements can be empty tables, but anyway
--  operation X[i][j] will return 0.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2025.

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

-- set new value
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
ans = Mat:hor {a, b}          -->  Mat {{1,2,5,6},
                                        {3,4,7,8}}

-- vertical concatenation
ans = Mat:ver {a, b}          -->  Mat {{1,2},{3,4},{5,6},{7,8}}

-- apply function of 1 argument
ans = a:map(function (x) return x^2 end)       -->  Mat {{1,4},{9,16}}

-- apply function as string
ans = a:map "x^2"            -->  Mat {{1,4},{9,16}}

-- apply function to matrices
-- element-wise
fn = function (x,y,z) return x*y+z end
aa = Mat:zip(fn, b,b,b)
ans = aa[1][1]                -->  30

-- use Gauss transform to solve equation
ans = (a .. Mat:V{5,11}):rref()  -->  Mat {{1,0,1},
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

-- outer product
vout = v1:outer(v2)
ans = vout[1][3]              -->  6

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

-- matrix exponential
m = a:exp()
ans = m[2][2]                --1> 164.1

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

-- dependencies
local Ver = require("matlib.utils")
local Cnorm, Cfloat, Cround = Ver.cross.norm, Ver.cross.float, Ver.cross.round
local Czero = Ver.cross.isZero
local Utils = Ver.utils
Ver = Ver.versions

-- Matrix transformations
local tf = require("matlib.matrix_tf")

--- 0 instead nil
local mt_access = {
  __index = function () return 0 end,
  __len = function () return end,
}


--- Simplify object when possible.
--  @param M Matrix.
--  @return Number in the case of single element.
local function _nummat(M)
  return M._rows == 1 and M._cols == 1 and M[1][1] or M
end


--- Correct range if possible.
--  @param i Positive or negative index value.
--  @param iRange Available range of indexes.
--  @return Corrected index or nil.
local function _toRange(i, iRange)
  if i < 0 then i = i + iRange + 1 end
  return i > 0 and i <= iRange and i or nil
end


--- Add new row to matrix
--  @param t Table (matrix).
--  @param i Index.
--  @return Matrix row.
local function _addRow(t, i)
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
__module__ = "Matrix operations. The matrices are spares by default. Indexation from 1."
}


--	MODULE

local matrix = {
  type = 'matrix', ismatrix=true,
  -- parameters
  ALIGN_WIDTH = 8,  -- number of columns to aligh width
  CONDITION_NUM = nil,  -- set limit for notification
  --STRIP = 1E-12,

  __len = mt_access.__len,
}


--- Check object type.
--  @param v Object to check.
--  @return True if the object is 'matrix' or reference.
local function _ismatrixex(v)
  return getmetatable(v) == matrix or tf.isref(v)
end


--- Set product of element to coefficient.
--  @param d Coefficient.
--  @param M Matrix.
--  @return Result of multiplication.
local function _kProd (d, M)
  local res, Mcols = {}, M._cols
  for r = 1, M._rows do
    local rr, mr = {}, M[r]
    for c = 1, Mcols do rr[c] = d*mr[c] end
    res[r] = rr
  end
  return matrix._init(#res, Mcols, res)
end


--- M1 + M2
--  @param M1 First matrix or number.
--  @param M2 Second matrix or number.
--  @return Sum matrix.
matrix.__add = function (M1, M2)
  M1 = _ismatrixex(M1) and M1 or matrix:fill(M2._rows, M2._cols, M1)
  M2 = _ismatrixex(M2) and M2 or matrix:fill(M1._rows, M1._cols, M2)
  if (M1._rows~=M2._rows or M1._cols~=M2._cols) then
    error "Different matrix size!"
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
    rows[1] = _toRange(vR, self._rows)
    num = true
  else  -- table
    local r1 = _toRange(vR[1] or 1, self._rows)
    local rn = _toRange(vR[2] or self._rows, self._rows)
    for r = r1, rn, (vR[3] or 1) do rows[#rows+1] = r end
  end
  local cols = {}
  if type(vC) == 'number' then
    cols[1] = _toRange(vC, self._cols)
    -- get element
    if num then return self[rows[1]][cols[1]] end
  else  -- table
    local c1 = _toRange(vC[1] or 1, self._cols)
    local cn = _toRange(vC[2] or self._cols, self._cols)
    for c = c1, cn, (vC[3] or 1) do cols[#cols+1] = c end
  end
  return tf.makeRange(self, rows, cols)
end


--- Simplify horizontal concatenation of two matrices.
--  @param M1 First matrix.
--  @param M2 Second matrix.
--  @return Concatenated matrix.
matrix.__concat = function (M1, M2)
  return tf.makeConcat({M1, M2}, false):copy()
end


--- M1 == M2
--  @param M1 First matrix.
--  @param M2 Second matrix.
--  @return True if all elements are the same.
matrix.__eq = function (M1, M2)
  if not (_ismatrixex(M1) and _ismatrixex(M2)) then return false end
  if M1._rows ~= M2._rows or M1._cols ~= M2._cols then return false end
  for r = 1, M1._rows do
    local ar, br = M1[r], M2[r]
    for c = 1, M1._cols do
      if ar[c] ~= br[c] then return false end
    end
  end
  return true
end


--- Metametod for access to elements.
--  @param v Key.
--  @return New matrix row or desired method.
matrix.__index = function (self, v)
  return matrix[v] or (type(v)=='number' and _addRow(self, v))
end


--- M1 * M2
--  @param M1 First matrix or number.
--  @param M2 Second matrix or number.
--  @return Result of multiplication.
matrix.__mul = function (M1, M2)
  if not _ismatrixex(M1) then return _kProd(M1, M2) end
  if not _ismatrixex(M2) then return _kProd(M2, M1) end
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
  return _nummat(matrix._init(#res, resCols, res))
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
  M1 = _ismatrixex(M1) and M1 or matrix:fill(M2._rows, M2._cols, M1)
  M2 = _ismatrixex(M2) and M2 or matrix:fill(M1._rows, M1._cols, M2)
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
  if self._rows > 1 and self._cols > 1 and self._cols <= matrix.ALIGN_WIDTH then
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


about['_ar'] = {"arithmetic: a+b, a-b, a*b, a^b, -a", nil, help.META}
about['_cmp'] = {"comparison: a==b, a~=b", nil, help.META}


--- Initialization of matrix with given size.
--  @param iR Number of rows.
--  @param iC Number of columns.
--  @param t Table for initialization.
--  @return Matrix object.
matrix._init = function (iR, iC, t)
  if iR <= 0 or iC <= 0 then
    error "Wrong matrix size!"
  end
  t._cols, t._rows = iC, iR
  local res = setmetatable(t, matrix)
  return matrix.STRIP and tf.clearLess(res, matrix.STRIP) or res
end


--- Create new matrix from list of tables.
--  @param t Table, where each sub table is a raw of matrix.
--  @return Matrix object.
matrix._new = function (self, t)
  if _ismatrixex(t) then
    return t
  elseif type(t) == 'number' or type(t) == 'table' and t.__mul then
    return matrix._init(1, 1, {{t}})
  end
  local cols, rows = 0, #t
  for _, v in ipairs(t) do
    if type(v) ~= 'table' then
      error 'Row must be table!'
    end
    cols = (cols < #v) and #v or cols
    setmetatable(v, mt_access)
  end
  return matrix._init(rows, cols, t)
end


--- Strip matrix components.
--  @param tol Required tolerance.
--  @return rounded matrix.
matrix._round = function (self, tol)
  for i = 1, self._rows do
    local row = rawget(self, i)
    if row then
      for j = 1, self._cols do
        local v = rawget(row, j)
	if v then row[j] = Cround(v, tol) end
      end
    end
  end
  return self
end


--- Cholesky decomposition.
--  @param M Positive definite symmetric matrix.
--  @return Lower part of the decomposition.
matrix.chol = function (self)
  if self._rows ~= self._cols then
    error "Square matrix is expected!"
  end
  local L = matrix:zeros(self._rows, self._cols)
  for i = 1, self._rows do
    local Li, Ai = L[i], self[i]
    for j = 1, i do
      local sum = 0
      for k = 1, j-1 do
        local v = L[j][k]
        v = (type(v) == 'table' and v.conj) and v:conj() or v
        sum = sum + v*Li[k]
      end
      sum = Ai[j] - sum   -- reuse
      if j < i then
        Li[j] = sum / L[j][j]
      else
        if sum < 0 then return nil end  -- not positive definite
        Li[j] = math.sqrt(sum)
      end
    end
  end
  return L
end
about[matrix.chol] = {"M:chol() --> lower_M|nil",
  "Cholesky decomposition of positive definite symmetric matrix.", TRANSFORM}


--- Get number of columns.
--  @return Number of columns.
matrix.cols = function (self) return self._cols end
about[matrix.cols] = {"M:cols() --> N", "Get number of columns."}


--- Create copy of matrix.
--  @return Deep copy.
matrix.copy = function (self)
  local res, Mcols = {}, self._cols
  for r = 1, self._rows do
    local rr, mr = {}, self[r]
    for c = 1, Mcols do rr[c] = mr[c] end
    res[r] = rr
  end
  return matrix._init(#res, Mcols, res)
end
about[matrix.copy] = {"M:copy() --> cpy_M",
  "Return copy of matrix.", help.OTHER}


-- Cross product.
matrix.cross = tf.vec_access.cross
about[matrix.cross] = {'V:cross(V2) --> M',
  'Cross product of two 3-element vectors.', VECTOR}


--- Find determinant.
--  @return Determinant.
matrix.det = function (self)
  if (self._rows ~= self._cols) then
    error "Square matrix is expected!"
  end
  local fn = tf.detList[self._rows]
  if fn then return fn(self) end
  -- in other cases
  local _, K = tf.gaussDown(matrix.copy(self))
  return K
end
about[matrix.det] = {"M:det() --> num", "Calculate determinant."}


--- Get diagonal vector.
--  @param v Diagonal elements (optional).
--  @return Vector of matrix.
matrix.diag = function (self)
  local res = {}
  for i = 1, math.min(self._rows, self._cols) do
    res[i] = {self[i][i]}
  end
  return matrix._init(#res, 1, res)
end
about[matrix.diag] = {'M:diag() --> V', 'Get diagonal of the matrix.'}


--- Create matrix with given diagonal elements.
--  @param v List of elements.
matrix.D = function (_, v, shift)
  shift = shift or 0
  local vec = _ismatrixex(v)
  if vec and (v._rows == 1 or v._cols == 1) or type(v) == 'table' then
    local n = vec and v._rows * v._cols or #v
    local res
    if shift >= 0 then
      res = matrix._init(n + shift, n + shift, {})
      for i = 1, n do res[i][i+shift] = vec and v(i) or v[i] end
    else
      res = matrix._init(n - shift, n - shift, {})
      for i = 1, n do res[i-shift][i] = vec and v(i) or v[i] end
    end
    return res
  end
  return nil
end
about[matrix.D] = {':D(list_v, shift_N=0) --> M',
  'Create new matrix with the given diagonal elements.', help.NEW}


-- Scalar product.
matrix.dot = tf.vec_access.dot
about[matrix.dot] = {'V:dot(V2) --> num',
  'Scalar product of two vectors.', VECTOR}


--- Find eigenvectors and eigenvalues.
--  @return Matrix with eigenvectors, matrix with eigenvalues in diagonal.
matrix.eig = function (self)
  if self._rows ~= self._cols then
    error "Square matrix is expected!"
  end
  matrix.ext_poly = matrix.ext_poly or require("matlib.polynomial")
  local p = matrix.ext_poly:char(self)
  local root = p:roots()
  local P, lam = matrix:zeros(self._rows), matrix:zeros(self._rows)
  for j = 1, #root do
    local v = tf.findEigenvector(self, root[j], 1E-4)
    -- save
    for i = 1, self._rows do P[i][j] = v[i][1] end
    lam[j][j] = root[j]
  end
  return P, lam
end
about[matrix.eig] = {'M:eig() --> vectors_M, values_M',
  'Find matrices of eigenvectors and eigenvalues.'}


--- Find matrix exponential, e^M
--  @return found matrix.
matrix.exp = function (self)
  local U, D = matrix.eig(self)
  local Ui = matrix.inv(U)
  for i = 1, D._rows do
    local v = D[i][i]
    if type(v) == 'table' then
      v = v.exp and v:exp() or math.exp(v:float())
    else
      v = math.exp(v)
    end
    -- U * D
    for j = 1, D._rows do U[j][i] = U[j][i] * v end
  end
  return U * Ui
end
about[matrix.exp] = {"M:exp() --> new_M", "Matrix exponential.", TRANSFORM}


--- Identity matrix.
--  @param rows Number of rows.
--  @param cols Number of columns. Can be omitted in case of square matrix.
--  @return Diagonal matrix with ones.
matrix.eye = function (_, iR, iC)
  if _ismatrixex(iR) then
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
matrix.fill = function (_, iR, iC, val)
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


--- Conjugate transpose.
--  @return Conjugate transformed matrix object.
matrix.H = function (self) return tf.makeT(self, true) end
about[matrix.H] = {"M:H() --> conj_Ref",
  "Return conjugabe transpose. ", TRANSFORM}


--- Horizonatal concatenation of matrices.
--  @param lst List of matrices.
--  @return concatenated matrix object.
matrix.hor = function (self, lst) return tf.makeConcat(lst, false) end
about[matrix.hor] = {":hor(mat_t) --> mat_Ref",
  "Horizontal concatenation for the given list of matrices.", "concat"}


--- Inverse matrix.
--  @return Result of inversion.
matrix.inv = function (self)
  if self._rows ~= self._cols then
    error "Square matrix is expected!"
  end
  local size = self._cols
  -- check simple cases
  local fn = tf.invList[size]
  if fn then
    local det = tf.detList[size](self)
    return (not Czero(det)) and _kProd(1/det, fn(self))
                       or matrix:fill(size, size, math.huge)
  end
  -- prepare matrix
  local res, det = matrix.copy(self), nil
  -- add "tail"
  for i = 1, size do
    local resi = res[i]
    setmetatable(resi, mt_access)
    resi[i+size] = 1
  end
  res._cols = 2*size
  res, det = tf.gaussDown(res)
  if Czero(det) then
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
    local cn = matrix.norm(self) * matrix.norm(res)
    if cn >= matrix.CONDITION_NUM then
      inform("Condition number is "..tostring(cn))
    end
  end
  return res
end
about[matrix.inv] = {"M:inv() --> inv_M", "Return inverse matrix."}


--- Kronecker product.
--  @param M Second matrix.
--  @return matrix, obtained with Kronecker product.
matrix.kron = function (self, M)
  local res = matrix._init(self._rows*M._rows, self._cols*M._cols, {})
  for i = 1, self._rows do
    for j = 1, self._cols do
      local v = self[i][j]
      if v ~= 0 then
        local rr, cc = (i-1)*M._rows, (j-1)*M._cols
        for p = 1, M._rows do
          local mp = M[p]
          local resr = res[rr + p]
          for q = 1, M._cols do resr[cc + q] = v * mp[q] end
        end
      end
    end
  end
  return res
end
about[matrix.kron] = {"M:kron(M2) --> M⊗M2", "Find Kronecker product."}


--- Kronecker sum.
--  @param M Second matrix.
--  @return matrix, obtained with Kronecker sum.
matrix.kronSum = function (self, M)
  if self._rows ~= self._cols or M._rows ~= M._cols then
    error('Square matrices expected')
  end
  return matrix.kron(self, matrix:eye(M)) + matrix.kron(matrix:eye(self), M)
end
about[matrix.kronSum] = {"M:kronSum(M2) --> M⊕M2", "Find Kronecker sum."}


--- LU transform
--  @return L matrix, U matrix, permutations
matrix.lu = function (self)
  if self._rows ~= self._cols then
    error "Square matrix is expected!"
  end
  -- check square
  local U, P = self:copy(), matrix:eye(self._rows, self._cols)
  local L = matrix:eye(self._rows, self._cols)
  for i = 1, self._rows do
    -- swap with maximum
    local k, max = i, Cnorm(U[i][i])
    for j = i+1, U._rows do
      local uj = Cnorm(U[j][i])
      if uj > max then
        k, max = j, uj
      end
    end
    for j = k-1, i, -1 do
      U[j], U[j+1] = U[j+1], U[j]
      P[j], P[j+1] = P[j+1], P[j]
    end
    -- fill U, L
    local Ui = U[i]
    if not Czero(Ui[i]) then
      for j = i + 1, U._rows do
        local Uj = U[j]
        local t = Uj[i] / Ui[i]
        L[j][i] = t
        for c = i, U._cols do Uj[c] = Uj[c] - t * Ui[c] end
      end
    end
  end
  return L, U, P
end
about[matrix.lu] = {"M:lu() --> L_M, U_M, perm_M",
  "LU decomposition for the matrix. Return L, U and P matrices.", TRANSFORM}


--- Apply function to each element.
--  @param fn Desired function.
--  @return Matrix where each element is obtained based on desired function.
matrix.map = function (self, fn)
  if type(fn) == 'string' then fn = Utils.Fn(fn, 1) end
  local res, Mcols = {}, self._cols
  for r = 1, self._rows do
    local rr, mr = {}, self[r]
    for c = 1, Mcols do rr[c] = fn(mr[c]) end
    res[r] = rr
  end
  return matrix._init(#res, Mcols, res)
end
about[matrix.map] = {"M:map(fn|str) --> found_M",
  "Apply the given function to all elements, return new matrix.",
  TRANSFORM}


--- Find minor for the matrix element.
--  @param ir Row index.
--  @param ic Column index.
--  @return minor matrix.
matrix.minor = function (self, ir, ic)
  assert(self._rows == self._cols)
  if ir > 0 and ic > 0 and ir <= self._rows and ic <= self._cols then
    return tf.firstMinor(tf.firstMinorSub(self, ir, ic))
  else
    -- determinant via minors
    return tf.firstMinor(self)
  end
end
about[matrix.minor] = {"M:minor(row_N, col_N) --> minor_M",
  "Find minor for the matrix element."}


--- Euclidean norm of the matrix at whole.
--  @return Norm value.
matrix.norm = function (self)
  local sum = 0
  for r = 1, self._rows do
    local mr = self[r]
    for c = 1, self._cols do
      sum = sum + Cnorm(mr[c])^2
    end
  end
  return math.sqrt(sum)
end
about[matrix.norm] = {"M:norm() --> num", "Euclidean norm."}


matrix.normalize = tf.vec_access.normalize
about[matrix.normalize] = {"V:normalize()",
  "Normalize to unit vector.", VECTOR}


-- Outer product.
matrix.outer = tf.vec_access.outer
about[matrix.outer] = {'V:outer(V2) --> M',
  'Outer product or two vectors.', VECTOR}


--- Quick pseudo inverse matrix.
--  Based on "Fast computation of Moore-Penrose inverse matrices"
--  paper by Pierre Courrieu.
--  @return Pseudo inverse matrix.
matrix.pinv = function (self)
  local m, n, transp = self._rows, self._cols, false
  local Mt, A = self:T(), nil
  if m < n then
    A, n, transp = self * Mt, m, true
  else
    A = Mt * self
  end
  A = matrix(A)  -- avoid scalar result
  local tol = math.huge
  for i = 1, A._rows do
    local v = Cnorm(A[i][i])
    tol = math.min(tol, (v > 0 and v or math.huge))
  end
  tol = tol * 1e-9
  local L, r, tmp = matrix:zeros(A._rows, A._cols), 0, nil
  for k = 1, n do
    r = r + 1
    local B = A({k, n}, k)
    if r > 1 then
      tmp = matrix(L({k, n}, {1, r-1}) * L(k, {1, r-1}):T())
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
      tmp = math.sqrt(assert(Cfloat(tmp)))
      L[k][r] = tmp
      for i = k+1, n do L[i][r] = L[i][r]/tmp end
    else
      r = r - 1
    end
  end
  L._cols = (r > 0) and r or 1
  local Lt = L:T()
  local K = matrix.inv(matrix(Lt * L))
  if transp then
    return (((Mt * L) * K) * K) * Lt
  end
  return L * (K * (K * (Lt * Mt)))
end
about[matrix.pinv] = {"M:pinv() --> inv_M",
  "Pseudo inverse matrix calculation."}


--- QR transformation
--  Expect #rows >= #cols.
--  @return Q and R matrices.
matrix.qr = function (self)
  local m, n = self._rows, self._cols
  if n > m then
    error "Wrong matrix size"
  end
  local Q = matrix:eye(m)
  local R = matrix.copy(self)
  for j = 1, math.min(m-1, n) do
    -- housholder transformation
    -- prepare v and v:T()
    local v = R({j,m}, j):copy()
    local v1 = v[1][1]
    local v1abs = Cnorm(v1)
    v[1][1] = v1 + v:norm() * (v1abs > 0 and (v1/v1abs) or 1)
    v:vec():normalize()
    local vt = v:H():copy()
    for r = 1, v._rows do v[r][1] = 2 * v[r][1] end
    -- update R
    local vvr = v * (vt * R({j, m}, {j, n}))
    local j1 = j - 1
    for r = j, m do
      local Rr, Vj1 = R[r], vvr[r-j1]
      for c = j, n do Rr[c] = Rr[c] - Vj1[c-j1] end
    end
    -- update Q
    local qvv = (Q({1, m}, {j, m}) * v) * vt
    for r = 1, m do
      local Qr, Qj1 = Q[r], qvv[r]
      for c = j, m do Qr[c] = Qr[c] - Qj1[c-j1] end
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
--  @return Value of rank.
matrix.rank = function (self)
  local mat = tf.gaussDown(matrix.copy(self))
  local i = 1
  while i <= mat._rows do
    local mati = mat[i]
    if not mati then break end
    -- find nonzero element
    local zeros = true
    for j = i, mat._cols do
      if not Czero(mati[j]) then
        zeros = false
        break
      end
    end
    if zeros then break end
    i = i + 1
  end
  return i - 1
end
about[matrix.rank] = {"M:rank() --> N", "Find rank of the matrix."}


--- Change matrix size.
--  @param iRows New number of rows.
--  @param iCols New number of columns.
--  @return Matrix with new size.
matrix.reshape = function (self, iRows, iCols)
  if not (iRows and iCols) then
    iRows = self:rows() * self:cols()
    iCols = 1
  end
  return tf.makeReshape(self, iRows, iCols)
end
about[matrix.reshape] = {"M:reshape(row_N=(rows*cols), col_N=1) --> mat_Ref",
  "Matrix with rearranged elements.", TRANSFORM}


--- Get number or rows.
--  @return Number of rows.
matrix.rows = function (self) return self._rows end
about[matrix.rows] = {"M:rows() --> N", "Get number of rows."}


--- Solve system of equations using Gauss method.
--  @return Transformed matrix.
matrix.rref = function (self) return tf.gaussUp(tf.gaussDown(self)) end
about[matrix.rref] = {"M:rref() --> upd_M",
  "Perform transformations using Gauss method.", TRANSFORM}


matrix.skew = tf.vec_access.skew
about[matrix.skew] = {"V:skew() --> M",
  "Make skew-symmetric matrix from the 3-element vector.", VECTOR}


--- Singular value decomposition for a matrix.
--  Find U, S, V such that M = U*S*V' and
--  U, V are orthonormal, S is diagonal matrix.
--  @param M Source matrix.
--  @return Matrices U, S, V.
matrix.svd = function (M)
  local transpose = M._rows < M._cols
  if transpose then M = M:T():copy() end
  -- main steps
  local U1, B, V1 = tf.bidiag(M)
  local U2, V2 = matrix:eye(U1), matrix:eye(V1)
  repeat
    local U3, B3, V3, E = tf.qrSweep(B)
    U2, V2, B = U2 * U3, V2 * V3, B3
  until E <= 1E-8
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
--  @return Table without metametods.
matrix.table = function (self)
  local res = {}
  -- simplify 'vector' representation
  if self._rows == 1 then
    for c = 1, self._cols do res[c] = self[1][c] end
  elseif self._cols == 1 then
    for r = 1, self._rows do res[r] = self[r][1] end
  -- full matrix
  else
    for r = 1, self._rows do
      local resr, mr = {}, self[r]
      for c = 1, self._cols do resr[c] = mr[c] end
      res[r] = resr
    end
  end
  return res
end
about[matrix.table] = {"M:table() --> tbl",
  "Convert to simple Lua table.", help.OTHER}


--- Get trace of the matrix.
--  @return Sum of elements of the main diagonal.
matrix.tr = function (self)
  local sum = 0
  for i = 1, math.min(self._rows, self._cols) do sum = sum + self[i][i] end
  return sum
end
about[matrix.tr] = {"M:tr() --> sum", "Get trace of the matrix."}


--- Transpose matrix.
--  @return Transposed matrix reference.
matrix.T = function (self) return tf.makeT(self) end
about[matrix.T] = {"M:T() --> transpose_Ref",
  "Return matrix transpose.", TRANSFORM}


--- Create column vector.
--  Simplified vector constructor.
--  @param t Table with vector elements.
--  @return Vector form of matrix.
matrix.V = function (_, t)
  return tf.makeT(matrix._init(1, #t, {t}))
end
about[matrix.V] = {":V {...} --> mat_Ref",
  "Create vector from list of numbers.", help.NEW}


--- Get reference to vector.
--  @return vector object.
matrix.vec = function (self)
  if self._cols ~= 1 and self._rows ~= 1 then
    inform("Not a vector")
    return nil
  end
  return tf.makeVector(self)
end
about[matrix.vec] = {"M:vec() --> vec_Ref|nil",
  "Create reference to vector data.", VECTOR}


--- Stack columns into the single vector.
--  @param M Source matrix.
--  @return column vector.
matrix.vectorize = function (M)
  return M:T():copy():reshape()
end
about[matrix.vectorize] = {"M:vectorize() --> V",
  "Create vector as a stack of columns.", TRANSFORM}


--- Vertical concatenation of matrices.
--  @param lst List of matrices.
--  @return concatenated matrix object.
matrix.ver = function (self, lst) return tf.makeConcat(lst, true) end
about[matrix.ver] = {":ver(mat_t} --> mat_Ref",
  "Vertical concatenation for the given list of matrices.", "concat"}


--- Create matrix of zeros.
--  @param nR Number of rows.
--  @param nC Number of columns. Can be omitted in case of square matrix.
--  @return Sparse matrix.
matrix.zeros = function (_, iR, iC)
  if _ismatrixex(iR) then iR, iC = iR._rows, iR._cols end  -- input is a matrix
  iC = iC or iR                          -- input is a number
  return matrix._init(iR, iC, {})
end
about[matrix.zeros] = {":zeros(row_N, col_N=row_N) --> M",
  "Create matrix of zeros.", help.NEW}


--- Apply function element-wise to matrices.
--  @param fn Function of N arguments.
--  @param ... List of N matrices.
--  @return New matrix.
matrix.zip = function (_, fn, ...)
  local arg = {...}
  local rows, cols = arg[1]._rows, arg[1]._cols
  if type(fn) == 'string' then fn = Utils.Fn(fn, #arg) end
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
about[matrix.zip] = {':zip(fn|str, ...) --> res_M',
  'Apply function to the given matrices element-wise.', TRANSFORM}

matrix._pack = function (self, acc)
  local rs, cs = self:rows(), self:cols()
  local spack = string.pack
  local t = {spack('B', acc['matrix']), spack('I2', rs), spack('I2', cs)}
  for r = 1, rs do
    t[#t+1] = Utils.pack_seq(self[r], 1, cs, acc)
  end
  return table.concat(t)
end

matrix._unpack = function (src, pos, acc, ver)
  local rs, cs, t = 0, 0, {}
  local unpack_num, sunpack = Utils.unpack_num, string.unpack
  rs, pos = sunpack('I2', src, pos)
  cs, pos = sunpack('I2', src, pos)
  for r = 1, rs do
    t[r], pos = Utils.unpack_seq(cs, src, pos, acc, ver)
  end
  return matrix._init(rs, cs, t), pos
end

-- constructor call
setmetatable(matrix, {__call = matrix._new})
about[matrix] = {" {row1_t, ...} --> new_M",
  "Create matrix from list of strings (tables).", help.NEW}


-- Config reference objects.
tf.initRef(matrix)


-- Comment to remove descriptions
matrix.about = about


return matrix

--=========================
--TODO: check SVD with complex numbers
--TODO: Fix eigenvectors for complex eigenvalues.
--TODO: single Householder algorithm
