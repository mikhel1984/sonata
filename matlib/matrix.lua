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
--[[TEST

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
ans = e(2,1)                  -->  1.5

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
-- same as g:range({2,-1},{2,3})
ans = g({2,-1},{2,3})         -->  Mat {{5,6},
                                        {8,9}}

-- insert elements
gg = Mat:eye(3)
gg:insert({1,2},{1,2}, a)
ans = gg:range({1,2},{1,2})   -->  a

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
ans = x1:cross(x2)            -->  Mat {{-3},{6},{-3}}

-- dot product of 2 vectors
ans = x1:dot(x2)              -->  32

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


local TRANSFORM = 'transform'


--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Matrix operations. The matrices are spares by default."
}


--	MODULE

local matrix = { type = 'matrix', ismatrix=true }


--- Check object type.
--  @param v Object to check.
--  @return True if the object is 'matrix'.
local function ismatrix(v) return getmetatable(v) == matrix end


--- M1 + M2
--  @param M1 First matrix or number.
--  @param M2 Second matrix or number.
--  @return Sum matrix.
matrix.__add = function (M1, M2)
  M1 = ismatrix(M1) and M1 or matrix:fill(
    M2._rows, M2._cols, getmetatable(M1) == mt_container and M1[1] or M1)
  M2 = ismatrix(M2) and M2 or matrix:fill(
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
    -- vector call
    if type(vR) == 'number' then
      return self._cols == 1 and self[toRange(vR,self._rows)][1]
        or self[1][toRange(vR,self._cols)]
    else
      vC = {1}
      return self._cols == 1 and matrix.range(self, vR, vC)
        or matrix.range(self, vC, vR)
    end
  end
  -- matrix call
  if type(vR) == 'number' then
    if type(vC) == 'number' then
      vR = toRange(vR, self._rows)
      vC = toRange(vC, self._cols)
      return vR and vC and self[vR][vC]
    else vR = {vR} end
  else
    if type(vC) == 'number' then vC = {vC} end
  end
  return matrix.range(self, vR, vC)
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
  if not ismatrix(M2) then return matrix._kProd(1/M2, M1) end
  return matrix.__mul(M1, matrix.inv(M2))
end


--- M1 == M2
--  @param M1 First matrix.
--  @param M2 Second matrix.
--  @return True if all elements are the same.
matrix.__eq = function (M1, M2)
  if not (ismatrix(M1) and ismatrix(M2)) then return false end
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
  if not ismatrix(M1) then return matrix._kProd(M1, M2) end
  if not ismatrix(M2) then return matrix._kProd(M2, M1) end
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
  M1 = ismatrix(M1) and M1 or matrix:fill(
    M2._rows, M2._cols, getmetatable(M1) == mt_container and M1[1] or M1)
  M2 = ismatrix(M2) and M2 or matrix:fill(
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
  local srow = {}
  for r = 1, self._rows do
    local scol, mr = {}, self[r]
    for c = 1, self._cols do
      local tmp = mr[c]
      table.insert(
        scol, type(tmp) == 'number' and Utils.numstr(tmp) or tostring(tmp))
    end
    table.insert(srow, table.concat(scol, "  "))
  end
  return table.concat(srow, "\n")
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


-- Determinants for simple cases
matrix._detList = {
-- 1x1
function (M) return M[1][1] end,
-- 2x2
function (M) return M[1][1]*M[2][2] - M[2][1]*M[1][2] end,
-- 3x3
function (M)
  local m1, m2, m3 = M[1], M[2], M[3]
  return m1[1]*(m2[2]*m3[3]-m2[3]*m3[2]) -
         m1[2]*(m2[1]*m3[3]-m2[3]*m3[1]) +
         m1[3]*(m2[1]*m3[2]-m2[2]*m3[1])
end
}


-- Inversion for simple cases
matrix._invList = {
-- 1x1
function (M) return 1 end,
-- 2x2
function (M)
  return matrix._init(2, 2, {{M[2][2], -M[1][2]}, {-M[2][1], M[1][1]}} )
end,
-- 3x3
function (M)
  local m1, m2, m3 = M[1], M[2], M[3]
  return matrix._init(3, 3, {
    {m2[2]*m3[3]-m2[3]*m3[2], -(m1[2]*m3[3]-m1[3]*m3[2]), m1[2]*m2[3]-m1[3]*m2[2]},
    {-(m2[1]*m3[3]-m2[3]*m3[1]), m1[1]*m3[3]-m1[3]*m3[1], -(m1[1]*m2[3]-m1[3]*m2[1])},
    {m2[1]*m3[2]-m2[2]*m3[1], -(m1[1]*m3[2]-m1[2]*m3[1]), m1[1]*m2[2]-m1[2]*m2[1]}
  })
end
}


--- Remove to small elements in-place.
--  @param M Source matrix.
--  @param dTol Threshold value.
matrix._clearLess = function (M, dTol)
  for i = 1, M._rows do
    local mi = M[i]
    for j = 1, M._cols do
      if math.abs(mi[j]) < dTol then mi[j] = 0 end
    end
  end
end


--- Inverse iteration method for eigenvector calculation.
--  @param M Source matrix.
--  @param v Eigenvalue estimation.
--  @param eps Error value.
--  @return Eigenvector estimation.
matrix._findEigenvector = function (M, v, eps)
  -- (M - v * I)^-1
  local iM = M:copy()
  -- add 'noize'
  v = (Cnorm(v) > 0) and 1.01 * v or 1E-4
  for i = 1, M._rows do iM[i][i] = M[i][i] - v end
  iM = iM:inv()
  -- random b
  local b = M:zeros(M._rows, 1)
  for i = 1, M._rows do b[i][1] = math.random() end
  b = b / b:norm()
  -- find
  local prev = b
  for i = 1, 10 do
    b = iM * b
    b = b / b:norm()
    local diff = (b - prev):norm()
    if diff < eps or diff > (2-eps) then
      break
    end
    prev = b
  end
  return b
end


--- Minor value calculation.
--  @param M Source matrix.
--  @return Minor value.
matrix._firstMinor = function (M)
  local det = matrix._detList[M._rows]
  if det then
    return det(M)
  else
    local sum, k, M1 = 0, 1, M[1]
    for i = 1, M._cols do
      if M1[i] ~= 0 then
        local m = matrix._firstMinorSub(M, 1, i)
        sum = sum + (k * M1[i]) * matrix._firstMinor(m)
      end
      k = -k
    end
    return sum
  end
end


--- Find sub-matrix for minor calculation.
--  @param M Source matrix.
--  @param ir Row index.
--  @param ic Column index.
--  @return Submatrix.
matrix._firstMinorSub = function (M, ir, ic)
  local res = {}
  for r = 1, ir-1 do
    local rr, mr = {}, M[r]
    for c = 1, ic-1 do rr[c] = mr[c] end
    for c = ic+1, M._cols do rr[c-1] = mr[c] end
    res[r] = rr
  end
  for r = ir+1, M._rows do
    local rr, mr = {}, M[r]
    for c = 1, ic-1 do rr[c] = mr[c] end
    for c = ic+1, M._cols do rr[c-1] = mr[c] end
    res[r-1] = rr
  end
  return matrix._init(M._rows-1, M._cols-1, res)
end


--- Fill second matrix with elements of the first one
--  using index tables.
--  @param from Source matrix.
--  @param tR Table with rows.
--  @param tC Table with columns.
--  @param to Destination matrix.
--  @return Obtained matrix.
matrix._fromTo = function (from, tR, tC, to)
  if type(tR) ~= 'table' or type(tC) ~= 'table' then
    error("Range is a table")
  end
  local dst = to or from
  -- update range
  if #tR == 1 then
    local r = toRange(tR[1], dst._rows)
    tR = {r, r, 1}
  else
    tR[1] = toRange(tR[1] or 1, dst._rows)
    tR[2] = toRange(tR[2] or dst._rows, dst._rows)
    tR[3] = tR[3] or 1
    if not (tR[1] and tR[2] and (tR[2]-tR[1])/tR[3] >= 0) then return nil end
  end
  if #tC == 1 then
    local c = toRange(tC[1], dst._cols)
    tC = {c, c, 1}
  else
    tC[1] = toRange(tC[1] or 1, dst._cols)
    tC[2] = toRange(tC[2] or dst._cols, dst._cols)
    tC[3] = tC[3] or 1
    if not (tC[1] and tC[2] and (tC[2]-tC[1])/tC[3] >= 0) then return nil end
  end

  -- fill matrix
  to = to or {}
  local i = 0
  for r = tR[1], tR[2], tR[3] do
    i = i+1
    local resi, mr = to[i] or {}, from[r]
    local j = 0
    for c = tC[1], tC[2], tC[3] do
      j = j+1
      resi[j] = mr[c]
    end
    to[i] = resi
  end
  return to
end


--- Transform matrix to upper triangle (in-place).
--  @param M Initial matrix.
--  @return Upper triangulated matrix and determinant.
matrix._gaussDown = function (M)
  local A = 1
  for k = 1, M._rows do
    -- look for nonzero element
    local i = k+1
    while M[k][k] == 0 and i <= M._rows do
      if M[i][k] ~= 0 then M[i], M[k], A = M[k], M[i], -A end
      i = i+1
    end
    local coef = M[k][k]
    A = A * coef
    if coef ~= 0 then
      -- normalization
      coef = 1/coef
      local mk = M[k]
      for c = k, M._cols do mk[c] = mk[c]*coef end
      -- subtraction
      for r = (k+1), M._rows do
        local mr = M[r]
        local v = mr[k]
        if v ~= 0 then
          for c = k, M._cols do mr[c] = mr[c]-v*mk[c] end
        end -- if
      end -- for
    end -- if
  end -- for
  return M, A
end


--- Transform triangle matrix to identity matrix (in-place).
--  @param M Initial matrix
--  @return Matrix with diagonal zeros.
matrix._gaussUp = function (M)
  for k = M._rows, 1, -1 do
    local mk = M[k]
    for r = k-1, 1, -1 do
      local mr = M[r]
      local v = mr[k]
      if v ~= 0 then
        for c = k, M._cols do mr[c] = mr[c]-v*mk[c] end
      end -- if
    end -- for
  end -- for
  return M
end


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


--- Prepare LU transformation for other functions.
--  @param M Initial square matrix.
--  @return "Compressed" LU, indexes, number of permutations
matrix._luPrepare = function (M)
  if M._rows ~= M._cols then error("Square matrix is expected!") end
  local a = matrix.copy(M)
  local vv = {}
  -- get scaling information
  for r = 1, a._rows do
    local big, abig, v = 0, 0, 0
    local ar = a[r]
    for c = 1, a._cols do
      v = Cnorm(ar[c])
      if v > abig then
        big = ar[c]
        abig = v
      end
    end
    vv[r] = 1.0/big
  end
  -- Crout's method
  local rmax, dum = nil, nil
  local TINY, d = 1e-20, 0
  local index = {}
  for c = 1, a._cols do
    for r = 1, c-1 do
      local ar = a[r]
      local sum = ar[c]
      for k = 1, r-1 do sum = sum - ar[k]*a[k][c] end
      ar[c] = sum
    end
    local big = 0        -- largest pivot element
    for r=c, a._rows do
      local ar = a[r]
      local sum = ar[c]
      for k = 1, c-1 do sum = sum - ar[k]*a[k][c] end
      ar[c] = sum
      sum = Cnorm(sum)
      dum = vv[r]*sum
      if Cnorm(dum) >= Cnorm(big) then big = dum; rmax = r end
    end
    local ac = a[c]
    if c ~= rmax then
      local armax = a[rmax]
      -- interchange rows
      for k = 1, a._rows do
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
    if c ~= a._cols then
      dum = 1.0 / ac[c]
      for r = c+1, a._rows do a[r][c] = dum*a[r][c] end
    end
  end
  return a, index, d
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


--- QR-type sweeps for SVD.
--  Find such U, B, V that B = U*M*V:T() and
--  U, V are orthonormal, B is bidiagonal rectangular. Estimate error E.
--  @param M Source matrix.
--  @return Matrices U, B, V, value E
matrix._qrSweep = function (M)
  local m, n, B = M._rows, M._cols, M
  local U, V = matrix:eye(m), matrix:eye(n)
  local w, TOL = math.min(m, n), 1E-13
  for k = 1, w-1 do
    -- V
    local c, s = matrix.givensRot(B[k][k], B[k][k+1])
    local Q = matrix:eye(n)
    Q[k  ][k] = c; Q[k  ][k+1] = -s
    Q[k+1][k] = s; Q[k+1][k+1] =  c
    B, V = B * Q, V * Q       -- Q is transposed!
    matrix._clearLess(B, TOL)
    -- U
    c, s = matrix.givensRot(B[k][k], B[k+1][k])
    Q = matrix:eye(m)
    Q[k  ][k] =  c; Q[k  ][k+1] = s
    Q[k+1][k] = -s; Q[k+1][k+1] = c
    U, B = U * Q:T(), Q * B
    matrix._clearLess(B, TOL)
  end
  -- find error (upper diagonal of lenght w-1)
  local e = 0
  for i = 1, w-1 do e = e + math.abs(B[i][i+1]) end
  return U, B, V, e
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
    local H1 = matrix.householder(B:range({1, m}, {k}), k)
    U, B = U * H1:H(), H1 * B
    if k < (w - 1) then
      local H2 = matrix.householder(B:range({k}, {1, n}):T(), k+1):H()
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


--- V1 x V2
--  @param V1 3-element vector.
--  @param V2 3-element vector.
--  @return Cross product.
matrix.cross = function (V1, V2)
  if (V1._rows*V1._cols ~= 3 or V2._rows*V2._cols ~= 3) then
    error("Vector with 3 elements is expected!")
  end
  local x1, y1, z1 = V1(1), V1(2), V1(3)
  local x2, y2, z2 = V2(1), V2(2), V2(3)
  return matrix._init(3, 1, {{y1*z2-z1*y2}, {z1*x2-x1*z2}, {x1*y2-y1*x2}})
end
about[matrix.cross] = {'V:cross(V2) --> V3',
  'Cross product or two 3-element vectors.'}


--- Find determinant.
--  @param M Initial matrix.
--  @return Determinant.
matrix.det = function (M)
  if (M._rows ~= M._cols) then error("Square matrix is expected!") end
  local fn = matrix._detList[M._rows]
  if fn then return fn(M) end
  -- in other cases
  local _, K = matrix._gaussDown(matrix.copy(M))
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
  local vec = ismatrix(v)
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


--- V1 . V2
--  @param V1 First vector.
--  @param V2 Second vector.
--  @return Scalar product.
matrix.dot = function (V1, V2)
  local n = V1._rows*V1._cols
  if n ~= V2._rows*V2._cols then error("Different vector size") end
  local s = 0
  for i = 1, n do
    s = s + V1(i)*V2(i)
  end
  return s
end
about[matrix.dot] = {'V:dot(V2) --> num', 'Scalar product of two vectors.'}


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
    local v = matrix._findEigenvector(M, root[j], 1E-4)
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
  if ismatrix(iR) then
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


--- Given's rotation.
--  Find such c, s, r that Mat{{c,s},{-s,c}} * Mat:V{d1,d2} = Mat:V{r,0}.
--  @param d1 First vector element.
--  @param d2 Second vector element.
--  @return cos, sin, vector length
matrix.givensRot = function (d1, d2)
   -- return cos, sin; r is omitted
   if d2 == 0 then
     local c = Utils.sign(d1)
     return (c == 0) and 1 or c, 0, Cnorm(d1)
   elseif d1 == 0 then
     return 0, Utils.sign(d2), Cnorm(d2)
   elseif Cnorm(d1) > Cnorm(d2) then
     local t = d2 / d1
     local u = Utils.sign(d1) * math.sqrt(1 + t*t)
     local c = 1 / u
     return c, t * c, d1 * u
   else
     local t = d1 / d2
     local u = Utils.sign(d2) * math.sqrt(1 + t*t)
     local s = 1 / u
     return t * s, s, d2 * u
   end
end
about[matrix.givensRot] = {":givensRot(x, y) --> cos_d, sin_d, len_d",
  "Find parameters of Givens rotation (c,s,r).", help.OTHER}


--- Householder transformation.
--  @param V Vector for reflection.
--  @param ik Index of start element.
--  @return Householder matrix.
matrix.householder = function (V, ik)
  local r, sum = V._rows, 0
  local u = matrix:zeros(1, r)   -- use row vector
  -- fill vector
  for i = ik, r do sum = sum + Cnorm(V[i][1])^2 end
  u[1][ik] = V[ik][1] + Utils.sign(V[ik][1]) * math.sqrt(sum)
  for i = ik+1, r do u[1][i] = V[i][1] end
  -- find matrix
  return matrix:eye(r) - u:H() * ( (2 / (u:norm() ^ 2)) * u)
end
about[matrix.householder] = {"M:householder(V, start_N) --> hh_M",
  "Find Householder matrix for the given vector.", TRANSFORM}


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
--  @return Transformed matrix.
matrix.H = function (M)
  local res, Mrows = {}, M._rows
  for c = 1, M._cols do
    local row = {}
    for r = 1, Mrows do
      local v = M[r][c]
      row[r] = (type(v) == 'table') and v.conj and v:conj() or v
    end
    res[c] = row
  end
  return matrix._init(#res, Mrows, res)
end
about[matrix.H] = {"M:H() --> conj_M",
  "Return conjugabe transpose. ", TRANSFORM}


--- Insert values from another matrix.
--  @param M1 Initial matrix, it is modified.
--  @param tR Range of rows.
--  @param tC Range of columns.
--  @param M2 Matrix to insert.
matrix.insert = function (M1, tR, tC, M2)
  matrix._fromTo(M2, tR, tC, M1)
end
about[matrix.insert] = {"M:insert(rows_t, cols_t, M2)",
  "Insert second matrix into the given range of indeces."}


--- Inverse matrix.
--  @param M Initial matrix.
--  @return Result of inversion.
matrix.inv = function (M)
  if M._rows ~= M._cols then error("Square matrix is expected!") end
  local size = M._cols
  -- check simple cases
  local fn = matrix._invList[size]
  if fn then
    local det = matrix._detList[size](M)
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
  res, det = matrix._gaussDown(res)
  if det == 0 then
    return matrix:fill(size, size, math.huge)
  end
  res = matrix._gaussUp(res)
  -- move result
  for r = 1, size do
    local resr = res[r]
    for c = 1, size do
      local p = size+c
      resr[c], resr[p] = resr[p], nil
    end
  end
  res._cols = size
  return res
end
about[matrix.inv] = {"M:inv() --> inv_M", "Return inverse matrix.", TRANSFORM}


--- LU transform
--  @param M Initial square matrix.
--  @return L matrix, U matrix, permutations
matrix.lu = function (M)
  local a, _, d = matrix._luPrepare(M)
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
    return matrix._firstMinor(matrix._firstMinorSub(M, ir, ic))
  else
    -- determinant via minors
    return matrix._firstMinor(M)
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
    local B = A:range({k, n}, {k})
    if r > 1 then
      tmp = L:range({k, n}, {1, r-1}) * L:range({k}, {1, r-1}):T()
      -- product can be scalar
      tmp = ismatrix(tmp) and tmp or matrix._init(1, 1, {{tmp}})
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
    local vt = v:H()
    for r = 1, v._rows do v[r][1] = 2 * v[r][1] end
    -- update R
    local vvr = v * (vt * R:range({j, m}, {j, n}))
    local j1 = j - 1
    for r = j, m do
      for c = j, n do R[r][c] = R[r][c] - vvr[r-j1][c-j1] end
    end
    -- update Q
    local qvv = (Q:range({1, m}, {j, m}) * v) * vt
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


--- Get sub matrix.
--  In case of sub matrix each index should be a table
--  of 2 or 3 elements: [begin,end[,step]].
--  @param M Source matrix.
--  @param tR Array of rows.
--  @param tC Array of columns.
--  @return Sub array or nil in case of error.
matrix.range = function (M, tR, tC)
  local res = matrix._fromTo(M, tR, tC)
  return matrix._init(#res, #res[1], res)
end
about[matrix.range] = {"M:range(rows_t, cols_t) --> range_M",
  "Get submatrix for the given range of rows and columnts."}


--- Matrix rank.
--  @param M Initial matrix.
--  @return Value of rank.
matrix.rank = function (M)
  local mat = matrix._gaussDown(matrix.copy(M))
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
  return matrix._gaussUp(matrix._gaussDown(M))
end
about[matrix.rref] = {"M:rref() --> upd_M",
  "Perform transformations using Gauss method."}


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
    U3, B, V3, E = matrix._qrSweep(B)
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
--  @return Transposed matrix.
matrix.T = function (M)
  local res, Mrows = {}, M._rows
  for c = 1, M._cols do
    local row = {}
    for r = 1, Mrows do row[r] = M[r][c] end
    res[c] = row
  end
  return matrix._init(#res, Mrows, res)
end
about[matrix.T] = {"M:T() --> transpose_M",
  "Return matrix transpose.", TRANSFORM}


--- Create vector.
--  Simplified vector constructor.
--  @param t Table with vector elements.
--  @return Vector form of matrix.
matrix.V = function (self, t)
  local res = {0, 0, 0}       -- prepare some memory
  for i = 1, #t do res[i] = {t[i]} end
  return matrix._init(#t, 1, res)
end
about[matrix.V] = {":V {...} --> new_V",
  "Create vector from list of numbers.", help.NEW}


--- Create matrix of zeros.
--  @param nR Number of rows.
--  @param nC Number of columns. Can be omitted in case of square matrix.
--  @return Sparse matrix.
matrix.zeros = function (self, iR, iC)
  if ismatrix(iR) then iR, iC = iR._rows, iR._cols end  -- input is a matrix
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


-- Comment to remove descriptions
matrix.about = about

return matrix

--=========================
--TODO: check SVD with complex numbers
--TODO: change matrix print
--TODO: matrix from list and size
--TODO: Fix eigenvectors for complex eigenvalues.
