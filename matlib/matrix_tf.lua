--[[		sonata/lib/matrix_tf.lua

--- Aux matrix transformations.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2023.

	module 'matrix_tf'
--]]

--      LOCAL

local Cnorm, Usign do
  local lib = require("matlib.utils")
  Cnorm = lib.cross.norm
  Usign = lib.utils.sign
end


--      MODULE

local transform = {}


-- Determinants for simple cases
transform.detList = {
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
transform.invList = {
-- 1x1
function (M) return 1 end,
-- 2x2
function (M)
  return M._init(2, 2, {{M[2][2], -M[1][2]}, {-M[2][1], M[1][1]}} )
end,
-- 3x3
function (M)
  local m1, m2, m3 = M[1], M[2], M[3]
  return M._init(3, 3, {
    {m2[2]*m3[3]-m2[3]*m3[2], -(m1[2]*m3[3]-m1[3]*m3[2]), m1[2]*m2[3]-m1[3]*m2[2]},
    {-(m2[1]*m3[3]-m2[3]*m3[1]), m1[1]*m3[3]-m1[3]*m3[1], -(m1[1]*m2[3]-m1[3]*m2[1])},
    {m2[1]*m3[2]-m2[2]*m3[1], -(m1[1]*m3[2]-m1[2]*m3[1]), m1[1]*m2[2]-m1[2]*m2[1]}
  })
end
}


--- Inverse iteration method for eigenvector calculation.
--  @param M Source matrix.
--  @param v Eigenvalue estimation.
--  @param eps Error value.
--  @return Eigenvector estimation.
transform.findEigenvector = function (M, v, eps)
  -- (M - v * I)^-1
  local iM = M:copy()
  -- add 'noize'
  v = (Cnorm(v) > 0) and 1.01 * v or 1E-4
  for i = 1, M._rows do iM[i][i] = M[i][i] - v end
  iM = iM:inv()
  -- random b
  local b = M._init(M._rows, 1, {})   -- zeros
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
transform.firstMinor = function (M)
  local det = transform.detList[M._rows]
  if det then
    return det(M)
  else
    local sum, k, M1 = 0, 1, M[1]
    for i = 1, M._cols do
      if M1[i] ~= 0 then
        local m = transform.firstMinorSub(M, 1, i)
        sum = sum + (k * M1[i]) * transform.firstMinor(m)
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
transform.firstMinorSub = function (M, ir, ic)
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
  return M._init(M._rows-1, M._cols-1, res)
end


--- Transform matrix to upper triangle (in-place).
--  @param M Initial matrix.
--  @return Upper triangulated matrix and determinant.
transform.gaussDown = function (M)
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
transform.gaussUp = function (M)
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


--- Prepare LU transformation for other functions.
--  @param M Initial square matrix.
--  @return "Compressed" LU, indexes, number of permutations
transform.luPrepare = function (M)
  if M._rows ~= M._cols then error("Square matrix is expected!") end
  local a = M:copy()
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


--- Given's rotation.
--  Find such c, s, r that Mat{{c,s},{-s,c}} * Mat:V{d1,d2} = Mat:V{r,0}.
--  @param d1 First vector element.
--  @param d2 Second vector element.
--  @return cos, sin, vector length
transform.givensRot = function (d1, d2)
   -- return cos, sin; r is omitted
   if d2 == 0 then
     local c = Usign(d1)
     return (c == 0) and 1 or c, 0, Cnorm(d1)
   elseif d1 == 0 then
     return 0, Usign(d2), Cnorm(d2)
   elseif Cnorm(d1) > Cnorm(d2) then
     local t = d2 / d1
     local u = Usign(d1) * math.sqrt(1 + t*t)
     local c = 1 / u
     return c, t * c, d1 * u
   else
     local t = d1 / d2
     local u = Usign(d2) * math.sqrt(1 + t*t)
     local s = 1 / u
     return t * s, s, d2 * u
   end
end


--- Remove to small elements in-place.
--  @param M Source matrix.
--  @param dTol Threshold value.
transform.clearLess = function (M, dTol)
  for i = 1, M._rows do
    local mi = M[i]
    for j = 1, M._cols do
      if Cnorm(mi[j]) < dTol then mi[j] = 0 end
    end
  end
end


--- QR-type sweeps for SVD.
--  Find such U, B, V that B = U*M*V:T() and
--  U, V are orthonormal, B is bidiagonal rectangular. Estimate error E.
--  @param M Source matrix.
--  @return Matrices U, B, V, value E
transform.qrSweep = function (M)
  local m, n, B = M._rows, M._cols, M
  local U, V = M:eye(m), M:eye(n)
  local w, TOL = math.min(m, n), 1E-13
  for k = 1, w-1 do
    -- V
    local c, s = transform.givensRot(B[k][k], B[k][k+1])
    local Q = M:eye(n)
    Q[k  ][k] = c; Q[k  ][k+1] = -s
    Q[k+1][k] = s; Q[k+1][k+1] =  c
    B, V = B * Q, V * Q       -- Q is transposed!
    transform.clearLess(B, TOL)
    -- U
    c, s = transform.givensRot(B[k][k], B[k+1][k])
    Q = M:eye(m)
    Q[k  ][k] =  c; Q[k  ][k+1] = s
    Q[k+1][k] = -s; Q[k+1][k+1] = c
    U, B = U * Q:T(), Q * B
    transform.clearLess(B, TOL)
  end
  -- find error (upper diagonal of lenght w-1)
  local e = 0
  for i = 1, w-1 do e = e + math.abs(B[i][i+1]) end
  return U, B, V, e
end


--- Householder transformation.
--  @param V Vector for reflection.
--  @param ik Index of start element.
--  @return Householder matrix.
transform.householder = function (V, ik)
  local r, sum = V._rows, 0
  local u = V._init(1, r, {})   -- use row vector
  -- fill vector
  for i = ik, r do sum = sum + Cnorm(V[i][1])^2 end
  u[1][ik] = V[ik][1] + Usign(V[ik][1]) * math.sqrt(sum)
  for i = ik+1, r do u[1][i] = V[i][1] end
  -- find matrix
  return V:eye(r) - u:H() * ( (2 / (u:norm() ^ 2)) * u)
end


-- Define reference to (conjugate) transpose matrix
local ref_transpose = {type='matrix_ref'}


--- Access to methods or data
--  @param k Key or column index.
--  @return Table reference or method.
ref_transpose.__index = function (self, k)
  if type(k) == 'number' then
    self._tbl._n = k
    return self._tbl
  elseif k == 'data' then
    return self._tbl._src
  else
    return self._tbl._src.__index(self, k)
  end
end


ref_transpose.__newindex = function ()
  error 'Wrong assignment'
end


-- Reference to column in transposed matrix.
local ref_transpose_t = {}


--- Get element.
--  @param k Element index.
--  @return matrix value.
ref_transpose_t.__index = function (self, k)
  return self._src[k][self._n]
end


--- Set element.
--  @param k Element index.
--  @param v New value.
ref_transpose_t.__newindex = function (self, k, v)
  self._src[k][self._n] = v
end


-- Reference to column in conjutage transposed matrix.
local ref_transpose_h = {}


--- Get element.
--  @param k Element index.
--  @return matrix value.
ref_transpose_h.__index = function (self, k)
  local v = self._src[k][self._n]
  return (type(v) == 'table') and v.conj and v:conj() or v
end


--- Set element.
--  @param k Element index.
--  @param v New value.
ref_transpose_h.__newindex = ref_transpose_t.__newindex


--- Create (conjugate) transposed matrix.
--  @param M Source matrix object.
--  @param hermit Flag shows that the matrix is conjugate.
--  @return referenced object.
transform.makeT = function (M, hermit)
  if getmetatable(M) == ref_transpose then
    return M._tbl._src  -- 'transpose' back
  end
  local o = {
    _cols = M._rows,
    _rows = M._cols,
    _tbl = setmetatable({_src = M, _n = 0}, 
                        hermit and ref_transpose_h or ref_transpose_t),
  }
  return setmetatable(o, ref_transpose)
end


-- Get range of elements
local ref_range = {type='matrix_ref'}


--- Get row or source data.
--  @param k Index or method.
--  @return Table row, data or method.
ref_range.__index = function (self, k)
  if type(k) == 'number' then
    self._tbl._n = self._ir[k] or 0
    return self._tbl
  elseif k == 'data' then
    return self._tbl._src
  else
    return self._tbl._src.__index(self, k)
  end
end


--- Copy data.
--  @param k Field name.
--  @param v Matrix to copy.
ref_range.__newindex = function (self, k, v)
  if k == 'data' then
    ref_range._copyData(self, v)
  else
    error 'Wrong alignment'
  end
end


--- Copy the given matrix.
--  @param other Matrix to copy.
ref_range._copyData = function (self, other)
  if self._cols ~= other._cols or self._rows ~= other._rows then
    error 'Different size'
  end
  local r, c = self._ir, self._tbl._ic
  local src = self._tbl._src
  for i = 1, self._rows do
    local row, orow = src[r[i]], other[i]
    for j = 1, self._cols do row[c[j]] = orow[j] end
  end
end


-- Access to column element.
local ref_range_r = {}


--- Read value.
--  @param k Index.
--  @return element in the given position or 0.
ref_range_r.__index = function (self, k)
  return self._src[self._n][self._ic[k] or 0]
end


--- Set value.
--  @param k Index.
--  @param v New value.
ref_range_r.__newindex = function (self, k, v) 
  self._src[self._n][self._ic[k] or 0] = v
end


--- Crate reference to the matrix range.
--  @param M Source matrix.
--  @param ir List of rows.
--  @param ic List of columns.
--  @return reference to the matrix range.
transform.makeRange = function (M, ir, ic)
  local o = {
    _rows = #ir,
    _cols = #ic,
    _ir = ir,
    _tbl = setmetatable(
      {_src = M, _ic = ic, _n = {}},
      ref_range_r)
  }
  return setmetatable(o, ref_range)
end


-- Change matrix shape
local ref_reshape = {type='matrix_ref'}


--- Access to the first index.
--  @param k Index or method name.
--  @return table or method.
ref_reshape.__index = function (self, k)
  local tbl = self._tbl
  if type(k) == 'number' then
    tbl._n = self._cols * (k - 1)
    return tbl
  elseif k == 'data' then
    return tbl._src
  else
    return tbl._src.__index(self, k)
  end
end


-- Don't modify object
ref_reshape.__newindex = ref_transpose.__newindex


-- Internal data.
local ref_reshape_t = {}


--- Get matrix value.
--  @param k Index.
--  @return element value.
ref_reshape_t.__index = function (self, k)
  local n = self._n + k
  local r = math.modf((n-1) / self._cols)
  local c = n - r * self._cols
  return self._src[r+1][c]
end


--- Set matrix value.
--  @param k Index.
--  @param v New value.
ref_reshape_t.__newindex = function (self, k, v)
  local n = self._n + k
  local r = math.modf((n-1) / self._cols)
  local c = n - r * self._cols
  self._src[r+1][c] = v
end


--- Create matrix with new shape.
--  @param M Source matrix.
--  @param rows New number of rows.
--  @param cols New number of columns.
--  @return reference to the matrix with new shape.
transform.makeReshape = function (M, rows, cols)
  local o = {
    _cols = cols,
    _rows = rows,
    _tbl = setmetatable({
      _src = M,
      _n = 0,
      _cols = M:cols(),
    }, ref_reshape_t)
  }
  return setmetatable(o, ref_reshape)
end


--- Simplify access to the vector elements.
local ref_vector = {
  type = 'vector',
  _name = {x=1, y=2, z=3},
}


--- Get element of method.
--  @param k Index or field.
--  @return found element.
ref_vector.__index = function (self, k)
  local ind = ref_vector._name[k] or k
  if type(ind) == 'number' then
    return self._column and self._src[ind][1] or self._src[1][ind]
  elseif k == 'data' then
    return self._src
  else
    return ref_vector[k]
  end
end


--- Vector length.
--  @return number of elements in the vector.
ref_vector.__len = function (self) 
  local src = self._src
  return math.max(src._rows, src._cols) 
end


--- Set new value.
--  @param k Index.
--  @param v Value to set.
ref_vector.__newindex = function (self, k, v)
  if k == 'data' then
    ref_vector._copyData(self, v)
  else
    local ind = ref_vector._name[k] or k
    if self._column then
      self._src[ind][1] = v
    else 
      self._src[1][ind] = v
    end
  end
end


ref_vector.__tostring = function (self)
  local res = {}
  for i = 1, #self do res[i] = tostring(self[i]) end
  return table.concat(res, '  ')
end


--- Copy one vector to another.
--  @param other Second vector object.
ref_vector._copyData = function (self, other)
  if getmetatable(other) ~= ref_vector then
    error 'Different types'
  end
  local len = #self
  if len ~= #other then
    error 'Different size'
  end
  for i = 1, len do self[i] = other[i] end
end


--- Cross product of two vectors.
--  @param V1 3-element vector.
--  @param V2 3-element vector.
--  @return Found vector.
ref_vector.cross = function (V1, V2)
  if #V1 ~= 3 or #V2 ~= 3 then
    error 'Vector with 3 elements is expected'
  end
  local x1, y1, z1 = V1[1], V1[2], V1[3]
  local x2, y2, z2 = V2[1], V2[2], V2[3]
  return V1._src._init(3, 1, 
    {{y1*z2-z1*y2}, {z1*x2-x1*z2}, {x1*y2-y1*x2}})
end


--- Scalar product of two vectors.
--  @param V1 First vector.
--  @param V2 Second vector.
--  @return dot product.
ref_vector.dot = function (V1, V2)
  local len = #V1
  if len ~= #V2 then
    error 'Different vector length'
  end
  local s = 0
  for i = 1, len do
    s = s + V1[i] * V2[i]
  end
  return s
end


--- Vector norm.
--  @return value of norm.
ref_vector.norm = function (self)
  -- TODO other norms
  local s = 0
  for i = 1, #self do
    local v = self[i]
    s = s + v*v
  end
  return math.sqrt(s)
end


--- Create reference to access vector elements.
--  @param M Source matrix with single row or column.
--  @return vector reference object.
transform.makeVector = function (M)
  local o = {
    _src = M,
    _column = (M._cols == 1),
  }
  return setmetatable(o, ref_vector)
end
transform.vec_access = ref_vector


--- Copy methametods for ref objects.
--  @param t Table with methametods.
transform.initRef = function (t)
  -- transpose / hermit
  ref_transpose.__add = t.__add
  ref_transpose.__sub = t.__sub
  ref_transpose.__mul = t.__mul
  ref_transpose.__div = t.__div
  ref_transpose.__unm = t.__unm
  ref_transpose.__pow = t.__pow
  ref_transpose.__idiv = t.__idiv
  ref_transpose.__eq = t.__eq
  ref_transpose.__call = t.__call
  ref_transpose.__concat = t.__concat
  ref_transpose.__tostring = t.__tostring
  -- range
  ref_range.__add = t.__add
  ref_range.__sub = t.__sub
  ref_range.__mul = t.__mul
  ref_range.__div = t.__div
  ref_range.__unm = t.__unm
  ref_range.__pow = t.__pow
  ref_range.__idiv = t.__idiv
  ref_range.__eq = t.__eq
  ref_range.__call = t.__call
  ref_range.__concat = t.__concat
  ref_range.__tostring = t.__tostring  
  -- reshape
  ref_reshape.__add = t.__add
  ref_reshape.__sub = t.__sub
  ref_reshape.__mul = t.__mul
  ref_reshape.__div = t.__div
  ref_reshape.__unm = t.__unm
  ref_reshape.__pow = t.__pow
  ref_reshape.__idiv = t.__idiv
  ref_reshape.__eq = t.__eq
  ref_reshape.__call = t.__call
  ref_reshape.__concat = t.__concat
  ref_reshape.__tostring = t.__tostring  
end


--- Check if the object is reference.
--  @return true when matrix is ref.
transform.isref = function (v)
  local mt = getmetatable(v)
  return mt == ref_transpose 
      or mt == ref_range      
      or mt == ref_reshape
end


return transform

--===============================
-- TODO check matrix elements on assignment
