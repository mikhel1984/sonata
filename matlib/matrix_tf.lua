--[[		sonata/lib/matrix_tf.lua

--- Aux matrix transformations and references.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2023.

	module 'matrix_tf'
--]]

--      LOCAL

local Cnorm, Usign, Czero, Unumstr do
  local lib = require("matlib.utils")
  Cnorm = lib.cross.norm
  Czero = lib.cross.isZero
  Usign = lib.utils.sign
  Unumstr = lib.utils.numstr
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
function (M) return M._init(1, 1, {{1}}) end,
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


--- Bidiagonalization.
--  Find such U, B, V that U*B*V:T() = M and
--  B is upper bidiagonal, U and V are ortogonal.
--  @param M Source matrix.
--  @return U, B, V
transform.bidiag = function (M)
  local m, n, B = m._rows, M._cols, M
  local U, V = M:eye(m), M:eye(n)
  local w = math.min(m, n)
  for k = 1, w do
    -- set zero to column elements
    local H1 = transform.householder(B({1, m}, k), k)
    U, B = U * H1:H(), H1 * B
    if k < (w - 1) then
      local H2 = transform.householder(B(k, {1, n}):T(), k+1):H()
      B, V = B * H2, V * H2    -- H2 is transposed!
    end
  end
  return U, B, V
end


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
  b = b * (1 / b:norm())
  -- find
  local prev = b
  for i = 1, 10 do
    b = iM * b
    b = b * (1 / b:norm())
    local diff = (b - prev):norm()
    if diff < eps or diff > (2-eps) then break end
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
      if not Czero(M1[i]) then
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
    local i = k + 1
    while Czero(M[k][k]) and i <= M._rows do
      if not Czero(M[i][k]) then
        M[i], M[k], A = M[k], M[i], -A
      end
      i = i + 1
    end
    local coef = M[k][k]
    A = A * coef
    if not Czero(coef) then
      -- normalization
      coef = 1 / coef
      local mk = M[k]
      for c = k, M._cols do mk[c] = mk[c] * coef end
      -- subtraction
      for r = (k + 1), M._rows do
        local mr = M[r]
        local v = mr[k]
        if not Czero(v) then
          for c = k, M._cols do mr[c] = mr[c] - v * mk[c] end
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
      if not Czero(v) then
        for c = k, M._cols do mr[c] = mr[c] - v * mk[c] end
      end -- if
    end -- for
  end -- for
  return M
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
    local mi = rawget(M, i)
    if mi then
      for j = 1, M._cols do
        local mij = rawget(mi, j)
        if mij and Cnorm(mij) < dTol then mi[j] = 0 end
      end
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
  local tbl = self._tbl
  if type(k) == 'number' then
    tbl.n = k
    return tbl
  elseif k == 'data' then
    return tbl.src
  else
    return tbl.src.__index(self, k)
  end
end


--- Copy data from one matrix to another.
--  @param other Source object.
ref_transpose._copyData = function (self, other)
  if self._cols ~= other._cols or self._rows ~= other._rows then
    error 'Different size'
  end
  for i = 1, self._rows do
    local dst, src = self[i], other[i]
    for j = 1, self._cols do
      dst[j] = src[j]
    end
  end
end


--- Copy data.
--  @param k Filed 'data' to do copy.
--  @param v Other matrix or reference.
ref_transpose.__newindex = function (self, k, v)
  if k == 'data' then
    ref_transpose._copyData(self, v)
  else
    error 'Wrong assignment'
  end
end


-- Reference to column in transposed matrix.
local ref_transpose_t = {}


--- Get element.
--  @param k Element index.
--  @return matrix value.
ref_transpose_t.__index = function (self, k)
  return self.src[k][self.n]
end


--- Set element.
--  @param k Element index.
--  @param v New value.
ref_transpose_t.__newindex = function (self, k, v)
  self.src[k][self.n] = v
end


-- Reference to column in conjutage transposed matrix.
local ref_transpose_h = {}


--- Get element.
--  @param k Element index.
--  @return matrix value.
ref_transpose_h.__index = function (self, k)
  local v = self.src[k][self.n]
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
    return M._tbl.src  -- 'transpose' back
  end
  local o = {
    _cols = M._rows,
    _rows = M._cols,
    _tbl = setmetatable({src = M, n = 0},
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
  local tbl = self._tbl
  if type(k) == 'number' then
    tbl.n = self._ir[k] or 0
    return tbl
  elseif k == 'data' then
    return tbl.src
  else
    return tbl.src.__index(self, k)
  end
end


--- Copy data.
ref_range.__newindex = ref_transpose.__newindex


--- Copy the given matrix.
--  @param other Matrix to copy.
ref_range._copyData = function (self, other)
  if self._cols ~= other._cols or self._rows ~= other._rows then
    error 'Different size'
  end
  local r, c = self._ir, self._tbl._ic
  local src = self._tbl.src
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
  return self.src[self.n][self._ic[k] or 0]
end


--- Set value.
--  @param k Index.
--  @param v New value.
ref_range_r.__newindex = function (self, k, v)
  self.src[self.n][self._ic[k] or 0] = v
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
    _tbl = setmetatable({
      src = M,
      _ic = ic,
      n = {}
    }, ref_range_r)
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
    tbl.n = self._cols * (k - 1)
    return tbl
  elseif k == 'data' then
    return tbl.src
  else
    return tbl.src.__index(self, k)
  end
end


-- Copy data.
ref_reshape.__newindex = ref_transpose.__newindex


-- Internal data.
local ref_reshape_t = {}


--- Get matrix value.
--  @param k Index.
--  @return element value.
ref_reshape_t.__index = function (self, k)
  local n = self.n + k
  local r = math.modf((n-1) / self.cols)
  local c = n - r * self.cols
  return self.src[r+1][c]
end


--- Set matrix value.
--  @param k Index.
--  @param v New value.
ref_reshape_t.__newindex = function (self, k, v)
  local n = self.n + k
  local r = math.modf((n-1) / self.cols)
  local c = n - r * self.cols
  self.src[r+1][c] = v
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
      src = M,
      n = 0,
      cols = M:cols(),
    }, ref_reshape_t)
  }
  return setmetatable(o, ref_reshape)
end


-- Concatenate matrices.
local ref_concat = {type='matrix_ref'}


--- Access to the first index.
--  @param k Index or method name.
--  @return table or method.
ref_concat.__index = function (self, k)
  local tbl = self._tbl
  if type(k) == 'number' then
    if tbl.vertical then
      local n, src = 1, tbl.src
      while n < #src and k > src[n]._rows do
        k, n = k - src[n]._rows, n + 1
      end
      tbl.mat = src[n]
    end
    tbl.n = k
    return tbl
  elseif k == 'data' then
    return tbl.src
  else
    return transform._methods.__index(self, k)
  end
end


--- Copy data.
ref_concat.__newindex = ref_transpose.__newindex


-- Internal data.
local ref_concat_t = {}


--- Read element.
--  @param k Element index.
--  @return element value.
ref_concat_t.__index = function (self, k)
  if not self.vertical then
    local n, src = 1, self.src
    while n < #src and k > src[n]._cols do
      k, n = k - src[n]._cols, n + 1
    end
    self.mat = src[n]
  end
  return self.mat[self.n][k]
end


--- Set element.
--  @param k Element index.
--  @param v New value.
ref_concat_t.__newindex = function (self, k, v)
  if not self.vertical then
    local n, src = 1, self.src
    while n < #src and k > src[n]._cols do
      k, n = k - src[n]._cols, n + 1
    end
    self.mat = src[n]
  end
  self.mat[self.n][k] = v
end


--- Concatenate matrices into one object.
--  @param lst List of matrices.
--  @param isvertical True for vertical concatenation.
--  @return concatenated matrix reference.
transform.makeConcat = function (lst, isvertical)
  local cols, rows = 0, 0
  for i, m in ipairs(lst) do
    if isvertical then
      assert(i == 1 or m._cols == cols)
      cols = m._cols
      rows = rows + m._rows
    else
      assert(i == 1 or m._rows == rows)
      rows = m._rows
      cols = cols + m._cols
    end
  end
  local o = {
    _cols = cols,
    _rows = rows,
    _tbl = setmetatable({
      n = 0,
      src = lst,
      mat = lst[1],
      vertical = isvertical,
    }, ref_concat_t)
  }
  return setmetatable(o, ref_concat)
end


--- Simplify access to the vector elements.
local ref_vector = {
  type = 'vector',
  _ind = {x=1, y=2, z=3},
}


--- Get element of method.
--  @param k Index or field.
--  @return found element.
ref_vector.__index = function (self, k)
  local ind = ref_vector._ind[k] or k
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
    local ind = ref_vector._ind[k] or k
    if self._column then
      self._src[ind][1] = v
    else
      self._src[1][ind] = v
    end
  end
end


--- Convert to string.
--  @return string representation.
ref_vector.__tostring = function (self)
  local res = {}
  for i = 1, #self do 
    local v = self[i]
    res[i] = (type(v) == 'number') and Unumstr(v) or tostring(v) 
  end
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
--  @param type_s Type of the norm.
--  @return value of norm.
ref_vector.norm = function (self, type_s)
  type_s = type_s or 'l2'
  local s = 0
  if type_s == 'l1' then
    for i = 1, #self do s = s + Cnorm(self[i]) end
  elseif type_s == 'l2' then
    for i = 1, #self do 
      local v = Cnorm(self[i])
      s = s + v * v
    end
    s = math.sqrt(s)
  elseif type_s == 'linf' then
    for i = 1, #self do s = math.max(s, Cnorm(self[i])) end
  else
    error 'Unknown type'
  end
  return s
end


--- Normalize in-place.
ref_vector.normalize = function (self)
  local s, len = 0, #self
  for i = 1, len do
    s = s + Cnorm(self[i])^2
  end
  s = math.sqrt(s)
  for i = 1, len do
    self[i] = self[i] / s
  end
end


--- Find skew-symmetric matrix.
--  @return matrix M^T = -M
ref_vector.skew = function (self)
  if #self ~= 3 then
    error 'Vector with 3 elements is expected'
  end
  local x, y, z = self.x, self.y, self.z
  return transform._methods._init(3, 3, {
    { 0,-z, y},
    { z, 0,-x},
    {-y, x, 0}
  })
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
  for _, v in ipairs {
    '__add', '__sub', '__mul', '__div', '__unm', '__pow',
    '__eq', '__call', '__concat', '__tostring',
  } do
    local fn = t[v]
    ref_transpose[v] = fn
    ref_range[v] = fn
    ref_reshape[v] = fn
    ref_concat[v] = fn
  end
  transform._methods = t
end


-- List of reference types
local refs = {
  [ref_transpose] = true,
  [ref_range] = true,
  [ref_reshape] = true,
  [ref_concat] = true,
}


--- Check if the object is reference.
--  @return true when matrix is ref.
transform.isref = function (v)
  return refs[getmetatable(v)]
end


return transform

--===============================
-- TODO check matrix elements on assignment
