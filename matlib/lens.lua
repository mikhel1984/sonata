--[[		sonata/lib/lens.lua

--- Matrix methods in paraxial optics.
--
--  Each element is represented in form of 2x2 unit matrix
--  { A, B,
--    C, D }
--  See, for example,
--  "Introduction to matrix methods in optics" by A.Gerrard, J.M.Burch.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2025.

	module 'lens'
--]]


----------------------- Tests ----------------------
--[[TEST_IT

-- use 'lens'
Lens = require 'matlib.lens'
-- external dependencies, can be loaded implicitly
require 'matlib.numeric'  -- for root searching
require 'matlib.complex'  -- for beam transformation

-- define a simple lense
n1 = 1      -- air
n2 = 1.56   -- glass
-- radius 200 mm, thickness 5 mm
lens1 = Lens:R(n1,200,n2)
        :T(5,n2)
        :R(n2,-200,n1)
ans = lens1:matrix():det()   --2>  1.0

-- in the chain definition
-- 'n' can be taken from a previous element
lens11 = Lens:R(n1,200,n2)
         :T(5)
         :R(-200, n1)
ans = (lens1 == lens11)       -->  true

-- if n==1 then it can be skipped
lens12 = Lens:R(200,n2)  -- skip first n
         :T(5)
         :R(-200)  -- first n from previous, skip last n
ans = (lens1 == lens12)       -->  true

-- get matrix element
ans = lens1.D                --2>  1

-- find cardinal points
pts = lens1:cardinal()
ans = pts.F1                 --2>  -177.76

-- print points
print(pts)

-- object is located 250 mm to the left
-- from the lens, find position of
-- the image
d1 = 250
fn = function (d2)
  return Lens:T(d1)..lens1..Lens:T(d2)
end
-- solve for B = 0, initial guess dist = 100
d2 = Lens:solve(fn, 'B', 100)
ans = d2                     --2>   623.21

-- check solution
-- assume the lens it thin
f = -pts.F1
ans = 1/d1 + 1/d2            --2>  1/f

-- ray transformation
y1 = 10          -- mm, height
V1 = n1 * 0.05   -- optical angle
sys1 = fn(d2)
y2, V2 = sys1(y1,V1)
ans = y2                     --2>  -24.83

-- from image to object
sys2 = sys1:inv()  -- transpose
ans, _ = sys2(y2, V2)        --3>  y1

-- create thin lens
lens2 = Lens:thin(f)
_, V3 = lens1(y1,V1)
_, ans = lens2(y1,V1)        --2>  V3

-- flat mirror
lens3 = Lens:M(math.huge, n1)
_, ans = lens3(y1,V1)        --2>  V1

-- afocal system
m = 10
lens4 = Lens:afocal(m)
ans, _ = lens4(y1,V1)        --2>  m*y1

-- arbitrary system matrix (ABCD)
lens5 = Lens(1, 0, -0.5, 1)
print(lens5)

-- make copy
ans = lens1:copy()            -->  lens1

-- gaussian beam parameters
lambda = 1024   -- nm, YAG-Nd laser
lambda = lambda * 1E-6  -- mm
ans, _ = Lens:gParam(1, lambda) --2>  3.26E-4

-- laser beam radius
_, ans = Lens:gSize(1, lambda, 1E5) --2>  32.61

-- laser beam transformation
_, ans = lens1:beam(1E6, 1, lambda)  --2>  0.991

-- laser cavity
air_rod = Lens: T(250) : T(30, 1.56)
cavity = Lens:M(-300) .. air_rod .. Lens:M(-300) .. air_rod
ans, _ = cavity:emit(lambda)         --1> 300.0

--]]


--	LOCAL

-- use letters for convenience
local keys = {A=1, B=2, C=3, D=4}

-- tolerance of comparison
local TOL = 1E-8
local LASER = 'laser'
local ANALYZE = 'analyze'


--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Matrix methods in paraxial optics."
}


--	MODULE

local lens = {
-- mark
type = 'lens',
-- {A, B, C, D}
key = keys,
}


--- Check object type.
--  @param v Object.
--  @return True if the object is 'lens'.
local function _islens(v) return getmetatable(v) == lens end


--- Matrix product.
--  @param L1 First matrix.
--  @param L2 Second matrix.
--  @return A, B, C, D
local function _prod (L1, L2)
  return L1[1]*L2[1]+L1[2]*L2[3], L1[1]*L2[2]+L1[2]*L2[4],
         L1[3]*L2[1]+L1[4]*L2[3], L1[3]*L2[2]+L1[4]*L2[4]
end


--- Check matrix module
--  @return true if unit matrix
local function _isunit (L) return math.abs(L[1]*L[4]-L[2]*L[3]-1) < TOL end


--- Concatenate components along the ray trace.
--  Equal to matrix product in the opposite order.
--  @param L1 First object.
--  @param L2 Second object.
--  @return Concatenated object.
lens.__concat = function (L1, L2)
  if not (_islens(L1) and _islens(L2)) then
    error 'Not a Lens object'
  end
  if not (_isunit(L1) and _isunit(L2)) then
    error "Wrong system matrices"
  end
  return lens._init({_prod(L2, L1)})
end


--- Compare two objects.
--  @param L1 First object.
--  @param L2 Second object.
--  @return True if the objects are equal.
lens.__eq = function (L1, L2)
  if not (_islens(L1) and _islens(L2)) then
    error 'Not a Lens object'
  end
  return math.abs(L1[1]-L2[1]) < TOL and math.abs(L1[2]-L2[2]) < TOL
     and math.abs(L1[3]-L2[3]) < TOL and math.abs(L1[4]-L2[4]) < TOL
end


--- Call unknown key.
--  @param t Table.
--  @param k Key.
--  @return Value or method.
lens.__index = function (t, k)
  return lens[k] or rawget(t, keys[k] or '')
end


--- Set unknown key.
--  @param t Table.
--  @param k Key.
--  @param v Value.
lens.__newindex = function (t, k, v)
  if keys[k] then t[keys[k]] = v end
end


--- String representation.
--  @return Equal string.
lens.__tostring = function (self)
  return string.format(
    "A: %.3f   B: %.3f\nC: %.3f   D: %.3f", self[1], self[2], self[3], self[4])
end


about['_op'] = {"operations: L1==L2, L1..L2", nil, help.META }


--- Object constructor.
--  @param t ABCD table.
--  @return 'Lens' object.
lens._init = function (t)
  t._nprev = t._nprev or nil
  return setmetatable(t, lens)
end


--- Make component for afocal system.
--  @param dm Transverse magnification.
--  @return Afocal matrix.
lens.afocal = function (self, dm)
  local L = {dm, 0, 0, 1/dm}
  if self[4] then  -- update and return
    self[1], self[2], self[3], self[4] = _prod(L, self)
    return self
  end
  return lens._init(L)
end
about[lens.afocal] = {":afocal(magn_d) --> L",
  "Find matrix for the afocal system.", help.NEW}


--- Find Gaussian beam parameters after transformation.
--  @param dR Input beam curvature.
--  @param dW Input beam spot radius.
--  @param dLam Wavelength.
--  @return Output beam radius and curvature.
lens.beam = function (self, dR, dW, dLam)
  lens.ext_complex = lens.ext_complex or require("matlib.complex")
  local lampi = dLam / math.pi
  local _1_q1 = lens.ext_complex(1/dR, lampi / (dW * dW))
  local _1_q2 = (self[3] + self[4]*_1_q1) / (self[1] + self[2]*_1_q1)
  return 1/_1_q2:re(), math.sqrt(lampi / _1_q2:im())
end
about[lens.beam] = {"L:beam(inCurv_d, inSize_d, lambda_d) --> outCurv_d, outSize_d",
  "Find output beam curvature and spot radius.", LASER}


--- Make a copy.
--  @return Copy of the object.
lens.copy = function (self)
  return lens._init({self[1], self[2], self[3], self[4], _nprev=self._nprev})
end
about[lens.copy] = {"L:copy() --> cpy_L",
  "Create a copy of the object.", help.OTHER}


--- Find output beam of a laser cavity.
--  @param lam Waveleight, m.
--  @return beam curvature and spot radius (for stable resonator).
lens.emit = function (self, lam)
  local tr = (self[1] + self[4]) * 0.5
  if math.abs(tr) <= 1 then
    local s = math.sqrt(1 - tr*tr)
    return 2*self[2]/(self[4]-self[1]), math.sqrt(lam*self[2]/(math.pi*s)),
      math.sqrt(-lam*s/math.pi/self[3]), (self[1]-self[4])/2/self[3]
  else
    local sh = math.sqrt(tr*tr - 1) * (tr > 1 and 1 or -1)
    return (self[1] - self[4] + 2*sh)/(2*self[3])
  end
end
about[lens.emit] = {
  "L:emit(lambda_d) --> outCurv_d, outSize_d|nil, waist_d|nil, shift_d|nil ",
  "Find laser cavity output beam curvature. In the case of stable cavity also returns size radius, waist radius and its shift from the plane.",
  LASER}


--- Find Gaussian beam characterictics.
--  @param dW0 Beam waist, m.
--  @param dLam Waveleight, m.
--  @return Divergence angle and Raileigh range.
lens.gParam = function (_, dW0, dLam)
  local t = math.pi * dW0 / dLam
  return 1/t, t * dW0
end
about[lens.gParam] = {":gParam(waist_d, lambda_d) --> div_d, range_d",
  "Find divergence angle and Raileigh range for a Gaussian beam.", LASER}


--- Find Gaussian beam propagation.
--  @param dW0 Beam waist, m.
--  @param dLam Waveleight, m.
--  @param dist Distance, m.
--  @return curvature and beam radius in the pose dist.
lens.gSize = function (_, dW0, dLam, dist)
  local t = (math.pi * dW0 * dW0 / dLam / dist) ^ 2
  return dist*(1 + t),  dW0 * math.sqrt(1 + 1/t)
end
about[lens.gSize] = {":gSize(waist_d, lambda_d, dist_d) --> curv_d, rad_d",
  "Find Gaussian beam radius and curvature at some distance.", LASER}


--- Inverse the component matrix.
--  @return Inverted matrix.
lens.inv = function (self)
  if not _isunit(self) then
    error 'Not a unit matrix'
  end
  return lens._init({self[4], -self[2], -self[3], self[1]})
end
about[lens.inv] = {"L:inv() --> inv_L",
  "Get the inverted system matrix.", help.OTHER}


--- Make component for a curved mirror.
--  @param dr Radius of a mirror surface.
--  @param dn Refractive index.
--  @return Reflection matrix.
lens.M = function (self, dr, dn)
  dn = dn or self._nprev or 1
  local L = {1, 0, 2*dn/dr, 1, _nprev=dn}
  if self[4] then  -- update and return
    self[1], self[2], self[3], self[4] = _prod(L, self)
    self._nprev = dn
    return self
  end
  return lens._init(L)
end
about[lens.M] = {":M(rad_d, n_d=1) --> L",
  "Find reflection matrix for the given radius and refractive index.",
  help.NEW}


--- Get elements as matrix.
--  @return 2x2 matrix.
lens.matrix = function (self)
  lens.ext_matrix = lens.ext_matrix or require('matlib.matrix')
  return lens.ext_matrix({{self[1], self[2]}, {self[3], self[4]}})
end
about[lens.matrix] = {"L:matrix() --> M", "Get elements as matrix.", help.OTHER}


--- Make component for refraction.
--  @param nin Initial refractive index.
--  @param r Radius of a surface.
--  @param nout Final refractive index.
--  @return Refraction matrix.
lens.R = function (self, nin, rad, nout)
  if not nout then
    if not rad then
      nin, rad, nout = self._nprev, nin, 1
    else
      nin, rad, nout = self._nprev or 1, nin, rad
    end
  end
  local L = {1,  0, -(nout-nin)/rad, 1, _nprev=nout}
  if self[4] then  -- update and return
    self[1], self[2], self[3], self[4] = _prod(L, self)
    self._nprev = nout
    return self
  end
  return lens._init(L)
end
about[lens.R] = {":R(nin_d, rad_d, nout_d) --> L",
  "Find refraction matrix for the given radius of surface and input and output refractive indeces.",
  help.NEW}


--- Find condition when fn(d).X == 0
--  where X in {A, B, C, D}.
--  @param fn System in form of function.
--  @param ind Index of the matrix element.
--  @param d0 Initial estimation of the variable.
--  @return The found parameter value.
lens.solve = function (_, fn, ind, d0)
  lens.ext_numeric = lens.ext_numeric or require('matlib.numeric')
  -- prepare 'equation'
  local eqn = function (d) return fn(d)[ind] end
  -- try to solve
  return lens.ext_numeric:newton(eqn, d0)
end
about[lens.solve] = {":solve(fn, index_N, initial_d) --> found_d",
  "Find condition when component with the given index is equal to 0, use initial assumption.",
  ANALYZE}


--- Make component for a thin lens.
--  @param df Focal length.
--  @return Thin lens matrix.
lens.thin = function (self, df)
  local L = { 1, 0, -1/df, 1 }
  if self[4] then  -- update and return
    self[1], self[2], self[3], self[4] = _prod(L, self)
    return self
  end
  return lens._init(L)
end
about[lens.thin] = {":thin(focal_d) --> L",
  "Find the thin lens system matrix for the given focal distance.", help.NEW}


--- Make component for translation.
--  @param dt Distance.
--  @param dn Refractive index.
--  @return Translational matrix.
lens.T = function (self, dt, dn)
  dn = dn or self._nprev or 1
  local L = {1, dt/dn, 0, 1, _nprev=dn}
  if self[4] then  -- update and return
    self[1], self[2], self[3], self[4] = _prod(L, self)
    self._nprev = dn
    return self
  end
  return lens._init(L)
end
about[lens.T] = {":T(dist_d, n_d=1) --> L",
  "Find translation matrix for the given distance and refractive index.",
  help.NEW}


--- Find ray parameters after transformation.
--  Ray is represented with height 'y' and angle 'V' equal to v*n
--  where v is geometrical angle and n is refractive index.
--  @param dy Position.
--  @param dV Optical angle.
--  @return y and V after transformation.
lens.transform = function (self, dy, dV)
  return (self[1]*dy + self[2]*dV), (self[3]*dy + self[4]*dV)
end
about[lens.transform] = {"L:transform(yIn_d, VIn_d) --> yOut_d, VOut_d",
  "Find the output ray position 'y' and optical angle 'V' (= v*n). Equal to L(y,V).",
  help.OTHER}
-- Simplified call of transformation.
lens.__call = lens.transform


-- explain the vector of cardinal points
local mt_cardinal = {
  -- mark
  type = 'cardinal_points',
  -- pretty pring
  __tostring = function (self)
    local txt = {}
    txt[1] = 'From the input plane'
    txt[2] = 'F1 at '..tostring(self.F1)
    txt[3] = 'H1 at '..tostring(self.H1)
    txt[4] = 'N1 at '..tostring(self.N1)
    txt[5] = 'From the output plane'
    txt[6] = 'F2 at '..tostring(self.F2)
    txt[7] = 'H2 at '..tostring(self.H2)
    txt[8] = 'N2 at '..tostring(self.N2)
    return table.concat(txt, '\n')
  end
}


--- Find cardinal points of the system.
--  @param dn1 Input refractive index, default is 1.
--  @param dn2 Output refractive index, default is 1.
--  @return List of cardinal points location.
lens.cardinal = function (self, dn1, dn2)
  dn1 = dn1 or 1
  dn2 = dn2 or 1
  local txt, res = {}, {}
  local C = self[3]

  local v = dn1 * self[4] / C      -- first focus point
  res.F1 = v
  res.H1 = v - dn1 / C             -- first principal point
  res.N1 = (self[4]*dn1 - dn2) / C -- first nodal point

  v = -dn2 * self[1] / C           -- second focus point
  res.F2 = v
  res.H2 = v + dn2 / C             -- second principal point
  res.N2 = (dn1 - self[1]*dn2) / C -- second nodal point

  return setmetatable(res, mt_cardinal)
end
about[lens.cardinal] = {"L:cardinal(nLft_d=1, nRht_d=1) --> points_t",
  "Find location of the cardinal points of the given system w.r.t input and output planes, use refractive indeces if need. Return table of distances.",
  ANALYZE}


-- Create arbitrary object
setmetatable(lens, {
__call = function (_, A, B, C, D)
  assert(A and B and C and D)
  return lens._init({A, B, C, D})
end})
about[lens] = {" (A_d, B_d, C_d, D_d) --> new_L",
  "Make new lens component.", help.NEW}


-- Comment to remove descriptions
lens.about = about

return lens

--======================================
