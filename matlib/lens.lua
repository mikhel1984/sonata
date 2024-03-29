--[[		sonata/lib/lens.lua

--- Matrix methods in paraxial optics.
--
--  Each element is represented in form of 2x2 unit matrix
--  { A, B,
--    C, D }
--  See, for example,
--  "Introduction to matrix methods in optics" by A.Gerrard, J.M.Burch.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2023.

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
lens1 = Lens:ref(200,n1,n2)
        ..Lens:trans(5,n2)
        ..Lens:ref(-200,n2,n1)
ans = lens1:isUnit()          -->  true

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
  return Lens:trans(d1)..lens1..Lens:trans(d2)
end
-- solve for B = 0, initial guess dist = 100
d2 = Lens:solve(fn, Lens.key.B, 100)
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

-- system matrix determinant
ans = lens1:det()            --3>  1

-- create thin lens
lens2 = Lens:thin(f)
_, V3 = lens1(y1,V1)
_, ans = lens2(y1,V1)        --2>  V3

-- flat mirror
lens3 = Lens:mirror(math.huge, n1)
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
ans, _ = Lens:gaussParam(1E-3, lambda) --2>  3.26E-4

-- laser beam radius
ans, _ = Lens:gaussSize(1E-3, lambda, 100) --2>  0.033

-- laser beam transformation
ans, _ = lens1:beam(1E-3, 1E3, lambda)  --2>  1.44E-3

--]]


--	LOCAL

-- use letters for convenience
local keys = {A=1, B=2, C=3, D=4}

-- tolerance of comparison
local TOL = 1E-8
local LASER = 'laser'


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
local function islens(v) return getmetatable(v) == lens end

--- Concatenate components along the ray trace.
--  Equal to matrix product in the opposite order.
--  @param L1 First object.
--  @param L2 Second object.
--  @return Concatenated object.
lens.__concat = function (L1, L2)
  if not (lens.isUnit(L1) and lens.isUnit(L2)) then
    error("Wrong system matrices")
  end
  return lens._init({
    L2[1]*L1[1]+L2[2]*L1[3], L2[1]*L1[2]+L2[2]*L1[4],
    L2[3]*L1[1]+L2[4]*L1[3], L2[3]*L1[2]+L2[4]*L1[4]
  })
end


--- Compare two objects.
--  @param L1 First object.
--  @param L2 Second object.
--  @return True if the objects are equal.
lens.__eq = function (L1, L2)
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
  if keys[k] then
    t[keys[k]] = v
  end
end


--- String representation.
--  @param L Lens object.
--  @return Equal string.
lens.__tostring = function (L)
  return string.format(
    "A: %.2f   B: %.2f\nC: %.2f   D: %.2f", L[1], L[2], L[3], L[4])
end


about['_op'] = {"operations: L1 == L2, L1 .. L2", nil, help.META }


--- Object constructor.
--  @param t ABCD table.
--  @return 'Lens' object.
lens._init = function(t) return setmetatable(t, lens) end


--- Make component for afocal system.
--  @param dm Transverse magnification.
--  @return Afocal matrix.
lens.afocal = function (self, dm)
  return lens._init({dm, 0, 0, 1/dm})
end
about[lens.afocal] = {":afocal(magn_d) --> L",
  "Find matrix for the afocal system.", help.NEW}


--- Find Gaussian beam parameters after transformation.
--  @param L Lens object.
--  @param dW Input beam radius, m.
--  @param dR Input beam curvature, m.
--  @param dLam Wavelength, nm.
--  @return Output beam radius and curvature.
lens.beam = function (L, dW, dR, dLam)
  lens.ext_complex = lens.ext_complex or require("matlib.complex")
  local lampi = dLam * 1E-9 / math.pi
  local _1_q1 = lens.ext_complex(1/dR, lampi / (dW * dW))
  local _1_q2 = (L[3] + L[4]*_1_q1) / (L[1] + L[2]*_1_q1)
  return math.sqrt(lampi / _1_q2:im()), 1/_1_q2:re()
end
about[lens.beam] = {"L:beam(inRad_d, inCurv_d, lambda_d) --> outRad_d, outCurv_d",
  "Find output beam radius and curvature.", LASER}


--- Make a copy.
--  @param L Initial object.
--  @return Copy of the object.
lens.copy = function (L)
  return lens._init({L[1], L[2], L[3], L[4]})
end
about[lens.copy] = {"L:copy() --> cpy_L", "Create a copy of the object."}


--- Find the matrix determinant.
--  @param L Lens object.
--  @return Determinant value.
lens.det = function (L)
  return L[1]*L[4] - L[2]*L[3]
end
about[lens.det] = {"L:det() --> determinant_d",
  "Find determinant of the system matrix.", help.OTHER}


--- Find Gaussian beam characterictics.
--  @param dW0 Beam waist, m.
--  @param dLam Waveleight, nm.
--  @return Divergence angle and Raileigh range.
lens.gaussParam = function (self, dW0, dLam)
  dLam = dLam * 1E-9
  local t = math.pi * dW0 / dLam
  return 1/t, t * dW0
end
about[lens.gaussParam] = {":gaussParam(waist_d, lambda_d) --> div_d, range_d",
  "Find divergence angle and Raileigh range for a Gaussian beam.", LASER}


--- Find Gaussian beam propagation.
--  @param dW0 Beam waist, m.
--  @param dLam Waveleight, nm.
--  @param dist Distance, m.
--  @return Beam radius and curvature in pose dist.
lens.gaussSize = function (self, dW0, dLam, dist)
  dLam = dLam * 1E-9
  local t = (math.pi * dW0 * dW0 / dLam / dist) ^ 2
  return dW0 * math.sqrt(1 + 1/t), dist * (1 + t)
end
about[lens.gaussSize] = {":gaussSize(waist_d, lambda_d, dist_d) --> rad_d, curv_d",
  "Find Gaussian beam radius and curvature at some distance.", LASER}


--- Inverse the component matrix.
--  @param L Lens object.
--  @return Inverted matrix.
lens.inv = function (L)
  -- assume the unit matrix
  return lens._init({L[4], -L[2], -L[3], L[1]})
end
about[lens.inv] = {"L:inv() --> inv_L", "Get the inverted system matrix.", help.OTHER}


--- Check matrix type.
--  @param L Lens object.
--  @return True if the matrix is unit.
lens.isUnit = function (L)
  return math.abs(L[1]*L[4]-L[2]*L[3]-1) < TOL
end
about[lens.isUnit] = {"L:isUnit() --> bool",
  "Check if the system matrix is unit.", help.OTHER}


--- Make component for a curved mirror.
--  @param dr Radius of a mirror surface.
--  @param dn Refractive index.
--  @return Reflection matrix.
lens.mirror = function (self, dr, dn)
  return lens._init({ 1, 0, 2*dn/dr, 1 })
end
about[lens.mirror] = {":mirror(rad_d, n_d) --> L",
  "Find reflection matrix for the given radius and refractive index.",
  help.NEW}


--- Make component for refraction.
--  @param dr Radius of a surface.
--  @param dn1 Initial refractive index.
--  @param dn2 Final refractive index.
--  @return Refraction matrix.
lens.ref = function (self, dr, dn1, dn2)
  return lens._init({ 1,  0, -(dn2-dn1)/dr, 1 })
end
about[lens.ref] = {":ref(rad_d, n1_d, n2_d) --> L",
  "Find refraction matrix for the given radius of surface and input and output refractive indeces.",
  help.NEW}


--- Find condition when fn(d).X == 0
--  where X in {A, B, C, D}.
--  @param fn System in form of function.
--  @param ind Index of the matrix element.
--  @param d0 Initial estimation of the variable.
--  @return The found parameter value.
lens.solve = function (self, fn, ind, d0)
  lens.ext_numeric = lens.ext_numeric or require('matlib.numeric')
  -- prepare 'equation'
  local eqn = function (d)
    local v = fn(d)
    return v[ind]
  end
  -- try to solve
  return lens.ext_numeric:newton(eqn, d0)
end
about[lens.solve] = {":solve(fn, index_N, initial_d) --> found_d",
  "Find condition when component with the given index is equal to 0, use initial assumption."}


--- Make component for a thin lens.
--  @param df Focal length.
--  @return Thin lens matrix.
lens.thin = function (self, df)
  return lens._init({ 1, 0, -1/df, 1 })
end
about[lens.thin] = {":thin(focalDist_d) --> L",
  "Find the thin lens system matrix for the given focal distance.", help.NEW}


--- Make component for translation.
--  @param dt Distance.
--  @param dn Refractive index.
--  @return Translational matrix.
lens.trans = function (self, dt, dn)
  dn = dn or 1
  return lens._init({ 1, dt/dn, 0, 1 })
end
about[lens.trans] = {":trans(dist_d, n_d=1) --> L",
  "Find translation matrix for the given distance and refractive index.",
  help.NEW}


--- Find ray parameters after transformation.
--  Ray is represented with height 'y' and angle 'V' equal to v*n
--  where v is geometrical angle and n is refractive index.
--  @param dy Position.
--  @param dV Optical angle.
--  @return y and V after transformation.
lens.transform = function (L, dy, dV)
  return (L[1]*dy + L[2]*dV), (L[3]*dy + L[4]*dV)
end
about[lens.transform] = {"L:transform(yIn_d, VIn_d) --> yOut_d, VOut_d",
  "Find the output ray position 'dy' and optical angle 'dV' (= v*n). Equal to call L(dy,dV)."}
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
--  @param L Lens object.
--  @param dn1 Input refractive index, default is 1.
--  @param dn2 Output refractive index, default is 1.
--  @return List of cardinal points location.
lens.cardinal = function (L, dn1, dn2)
  dn1 = dn1 or 1
  dn2 = dn2 or 1
  local txt, res = {}, {}
  local C = L[3]

  local v = dn1 * L[4] / C      -- first focus point
  res.F1 = v
  res.H1 = v - dn1 / C          -- first principal point
  res.N1 = (L[4]*dn1 - dn2) / C -- first nodal point

  v = -dn2 * L[1] / C           -- second focus point
  res.F2 = v
  res.H2 = v + dn2 / C          -- second principal point
  res.N2 = (dn1 - L[1]*dn2) / C -- second nodal point

  return setmetatable(res, mt_cardinal)
end
about[lens.cardinal] = {"L:cardinal(nLft_d=1, nRht_d=1) --> points_t",
  "Find location of the cardinal points of the given system w.r.t input and output planes, use refractive indeces if need. Return table of distances.",
  help.OTHER}


-- Create arbitrary object
setmetatable(lens, {
__call = function (self, A, B, C, D)
  assert(A and B and C and D)
  return lens._init({A, B, C, D})
end})
about[lens] = {" (A_d, B_d, C_d, D_d) --> new_L", "Make new lens component.", help.NEW}


-- Comment to remove descriptions
lens.about = about

return lens

--======================================
--TODO rename ref -> R, trans -> T
