--[[		sonata/lib/lens.lua

--- Matrix methods in paraxial optics.
--
--  Each element is represented in form of 2x2 unit matrix 
--  { A, B,
--    C, D }
--  See, for example, "Introduction to matrix methods in optics" by A.Gerrard, J.M.Burch.
--
--  @author Stanislav Mikhel

	module 'lens'
--]]

----------------------- Tests ----------------------
--[[TEST

-- use 'lens'
Lens = require 'lib.lens'

-- example
a = Lens()
ans = a.type   -->  'lens'

ans = math.pi  --2> 355/113

--]]

--	LOCAL

-- use letters for convenience
local keys = {A=1, B=2, C=3, D=4}

-- tolerance of comparison
local TOL = 1E-8

--- Check object type.
--  @param v Object.
--  @return True if the object is 'lens'.
local function islens(v) return type(v)=='table' and v.islens end

--	INFO

local help = SonataHelp and (require "core.help") or {new=function () return {} end}

-- description
local about = help:new("Matrix methods in paraxial optics")

--	MODULE

local lens = {
-- mark
type = 'lens', islens = true,
-- {A,B,C,D}
keys = keys,
}

--- Call unknown key.
--  @param t Table.
--  @param k Key.
--  @return Value or method.
lens.__index = function (t,k)
  return lens[k] or rawget(t, keys[k] or '')
end

--- Set unknown key.
--  @param t Table.
--  @param k Key.
--  @param v Value.
lens.__newindex = function (t,k,v)
  if keys[k] then
    t[keys[k]] = v
  end
end

--- Check matrix type.
--  @param L Lens object.
--  @return True if the matrix is unit.
lens.isUnit = function (L) 
  return math.abs(L[1]*L[4]-L[2]*L[3]-1) < TOL 
end
about[lens.isUnit] = {"isUnit(L)", "Check if the system matrix is unit.", help.OTHER}

--- Object constructor.
--  @param t ABCD table.
--  @return 'Lens' object.
lens._init_ = function(self,t) return setmetatable(t,self) end

--- Make component for translation.
--  @param dt Distance.
--  @param dn Refractive index.
--  @return Translational matrix.
lens.trans = function (dt, dn)
  return lens:_init_({ 1, dt/dn, 0, 1 })
end
about[lens.trans] = {"trans(dt,dn)", "Find translation matrix for the given distance and refractive index.", help.NEW}

--- Make component for refraction.
--  @param dr Radius of a surface.
--  @param dn1 Initial refractive index.
--  @param dn2 Final refractive index.
--  @return Refraction matrix.
lens.ref = function (dr, dn1, dn2)
  return lens:_init_({ 1,  0, -(dn2-dn1)/dr, 1 })
end
about[lens.ref] = {"ref(dr,dn1,dn2)", "Find refraction matrix for the given radius of surface and input and output refractive indeces.", help.NEW}

--- Make component for a thin lens.
--  @param df Focal length.
--  @return Thin lens matrix.
lens.thin = function (df)
  return lens:_init_({ 1, 0, -1/df, 1 })
end
about[lens.thin] = {"thin(df)", "Find the thin lens system matrix for the given focal distance.", help.NEW}

--- Make component for a curved mirror.
--  @param dr Radius of a mirror surface.
--  @param dn Refractive index.
--  @return Reflection matrix.
lens.mirror = function (dr, dn)
  return lens:_init_({ 1, 0, 2*dn/dr, 1 })
end
about[lens.mirror] = {"mirror(dr,dn)", "Find reflection matrix for the given radius and refractive index.", help.NEW}

--- Concatenate components along the ray trace.
--  Equal to matrix product in the opposite order.
--  @param L1 First object.
--  @param L2 Second object.
--  @return Concatenated object.
lens.__concat = function (L1, L2)
  if not (lens.isUnit(L1) and lens.isUnit(L2)) then error("Wrong system matrices") end
  return lens:_init_({
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

lens.operations = 'operations'
about[lens.operations] = {lens.operations, "L1 == L2, L1 .. L2", help.META }

--- String representation.
--  @param L Lens object.
--  @return Equal string.
lens.__tostring = function (L)
  return string.format("A: %.2f   B: %.2f\nC: %.2f   D: %.2f", L[1], L[2], L[3], L[4])
end

--- Find ray parameters after transformation. 
--  Ray is represented with height 'y' and angle 'V' equal to v*n
--  where v is geometrical angle and n is refractive index.
--  @param dy Position.
--  @param dV Optical angle.
--  @return y and V after transformation.
lens.transform = function (L, dy, dV)
  return (L[1]*dy + L[2]*dV), (L[3]*dy + L[4]*dV)
end
about[lens.transform] = {"transform(L,dy,dV)", "Find the output ray position 'dy' and optical angle 'dV' (= v*n). Equal to call L(dy,dV)."}
-- Simplified call of transformation.
lens.__call = lens.transform

--- Transpose the component matrix.
--  @param L Lens object.
--  @return Transposed object.
lens.T = function (L)
  return lens:_init_({L[1], L[3], L[2], L[4]})
end
about[lens.T] = {"T(L)", "Get the transposed system matrix.", help.OTHER}

--- Find condition when fn(d).X == 0
--  where X in {A,B,C,D}.
--  @param fn System in form of function.
--  @param ind Index of the matrix element. 
--  @param d0 Initial estimation of the variable.
--  @return The found parameter value.
lens.solve = function (fn, ind, d0)
  lens.ext_numeric = lens.ext_numeric or require('lib.numeric')
  -- prepare 'equation'
  local eqn = function (d)
    local v = fn(d)
    return v[ind]
  end
  -- try to solve
  return lens.ext_numeric.Newton(eqn, d0)
end
about[lens.solve] = {"solve(fn,ind,d0)", "Find condition when component with the given index is equal to 0, d0 is the initial assumption."}

--- Find the matrix determinant.
--  @param L Lens object.
--  @return Determinant value.
lens.det = function (L)
  return L[1]*L[4] - L[2]*L[3]
end
about[lens.det] = {"det(L)", "Find determinant of the system matrix.", help.OTHER}

-- Create arbitrary object
setmetatable(lens, {__call = function (self,t) 
  assert(#t == 4)
  return lens:_init_(t)
end})
lens.Lens = 'Lens'
about[lens.Lens] = {"Lens(t)", "Make new lens component.", help.NEW}

--- Make a copy.
--  @param L Initial object.
--  @return Copy of the object.
lens.copy = function (L)
  return lens:_init_({L[1],L[2],L[3],L[4]})
end
about[lens.copy] = {"copy(L)", "Create a copy of the object."} 

-- explain the vector of cardinal points
local mt_cardinal = {
  -- mark
  type = 'cardinal_points',
  -- pretty pring
  __tostring = function (self)
    local txt = {}
    txt[1] = 'From the input plane'
    txt[2] = 'F1 at '..tostring(self[1]) 
    txt[3] = 'H1 at '..tostring(self[2])
    txt[4] = 'N1 at '..tostring(self[3]) 
    txt[5] = 'From the output plane'
    txt[6] = 'F2 at '..tostring(self[4])
    txt[7] = 'H2 at '..tostring(self[5])
    txt[8] = 'N2 at '..tostring(self[6])
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
  local txt, res = {}, {0,0,0;0,0,0}
  local C = L[3]

  local v = dn1 * L[4] / C      -- first focus point
  res[1] = v 
  res[2] = v - dn1 / C          -- first principal point 
  res[3] = (L[4]*dn1 - dn2) / C -- first nodal point

  v = -dn2 * L[1] / C           -- second focus point
  res[4] = v
  res[5] = v + dn2 / C          -- second principal point
  res[6] = (dn1 - L[1]*dn2) / C -- second nodal point

  return setmetatable(res, mt_cardinal)
end
about[lens.cardinal] = {"cardinal(L[,dn1=1,dn2=1])", "Find location of the cardinal points of the given system w.r.t input and output planes, use refractive indeces if need. Return list of distances.", help.OTHER}

-- Comment to remove descriptions
lens.about = about

return lens

--======================================

