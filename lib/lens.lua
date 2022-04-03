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
-- external dependencies, can be loaded implicitly 
Num = require 'lib.numeric' -- for root searching

-- define a simple lense 
n1 = 1      -- air 
n2 = 1.56   -- glass 
-- radius 200 mm, thickness 5 mm
lens1 = Lens.ref(200,n1,n2)..Lens.trans(5,n2)..Lens.ref(-200,n2,n1) 
ans = lens1:isUnit()          --> true

-- get matrix element
ans = lens1.D                --2> 1

-- find cardinal points 
pts = lens1:cardinal() 
ans = pts[1]                 --2> -177.76

-- print points
print(pts)

-- object is located 250 mm to the left 
-- from the lens, find position of 
-- the image 
d1 = 250
fn = function (d2)
  return Lens.trans(d1,n1)..lens1..Lens.trans(d2,n1) 
end
-- solve for B = 0, initial guess dist = 100
d2 = Lens.solve(fn, Lens.key.B, 100)  
ans = d2                     --2>  623.21

-- check solution 
-- assume the lens it thin 
f = -pts[1]
ans = 1/d1 + 1/d2            --2> 1/f

-- ray transformation 
y1 = 10          -- mm, height   
V1 = n1 * 0.05   -- optical angle
sys1 = fn(d2) 
y2, V2 = sys1(y1,V1)
ans = y2                     --2> -24.83

-- from image to object 
sys2 = sys1:inv()  -- transpose
ans, _ = sys2(y2, V2)        --3> y1

-- system matrix determinant
ans = lens1:det()            --3> 1

-- create thin lens
lens2 = Lens.thin(f)
_, V3 = lens1(y1,V1)
_, ans = lens2(y1,V1)        --2> V3

-- flat mirror 
lens3 = Lens.mirror(math.huge, n1)
_, ans = lens3(y1,V1)        --2> V1

-- afocal system 
m = 10
lens4 = Lens.afocal(m) 
ans, _ = lens4(y1,V1)        --2> m*y1

-- arbitrary system matrix 
lens5 = Lens {1, 0, -0.5, 1} 
print(lens5)

-- make copy 
ans = lens1:copy()            --> lens1

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

local help = SonataHelp or {new=function () return {} end}
-- description
local about = help:new("Matrix methods in paraxial optics")

--	MODULE

local lens = {
-- mark
type = 'lens', islens = true,
-- {A,B,C,D}
key = keys,
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

--- Make component for afocal system.
--  @param dm Transverse magnification.
--  @return Afocal matrix.
lens.afocal = function (dm)
  return lens:_init_({dm, 0, 0, 1/dm}) 
end
about[lens.afocal] = {"afocal(dm)", "Find matrix for the afocal system.", help.NEW}

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

--- Inverse the component matrix.
--  @param L Lens object.
--  @return Inverted matrix.
lens.inv = function (L)
  -- assume the unit matrix
  return lens:_init_({L[4], -L[2], -L[3], L[1]})
end
about[lens.inv] = {"inv(L)", "Get the inverted system matrix.", help.OTHER}

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
about[lens.Lens] = {"Lens {dA,dB,dC,dD}", "Make new lens component.", help.NEW}

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

