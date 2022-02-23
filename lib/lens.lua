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

--- Check object type.
--  @param v Object.
--  @return True if the object is 'lens'.
local function islens(v) return type(v)=='table' and v.islens end

--- Check matrix type.
--  @param L Lens object.
--  @return True if the matrix is unit.
local function isUnit(L) return math.abs(L[1]*L[4]-L[2]*L[3]-1) < 1E-6 end

--	INFO

local help = SonataHelp and (require "core.help") or {new=function () return {} end}

-- description
local about = help:new("Matrix methods in paraxial optics")

--	MODULE

local lens = {
-- mark
type = 'lens', islens = true,
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

--- Object constructor.
--  @param t ABCD table.
--  @return 'Lens' object.
lens._init_ = function(self,t) return setmetatable(t,self) end

--- Make component for transition.
--  @param dt Distance.
--  @param dn Refractive index.
--  @return Transition matrix.
lens.trans = function (dt, dn)
  return lens:_init_({ 1, dt/dn, 0, 1 })
end

--- Make component for refraction.
--  @param dr Radius of a surface.
--  @param dn1 Initial refractive index.
--  @param dn2 Final refractive index.
--  @return Refraction matrix.
lens.ref = function (dr, dn1, dn2)
  return lens:_init_({ 1,  0, -(dn2-dn1)/dr, 1 })
end

--- Make component for a thin lens.
--  @param df Focal length.
--  @return Thin lens matrix.
lens.thin = function (df)
  return lens:_init_({ 1, 0, -1/df, 1 })
end

--- Make component for a curved mirror.
--  @param dr Radius of a mirror surface.
--  @param dn Refractive index.
--  @return Curved mirror matrix.
lens.mirror = function (dr, dn)
  return lens:_init_({ 1, 0, 2*dn/dr, 1 })
end

--- Concatenate components along the ray trace.
--  Equal to matrix product in the opposite order.
--  @param L1 First object.
--  @param L2 Second object.
--  @return Concatenated object.
lens.__concat = function (L1, L2)
  if not (isUnit(L1) and isUnit(L2)) then error("Wrong system matrices") end
  return lens:_init_({
    L2[1]*L1[1]+L2[2]*L1[3], L2[1]*L1[2]+L2[2]*L1[4],
    L2[3]*L1[1]+L2[4]*L1[3], L2[3]*L1[2]+L2[4]*L1[4]
  })
end

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
-- Simplified call of transformation.
lens.__call = lens.transform

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

-- Comment to remove descriptions
lens.about = about

return lens

--======================================

--[[
n = 1.5
a = lens.ref(0.2, 1, n) 
b = lens.trans(0.005, n)
c = lens.ref(-0.2, n, 1)

sys = a..b..c
print(sys(0.1, 0))
]]
