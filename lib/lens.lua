--[[		sonata/lib/lens.lua

--- Matrix methods in paraxial optics
--  @author Stanislav Mikhel

	module 'lens'
--]]

-- Define here your tests, save results to 'ans', use --> for the strict equality 
-- and --n> for the n-digit precision in the case of floating numbers.
--[[TEST

-- use 'lens'
Lens = require 'lib.lens'

-- example
a = Lens()
ans = a.type   -->  'lens'

ans = math.pi  --2> 355/113

--]]

--	LOCAL

--- Check object type.
--  @param t Object.
--  @return True if the object is lens.
local function islens(v) return type(v)=='table' and v.islens end

local function isUnit(L) return math.abs(L[1]*L[4]-L[2]*L[3]-1) < 1E-6 end

--	INFO

local help = SonataHelp and (require "core.help") or {new=function () return {} end}

--	MODULE

local lens = {
-- mark
type = 'lens', islens = true,
-- description
about = help:new("Matrix methods in paraxial optics"),
}
-- methametods
lens.__index = lens


--- Constructor example.
--  @param t Some value.
--  @return New object of lens.
lens._new_ = function(self,t) return setmetatable(t,self) end

-- dt distance
-- dn refractive index
lens.trans = function (dt, dn)
  return lens:_new_({ 1, dt/dn, 0, 1 })
end

-- dr radius of surface
-- dn1 initial refractive index
-- dn2 final refractive index
lens.ref = function (dr, dn1, dn2)
  return lens:_new_({ 1,  0, -(dn2-dn1)/dr, 1 })
end

-- df focal length
lens.thin = function (df)
  return lens:_new_({ 1, 0, -1/df, 1 })
end

-- dr radius of surface
-- dn refractive index
lens.mirror = function (dr, dn)
  return lens:_new_({ 1, 0, 2*dn/dr, 1 })
end


-- concatenate matrices along the ray trace
-- equal to matrix product in the opposite order
lens.__concat = function (L1, L2)
  if not (isUnit(L1) and isUnit(L2)) then error("Wrong system matrices") end
  return lens:_new_({
    L2[1]*L1[1]+L2[2]*L1[3], L2[1]*L1[2]+L2[2]*L1[4],
    L2[3]*L1[1]+L2[4]*L1[3], L2[3]*L1[2]+L2[4]*L1[4]
  })
end

lens.__tostring = function (L)
  return string.format("A:%.2d B:%.2d\nC:%.2d D:%2d", L[1], L[2], L[3], L[4])
end

-- dy position
-- dV optical angle
lens.transform = function (L, dy, dV)
  return (L[1]*dy + L[2]*dV), (L[3]*dy + L[4]*dV)
end

lens.__call = lens.transform

-- simplify constructor call
--setmetatable(lens, {__call = function (self,v) return lens:new(v) end})
--lens.Lens = 'Lens'
--lens.about[lens.Lens] = {"Lens(t)", "Create new lens.", help.NEW}

--- Method example.
--  It is good idea to define method for the copy creation.
--  @param t Initial object.
--  @return Copy of the object.
lens.copy = function (t)
  -- some logic
  return lens:new(argument)
end
lens.about[lens.copy] = {"copy(t)", "Create a copy of the object."} -- third element is optional, default is 'base'

-- Uncomment to remove descriptions
--lens.about = nil

return lens

--======================================
--TODO: write new functions
