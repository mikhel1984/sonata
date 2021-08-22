--[[		sonata/lib/geodesy.lua

--- Coordinate transformations and other geodetic tasks
--  @author Stanislav Mikhel

	module 'geodesy'
--]]

--[[TEST

-- use 'geodesy'
Geo = require 'lib.geodesy'

-- example
a = Geo()
ans = a.type   -->  'geodesy'

--]]

--	LOCAL

local s_TOL = math.rad(1E-4 / 3600)   -- 0.0001'' 

--- Check object type.
--  @param t Object.
--  @return True if the object is geodesy.
local function isgeodesy(t) return type(t)=='table' and t.isgeodesy end

local function toRad(d,m,s) return math.rad(d + (m or 0) / 60 + (s or 0) / 3600) end

--	INFO

local help = LC_DIALOG and (require "core.help") or {new=function () return {} end}

--	MODULE

local ellipsoid = {}
-- methametods
ellipsoid.__index = ellipsoid

-- t = {B = b, L = l, H = h}
ellipsoid.toXYZ = function (E, t)
  -- convert to radians
  local B, L, H = math.rad(t.B), math.rad(t.L), t.H
  local sB = math.sin(B)
  local N = E.a / math.sqrt(1 - E.e2 * sB * sB)
  local NH = (N + H) * math.cos(B)
  return {
    X = NH * math.cos(L),
    Y = NH * math.sin(L),
    Z = ((1-E.e2)*N + H) * sB
  }
end

-- t = {X = x, Y = y, Z = z}
ellipsoid.toBLH = function (E, t)
  local X, Y, Z = t.X, t.Y, t.Z
  local D = math.sqrt(X*X + Y*Y)
  local B, L, H  -- result in rad
  if D < 1E-6 then
    B = (Z == 0 and 0 or (Z > 0 and 0.5 or -0.5)) * math.pi
    local sB = math.sin(B)
    L = 0
    H = Z * sB - E.a * math.sqrt(1 - E.e2 * sB * sB)
  else
    local La = math.asin(Y/D)
    -- check Y
    if Y < 0 then
      L = (X < 0) and (math.pi + La) or (2*math.pi - La)
    elseif Y > 0 then
      L = (X < 0) and (math.pi - La) or La
    else -- Y == 0
      L = (X < 0) and math.pi or 0
    end
    -- check Z
    if Z == 0 then
      B = 0
      H = D - E.a
    else
      -- auxiliary
      local r = math.sqrt(X*X + Y*Y + Z*Z)
      local c = math.asin(Z / r)
      local p = E.e2 * E.a / (2 * r)
      local s2, b, sb = 0
      repeat
        local s1 = s2
        b = c + s1
        sb = math.sin(b)
        s2 = math.asin(p * sb / math.sqrt(1 - E.e2 * sb * sb))
      until math.abs(s2-s1) < s_TOL
      B = b
      H = D * math.cos(B) + Z * sb - E.a * math.sqrt(1 - E.e2 * sb * sb)
    end
  end
  return {
    B = math.deg(B),
    L = math.deg(L),
    H = H
  }
end

-- par = {dX, dY, dZ; wx, wy, wz; m}
ellipsoid._fwdXYZ = function (par)
  local m = 1 + par[7]
  local wx, wy, wz = par[4], par[5], par[6]
  -- t = {X = x, Y = y, Z = z}
  return function (t) 
    return {
      X = m * ( t.X + wz * t.Y - wy * t.Z) + par[1],
      Y = m * (-wz * t.X + t.Y + wx * t.Z) + par[2],
      Z = m * ( wy * t.X - wx * t.Y + t.Z) + par[3]
    }
  end
end

ellipsoid._bwdXYZ = function (par)
  local m = 1 - par[7]
  local wx, wy, wz = par[4], par[5], par[6]
  return function (t)
    return {
      X = m * ( t.X - wz * t.Y + wy * t.Z) - par[1],
      Y = m * ( wz * t.X + t.Y - wx * t.Z) - par[2],
      Z = m * (-wy * t.X + wx * t.Y + t.Z) - par[3]
    }
  end
end

setmetatable(ellipsoid, {
__call = function (self,t)
  local f = t.f  -- flattening
  t.e2 = f*(2-f)
  t.b = t.a*(1-f)
  return setmetatable(t, self)
end
})


local geodesy = {
-- mark
type = 'geodesy', isgeodesy = true,
-- description
about = help:new("Coordinate transformations and other geodetic tasks"),

-- ellipsoid approximations
WGS84 = ellipsoid {a = 6378137, f = 1/298.257223563}, -- 
-- russian systems
PZ90 = ellipsoid {a = 6378136, f = 1/298.25784}, 
PZ9002 = ellipsoid {a = 6378136, f = 1/298.25784}, 
SK42 = ellipsoid {a = 6378245, f = 1/298.3}
}
-- methametods
geodesy.__index = geodesy

-- Uncomment to remove descriptions
--geodesy.about = nil

return geodesy

--======================================