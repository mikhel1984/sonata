--[[		sonata/lib/geodesy.lua

--- Coordinate transformations and other geodetic tasks
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2021.

	module 'geodesy'
--]]

--[[TEST

-- use 'geodesy'
Geo = require 'lib.geodesy'

-- generate random from number -1 to 1
rnd = function () return 2*math.random()-1 end
-- random coordinates (degrees and meters)
t0 = {B=rnd()*90, L=rnd()*180, H=rnd()*1000}
print(t0.B, t0.L, t0.H)

--test in WGS84
wgs84 = Geo.WGS84
-- BLH to XYZ
t1 = wgs84:toXYZ(t0)
print(t1.X, t1.Y, t1.Z)

-- XYZ to BHL
t2 = wgs84:toBLH(t1)
print(t2.B, t2.L, t2.H)
ans = t2.B                 --3> t0.B

ans = t2.L                 --3> t0.L

ans = t2.H                 --3> t0.H



--]]

--	LOCAL

-- Compatibility with previous versions
local Ver = require "lib.versions"

local s_TOL = math.rad(1E-4 / 3600)   -- 0.0001'' 

--- Check object type.
--  @param t Object.
--  @return True if the object is geodesy.
local function isgeodesy(t) return type(t)=='table' and t.isgeodesy end

--	INFO

local help = SONATA_DIALOG and (require "core.help") or {new=function () return {} end}

--	MODULE

-- Reference ellipsoid
local ellipsoid = {}
-- methametods
ellipsoid.__index = ellipsoid

-- temporary save here the function description
local _about_ = help:new("Coordinate transformations and other geodetic tasks")

--- Ellipsoid object constructor.
--  @param t Table with parameters, obligatory are semi-major axis, flattening.
--  @return New ellipsoid.
ellipsoid.new = function (self,t)
  local f = t.f  -- flattening
  t.e2 = f*(2-f)
  t.b = t.a*(1-f)
  t.xyzInto = {}
  return setmetatable(t, self)
end

--- Transform Geodetic coordinats to Cartesian.
--  @param E Reference ellipsoid object.
--  @param t Dictionary with coordinates: B (deg), L (deg), H (m).
--  @return Dictionary with coordinates in meters: X, Y, Z.
ellipsoid.toXYZ = function (E, t)
  -- convert to radians
  local B, L, H = math.rad(t.B), math.rad(t.L), t.H
  local sB = math.sin(B)
  local N = E.a / math.sqrt(1 - E.e2 * sB * sB)
  local NH = (N + H) * math.cos(B)
  return {
    X = NH * math.cos(L),
    Y = NH * math.sin(L),
    Z = ((1-E.e2)*N + H) * sB }
end
_about_[ellipsoid.toXYZ] = {"toXYZ(E,tBLH)", "Transform Geodetic coordinates to Cartesian."}

--- Transform Cartesian coordinates to Geodetic.
--  @param E Reference ellipsoid object.
--  @param t Dictionary with coordinates in meters: X, Y, Z.
--  @return Dictionary with coordinates: B (deg), L (deg), H (m).
ellipsoid.toBLH = function (E, t)
  local X, Y, Z = t.X, t.Y, t.Z
  local D = math.sqrt(X*X + Y*Y)
  local B, L, H  -- result in rad
  if D < 1E-10 then
    B = (Z == 0 and 0 or (Z > 0 and 0.5 or -0.5)) * math.pi
    L = 0
    H = math.abs(Z) - E.b
  else
    -- solve Browning's iterative equation
    local e22 = E.e2 / (1 - E.e2)
    -- initial value for tan(B)
    local tg = Z / D * (1 + e22 * E.b / math.sqrt(X*X + Y*Y + Z*Z))
    for i = 1,2 do       -- TODO: use tol estimation
      tg = tg * (1 - E.f)    -- get tan(theta)
      local theta = math.atan(tg)
      tg = (Z + e22 * E.b * math.sin(theta)^3) / (D - E.e2 * E.a * math.cos(theta)^3)
    end
    B = math.atan(tg)
    L = Ver.atan2(Y,X)
    local sb = math.sin(B)
    local N = E.a / math.sqrt(1 - E.e2 * sb * sb)
    if math.abs(tg) > 1 then
      H = Z / sb - (1 - E.e2) * N
    else
      H = D / math.cos(B) - N
    end
  end
  return {
    B = math.deg(B),
    L = math.deg(L),
    H = H }
end
_about_[ellipsoid.toBLH] = {"toBLH(E,tXYZ)", "Transform Cartesian coordinates to Geodetic."}

--- Transform coordinates between two Cartesian systems, forward direction.
--  @param par List of translations, rotations and scale {dX, dY, dZ; wx, wy, wz; m}.
--  @return Function for coordinate transformation.
ellipsoid._fwdXYZ = function (par)
  local m = 1 + par[7]
  local wx, wy, wz = par[4], par[5], par[6]
  return function (t)    -- t = {X = x, Y = y, Z = z}
    return {
      X = m * ( t.X + wz * t.Y - wy * t.Z) + par[1],
      Y = m * (-wz * t.X + t.Y + wx * t.Z) + par[2],
      Z = m * ( wy * t.X - wx * t.Y + t.Z) + par[3]
    }
  end
end

--- Transform coordinates between two Cartesian systems, backward direction.
--  @param par List of translations, rotations and scale {dX, dY, dZ; wx, wy, wz; m}.
--  @return Function for coordinate transformation.
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

-- Datum conversation (Molodensky method)

-- Forward direction
ellipsoid._fwdBLH = function (A,B,par)
  local dX, dY, dZ = par[1], par[2], par[3]
  local da, df = B.a - A.a, B.f - A.f
  return function (t)
    local B, L, H = math.deg(t.B), math.deg(t.L), t.H
    local sB, cB = math.sin(B), math.cos(B)
    local sL, cL = math.sin(L), math.cos(L)
    local tmp = 1 - A.e2*sB*sB
    local M = A.a*(1-A.e2)/tmp^1.5
    local N = A.a/math.sqrt(tmp)
    local dB = (-dX*sB*cL - dY*sB*sL + dZ*cB+N*A.e2*sB*cB*da/A.a + sB*cB*(M/(1-f)+N*(1-f))*df) / (M + H)
    local dL = (-dX*sL + dY*cL) / ((N + H)*cB)
    local dH = dX*cB*cL + dY*cB*sL + dZ*sL - A.a*da/N + N*(1-f)*sL*sL*df
    return {
      B = math.deg(B + dB),
      L = math.deg(L + dL),
      H = H + dH
    } 
  end
end


-- Collection of Geodetic methods
local geodesy = {
-- mark
type = 'geodesy', isgeodesy = true,
-- description
about = _about_,

-- Ellipsoids
WGS84 = ellipsoid:new {a = 6378137, f = 1/298.257223563},
-- russian systems
PZ90 = ellipsoid:new {a = 6378136, f = 1/298.25784},
PZ9002 = ellipsoid:new {a = 6378136, f = 1/298.25784},
SK42 = ellipsoid:new {a = 6378245, f = 1/298.3}
}
-- methametods
geodesy.__index = geodesy

-- WGS84 and PZ90   dx     dy   dz  wx  wy             wz           m
local pz90wgs84 = {-1.1, -0.3, -0.9; 0, 0, math.rad(-0.2/3600); -0.12E-6 }
-- xyz
geodesy.PZ90.xyzInto[geodesy.WGS84] = ellipsoid._fwdXYZ(pz90wgs84)
geodesy.WGS84.xyzInto[geodesy.PZ90] = ellipsoid._bwdXYZ(pz90wgs84)

-- PZ90 and PZ9002
local pz9002pz90 = {1.07, 0.03, -0.02; 0, 0, math.rad(0.13/3600); 0.22E-6}
-- xyz
geodesy.PZ9002.xyzInto[geodesy.PZ90] = ellipsoid._fwdXYZ(pz9002pz90)
geodesy.PZ90.xyzInto[geodesy.PZ9002] = ellipsoid._bwdXYZ(pz9002pz90)

-- PZ9002 and WGS84
local pz9002wgs84 = {-0.36, 0.08, 0.18; 0, 0, 0; 0}
-- xyz
geodesy.PZ9002.xyzInto[geodesy.WGS84] = ellipsoid._fwdXYZ(pz9002wgs84)
geodesy.WGS84.xyzInto[geodesy.PZ9002] = ellipsoid._bwdXYZ(pz9002wgs84)

-- SK42 and PZ90
local sk42pz90 = {25, -141, -80; 0, math.rad(-0.35/3600), math.rad(-0.66/3600); 0}
-- xyz
geodesy.SK42.xyzInto[geodesy.PZ90] = ellipsoid._fwdXYZ(sk42pz90)
geodesy.PZ90.xyzInto[geodesy.SK42] = ellipsoid._bwdXYZ(sk42pz90)

-- SK42 and PZ9002 
local sk42pz9002 = {23.93, -141.03, -79.98; 0, math.rad(-0.35/3600), math.rad(-0.79/3600); -0.22E-6} 
geodesy.SK42.xyzInto[geodesy.PZ9002] = ellipsoid._fwdXYZ(sk42pz9002)
geodesy.PZ9002.xyzInto[geodesy.SK42] = ellipsoid._bwdXYZ(sk42pz9002)

--- Convert degrees to radians.
--  @param d Degrees.
--  @param m Minutes (optional).
--  @param s Seconds (optional).
--  @return Angle in radians.
geodesy.dms2rad = function (d,m,s) return math.rad(d + (m or 0) / 60 + (s or 0) / 3600) end
geodesy.about[geodesy.dms2rad] = {"dms2rad(d[,m[,s]])", "Convert degrees, minutes and seconds to radians", help.OTHER}

--- Convert degrees to degrees-minutes-seconds.
--  @param d Angle in degrees.
--  @return Degrees, minutes, seconds.
geodesy.deg2dms = function (d)
  local deg = math.floor(d)
  local min = math.floor(60 * (d - deg))
  local sec = 3600 * (d - deg) - 60 * min
  return deg, min, sec
end
geodesy.about[geodesy.deg2dms] = {"deg2dms(d)", "Return degrees, minutes and seconds for the given angle degrees."}

-- Uncomment to remove descriptions
--geodesy.about = nil

return geodesy

--======================================
