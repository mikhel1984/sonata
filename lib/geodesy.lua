--[[		sonata/lib/geodesy.lua

--- Coordinate transformations and other geodetic tasks.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2022.

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

-- find topocentric coordinates
tg = {X=t1.X+10, Y=t1.Y+20, Z=t1.Z+30}
tc = Geo:toENU(t0, t1, tg)

-- back to cartesian
tg2 = Geo:fromENU(t0, t1, tc)
ans = tg2.X                --3> tg.X

-- transform XYZ from WGS84 to PZ90
pz90 = Geo.PZ90
-- get function
xyz_wgs84_pz90 = wgs84.xyzInto[pz90]
t3 = xyz_wgs84_pz90(t1) 

-- backward transformation
xyz_pz90_wgs84 = pz90.xyzInto[wgs84]
t4 = xyz_pz90_wgs84(t3)
ans = t4.X                 --2> t1.X

-- datum transformation
blh_wgs84_pz90 = wgs84.blhInto[pz90]
t5 = blh_wgs84_pz90(t0)

-- Gauss-Kruger projection
sk = Geo.SK42
pt = {B=55.752, L=37.618}
t6 = sk:projGK(pt)
ans = t6.N                  --2> 6181924.245

ans = t6.E                  --2> 7413223.481

-- Merkator projection
t7 = wgs84:projM(pt)
print(t7.N, t7.E)

-- inverse problem
p1 = {B=rnd()*50, L=rnd()*50}
p2 = {B=rnd()*50, L=rnd()*50}
s, a1, a2 = wgs84:solveInv(p1,p2)
ans = (s >= 0)               --> true

-- direct problem 
p3, a3 = wgs84:solveDir(p1,a1,s)
ans = a3                    --2> a2

ans = p3.B                  --2> p2.B

-- equator acceleration 
ans = Geo:grav(0)           --1> 9.78

--]]

--	LOCAL

-- Compatibility with previous versions
local Ver = require("lib.utils").versions

local PROB, TRANS, PROJ = "problems", "transform", "projection"

--- Check object type.
--  @param v Object.
--  @return True if the object is geodesy.
local function isgeodesy(v) return type(v)=='table' and v.isgeodesy end

--- Inverse hyperbolic tangent.
--  @param d Some number.
--  @return Areatangent value.
local function arth(d) return math.log((1+d)/(1-d)) / 2 end

--	INFO

local help = SonataHelp or {new=function () return {} end}
-- description
local about = help:new("Coordinate transformations and other geodetic tasks.")

--	MODULE

-- Reference ellipsoid
local ellipsoid = {}

-- methametods
ellipsoid.__index = ellipsoid

--- Datum transformation from E2 to E1.
--  @param E1 Dst ellipsoid object.
--  @param E2 Src ellipsoid object.
--  @param par Transformation parameters.
--  @return Function for transformation.
ellipsoid._bwdBLH = function (E1,E2,tPar)
  local da, df = E1.a - E2.a, E1.f - E2.f
  return function (t)
    local dB, dL, dH = ellipsoid._molodensky_(E2, -tPar[1], -tPar[2], -tPar[3], da, df, t)
    return {
      B = t.B + dB,
      L = t.L + dL,
      H = t.H + dH
    }
  end
end

--- Transform coordinates between two Cartesian systems, backward direction.
--  @param par List of translations, rotations and scale {dX, dY, dZ; wx, wy, wz; m}.
--  @return Function for coordinate transformation.
ellipsoid._bwdXYZ = function (tPar)
  local m = 1 - tPar[7]
  local wx, wy, wz = tPar[4], tPar[5], tPar[6]
  return function (t)
    return {
      X = m * ( t.X - wz * t.Y + wy * t.Z) - tPar[1],
      Y = m * ( wz * t.X + t.Y - wx * t.Z) - tPar[2],
      Z = m * (-wy * t.X + wx * t.Y + t.Z) - tPar[3]
    }
  end
end

--- Datum transformation from E1 to E2.
--  @param E1 Src ellipsoid object.
--  @param E2 Dst ellipsoid object.
--  @param par Transformation parameters.
--  @return Function for transformation.
ellipsoid._fwdBLH = function (E1,E2,tPar)
  local da, df = E2.a - E1.a, E2.f - E1.f
  return function (t)
    local dB, dL, dH = ellipsoid._molodensky_(E1, tPar[1], tPar[2], tPar[3], da, df, t)
    return {
      B = t.B + dB,
      L = t.L + dL,
      H = t.H + dH
    } 
  end
end

--- Transform coordinates between two Cartesian systems, forward direction.
--  @param par List of translations, rotations and scale {dX, dY, dZ; wx, wy, wz; m}.
--  @return Function for coordinate transformation.
ellipsoid._fwdXYZ = function (tPar)
  local m = 1 + tPar[7]
  local wx, wy, wz = tPar[4], tPar[5], tPar[6]
  return function (t)    -- t = {X = x, Y = y, Z = z}
    return {
      X = m * ( t.X + wz * t.Y - wy * t.Z) + tPar[1],
      Y = m * (-wz * t.X + t.Y + wx * t.Z) + tPar[2],
      Z = m * ( wy * t.X - wx * t.Y + t.Z) + tPar[3]
    }
  end
end

--- Molodensky transformation. Axis are assumed to be parallel to each other.
--  @param E Ellipsoid object.
--  @param dx Shift in X, m.
--  @param dy Shift in Y, m.
--  @param dz Shift in Z, m.
--  @param da Differenct in major axis.
--  @param df Difference in flattening.
--  @param t Coordinates of a point.
--  @return Shift in B, L, H coordinates.
ellipsoid._molodensky_ = function (E,dx,dy,dz,da,df,t)
  local B, L = math.rad(t.B), math.rad(t.L)
  local sB, cB = math.sin(B), math.cos(B)
  local sL, cL = math.sin(L), math.cos(L)
  local f1 = 1 - E.f
  local M, N = ellipsoid._radiusMeridian_(E, B), ellipsoid._radiusVertical_(E, B)
  local dB = (-dx*sB*cL - dy*sB*sL + dz*cB + N*E.e2*sB*cB*da/E.a + sB*cB*(M/f1 + N*f1)*df) / (M + t.H)
  local dL = (-dx*sL + dy*cL) / ((N + t.H)*cB)
  local dH = dx*cB*cL + dy*cB*sL + dz*sL - E.a*da/N + N*f1*sB*sB*df
  return math.deg(dB), math.deg(dL), dH
end

--- Find the meridian radius
--  @param E Ellipsoid object.
--  @param d 
--  @return Radius value.
ellipsoid._radiusMeridian_ = function (E, d)
  local s = math.sin(d)
  return E.a * (1 - E.e2) / (1 - E.e2*s*s)^1.5
end

--- Find the vertical radius
--  @param E Ellipsoid object.
--  @paramd d 
--  @return Radius value.
ellipsoid._radiusVertical_ = function (E, d)
  local s = math.sin(d)
  return E.a / math.sqrt(1 - E.e2*s*s)
end

--- Find coefficients.
--  @param e2 Square excentrisitet. 
--  @param cosa2 Square cosine of the angle.
--  @return A and B values. 
ellipsoid._vincentyAB_ = function (e2, cosa2)
  local u2 = e2 * cosa2 
  local A = 1 + u2*(4096 + u2*(-768 + u2*(320 - 175*u2)))/16384.0 
  local B = u2*(256 + u2*(-128 + u2*(74 - 47*u2)))/1024.0
  return A, B
end

--- Ellipsoid object constructor.
--  @param t Table with parameters, obligatory are semi-major axis, flattening.
--  @return New ellipsoid.
ellipsoid.new = function (self,t)
  local f = t.f
  t.e2 = f*(2-f)
  t.b = t.a*(1-f)
  t.xyzInto = {}  -- cartesian coordinates transformation
  t.blhInto = {}  -- datum transformation
  return setmetatable(t, self)
end

--- Find Gauss-Kruger (transverse Mercator) projection of the geodetic point.
--  @param E Ellipsoid object.
--  @param t Table with longitude L and lattitude B.
--  @return Table with North and East position (in meters).
ellipsoid.projGK = function (E, t)
  local zone, F = math.floor(t.L/6 + 1), 1.0  -- index and scale
  local L0, B0 = math.rad(zone*6 - 3), 0  -- begining
  local N0, E0 = 0, zone*1E6 + 500000.0   -- shifting
  local B, L, n = math.rad(t.B), math.rad(t.L), (E.a - E.b)/(E.a + E.b)
  local M = F * E.b * ( (1+n*(1+5.0/4*n*(1+n)))*(B-B0)
    - 3*n*(1 + n*(1 + 7.0/8*n)) * math.sin(B-B0) * math.cos(B+B0)
    + 15.0/8.0*n*n*(1+n) * math.sin(2*(B-B0)) * math.cos(2*(B+B0))
    - 35.0/24.0*n^3 * math.sin(3*(B-B0)) * math.cos(3*(B+B0)))
  local sB, cB, tB2 = math.sin(B), math.cos(B), math.tan(B)^2
  local v, p = F*ellipsoid._radiusVertical_(E,B), F*ellipsoid._radiusMeridian_(E,B)
  local nn = v / p - 1
  local coef = {cB, 
    sB*cB/2.0, 
    cB^3 * (v/p - tB2) / 6.0,
    sB * cB^3 * (5 - tB2 + 9*nn) / 24.0, 
    cB^5 * (5 + tB2 * (tB2 - 18 - 58*nn) + 14*nn) / 120.0,
    sB * cB^5 * (61 + tB2 * (tB2 - 58)) / 720.0
  }
  L = L-L0; B = L * L  -- reuse
  return {
    N = N0 + M + v*B*(coef[2] + B*(coef[4] + B*coef[6])),
    E = E0 + v*L*(coef[1] + B*(coef[3] + B*coef[5]))
  }
end

--- Find Mercator projection of the geodetic point.
--  @param E Ellipsoid object.
--  @param t Table with longitude L and lattitude B.
--  @param iL Initial meridian (0 by default).
--  @return Table with North and East position (in meters).
ellipsoid.projM = function (E, t, iL)
  iL = iL or 0  -- initial meridian 
  local C = E.a -- use equator radius 
  local ex, sB = math.sqrt(E.e2), math.sin(math.rad(t.B))
  return {
    N = C * (arth(sB) - ex*arth(ex*sB)),
    E = C * math.rad(t.L - iL)
  }
end

--- Solve the direct geodetic problem:
--  find the socond point and azimuth if the initial point, 
--  its azimuth and distance are given. 
--  Uses Vincenty's formulae. 
--  @param E Ellipsoid object.
--  @param t1 First point (B,L).
--  @param dA Azimuth in the first point.
--  @param dist Distance to the second point.
--  @return Second point position (B,L) and orientation (deg).
ellipsoid.solveDir = function (E, t1, dA, dist)
  -- transform arguments
  local U1 = math.atan((1-E.f)*math.tan(math.rad(t1.B)))
  local sU1, cU1 = math.sin(U1), math.cos(U1)
  local ca1, sa1 = math.cos(math.rad(dA)), math.sin(math.rad(dA))
  -- prepare iterations
  local sig1 = Ver.atan2(math.tan(U1), ca1) * 2  -- multiplied sigma1
  local alsin = cU1*sa1 
  local alcos2 = 1 - alsin*alsin 
  local A, B = ellipsoid._vincentyAB_(E.e2, alcos2)
  A = dist / (E.b * A)  -- reuse
  local sig, ssig, csigm = A
  -- iterative part
  repeat
    csigm = math.cos(sig1 + sig)
    ssig = math.sin(sig)
    local dsig = B*ssig*(csigm + B/4*(math.cos(sig)*(-1 + 2*csigm*csigm) - B/6*csigm*(-3 + 4*ssig*ssig)*(-3 + 4*csigm*csigm)))
    local prev = sig
    sig = A + dsig
  until math.abs(prev - sig) < 1E-9
  -- result
  local csig = math.cos(sig)
  local B2 = Ver.atan2(sU1*csig + cU1*ssig*ca1, (1-E.f)*math.sqrt(1 - alcos2 + (sU1*ssig - cU1*csig*ca1)^2))
  local C = E.f / 16 * alcos2 * (4 + E.f*(4 - 3*alcos2))
  local L = Ver.atan2(ssig*sa1, cU1*csig-sU1*ssig*sa1) - (1-C)*E.f*alsin*(sig + C*ssig*(csigm + C*csig*(-1 + 2*csigm*csigm))) -- lambda-...
  local azimuth2 = Ver.atan2(alsin, -sU1*ssig + cU1*csig*ca1)
  return {B = math.deg(B2), L = t1.L + math.deg(L)}, math.deg(azimuth2)
end

--- Solve the inverse geodetic problem:
--  find distance and azimuths for the two given points.
--  Uses Vincenty's formulae.
--  @param E Ellipsoid object.
--  @param t1 First point (B,L).
--  @param t2 Second point (B,L).
--  @return distance (m), first and second azimuths (deg)
ellipsoid.solveInv = function (E, t1, t2)
  -- reduced latitude
  local U1 = math.atan((1-E.f)*math.tan(math.rad(t1.B)))
  local U2 = math.atan((1-E.f)*math.tan(math.rad(t2.B)))
  local cU1, cU2, sU1, sU2 = math.cos(U1), math.cos(U2), math.sin(U1), math.sin(U2)
  -- prepare variables
  local L = math.rad(t2.L) - math.rad(t1.L)
  local lam, ssig, csig, sig, alcos2, csigm = L
  -- iterative part
  repeat
    ssig = math.sqrt((cU2*math.sin(lam))^2 + (cU1*sU2-sU1*cU2*math.cos(lam))^2)
    csig = sU1*sU2 + cU1*cU2*math.cos(lam) 
    sig = Ver.atan2(ssig, csig)
    local alsin = cU1*cU2*math.sin(lam) / ssig 
    alcos2 = 1 - alsin*alsin
    csigm = csig - 2*sU1*sU2 / alcos2 
    local C = E.f / 16 * alcos2 * (4 + E.f*(4 - 3*alcos2)) 
    local prev = lam
    lam = L + (1-C)*E.f*alsin*(sig + C*ssig*(csigm + C*csig*(-1 + 2*csigm*csigm)))
  until math.abs(lam - prev) < 1E-12
  -- result
  local A, B = ellipsoid._vincentyAB_(E.e2, alcos2)
  local dsig = B*ssig*(csigm + B/4*(csig*(-1 + 2*csigm*csigm) - B/6*csigm*(-3 + 4*ssig*ssig)*(-3 + 4*csigm*csigm)))
  local dist = E.b * A * (sig - dsig) 
  local azimuth1 = Ver.atan2(cU2*math.sin(lam), cU1*sU2 - sU1*cU2*math.cos(lam))
  local azimuth2 = Ver.atan2(cU1*math.sin(lam), -sU1*cU2 + cU1*sU2*math.cos(lam))
  return dist, math.deg(azimuth1), math.deg(azimuth2)
end

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
    local N = ellipsoid._radiusVertical_(E, B)
    if math.abs(tg) > 1 then
      H = Z / math.sin(B) - (1 - E.e2) * N
    else
      H = D / math.cos(B) - N
    end
  end
  return {
    B = math.deg(B),
    L = math.deg(L),
    H = H }
end

--- Transform Geodetic coordinats to Cartesian.
--  @param E Reference ellipsoid object.
--  @param t Dictionary with coordinates: B (deg), L (deg), H (m).
--  @return Dictionary with coordinates in meters: X, Y, Z.
ellipsoid.toXYZ = function (E, t)
  -- convert to radians
  local B, L, H = math.rad(t.B), math.rad(t.L), t.H
  local N = ellipsoid._radiusVertical_(E, B)
  local NH = (N + H) * math.cos(B)
  return {
    X = NH * math.cos(L),
    Y = NH * math.sin(L),
    Z = ((1-E.e2)*N + H) * math.sin(B) }
end


-- Collection of Geodetic methods
local geodesy = {
-- mark
type = 'geodesy', isgeodesy = true,

-- Ellipsoids
WGS84 = ellipsoid:new {a = 6378137, f = 1/298.257223563, 
  -- additional parameters
  Me = 5.98E24,  -- kg, mass of earth
  G  = 6.67E-11, -- m^3/kg/s^2, gravitational constant
  GMe = 3.986004418E14, -- m^3/s^2
  omega = 7.292115E-5, -- rad/s, rotation rate
  J2 = 1.081874E-3, -- dynamic form factor
},
-- russian systems
PZ90 = ellipsoid:new {a = 6378136, f = 1/298.25784,
  -- additional parameters
  GMe = 398600.4418E9, -- m^3/s^2
  omega = 7.292115E-5, -- rad/s, rotation rate
  J2 = 1082.62575E-6, -- dynamic form factor
},
PZ9002 = ellipsoid:new {a = 6378136, f = 1/298.25784},
SK42 = ellipsoid:new {a = 6378245, f = 1/298.3}
}

-- methametods
geodesy.__index = geodesy

--- Convert degrees to radians.
--  @param self Do nothing.
--  @param d Degrees.
--  @param m Minutes (optional).
--  @param s Seconds (optional).
--  @return Angle in radians.
geodesy.dms2rad = function (self,d,m,s) return math.rad(d + (m or 0) / 60 + (s or 0) / 3600) end
about[geodesy.dms2rad] = {"Geo:dms2rad(d,[m=0,s=0])", "Convert degrees, minutes and seconds to radians.", help.OTHER}

--- Convert degrees to degrees-minutes-seconds.
--  @param self Do nothing.
--  @param d Angle in degrees.
--  @return Degrees, minutes, seconds.
geodesy.deg2dms = function (self,d)
  local deg = math.floor(d)
  local min = math.floor(60 * (d - deg))
  local sec = 3600 * (d - deg) - 60 * min
  return deg, min, sec
end
about[geodesy.deg2dms] = {"Geo:deg2dms(d)", "Return degrees, minutes and seconds for the given angle value.", help.OTHER}

--- International gravity formula (WGS).
--  @param self Do nothing.
--  @param B Latitude, deg.
--  @return Acceleration value.
geodesy.grav = function (self,dB)
  local s = math.sin(math.rad(dB))
  s = s * s  -- get square
  return 9.8703185*(1 + s*(0.00527889 + 0.000023462*s))
end
about[geodesy.grav] = {"Geo:grav(dB)", "International gravity formula, angle in degrees.", help.OTHER}


-- Access to the ellipsoid object methods.
geodesy.toXYZ = ellipsoid.toXYZ
about[geodesy.toXYZ] = {"toXYZ(tBLH)", "Transform Geodetic coordinates to Cartesian.", TRANS}

geodesy.toBLH = ellipsoid.toBLH
about[geodesy.toBLH] = {"toBLH(tXYZ)", "Transform Cartesian coordinates to Geodetic.", TRANS}

geodesy.projGK = ellipsoid.projGK
about[geodesy.projGK] = {"projGK(tBL)", "Return north and east positions of the point after Gauss-Kruger projection.", PROJ}

geodesy.projM = ellipsoid.projM
about[geodesy.projM] = {"projM(tBL)", "Return north and east positions of the point after Mercator projectoin.", PROJ}

geodesy.solveInv = ellipsoid.solveInv
about[geodesy.solveInv] = {"solveInv(BLH1,BLH2)", 
  "Solve inverse geodetic problem, find distance and azimuths for two points.", PROB}

-- ellipsoid.solveDir = function (E, t1, a1, dist)
geodesy.solveDir = ellipsoid.solveDir
about[geodesy.solveDir] = {"solveDir(BLH,azimuth,dist)", 
  "Solve direct geodetic problem, find second point position and its orientation if the first point, azimuth and distance are given.", PROB}


--- Simplify configuration of coordinate transformation between ellipsoids.
--  @param E1 First ellipsoid object.
--  @param E2 Second ellipsoid object.
--  @param par List of parameters {dX, dY, dZ; wx, wy, wz; m}
local _setTranslation_ = function (E1, E2, tPar)
  -- cartesian
  E1.xyzInto[E2] = ellipsoid._fwdXYZ(tPar)
  E2.xyzInto[E1] = ellipsoid._bwdXYZ(tPar)
  -- datum
  E1.blhInto[E2] = ellipsoid._fwdBLH(E1, E2, tPar)
  E2.blhInto[E1] = ellipsoid._bwdBLH(E1, E2, tPar)
end

-- PZ90 to WGS84
_setTranslation_(geodesy.PZ90, geodesy.WGS84, 
  {-1.1, -0.3, -0.9; 0, 0, math.rad(-0.2/3600); -0.12E-6})
-- PZ90 and PZ9002
_setTranslation_(geodesy.PZ9002, geodesy.PZ90, 
  {1.07, 0.03, -0.02; 0, 0, math.rad(0.13/3600); 0.22E-6})
-- PZ9002 and WGS84
_setTranslation_(geodesy.PZ9002, geodesy.WGS84, 
  {-0.36, 0.08, 0.18; 0, 0, 0; 0})
-- SK42 and PZ90
_setTranslation_(geodesy.SK42, geodesy.PZ90, 
  {25, -141, -80; 0, math.rad(-0.35/3600), math.rad(-0.66/3600); 0})
-- SK42 and PZ9002 
_setTranslation_(geodesy.SK42, geodesy.PZ9002, 
  {23.93, -141.03, -79.98; 0, math.rad(-0.35/3600), math.rad(-0.79/3600); -0.22E-6})

geodesy.xyzInto = 'A.xyzInto[B]'
about[geodesy.xyzInto] = {"A.xyzInto[B]", "Get function to transform coordinates from A to B system.", TRANS}
geodesy.blhInto = 'A.blhInto[B]'
about[geodesy.blhInto] = {"A.blhInto[B]", "Get function to transform geodetic coordinates from A to B system using the Molodensky method.", TRANS}

--- Find topocentric coordinates of a point.
--  @param self Do nothing.
--  @param g Geodetic coordinates of the reference point.
--  @param r Cartesian coordinates of the reference point.
--  @param p Cartesian coordinates of the observed point.
--  @return Topocentric coordinates of the observed point. 
geodesy.toENU = function (self,tG, tR, tP)
  local sB, cB = math.sin(math.rad(tG.B)), math.cos(math.rad(tG.B))
  local sL, cL = math.sin(math.rad(tG.L)), math.cos(math.rad(tG.L))
  local dx, dy, dz = tP.X-tR.X, tP.Y-tR.Y, tP.Z-tR.Z
  return {
    X = -sL*dx + cL*dy,
    Y = -sB*cL*dx - sB*sL*dy + cB*dz,
    Z = cB*cL*dx + cB*sL*dy + sB*dz
  }
end
about[geodesy.toENU] = {"Geo:toENU(tBLr,tXYZr,tCatr)", "Get topocentric coordinates of a point in reference frame.", TRANS}

--- Find cartesian coordinates of a point with topocentric coordinates.
--  @param self Do nothing.
--  @param g Geodetic coordinates of the reference point.
--  @param r Cartesian coordinates of the reference point.
--  @param l Topocentric coordinates of the observed point.
--  @return Cartesian coordinates of the observed point.
geodesy.fromENU = function (self,tG, tR, tL)
  local sB, cB = math.sin(math.rad(tG.B)), math.cos(math.rad(tG.B))
  local sL, cL = math.sin(math.rad(tG.L)), math.cos(math.rad(tG.L))
  return {
    X = tR.X - sL*tL.X - sB*cL*tL.Y + cB*cL*tL.Z,
    Y = tR.Y + cL*tL.X - sB*sL*tL.Y + cB*sL*tL.Z,
    Z = tR.Z + cB*tL.Y + sB*tL.Z
  }
end
about[geodesy.fromENU] = {"Geo:fromENU(tBLr,tXYZr,tTop)", "Get cartesian coordinates of a local point in reference frame.", TRANS}

-- Comment to remove descriptions
geodesy.about = about

return geodesy

--======================================
--TODO: check correctness (Merkator, Molodensky)
--TODO: newEllipsoid to define own ellipsoid object
--TODO: ellipsoid.__tostring
