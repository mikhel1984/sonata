--[[		sonata/lib/geodesy.lua

--- Coordinate transformations and other geodetic tasks.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2025.

	module 'geodesy'
--]]


--[[TEST_IT

-- use 'geodesy'
Geo = require 'matlib.geodesy'

-- generate random from number -1 to 1
rnd = function () return 2*math.random()-1 end
-- random coordinates (degrees and meters)
t0 = {B=rnd()*90, L=rnd()*180, H=rnd()*1000}
-- latitude, longitude, height
print(t0.B, t0.L, t0.H)

-- default is WGS84
wgs84 = Geo()
-- BLH to XYZ
t1 = wgs84:toXYZ(t0)
print(t1.X, t1.Y, t1.Z)

-- XYZ to BHL
t2 = wgs84:toBLH(t1)
print(t2.B, t2.L, t2.H)
ans = t2.B                  --.3>  t0.B

ans = t2.L                  --.3>  t0.L

ans = t2.H                  --.3>  t0.H

-- add another ellipsoid
-- use russian PZ90
pz90 = Geo { a = 6378136, f = 1/298.25784,
  -- additional parameters (optional)
  GMe = 398600.4418E9,  -- m^3/s^2
  omega = 7.292115E-5,  -- rad/s, rotation rate
  J2 = 1082.62575E-6,   -- dynamic form factor
}
-- define transformation
pz90:into( wgs84,
  {-1.1, -0.3, -0.9},          -- linear
  {0, 0, math.rad(-0.2/3600)}, -- angular
  -0.12E-6)                    -- scale
-- transform XYZ from WGS84 to PZ90
print(pz90)  -- show

-- transform
xyz_wgs84_pz90 = wgs84.xyzInto[pz90]
t3 = xyz_wgs84_pz90(t1)

-- backward transformation
xyz_pz90_wgs84 = pz90.xyzInto[wgs84]
t4 = xyz_pz90_wgs84(t3)
ans = t4.X                  --.2>  t1.X

-- datum transformation
blh_wgs84_pz90 = wgs84.blhInto[pz90]
t5 = blh_wgs84_pz90(t0)

-- UTM to lat/lon
utm = {N=5601281, E=625394, zone=42, hs='N'}
ll = wgs84:utm2bl(utm)
ans = ll.B                  --.2>  50.55

ans = ll.L                  --.2>  70.77

-- lat/lon to UTM
utm1 = wgs84:bl2utm(ll)
ans = utm1.N                --.2>  utm.N

ans = utm1.E                --.2>  utm.E

ans = utm1.zone               -->  utm.zone

-- inverse problem
p1 = {B=rnd()*50, L=rnd()*50}
p2 = {B=rnd()*50, L=rnd()*50}
s, a1, a2 = wgs84:solveInv(p1,p2)
ans = (s >= 0)                -->  true

-- direct problem
p3, a3 = wgs84:solveDir(p1,a1,s)
ans = a3                    --.2>  a2

ans = p3.B                  --.2>  p2.B

-- equator acceleration
ans = Geo:grav(0)           --.1>  9.78

-- find geohash
h = Geo:hashEncode(p1, 7)
print(h)

-- position from geohash
p4, _ = Geo:hashDecode(h)
ans = p4.B                  --.2>  p1.B

--]]


--	LOCAL

local Vatan2, Calc do
  local lib = require("matlib.utils")
  Vatan2 = lib.versions.atan2
  Calc = lib.calc
end

-- categories
local PROB, TRANS, PROJ = "problems", "transform", "projection"


--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Coordinate transformations and other geodetic tasks."
}


--	MODULE

-- Reference ellipsoid
local ellipsoid = {}


--- Datum transformation from E2 to E1.
--  @param E1 Dst ellipsoid object.
--  @param E2 Src ellipsoid object.
--  @param par Transformation parameters.
--  @return Function for transformation.
local function _bwdBLH (E1, E2, lin)
  local da, df = E1.a - E2.a, E1.f - E2.f
  return function (t)
    local dB, dL, dH = ellipsoid._molodensky(E2, -lin[1], -lin[2], -lin[3], da, df, t)
    return {
      B = t.B + dB,
      L = t.L + dL,
      H = t.H + dH }
  end
end


--- Transform coordinates between two Cartesian systems, backward direction.
--  @param par List of translations, rotations and scale {dX, dY, dZ; wx, wy, wz; m}.
--  @return Function for coordinate transformation.
local function _bwdXYZ (lin, rot, m)
  m = 1 - m
  local wx, wy, wz = rot[1], rot[2], rot[3]
  return function (t)
    return {
      X = m * ( t.X - wz * t.Y + wy * t.Z) - lin[1],
      Y = m * ( wz * t.X + t.Y - wx * t.Z) - lin[2],
      Z = m * (-wy * t.X + wx * t.Y + t.Z) - lin[3] }
  end
end


--- Datum transformation from E1 to E2.
--  @param E1 Src ellipsoid object.
--  @param E2 Dst ellipsoid object.
--  @param par Transformation parameters.
--  @return Function for transformation.
local function _fwdBLH (E1, E2, lin)
  local da, df = E2.a - E1.a, E2.f - E1.f
  return function (t)
    local dB, dL, dH = ellipsoid._molodensky(
      E1, lin[1], lin[2], lin[3], da, df, t)
    return {
      B = t.B + dB,
      L = t.L + dL,
      H = t.H + dH }
  end
end


--- Transform coordinates between two Cartesian systems, forward direction.
--  @param par List of translations, rotations and scale {dX, dY, dZ; wx, wy, wz; m}.
--  @return Function for coordinate transformation.
local function _fwdXYZ (lin, rot, m)
  m = 1 + m
  local wx, wy, wz = rot[1], rot[2], rot[3]
  return function (t)    -- t = {X = x, Y = y, Z = z}
    return {
      X = m * ( t.X + wz * t.Y - wy * t.Z) + lin[1],
      Y = m * (-wz * t.X + t.Y + wx * t.Z) + lin[2],
      Z = m * ( wy * t.X - wx * t.Y + t.Z) + lin[3] }
  end
end


--- Find the meridian radius
--  @param E Ellipsoid object.
--  @param d Latitude angle.
--  @return Radius value.
local function _radiusMeridian (E, d)
  local s = math.sin(d)
  return E.a * (1 - E.e2) / (1 - E.e2*s*s)^1.5
end


--- Find the vertical radius
--  @param E Ellipsoid object.
--  @paramd d Latitude angle.
--  @return Radius value.
local function _radiusVertical (E, d)
  local s = math.sin(d)
  return E.a / math.sqrt(1 - E.e2*s*s)
end


--- Find coefficients.
--  @param e2 Square excentrisitet.
--  @param cosa2 Square cosine of the angle.
--  @return A and B values.
local function _vincentyAB (e2, cosa2)
  local u2 = e2 * cosa2
  local A = 1 + u2*(4096 + u2*(-768 + u2*(320 - 175*u2)))/16384.0
  local B = u2*(256 + u2*(-128 + u2*(74 - 47*u2)))/1024.0
  return A, B
end


-- methametods
ellipsoid.__index = ellipsoid


--- Object view.
--  @return string representation.
ellipsoid.__tostring = function (self)
  return string.format("Ellipsoid {a=%d, f=%f}", self.a, self.f)
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
ellipsoid._molodensky = function (E, dx, dy, dz, da, df, t)
  local B, L = math.rad(t.B), math.rad(t.L)
  local sB, cB = math.sin(B), math.cos(B)
  local sL, cL = math.sin(L), math.cos(L)
  local f1 = 1 - E.f
  local M, N = _radiusMeridian(E, B), _radiusVertical(E, B)
  local dB = (-dx*sB*cL - dy*sB*sL + dz*cB + N*E.e2*sB*cB*da/E.a
               + sB*cB*(M/f1 + N*f1)*df) / (M + t.H)
  local dL = (-dx*sL + dy*cL) / ((N + t.H)*cB)
  local dH = dx*cB*cL + dy*cB*sL + dz*sL - E.a*da/N + N*f1*sB*sB*df
  return math.deg(dB), math.deg(dL), dH
end



--- Prepare coefficients for UTM transformations.
--  @param E Ellipsoid object.
ellipsoid._setUtmArrays = function (E)
  local n1 = E.f / (2 - E.f)
  local n = {n1, n1*n1, n1^3, n1^4, n1^5, n1^6}
  -- LL to UTM
  E.utmAlpha = {
    0.5*n[1] - 2.0/3*n[2] + 5.0/16*n[3] + 4.1/18*n[4] - 127.0/288*n[5] + 78.91/378*n[6],
    13.0/48*n[2] - 3.0/5*n[3] + 55.7/144*n[4] + 28.1/63*n[5] - 198343.3/193536*n[6],
    6.1/24*n[3] - 10.3/14*n[4] + 1506.1/2688*n[5] + 16760.3/18144*n[6],
    4956.1/16128*n[4] - 179.0/168*n[5] + 66016.61/72576*n[6],
    3472.9/8064*n[5] - 341888.9/199584*n[6],
    2123789.41/3193344*n[6]}
  -- UTM to LL
  E.utmBeta = {
    0.5*n[1] - 2.0/3*n[2] + 37.0/96*n[3] - 0.1/36*n[4] - 81.0/512*n[5] + 961.99/6048*n[6],
    1.0/48*n[2] + 1.0/15*n[3] - 43.7/144*n[4] + 46.0/105*n[5] - 111871.1/387072*n[6],
    1.7/48*n[3] - 3.7/84*n[4] - 20.9/448*n[5] + 556.9/9072*n[6],
    439.7/16128*n[4] - 11.0/504*n[5] - 8302.51/72576*n[6],
    458.3/16128*n[5] - 10884.7/399168*n[6],
    206486.93/6386688*n[6]}
  -- amplitude
  E.utmA = 0.9996 * E.a/(1+n[1]) * (1 + n[2]/4.0 + n[4]/64.0 + n[6]/256.0)
end


--- Define transformation.
--  @param E1 First ellipsoid.
--  @param E2 Second ellipsoid.
--  @param lin Linear transformation.
--  @param rot Rotational transformation.
--  @param m Scaling parameter.
ellipsoid.into = function (E1, E2, lin, rot, m)
  -- cartesian
  E1.xyzInto[E2] = _fwdXYZ(lin, rot, m)
  E2.xyzInto[E1] = _bwdXYZ(lin, rot, m)
  -- datum
  E1.blhInto[E2] = _fwdBLH(E1, E2, lin)
  E2.blhInto[E1] = _bwdBLH(E1, E2, lin)
end


--- Convert lat/lon to UTM coordinates.
--  Based on Karney's method and
--  http://www.movable-type.co.uk/scripts/latlong-utm-mgrs.html
--  @param t Table with longitude L and lattitude B
--  @return Table {N=,E=,hs=,zone=} and scale value.
ellipsoid.bl2utm = function (self, t)
  if not (-80 <= t.B and t.B <= 84) then
    error('Latitude outside UTM limits')
  end
  local zone = math.floor((t.L + 180)/6.0) + 1
  local l0 = (zone-1)*6 - 177  -- lon of central meridian
  -- Norway / Svalbard exception
  if 31 <= zone and zone <= 36 then
    local pos = math.floor(t.B/8.0 + 10)
    if pos == 17 then  -- 'V'
      if zone == 31 and t.L >= 3  then zone, l0 = zone + 1, l0 + 6 end
    elseif pos > 18 then  -- 'X'
      if zone == 32 and t.L < 9   then zone, l0 = zone - 1, l0 - 6 end
      if zone == 32 and t.L >= 9  then zone, l0 = zone + 1, l0 + 6 end
      if zone == 34 and t.L < 21  then zone, l0 = zone - 1, l0 - 6 end
      if zone == 34 and t.L >= 21 then zone, l0 = zone + 1, l0 + 6 end
      if zone == 36 and t.L < 33  then zone, l0 = zone - 1, l0 - 6 end
      if zone == 36 and t.L >= 33 then zone, l0 = zone + 1, l0 + 6 end
    end
  end
  local lat, lon = math.rad(t.B), math.rad(t.L - l0)
  -- find projection
  if not self.utmA then self:_setUtmArrays() end
  local alpha, e = self.utmAlpha, math.sqrt(self.e2)
  local clon = math.cos(lon)
  local tau, slat = math.tan(lat), math.sin(lat)
  local n1 = Calc.sinh( e*Calc.atanh( e*tau/math.sqrt(1+tau*tau)) )
  local tau1 = tau*math.sqrt(1+n1*n1) - n1*math.sqrt(1+tau*tau)
  local xi1 = Vatan2(tau1, clon)
  local eta1 = Calc.asinh(math.sin(lon) / math.sqrt(tau1*tau1 + clon*clon))
  local xi, eta, p1, q1 = xi1, eta1, 1, 0
  for i = 1, 6 do
    local sxi, cxi = math.sin(2*i*xi1), math.cos(2*i*xi1)
    local seta, ceta = Calc.sinh(2*i*eta1), Calc.cosh(2*i*eta1)
    xi  =  xi + alpha[i] * sxi * ceta
    eta = eta + alpha[i] * cxi * seta
    p1  =  p1 + 2*i*alpha[i] * cxi * ceta
    q1  =  q1 + 2*i*alpha[i] * sxi * seta
  end
  -- use UTM scale for the central meridian 0.9996
  local pose = {
    E = self.utmA * eta + 500e3,  -- add false easting
    N = self.utmA * xi,
    zone = zone,
    hs = t.B >= 0 and 'N' or 'S',  -- hemisphere
    H = t.H or 0,
  }
  if pose.N < 0 then pose.N = pose.N + 10000e3 end  -- add false northing
  local scale = self.utmA / self.a * math.sqrt(
    (p1*p1 + q1*q1)*(1-e*e*slat*slat)*(1+tau*tau) / (tau1*tau1 + clon*clon))
  return pose, scale
end


--- Ellipsoid object constructor.
--  @param t Table with parameters, obligatory are semi-major axis, flattening.
--  @return New ellipsoid.
ellipsoid.new = function (self, t)
  local f = t.f
  t.e2 = f*(2-f)
  t.b = t.a*(1-f)
  t.xyzInto = {}  -- cartesian coordinates transformation
  t.blhInto = {}  -- datum transformation
  return setmetatable(t, self)
end


--- Solve the direct geodetic problem:
--  find the socond point and azimuth if the initial point,
--  its azimuth and distance are given.
--  Uses Vincenty's formulae.
--  @param t1 First point (B,L).
--  @param dA Azimuth in the first point.
--  @param dist Distance to the second point.
--  @return Second point position (B,L) and orientation (deg).
ellipsoid.solveDir = function (self, t1, dA, dist)
  -- transform arguments
  local U1 = math.atan((1-self.f)*math.tan(math.rad(t1.B)))
  local sU1, cU1 = math.sin(U1), math.cos(U1)
  local ca1, sa1 = math.cos(math.rad(dA)), math.sin(math.rad(dA))
  -- prepare iterations
  local sig1 = Vatan2(math.tan(U1), ca1) * 2  -- multiplied sigma1
  local alsin = cU1*sa1
  local alcos2 = 1 - alsin*alsin
  local A, B = _vincentyAB(self.e2, alcos2)
  A = dist / (self.b * A)  -- reuse
  local sig, ssig, csigm = A, 0, 0
  -- iterative part
  repeat
    csigm = math.cos(sig1 + sig)
    ssig = math.sin(sig)
    local dsig = B*ssig*(csigm + B/4*(math.cos(sig)*(-1 + 2*csigm*csigm)
      -B/6*csigm*(-3 + 4*ssig*ssig)*(-3 + 4*csigm*csigm)))
    local prev = sig
    sig = A + dsig
  until math.abs(prev - sig) < 1E-9
  -- result
  local csig = math.cos(sig)
  local B2 = Vatan2(
    sU1*csig + cU1*ssig*ca1,
    (1-self.f)*math.sqrt(1 - alcos2 + (sU1*ssig - cU1*csig*ca1)^2))
  local C = self.f / 16 * alcos2 * (4 + self.f*(4 - 3*alcos2))
  local L = Vatan2(ssig*sa1, cU1*csig-sU1*ssig*sa1)
    -(1-C)*self.f*alsin*(sig + C*ssig*(csigm + C*csig*(-1 + 2*csigm*csigm))) -- lambda-...
  local azimuth2 = Vatan2(alsin, -sU1*ssig + cU1*csig*ca1)
  return {B = math.deg(B2), L = t1.L + math.deg(L)}, math.deg(azimuth2)
end


--- Solve the inverse geodetic problem:
--  find distance and azimuths for the two given points.
--  Uses Vincenty's formulae.
--  @param t1 First point (B,L).
--  @param t2 Second point (B,L).
--  @return distance (m), first and second azimuths (deg)
ellipsoid.solveInv = function (self, t1, t2)
  -- reduced latitude
  local U1 = math.atan((1-self.f)*math.tan(math.rad(t1.B)))
  local U2 = math.atan((1-self.f)*math.tan(math.rad(t2.B)))
  local cU1, cU2 = math.cos(U1), math.cos(U2)
  local sU1, sU2 = math.sin(U1), math.sin(U2)
  -- prepare variables
  local L = math.rad(t2.L) - math.rad(t1.L)
  local lam, ssig, csig, sig, alcos2, csigm = L, 0, 0, 0, 0, 0
  -- iterative part
  repeat
    ssig = math.sqrt((cU2*math.sin(lam))^2 + (cU1*sU2-sU1*cU2*math.cos(lam))^2)
    csig = sU1*sU2 + cU1*cU2*math.cos(lam)
    sig = Vatan2(ssig, csig)
    local alsin = cU1*cU2*math.sin(lam) / ssig
    alcos2 = 1 - alsin*alsin
    csigm = csig - 2*sU1*sU2 / alcos2
    local C = self.f / 16 * alcos2 * (4 + self.f*(4 - 3*alcos2))
    local prev = lam
    lam = L + (1-C)*self.f*alsin*
      (sig + C*ssig*(csigm + C*csig*(-1 + 2*csigm*csigm)))
  until math.abs(lam - prev) < 1E-12
  -- result
  local A, B = _vincentyAB(self.e2, alcos2)
  local dsig = B*ssig*(csigm + B/4*(csig*(-1 + 2*csigm*csigm)
    -B/6*csigm*(-3 + 4*ssig*ssig)*(-3 + 4*csigm*csigm)))
  local dist = self.b * A * (sig - dsig)
  local azimuth1 = Vatan2(
    cU2*math.sin(lam), cU1*sU2 - sU1*cU2*math.cos(lam))
  local azimuth2 = Vatan2(
    cU1*math.sin(lam), -sU1*cU2 + cU1*sU2*math.cos(lam))
  return dist, math.deg(azimuth1), math.deg(azimuth2)
end


--- Transform Cartesian coordinates to Geodetic.
--  @param t Dictionary with coordinates in meters: X, Y, Z.
--  @return Dictionary with coordinates: B (deg), L (deg), H (m).
ellipsoid.toBLH = function (self, t)
  local X, Y, Z = t.X, t.Y, t.Z
  local D = math.sqrt(X*X + Y*Y)
  local B, L, H = 0, 0, 0 -- result in rad
  if D < 1E-10 then
    B = (Z == 0 and 0 or (Z > 0 and 0.5 or -0.5)) * math.pi
    L = 0
    H = math.abs(Z) - self.b
  else
    -- solve Browning's iterative equation
    local e22 = self.e2 / (1 - self.e2)
    -- initial value for tan(B)
    local tg = Z / D * (1 + e22 * self.b / math.sqrt(X*X + Y*Y + Z*Z))
    for i = 1, 2 do       -- TODO: use tol estimation
      tg = tg * (1 - self.f)    -- get tan(theta)
      local theta = math.atan(tg)
      tg = (Z + e22 * self.b * math.sin(theta)^3) /
        (D - self.e2 * self.a * math.cos(theta)^3)
    end
    B = math.atan(tg)
    L = Vatan2(Y, X)
    local N = _radiusVertical(self, B)
    if math.abs(tg) > 1 then
      H = Z / math.sin(B) - (1 - self.e2) * N
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
--  @param t Dictionary with coordinates: B (deg), L (deg), H (m).
--  @return Dictionary with coordinates in meters: X, Y, Z.
ellipsoid.toXYZ = function (self, t)
  -- convert to radians
  local B, L, H = math.rad(t.B), math.rad(t.L), t.H
  local N = _radiusVertical(self, B)
  local NH = (N + H) * math.cos(B)
  return {
    X = NH * math.cos(L),
    Y = NH * math.sin(L),
    Z = ((1-self.e2)*N + H) * math.sin(B) }
end


--- Convert UTM coordinates to lat/lon.
--  Based on Karney's method and
--  http://www.movable-type.co.uk/scripts/latlong-utm-mgrs.html
--  @param t Table of coordinates {N=,E=,hs=,zone=}.
--  @return Table with longitude/lattitude and scale value.
ellipsoid.utm2bl = function (self, t)
  -- substract false easting and northing
  local x, y = t.E - 500e3, t.hs == 'S' and t.N - 10000e3 or t.N
  if not self.utmA then self:_setUtmArrays() end
  local beta, e = self.utmBeta, math.sqrt(self.e2)
  local eta, xi = x / self.utmA, y / self.utmA
  local xi1, eta1, p, q = xi, eta, 1, 0
  for j = 1, 6 do
    local sxi, cxi = math.sin(2*j*xi), math.cos(2*j*xi)
    local seta, ceta = Calc.sinh(2*j*eta), Calc.cosh(2*j*eta)
    xi1  = xi1 - beta[j] * sxi * ceta
    eta1 = eta1 - beta[j] * cxi * seta
    p    = p - 2*j*beta[j] * cxi * ceta
    q    = q - 2*j*beta[j] * sxi * seta
  end
  local sheta1 = Calc.sinh(eta1)
  local cxi1 =  math.cos(xi1)
  local tau1 = math.sin(xi1) / math.sqrt(sheta1*sheta1 + cxi1*cxi1)
  local taui = tau1
  -- find latitude
  repeat
    local sqrti = math.sqrt(1 + taui*taui)
    local si = Calc.sinh( e*Calc.atanh(e*taui/sqrti) )
    local taui1 = taui*math.sqrt(1+si*si) - si*sqrti
    local diff = (tau1 - taui1) / math.sqrt(1+taui1*taui1) *
      (1 + (1-e*e)*taui*taui) / ((1-e*e)*sqrti)
    taui = taui + diff
  until math.abs(diff) < 1E-12
  -- convert result
  local lat = math.atan(taui)
  local slat = math.sin(lat)
  local scale = self.utmA / self.a * math.sqrt(
    (p*p + q*q)*(1-e*e*slat*slat)*(1+taui*taui) *(sheta1*sheta1 + cxi1*cxi1))
  local l0 = (t.zone-1)*6 - 177
  local res = {
    B = math.deg(lat),
    L = math.deg(Vatan2(sheta1, cxi1)) + l0,
    H = t.H or 0,
  }
  return res, scale
end


-- Collection of Geodetic methods
local geodesy = {
-- mark
type = 'geodesy',

-- for geohash
base32 = '0123456789bcdefghjkmnpqrstuvwxyz',
}


-- methametods
geodesy.__index = geodesy


--- Convert to degrees.
--  @param d Degrees.
--  @param m Minutes (optional).
--  @param s Seconds (optional).
--  @return Angle in radians.
geodesy.dms2deg = function (_, d, m, s)
  return d + (m or 0) / 60 + (s or 0) / 3600
end
about[geodesy.dms2deg] = {":dms2deg(deg_d, min_d=0, sec_d=0) --> deg",
  "Convert degrees, minutes and seconds to degrees.", help.OTHER}


--- Convert degrees to degrees-minutes-seconds.
--  @param d Angle in degrees.
--  @return Degrees, minutes, seconds.
geodesy.deg2dms = function (_, d)
  local deg = math.floor(d)
  local min = math.floor(60 * (d - deg))
  local sec = 3600 * (d - deg) - 60 * min
  return deg, min, sec
end
about[geodesy.deg2dms] = {":deg2dms(deg_d) --> deg, min, sec",
  "Return degrees, minutes and seconds for the given angle value.", help.OTHER}


--- International gravity formula (WGS).
--  @param B Latitude, deg.
--  @return Acceleration value.
geodesy.grav = function (_, dB)
  local s = math.sin(math.rad(dB))
  s = s * s  -- get square
  return 9.780327*(1 + s*(0.0052792 + 0.0000232*s))
end
about[geodesy.grav] = {":grav(latitude_d) --> acceleration",
  "International gravity formula, angle in degrees.", help.OTHER}



--- Convert hash to corrdinates.
--  @param sHash Geohash string.
--  @return Central point and range of the zone.
geodesy.hashDecode = function (_, sHash)
  sHash = string.lower(sHash)
  -- find bounds
  local evenBit = true
  local latMin, latMax = -90, 90
  local lonMin, lonMax = -180, 180
  for k = 1, #sHash do
    local idx, bt = -1, string.byte(sHash, k, k)
    for i = 1, #geodesy.base32 do
      if string.byte(geodesy.base32, i, i) == bt then
        idx = i-1
        break
      end
    end
    for n = 4, 0, -1 do
      local bitN, _ = math.modf(idx / 2^n)
      bitN = math.fmod(bitN, 2)
      if evenBit then
        -- longitude
        local lonMid = (lonMin + lonMax) * 0.5
        if bitN == 1 then lonMin = lonMid else lonMax = lonMid end
      else
        -- latitude
        local latMid = (latMin + latMax) * 0.5
        if bitN == 1 then latMin = latMid else latMax = latMid end
      end
      evenBit = not evenBit
    end
  end
  -- center, range
  return {B = (latMin+latMax)*0.5, L = (lonMin+lonMax)*0.5},
         {latMax - latMin, lonMax - lonMin}
end
about[geodesy.hashDecode] = {":hashDecode(hash_s) --> coord_t, range_t",
  "Find central point and range of the zone.", PROJ}


--- Geohash from coordinates
--  Based on https://www.movable-type.co.uk/scripts/geohash.html
--  @param t Coordinates (lat, lon).
--  @param N Number of letters (1-12, optional).
--  @return String with hash.
geodesy.hashEncode = function (_, t, N)
  N = N or 6
  local latMin, latMax = -90, 90
  local lonMin, lonMax = -180, 180
  local evenBit, idx, bit = true, 0, 0
  local hash = {}
  while #hash < N do
    idx = idx * 2
    if evenBit then
      -- E-W longitude
      local lonMid = (lonMin + lonMax) * 0.5
      if t.L >= lonMid then
        idx, lonMin = idx + 1, lonMid
      else
        lonMax = lonMid
      end
    else
      -- N-S latitude
      local latMid = (latMin + latMax) * 0.5
      if t.B >= latMid then
        idx, latMin = idx+1, latMid
      else
        latMax = latMid
      end
    end
    evenBit, bit = not evenBit, bit+1
    if bit == 5 then
      hash[#hash+1] = string.sub(geodesy.base32, idx+1, idx+1)
      bit, idx = 0, 0
    end
  end
  return table.concat(hash)
end
about[geodesy.hashEncode] = {":hashEncode(coord_t, letter_N=6) --> hash_s",
  "Find hash for the given point.", PROJ}


-- Access to the ellipsoid object methods.
geodesy.toXYZ = ellipsoid.toXYZ
about[geodesy.toXYZ] = {"E:toXYZ(blh_t) --> xyz_t",
  "Transform Geodetic coordinates to Cartesian.", TRANS}


geodesy.toBLH = ellipsoid.toBLH
about[geodesy.toBLH] = {"E:toBLH(xyz_t) --> blh_t",
  "Transform Cartesian coordinates to Geodetic.", TRANS}


geodesy.utm2bl = ellipsoid.utm2bl
about[geodesy.utm2bl] = {"E:utm2bl(utm_t) --> blh_t",
  "Find Geodetic coordinates for the given UTM pose and zone.", PROJ}


geodesy.bl2utm = ellipsoid.bl2utm
about[geodesy.bl2utm] = {"E:bl2utm(blh_t) --> utm_t",
  "Find UTM projection for the given coordinates.", PROJ}


geodesy.solveDir = ellipsoid.solveDir
about[geodesy.solveDir] = {"E:solveDir(blh_t, az1_d, dist_d) --> blh_t, az2_d",
  "Solve direct geodetic problem, find second point position and its orientation if the first point, azimuth and distance are given.",
  PROB}


geodesy.solveInv = ellipsoid.solveInv
about[geodesy.solveInv] = {"E:solveInv(blh1_t, blh2_t) --> dist_d, az1_d, az2_d",
  "Solve inverse geodetic problem, find distance and azimuths for two points.",
  PROB}


geodesy.into = ellipsoid.into
about[geodesy.into] = {"E:into(E2, lin, rot, m)",
 "Define transormation rules between ellipsoids.", TRANS}


geodesy.xyzInto = 'A.xyzInto[B]'
about[geodesy.xyzInto] = {"E.xyzInto[E2] --> fn",
  "Get function to transform coordinates from E to E2 system.", TRANS}
geodesy.blhInto = 'A.blhInto[B]'
about[geodesy.blhInto] = {"E.blhInto[E2] --> fn",
  "Get function to transform geodetic coordinates from E to E2 system using the Molodensky method.",
  TRANS}


setmetatable(geodesy, {
__call = function (_, t)
  if t then
    return ellipsoid:new(t)
  end
  -- WGS84 by default
  return ellipsoid:new {a = 6378137, f = 1/298.257223563,
    -- additional parameters
    Me = 5.98E24,          -- kg, mass of earth
    G  = 6.67E-11,         -- m^3/kg/s^2, gravitational constant
    GMe = 3.986004418E14,  -- m^3/s^2
    omega = 7.292115E-5,   -- rad/s, rotation rate
    J2 = 1.081874E-3,      -- dynamic form factor
  }
end })
about[geodesy] = {" (param_t=nil) --> E",
  "Produce ellipsoid with the given params {'a', 'f'}.", help.STATIC}


-- Comment to remove descriptions
geodesy.about = about

return geodesy

--======================================
--TODO: check correctness (Molodensky)
