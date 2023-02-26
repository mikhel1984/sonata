--[[		sonata/lib/quaternion.lua

--- Operations with unit quaternions.
--
--  Object structure: </br>
--  <code>{w, i, j, k} </code></br>
--  where <i>w</i> is a real part, <i>i, j, k</i> are imaginary elements.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2023.

	module 'quaternion'
--]]

--[[TEST

-- use 'quaternion'
Quat = require 'lib.quaternion'
-- external dependencies, can be loaded implicitly
require 'lib.matrix'

-- quaternion
-- set {w,x,y,z}
a = Quat {1,2,3,4}
-- part of elements
b = Quat {w=3, x=4}
ans = b                       --> Quat{3,4,0,0}

-- conjugation
ans = a:conj()                --> Quat{1,-2,-3,-4}

-- real when imaginary are zeros
ans = a + a:conj()            --> 2

-- norm
ans = b:abs()                --1> 5.000

-- inversion
c = a*a:inv()
ans = c:w()                  --1> 1.000

-- arithmetic
ans = a+b                     --> Quat{4,6,3,4}

ans = a*b                     --> Quat{-5,10,25}

ans = 3*b                     --> Quat{9,12,0,0}

-- power
ans = b^3                     --> b * b * b

-- unit quaternion
a = a:normalize()
ans = a:abs()                --1> 1.000

-- unit power
aa = a^1.5
ans = aa:x()                 --3> 0.324

ans = aa:y()                 --3> 0.486

ans = aa:z()                 --3> 0.648

-- rotation matrix
m = a:toRot()
d = Quat:fromRot(m)
ans = (d-a):abs()            --1> 0.000

-- use angle
-- and axis
ang = 0.5
axis = {1,1,1}
f = Quat:fromAA(ang,axis)
ans,_ = f:toAA()             --3> ang

-- rotate vector
p = a:rotate({1,0,0})
ans = p[1]                   --3> -0.667

-- spherical interpolation
d = a:slerp(b,0.5)
ans = d:w()                  --3> 0.467

-- show
print(d)

--]]

--	LOCAL

local Ver = require("lib.utils")
local Utils = Ver.utils
local Cross = Ver.cross
Ver = Ver.versions

local ROTATION = 'rotation'

--- Check object type.
--  @param t Object.
--  @return True if the object is quaternion.
local function isquaternion(v) return type(v)=='table' and v.isquaternion end

--- Get float point value if possible.
--  @param v Real value.
--  @return floating point number.
local function tofloat(v) return type(v) == 'table' and v:float() or v end

--- Simplify output when possible.
--  @param Q Quaternion object.
--  @return Quaternion or value.
local function numquat(Q)
  return Cross.eq(Q._[2], 0) and Cross.eq(Q._[3], 0) and Cross.eq(Q._[4], 0)
    and Cross.simp(Q._[1]) or Q
end

--- Number representation.
--  @param v Value.
--  @return String representation.
local function numStr(v)
  return type(v) == 'number' and Utils.numstr(v) or tostring(v)
end

--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Operations with quaternions."
}

--	MODULE

local quaternion = {
-- mark
type = 'quaternion', isquaternion = true,
-- simplify
_simp = numquat
}

--- Q1 + Q2
--  @param Q1 First quaternion.
--  @param Q2 Second quaternion.
--  @return Sum object.
quaternion.__add = function (Q1, Q2)
  if not (isquaternion(Q1) and isquaternion(Q2)) then
    local p = Cross.convert(Q1, Q2)
    if p then
      return Q1 + p
    else
      return Cross.convert(Q2, Q1) + Q2
    end
  end
  local q1, q2 = Q1._, Q2._
  return numquat(
    quaternion:_new({q1[1]+q2[1], q1[2]+q2[2], q1[3]+q2[3], q1[4]+q2[4]}))
end

-- metamethods
quaternion.__index = quaternion

-- Set by key
quaternion.__newindex = function () error("Immutable object") end

--- Q1 * Q2
--  @param Q1 First quaternion.
--  @param Q2 Second quaternion.
--  @return Product.
quaternion.__mul = function (Q1, Q2)
  if not (isquaternion(Q1) and isquaternion(Q2)) then
    local p = Cross.convert(Q1, Q2)
    if p then
      return Q1 * p
    else
      return Cross.convert(Q2, Q1) * Q2
    end
  end
  local q1, q2 = Q1._, Q2._
  return numquat(quaternion:_new(
    {q1[1]*q2[1]-q1[2]*q2[2]-q1[3]*q2[3]-q1[4]*q2[4],
     q1[1]*q2[2]+q1[2]*q2[1]+q1[3]*q2[4]-q1[4]*q2[3],
     q1[1]*q2[3]-q1[2]*q2[4]+q1[3]*q2[1]+q1[4]*q2[2],
     q1[1]*q2[4]+q1[2]*q2[3]-q1[3]*q2[2]+q1[4]*q2[1]}))
end

--- Q ^ k
--  @param Q Quaternion.
--  @param d Any number for unit quaternion, -1 or positive integer for others.
--  @return Power value.
quaternion.__pow = function (Q, d)
  if d == -1 then
    return quaternion.inv(Q)
  elseif d >= 0 and Ver.isInteger(d) then
    -- positive integer use for all quaternions
    local res, acc = quaternion:_new{1, 0, 0, 0}, Q
    while d > 0 do
      if d % 2 == 1 then res = quaternion.__mul(res, acc) end
      d = math.modf(d * 0.5)
      if d > 0 then acc = quaternion.__mul(acc, acc) end
    end
    return res
  else
    if math.abs(quaternion._norm2(Q)-1) > 1E-4 then
      error("Unit quaternion is required!")
    end
    local angle, axis = quaternion.toAA(Q)
    angle = 0.5 * d * angle   -- new angle
    local sa = math.sin(angle)
    return quaternion:_new {
      math.cos(angle), sa*axis[1], sa*axis[2], sa*axis[3]}
  end
end

--- Q1 - Q2
--  @param Q1 First quaternion.
--  @param Q2 Second quaternion.
--  @return Difference.
quaternion.__sub = function (Q1, Q2)
  if not (isquaternion(Q1) and isquaternion(Q2)) then
    local p = Cross.convert(Q1, Q2)
    if p then
      return Q1 - p
    else
      return Cross.convert(Q2, Q1) - Q2
    end
  end
  local q1, q2 = Q1._, Q2._
  return numquat(
    quaternion:_new({q1[1]-q2[1], q1[2]-q2[2], q1[3]-q2[3], q1[4]-q2[4]}))
end

--- String representation of the object.
--  @param Q Quaternion.
--  @return String with quaternion elements.
quaternion.__tostring = function (Q)
  local x, y, z = numStr(Q._[2]), numStr(Q._[3]), numStr(Q._[4])
  return string.format("%s%s%si%s%sj%s%sk", numStr(Q._[1]),
          x:sub(1, 1) == '-' and '' or '+', x,
          y:sub(1, 1) == '-' and '' or '+', y,
          z:sub(1, 1) == '-' and '' or '+', z)
end

--- -Q
--  @param Q Quaternion.
--  @return Opposite quaternion.
quaternion.__unm = function (Q)
  local q = Q._
  return quaternion:_new({-q[1], -q[2], -q[3], -q[4]})
end

quaternion.arithmetic = 'arithmetic'
about[quaternion.arithmetic] = {
  quaternion.arithmetic, 'a + b, a - b, a * b, a ^ k, -a', help.META}

quaternion._convert = function (v)
  return (type(v) == 'number' or type(v) == 'table' and v.float)
         and quaternion:_new({v, 0, 0, 0})
end

--- Quaternion constructor.
--  @param t Table of coefficients (w-i-j-k)
--  @return New quaternion.
quaternion._new = function(self, t) return setmetatable({_=t}, self) end

--- Find square norm.
--  @param Q Quaternion.
--  @return Value of norm.
quaternion._norm2 = function (Q)
  local q = Q._
  return Cross.float(q[1])^2 + Cross.float(q[2])^2 + Cross.float(q[3])^2
    + Cross.float(q[4])^2
end

--- Norm of the quaternion.
--  @return Value of norm.
quaternion.abs = function (Q) return math.sqrt(quaternion._norm2(Q)) end
about[quaternion.abs] = {'Q:abs() --> num', 'Value of the norm.'}
quaternion._norm = quaternion.abs

--- Get conjugated quaternion.
--  @param Q Quaternion.
--  @return Conjugation.
quaternion.conj = function (Q)
  local q = Q._
  return quaternion:_new({q[1], -q[2], -q[3], -q[4]})
end
about[quaternion.conj] = {'Q:conj() --> conj_Q', 'Get conjugation.'}

--- Check quality of two quaternions.
--  @param Q1 First quaternion.
--  @param Q2 Second quaternion.
--  @return True if the quaternions are equal.
quaternion.eq = function (Q1, Q2)
  if not (isquaternion(Q1) and isquaternion(Q2)) then
    local p = Cross.convert(Q1, Q2)
    if p then
      return Q1 == p
    else
      return Cross.convert(Q2, Q1) == Q2
    end
  end
  local q1, q2 = Q1._, Q2._
  return Cross.eq(q1[1], q2[1]) and Cross.eq(q1[2], q2[2]) and
         Cross.eq(q1[3], q2[3]) and Cross.eq(q1[4], q2[4])
end
quaternion.__eq = quaternion.eq

quaternion.comparison = 'comparison'
about[quaternion.comparison] = {
  quaternion.comparison, 'a == b, a ~= b', help.META}

--- Build quaternion from angle-axis representation.
--  @param self Do nothing.
--  @param dAng Angle of rotation.
--  @param vAxe Axis in form of vector object or table with 3 elements.
--  @return New quaternion.
quaternion.fromAA = function (self, dAng, vAxe)
  local x, y, z = nil, nil, nil
  if vAxe.vsmatrix then
    x, y, z = vAxe(1), vAxe(2), vAxe(3)
  else
    x, y, z = vAxe[1], vAxe[2], vAxe[3]
  end
  local d = math.sqrt(x*x+y*y+z*z)
  if d > 0 then d = math.sin(dAng*0.5) / d end
  return quaternion:_new({math.cos(dAng*0.5), x*d, y*d, z*d})
end
about[quaternion.fromAA] = {':fromAA(angle_d,axis_d) --> Q','Create quaternion using angle and axis.', ROTATION}

--- Get quaternion from rotation matrix.
--  @param self Do nothing.
--  @param M Rotation matrix 3x3.
--  @return Equal quaternion.
quaternion.fromRot = function (self, M)
  assert(M.ismatrix)
  local tr = M[1][1] + M[2][2] + M[3][3]
  if tr > 0 then
    local S = 2*math.sqrt(1+tr)
    return quaternion:_new(
      {0.25*S, (M[3][2]-M[2][3])/S, (M[1][3]-M[3][1])/S, (M[2][1]-M[1][2])/S})
  elseif M[1][1] > M[2][2] and M[1][1] > M[3][3] then
    local S = 2*math.sqrt(1+M[1][1]-M[2][2]-M[3][3])
    return quaternion:_new(
      {(M[3][2]-M[2][3])/S, 0.25*S, (M[1][2]+M[2][1])/S, (M[1][3]+M[3][1])/S})
  elseif M[2][2] > M[3][3] then
    local S = 2*math.sqrt(1+M[2][2]-M[1][1]-M[3][3])
    return quaternion:_new(
      {(M[1][3]-M[3][1])/S, (M[1][2]+M[2][1])/S, 0.25*S, (M[2][3]+M[3][2])/S})
  else
    local S = 2*math.sqrt(1+M[3][3]-M[1][1]-M[2][2])
    return quaternion:_new(
      {(M[2][1]-M[1][2])/S, (M[1][3]+M[3][1])/S, (M[2][3]+M[3][2])/S, 0.25*S})
  end
end
about[quaternion.fromRot] = {':fromRot(M) --> Q', 'Convert rotation matrix to quaternion.', ROTATION}

--- Get imaginary part.
--  @param Q Quaternion.
--  @return Table with imaginary elements.
quaternion.im = function (Q) return {Q._[2], Q._[3], Q._[4]} end
about[quaternion.im] = {'Q:im() --> imaginary_t', 
  'Get table of the imaginary part.', help.OTHER}

--- Get inversion.
--  @param Q Quaternion.
--  @return Inverted quaternion.
quaternion.inv = function (Q)
  local k = 1 / quaternion._norm2(Q)
  local q = Q._
  return quaternion:_new({q[1]*k, -q[2]*k, -q[3]*k, -q[4]*k})
end
about[quaternion.inv] = {'Q:inv() --> inv_Q', 'Find inverted quaternion.'}

--- Get equivalent square matrix
--  @param Q Quaternion.
--  @return Equivalent matrix representation.
quaternion.mat = function (Q)
  quaternion.ext_matrix = quaternion.ext_matrix or require('lib.matrix')
  local q = Q._
  return quaternion.ext_matrix:init(4, 4,
    {{q[1], -q[2], -q[3], -q[4]},
     {q[2],  q[1], -q[4],  q[3]},
     {q[3],  q[4],  q[1], -q[2]},
     {q[4], -q[3],  q[2],  q[1]}})
end
about[quaternion.mat] = {'Q:mat() --> M', 
  'Equivalent matrix representation.', help.OTHER}

--- Transform quaternion into unit 'vector' in-place.
--  @param Q Quaternion.
quaternion.normalize = function (Q)
  local k = math.sqrt(quaternion._norm2(Q))
  local q = Q._
  return k > 0 and  quaternion:_new({q[1]/k, q[2]/k, q[3]/k, q[4]/k}) or Q
end
about[quaternion.normalize] = {'Q:normalize() --> unit_Q', 
  'Return unit quaternion.', help.OTHER}

--- Get real part.
--  @param Q Quaternion.
--  @return Real element.
quaternion.re = function(Q) return Q._[1] end
about[quaternion.re] = {'Q:re() --> var', 'Real part.', help.OTHER}

--- Apply quaternion to rotate the vector.
--  @param Q Quaternion.
--  @param vec Vector in form of matrix object or the table with 3 elements.
--  @return Table with new orientation.
quaternion.rotate = function (Q, vec)
  assert(math.abs(quaternion.abs(Q)-1) < 1E-3)
  local p1 = nil
  if vec.ismatrix then
    p1 = quaternion:_new({0, vec(1), vec(2), vec(3)})
  else
    p1 = quaternion:_new({0, vec[1], vec[2], vec[3]})
  end
  local p2 = Q*p1*quaternion.conj(Q)  -- TODO reuse p1
  return {p2._[2], p2._[3], p2._[4]}
end
about[quaternion.rotate] = {'Q:rotate(inVec) --> outVec_t', 
  'Apply quaternion to rotate the vector.', ROTATION}

--- Spherical linear interpolation.
--  @param Q1 Start quaternion.
--  @param Q2 End quaternion.
--  @param f Part from 0 to 1.
--  @return Intermediate quaternion.
quaternion.slerp = function (Q1, Q2, f)
  -- assume quaternions are not unit
  local qa = quaternion.normalize(Q1)
  local qb = quaternion.normalize(Q2)
  local dot = qa._[1]*qb._[1]+qa._[2]*qb._[2]+qa._[3]*qb._[3]+qa._[4]*qb._[4]
  -- should be positive
  if dot < 0 then qb = -qb; dot = -dot end
  -- linear interpolation for close points
  if dot > 0.999 then
    return quaternion.normalize(qa + f*(qb-qa))
  end
  -- calculate
  local theta = math.acos(dot)
  local sin_th = math.sin(theta)
  return (math.sin((1-f)*theta)/sin_th) * qa + (math.sin(f*theta)/sin_th) * qb
end
about[quaternion.slerp] = {'Q:slerp(end_Q,rat_f) --> rat_Q', 
  'Spherical linear interpolation for part t.', help.OTHER}

--- Get angle and axis of the quaternion.
--  @param Q Quaternion.
--  @return Angle and table with unit vector of direction.
quaternion.toAA = function (Q)
  -- normalize
  local d = quaternion.abs(Q)
  local w, x, y, z = Q._[1]/d, Q._[2]/d, Q._[3]/d, Q._[4]/d
  -- get sin
  local v = math.sqrt(x*x+y*y+z*z)     -- what if v == 0 ?
  return 2*Ver.atan2(v, w), {x/v, y/v, z/v}
end
about[quaternion.toAA] = {'Q:toAA() --> angle_d, axis_t', 
  'Get angle and axis of rotation.', ROTATION}

--- Get rotation matrix.
--  @param Q Quaternion.
--  @return Rotation matrix 3x3.
quaternion.toRot = function (Q)
  quaternion.ext_matrix = quaternion.ext_matrix or require('lib.matrix')
  local s = 1 / quaternion._norm2(Q)
  local w, x, y, z = Q._[1], Q._[2], Q._[3], Q._[4]
  return quaternion.ext_matrix {
    {1-2*s*(y*y+z*z), 2*s*(x*y-z*w), 2*s*(x*z+y*w)},
    {2*s*(x*y+z*w), 1-2*s*(x*x+z*z), 2*s*(y*z-x*w)},
    {2*s*(x*z-y*w), 2*s*(y*z+x*w), 1-2*s*(x*x+y*y)}}
end
about[quaternion.toRot] = {'Q:toRot() --> M', 'Get equal rotation matrix.', ROTATION}

--- Get w component.
--  @param Q Quaternion.
--  @return w element.
quaternion.w = function (Q) return Q._[1] end
about[quaternion.w] = {"Q:w() --> var", "Get w component.", help.OTHER}

--- Get x component.
--  @param Q Quaternion.
--  @return x element.
quaternion.x = function (Q) return Q._[2] end
about[quaternion.x] = {"Q:x() --> var", "Get x component.", help.OTHER}

--- Get y component.
--  @param Q Quaternion.
--  @return y element.
quaternion.y = function (Q) return Q._[3] end
about[quaternion.y] = {"Q:y() --> var", "Get y component.", help.OTHER}

--- Get z component.
--  @param Q Quaternion.
--  @return z element.
quaternion.z = function (Q) return Q._[4] end
about[quaternion.z] = {"Q:z() --> var", "Get z component.", help.OTHER}

-- simplify constructor call
setmetatable(quaternion,
{__call = function (self, v)
  assert(type(v) == 'table', "Table is expected")
  v[1] = v[1] or v.w or 0
  v[2] = v[2] or v.x or 0
  v[3] = v[3] or v.y or 0
  v[4] = v[4] or v.z or 0
  local p = v[1]
  assert(type(p) == 'number' or type(p) == 'table' and p.float, "Wrong part w")
  p = v[2]
  assert(type(p) == 'number' or type(p) == 'table' and p.float, "Wrong part x")
  p = v[3]
  assert(type(p) == 'number' or type(p) == 'table' and p.float, "Wrong part y")
  p = v[4]
  assert(type(p) == 'number' or type(p) == 'table' and p.float, "Wrong part z")
  return quaternion:_new(v)
end
})

about[quaternion] = {" {w,x,y,z} --> new_Q", "Create new quaternion.", help.NEW}

-- Comment to remove descriptions
quaternion.about = about

return quaternion

--======================================
--TODO add math functions
--TODO remove re, im
