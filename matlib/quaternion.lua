--[[		sonata/lib/quaternion.lua

--- Operations with unit quaternions.
--
--  Object structure: </br>
--  <code>{w, i, j, k} </code></br>
--  where <i>w</i> is a real part, <i>i, j, k</i> are imaginary elements.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2025.

	module 'quaternion'
--]]


--[[TEST_IT

-- use 'quaternion'
Quat = require 'matlib.quaternion'
-- external dependencies, can be loaded implicitly
require 'matlib.matrix'
-- for pack/unpack
D = require 'matlib.data'

-- quaternion
-- set {w,x,y,z}
a = Quat {1,2,3,4}
-- part of elements
b = Quat {w=3, x=4}
ans = b                       -->  Quat{3, 4, 0, 0}

-- conjugation
ans = a:conj()                -->  Quat{1, -2,-3,-4}

-- real when imaginary are zeros
ans = a + a:conj()            -->  2

-- norm
ans = b:abs()               --.1>  5.000

-- inversion
c = a*a:inv()
ans = c:w()                 --.1>  1.000

-- arithmetic
ans = a+b                     -->  Quat{4, 6,3,4}

ans = a*b                     -->  Quat{-5, 10,25,}

ans = 3*b                     -->  Quat{9, 12}

-- power
ans = b^3                     -->  b * b * b

-- unit quaternion
a = a:normalized()
ans = a:abs()               --.1>  1.000

-- unit power
aa = a^1.5
ans = aa:x()                --.3>  0.324

ans = aa:y()                --.3>  0.486

ans = aa:z()                --.3>  0.648

-- rotation matrix
m = a:toRot()
d = Quat:fromRot(m)
ans = (d-a):abs()           --.1>  0.000

-- use angle
-- and axis
ang = 0.5
axis = {1,1,1}
f = Quat:fromAA(ang,axis)
ans,_ = f:toAA()            --.3>  ang

-- RPY
roll = 0.1
qq = Quat:fromRPY(roll, 0.2, 0.3)
r, p, y = qq:toRPY()
ans = r                     --.1>  roll

-- rotate vector
p = a:rotate({1,0,0})
ans = p[1]                  --.3>  -0.667

-- spherical interpolation
d = Quat:slerp(a, b, 0.5)
ans = d:w()                 --.3>  0.467

-- show
print(d)

-- exp
q = a:exp()
ans = q:x()                 --.3>  0.371

-- log
r = q:log()
ans = r:w()                 --.3>  a:w()

-- pack
t = D:pack(a)
ans = type(t)                 -->  'string'

-- unpack
aa = D:unpack(t)
ans = aa:w()                --.3>  a:w()

--]]


--	LOCAL

local _ext = {
  utils = require("matlib.utils"),
  -- matrix = require("matlib.matrix"),
}


local _ver = _ext.utils.versions
local _cross = _ext.utils.cross
local _utils = _ext.utils.utils
local _num2str = _utils.numstr

-- categories
local ROTATION = 'rotation'


--- Number representation.
--  @param v Value.
--  @return String representation.
local function _numStr(v)
  return type(v) == 'number' and _num2str(v) or tostring(v)
end


--	INFO

local _help = SonataHelp or {}
-- description
local _about = {
__module__ = "Operations with quaternions."
}


--	MODULE

local quaternion = {
-- mark
type = 'quaternion',
isquaternion = true,
-- simplify
_simp = function (Q)
  return _cross.isZero(Q._[2]) and _cross.isZero(Q._[3]) and _cross.isZero(Q._[4])
    and _cross.simp(Q._[1]) or Q
end
}


--- Check object type.
--  @param t Object.
--  @return True if the object is quaternion.
local function _isquaternion(v) return getmetatable(v) == quaternion end


--- Make quaternion or return number.
local function _numOrQuat(w, x, y, z)
  return _cross.isZero(x) and _cross.isZero(y) and _cross.isZero(z) and w
    or quaternion._new(w, x, y, z)
end


--- Q1 + Q2
--  @param Q1 First quaternion.
--  @param Q2 Second quaternion.
--  @return Sum object.
quaternion.__add = function (Q1, Q2)
  if not _isquaternion(Q2) then
    local v = quaternion._convert(Q2)
    return v and Q1 + v or Q2.__add(Q1, Q2)
  elseif not _isquaternion(Q1) then
    local v = quaternion._convert(Q1)
    return v and v + Q2 or error('Not def')
  end
  local q1, q2 = Q1._, Q2._
  return _numOrQuat(q1[1]+q2[1], q1[2]+q2[2], q1[3]+q2[3], q1[4]+q2[4])
end


--- Check Q1 == Q2
--  @param Q1 First quaternion.
--  @param Q2 Second quaternion.
--  @return True if the quaternions are equal.
quaternion.__eq = function (Q1, Q2)
  if not (_isquaternion(Q1) and _isquaternion(Q2)) then
    local p = _cross.convert(Q1, Q2)
    if p then
      return Q1 == p
    else
      return _cross.convert(Q2, Q1) == Q2
    end
  end
  local q1, q2 = Q1._, Q2._
  return _cross.eq(q1[1], q2[1]) and _cross.eq(q1[2], q2[2])
    and  _cross.eq(q1[3], q2[3]) and _cross.eq(q1[4], q2[4])
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
  if not _isquaternion(Q2) then
    local v = quaternion._convert(Q2)
    return v and Q1 * v or Q2.__mul(Q1, Q2)
  elseif not _isquaternion(Q1) then
    local v = quaternion._convert(Q1)
    return v and v * Q2 or error('Not def')
  end
  local q1, q2 = Q1._, Q2._
  return _numOrQuat(
    q1[1]*q2[1] - q1[2]*q2[2] - q1[3]*q2[3] - q1[4]*q2[4],
    q1[1]*q2[2] + q1[2]*q2[1] + q1[3]*q2[4] - q1[4]*q2[3],
    q1[1]*q2[3] - q1[2]*q2[4] + q1[3]*q2[1] + q1[4]*q2[2],
    q1[1]*q2[4] + q1[2]*q2[3] - q1[3]*q2[2] + q1[4]*q2[1])
end


--- Q ^ k
--  @param d Any number for unit quaternion, -1 or positive integer for others.
--  @return Power value.
quaternion.__pow = function (self, d)
  if d == -1 then
    return quaternion.inv(self)
  elseif d >= 0 and _ver.isInteger(d) then
    -- positive integer use for all quaternions
    local res, acc = quaternion._new(1, 0, 0, 0), self
    while d > 0 do
      if d % 2 == 1 then res = quaternion.__mul(res, acc) end
      d = math.floor(d * 0.5)
      if d > 0 then acc = quaternion.__mul(acc, acc) end
    end
    return res
  else
    local q = quaternion.normalized(self)
    local angle, axis = quaternion.toAA(self)
    if not axis then return 1.0 end  -- no imaginary part
    angle = 0.5 * d * angle   -- new angle
    local sa = math.sin(angle)
    return _numOrQuat (
      math.cos(angle), sa*axis[1], sa*axis[2], sa*axis[3])
  end
end


--- Q1 - Q2
--  @param Q1 First quaternion.
--  @param Q2 Second quaternion.
--  @return Difference.
quaternion.__sub = function (Q1, Q2)
  if not _isquaternion(Q2) then
    local v = quaternion._convert(Q2)
    return v and Q1 - v or Q2.__sub(Q1, Q2)
  elseif not _isquaternion(Q1) then
    local v = quaternion._convert(Q1)
    return v and v - Q2 or error('Not def')
  end
  local q1, q2 = Q1._, Q2._
  return _numOrQuat(q1[1]-q2[1], q1[2]-q2[2], q1[3]-q2[3], q1[4]-q2[4])
end


--- String representation of the object.
--  @return String with quaternion elements.
quaternion.__tostring = function (self)
  local x, y, z = _numStr(self._[2]), _numStr(self._[3]), _numStr(self._[4])
  return string.format("%s%s%si%s%sj%s%sk", _numStr(self._[1]),
    x:sub(1, 1) == '-' and '' or '+', x,
    y:sub(1, 1) == '-' and '' or '+', y,
    z:sub(1, 1) == '-' and '' or '+', z)
end


--- -Q
--  @return Opposite quaternion.
quaternion.__unm = function (self)
  local q = self._
  return quaternion._new(-q[1], -q[2], -q[3], -q[4])
end


_about['_ar'] = {'arithmetic: a+b, a-b, a*b, a^k, -a', nil, _help.META}
_about['_cmp'] = {'comparison: a==b, a~=b', nil, _help.META}


--- Transform number to quaternion.
--  @param v Input value.
--  @return quaternion or false.
quaternion._convert = function (v)
  return (type(v) == 'number' or type(v) == 'table' and v.float)
    and quaternion._new(v, 0, 0, 0)
end


--- Quaternion constructor.
--  @return New quaternion.
quaternion._new = function(w, i, j, k)
  return setmetatable({_={w,i,j,k}}, quaternion)
end


--- Find square norm.
--  @param Q Quaternion.
--  @return Value of norm.
quaternion._norm2 = function (Q)
  local q = Q._
  return _cross.float(q[1])^2 + _cross.float(q[2])^2 + _cross.float(q[3])^2
    + _cross.float(q[4])^2
end


--- Dump to binary string.
--  @param acc Accumulator table.
--  @return String with object representation.
quaternion._pack = function (self, acc)
  local t = {string.pack('B', acc['quaternion']), _utils.pack_seq(self._, 1, 4, acc)}
  return table.concat(t)
end


--- Undump from binary string.
--  @param src Source string.
--  @param pos Start position.
--  @param acc Accumulator table.
--  @param ver Pack algorithm version.
--  @return Complex object.
quaternion._unpack = function (src, pos, acc, ver)
  local t, p = _utils.unpack_seq(4, src, pos, acc, ver)
  return quaternion._new(t[1], t[2], t[3], t[4]), p
end



--- Norm of the quaternion.
--  @return Value of norm.
quaternion.abs = function (self) return math.sqrt(quaternion._norm2(self)) end
_about[quaternion.abs] = {'Q:abs() --> num', 'Value of the norm.'}
quaternion._norm = quaternion.abs


--- Get conjugated quaternion.
--  @return Conjugation.
quaternion.conj = function (self)
  local q = self._
  return quaternion._new(q[1], -q[2], -q[3], -q[4])
end
_about[quaternion.conj] = {'Q:conj() --> conj_Q', 'Get conjugation. Equal to ~Q.'}
quaternion.__bnot = quaternion.conj


--- Find exponential for the quaternion.
quaternion.exp = function (self)
  local w, x, y, z = _ver.unpack(self._)
  local v = math.sqrt(_cross.float(x)^2 + _cross.float(y)^2 + _cross.float(z)^2)
  w = math.exp(assert(_cross.float(w)))  -- reuse
  if v == 0 then return w end
  local svv = math.sin(v) / v * w
  return quaternion._new(math.cos(v)*w, x*svv, y*svv, z*svv)
end
_about[quaternion.exp] = {'Q:exp() --> exp_Q', 'Quaternion exponential.'}


--- Build quaternion from angle-axis representation.
--  @param dAng Angle of rotation.
--  @param vAxe Axis in form of vector object or table with 3 elements.
--  @return New quaternion.
quaternion.fromAA = function (_, dAng, vAxe)
  local x, y, z = nil, nil, nil
  if vAxe.ismatrix then
    x, y, z = vAxe(1), vAxe(2), vAxe(3)
  else
    x, y, z = vAxe[1], vAxe[2], vAxe[3]
  end
  local d = math.sqrt(x*x+y*y+z*z)
  if d > 0 then d = math.sin(dAng*0.5) / d end
  return quaternion._new(math.cos(dAng*0.5), x*d, y*d, z*d)
end
_about[quaternion.fromAA] = {':fromAA(angle_d, axis_t|V) --> Q',
  'Create quaternion using angle and axis.', ROTATION}


--- Get quaternion from rotation matrix.
--  @param M Rotation matrix 3x3.
--  @return Equal quaternion.
quaternion.fromRot = function (_, M)
  assert(M.ismatrix)
  local tr = M[1][1] + M[2][2] + M[3][3]
  if tr > 0 then
    local S = 2*math.sqrt(1+tr)
    return quaternion._new(
      0.25*S, (M[3][2]-M[2][3])/S, (M[1][3]-M[3][1])/S, (M[2][1]-M[1][2])/S)
  elseif M[1][1] > M[2][2] and M[1][1] > M[3][3] then
    local S = 2*math.sqrt(1+M[1][1]-M[2][2]-M[3][3])
    return quaternion._new(
      (M[3][2]-M[2][3])/S, 0.25*S, (M[1][2]+M[2][1])/S, (M[1][3]+M[3][1])/S)
  elseif M[2][2] > M[3][3] then
    local S = 2*math.sqrt(1+M[2][2]-M[1][1]-M[3][3])
    return quaternion._new(
      (M[1][3]-M[3][1])/S, (M[1][2]+M[2][1])/S, 0.25*S, (M[2][3]+M[3][2])/S)
  else
    local S = 2*math.sqrt(1+M[3][3]-M[1][1]-M[2][2])
    return quaternion._new(
      (M[2][1]-M[1][2])/S, (M[1][3]+M[3][1])/S, (M[2][3]+M[3][2])/S, 0.25*S)
  end
end
_about[quaternion.fromRot] = {':fromRot(M) --> Q',
  'Convert rotation matrix to quaternion.', ROTATION}


--- Obtain quaternion from the Euler angles.
--  @param roll Angle w.r.t. X
--  @param pitch Angle w.r.t. Y
--  @param yaw Angle w.r.t. Z
--  @return Equal quaternion.
quaternion.fromRPY = function (_, roll, pitch, yaw)
  local cr = math.cos(roll * 0.5)
  local sr = math.sin(roll * 0.5)
  local cp = math.cos(pitch * 0.5)
  local sp = math.sin(pitch * 0.5)
  local cy = math.cos(yaw * 0.5)
  local sy = math.sin(yaw * 0.5)
  return quaternion._new(
    cr * cp * cy + sr * sp * sy,
    sr * cp * cy - cr * sp * sy,
    cr * sp * cy + sr * cp * sy,
    cr * cp * sy - sr * sp * cy)
end
_about[quaternion.fromRPY] = {":fromRPY(roll_d, pitch_d, yaw_d) --> Q",
  "Convert Euler angles to quaternion.", ROTATION}


--- Get inversion.
--  @return Inverted quaternion.
quaternion.inv = function (self)
  local k = 1 / quaternion._norm2(self)
  local q = self._
  return quaternion._new(q[1]*k, -q[2]*k, -q[3]*k, -q[4]*k)
end
_about[quaternion.inv] = {'Q:inv() --> inv_Q', 'Find inverted quaternion.'}


--- Logarithm of the quaternion.
--  @return logarithm value.
quaternion.log = function (self)
  local w, x, y, z = _ver.unpack(self._)
  local v = math.sqrt(_cross.float(x)^2 + _cross.float(y)^2 + _cross.float(z)^2)
  w = _cross.float(w)
  local n = math.sqrt(w*w + v*v)
  if v == 0 then return math.log(n) end
  local k = math.acos(w / n) / v
  return quaternion._new(math.log(n), x*k, y*k, z*k)
end
_about[quaternion.log] = {'Q:log() --> log_Q', 'Quaternion logarithm.'}


--- Transform quaternion into unit 'vector' in-place.
--  @return unit quaternion or the same one.
quaternion.normalized = function (self)
  local k = math.sqrt(quaternion._norm2(self))
  local q = self._
  return k > 0 and quaternion._new(q[1]/k, q[2]/k, q[3]/k, q[4]/k) or self
end
_about[quaternion.normalized] = {'Q:normalized() --> unit_Q',
  'Return unit quaternion.', _help.OTHER}


--- Apply quaternion to rotate the vector.
--  @param vec Vector in form of matrix object or the table with 3 elements.
--  @return Table with new orientation.
quaternion.rotate = function (self, vec)
  assert(math.abs(quaternion.abs(self)-1) < 1E-3)
  local p1 = nil
  if vec.ismatrix then
    p1 = quaternion._new(0, vec(1), vec(2), vec(3))
  else
    p1 = quaternion._new(0, vec[1], vec[2], vec[3])
  end
  local p2 = self*p1*quaternion.conj(self)
  return {p2._[2], p2._[3], p2._[4]}
end
_about[quaternion.rotate] = {'Q:rotate(in_t|V) --> out_t',
  'Apply quaternion to rotate the vector.', ROTATION}


--- Spherical linear interpolation.
--  @param Q1 Start quaternion.
--  @param Q2 End quaternion.
--  @param f Part from 0 to 1.
--  @return Intermediate quaternion.
quaternion.slerp = function (_, Q1, Q2, f)
  -- assume quaternions are not unit
  local qa = quaternion.normalized(Q1)
  local qb = quaternion.normalized(Q2)
  local dot = qa._[1]*qb._[1] + qa._[2]*qb._[2] + qa._[3]*qb._[3] + qa._[4]*qb._[4]
  -- should be positive
  if dot < 0 then qb = -qb; dot = -dot end
  -- linear interpolation for close points
  if dot > 0.999 then
    return quaternion.normalized(qa + f*(qb-qa))
  end
  -- calculate
  local theta = math.acos(dot)
  local sin_th = math.sin(theta)
  return (math.sin((1-f)*theta)/sin_th) * qa + (math.sin(f*theta)/sin_th) * qb
end
_about[quaternion.slerp] = {':slerp(beg_Q, end_Q, rat_f) --> rat_Q',
  'Spherical linear interpolation for the given ratio.', _help.OTHER}


--- Get angle and axis of the quaternion.
--  @return Angle and table with unit vector of direction.
quaternion.toAA = function (self)
  if math.abs(quaternion._norm2(self)-1) > 1E-4 then
    error "Unit quaternion is required!"
  end
  local w, x, y, z = _ver.unpack(self._)
  -- get sin
  local v = math.sqrt(x*x + y*y + z*z)
  if math.abs(v) < 1E-16 then
    return 1.0, nil
  else
    return 2*_ver.atan2(v, w), {x/v, y/v, z/v}
  end
end
_about[quaternion.toAA] = {'Q:toAA() --> angle_d, axis_t|nil',
  'Get angle and axis of rotation.', ROTATION}


--- Get rotation matrix.
--  @return Rotation matrix 3x3.
quaternion.toRot = function (self)
  _ext.matrix = _ext.matrix or require('matlib.matrix')
  if math.abs(quaternion._norm2(self)-1) > 1E-4 then
    error "Unit quaternion is required!"
  end
  local w, x, y, z = _ver.unpack(self._)
  return _ext.matrix {
    {1-2*(y*y+z*z), 2*(x*y-z*w), 2*(x*z+y*w)},
    {2*(x*y+z*w), 1-2*(x*x+z*z), 2*(y*z-x*w)},
    {2*(x*z-y*w), 2*(y*z+x*w), 1-2*(x*x+y*y)}}
end
_about[quaternion.toRot] = {'Q:toRot() --> M',
  'Get equal rotation matrix.', ROTATION}


--- Get Euler angles.
--  @return roll, pitch, yaw
quaternion.toRPY = function (self)
  if math.abs(quaternion._norm2(self)-1) > 1E-4 then
    error("Unit quaternion is required!")
  end
  local w, x, y, z = _ver.unpack(self._)
  local roll  = _ver.atan2(2*(w*x + y*z), 1 - 2*(x*x + y*y))
  local pitch = math.asin(2*(w*y - z*x))
  local yaw   = _ver.atan2(2*(w*z + x*y), 1 - 2*(y*y + z*z))
  return roll, pitch, yaw
end
_about[quaternion.toRPY] = {"Q:toRPY() --> roll_d, pitch_d, yaw_d",
  "Get Euler angles.", ROTATION}


--- Get w component.
--  @param Q Quaternion.
--  @return w element.
quaternion.w = function (Q) return Q._[1] end
_about[quaternion.w] = {"Q:w() --> var", "Get w component.", _help.OTHER}
-- marker of a complex number
quaternion.re = quaternion.w


--- Get x component.
--  @param Q Quaternion.
--  @return x element.
quaternion.x = function (Q) return Q._[2] end
_about[quaternion.x] = {"Q:x() --> var", "Get x component.", _help.OTHER}


--- Get y component.
--  @param Q Quaternion.
--  @return y element.
quaternion.y = function (Q) return Q._[3] end
_about[quaternion.y] = {"Q:y() --> var", "Get y component.", _help.OTHER}


--- Get z component.
--  @param Q Quaternion.
--  @return z element.
quaternion.z = function (Q) return Q._[4] end
_about[quaternion.z] = {"Q:z() --> var", "Get z component.", _help.OTHER}


-- simplify constructor call
setmetatable(quaternion,
{__call = function (_, v)
  if _isquaternion(v) then return v end
  assert(type(v) == 'table', "Table is expected")
  local w = v[1] or v.w or 0
  local x = v[2] or v.x or 0
  local y = v[3] or v.y or 0
  local z = v[4] or v.z or 0
  assert(type(w) == 'number' or type(w) == 'table' and w.float, "Wrong part w")
  assert(type(x) == 'number' or type(x) == 'table' and x.float, "Wrong part x")
  assert(type(y) == 'number' or type(y) == 'table' and y.float, "Wrong part y")
  assert(type(z) == 'number' or type(z) == 'table' and z.float, "Wrong part z")
  return quaternion._new(w, x, y, z)
end
})
_about[quaternion] = {" {x, y, z, w} --> new_Q",
  "Create new quaternion.", _help.NEW}


-- Comment to remove descriptions
quaternion.about = _about

return quaternion

--======================================
