--[[		sonata/lib/quaternion.lua

--- Operations with unit quaternions.
--
--  Object structure: </br>
--  <code>{w,i,j,k} </code></br>
--  where <i>w</i> is a real part, <i>i, j, k</i> are imaginary elements.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2021.

	module 'quaternion'
--]]

--[[TEST

-- use 'quaternion'
Quat = require 'lib.quaternion'
-- external dependencies, can be loaded implicitly
require 'lib.matrix'

-- quaternion
-- set {w,i,j,k}
a = Quat {1,2,3,4}
-- part of elements
b = Quat {w=3, i=4}
ans = b                       --> Quat{3,4,0,0}

-- conjugation
ans = a:conj()                --> Quat{1,-2,-3,-4}

-- real when imaginary are zeros
ans = a + a:conj()            --> 2

-- norm
ans = b:abs()                --1> 5.000

-- inversion
c = a*a:inv()
ans = c.w                    --1> 1.000

-- arithmetic
ans = a+b                     --> Quat{4,6,3,4}

ans = a*b                     --> Quat{-5,10,25}

ans = 3*b                     --> Quat{9,12,0,0}

-- power
ans = b^3                     --> b * b * b

-- unit quaternion
a:normalize()
ans = a:abs()                --1> 1.000

-- unit power
aa = a^1.5
ans = aa.i                   --3> 0.324

ans = aa.j                   --3> 0.486

ans = aa.k                   --3> 0.648

-- rotation matrix
m = a:toRot()
d = Quat.fromRot(m)
ans = Quat.abs(d-a)          --1> 0.000

-- use angle 
-- and axis
ang = 0.5
axis = {1,1,1}
f = Quat.fromAA(ang,axis)
ans,_ = f:toAA()             --3> ang

-- rotate vector
p = a:rotate({1,0,0})
ans = p[1]                   --3> -0.667

-- spherical interpolation
d = Quat.slerp(a,b,0.5)
ans = d.w                    --3> 0.467

-- make copy
ans = d:copy()                --> d

-- show 
print(d)

--]]

--	LOCAL

local Ver = require("lib.utils").versions

local ROTATION = 'rotation'

-- indeces of components
local keys = {w=1, i=2, j=3, k=4}

--- Check object type.
--  @param t Object.
--  @return True if the object is quaternion.
local function isquaternion(v) return type(v)=='table' and v.isquaternion end

local function numquat(Q) return Q[2]==0 and Q[3]==0 and Q[4]==0 and Q[1] or Q end

--	INFO

local help = SonataHelp and (require "core.help") or {new=function () return {} end}

--	MODULE

local quaternion = {
-- mark
type = 'quaternion', isquaternion = true,
-- description
about = help:new("Operations with quaternions."),
}

-- Get method or key 
quaternion.__index = function (t,k)
  return quaternion[k] or rawget(t,keys[k] or '')
end

-- Set by key
quaternion.__newindex = function (t,k,v)
  if keys[k] then 
    t[keys[k]] = v
  end
end

--- Check arguments.
--  @param Q1 First quaternion or number.
--  @param Q2 Second quaternion or number.
--  @return Two quaternions.
quaternion._args_ = function (Q1,Q2)
  Q1 = isquaternion(Q1) and Q1 or quaternion:_new_({Q1,0,0,0})
  if Q2 then
    Q2 = isquaternion(Q2) and Q2 or quaternion:_new_({Q2,0,0,0})
  end
  return Q1, Q2
end

--- Quaternion constructor.
--  @param t Table of coefficients (w-i-j-k)
--  @return New quaternion.
quaternion._new_ = function(self,t) return setmetatable(t,self) end

--- Build quaternion from angle-axis representation.
--  @param dAng Angle of rotation.
--  @param vAxe Axis in form of vector object or table with 3 elements.
--  @return New quaternion.
quaternion.fromAA = function (dAng, vAxe)
  local x,y,z
  if vAxe.vsmatrix then
    x,y,z = vAxe(1), vAxe(2), vAxe(3)
  else
    x,y,z = vAxe[1], vAxe[2], vAxe[3] 
  end
  local d = math.sqrt(x*x+y*y+z*z)
  if d > 0 then d = math.sin(dAng*0.5) / d end
  return quaternion:_new_({math.cos(dAng*0.5), x*d, y*d, z*d})
end
quaternion.about[quaternion.fromAA] = {'fromAA(fAng,vAxe)','Create quaternion using angle and axis.',ROTATION}

--- Get angle and axis of the quaternion.
--  @param Q Quaternion.
--  @return Angle and table with unit vector of direction.
quaternion.toAA = function (Q)
  -- normalize
  local d = quaternion.abs(Q)
  local w,x,y,z = Q[1]/d,Q[2]/d,Q[3]/d,Q[4]/d
  -- get sin
  local v = math.sqrt(x*x+y*y+z*z)     -- what if v == 0 ?
  return 2*Ver.atan2(v,w), {x/v, y/v, z/v} 
end
quaternion.about[quaternion.toAA] = {'toAA(Q)','Get angle and axis of rotation.',ROTATION}

--- Make copy.
--  @param Q Initial object.
--  @return Copy of the quaternion.
quaternion.copy = function (Q) return quaternion:_new_({Q[1],Q[2],Q[3],Q[4]}) end
quaternion.about[quaternion.copy] = {"copy(t)", "Create a copy of the quaternion.",help.OTHER}

--- Find square norm.
--  @param Q Quaternion.
--  @return Value of norm.
quaternion._norm_ = function (Q) return Q[1]*Q[1]+Q[2]*Q[2]+Q[3]*Q[3]+Q[4]*Q[4] end

--- Get conjugated quaternion.
--  @param Q Quaternion.
--  @return Conjugation.
quaternion.conj = function (Q) return quaternion:_new_({Q[1],-Q[2],-Q[3],-Q[4]}) end
quaternion.about[quaternion.conj] = {'conj(Q)','Get conjugation.'}

--- Norm of the quaternion.
--  @param Q Quaternion.
--  @return Value of norm.
quaternion.abs = function (Q) return math.sqrt(quaternion._norm_(Q)) end
quaternion.about[quaternion.abs] = {'abs(Q)','Value of the norm.'}

--- Get inversion.
--  @param Q Quaternion.
--  @return Inverted quaternion.
quaternion.inv = function (Q) 
  local k = 1 / quaternion._norm_(Q)
  return quaternion:_new_({Q[1]*k, -Q[2]*k, -Q[3]*k, -Q[4]*k})
end
quaternion.about[quaternion.inv] = {'inv(Q)','Find inverted quaternion.'}

--- Transform quaternion into unit 'vector' in-place.
--  @param Q Quaternion.
quaternion.normalize = function (Q)
  local k = math.sqrt(quaternion._norm_(Q))
  if k > 0 then 
    for i = 1,4 do Q[i] = Q[i]/k end
  end
end
quaternion.about[quaternion.normalize] = {'normalize(Q)','Make unit quaternion.',help.OTHER}

--- Apply quaternion to rotate the vector.
--  @param Q Quaternion.
--  @param vec Vector in form of matrix object or the table with 3 elements.
--  @return Table with new orientation.
quaternion.rotate = function (Q,vec)
  assert(math.abs(quaternion.abs(Q)-1) < 1E-3)
  local p1,p2 
  if vec.ismatrix then
    p1 = quaternion:_new_({0,vec(1),vec(2),vec(3)})
  else
    p1 = quaternion:_new_({0,vec[1],vec[2],vec[3]})
  end
  p2 = Q*p1*quaternion.conj(Q) 
  return {p2[2],p2[3],p2[4]}
end
quaternion.about[quaternion.rotate] = {'rotate(Q,vec)','Apply quaternion to rotate the vector.',ROTATION}

--- Get rotation matrix.
--  @param Q Quaternion.
--  @return Rotation matrix 3x3.
quaternion.toRot = function (Q)
  quaternion.ext_matrix = quaternion.ext_matrix or require('lib.matrix')
  local s = 1 / quaternion._norm_(Q)
  local w,i,j,k = Q[1],Q[2],Q[3],Q[4]
  return quaternion.ext_matrix {
    {1-2*s*(j*j+k*k), 2*s*(i*j-k*w), 2*s*(i*k+j*w)},
    {2*s*(i*j+k*w), 1-2*s*(i*i+k*k), 2*s*(j*k-i*w)},
    {2*s*(i*k-j*w), 2*s*(j*k+i*w), 1-2*s*(i*i+j*j)}}
end
quaternion.about[quaternion.toRot] = {'toRot(Q)','Get equal rotation matrix.',ROTATION}

--- Get quaternion from rotation matrix.
--  @param M Rotation matrix 3x3.
--  @return Equal quaternion.
quaternion.fromRot = function (M)
  assert(M.ismatrix)
  local tr = M[1][1] + M[2][2] + M[3][3]
  if tr > 0 then
    local S = 2*math.sqrt(1+tr)
    return quaternion:_new_({0.25*S, (M[3][2]-M[2][3])/S, (M[1][3]-M[3][1])/S, (M[2][1]-M[1][2])/S})
  elseif M[1][1] > M[2][2] and M[1][1] > M[3][3] then
    local S = 2*math.sqrt(1+M[1][1]-M[2][2]-M[3][3])
    return quaternion:_new_({(M[3][2]-M[2][3])/S, 0.25*S, (M[1][2]+M[2][1])/S, (M[1][3]+M[3][1])/S})
  elseif M[2][2] > M[3][3] then
    local S = 2*math.sqrt(1+M[2][2]-M[1][1]-M[3][3])
    return quaternion:_new_({(M[1][3]-M[3][1])/S, (M[1][2]+M[2][1])/S, 0.25*S, (M[2][3]+M[3][2])/S})
  else 
    local S = 2*math.sqrt(1+M[3][3]-M[1][1]-M[2][2])
    return quaternion:_new_({(M[2][1]-M[1][2])/S, (M[1][3]+M[3][1])/S, (M[2][3]+M[3][2])/S, 0.25*S})
  end
end
quaternion.about[quaternion.fromRot] = {'fromRot(M)','Convert rotation matrix to quaternion.',ROTATION}

--- Q1 + Q2
--  @param Q1 First quaternion.
--  @param Q2 Second quaternion.
--  @return Sum object.
quaternion.__add = function (Q1,Q2)
  Q1,Q2 = quaternion._args_(Q1,Q2)
  return numquat(quaternion:_new_({Q1[1]+Q2[1],Q1[2]+Q2[2],Q1[3]+Q2[3],Q1[4]+Q2[4]}))
end

--- Q1 - Q2
--  @param Q1 First quaternion.
--  @param Q2 Second quaternion.
--  @return Difference.
quaternion.__sub = function (Q1,Q2)
  Q1,Q2 = quaternion._args_(Q1,Q2)
  return numquat(quaternion:_new_({Q1[1]-Q2[1],Q1[2]-Q2[2],Q1[3]-Q2[3],Q1[4]-Q2[4]}))
end

--- -Q
--  @param Q Quaternion.
--  @return Opposite quaternion.
quaternion.__unm = function (Q) return quaternion:_new_({-Q[1],-Q[2],-Q[3],-Q[4]}) end

--- Q1 * Q2
--  @param Q1 First quaternion.
--  @param Q2 Second quaternion.
--  @return Product.
quaternion.__mul = function (Q1,Q2)
  Q1,Q2 = quaternion._args_(Q1,Q2)
  return numquat(quaternion:_new_(
    {Q1[1]*Q2[1]-Q1[2]*Q2[2]-Q1[3]*Q2[3]-Q1[4]*Q2[4],
     Q1[1]*Q2[2]+Q1[2]*Q2[1]+Q1[3]*Q2[4]-Q1[4]*Q2[3],
     Q1[1]*Q2[3]-Q1[2]*Q2[4]+Q1[3]*Q2[1]+Q1[4]*Q2[2],
     Q1[1]*Q2[4]+Q1[2]*Q2[3]-Q1[3]*Q2[2]+Q1[4]*Q2[1]}))
end

--- Q ^ k
--  @param Q Quaternion.
--  @param d Any number for unit quaternion, -1 or positive integer for others.
--  @return Power value.
quaternion.__pow = function (Q,d)
  if d == -1 then
    return quaternion.inv(Q)
  elseif d >= 0 and Ver.isInteger(d) then
    -- positive integer use for all quaternions
    local res, acc = quaternion:_new_{1,0,0,0}, quaternion.copy(Q)
    while d > 0 do
      if d % 2 == 1 then res = quaternion.__mul(res, acc) end
      d = math.modf(d * 0.5)
      if d > 0 then acc = quaternion.__mul(acc, acc) end
    end
    return res
  else
    if math.abs(quaternion._norm_(Q)-1) > 1E-4 then error("Can't apply power function!") end
    local angle, axis = quaternion.toAA(Q)
    angle = 0.5 * d * angle   -- new angle
    local sa = math.sin(angle)
    return quaternion:_new_ {math.cos(angle), sa*axis[1], sa*axis[2], sa*axis[3]}
  end
end

quaternion.arithmetic = 'arithmetic'
quaternion.about[quaternion.arithmetic] = {quaternion.arithmetic, 'a + b, a - b, a * b, a ^ k, -a', help.META}

--- Check quality of two quaternions.
--  @param Q1 First quaternion.
--  @param Q2 Second quaternion.
--  @return True if the quaternions are equal.
quaternion.eq = function (Q1,Q2)
  Q1,Q2 = quaternion._args_(Q1,Q2)
  return Q1[1]==Q2[1] and Q1[2]==Q2[2] and Q1[3]==Q2[3] and Q1[4]==Q2[4]
end
quaternion.__eq = quaternion.eq

quaternion.comparison = 'comparison'
quaternion.about[quaternion.comparison] = {quaternion.comparison, 'a == b, a ~= b', help.META}

--- String representation of the object.
--  @param Q Quaternion.
--  @return String with quaternion elements.
quaternion.__tostring = function (Q)
  return string.format("%.3f%+.3fi%+.3fj%+.3fk", Q[1], Q[2], Q[3], Q[4])
end

--- Get real part.
--  @param Q Quaternion.
--  @return Real element.
quaternion.real = function(Q) return Q[1] end
quaternion.about[quaternion.real] = {'real(Q)','Real part (same as Q.w).',help.OTHER}

--- Get imaginary part.
--  @param Q Quaternion.
--  @return Table with imaginary elements.
quaternion.imag = function (Q) return {Q[2],Q[3],Q[4]} end
quaternion.about[quaternion.imag] = {'imag(Q)', 'Get table of the imaginary part.', help.OTHER}

--- Spherical linear interpolation.
--  @param Q1 Start quaternion.
--  @param Q2 End quaternion.
--  @param f Part from 0 to 1.
--  @return Intermediate quaternion.
quaternion.slerp = function (Q1,Q2,f)
  -- assume quaternions are not unit
  local qa = quaternion.copy(Q1) 
  local qb = quaternion.copy(Q2)
  quaternion.normalize(qa); quaternion.normalize(qb)
  local dot = qa[1]*qb[1]+qa[2]*qb[2]+qa[3]*qb[3]+qa[4]*qb[4]
  -- should be positive
  if dot < 0 then qb = -qb; dot = -dot end
  -- linear interpolation for close points
  if dot > 0.999 then
    local res = qa + f*(qb-qa)
    quaternion.normalize(res)
    return res
  end
  -- calculate
  local theta = math.acos(dot)
  local sin_th = math.sin(theta)
  return (math.sin((1-f)*theta)/sin_th) * qa + (math.sin(f*theta)/sin_th) * qb
end
quaternion.about[quaternion.slerp] = {'slerp(Q1,Q2,f)','Spherical linear interpolation for part t.', help.OTHER}

--- Get equivalent square matrix
--  @param Q Quaternion.
--  @return Equivalent matrix representation.
quaternion.mat = function (Q)
  quaternion.ext_matrix = quaternion.ext_matrix or require('lib.matrix')
  return quaternion.ext_matrix:init(4,4,
    {{Q[1],-Q[2],-Q[3],-Q[4]},
     {Q[2], Q[1],-Q[4], Q[3]},
     {Q[3], Q[4], Q[1],-Q[2]},
     {Q[4],-Q[3], Q[2], Q[1]}})
end
quaternion.about[quaternion.mat] = {'mat(Q)','Equivalent matrix representation.',help.OTHER}

-- simplify constructor call
setmetatable(quaternion, 
{__call = function (self,v) 
  assert(type(v) == 'table')
  v[1] = v[1] or v.w or 0
  v[2] = v[2] or v.i or 0
  v[3] = v[3] or v.j or 0
  v[4] = v[4] or v.k or 0
  return quaternion:_new_(v) 
end
})

quaternion.Quat = 'Quat'
quaternion.about[quaternion.Quat] = {"Quat(t={0,0,0,0})", "Create new quaternion.", help.NEW}

-- Uncomment to remove descriptions
--quaternion.about = nil

return quaternion

--======================================
