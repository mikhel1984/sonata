--[[      sonatalib/quaternion.lua

--- Operations with unit quaternions.
--
--  Object structure:        </br>
--  <code>{w,x,y,z} </code></br>
--  where <i>w</i> is a real part, <i>x, y, z</i> are imaginary elements.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonatalib</a> collection, 2017-2019.

           module 'quaternion'
--]]

--[[TEST

-- initialize
Quat = require 'sonatalib.quaternion'

-- new quaternion
-- set {w,x,y,z}
a = Quat {1,2,3,4}
-- part of elements
b = Quat {w=3, x=4}
ans = b               --> Quat{3,4,0,0}

-- conjugation
ans = a:conj()        --> Quat{1,-2,-3,-4}

-- norm
ans = b:abs()         --1> 5.000

-- inversion
c = a*a:inv()
ans = c:qw()          --1> 1.000

-- arithmetic
ans = a+b             --> Quat{4,6,3,4}

ans = a*b             --> Quat{-5,10,25}

ans = 3*b             --> Quat{9,12,0,0}

-- power
ans = b^3             --> b * b * b

-- unit quaternion
a:normalize()
ans = a:abs()         --1> 1.000

-- unit power
aa = a^1.5
ans = aa:qz()         --3> 0.648

-- rotation matrix
m = a:toRot()
d = Quat.fromRot(m)
ans = Quat.abs(d-a)   --3> 0.0456

-- use angle 
-- and axis
ang = 0.5
axis = {1,1,1}
f = Quat.fromAA(ang,axis)
ans,_ = f:toAA()      --3> ang

-- rotate vector
p = a:rotate({1,0,0})
ans = p[1]            --3> -0.667

-- spherical interpolation
d = Quat.slerp(a,b,0.5)
ans = d:qw()          --3> 0.467

-- make copy
ans = d:copy()        --> d

--]]

--	LOCAL

local Ver = require "sonatalib.versions"

-- element indexation
local Q_w, Q_i, Q_j, Q_k = 1, 2, 3, 4
local ROTATION = 'rotation'

--- Check object type.
--  @param t Object.
--  @return True if the object is quaternion.
local function isquaternion(t) return type(t)=='table' and t.isquaternion end

--	INFO

local help = LC_DIALOG and (require "sonatalib.help") or {new=function () return {} end}

--	MODULE

local quaternion = {
-- mark
type = 'quaternion', isquaternion = true,
-- description
about = help:new("Operations with quaternions."),
}
-- methametods
quaternion.__index = quaternion

-- interaction with matrices
quaternion.lc_matrix = require 'sonatalib.matrix'

--- Check arguments.
--  @param q1 First quaternion or number.
--  @param q2 Second quaternion or number.
--  @return Two quaternions.
quaternion._args_ = function (q1,q2)
   q1 = isquaternion(q1) and q1 or quaternion:new({q1,0,0,0})
   if q2 then
      q2 = isquaternion(q2) and q2 or quaternion:new({q2,0,0,0})
   end
   return q1, q2
end

--- Quaternion constructor.
--  @param t Table of coefficients (w-i-j-k)
--  @return New quaternion.
quaternion.new = function(self,t) return setmetatable(t,self) end

--- Build quaternion from angle-axis representation.
--  @param fAng Angle of rotation.
--  @param vAxe Axis in form of vector object or table with 3 elements.
--  @return New quaternion.
quaternion.fromAA = function (fAng, vAxe)
   local x,y,z
   if vAxe.ismatrix then
      x,y,z = vAxe(1), vAxe(2), vAxe(3)
   else
      x,y,z = vAxe[1], vAxe[2], vAxe[3] 
   end
   local d = math.sqrt(x*x+y*y+z*z)
   d = math.sin(fAng*0.5) / d    -- normalize and multiply by angle
   return quaternion:new({math.cos(fAng*0.5), x*d, y*d, z*d})
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
   local v = math.sqrt(x*x+y*y+z*z)
   return 2*Ver.atan2(v,w), {x/v, y/v, z/v} 
end
quaternion.about[quaternion.toAA] = {'toAA(Q)','Get angle and axis of rotation.',ROTATION}

--- Make copy.
--  @param Q Initial object.
--  @return Copy of the quaternion.
quaternion.copy = function (Q) return quaternion:new({Q[1],Q[2],Q[3],Q[4]}) end
quaternion.about[quaternion.copy] = {"copy(t)", "Create a copy of the quaternion.",help.OTHER}

--- Find square norm.
--  @param Q Quaternion.
--  @return Value of norm.
quaternion._norm_ = function (Q) return Q[1]*Q[1]+Q[2]*Q[2]+Q[3]*Q[3]+Q[4]*Q[4] end

--- Get conjugated quaternion.
--  @param Q Quaternion.
--  @return Conjugation.
quaternion.conj = function (Q) return quaternion:new({Q[1],-Q[2],-Q[3],-Q[4]}) end
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
   return quaternion:new({Q[1]*k, -Q[2]*k, -Q[3]*k, -Q[4]*k})
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
      p1 = quaternion:new({0,vec(1),vec(2),vec(3)})
   else
      p1 = quaternion:new({0,vec[1],vec[2],vec[3]})
   end
   p2 = Q*p1*quaternion.conj(Q) 
   return {p2[2],p2[3],p2[4]}
end
quaternion.about[quaternion.rotate] = {'rotate(Q,vec)','Apply quaternion to rotate the vector.',ROTATION}

--- Get rotation matrix.
--  @param Q Quaternion.
--  @return Rotation matrix 3x3.
quaternion.toRot = function (Q)
   local mat = quaternion.lc_matrix
   local s = 1 / quaternion._norm_(Q)
   local w,i,j,k = Q[1],Q[2],Q[3],Q[4]
   return mat {
      {1-2*s*(j*j+k*k), 2*s*(i*j-k*w), 2*s*(i*j+j*w)},
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
      return quaternion:new({0.25*S, (M[3][2]-M[2][3])/S, (M[1][3]-M[3][1])/S, (M[2][1]-M[1][2])/S})
   elseif M[1][1] > M[2][2] and M[1][1] > M[3][3] then
      local S = 2*math.sqrt(1+M[1][1]-M[2][2]-M[3][3])
      return quaternion:new({(M[3][2]-M[2][3])/S, 0.25*S, (M[1][2]+M[2][1])/S, (M[1][3]+M[3][1])/S})
   elseif M[2][2] > M[3][3] then
      local S = 2*math.sqrt(1+M[2][2]-M[1][1]-M[3][3])
      return quaternion:new({(M[1][3]-M[3][1])/S, (M[1][2]+M[2][1])/S, 0.25*S, (M[2][3]+M[3][2])/S})
   else 
      local S = 2*math.sqrt(1+M[3][3]-M[1][1]-M[2][2])
      return quaternion:new({(M[2][1]-M[1][2])/S, (M[1][3]+M[3][1])/S, (M[2][3]+M[3][2])/S, 0.25*S})
   end
end
quaternion.about[quaternion.fromRot] = {'fromRot(M)','Convert rotation matrix to quaternion.',ROTATION}

--- Q1 + Q2
--  @param Q1 First quaternion.
--  @param Q2 Second quaternion.
--  @return Sum object.
quaternion.__add = function (Q1,Q2)
   Q1,Q2 = quaternion._args_(Q1,Q2)
   return quaternion:new({Q1[1]+Q2[1],Q1[2]+Q2[2],Q1[3]+Q2[3],Q1[4]+Q2[4]})
end

--- Q1 - Q2
--  @param Q1 First quaternion.
--  @param Q2 Second quaternion.
--  @return Difference.
quaternion.__sub = function (Q1,Q2)
   Q1,Q2 = quaternion._args_(Q1,Q2)
   return quaternion:new({Q1[1]-Q2[1],Q1[2]-Q2[2],Q1[3]-Q2[3],Q1[4]-Q2[4]})
end

--- -Q
--  @param Q Quaternion.
--  @return Opposite quaternion.
quaternion.__unm = function (Q) return quaternion:new({-Q[1],-Q[2],-Q[3],-Q[4]}) end

--- Q1 * Q2
--  @param Q1 First quaternion.
--  @param Q2 Second quaternion.
--  @return Product.
quaternion.__mul = function (Q1,Q2)
   Q1,Q2 = quaternion._args_(Q1,Q2)
   return quaternion:new(
      {Q1[1]*Q2[1]-Q1[2]*Q2[2]-Q1[3]*Q2[3]-Q1[4]*Q2[4],
       Q1[1]*Q2[2]+Q1[2]*Q2[1]+Q1[3]*Q2[4]-Q1[4]*Q2[3],
       Q1[1]*Q2[3]-Q1[2]*Q2[4]+Q1[3]*Q2[1]+Q1[4]*Q2[2],
       Q1[1]*Q2[4]+Q1[2]*Q2[3]-Q1[3]*Q2[2]+Q1[4]*Q2[1]})
end

--- Q ^ k
--  @param Q Quaternion.
--  @param k Each number for unit quaternion, -1 or positive integer for others.
--  @return Power value.
quaternion.__pow = function (Q,k)
   if k == -1 then
      return quaternion.inv(Q)
   elseif k >= 0 and Ver.isInteger(k) then
      -- positive integer use for all quaternions
      local res, acc = quaternion:new{1,0,0,0}, quaternion.copy(Q)
      while k > 0 do
         if k % 2 == 1 then res = quaternion.__mul(res, acc) end
	 k = math.modf(k * 0.5)
	 if k > 0 then acc = quaternion.__mul(acc, acc) end
      end
      return res
   else
      if math.abs(quaternion._norm_(Q)-1) > 1E-4 then error("Can't apply power function!") end
      local angle, axis = quaternion.toAA(Q)
      angle = 0.5 * k * angle    -- new angle
      local sa = math.sin(angle)
      return quaternion:new {math.cos(angle), sa*axis[1], sa*axis[2], sa*axis[3]}
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
   return string.format("%.3f%+.3fi%+.3fj%+.3fk", Q[Q_w], Q[Q_i], Q[Q_j], Q[Q_k])
end

--- Get real part.
--  @param Q Quaternion.
--  @return Real element.
quaternion.qw = function(Q) return Q[Q_w] end
quaternion.real = quaternion.qw
quaternion.about[quaternion.qw] = {'qw(Q)','Real part (same as real(Q)).',help.OTHER}

--- Get x.
--  @param Q Quaternion.
--  @return Element x.
quaternion.qx = function(Q) return Q[Q_i] end
quaternion.about[quaternion.qx] = {'qx(Q)','Element x.',help.OTHER}

--- Get y.
--  @param Q Quaternion.
--  @return Element y.
quaternion.qy = function(Q) return Q[Q_j] end
quaternion.about[quaternion.qy] = {'qy(Q)','Element y.',help.OTHER}

--- Get z.
--  @param Q Quaternion.
--  @return Element z.
quaternion.qz = function(Q) return Q[Q_k] end
quaternion.about[quaternion.qz] = {'qz(Q)','Element z.',help.OTHER}

--- Get imaginary part.
--  @param Q Quaternion.
--  @return Table with imaginary elements.
quaternion.imag = function (Q) return {Q[Q_i],Q[Q_j],Q[Q_k]} end
quaternion.about[quaternion.imag] = {'imag(Q)', 'Get table of the imaginary part.', help.OTHER}

--- Spherical linear interpolation.
--  @param Q1 Start quaternion.
--  @param Q2 End quaternion.
--  @param t Part from 0 to 1.
--  @return Intermediate quaternion.
quaternion.slerp = function (Q1,Q2,t)
   -- assume quaternions are not unit
   local qa = quaternion.copy(Q1) 
   local qb = quaternion.copy(Q2)
   quaternion.normalize(qa); quaternion.normalize(qb)
   local dot = qa[1]*qb[1]+qa[2]*qb[2]+qa[3]*qb[3]+qa[4]*qb[4]
   -- should be positive
   if dot < 0 then qb = -qb; dot = -dot end
   -- linear interpolation for close points
   if dot > 0.999 then
      local res = qa + t*(qb-qa)
      quaternion.normalize(res)
      return res
   end
   -- calculate
   local theta = math.acos(dot)
   local sin_th = math.sin(theta)
   return (math.sin((1-t)*theta)/sin_th) * qa + (math.sin(t*theta)/sin_th) * qb
end
quaternion.about[quaternion.slerp] = {'slerp(Q1,Q2,t)','Spherical linear interpolation for part t.', help.OTHER}

--- Get equivalent square matrix
--  @param Q Quaternion.
--  @return Equivalent matrix representation.
quaternion.mat = function (Q)
   return quaternion.lc_matrix:init(4,4,
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
   v[2] = v[2] or v.x or 0
   v[3] = v[3] or v.y or 0
   v[4] = v[4] or v.z or 0
   return quaternion:new(v) end
})

quaternion.Quat = 'Quat'
quaternion.about[quaternion.Quat] = {"Quat(t={0,0,0,0})", "Create new quaternion.", help.NEW}

-- free memory in case of standalone usage
if not LC_DIALOG then quaternion.about = nil end

return quaternion

--======================================
