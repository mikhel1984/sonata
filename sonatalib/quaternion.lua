--[[       sonatalib/quaternion.lua

--- Operations with quaternions.
--  @author My Name

           module 'quaternion'
--]]

--[[ TEST

Quat = require 'sonatalib.quaternion'

--]]

--	LOCAL

-- element indexation
local Q_w, Q_i, Q_j, Q_k = 1, 2, 3, 4

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

quaternion.lc_matrix = require 'sonatalib.matrix'

quaternion._args_ = function (q1,q2)
   q1 = isquaternion(q1) and q1 or quaternion:new({q1,0,0,0})
   if q2 then
      q2 = isquaternion(q2) and q2 or quaternion:new({q2,0,0,0})
   end
   return q1, q2
end

--- Quaternion constructor.
--  @param t Table of coefficients (w-i-j-k)
--  @return New object of quaternion.
quaternion.new = function(self,t) return setmetatable(t,self) end

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

quaternion.toAA = function (Q)
   -- normalize
   local d = quaternion.abs(Q)
   local w,x,y,z = Q[1]/d,Q[2]/d,Q[3]/d,Q[4]/d
   -- get sin
   local v = math.sqrt(x*x+y*y+z*z)
   return math.atan(v,w), {x/v, y/v, z/v}    -- replace atan with Ver.atan2
end

--- Make copy.
--  @param Q Initial object.
--  @return Copy of the quaternion.
quaternion.copy = function (Q) return quaternion:new({Q[1],Q[2],Q[3],Q[4]}) end
quaternion.about[quaternion.copy] = {"copy(t)", "Create a copy of the quaternion."}

quaternion._norm_ = function (Q) return Q[1]*Q[1]+Q[2]*Q[2]+Q[3]*Q[3]+Q[4]*Q[4] end

quaternion.conj = function (Q) return quaternion:new({Q[1],-Q[2],-Q[3],-Q[4]}) end

quaternion.abs = function (Q) return math.sqrt(quaternion._norm_(Q)) end

quaternion.inv = function (Q) 
   local k = quaternion._norm_(Q)
   if k > 0 then k = 1 / k end              -- should it exists ?
   return quaternion:new({Q[1]*k, Q[2]*k, Q[3]*k, Q[4]*k})
end

quaternion.normalize = function (Q)
   local k = math.sqrt(quaternion._norm_(Q))
   if k > 0 then 
      for i = 1,4 do Q[i] = Q[i]/k end
   end
   return Q
end

quaternion.rotate = function (Q,vec)
   assert(math.abs(quaternion.abs(Q)-1) < 1E-3)
   local p1,p2 
   if vec.ismatrix then
      p1 = quaternion:init({0,vec(1),vec(2),vec(3)})
   else
      p1 = quaternion:init({0,vec[1],vec[2],vec[3]})
   end
   p2 = Q*p1*quaternion.conj(Q) 
   return {p2[2],p2[3],p2[4]}
end

quaternion.toRot = function (Q)
   local mat = quaternion.lc_matrix
   local s = 1 / quaternion._norm_(Q)
   local w,i,j,k = Q[1],Q[2],Q[3],Q[4]
   return mat {
      {1-2*s*(j*j+k*k), 2*s*(i*j-k*w), 2*s*(i*j+j*w)},
      {2*s*(i*j+k*w), 1-2*s*(i*i+k*k), 2*s*(j*k-i*w)},
      {2*s*(i*k-j*w), 2*s*(j*k+i*w), 1-2*s*(i*i+j*j)}}
end

quaternion.__add = function (Q1,Q2)
   Q1,Q2 = quaternion._args_(Q1,Q2)
   return quaternion:new({Q1[1]+Q2[1],Q1[2]+Q2[2],Q1[3]+Q2[3],Q1[4]+Q2[4]})
end

quaternion.__sub = function (Q1,Q2)
   Q1,Q2 = quaternion._args_(Q1,Q2)
   return quaternion:new({Q1[1]-Q2[1],Q1[2]-Q2[2],Q1[3]-Q2[3],Q1[4]-Q2[4]})
end

quaternion.__unm = function (Q) return quaternion:new({-Q[1],-Q[2],-Q[3],-Q[4]}) end

quaternion.__mul = function (Q1,Q2)
   Q1,Q2 = quaternion._args_(Q1,Q2)
   return quaternion:new(
      {Q1[1]*Q2[1]-Q1[2]*Q2[2]-Q1[3]*Q2[3]-Q1[4]*Q2[4],
       Q1[1]*Q2[2]+Q1[2]*Q2[1]+Q1[3]*Q2[4]-Q1[4]*Q2[3],
       Q1[1]*Q2[3]-Q1[2]*Q2[4]+Q1[3]*Q2[1]+Q1[4]*Q2[2],
       Q1[1]*Q2[4]+Q1[2]*Q2[3]-Q1[3]*Q2[2]+Q1[4]*Q2[1]})
end

quaternion.eq = function (Q1,Q2)
   Q1,Q2 = quaternion._args_(Q1,Q2)
   return Q1[1]==Q2[1] and Q1[2]==Q2[2] and Q1[3]==Q2[3] and Q1[4]==Q2[4]
end

quaternion.__eq = quaternion.eq

quaternion.qw = function(Q) return Q[Q_w] end
quaternion.qi = function(Q) return Q[Q_i] end
quaternion.qj = function(Q) return Q[Q_j] end
quaternion.qk = function(Q) return Q[Q_k] end

quaternion._i = quaternion:new({0,1,0,0})
quaternion._j = quaternion:new({0,0,1,0})
quaternion._k = quaternion:new({0,0,0,1})

quaternion.__tostring = function (Q)
   return string.format("%.3f%+.3fi%+.3fj%+.3fk", Q[Q_w], Q[Q_i], Q[Q_j], Q[Q_k])
end

-- simplify constructor call
setmetatable(quaternion, 
{__call = function (self,v) 
   assert(type(v) == 'table')
   for i = 1,4 do v[i] = v[i] or 0 end
   return quaternion:new(v) end
})

quaternion.Quat = 'Quat'
quaternion.about[quaternion.Quat] = {"Quat(t)", "Create new quaternion.", help.NEW}

-- free memory in case of standalone usage
if not LC_DIALOG then quaternion.about = nil end

--return quaternion

--======================================
--TODO: should the inverse quaternion exists if the norm is zero?

--[[
a = quaternion.fromAA(0.5, {1,1,1})
b = quaternion.fromAA(0.3, {2,2,1})
ma = a:toRot()
mb = b:toRot()
c = a:conj() * b
print(c:toRot())
print(ma:T()*mb)
]]

