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

quaternion._args_ = function (q1,q2)
   q1 = isquaternion(q1) and q1 or quaternion:new({q1,0,0,0})
   if q2 then
      q2 = isquaternion(q2) and q2 or quaternion:new({q2,0,0,0})
   end
   return q1, q2
end

--- Constructor example.
--  @param t Some value.
--  @return New object of quaternion.
quaternion.new = function(self,t)
   return setmetatable(t,self)
end

--- Method example.
--  It is good idea to define method for the copy creation.
--  @param t Initial object.
--  @return Copy of the object.
quaternion.copy = function (Q) return quaternion:new({Q[1],Q[2],Q[3],Q[4]}) end
quaternion.about[quaternion.copy] = {"copy(t)", "Create a copy of the quaternion."}

quaternion._norm_ = function (Q) return Q[1]*Q[1]+Q[2]*Q[2]+Q[3]*Q[3]+Q[4]*Q[4] end

quaternion.conj = function (Q) return quaternion:new({Q[1],-Q[2],-Q[3],-Q[4]}) end

quaternion.abs = function (Q) return math.sqrt(quaternion._norm_(Q)) end

quaternion.inv = function (Q) 
   local k = 1 / quaternion._norm_(Q)
   return quaternion:new({Q[1]*k, Q[2]*k, Q[3]*k, Q[4]*k})
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
