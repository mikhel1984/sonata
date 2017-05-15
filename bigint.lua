
local rational = require "rational"

local bigint = {}
bigint.__index = bigint

local function iabs(v) return (v < 0) and (-v) or v end
local function nval(b, n) return b[n] and (b[0] < 0 and -b[n] or b[n]) or 0 end

function bigint:new(a)
   assert(rational.isint(a), "Integer number is expected")
   local o = {}
   o[0] = (a < 0) and -1 or 1
   a = iabs(a)
   local i = 1
   repeat 
      o[i], a = math.floor(a % 10), math.floor(a/10)
      i = i + 1
   until a == 0
   setmetatable(o, self)
   return o
end

bigint.copy = function (v)
   local c = bigint:new(v[0])
   for i = 1, #v do
      c[i] = v[i]
   end
   return c
end

bigint.__add = function (a, b)
   a = (type(a) == "number") and bigint:new(a) or a
   b = (type(b) == "number") and bigint:new(b) or b
   local s = math.max(#a, #b)
   local sum = bigint:new(0)
   local d, ci = 0, 0
   for i = 1, s do
      ci = nval(a, i) + nval(b, i) + d
      if ci >= 10 then
         sum[i] = ci - 10
	 d = 1 
      elseif ci <= -10 then 
         sum[i] = -ci - 10
	 d = -1
      else
         sum[i] = iabs(ci)
	 d = 0
      end
   end
   sum[0] = (ci < 0) and -1 or 1
   if d ~= 0 then sum[s+1] = 1 end
   return sum
end

bigint.__sub = function (a,b)
   b = (type(b) == "number") and bigint:new(b) or b
   return bigint.__add(a, -b)
end

bigint.__unm = function (v)
   local new = bigint.copy(v)
   new[0] = -new[0] 
   return new
end

bigint.__tostring = function (v) 
   local str = table.concat(v)
   str = string.reverse(str)
   return (v[0] > 0) and str or ('-'..str)
end

t = bigint:new(123456)
print(t)

a = bigint:new(-789)
b = bigint:new(456)

c = a - b
print(c)
