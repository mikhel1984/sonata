
local rational = require "rational"

local bigint = {}
bigint.__index = bigint

function bigint:new(a)
   assert(rational.isint(a), "Integer number is expected")
   local o = {}
   o[0] = (a < 0) and '-' or '+'
   a = math.abs(a)
   local i = 1
   repeat 
      o[i] = math.floor(a % 10)
      a = math.floor(a / 10)
      i = i + 1
   until a <= 0
   setmetatable(o, self)
   return o
end

bigint.__tostring = function (v) 
   local str = table.concat(v)
   str = string.reverse(str)
   return (v[0] == '+') and str or ('-'..str)
end

t = bigint:new(123456)
print(t)
