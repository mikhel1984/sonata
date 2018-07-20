--[[      liblc/rational.lua 

--- Rational number operatons support.
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

            module 'rational'
--]]

-------------------- Tests -------------------
--[[!!
-- import 'rational'
Rat = require 'liblc.rational'

-- numerator, denominator
a = Rat(1,2)
-- only numerator
b = Rat(2)
ans = b                 --> Rat(2,1)

-- simplification
k = 234781
ans = Rat(2*k,3*k)      --> Rat(2,3)

-- arithmetic
ans = a + b             --> Rat(5,2)

ans = 2 * a             --> Rat(1)

ans = Rat(2,3)*Rat(3,2) --> Rat(1)

ans = a / Rat(1,3)      --> Rat(3,2)

ans = a ^ 3             --> Rat(1,8)

ans = 2 ^ a             --~ 1.4142

-- comparison
ans = (b == b)          --> true

ans = (a >= b)          --> false

-- greatest common division
ans = Rat.gcd(125,65)   --> 5

-- represent as decimal
ans = a:decimal()       --> 0.5

-- numerator
ans = b:Nu()            --> 2

-- denominator
ans = b:De()            --> 1

-- make copy
ans = a:copy()          --> a

-- show
print(a)
]]

--	LOCAL

local Ver = require('liblc.versions')

local NUM, DENOM = 1, 2

-- Check object type.
local function isrational(v) return type(v) == 'table' and v.isrational end

-- Number representation.
--    @param v Variable.
--    @return String representation.
local function numStr(v) return type(v) == 'number' and string.format('%d', v) or tostring(v) end

--	INFO

local help = lc_version and (require "liblc.help") or {new=function () return {} end}

--	MODULE

local rational = {
-- mark
type = 'rational', isrational = true,
-- description
about = help:new("Computations with rational numbers."),
}
rational.__index = rational

--- The greatest common divisor. 
--    @param a First integer.
--    @param b Second integer.
--    @return Greatest common divisor.
rational.gcd = function (a,b)
   return (a == 0 or (type(a)=='table' and a:eq(0))) and b or rational.gcd(b % a, a)
end
rational.about[rational.gcd] = {"gcd(a,b)", "Calculate the greatest common divisor for two integers.", help.OTHER}

--- Create new object, set metatable.
--    @param n Numerator.
--    @param dn Denominator. Default is 1.
--    @return New rational object.
rational.new = function (self, n, dn)
   dn = dn or 1
   local g = rational.gcd(n,dn)
   return setmetatable({n/g, dn/g}, self)   
end

--- Create copy of the rational number.
--    @param v Source value.
--    @return Rational number.
rational.copy = function (v)
   return rational:new(v[NUM], v[DENOM])
end
rational.about[rational.copy] = {"copy(v)", "Get copy of the rational number.", help.OTHER}

-- Argument type correction.
--    @param a First rational or natural number.
--    @param b Second rational or natural number.
--    @return Arguments as rational numbers.
rational._args = function (a,b)
   a = isrational(a) and a or rational:new(a)
   if b then
      b = isrational(b) and b or rational:new(b)
   end
   return a,b
end

-- a + b
rational.__add = function (a, b)   
   a,b = rational._args(a,b)
   return rational:new(a[1]*b[2]+a[2]*b[1], a[2]*b[2])
end

-- a - b
rational.__sub = function (a, b)   
   a,b = rational._args(a,b)
   return rational:new(a[1]*b[2]-a[2]*b[1], a[2]*b[2])
end

-- a * b
rational.__mul = function (a, b)
   a,b = rational._args(a,b)
   return rational:new(a[1]*b[1], a[2]*b[2])
end

-- a / b
rational.__div = function (a, b)
   a,b = rational._args(a,b)
   return rational:new(a[1]*b[2], a[2]*b[1])
end

-- - v
rational.__unm = function (v)
   return rational:new(-a[1], a[2])
end

-- a ^ b
rational.__pow = function (a, b)
   b = (type(b) == "number") and b or (b[1]/b[2])  -- to float point
   if type(a) == "number" then
      return a^b
   else
      if not (Ver.isInteger(b) and b >= 0) then error("Power must be a non-negative integer") end
      return rational:new((a[1])^b, (a[2])^b) 
   end
end

rational.arithmetic = 'arithmetic'
rational.about[rational.arithmetic] = {rational.arithmetic, "a+b, a-b, a*b, a/b, -a, a^b} ", help.META}

-- a == b
rational.__eq = function (a,b)
   a,b = rational._args(a,b)
   return a[1] == b[1] and a[2] == b[2]
end

-- a < b
rational.__lt = function (a,b)
   a,b = rational._args(a,b)
   return (a[1]*b[2]) < (b[1]*a[2])
end

-- a <= b
rational.__le = function (a,b)
   a,b = rational._args(a,b)
   return (a[1]*b[2]) <= (b[1]*a[2])
end

rational.comparison = 'comparison'
rational.about[rational.comparison] = {rational.comparison, "a<b, a<=b, a>b, a>=b, a==b, a~=b ", help.META}

-- String representation.
rational.__tostring = function (v)
   return numStr(v[NUM])..'/'..numStr(v[DENOM])
end

--- Float point representation.
--    @param v Rational number.
--    @return Decimal fraction.
rational.decimal = function (v) return v[NUM] / v[DENOM] end
rational.about[rational.decimal] = {"decimal(v)", "Return rational number as decimal."}

--- Get numerator.
--    @param v Rational number.
--    @return Numerator.
rational.Nu = function (v) return v[NUM] end
rational.about[rational.Nu] = {"Nu(v)", "Return the numerator of rational number."}

--- Get denominator.
--    @param v Rational number.
--    @return Denominator.
rational.De = function (v) return v[DENOM] end
rational.about[rational.De] = {"De(v)", "Return the denominator of the rational number."}

-- list of prime numbers
-- result is not sorted
--[[ for future
rational.prime = function (v)
   if not (v > 0 and Ver.isInteger(v)) then error("Positive integer is expected!") end
   -- use "sieve of Eratosthenes"
   local tmp = {}
   for i = 1,v,2 do tmp[i] = true end
   -- remove non prime
   for i = 3,math.floor(math.sqrt(v)),2 do
      if tmp[i] then
         for j = i+i,v,i do 
	    if tmp[j] then tmp[j] = false end
	 end
      end
   end
   if v > 1 then tmp[2] = true end
   -- collect
   local res = {}
   for k,v in pairs(tmp) do
      if v then res[#res+1] = k end
   end
   return res
end
]]

-- simplify constructor call
setmetatable(rational, {__call = function (self, n, d) return rational:new(n,d) end})
rational.Rat = 'Rat'
rational.about[rational.Rat] = {"Rat(m[,n])", "Create rational number using num (and denom).", help.NEW}

--- Rational number serialization.
--    @param obj Rational number.
--    @return String, suitable for exchange.
rational.serialize = function (obj)
   local s = {}
   s[#s+1] = tostring(obj[NUM])
   s[#s+1] = tostring(obj[DENOM])
   s[#s+1] = "metatablename='Rat'"
   s[#s+1] = "modulename='rational'"
   return string.format("{%s}", table.concat(s, ','))
end
rational.about[rational.serialize] = {"serialize(obj)", "Save internal representation of rational number.", help.OTHER}

-- free memory if need
if not lc_version then rational.about = nil end

return rational

--======================================
-- TODO: create module for calculations with integer numbers, add prime and gcd
