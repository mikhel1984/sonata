--[[      liblc/rational.lua 

--- Rational number operatons support.
--  @author <a href="mailto:vpsys@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

            module 'rational'
--]]

-------------------- Tests -------------------
--[[!!
Rat = require 'liblc.rational'

a = Rat(1,2)
b = Rat(2)
ans = b                 --> Rat(2,1)

k = 234781
ans = Rat(2*k,3*k)      --> Rat(2,3)

ans = a + b             --> Rat(5,2)

ans = 2 * a             --> Rat(1)

ans = Rat(2,3)*Rat(3,2) --> Rat(1)

ans = a / Rat(1,3)      --> Rat(3,2)

ans = a ^ 3             --> Rat(1,8)

ans = 2 ^ a             --~ 1.4142

ans = (b == b)          --> true

ans = (a >= b)          --> false

ans = Rat.gcd(125,65)   --> 5

ans = a:decimal()       --> 0.5

ans = b:Nu()            --> 2

ans = b:De()            --> 1

ans = a:copy()          --> a

print(a)
]]

-------------------------------------------- 
-- @class table
-- @name rational
-- @field type Define object type string.
-- @field about Function description collection.
local rational = {}
rational.__index = rational
-- mark
rational.type = 'rational'
rational.isrational = true
-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
rational.about = help:new("Computations with rational numbers.")

--- Check object type.
--    <i>Private function.</i>
--    @param v Object for checking.
--    @return True if table is a rational number.
local function isrational(v) return type(v) == 'table' and v.isrational end

--- Check if value can be an integer.
--    <i>Private function.</i>
--    @param x Number.
--    @return <code>true</code> if the number doesn't have a fractional part.
local function isint (x)
   assert(type(x) == 'number', "Number is expected")
   local _,a = math.modf(x)
   return a == 0
end

--- The greatest common deviser.
--    @param a First integer.
--    @param b Second integer.
--    @return Greatest common deviser.
rational.gcd = function (a,b)
   return (a == 0 or (type(a)=='table' and a:eq(0))) and b or rational.gcd(b % a, a)
end
rational.about[rational.gcd] = {"gcd(a,b)", "Calculate the greatest common deviser for two integers.", help.OTHER}

--- Create new object, set metatable.
--    @param n Numerator.
--    @param dn Denominator. Default is 1.
--    @return New rational object.
function rational:new(n, dn)
   dn = dn or 1
   local g = rational.gcd(n,dn)
   local o = {num=n/g, denom=dn/g}   
   setmetatable(o, self)
   return o
end

--- Create copy of the rational number.
--    @param v Source value.
--    @return Rational number.
rational.copy = function (v)
   return rational:new(v.num, v.denom)
end
rational.about[rational.copy] = {"copy(v)", "Get copy of the rational number.", help.OTHER}

--- Argument type correction.
--    <i>Private function.</i>
--    @param a First rational or natural number.
--    @param b Second rational or natural number.
--    @return Arguments as rational numbers.
local function args(a,b)
   a = isrational(a) and a or rational:new(a)
   if b then
      b = isrational(b) and b or rational:new(b)
   end
   return a,b
end

--- a + b
--    @param a First rational or integer number.
--    @param b Second rational or integer number.
--    @return Sum of the rational numbers.
rational.__add = function (a, b)   
   a,b = args(a,b)
   return rational:new(a.num*b.denom+a.denom*b.num, a.denom*b.denom)
end

--- a - b
--    @param a First rational or integer number.
--    @param b Second rational or integer number.
--    @return Subtraction of the rational numbers.
rational.__sub = function (a, b)   
   a,b = args(a,b)
   return rational:new(a.num*b.denom-a.denom*b.num, a.denom*b.denom)
end

--- a * b
--    @param a First rational or integer number.
--    @param b Second rational or integer number.
--    @return Multiplication of the rational numbers.
rational.__mul = function (a, b)
   a,b = args(a,b)
   return rational:new(a.num*b.num, a.denom*b.denom)
end

--- a / b
--    @param a First rational or integer number.
--    @param b Second rational or integer number.
--    @return Subtraction of the rational numbers.
rational.__div = function (a, b)
   a,b = args(a,b)
   return rational:new(a.num*b.denom, a.denom*b.num)
end

--- - v
--    @param v Rational number
--    @return Negative value.
rational.__unm = function (v)
   return rational:new(-a.num, a.denom)
end

--- a ^ b
--    @param a First number.
--    @param b Second number.
--    @return Power.
rational.__pow = function (a, b)
   b = (type(b) == "number") and b or (b.num/b.denom)  -- to float point
   if type(a) == "number" then
      return math.pow(a, b)
   else
      assert(isint(b) and b >= 0, "Power must be a non-negative integer")
      return rational:new(math.pow(a.num, b), math.pow(a.denom, b)) 
   end
end

rational.arithmetic = 'arithmetic'
rational.about[rational.arithmetic] = {rational.arithmetic, "a+b, a-b, a*b, a/b, -a, a^b} ", help.BASE}

--- a == b
--    @param a First rational number.
--    @param b Second rational number.
--    @return <code>true</code> In case of equality.
rational.__eq = function (a,b)
   a,b = args(a,b)
   return a.num == b.num and a.denom == b.denom
end

--- a < b
--    @param a First rational number.
--    @param b Second rational number.
--    @return <code>true</code> if relation is right.
rational.__lt = function (a,b)
   a,b = args(a,b)
   return (a.num*b.denom) < (b.num*a.denom)
end

--- a <= b
--    @param a First rational number.
--    @param b Second rational number.
--    @return <code>true</code> if relation is right.
rational.__le = function (a,b)
   a,b = args(a,b)
   return (a.num*b.denom) <= (b.num*a.denom)
end

rational.comparison = 'comparison'
rational.about[rational.comparison] = {rational.comparison, "a<b, a<=b, a>b, a>=b, a==b, a~=b ", help.BASE}

--- Number representation.
--    <i>Private function.</i>
--    @param v Variable.
--    @return String representation.
local function numstr(v) return type(v) == 'number' and string.format('%d', v) or tostring(v) end

--- String representation.
--    @param v Rational number.
--    @return String form.
rational.__tostring = function (v)
   return numstr(v.num)..'/'..numstr(v.denom)
end

--- Float point representation.
--    @param v Rational number.
--    @return Decimal fraction.
rational.decimal = function (v) return v.num / v.denom end
rational.about[rational.decimal] = {"decimal(v)", "Return rational number as decimal.", help.OTHER}

--- Get numerator.
--    @param v Rational number.
--    @return Numerator.
rational.Nu = function (v) return v.num end
rational.about[rational.Nu] = {"Nu(v)", "Return the numerator of rational number.", help.OTHER}

--- Get denominator.
--    @param v Rational number.
--    @return Denominator.
rational.De = function (v) return v.denom end
rational.about[rational.De] = {"De(v)", "Return the denominator of the rational number.", help.OTHER}

-- list of prime numbers
-- result is not sorted
rational.prime = function (v)
   assert(v > 0 and isint(v), "Positive integer is expected!")
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

-- simplify constructor call
setmetatable(rational, {__call = function (self, n, d) return rational:new(n,d) end})
rational.Rat = 'Rat'
rational.about[rational.Rat] = {"Rat(m[,n])", "Create rational number using num (and denom).", help.NEW}

--- Rational number serialization.
--    @param obj Rational number.
--    @return String, suitable for exchange.
rational.serialize = function (obj)
   local s = {}
   s[#s+1] = string.format("num=%d", obj.num)
   s[#s+1] = string.format("denom=%d", obj.denom)
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
