------------  rational.lua ----------------
--
-- Rational number operatons support.
--
-- This file is a part of liblc collection. 
-- Stanislav Mikhel, 2017.
----------------------------------------

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
]]
----------------------------------------------

local rational = {}
rational.__index = rational

rational.type = 'rational'

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
rational.about = help:new("Computations with rational numbers.")

local function isrational(v) return type(v) == 'table' and v.type == rational.type end

-- check if value can be an integer
local function isint (x)
   assert(type(x) == 'number', "Number is expected")
   local _,a = math.modf(x)
   return a == 0
end

-- greatest common devisor
rational.gcd = function (a,b)
   return (a == 0 or (type(a)=='table' and a:eq(0))) and b or rational.gcd(b % a, a)
end
rational.about[rational.gcd] = {"gcd(a,b)", "Calculate the greatest common devisor for two integers.", help.OTHER}

-- constructor
function rational:new(n, dn)
   dn = dn or 1
   local mn, mdn = getmetatable(n), getmetatable(dn)
   assert((type(n)=='number' and isint(n)) or (mn and mn.__div and mn.__mod), "Wrong number type")
   assert((type(dn)=='number' and isint(dn)) or (mdn and mdn.__div and mdn.__mod), "Wrong number type")
   local g = rational.gcd(n,dn)
   local o = {num=n/g, denom=dn/g}   
   setmetatable(o, self)
   return o
end

local function args(a,b)
   a = isrational(a) and a or rational:new(a)
   if b then
      b = isrational(b) and b or rational:new(b)
   end
   return a,b
end

-- a + b
rational.__add = function (a, b)   
   a,b = args(a,b)
   return rational:new(a.num*b.denom+a.denom*b.num, a.denom*b.denom)
end

-- a - b
rational.__sub = function (a, b)   
   a,b = args(a,b)
   return rational:new(a.num*b.denom-a.denom*b.num, a.denom*b.denom)
end

-- a * b
rational.__mul = function (a, b)
   a,b = args(a,b)
   return rational:new(a.num*b.num, a.denom*b.denom)
end

-- a / b
rational.__div = function (a, b)
   a,b = args(a,b)
   return rational:new(a.num*b.denom, a.denom*b.num)
end

-- -v
rational.__unm = function (v)
   return rational:new(-a.num, a.denom)
end

-- a^b
rational.__pow = function (a, b)
   b = (type(b) == "number") and b or (b.num/b.denom)  -- to float point
   if type(a) == "number" then
      return math.pow(a, b)
   else
      assert(isint(b) and b >= 0, "Power must be a nonnegative integer")
      return rational:new(math.pow(a.num, b), math.pow(a.denom, b)) 
   end
end

rational.arithmetic = 'arithmetic'
rational.about[rational.arithmetic] = {rational.arithmetic, "a+b, a-b, a*b, a/b, -a, a^b}", help.BASE}

-- a == b
rational.__eq = function (a,b)
   a,b = args(a,b)
   return a.num == b.num and a.denom == b.denom
end

-- a < b
rational.__lt = function (a,b)
   a,b = args(a,b)
   return (a.num*b.denom) < (b.num*a.denom)
end

-- a <= b
rational.__le = function (a,b)
   a,b = args(a,b)
   return (a.num*b.denom) <= (b.num*a.denom)
end

rational.comparation = 'comparation'
rational.about[rational.comparation] = {rational.comparation, "a<b, a<=b, a>b, a>=b, a==b, a~=b", help.BASE}

-- representation
local function numstr(v) return type(v) == 'number' and string.format('%d', v) or tostring(v) end

rational.__tostring = function (v)
   return numstr(v.num)..'/'..numstr(v.denom)
end

-- to float point
rational.decimal = function (v) return v.num / v.denom end
rational.about[rational.decimal] = {"decimal(v)", "Return rational number as decimal.", help.OTHER}

rational.Nu = function (v) return v.num end
rational.about[rational.Nu] = {"Nu(v)", "Return the numerator of rational number.", help.OTHER}

rational.De = function (v) return v.denom end
rational.about[rational.De] = {"De(v)", "Return the denominator of the rational number.", help.OTHER}

-- simplify constructor call
setmetatable(rational, {__call = function (self, n, d) return rational:new(n,d) end})
rational.Rat = 'Rat'
rational.about[rational.Rat] = {"Rat(m[,n])", "Create rational number using num (and denom).", help.NEW}

-- serialize rational number
rational.serialize = function (obj)
   local s = {}
   s[#s+1] = string.format("num=%d", obj.num)
   s[#s+1] = string.format("denom=%d", obj.denom)
   s[#s+1] = "metatablename='Rat'"
   s[#s+1] = "modulename='rational'"
   return string.format("{%s}", table.concat(s, ','))
end
rational.about[rational.serialize] = {"serialize(obj)", "Save internal representation of rational number.", help.OTHER}

return rational

