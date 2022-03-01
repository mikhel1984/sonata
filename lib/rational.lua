--[[		sonata/lib/rational.lua 

--- Rational number operations support.
--  
--  Object structure: </br>
--  <code>{numerator,denominator}</code></br>
--  where both numbers are integers.
--  
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2021.

	module 'rational'
--]]

-------------------- Tests -------------------
--[[TEST

-- use 'rational'
Rat = require 'lib.rational'

-- numerator, denominator
a = Rat(1,2)
-- only numerator
b = Rat(2)
ans = b                       --> Rat(2,1)

-- simplification
k = 234781
ans = Rat(2*k,3*k)            --> Rat(2,3)

-- arithmetic
ans = a + b                   --> Rat(5,2)

ans = 2 * a                   --> 1

ans = Rat(2,3)*Rat(3,2)       --> 1

ans = a / Rat(1,3)            --> Rat(3,2)

ans = a ^ 3                   --> Rat(1,8)

ans = 2 ^ a                  --3> 1.414

-- comparison
ans = (b == b)                --> true

ans = (a >= b)                --> false

-- greatest common division
ans = Rat.gcd(125,65)         --> 5

-- represent as decimal
ans = a:val()                 --> 0.5

-- numerator
ans = b:Nu()                  --> 2

-- denominator
ans = b:De()                  --> 1

-- make copy
ans = a:copy()                --> a

-- show
print(a)

--]]

--	LOCAL

local Ver = require("lib.utils").versions

--  NUM, DENOM = 1, 2

--- Check object type.
--  @param v Test object.
--  @return True for rational number.
local function isrational(v) return type(v) == 'table' and v.isrational end

local function numrat(R) return R[2] == 1 and R[1] or R end

--- Number representation.
--  @param v Value.
--  @return String representation.
local function numStr(v) 
  return type(v) == 'number' and string.format('%d', v) or tostring(v) 
end

--	INFO

local help = SonataHelp and (require "core.help") or {new=function () return {} end}

--	MODULE

local rational = {
-- mark
type = 'rational', isrational = true,
-- description
about = help:new("Computations with rational numbers."),
}
rational.__index = rational

--- The greatest common divisor. 
--  @param va First integer.
--  @param vb Second integer.
--  @return Greatest common divisor.
rational.gcd = function (va,vb)
  return (va == 0 or (type(va)=='table' and va:eq(0))) and vb or rational.gcd(vb % va, va)
end
rational.about[rational.gcd] = {"gcd(va,vb)", "Calculate the greatest common divisor for two integers.", help.OTHER}

--- Create new object, set metatable.
--  @param vn Numerator.
--  @param vd Denominator. Default is 1.
--  @return New rational object.
rational._new_ = function (self, vn, vd)
  vd = vd or 1
  local g = rational.gcd(vd,vn)         -- inverse order move sign to denominator
  return setmetatable({vn/g, vd/g}, self)  
end

--- Create copy of the rational number.
--  @param R Source value.
--  @return Rational number.
rational.copy = function (R) return setmetatable({R[1], R[2]}, rational) end
rational.about[rational.copy] = {"copy(R)", "Get copy of the rational number.", help.OTHER}

--- Argument type correction.
--  @param a First rational or natural number.
--  @param b Second rational or natural number.
--  @return Arguments as rational numbers.
rational._args_ = function (a,b)
  a = isrational(a) and a or rational:_new_(a)
  if b then
    b = isrational(b) and b or rational:_new_(b)
  end
  return a,b
end

--- R1 + R2
--  @param R1 First rational or integer number.
--  @param R2 Second rational or integer number.
--  @return Sum.
rational.__add = function (R1, R2)  
  R1,R2 = rational._args_(R1,R2)
  return numrat(rational:_new_(R1[1]*R2[2]+R1[2]*R2[1], R1[2]*R2[2]))
end

--- R1 - R2
--  @param R1 First rational or integer number.
--  @param R2 Second rational or integer number.
--  @return Difference.
rational.__sub = function (R1, R2)  
  R1,R2 = rational._args_(R1,R2)
  return numrat(rational:_new_(R1[1]*R2[2]-R1[2]*R2[1], R1[2]*R2[2]))
end

--- R1 * R2
--  @param R1 First rational or integer number.
--  @param R2 Second rational or integer number.
--  @return Product.
rational.__mul = function (R1, R2)
  R1,R2 = rational._args_(R1,R2)
  return numrat(rational:_new_(R1[1]*R2[1], R1[2]*R2[2]))
end

--- R1 / R2
--  @param R1 First rational or integer number.
--  @param R2 Second rational or integer number.
--  @return Ratio.
rational.__div = function (R1, R2)
  R1,R2 = rational._args_(R1,R2)
  return numrat(rational:_new_(R1[1]*R2[2], R1[2]*R2[1]))
end

--- -R
--  @param R Rational number.
--  @preturn Opposite rational number.
rational.__unm = function (R) return rational:_new_(-R[1], R[2]) end

--- R1 ^ R2
--  @param R1 Rational or real number.
--  @param R2 Rational or real number.
--  @return Power value.
rational.__pow = function (R1, R2)
  R2 = (type(R2) == "number") and R2 or (R2[1]/R2[2])  -- to float point
  if type(R1) == "number" then
    return R1^R2
  else
    if not (Ver.isInteger(R2) and R2 >= 0) then error("Power must be a non-negative integer") end
    return numrat(rational:_new_((R1[1])^R2, (R1[2])^R2))
  end
end

rational.arithmetic = 'arithmetic'
rational.about[rational.arithmetic] = {rational.arithmetic, "R1+R2, R1-R2, R1*R2, R1/R2, -R, R1^R2} ", help.META}

--- R1 == R2
--  @param R1 First number.
--  @param R2 Second number.
--  @return True if the numbers are equal.
rational.__eq = function (R1,R2)
  R1,R2 = rational._args_(R1,R2)
  return R1[1] == R2[1] and R1[2] == R2[2]
end

--- R1 < R2
--  @param R1 First number.
--  @param R2 Second number.
--  @return True if the first number is less.
rational.__lt = function (R1,R2)
  R1,R2 = rational._args_(R1,R2)
  return (R1[1]*R2[2]) < (R2[1]*R1[2])
end

--- R1 <= R2
--  @param R1 First number.
--  @param R2 Second number.
--  @return True in the first value is less or equal then the second one.
rational.__le = function (R1,R2)
  R1,R2 = rational._args_(R1,R2)
  return (R1[1]*R2[2]) <= (R2[1]*R1[2])
end

rational.comparison = 'comparison'
rational.about[rational.comparison] = {rational.comparison, "R1<R2, R1<=R2, R1>R2, R1>=R2, R1==R2, R1~=R2", help.META}

--- String representation.
--  @param R Rational number.
--  @return String with numerator and denominator.
rational.__tostring = function (R) 
  return string.format("%s/%s", numStr(R[1]), numStr(R[2])) 
end

--- Float point representation.
--  @param R Rational number.
--  @return Decimal fraction.
rational.val = function (R) return R[1] / R[2] end
rational.about[rational.val] = {"val(R)", "Return rational number as decimal."}

--- Get numerator.
--  @param R Rational number.
--  @return Numerator.
rational.Nu = function (R) return R[1] end
rational.about[rational.Nu] = {"Nu(R)", "Return the numerator of rational number."}

--- Get denominator.
--  @param R Rational number.
--  @return Denominator.
rational.De = function (R) return R[2] end
rational.about[rational.De] = {"De(R)", "Return the denominator of the rational number."}

-- simplify constructor call
setmetatable(rational, {__call = function (self, n, d) return rational:_new_(n,d) end})
rational.Rat = 'Rat'
rational.about[rational.Rat] = {"Rat(m[,n=1])", "Create rational number using num (and denom).", help.NEW}

-- Uncomment to remove descriptions
--rational.about = nil

return rational

--======================================
--TODO: string representation as true ratio (str)
--TODO: use keys (num, den)
