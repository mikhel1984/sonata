--[[		sonata/lib/rational.lua 

--- Rational number operations support.
--
--  Object structure: </br>
--  <code>{numerator,denominator}</code></br>
--  where both numbers are integers.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2022.

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
ans = a:float()               --> 0.5

-- make from decimal 
ans = Rat.from(0.25)          --> Rat(1,4)

-- numerator
ans = b:Nu()                  --> 2

-- denominator
ans = b:De()                  --> 1

-- make copy
ans = a:copy()                --> a

-- show
print(a)

-- result is rational 
ans = a + 1                   --> Rat(3,2)

-- result is float
ans = a + 0.5                 --> 1

--]]

--	LOCAL

local Ver = require("lib.utils")
local Cross = Ver.cross
Ver = Ver.versions

--  NUM, DENOM = 1, 2

--- Check object type.
--  @param v Test object.
--  @return True for rational number.
local function isrational(v) return type(v) == 'table' and v.isrational end

local function numrat(R) return Cross.eq(R[2],1) and Cross.simp(R[1]) or R end

--- Number representation.
--  @param v Value.
--  @return String representation.
local function numStr(v) 
  return type(v) == 'number' and string.format('%d', v) or tostring(v) 
end

--	INFO

local help = SonataHelp or {new=function () return {} end}
-- description
local about = help:new("Computations with rational numbers.")

--	MODULE

local rational = {
-- mark
type = 'rational', isrational = true,
-- simplification
_simp_ = numrat,
}

--- R1 + R2
--  @param R1 First rational or integer number.
--  @param R2 Second rational or integer number.
--  @return Sum.
rational.__add = function (R1, R2)  
  if not (isrational(R1) and isrational(R2)) then 
    local p = Cross.convert(R1, R2) 
    if p then 
      return R1 + p
    else
      p = Cross.convert(R2, R1) 
      return p and (p + R2) or (Cross.float(R1) + Cross.float(R2))
    end
  end
  return numrat(rational:_new_(R1[1]*R2[2]+R1[2]*R2[1], R1[2]*R2[2]))
end

--- R1 / R2
--  @param R1 First rational or integer number.
--  @param R2 Second rational or integer number.
--  @return Ratio.
rational.__div = function (R1, R2)
  if not (isrational(R1) and isrational(R2)) then 
    local p = Cross.convert(R1, R2) 
    if p then return R1 / p
    else
      p = Cross.convert(R2, R1) 
      return p and (p / R2) or (Cross.float(R1) / Cross.float(R2))
    end
  end
  return numrat(rational:_new_(R1[1]*R2[2], R1[2]*R2[1]))
end

-- methametods
rational.__index = rational

--- R1 <= R2
--  @param R1 First number.
--  @param R2 Second number.
--  @return True in the first value is less or equal then the second one.
rational.__le = function (R1,R2)
  if not (isrational(R1) and isrational(R2)) then 
    local p = Cross.convert(R1, R2) 
    if p then return R1 <= p
    else
      p = Cross.convert(R2, R1) 
      if p then return p <= R2 
      else 
        return Cross.float(R1) <= Cross.float(R2)
      end
    end
  end
  return (R1[1]*R2[2]) <= (R2[1]*R1[2])
end

--- R1 < R2
--  @param R1 First number.
--  @param R2 Second number.
--  @return True if the first number is less.
rational.__lt = function (R1,R2)
  if not (isrational(R1) and isrational(R2)) then 
    local p = Cross.convert(R1, R2) 
    if p then return R1 < p
    else
      p = Cross.convert(R2, R1) 
      if p then return p < R2 
      else 
        return Cross.float(R1) < Cross.float(R2)
      end
    end
  end
  return (R1[1]*R2[2]) < (R2[1]*R1[2])
end

--- R1 * R2
--  @param R1 First rational or integer number.
--  @param R2 Second rational or integer number.
--  @return Product.
rational.__mul = function (R1, R2)
  if not (isrational(R1) and isrational(R2)) then 
    local p = Cross.convert(R1, R2) 
    if p then return R1 * p
    else
      p = Cross.convert(R2, R1) 
      return p and (p * R2) or (Cross.float(R1) * Cross.float(R2))
    end
  end
  return numrat(rational:_new_(R1[1]*R2[1], R1[2]*R2[2]))
end

--- R1 ^ R2
--  @param R1 Rational or real number.
--  @param R2 Rational or real number.
--  @return Power value.
rational.__pow = function (R1, R2)
  R2 = Cross.float(R2)
  if type(R1) == "number" then
    return R1^R2
  else
    if not (Ver.isInteger(R2) and R2 >= 0) then error("Power must be a non-negative integer") end
    return numrat(rational:_new_(R1[1]^R2, R1[2]^R2))
  end
end

--- R1 - R2
--  @param R1 First rational or integer number.
--  @param R2 Second rational or integer number.
--  @return Difference.
rational.__sub = function (R1, R2)  
  if not (isrational(R1) and isrational(R2)) then 
    local p = Cross.convert(R1, R2) 
    if p then return R1 - p
    else
      p = Cross.convert(R2, R1) 
      return p and (p - R2) or (Cross.float(R1) - Cross.float(R2))
    end
  end
  return numrat(rational:_new_(R1[1]*R2[2]-R1[2]*R2[1], R1[2]*R2[2]))
end

--- String representation.
--  @param R Rational number.
--  @return String with numerator and denominator.
rational.__tostring = function (R) 
  if type(R[1]) == 'number' and type(R[2]) == 'number' and math.abs(R[1]) > R[2] then
    local n = math.abs(R[1])       -- numerator
    local v = math.modf(n / R[2]) 
    return string.format("%s%d %d/%d", R[1] < 0 and '-' or '', v, n % R[2], R[2])
  else
    return string.format("%s/%s", numStr(R[1]), numStr(R[2])) 
  end
end

--- -R
--  @param R Rational number.
--  @preturn Opposite rational number.
rational.__unm = function (R) return rational:_new_(-R[1], R[2]) end

rational.arithmetic = 'arithmetic'
about[rational.arithmetic] = {rational.arithmetic, "R1+R2, R1-R2, R1*R2, R1/R2, -R, R1^R2", help.META}

rational.comparison = 'comparison'
about[rational.comparison] = {rational.comparison, "R1<R2, R1<=R2, R1>R2, R1>=R2, R1==R2, R1~=R2", help.META}

rational._convert_ = function (v)
  return (type(v) == 'number' and Ver.isInteger(v) or type(v) == 'table' and v.__mod) 
         and rational:_new_(v,1)
end

--- Create new object, set metatable.
--  @param vn Numerator.
--  @param vd Denominator. Default is 1.
--  @return New rational object.
rational._new_ = function (self, vn, vd)
  local g = rational.gcd(vd,vn)         -- inverse order move sign to denominator
  return setmetatable({vn/g, vd/g}, self)  
end

--- Create copy of the rational number.
--  @param R Source value.
--  @return Rational number.
rational.copy = function (R) return 
  setmetatable({Cross.copy(R[1]), Cross.copy(R[2])}, rational) 
end
about[rational.copy] = {"copy(R)", "Get copy of the rational number.", help.OTHER}

--- Get denominator.
--  @param R Rational number.
--  @return Denominator.
rational.De = function (R) return R[2] end
about[rational.De] = {"De(R)", "Return the denominator of the rational number."}

--- R1 == R2
--  @param R1 First number.
--  @param R2 Second number.
--  @return True if the numbers are equal.
rational.eq = function (R1,R2)
  if not (isrational(R1) and isrational(R2)) then 
    local p = Cross.convert(R1, R2) 
    if p then return R1 == p
    else
      p = Cross.convert(R2, R1) 
      if p then return p == R2 
      else 
        return Cross.float(R1) == Cross.float(R2)
      end
    end
  end
  return Cross.eq(R1[1],R2[1]) and Cross.eq(R1[2],R2[2])
end
about[rational.eq] = {"eq(R1,R2)", "Compare two objects.", help.OTHER}
rational.__eq = rational.eq

--- Float point representation.
--  @param R Rational number.
--  @return Decimal fraction.
rational.float = function (R)
  return (R[1] < 0 and -1 or 1) * (Cross.norm(R[1]) / Cross.norm(R[2]))
end
about[rational.float] = {"float(R)", "Return rational number as decimal."}

--- Get rational number from floating point.
--  @param f Source number.
--  @param N Number of digits after coma.
--  @return Ratio estimation.
rational.from = function (f, N)
  N = N or 5   -- number of digits
  assert(N > 0)
  local den = math.pow(10, N)
  local int = math.modf(f * den) 
  return rational:_new_(int, den) 
end
about[rational.from] = {"from(F,[N=5])", "Estimate ratio from floating point value.", help.NEW}

--- The greatest common divisor. 
--  @param va First integer.
--  @param vb Second integer.
--  @return Greatest common divisor.
rational.gcd = function (va,vb)
  return Cross.eq(va,0) and vb or rational.gcd(vb % va, va)
end
about[rational.gcd] = {"gcd(va,vb)", "Calculate the greatest common divisor for two integers.", help.OTHER}

--- Get numerator.
--  @param R Rational number.
--  @return Numerator.
rational.Nu = function (R) return R[1] end
about[rational.Nu] = {"Nu(R)", "Return the numerator of rational number."}


-- call constructor, check arguments
setmetatable(rational, {
__call = function (self, n, d) 
  d = d or 1
  assert(type(n) == 'number' and Ver.isInteger(n) or type(n) == 'table' and n.__mod, 
         "Wrong numerator type")
  assert(type(d) == 'number' and Ver.isInteger(d) or type(d) == 'table' and d.__mod, 
         "Wrong denomenator type")
  assert(not Cross.eq(d,0), "Wrond denomenator value")
  return rational:_new_(n,d) 
end})
rational.Rat = 'Rat'
about[rational.Rat] = {"Rat(m,[n=1])", "Create rational number using num (and denom).", help.NEW}

-- Comment to remove descriptions
rational.about = about

return rational

--======================================
--TODO: use keys (num, den)
