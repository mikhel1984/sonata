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
ans = b:num()                 --> 2

-- denominator
ans = b:denom()               --> 1

-- show
print(a)

-- result is rational 
ans = a + 1                   --> Rat(3,2)

-- result is float
ans = a + 0.5                 --> 1

--]]

--	LOCAL

local Ver = require("lib.utils")
local Utils = Ver.utils
local Cross = Ver.cross
Ver = Ver.versions

--  NUM, DENOM = 1, 2

--- Check object type.
--  @param v Test object.
--  @return True for rational number.
local function isrational(v) return type(v) == 'table' and v.isrational end

local mabs = math.abs

local function isbig(v) return type(v) == 'number' and mabs(v) > 1E10 end

local function numrat(R) 
  return Cross.eq(R._v[2],1) and Cross.simp(R._v[1])               -- x / 1
    or (isbig(R._v[1]) or isbig(R._v[2])) and (R._v[1] / R._v[2])  -- float num or denom
    or R 
end

--- Number representation.
--  @param v Value.
--  @return String representation.
local function numStr(v)
  return type(v) == 'number' and Utils.numstr(v) or tostring(v) 
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
  local r1, r2 = R1._v, R2._v
  return numrat(rational:_new_(r1[1]*r2[2]+r1[2]*r2[1], r1[2]*r2[2]))
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
  local r1, r2 = R1._v, R2._v
  return numrat(rational:_new_(r1[1]*r2[2], r1[2]*r2[1]))
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
  local r1, r2 = R1._v, R2._v
  return (r1[1]*r2[2]) <= (r2[1]*r1[2])
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
  local r1, r2 = R1._v, R2._v
  return (r1[1]*r2[2]) < (r2[1]*r1[2])
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
  local r1, r2 = R1._v, R2._v
  return numrat(rational:_new_(r1[1]*r2[1], r1[2]*r2[2]))
end

--- "Protect" from table modification.
rational.__newindex = function () error("Immutable object") end

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
    local r1 = R1._v
    return numrat(rational:_new_(r1[1]^R2, r1[2]^R2))
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
  local r1, r2 = R1._v, R2._v
  return numrat(rational:_new_(r1[1]*r2[2]-r1[2]*r2[1], r1[2]*r2[2]))
end

--- String representation.
--  @param R Rational number.
--  @return String with numerator and denominator.
rational.__tostring = function (R) 
  local r = R._v
  if type(r[1]) == 'number' and type(r[2]) == 'number' and mabs(r[1]) > r[2] then
    local n = mabs(r[1])       -- numerator
    local v = math.modf(n / r[2]) 
    return string.format("%s%d %d/%d", r[1] < 0 and '-' or '', v, n % r[2], r[2])
  else
    return string.format("%s/%s", numStr(r[1]), numStr(r[2])) 
  end
end

--- -R
--  @param R Rational number.
--  @preturn Opposite rational number.
rational.__unm = function (R) return rational:_new_(-R._v[1], R._v[2]) end

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
  return setmetatable({_v = {vn/g, vd/g}}, self)  
end

--- Get denominator.
--  @param R Rational number.
--  @return Denominator.
rational.denom = function (R) return R._v[2] end
about[rational.denom] = {"denom(R)", "Return the denominator of the rational number."}

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
  local r1, r2 = R1._v, R2._v
  return Cross.eq(r1[1],r2[1]) and Cross.eq(r1[2],r2[2])
end
about[rational.eq] = {"eq(R1,R2)", "Compare two objects.", help.OTHER}
rational.__eq = rational.eq

--- Float point representation.
--  @param R Rational number.
--  @return Decimal fraction.
rational.float = function (R)
  local r = R._v
  return (r[1] < 0 and -1 or 1) * (Cross.norm(r[1]) / Cross.norm(r[2]))
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
rational.num = function (R) return R._v[1] end
about[rational.num] = {"num(R)", "Return the numerator of rational number."}


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
