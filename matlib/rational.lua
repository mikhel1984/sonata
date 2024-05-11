--[[		sonata/lib/rational.lua

--- Rational number operations support.
--
--  Object structure: </br>
--  <code>{numerator,denominator}</code></br>
--  where both numbers are integers.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2024.

	module 'rational'
--]]


-------------------- Tests -------------------
--[[TEST_IT

-- use 'rational'
Rat = require 'matlib.rational'

-- numerator, denominator
a = Rat(1,2)
-- only numerator
b = Rat(2)
ans = b                       -->  Rat(2,1)

-- simplification
k = 234781
ans = Rat(2*k,3*k)            -->  Rat(2,3)

-- arithmetic
ans = a + b                   -->  Rat(5,2)

ans = 2 * a                   -->  1

ans = Rat(2,3)*Rat(3,2)       -->  1

ans = a / Rat(1,3)            -->  Rat(3,2)

ans = a ^ 3                   -->  Rat(1,8)

ans = 2 ^ a                  --3>  1.414

-- comparison
ans = (b == b)                -->  true

ans = (a >= b)                -->  false

-- represent as decimal
ans = a:float()               -->  0.5

-- from decimal
ans = Rat:from(math.pi)       -->  Rat(333, 106)

-- numerator
ans = b:num()                 -->  2

-- denominator
ans = b:denom()               -->  1

-- show
print(a)

-- continued fraction to rational
-- 1 + 1/(2+1/(3+1/4))
c = Rat:fromCF {[0]=1, 2, 3, 4}
ans = c                       -->  Rat(43,30)

-- rational to continued fraction
d = c:toCF()
ans = d[1]                    -->  2

-- show continued fraction
print(d)

-- result is rational
ans = a + 1                   -->  Rat(3,2)

-- result is float
ans = a + 0.5                 -->  1

--]]

--	LOCAL

local Vinteger, Unumstr, Cross do
  local lib = require("matlib.utils")
  Vinteger = lib.versions.isInteger
  Unumstr = lib.utils.numstr
  Cross = lib.cross
end
local mabs = math.abs


--- Check if the element is huge
--  @param v Variable.
--  @return true for big input.
local function isbig(v) return type(v) == 'number' and mabs(v) > 1E10 end


--- Simplify rational when possible.
--  @param R Rational number.
--  @return same object or some number
local function numrat(R)
  local a, b = R._[1], R._[2]
  return Cross.eq(b, 1) and Cross.simp(a)               -- x / 1
    -- float num or denom
    or (isbig(a) or isbig(b)) and (a / b)
    or R
end


--- Number representation.
--  @param v Value.
--  @return String representation.
local function numStr(v)
  return type(v) == 'number' and Unumstr(v) or tostring(v)
end


-- Continued fraction printing
local mt_continued = {
__tostring = function (t)
  local res = {tostring(t[0])}
  for i = 1, #t do
    res[#res+1] = '+L'..tostring(t[i])
  end
  return string.format("{%s}", table.concat(res))
end
}


--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Computations with rational numbers."
}

local CONTINUATED = 'continuated frac'


--	MODULE

local rational = {
-- mark
type = 'rational',
-- simplification
_simp = numrat,
-- print format
MIXED = false,
}


--- Check object type.
--  @param v Test object.
--  @return True for rational number.
local function isrational(v) return getmetatable(v) == rational end


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
  local r1, r2 = R1._, R2._
  return numrat(rational._new(r1[1]*r2[2]+r1[2]*r2[1], r1[2]*r2[2]))
end


--- R1 / R2
--  @param R1 First rational or integer number.
--  @param R2 Second rational or integer number.
--  @return Ratio.
rational.__div = function (R1, R2)
  if not (isrational(R1) and isrational(R2)) then
    local p = Cross.convert(R1, R2)
    if p then
      return R1 / p
    else
      p = Cross.convert(R2, R1)
      return p and (p / R2) or (Cross.float(R1) / Cross.float(R2))
    end
  end
  local r1, r2 = R1._, R2._
  return numrat(rational._new(r1[1]*r2[2], r1[2]*r2[1]))
end


--- R1 == R2
--  @param R1 First number.
--  @param R2 Second number.
--  @return True if the numbers are equal.
rational.__eq = function (R1, R2)
  if not (isrational(R1) and isrational(R2)) then
    local p = Cross.convert(R1, R2)
    if p then
      return R1 == p
    else
      p = Cross.convert(R2, R1)
      if p then
        return p == R2
      else
        return Cross.float(R1) == Cross.float(R2)
      end
    end
  end
  local r1, r2 = R1._, R2._
  return Cross.eq(r1[1], r2[1]) and Cross.eq(r1[2], r2[2])
end


-- methametods
rational.__index = rational


--- R1 <= R2
--  @param R1 First number.
--  @param R2 Second number.
--  @return True in the first value is less or equal then the second one.
rational.__le = function (R1, R2)
  if not (isrational(R1) and isrational(R2)) then
    local p = Cross.convert(R1, R2)
    if p then
      return R1 <= p
    else
      p = Cross.convert(R2, R1)
      if p then
        return p <= R2
      else
        return Cross.float(R1) <= Cross.float(R2)
      end
    end
  end
  local r1, r2 = R1._, R2._
  return (r1[1]*r2[2]) <= (r2[1]*r1[2])
end


--- R1 < R2
--  @param R1 First number.
--  @param R2 Second number.
--  @return True if the first number is less.
rational.__lt = function (R1, R2)
  if not (isrational(R1) and isrational(R2)) then
    local p = Cross.convert(R1, R2)
    if p then
      return R1 < p
    else
      p = Cross.convert(R2, R1)
      if p then
        return p < R2
      else
        return Cross.float(R1) < Cross.float(R2)
      end
    end
  end
  local r1, r2 = R1._, R2._
  return (r1[1]*r2[2]) < (r2[1]*r1[2])
end


--- R1 * R2
--  @param R1 First rational or integer number.
--  @param R2 Second rational or integer number.
--  @return Product.
rational.__mul = function (R1, R2)
  if not (isrational(R1) and isrational(R2)) then
    local p = Cross.convert(R1, R2)
    if p then
      return R1 * p
    else
      p = Cross.convert(R2, R1)
      return p and (p * R2) or (Cross.float(R1) * Cross.float(R2))
    end
  end
  local r1, r2 = R1._, R2._
  return numrat(rational._new(r1[1]*r2[1], r1[2]*r2[2]))
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
    if not Vinteger(R2) then
      error "Power must be integer"
    end
    local r1 = R1._
    if R2 >= 0 then
      return numrat(rational._new(r1[1]^R2, r1[2]^R2))
    else
      return numrat(rational._new(r1[2]^R2, r1[1]^R2))
    end
  end
end


--- R1 - R2
--  @param R1 First rational or integer number.
--  @param R2 Second rational or integer number.
--  @return Difference.
rational.__sub = function (R1, R2)
  if not (isrational(R1) and isrational(R2)) then
    local p = Cross.convert(R1, R2)
    if p then
      return R1 - p
    else
      p = Cross.convert(R2, R1)
      return p and (p - R2) or (Cross.float(R1) - Cross.float(R2))
    end
  end
  local r1, r2 = R1._, R2._
  return numrat(rational._new(r1[1]*r2[2]-r1[2]*r2[1], r1[2]*r2[2]))
end


--- String representation.
--  @return String with numerator and denominator.
rational.__tostring = function (self)
  local r = self._
  if type(r[1]) == 'number' and type(r[2]) == 'number'
     and mabs(r[1]) > r[2]
  then
    if rational.MIXED then
      local n = mabs(r[1])       -- numerator
      local v = math.modf(n / r[2])
      return string.format(
        "%s%d %d/%d", r[1] < 0 and '-' or '', v, n % r[2], r[2])
    else
      return string.format("%d/%d", r[1], r[2])
    end
  else
    return string.format("%s/%s", numStr(r[1]), numStr(r[2]))
  end
end


--- -R
--  @preturn Opposite rational number.
rational.__unm = function (self) return rational._new(-self._[1], self._[2]) end


about['_ar'] = {"arithmetic: a+b, a-b, a*b, a/b, -a, a^b", nil, help.META}
about['_cmp'] = {"comparison: a<b, a<=b, a>b, a>=b, a==b, a~=b", nil, help.META}


--- Convert value to rational number.
--  @param v Source value.
--  @return Rational number of false.
rational._convert = function (v)
  return (type(v) == 'number' and Vinteger(v) or type(v) == 'table' and v.__mod)
         and rational._new(v, 1)
end


--- Find rational number for the given continued fraction.
--  @param t Continued fraction coefficients.
--  @return Numerator and denomenator.
rational._cont2rat = function (t)
  local a, b = 0, 1
  for i = #t, 1, -1 do
    b, a = t[i] * b + a, b
  end
  return t[0] * b + a, b
end


--- Check if the number is 0.
--  @return true for zero.
rational._isZero = function (self)
  return Cross.isZero(self._[1])
end


--- Create new object, set metatable.
--  @param vn Numerator.
--  @param vd Denominator.
--  @return New rational object.
rational._new = function (vn, vd)
  local g = rational._gcd(vd, vn)     -- inverse order move sign to denominator
  return setmetatable({_ = {vn/g, vd/g}}, rational)
end


--- Get denominator.
--  @return Denominator.
rational.denom = function (self) return self._[2] end
about[rational.denom] = {"R:denom() --> var",
  "Return the denominator of the rational number."}



--- Float point representation.
--  @return Decimal fraction.
rational.float = function (self)
  local r = self._
  return (r[1] < 0 and -1 or 1) * (Cross.norm(r[1]) / Cross.norm(r[2]))
end
about[rational.float] = {"R:float() --> num",
  "Return rational number as decimal."}


--- Get rational number approximation.
--  @param f Source number.
--  @param fErr Precision, default is 0.001.
rational.from = function (_, f, fErr)
  fErr = fErr or 1E-3
  local f0, acc, c = math.abs(f), {}, nil
  acc[0], c = math.modf(f0)
  local a, b = acc[0], 1
  while c > 0 and math.abs(a/b - f0) > fErr do
    acc[#acc+1], c = math.modf(1/c)
    a, b = rational._cont2rat(acc)
  end
  return rational._new(f >= 0 and a or -a, b)
end
about[rational.from] = {":from(src_f, err_f=1E-3) --> R",
  "Estimate ratio from floating point value.", help.NEW}


--- Get rational number from continued fraction coefficients.
--  @param t List of coefficients.
--  @return Rational number.
rational.fromCF = function (_, t)
  local check = {}
  for i, v in ipairs(t) do
    if (type(v) == 'number' and Vinteger(v)
      or type(v) == 'table' and v.__mod) and v > 0
    then
      check[i] = v
    else
      error "Positive integer is expected"
    end
  end
  local t0 = t[0] or 0
  if (type(t0) == 'number' and Vinteger(t0)
    or type(t0) == 'table' and t0.__mod)
  then
    check[0] = t0
  else
    error "Integer is expected"
  end
  return rational._new(rational._cont2rat(check))
end
about[rational.fromCF] = {":fromCF(coeff_t) --> R",
  "Transform continued fraction to rational number.", CONTINUATED}


--- The greatest common divisor.
--  @param va First integer.
--  @param vb Second integer.
--  @return Greatest common divisor.
rational._gcd = function (va, vb)
  return Cross.isZero(va) and vb or rational._gcd(vb % va, va)
end


--- Get numerator.
--  @return Numerator.
rational.num = function (self) return self._[1] end
about[rational.num] = {"R:num() --> var",
  "Return the numerator of rational number."}


--- Find continued fraction coefficients.
--  @return Table of coefficients t such that R = t[0] + 1/(t[1]+1/(t[2]+1/...
rational.toCF = function (self)
  local a, b = self._[1], self._[2]
  if a < 0 then
    error "Positive is expected"
  end
  local numbers = (type(a) == 'number' and type(b) == 'number')
  local res = {}
  for i = 0, math.huge do
    local c = numbers and math.modf(a / b) or (a / b)
    res[i] = c
    a = a - b * c
    if a <= 1 then break end
    a, b = b, a
  end
  res[#res+1] = math.modf(b)
  return setmetatable(res, mt_continued)
end
about[rational.toCF] = {"R:toCF() --> coeff_t",
  "Transform rational number to continued fraction.", CONTINUATED}


-- call constructor, check arguments
setmetatable(rational, {
__call = function (_, n, d)
  if isrational(n) and not d then
    return n
  end
  d = d or 1
  assert(
    type(n) == 'number' and Vinteger(n) or type(n) == 'table' and n.__mod,
    "Wrong numerator type")
  assert(
    type(d) == 'number' and Vinteger(d) or type(d) == 'table' and d.__mod,
    "Wrong denomenator type")
  assert(not Cross.isZero(d), "Wrond denomenator value")
  return rational._new(n, d)
end})
about[rational] = {" (num, denom=1) --> new_R",
  "Create rational number using num (and denom).", help.NEW}


-- Comment to remove descriptions
rational.about = about

return rational

--======================================
