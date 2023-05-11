--[[		sonata/lib/rational.lua

--- Rational number operations support.
--
--  Object structure: </br>
--  <code>{numerator,denominator}</code></br>
--  where both numbers are integers.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2023.

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

-- represent as decimal
ans = a:float()               --> 0.5

-- from decimal
ans = Rat:from(math.pi)       --> Rat(333, 106)

-- numerator
ans = b:num()                 --> 2

-- denominator
ans = b:denom()               --> 1

-- show
print(a)

-- continued fraction to rational
-- 1 + 1/(2+1/(3+1/4)) 
c = Rat:fromCont {[0]=1, 2, 3, 4}
ans = c                       --> Rat(43,30)

-- rational to continued fraction
d = c:toCont()
ans = d[1]                    --> 2

-- show continued fraction
print(d)

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




local mabs = math.abs


local function isbig(v) return type(v) == 'number' and mabs(v) > 1E10 end


local function numrat(R)
  return Cross.eq(R._[2], 1) and Cross.simp(R._[1])               -- x / 1
    -- float num or denom
    or (isbig(R._[1]) or isbig(R._[2])) and (R._[1] / R._[2])
    or R
end


--- Number representation.
--  @param v Value.
--  @return String representation.
local function numStr(v)
  return type(v) == 'number' and Utils.numstr(v) or tostring(v)
end


-- Continued fraction printing
local _continued = {
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


--	MODULE

local rational = {
-- mark
type = 'rational', 
-- simplification
_simp = numrat,
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
    if p then return R1 / p
    else
      p = Cross.convert(R2, R1)
      return p and (p / R2) or (Cross.float(R1) / Cross.float(R2))
    end
  end
  local r1, r2 = R1._, R2._
  return numrat(rational._new(r1[1]*r2[2], r1[2]*r2[1]))
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
    if p then return R1 <= p
    else
      p = Cross.convert(R2, R1)
      if p then return p <= R2
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
    if p then return R1 < p
    else
      p = Cross.convert(R2, R1)
      if p then return p < R2
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
    if p then return R1 * p
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
    if not (Ver.isInteger(R2) and R2 >= 0) then
      error("Power must be a non-negative integer")
    end
    local r1 = R1._
    return numrat(rational._new(r1[1]^R2, r1[2]^R2))
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
  local r1, r2 = R1._, R2._
  return numrat(rational._new(r1[1]*r2[2]-r1[2]*r2[1], r1[2]*r2[2]))
end


--- String representation.
--  @param R Rational number.
--  @return String with numerator and denominator.
rational.__tostring = function (R)
  local r = R._
  if type(r[1]) == 'number' and type(r[2]) == 'number'
        and mabs(r[1]) > r[2] then
    local n = mabs(r[1])       -- numerator
    local v = math.modf(n / r[2])
    return string.format(
      "%s%d %d/%d", r[1] < 0 and '-' or '', v, n % r[2], r[2])
  else
    return string.format("%s/%s", numStr(r[1]), numStr(r[2]))
  end
end


--- -R
--  @param R Rational number.
--  @preturn Opposite rational number.
rational.__unm = function (R) return rational._new(-R._[1], R._[2]) end


about['_ar'] = {"arithmetic: a+b, a-b, a*b, a/b, -a, a^b", '', help.META}
about['_cmp'] = {"comparison: a<b, a<=b, a>b, a>=b, a==b, a~=b", '', help.META}


--- Convert value to rational number.
--  @param v Source value.
--  @return Rational number of false.
rational._convert = function (v)
  return (type(v) == 'number' and Ver.isInteger(v)
            or type(v) == 'table' and v.__mod)
         and rational._new(v, 1)
end


--- Find rational number for the given continued fraction.
--  @param t Continued fraction coefficients.
--  @return Numerator and denomenator.
rational._cont2rat = function (t)
  local a, b = 0, 1
  for i = #t, 1, -1 do
    a = t[i] * b + a
    a, b = b, a
  end
  return t[0] * b + a, b
end


--- Create new object, set metatable.
--  @param vn Numerator.
--  @param vd Denominator. Default is 1.
--  @return New rational object.
rational._new = function (vn, vd)
  local g = rational._gcd(vd, vn)     -- inverse order move sign to denominator
  return setmetatable({_ = {vn/g, vd/g}}, rational)
end


--- Get denominator.
--  @param R Rational number.
--  @return Denominator.
rational.denom = function (R) return R._[2] end
about[rational.denom] = {"R:denom() --> var", "Return the denominator of the rational number."}


--- R1 == R2
--  @param R1 First number.
--  @param R2 Second number.
--  @return True if the numbers are equal.
rational.eq = function (R1, R2)
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
  local r1, r2 = R1._, R2._
  return Cross.eq(r1[1], r2[1]) and Cross.eq(r1[2], r2[2])
end
about[rational.eq] = {"R:eq(x) --> bool", "Compare two objects.", help.OTHER}
rational.__eq = rational.eq


--- Float point representation.
--  @param R Rational number.
--  @return Decimal fraction.
rational.float = function (R)
  local r = R._
  return (r[1] < 0 and -1 or 1) * (Cross.norm(r[1]) / Cross.norm(r[2]))
end
about[rational.float] = {"R:float() --> num", "Return rational number as decimal."}


--- Get rational number approximation.
--  @param self Do nothing.
--  @param f Source number.
--  @param fErr Precision, default is 0.001.
rational.from = function (self, f, fErr)
  fErr = fErr or 1E-3
  local f0 = math.abs(f)
  local c, acc = f0, {}
  acc[0], c = math.modf(c)
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
--  @param self Do nothing.
--  @param t List of coefficients.
--  @return Rational number.
rational.fromCont = function (self, t)
  local check = {}
  for i, v in ipairs(t) do
    if (type(v) == 'number' and Ver.isInteger(v)
          or type(v) == 'table' and v.__mod) and v > 0 then
      check[i] = v
    else error("Positive integer is expected") end
  end
  local t0 = t[0] or 0
  if (type(t0) == 'number' and Ver.isInteger(t0)
        or type(t0) == 'table' and t0.__mod) then
    check[0] = t0
  else error("Integer is expected") end
  return rational._new(rational._cont2rat(check))
end
about[rational.fromCont] = {":fromCont(coeff_t) --> R",
  "Transform continued fraction to rational number.", help.NEW}


--- The greatest common divisor.
--  @param va First integer.
--  @param vb Second integer.
--  @return Greatest common divisor.
rational._gcd = function (va, vb)
  return Cross.eq(va, 0) and vb or rational._gcd(vb % va, va)
end


--- Get numerator.
--  @param R Rational number.
--  @return Numerator.
rational.num = function (R) return R._[1] end
about[rational.num] = {"R:num() --> var", "Return the numerator of rational number."}


--- Find continued fraction coefficients.
--  @param R Positive rational number.
--  @return Table of coefficients t such that R = t[0] + 1/(t[1]+1/(t[2]+1/...
rational.toCont = function (R)
  local a, b, c = R._[1], R._[2], nil
  if a < 0 then error("Positive is expected") end
  local numbers = (type(a) == 'number' and type(b) == 'number')
  local res = {}
  for i = 0, math.huge do
    c = numbers and math.modf(a / b) or (a / b)
    res[i] = c
    a = a - b * c
    if a <= 1 then break end
    a, b = b, a
  end
  res[#res+1] = math.modf(b)
  return setmetatable(res, _continued)
end
about[rational.toCont] = {"R:toCont() --> coeff_t", 
  "Transform rational number to continued fraction.", help.OTHER}


-- call constructor, check arguments
setmetatable(rational, {
__call = function (self, n, d)
  d = d or 1
  assert(
    type(n) == 'number' and Ver.isInteger(n) or type(n) == 'table' and n.__mod,
    "Wrong numerator type")
  assert(
    type(d) == 'number' and Ver.isInteger(d) or type(d) == 'table' and d.__mod,
    "Wrong denomenator type")
  assert(not Cross.eq(d, 0), "Wrond denomenator value")
  return rational._new(n, d)
end})
about[rational] = {" (num, denom=1) --> new_R", 
  "Create rational number using num (and denom).", help.NEW}


-- Comment to remove descriptions
rational.about = about

return rational

--======================================
-- TODO choose print format
