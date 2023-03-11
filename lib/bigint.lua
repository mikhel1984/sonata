--[[		sonata/lib/bigint.lua

--- Operations with arbitrary long integer numbers.
--
--  Object structure: </br>
--  <code> {sign=S, _base=B, v1, ... vn} </code></br>
--  where <code>S</code> is +1/-1, B is 10 by default,
--  v1 - vn are digits of the number in reverse order.
--  For example, number <code>123</code> is represented as
--  <code>{sign=1, _base=10, 3, 2, 1}</code>.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2023.

	module 'bigint'
--]]

---------------- Tests -----------------
--[[TEST
-- use 'bigint'
Int = require 'lib.bigint'

-- from integer
a = Int(123)
ans = a:float()               --> 123

-- from string
b = Int('456')
ans = b:float()               --> 456

-- from table
-- 'sign' and 'base' can be skipped
g = Int {1,2,3,sign=-1,base=10}
ans = g:float()               --> -123

-- check equality
ans = (a == -g)               --> true

-- arithmetical operations
ans = (a+b):float()           --> 579

ans = (a-b):float()           --> -333

ans = (a*Int(2)):float()      --> 246

ans = (b/2):float()           --> 228

ans = (b%a):float()           --> 87

ans = (a^3):float()           --> 1860867

-- absolute value
ans = Int('-25'):abs():float()  --> 25

-- factorial
c = Int(50):fact()
ans = c:float()               --1> 3E64

ans = (a > b)                 --> false

-- compare with number
ans = a:eq(123)               --> true

-- number of digits
ans = #a                      --> 3

-- 2nd digit (from the lowest)
ans = a:at(2)                 --> 2

-- get numeric base
ans = g:base()                --> 10

-- change numeric base
v = g:rebase(60)
ans = tostring(v)             --> '-2|3'

-- operations with different bases
-- transform to the biggest common base
w = v + b
ans = tostring(w)             --> '5|33'

-- comparison
ans = (v == g)                --> true

-- simple print
print(a)

-- check if it prime
-- iterate though multipliers
ans = Int(1229):isPrime()       --> true

-- Fermat theorem
ans = Int(1229):isPrime('Fermat') --> true

-- factorize
t = b:factorize()
ans = #t                      --> 5

-- check factorization
ans = 1
for i = 1,#t do
  ans = ans * (t[i]:float())
end                           --> 456

-- pseudo-random number
-- from 0 to b
print(Int:random(b))

-- greatest common divisor
ans = a:gcd(b):float()        --> 3

-- with numbers
-- result is bigint
ans = a + 1.0                 --> Int(124)

-- result is float
ans = a - 0.5                 --> 122.5

--]]

--	LOCAL

local Ver = require("lib.utils")
local Cross = Ver.cross
Ver = Ver.versions

local ZERO = string.byte('0')

local NUMB = 'numbers'

--- Check object type.
--  @param v Object.
--  @return True if the object is a big integer.
local function isbigint(v) return type(v) == 'table' and v.isbigint end

--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Operations with arbitrary long integers."
}

--	MODULE

local bigint = {
-- mark
type='bigint', isbigint=true,
}

--- B1 + B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Sum object.
bigint.__add = function (B1, B2)
  if isbigint(B1) and isbigint(B2) then
    B1, B2 = bigint._simbase(B1, B2)
  else
    local p = Cross.convert(B1, B2)
    if p then
      return B1 + p
    else
      p = Cross.convert(B2, B1)
      return p and (p + B2) or (Cross.float(B1) + Cross.float(B2))
    end
  end
  if B1.sign > 0 then
    return (B2.sign > 0) and bigint._sum(B1, B2) or bigint._sub(B1, B2)
  else
    return (B2.sign > 0) and bigint._sub(B2, B1) or -bigint._sum(B1, B2)
  end
end

--- B1 / B1
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Ratio object.
bigint.__div = function (B1, B2)
  if isbigint(B1) and isbigint(B2) then
    B1, B2 = bigint._simbase(B1, B2)
  else
    local p = Cross.convert(B1, B2)
    if p then
      return B1 / p
    else
      p = Cross.convert(B2, B1)
      return p and (p / B2) or (Cross.float(B1) / Cross.float(B2))
    end
  end
  local res, _ = bigint._div(B1, B2)
  return res
end

-- methametods
bigint.__index = bigint

--- a <= b
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return True if the first value is less or equal to the second.
bigint.__le = function (B1, B2)
  if isbigint(B1) and isbigint(B2) then
    B1, B2 = bigint._simbase(B1, B2)
  else
    local p = Cross.convert(B1, B2)
    if p then
      return B1 <= p
    else
      p = Cross.convert(B2, B1)
      if p then return p <= B2
      else
        return Cross.float(B1) <= Cross.float(B2)
      end
    end
  end
  return not bigint._gt(B1, B2)
end

bigint.__len = function (B) return #B._ end

--- B1 < B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return True if the first value is less then the second one.
bigint.__lt = function (B1, B2)
  if isbigint(B1) and isbigint(B2) then
    B1, B2 = bigint._simbase(B1, B2)
  else
    local p = Cross.convert(B1, B2)
    if p then
      return B1 < p
    else
      p = Cross.convert(B2, B1)
      if p then return p < B2
      else
        return Cross.float(B1) < Cross.float(B2)
      end
    end
  end
  if B1.sign < B2.sign then return true end
  local b1, b2 = B1._, B2._
  if #b1 == #b2 then   -- equal length
    for i = #b1, 1, -1 do
      if b1[i] ~= b2[i] then
        return (B1.sign > 0 and b1[i] < b2[i]) or
               (B1.sign < 0 and b1[i] > b2[i])
      end
    end
    return false
  else                 -- different length
    return (B1.sign > 0 and #b1 < #b2) or (B1.sign < 0 and #b1 > #b2)
  end
end

--- B1 % B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Remainder object.
bigint.__mod = function (B1, B2)
  if isbigint(B1) and isbigint(B2) then
    B1, B2 = bigint._simbase(B1, B2)
  else
    local p = Cross.convert(B1, B2)
    if p then
      return B1 % p
    else
      p = Cross.convert(B2, B1)
      return p and (p % B2) or (Cross.float(B1) % Cross.float(B2))
    end
  end
  local _, res = bigint._div(B1, B2)
  return res
end

--- B1 * B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Product object.
bigint.__mul = function (B1, B2)
  if isbigint(B1) and isbigint(B2) then
    B1, B2 = bigint._simbase(B1, B2)
  else
    local p = Cross.convert(B1, B2)
    if p then
      return B1 * p
    else
      p = Cross.convert(B2, B1)
      return p and (p * B2) or (Cross.float(B1) * Cross.float(B2))
    end
  end
  local res = bigint._mul(B1, B2)
  res.sign = B1.sign * B2.sign
  return res
end

bigint.__newindex = function () error("Immutable object") end

--- B1 ^ B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Power of the number.
bigint.__pow = function (B1, B2)
  if isbigint(B1) and isbigint(B2) then
    B1, B2 = bigint._simbase(B1, B2)
  else
    local p = Cross.convert(B1, B2)
    if p then
      return B1 ^ p
    else
      p = Cross.convert(B2, B1)
      return p and (p ^ B2) or (Cross.float(B1) ^ Cross.float(B2))
    end
  end
  if B2.sign < 0 then error('Negative power!') end
  local y, x = bigint._1, B1
  y._base = B1._base
  if #B2 == 1 and B2._[1] == 0 then
    assert(#B1 > 1 or B1._[1] ~= 0, "Error: 0^0!")
    return res
  end
  local dig, mul, rest = {}, bigint._mul, nil
  for i = 1, #B2 do dig[i] = B2._[i] end
  while #dig > 1 or #dig == 1 and dig[1] > 1 do
    dig, rest = bigint._divBase(dig, B1._base, 2)
    if rest == 1 then
      y = mul(y, x)
    end
    x = mul(x, x)
  end
  return mul(x, y)
end

--- B1 - B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Difference object.
bigint.__sub = function (B1, B2)
  if isbigint(B1) and isbigint(B2) then
    B1, B2 = bigint._simbase(B1, B2)
  else
    local p = Cross.convert(B1, B2)
    if p then
      return B1 - p
    else
      p = Cross.convert(B2, B1)
      return p and (p - B2) or (Cross.float(B1) - Cross.float(B2))
    end
  end
  if B1.sign > 0 then
    return (B2.sign > 0) and bigint._sub(B1, B2) or bigint._sum(B1, B2)
  else
    return (B2.sign > 0) and -bigint._sum(B1, B2) or bigint._sub(B2, B1)
  end
end

--- - B
--  @param B Bigint object.
--  @return Opposite value.
bigint.__unm = function (B)
  local res = bigint:_new({base=B._base, sign=-B.sign, 0})
  res._ = B._
  return res
end

--- String representation.
--  @param B Bigint object.
--  @return String object.
bigint.__tostring = function (B)
  local s = nil
  if B._base > 10 then
    s = {}
    local b = B._
    local n = #b+1
    for i = 1, #b do s[i] = b[n-i] end
    s = table.concat(s, '|')
  else
    s = string.reverse(table.concat(B._, (B._base == 10) and '' or '|'))
  end
  return B.sign < 0 and ('-' .. s) or s
end

bigint.arithmetic = 'arithmetic'
about[bigint.arithmetic] = {
  bigint.arithmetic, "a+b, a-b, a*b, a/b, a%b, a^b, -a, #a", help.META}
bigint.comparison = 'comparison'
about[bigint.comparison] = {
  bigint.comparison, "a<b, a<=b, a>b, a>=b, a==b, a~=b", help.META}

--- Correct function arguments if need.
--  @param num1 First number representation.
--  @param num2 Second number representation (optional).
--  @return Bigint objects with the same numeric bases.
bigint._args = function (num1, num2)
  num1 = isbigint(num1) and num1 or bigint:_new(num1)
  num2 = isbigint(num2) and num2 or bigint:_new(num2)
  if num1._base > num2._base then
    num2 = num2:rebase(num1._base)
  elseif num1._base < num2._base then
    num1 = num1:rebase(num2._base)
  end
  return num1, num2
end

--- Try to convert object into bigint.
--  @param v Source objec.
--  @return Int object.
bigint._convert = function (v)
  return Ver.isInteger(v) and bigint:_new(v)
end

--- Deep copy of the object.
--  @param B Original bigint object.
--  @return Deep copy.
bigint._copy = function (B)
  local c = bigint:_new({B._[1], sign=B.sign, base=B._base})
  for i = 2, #B do c._[i] = B._[i] end
  return c
end

--- In-place decrement for positive number.
--  @param B Number to decrease by 1.
bigint._decr = function (B)
  local b = B._
  if #b == 1 and b[1] == 0 then return end
  local dif = 1
  for i = 1, #b-1 do
    b[i] = b[i] - dif
    if b[i] < 0 then
      b[i] = B._base - 1
    else
      dif = 0
      break
    end
  end
  if dif > 0 then
    local bb = b[#b]
    b[#b] = (bb > 1) and (bb - 1) or (#b == 1) and 0 or nil
  end
end

--- Main algorithm for division.
--  @param B1 First number representation.
--  @param B2 Second number representation.
--  @return The quotient and remainder.
bigint._div = function (B1, B2)
  local b1, b2 = B1._, B2._
  if #b2 == 1 and b2[1] == 0 then error("Divide by 0!") end
  local d = B1._base
  local res = bigint:_new({0, base=d})
  if #b1 < #b2 then  -- too short
    return res, B1
  end
  local rem = bigint:_new({0, base=d});
  rem._[1] = nil
  local k = #b1 - #b2 + 1
  Ver.move(b1, k+1, #b1, 1, rem._)  -- copy last elements
  local v2, den, acc = B2:float(), B2:abs(), {}
  for i = k, 1, -1 do
    table.insert(rem._, 1, b1[i])
    if rem >= den then
      local n = math.modf(rem:float() / v2)  -- estimate
      local tmp = rem - den * bigint:_new({n, base=d})
      if tmp.sign < 0 then
        n = n - 1
        tmp = tmp + den
      elseif tmp > den then
        n = n + 1
        tmp = tmp - den
      end
      rem = tmp
      acc[#acc+1] = n
    elseif #acc > 0 then
      acc[#acc+1] = 0
    end
  end
  for i, v in ipairs(acc) do res._[#acc-i+1] = v end
  res.sign = B1.sign*B2.sign
  return res, rem
end

--- Divide elements in the list ot given number, find reminder.
--  @param t List of numbers.
--  @param iOld Initial bases.
--  @param iNew New bases.
--  @return Quotient and reminder.
bigint._divBase = function (t, iOld, iNew)
  local rest, set = 0, false
  for i = #t, 1, -1 do
    rest = rest * iOld + t[i]
    local n, _ = math.modf(rest / iNew)
    if set or n > 0 then
      t[i] = n
      set = true
    else
      t[i] = nil
    end
    rest = rest - iNew * n
  end
  return t, rest
end

--- Greatest common devision for bigint objects.
--  @param B1 First value.
--  @param B2 Second value.
--  @return Bigint gcd.
bigint._gcd = function (B1, B2)
  return (#B1._ == 1 and B1._[1] == 0) and B2 or bigint._gcd(B2 % B1, B1)
end

--- Check if the first number is greater then the second
--  @param B1 First number.
--  @param B2 Second number.
--  @return True if B1 > B2.
bigint._gt = function (B1, B2)
  if B1.sign > B2.sign then return true end
  local b1, b2 = B1._, B2._
  if #b1 == #b2 then
    for i = #b1, 1, -1 do
      if b1[i] ~= b2[i] then
        return (B1.sign > 0 and b1[i] > b2[i]) or
               (B1.sign < 0 and b1[i] < b2[i])
      end
    end
    return false
  else
    return (B1.sign > 0 and #b1 > #b2) or (B1.sign < 0 and #b1 < #b2)
  end
end

--- In-place increment for positive number.
--  @param B Number to increase by 1.
bigint._incr = function (B)
  local add, b = 1, B._
  for i = 1, #b do
    b[i] = b[i] + add
    if b[i] == B._base then
      b[i], add = 0, 1
    else
      add = 0
      break
    end
  end
  if add > 0 then
    b[#b+1] = 1
  end
end

--- Check if it prime using the Fermat theorem.
--  @param B Number.
--  @return true if prime.
bigint._primeFermat = function (B)
  local a = nil
  local div, pow = bigint._div, bigint._powm
  for i = 1, 5 do
    repeat
      a = bigint:random(B)
    until a:float() >= 2
    local v1 = pow(a, B, B)
    local _, v2 = div(a, B)
    if v1 ~= v2 then return false end
  end
  return true
end

--- Straightforward product algorithm.
--  @param B1 First bigint multiplier.
--  @param B2 Second bigint multiplier.
--  @return Product without sign.
bigint._mul = function (B1, B2)
  local sum = bigint:_zero(B1._base, 1)
  local b1, b2, s = B1._, B2._, sum._
  -- get products
  for i = 0, #b1-1 do
    local v = b1[i+1]
    if v > 0 then
      for j = 1, #b2 do
        local pos = i+j
        s[pos] = (s[pos] or 0) + v * b2[j]
      end
    else
      s[i+1] = s[i+1] or 0
    end
  end
  -- rearrange
  local rest, d = 0, B1._base
  for i = 1, #s do
    local si = s[i] + rest
    rest = math.modf(si / d)
    s[i] = si - rest * d
  end
  if rest > 0 then s[#s+1] = rest end
  return sum
end

--- Create new object, set metatable.
--  @param num Integer, string or table.
--  @return Bigint object.
bigint._new = function (self, num)
  local int, acc = {_base=10, sign=1}, {}
  -- prepare
  if type(num) == 'table' then
    int._base = num.base or 10
    int.sign   = num.sign or 1
    for i = #num, 1, -1 do
      acc[#acc+1] = num[i] -- reverse
    end
  elseif type(num) == 'string' then
    local sign, s = string.match(num, '^([+-]?)(%d+)$')
    if sign == '-' then int.sign = -1 end
    s = string.reverse(s)
    for i = 1, #s do
      acc[i] = string.byte(s, i)-ZERO
    end
  elseif type(num) == 'number' and Ver.isInteger(num) then
    if num < 0 then
      int.sign = -1
      num = -num
    end
    repeat
      local n, _ = math.modf(num / 10)
      acc[#acc+1] = num - 10*n
      num = n
    until num == 0
  end
  -- check result
  if #acc == 0 then
    error('Wrong number '..tostring(num))
  end
  int._ = acc
  return setmetatable(int, self)
end

--- Find (B1 ^ B2) % B3
--  @param B1 First bigint object.
--  @param B2 Second bigint object.
--  @param B3 Third bigint object.
--  @return Modular power.
bigint._powm = function (B1, B2, B3)
  local div = bigint._div
  _, B1 = div(B1, B3)
  if #B1._ == 1 and B1._[1] == 0 then
    --return bigint:_new({0, base=B1._base})
    return bigint:_zero(B1._base, 1)
  end
  local y, x = bigint._1, B1
  y._base = B1._base
  local dig, mul, rest = bigint._copy(B2), bigint._mul, nil
  local d = dig._
  while #d > 1 or #d == 1 and d[1] > 1 do
    d, rest = bigint._divBase(d, B1._base, 2)
    if rest == 1 then
      _, y = div(mul(y, x), B3)
    end
    _, x = div(mul(x, x), B3)
  end
  _, rest = div(mul(x,y), B3)
  return rest
end

--- Transform numbers into the same base.
--  @param num1 First object.
--  @param num2 Second object.
--  @return Save numbers in common base.
bigint._simbase = function (num1, num2)
  if num1._base > num2._base then
    num2 = num2:rebase(num1._base)
  elseif num1._base < num2._base then
    num1 = num1:rebase(num2._base)
  end
  return num1, num2
end

--- Estimate square root using Babylonian method.
--  @param B Bigint object.
--  @return Estimation of sqrt(B).
bigint._sqrt = function (B)
  local ai = bigint:_new({1, base=B._base})
  local sum, div, sub = bigint._sum, bigint._div, bigint._sub
  repeat
    local aii, _ = div(B, ai)
    aii._ = bigint._divBase(sum(ai, aii)._, B._base, 2)
    ai, aii = aii, sub(aii, ai)
  until #aii._ == 1 and (aii._[1] <= 1)   -- TODO: check and decrease if need
  return ai
end

--- Get subtraction for two positive bigint numbers.
--  @param B1 First bigint object.
--  @param B2 Second bigint object.
--  @return Difference of the values.
bigint._sub = function (B1, B2)
  local r, n = 1, B1._base
  local res = bigint:_zero(n, 1)
  local b1, b2 = B1._, B2._
  -- find the biggest number
  if #b1 < #b2 then
    r = -1
  elseif #b1 == #b2 then
    local i = #b1
    -- find first difference
    while i > 0 and b1[i] == b2[i] do i = i - 1 end
    if i == 0 then return res end
    if b1[i] < b2[i] then r = -1 end
  end
  if r == -1 then
    b1, b2 = b2, b1
  end
  -- subtraction
  local rr, sub = res._, 0
  for i = 1, math.max(#b1, #b2) do
    local v = b1[i] - (b2[i] or 0) - sub
    if v < 0 then
      rr[i], sub = v + n, 1
    else
      rr[i], sub = v, 0
    end
  end
  -- simplify
  while #rr > 1 and rr[#rr] == 0 do rr[#rr] = nil end
  res.sign = r
  return res
end

--- Get sum of the two positive big numbers.
--  @param B1 First bigint object.
--  @param B2 Second bigint object.
--  @return Sum of the values.
bigint._sum = function (B1, B2)
  local n, add = B1._base, 0
  local res = bigint:_zero(n, 1)
  local b1, b2, rr = B1._, B2._, res._
  for i = 1, math.max(#b1, #b2) do
    local v = (b1[i] or 0) + (b2[i] or 0) + add
    if v >= n then
      rr[i], add = v - n, 1
    else
      rr[i], add = v, 0
    end
  end
  if add == 1 then rr[#rr+1] = 1 end
  return res
end

--- Searching for prime factor.
--  @param B Integer number.
--  @param B0 Initial multiplier.
--  @return Pair of multipliers or nil.
bigint._trivialSearch = function (B, B0)
  local n = B0 and B0:_copy() or bigint:_new({2})
  n = n:rebase(B._base)
  local sq = bigint._sqrt(B)
  while #sq._ > #n._ or not bigint._gt(n, sq) do
    local v1, v2 = bigint._div(B, n)
    if #v2._ == 1 and v2._[1] == 0 then
      return n, v1
    end
    bigint._incr(n)
  end
  return nil  -- not found
end

--- Create zero element.
--  @param nBase Base value.
--  @param iSign Sign value.
--  @return Zero number.
bigint._zero = function (self, nBase, iSign)
  return setmetatable({_={0}, _base=nBase, sign=iSign}, self)
end

bigint._1 = bigint:_new({1})

--- Absolute value of number.
--  @param B Bigint or integer number.
--  @return Absolute value.
bigint.abs = function (B)
  local a = bigint:_zero(B._base, 1)
  a._ = B._
  return a
end
about[bigint.abs] = {"B:abs() --> num", "Return module of arbitrary long number."}

--- Get digit.
--  @param B Bigint object.
--  @param N Index.
--  @return Digit at the N-th position.
bigint.at = function (B, N) return B._[N] end
about[bigint.at] = {"B:at(N) --> int", "Get N-th digit.", help.OTHER}

--- Get numeric base.
--  @param B Bigint object.
--  @return Base value.
bigint.base = function (B) return B._base end
about[bigint.base] = {"B:base() --> int", "Current numeric base."}


--- a == b.
--  In Lua v == 0 is always false because in the case of number
--  the program tries to convert everything into number.
--  For two bigint objects using of <code>==</code> is also possible.
--  @param B1 First bigint object or integer.
--  @param B2 Second bigint object or integer.
--  @return True if numbers have the same values and signs.
bigint.eq = function (B1, B2)
  if isbigint(B1) and isbigint(B2) then
    B1, B2 = bigint._simbase(B1, B2)
  else
    local p = Cross.convert(B1, B2)
    if p then
      return B1 == p
    else
      p = Cross.convert(B2, B1)
      if p then return p == B2
      else
        return Cross.float(B1) == Cross.float(B2)
      end
    end
  end
  local b1, b2 = B1._, B2._
  if #b1 == #b2 and B1.sign == B2.sign then
    for i = 1, #b1 do
      if b1[i] ~= b2[i] then return false end
    end
    return true
  end
  return false
end
about[bigint.eq] = {
  "B:eq(x) --> bool", "Check equality with the second value.", help.OTHER}
-- redefine equality
bigint.__eq = bigint.eq

--- B!
--  Use fact that n*(n-1)*...*2*1 = (n*1)*((n-1)*2)*...
--  @param B Bigint object.
--  @return Factorial of the number as bigint object.
bigint.fact = function (B)
  assert(B.sign > 0, "Non-negative value is expected!")
  local N = B:float()
  if N <= 1 then return bigint._1 end
  if N == 2 then return B end  
  local n, m = math.modf((N-2) * 0.5)
  local two = bigint:_new(2):rebase(B._base)
  local S, d, acc = B, B, B
  for i = 1, n do
    d = bigint._sub(d, two)
    S = bigint._sum(S, d)
    acc = bigint._mul(acc, S)
  end
  if m > 1E-3 then   -- m > 0
    local v = n + two
    acc = bigint._mul(acc, v)
  end
  return acc
end
about[bigint.fact] = {
  "B:fact() --> B!", "Return factorial of non-negative integer B."}

--- Find multipliers for the number.
--  @param B Integer number.
--  @return List of prime numbers.
bigint.factorize = function (B)
  local v, res = B, {}
  if B.sign < 0 then res[1] = -1 end
  local n, q = nil, nil
  while true do
    n, q = bigint._trivialSearch(v, n)
    if n == nil then
      res[#res+1] = v
      break
    else
      res[#res+1] = n
      v = q
    end
  end
  return res
end
about[bigint.factorize] = {
  "B:factorize() --> primeBs_t", "Find the list of multipliers.", NUMB}

--- Float number representation.
--  @param B Bigint object.
--  @return Integer if possible, otherwise float point number.
bigint.float = function (B)
  local d, v, sum = B._base, 1, 0
  local b = B._
  for i = 1, #b do
    sum = sum + b[i]*v
    v = v * d
    if v > 1E18 then
      local b1 = #b-1
      sum = (b[#b] + b[b1]/d) * d^b1
      break
    end
  end
  return B.sign >= 0 and sum or (-sum)
end
about[bigint.float] = {"B:float() --> num",
  "Represent current big integer as number if it possible.", help.OTHER}

--- Greatest common devision for two (big) numbers.
--  @param B1 First value.
--  @param B2 Second value.
--  @return Bigint gcd.
bigint.gcd = function (B1, B2)
  B1, B2 = bigint._args(B1, B2)
  return bigint._gcd(B1, B2)
end
about[bigint.gcd] = {
  "B:gcd(B2) --> B3", "Find the greatest common divisor for two integers.", NUMB}

-- TODO try https://en.wikipedia.org/wiki/Primality_test
--- Check if the number is prime.
--  @param B Number.
--  @param sMethod Trivial search by default. Can be 'Fremat'.
--  @return true if prime.
bigint.isPrime = function (B, sMethod)
  if sMethod == 'Fermat' then
    return bigint._primeFermat(B)
  end
  -- default is a simple search
  local v1, v2 = bigint._trivialSearch(B)
  return v1 == nil
end
about[bigint.isPrime] = {"B:isPrime([method_s]) --> bool",
  "Check if the number is prime. Set 'Fermat' method to use the small Fermat theorem.",
  NUMB}

--- Generate random number.
--  @param self Do nothing.
--  @param B Upper limit.
--  @return Number from 0 to B.
bigint.random = function (self, B)
  B = isbigint(B) and B or bigint:_new(B)
  local d, set, v = B._base, false, 0
  local res = bigint:_zero(d, B.sign)
  local n = math.random(1, #B)
  local b, rr = B._, res._
  local any = (n ~= #b)
  for i = n, 1, -1 do
    -- generate
    if any then
      v = math.random(1, d) - 1
    else
      v = math.random(1, b[i]+1)-1
      any = (v < b[i])
    end
    -- add
    if set or v > 0 then
      rr[i] = v
      set = true
    end
  end
  return res
end
about[bigint.random] = {":random(B) --> rand_B",
  "Generate pseudo-random value from 0 to B.", help.STATIC}

--- Change current numeric base.
--  @param B Bigint object.
--  @param N New base.
--  @return Copy with new base.
bigint.rebase = function (B, N)
  if N <= 0 then error("Wrong base "..tostring(N)) end
  if B._base == N then return B end
  local res = bigint:_zero(N, B.sign)
  local b, rr = B._, res._
  rr[1] = nil    -- remove zero
  -- reverse order
  local dig, n = {}, #b+1
  for i, v in ipairs(b) do dig[i] = v end
  repeat
    dig, n = bigint._divBase(dig, B._base, N)
    rr[#rr+1] = n
  until #dig == 0
  return res
end
about[bigint.rebase] = {
  "B:rebase(N) --> upd_B", "Convert number to the new numeric base."}

-- simplify constructor call
setmetatable(bigint, {
__call = function (self, v)
  return bigint:_new(v)
end})
about[bigint] = {" (var) --> new_B", 
  "Create number from integer, string or table.", help.STATIC}

-- Comment to remove descriptions
bigint.about = about

return bigint

--=================================
