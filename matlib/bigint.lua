--[[		sonata/lib/bigint.lua

--- Operations with arbitrary long integer numbers.
--
--  Object structure: </br>
--  <code> {_sign=S, _={v1, ... vn}} </code></br>
--  where <code>S</code> is +1/-1,
--  v1 - vn are digits of the number in reverse order.
--  For example, number <code>123</code> is represented as
--  <code>{_sign=1, _={3, 2, 1}}</code> when BASE is 10.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2024.

	module 'bigint'
--]]


---------------- Tests -----------------
--[[TEST_IT
-- use 'bigint'
Int = require 'matlib.bigint'

-- from integer
a = Int(123)
ans = a:float()               -->  123

-- from string
b = Int('456')
ans = b:float()               -->  456

-- 'default' form for base <= 16
ans = Int('0xABC')            -->  Int('10,11,12:16')

-- define base explicitly
g = Int('-1,2,3:10')
ans = g:float()               -->  -123

-- check positive/negative
ans = g:sign()                -->  -1

-- check equality
ans = (a == -g)               -->  true

-- arithmetical operations
ans = (a+b):float()           -->  579

ans = (a-b):float()           -->  -333

ans = (a*Int(2)):float()      -->  246

ans = (b/2):float()           -->  228

ans = (b%a):float()           -->  87

ans = (a^3):float()           -->  1860867

ans = (a > b)                 -->  false

-- absolute value
ans = Int('-25'):abs():float()  -->  25

-- factorial
c = Int(50):F()
ans = c:float() / 3E64       --1>  1.0

-- ratio of factorials
ans = Int:ratF(Int(50), Int(49))  -->  Int(50)

-- subfactorial
ans = Int(4):subF()           --> Int(9)

-- digits for a different numeric base
v = g:digits(60)
ans = tostring(v)             -->  '-2,3:60'

-- improve view
c16 = c:digits(16)
print(c16:group(4))

-- number of digits
ans = #v                      -->  2

-- 2nd digit (from the lowest)
ans = v[2]                    -->  2

-- base and sign
ans = v.base == 60 and v.sign == -1  -->  true

-- print digits
print(v)

-- back to bigint
ans = Int(v)                  -->  g

-- comparison
ans = (a ~= g)                -->  true

-- simple print
print(a)

-- find permutations
ans = Int:P(10, 5)            -->  Int(30240)

-- find combinations
ans = Int:C(10, 3)            -->  Int(120)

-- with repititions
ans = Int:C(10, 3, true)      -->  Int(220)

-- check if it prime
-- iterate though multipliers
ans = Int(1229):isPrime()     -->  true

-- Fermat theorem
ans = Int(1229):isPrime('Fermat') -->  true

-- factorize
t = b:factorize()
ans = #t                      -->  5

-- check factorization
ans = 1
for i = 1,#t do
  ans = ans * (t[i]:float())
end                           -->  456

-- pseudo-random number
-- from 0 to b
print(b:random())

-- greatest common divisor
ans = Int:gcd(a,b,g):float()  -->  3

-- least common multiple
ans = Int:lcm(a,b):float()    --> 18696

-- with numbers
-- result is bigint
ans = a + 1.0                 -->  Int(124)

-- result is float
ans = a - 0.5                 -->  122.5

--]]

--	LOCAL

local Vinteger, Vmove, Cfloat, Cconvert do
  local lib = require("matlib.utils")
  Vinteger = lib.versions.isInteger
  Vmove = lib.versions.move
  Cfloat = lib.cross.float
  Cconvert = lib.cross.convert
end

local SEP = ','
local NUMB = 'numbers'
local COMB = 'combinations'

-- max number for one position
local BASE = math.floor(math.sqrt((math.maxinteger or (2^52)) / 10))
local log10 = math.log(10)
local logBase = math.log(BASE) / log10


--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Operations with arbitrary long integers."
}


--	MODULE

local mt_digits = {
map = {'1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', [0]='0'},
mapChar = {},
}
-- Fill inverted mapping
for k, v in pairs(mt_digits.map) do mt_digits.mapChar[v] = k end
mt_digits.__index = mt_digits


--- B << n
--  @param N number of positions.
--  @return left shifted number.
mt_digits.__shl = function (self, N)
  assert(Vinteger(N) and N >= 0)
  local res = {}
  for i = 1, N do res[i] = 0 end
  Vmove(self, 1, #self, N+1, res)
  res.base = self.base
  res.sign = self.sign
  return setmetatable(res, mt_digits)
end


--- B >> n
--  @param N number of positions.
--  @return right shifted number.
mt_digits.__shr = function (self, N)
  assert(Vinteger(N) and N >= 0)
  local res = {}
  if N >= #self then
    res[1] = 0
  else
    for i = 1, #self-N do res[i] = self[i+N] end
  end
  res.base = self.base
  res.sign = self.sign
  return setmetatable(res, mt_digits)
end


--- String representation.
--  @return string.
mt_digits.__tostring = function (self)
  local s, n = nil, #self + 1
  if self.base <= 16 then
    local acc = {}
    for i = 1, #self do acc[i] = mt_digits.map[ self[n-i] ] end
    s = table.concat(acc, '')
  else
    local acc = {}
    for i = 1, #self do acc[i] = self[n-i] end
    s = table.concat(acc, ',')
  end
  local rst = (self.base == 10) and '' or string.format(':%d', self.base)
  return string.format('%s%s%s', self.sign < 0 and '-' or '', s, rst)
end


--- Improve string view
--  @param N Number of digits in group.
--  @param sep Separator, optional.
--  @return 'Sparse' string representation.
mt_digits.group = function (self, N, sep)
  N, sep = N or 3, sep or '`'
  local n, acc = #self + 1, {}
  local small = (self.base <= 16)
  for i = 1, #self do
    local ni = n - i
    acc[#acc+1] = small and mt_digits.map[ self[ni] ] or self[ni]
    if i < #self then
      if (ni-1) % N == 0 then
        acc[#acc+1] = sep
      elseif not small then
        acc[#acc+1] = SEP
      end
    end
  end
  local rst = (self.base == 10) and '' or string.format(':%d', self.base)
  return string.format(
    '%s%s%s', self.sign < 0 and '-' or '', table.concat(acc, ''), rst)
end


-- Main module
local bigint = {
type = 'bigint',
}
assert(BASE > 2)


--- Check object type.
--  @param v Object.
--  @return True if the object is a big integer.
local function isbigint(v) return getmetatable(v) == bigint end


--- B1 + B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Sum object.
bigint.__add = function (B1, B2)
  if not (isbigint(B1) and isbigint(B2)) then
    local p = Cconvert(B1, B2)
    if p then
      return B1 + p
    else
      p = Cconvert(B2, B1)
      return p and (p + B2) or (Cfloat(B1) + Cfloat(B2))
    end
  end
  if B1._sign > 0 then
    return (B2._sign > 0) and bigint._sum(B1, B2) or bigint._sub(B1, B2:abs())
  else
    return (B2._sign > 0) and bigint._sub(B2, B1:abs()) or -bigint._sum(B1, B2)
  end
end


--- B1 / B1
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Ratio object.
bigint.__div = function (B1, B2)
  if not (isbigint(B1) and isbigint(B2)) then
    local p = Cconvert(B1, B2)
    if p then
      return B1 / p
    else
      p = Cconvert(B2, B1)
      return p and (p / B2) or (Cfloat(B1) / Cfloat(B2))
    end
  end
  local res, _ = bigint._div(B1, B2)
  return res
end


--- a == b.
--  In Lua v == 0 is always false because in the case of number
--  the program tries to convert everything into number.
--  For two bigint objects using of <code>==</code> is also possible.
--  @param B1 First bigint object or integer.
--  @param B2 Second bigint object or integer.
--  @return True if numbers have the same values and signs.
bigint.__eq = function (B1, B2)
  if not (isbigint(B1) and isbigint(B2)) then
    local p = Cconvert(B1, B2)
    if p then
      return B1 == p
    else
      p = Cconvert(B2, B1)
      if p then
        return p == B2
      else
        return Cfloat(B1) == Cfloat(B2)
      end
    end
  end
  return bigint._cmp(B1, B2) == 0
end


-- methametods
bigint.__index = bigint


--- a <= b
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return True if the first value is less or equal to the second.
bigint.__le = function (B1, B2)
  if not (isbigint(B1) and isbigint(B2)) then
    local p = Cconvert(B1, B2)
    if p then
      return B1 <= p
    else
      p = Cconvert(B2, B1)
      if p then
        return p <= B2
      else
        return Cfloat(B1) <= Cfloat(B2)
      end
    end
  end
  return bigint._cmp(B1, B2) < 1
end

--- B1 < B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return True if the first value is less then the second one.
bigint.__lt = function (B1, B2)
  if not (isbigint(B1) and isbigint(B2)) then
    local p = Cconvert(B1, B2)
    if p then
      return B1 < p
    else
      p = Cconvert(B2, B1)
      if p then
        return p < B2
      else
        return Cfloat(B1) < Cfloat(B2)
      end
    end
  end
  return bigint._cmp(B1, B2) == -1
end


--- B1 % B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Remainder object.
bigint.__mod = function (B1, B2)
  if not (isbigint(B1) and isbigint(B2)) then
    local p = Cconvert(B1, B2)
    if p then
      return B1 % p
    else
      p = Cconvert(B2, B1)
      return p and (p % B2) or (Cfloat(B1) % Cfloat(B2))
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
  if not (isbigint(B1) and isbigint(B2)) then
    local p = Cconvert(B1, B2)
    if p then
      return B1 * p
    else
      p = Cconvert(B2, B1)
      return p and (p * B2) or (Cfloat(B1) * Cfloat(B2))
    end
  end
  local res = bigint._mul(B1, B2)
  res._sign = B1._sign * B2._sign
  return res
end


--- Don't allow B[x] = y
bigint.__newindex = function () error("Immutable object") end


--- B1 ^ B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Power of the number.
bigint.__pow = function (B1, B2)
  if not (isbigint(B1) and isbigint(B2)) then
    local p = Cconvert(B1, B2)
    if p then
      return B1 ^ p
    else
      p = Cconvert(B2, B1)
      return p and (p ^ B2) or (Cfloat(B1) ^ Cfloat(B2))
    end
  end
  if B2._sign < 0 then error('Negative power!') end
  local y, x = bigint._1, B1
  if bigint._isZero(B2) then
    assert(not bigint._isZero(B1), "Error: 0^0!")
    return bigint._1
  end
  local dig, mul, rest = {}, bigint._mul, nil
  Vmove(B2._, 1, #B2._, 1, dig)
  while #dig > 1 or dig[1] > 1 do
    dig, rest = bigint._divBase(dig, BASE, 2)  -- x/2
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
  if not (isbigint(B1) and isbigint(B2)) then
    local p = Cconvert(B1, B2)
    if p then
      return B1 - p
    else
      p = Cconvert(B2, B1)
      return p and (p - B2) or (Cfloat(B1) - Cfloat(B2))
    end
  end
  if B1._sign > 0 then
    return (B2._sign > 0) and bigint._sub(B1, B2) or bigint._sum(B1, B2)
  else
    return (B2._sign > 0) and -bigint._sum(B1, B2)
      or bigint._sub(B2:abs(), B1:abs())
  end
end


--- - B
--  @return Opposite value.
bigint.__unm = function (self) return bigint._newTable(self._, -self._sign) end


--- String representation.
--  @return String object.
bigint.__tostring = function (self)
  if (#self._ * logBase) < 9 then
    return tostring(self:float())
  else
    return tostring(self:digits(10))
  end
end


about['_ar'] = {"arithmetic: a+b, a-b, a*b, a/b, a%b, a^b, -a, #a",
  nil, help.META}
about['_cmp'] = {"comparison: a<b, a<=b, a>b, a>=b, a==b, a~=b", nil, help.META}


--- Correct function arguments if need.
--  @param num1 First number representation.
--  @param num2 Second number representation (optional).
--  @return Bigint objects with the same numeric bases.
bigint._args = function (num1, num2)
  num1 = isbigint(num1) and num1 or bigint._newNumber(num1)
  num2 = isbigint(num2) and num2 or bigint._newNumber(num2)
  return num1, num2
end


--- Compare 2 bigint numbers.
--  @param B1 First bigint value.
--  @param B2 Second bigint value.
--  @return -1 for less, 0 for equal, +1 for greater
bigint._cmp = function (B1, B2)
  -- check signes
  local sign = B1._sign
  if sign ~= B2._sign then
    return sign
  end
  -- check size
  local b1, b2 = B1._, B2._
  if #b1 < #b2 then
    return -sign
  elseif #b1 > #b2 then
    return sign
  end
  -- compare elements
  for i = #b1, 1, -1 do
    local d = b1[i] - b2[i]
    if d > 0 then
      return sign
    elseif d < 0 then
      return -sign
    end
  end
  return 0
end


--- Try to convert object into bigint.
--  @param v Source objec.
--  @return Int object.
bigint._convert = function (v)
  return Vinteger(v) and bigint._newNumber(v) or nil
end


--- Deep copy of the object.
--  @return Deep copy.
bigint._copy = function (self)
  local c, b = {}, self._
  Vmove(b, 1, #b, 1, c)
  return bigint._newTable(c, self._sign)
end


--- Main algorithm for division.
--  @param B1 First number representation.
--  @param B2 Second number representation.
--  @return The quotient and remainder.
bigint._div = function (B1, B2)
  if bigint._isZero(B2) then
    error "Divide by 0!"
  end
  local res, rem = bigint._q_r(B1:abs(), B2:abs())
  res._sign = B1._sign*B2._sign
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
    local n = math.floor(rest / iNew)
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
  return bigint._isZero(B1) and B2 or bigint._gcd(B2 % B1, B1)
end


--- In-place increment.
--  @param B Number to change.
--  @param n Step, not more then the current base.
bigint._incr = function (B, n)
  assert(-BASE < n and n < BASE)
  local b = B._
  local v = (n < 0 and -n or n)
  if B._sign * n >= 0 then
    -- increase
    for i = 1, #b + 1 do
      local bi = (b[i] or 0) + v
      if bi >= BASE then
        b[i], v = bi - BASE, 1
      else
        b[i] = bi
        break
      end
    end
  else
    -- decrease
    for i = 1, #b do
      local bi = b[i] - v
      if bi < 0 then
        b[i], v = BASE + bi, 1
      else
        b[i], v = bi, 0
        break
      end
    end
    if v == 1 then  -- sign is changed
      b[1] = BASE - b[1]
      B._sign = (b[1] == 0) and 1 or (-B._sign)
    elseif #b > 1 and b[#b] == 0 then
      b[#b] = nil
    end
  end
end


--- Check for zero value.
--  @return true when B == 0
bigint._isZero = function (self) return #self._ == 1 and self._[1] == 0 end


--- Find the least common multiple for two numbers.
--  @param B1 First bigint value.
--  @param B2 Second bigint value.
--  @return least common multiplier.
bigint._lcm = function (B1, B2)
  return B1 * (B2 / bigint._gcd(B1, B2))
end


--- Straightforward product algorithm.
--  @param B1 First bigint multiplier.
--  @param B2 Second bigint multiplier.
--  @return Product without sign.
bigint._mul = function (B1, B2)
  local sum = bigint._newTable({0}, 1)
  local b1, b2, s = B1._, B2._, sum._
  -- get products
  for i = 0, #b1-1 do
    local v = b1[i+1]
    if v > 0 then
      for j = 1, #b2 do
        local pos = i + j
        s[pos] = (s[pos] or 0) + v * b2[j]
      end
    else
      s[i+1] = s[i+1] or 0
    end
  end
  -- rearrange
  local rest = 0
  for i = 1, #s do
    local si = s[i] + rest
    rest = math.floor(si / BASE)
    s[i] = si - rest * BASE
  end
  if rest > 0 then s[#s+1] = rest end
  return sum
end


--- Multiply to small number in-place.
--  @param t Digits of number.
--  @param x Multiplier.
bigint._mulX = function (t, x)
  assert(x < BASE)
  local add = 0
  for i = 1, #t do
    local v = t[i] * x + add
    if v >= BASE then
      v, add = v - BASE, 1
    else
      add = 0
    end
    t[i] = v
  end
  if add > 0 then t[#t+1] = 1 end
end


--- Create new object, set metatable.
--  @param num Integer, string or table.
--  @return Bigint object.
bigint._new = function (num)
  -- prepare
  if type(num) == 'table' then
    assert(#num > 0, "Wrong input")
    local base = num.base
    assert(base and base > 0 and Vinteger(base), "Wrong base")
    assert(num.sign, "Wrong sign")
    local acc = {}
    for i, v in ipairs(num) do
      if v < 0 or v >= base then error("Wrong digit at "..tostring(i)) end
      acc[i] = v
    end
    return bigint._newTable(
      bigint._rebase(acc, base, BASE), num.sign)
  elseif type(num) == 'string' then
    return bigint._newString(num)
  elseif type(num) == 'number' and Vinteger(num) then
    return bigint._newNumber(num)
  end
  error("Wrong number: "..tostring(num))
end


--- Make bigint from number.
--  @param num Integer number.
--  @return new bigint.
bigint._newNumber = function (num)
   local acc, sign = {}, 1
   if num < 0 then
     sign, num = -1, -num
   end
   local ibase = 1.0 / BASE
   repeat
     acc[#acc+1] = num % BASE
     num = math.floor(num * ibase)
   until num == 0
   return bigint._newTable(acc, sign)
end


--- Parse string representation, get integer number.
--  @param s Iput string.
--  @return new bigint.
bigint._newString = function (s)
  local sgn, body, sbase = string.match(s, "^([+-]?)([^:]+):?(%d*)$")
  -- check base
  local base = 10
  if #sbase == 0 then
    if     string.find(body, '^0x') then
      base, body = 16, string.sub(body, 3, -1)
    elseif string.find(body, '^0b') then
      base, body = 2, string.sub(body, 3, -1)
    end
  else
    base = tonumber(sbase)
    assert(base and base > 1, "Wrong base")
  end
  -- get digits
  local acc = {}
  if string.find(body, SEP) or base > 16 then
    -- all digits in decimal form
    for dig in string.gmatch(body, '%d+') do
      local v = tonumber(dig)
      assert(v and v < base)
      acc[#acc+1] = v
    end
  else
    -- sequential digits without separation for small bases
    for dig in string.gmatch(body, '.') do
      local v = mt_digits.mapChar[dig]
      if v then acc[#acc+1] = v end
    end
  end
  -- reverse
  for i = 1, math.floor(#acc / 2) do
    local j = #acc+1-i
    acc[i], acc[j] = acc[j], acc[i]
  end
  return bigint._newTable(bigint._rebase(acc, base, BASE), sgn == '-' and -1 or 1)
end


--- Make bigint from table.
--  @param t Digits in reverse order.
--  @param sn Sign (+1/-1).
--  @return new bigint.
bigint._newTable = function (t, sn)
  return setmetatable({_=t, _sign=sn}, bigint)
end


--- Find (B1 ^ B2) % B3
--  @param B1 First bigint object.
--  @param B2 Second bigint object.
--  @param B3 Third bigint object.
--  @return Modular power.
bigint._powm = function (B1, B2, B3)
  local div = bigint._div
  _, B1 = div(B1, B3)
  if bigint._isZero(B1) then return bigint._0 end
  local y, x = bigint._1, B1
  local dig, mul, rest = bigint._copy(B2), bigint._mul, nil
  local d = dig._
  while #d > 1 or d[1] > 1 do
    d, rest = bigint._divBase(d, BASE, 2)
    if rest == 1 then
      _, y = div(mul(y, x), B3)
    end
    _, x = div(mul(x, x), B3)
  end
  _, rest = div(mul(x, y), B3)
  return rest
end


--- Check if it prime using the Fermat theorem.
--  @param B Number.
--  @return true if prime.
bigint._primeFermat = function (B)
  for i = 1, 5 do
    local a = nil
    repeat
      a = bigint.random(B)
    until #a._ > 1 or a._[1] >= 2
    local v1 = bigint._powm(a, B, B)
    local _, v2 = bigint._div(a, B)
    if v1 ~= v2 then return false end
  end
  return true
end


--- Find quotient and reminder for 2 positive numbers.
--  @param a First integer.
--  @param b Second integer.
--  @return quotient and reminder.
bigint._q_r = function (a, b)
  local cmp = bigint._cmp
  local v = cmp(a, b)
  if v < 0 then
    return bigint._0, a
  elseif v == 0 then
    return bigint._1, bigint._0
  elseif #b._ == 1 then
    local c = bigint._copy(a)
    local _, r = bigint._divBase(c._, BASE, b._[1])
    return c, bigint._newTable({r}, 1)
  end
  -- find max doubled
  local c = bigint._copy(b)
  repeat
    bigint._mulX(c._, 2)
  until cmp(a, c) < 0
  bigint._divBase(c._, BASE, 2)
  -- find quotient and reminder
  a = bigint._sub(a, c)
  local n = bigint._newTable({1}, 1)
  while cmp(c, b) ~= 0 do
    bigint._divBase(c._, BASE, 2)
    bigint._mulX(n._, 2)
    if cmp(c, a) <= 0 then
      a = bigint._sub(a, c)
      bigint._incr(n, 1)
    end
  end
  return n, a
end


--- Change numeric base.
--  @param t Table with digits.
--  @param Nfrom Source base.
--  @param Nto Destination base.
--  @return result of conversation.
bigint._rebase = function (t, Nfrom, Nto)
  if Nfrom == Nto then return t end
  local res = {base=Nto}
  -- reverse order
  local dig = {}
  Vmove(t, 1, #t, 1, dig)
  repeat
    dig, res[#res+1] = bigint._divBase(dig, Nfrom, Nto)
  until #dig == 0
  return res
end


--- Estimate square root using Babylonian method.
--  @return Estimation of sqrt(B).
bigint._sqrt = function (self)
  local ai = (#self._ > 1 or self._[1] > 1) and bigint._2 or bigint._1
  local sum, sub = bigint._sum, bigint._sub
  repeat
    local aii, _ = bigint._div(self, ai)
    aii._ = bigint._divBase(sum(ai, aii)._, BASE, 2)  -- (ai + aii) / 2
    ai, aii = aii, sub(aii, ai)
  until #aii._ == 1 and (aii._[1] <= 1)
  return ai
end


--- Get subtraction for two positive bigint numbers.
--  @param B1 First bigint object.
--  @param B2 Second bigint object.
--  @return Difference of the values.
bigint._sub = function (B1, B2)
  local res = bigint._newTable({0}, 1)
  local cmp = bigint._cmp(B1, B2)
  if cmp == 0 then return res end
  local add, base1 = 1, BASE - 1
  local b1, b2, rr = B1._, B2._, res._
  if cmp < 0 then
    b1, b2 = b2, b1
    res._sign = -1
  end
  for i = 1, #b1 do
    local v = b1[i] + base1 - (b2[i] or 0) + add
    if v >= BASE then
      rr[i], add = v - BASE, 1
    else
      rr[i], add = v, 0
    end
  end
  -- simplify
  while #rr > 1 and rr[#rr] == 0 do rr[#rr] = nil end
  return res
end


--- Get sum of the two positive big numbers.
--  @param B1 First bigint object.
--  @param B2 Second bigint object.
--  @return Sum of the values.
bigint._sum = function (B1, B2)
  local add = 0
  local res = bigint._newTable({0}, 1)
  local b1, b2, rr = B1._, B2._, res._
  for i = 1, math.max(#b1, #b2) do
    local v = (b1[i] or 0) + (b2[i] or 0) + add
    if v >= BASE then
      rr[i], add = v - BASE, 1
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
  local n = B0 and B0:_copy() or bigint._newTable({2}, 1)
  local sq = bigint._sqrt(B)
  while bigint._cmp(sq, n) > 0 do
    local v1, v2 = bigint._div(B, n)
    if bigint._isZero(v2) then
      return n, v1
    end
    bigint._incr(n, 1)
  end
  return nil  -- not found
end


--- Common return values
bigint._0 = bigint._newTable({0}, 1)
bigint._1 = bigint._newTable({1}, 1)
bigint._2 = bigint._newTable({2}, 1)


--- Absolute value of number.
--  @return Absolute value.
bigint.abs = function (self) return bigint._newTable(self._, 1) end
about[bigint.abs] = {"B:abs() --> abs_B",
  "Return module of arbitrary long number."}


--- Get sign of the number.
--  @return sign in form -1/+1.
bigint.sign = function (self) return self._sign end
about[bigint.sign] = {"B:sign() --> int", "Return +1/-1."}


--- Get list of digits for the given base.
--  @param N New base.
--  @return Table with digits of the found number.
bigint.digits = function (self, N)
  N = N or 10
  assert(Vinteger(N) and N > 0, "Wrong base")
  local b = self._
  local res = bigint._rebase(b, BASE, N)
  res.sign = self._sign
  return setmetatable(res, mt_digits)
end
about[bigint.digits] = {"B:digits(N=10) --> tbl",
  "Get digits in the new numeric base."}


--- Find number of combinations.
--  @param n Total number of elements.
--  @param k Group size.
--  @param isRepeat Repetition is allowed.
--  @return Bigint for combination number.
bigint.C = function (_, n, k, isRepeat)
  n, k = bigint._args(n, k)
  if isRepeat then
    n = n - bigint._1
    return bigint:ratF(n+k, n) / bigint.F(k)
  end
  return bigint:ratF(n, k) / bigint.F(n-k)
end
about[bigint.C] = {":C(n, k, isRepeat=false) --> combinations_B",
  "Number of combinations C(n,k) with or without repetition.", COMB}


--- B!
--  Use the fact that n*(n-1)*...*2*1 = (n*1) * ((n-1)*2) * ...
--  @return Factorial of the number as bigint object.
bigint.F = function (self)
  assert(self._sign > 0, "Non-negative value is expected!")
  if #self._ == 1 then
    if     self._[1] <= 1 then return bigint._1
    elseif self._[1] == 2 then return self
    end
  end
  local S, acc = self, self
  local d = self - bigint._2
  local dd = d._
  while #dd > 1 or dd[1] > 1 do
    S = bigint._sum(S, d)
    acc = bigint._mul(acc, S)
    bigint._incr(d, -2)
  end
  if dd[1] == 1 then
    S = self + bigint._1
    bigint._divBase(S._, BASE, 2)
    acc = bigint._mul(acc, S)
  end
  return acc
end
about[bigint.F] = {"B:F() --> B!",
  "Return factorial of non-negative integer B.", COMB}


--- Get n!!
--  @return double factorial.
bigint.FF = function (self)
  -- TODO >= -1
  assert(self._sign > 0, "Non-negative value is expected!")
  if #self._ == 1 and self._[1] <= 1 then return bigint._1 end
  local res = self
  local d = bigint._sub(self, bigint._2)
  while #d._ > 1 or d._[1] > 1 do
    res = bigint._mul(res, d)
    bigint._incr(d, -2)
  end
  return res
end
about[bigint.FF] = {"B:FF() --> B!!", "Find double factorial.", COMB}


--- Find multipliers for the number.
--  @return List of prime numbers.
bigint.factorize = function (self)
  local v, res = self, {}
  if self._sign < 0 then res[1] = -1 end
  local n, q = nil, nil
  while true do
    n, q = bigint._trivialSearch(v, n)
    if n == nil then
      res[#res+1] = v
      break
    else  -- n, q ~= nil
      res[#res+1] = n
      v = q
    end
  end
  return res
end
about[bigint.factorize] = {"B:factorize() --> prime_t",
  "Find prive multipliers.", NUMB}


--- Float number representation.
--  @return Integer if possible, otherwise float point number.
bigint.float = function (self)
  local b, res = self._, 0
  if #b > 2 and #b * logBase > 9 then
    local s = math.log(b[#b] + b[#b-1]/BASE) / log10
    s = s + (#b-1)*logBase
    res = 10^s
  else
    -- exact
    for i = #b, 1, -1 do res = res * BASE + b[i] end
  end
  return self._sign >= 0 and res or (-res)
end
about[bigint.float] = {"B:float() --> num",
  "Represent current big integer as number if it possible.", help.OTHER}


--- Greatest common devision for several numbers.
--  @param ... List of numbers.
--  @return Bigint gcd.
bigint.gcd = function (_, ...)
  local t = {...}
  if #t == 0 then error('No numbers') end
  -- compare element-wise
  local res = isbigint(t[1]) and t[1] or bigint._newNumber(t[1])
  for i = 2, #t do
    local ti = t[i]
    res = bigint._gcd(
      res,
      isbigint(ti) and ti or bigint._newNumber(ti))
  end
  return res
end
about[bigint.gcd] = {":gcd(...) --> B",
  "Find the greatest common divisor for the given integers.", NUMB}


-- TODO try https://en.wikipedia.org/wiki/Primality_test
--- Check if the number is prime.
--  @param sMethod Trivial search by default. Can be 'Fremat'.
--  @return true if prime.
bigint.isPrime = function (self, sMethod)
  if sMethod == 'Fermat' then
    return bigint._primeFermat(self)
  end
  -- default is a simple search
  local v1, _ = bigint._trivialSearch(self)
  return v1 == nil
end
about[bigint.isPrime] = {"B:isPrime(method_s=nil) --> bool",
  "Check if the number is prime. Set 'Fermat' method to use the small Fermat theorem.",
  NUMB}


--- Least common multiple.
--  @param ... List of numbers.
--  @return Bigint lcm.
bigint.lcm = function (_, ...)
  local t = {...}
  if #t == 0 then error('No numbers') end
  -- compare element-wise
  local res = isbigint(t[1]) and t[1] or bigint._newNumber(t[1])
  for i = 2, #t do
    local ti = t[i]
    res = bigint._lcm(res,
                      isbigint(ti) and ti or bigint._newNumber(ti))
  end
  return res
end
about[bigint.lcm] = {":lcm(...) --> B",
  "Find the least common multiple for the given integers.", NUMB}


--- Permutations without repetition.
--  @param n Number of elements.
--  @param k Size of group.
--  @param isRepeat Repetition is allowed.
--  @return Number of permutations.
bigint.P = function (_, n, k, isRepeat)
  n, k = bigint._args(n, k)
  return isRepeat and n^k or bigint:ratF(n, n-k)
end
about[bigint.P] = {":P(n, k, isRepeat=false) --> permutaions_B",
  "Find permutations with or without repetition.", COMB}


--- Generate random number.
--  @return Number from 0 to given number.
bigint.random = function (self)
  local set, v = false, 0
  local res = bigint._newTable({0}, self._sign)
  local b, rr = self._, res._
  local n = math.random(1, #b)
  local any = (n ~= #b)
  for i = n, 1, -1 do
    -- generate
    if any then
      v = math.random(0, BASE-1)
    else
      v = math.random(0, b[i])
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
about[bigint.random] = {"B:random() --> rand_B",
  "Generate pseudo-random value from 0 to B.", help.STATIC}


--- Find ratio of factorials n!/k!
--  @param B Numerator.
--  @param B2 Denominator.
--  @return Bigint for ration.
bigint.ratF = function (_, B, B2)
  assert(B._sign > 0 and B2._sign > 0, "Non-negative expected")
  local d = bigint._sub(B, B2)
  local dd = d._
  if d._sign < 0 then return bigint._0 end
  if #dd == 1 then
    if dd[1] == 0 then return bigint._1 end
    if dd[1] == 1 then return B end
  end
  local binc = B2 + bigint._1
  local S = B * binc
  if dd[1] == 2 then return S end
  local acc = S
  bigint._incr(d, -2)
  while #dd > 1 or dd[1] > 1 do
    S = bigint._sum(S, d)
    acc = bigint._mul(acc, S)
    bigint._incr(d, -2)
  end
  if dd[1] == 1 then
    S = B + binc
    bigint._divBase(S._, BASE, 2)
    acc = bigint._mul(acc, S)
  end
  return acc
end
about[bigint.ratF] = {":ratF(num_B, denom_B) --> num!/denom!",
  "Find ratio of factorials.", COMB}


--- Find !n.
--  @return Subfactorial value.
bigint.subF = function (self)
  assert(self._sign > 0 and (#self._ > 1 or self._[1] > 0), "Positive expected")
  local res = bigint._0
  if #self._ == 1 and self._[1] == 1 then return res end
  local acc, add = bigint._1, true
  local d = bigint._copy(self)
  while true do
    if add then
      res = res + acc
    else
      res = res - acc
    end
    if #d._ == 1 and d._[1] == 2 then break end
    acc = acc * d
    add = not add
    bigint._incr(d, -1)
  end
  res._sign = 1
  return res
end
about[bigint.subF] = {"B:subF() --> !B",
  "Find subfactorial of the number.", COMB}


-- simplify constructor call
setmetatable(bigint, {
__call = function (_, v)
  if isbigint(v) then
    return v
  end
  return bigint._new(v)
end})
about[bigint] = {" (num|str|tbl) --> new_B",
  "Create number from integer, string or table.", help.STATIC}


-- Comment to remove descriptions
bigint.about = about

return bigint

--=================================
