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
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2023.

	module 'bigint'
--]]


---------------- Tests -----------------
--[[TEST
-- use 'bigint'
Int = require 'matlib.bigint'

-- from integer
a = Int(123)
ans = a:float()               --> 123

-- from string
b = Int('456')
ans = b:float()               --> 456

-- 'default' form for base <= 16
ans = Int('0xABC')            --> Int('10,11,12:16')

-- define base explicitly
g = Int('-1,2,3:10')
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
c = Int(50):F()
ans = c:float() / 3E64       --1> 1.0

ans = (a > b)                 --> false

-- compare with number
ans = a:eq(123)               --> true

-- digits for a different numeric base
v = g:to(60)
ans = tostring(v)             --> '-2,3:60'

-- number of digits
ans = #v                      --> 2

-- 2nd digit (from the lowest)
ans = v[2]                    --> 2

-- base and sign
ans = v.base == 60 and v.sign == -1  --> true

-- print digits
print(v)

-- back to bigint
ans = Int(v)                  --> g

-- comparison
ans = (a ~= g)                --> true

-- simple print
print(a)

-- find permutations
ans = Int:P(10, 5)            --> Int(30240)

-- find combinations
ans = Int:C(10, 3)            --> Int(120)

-- check if it prime
-- iterate though multipliers
ans = Int(1229):isPrime()     --> true

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

local Ver = require("matlib.utils")
local Cross = Ver.cross
Ver = Ver.versions

local SEP = ','
local NUMB = 'numbers'
local COMB = 'combinations'

-- max number for one position
local BASE = 100000


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

}  -- mt_digits


-- Fill inverted mapping
for i, v in ipairs(mt_digits.map) do mt_digits.mapChar[v] = i end


--- String representation.
--  @param t List of digits.
--  @return string.
mt_digits.__tostring = function (t)
  local s, n = nil, #t + 1
  if t.base <= 16 then
    local acc = {}
    for i = 1, #t do acc[i] = mt_digits.map[ t[n-i] ] end
    s = table.concat(acc, '')
  else
    local acc = {}
    for i = 1, #t do acc[i] = t[n-i] end
    s = table.concat(acc, ',')
  end
  local rst = (t.base == 10) and '' or string.format(':%d', t.base)
  return string.format('%s%s%s', t.sign < 0 and '-' or '', s, rst)
end


-- Main module
local bigint = { type='bigint' }


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
    local p = Cross.convert(B1, B2)
    if p then
      return B1 + p
    else
      p = Cross.convert(B2, B1)
      return p and (p + B2) or (Cross.float(B1) + Cross.float(B2))
    end
  end
  if B1._sign > 0 then
    return (B2._sign > 0) and bigint._sum(B1, B2) or bigint._sub(B1, B2)
  else
    return (B2._sign > 0) and bigint._sub(B2, B1) or -bigint._sum(B1, B2)
  end
end


--- B1 / B1
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Ratio object.
bigint.__div = function (B1, B2)
  if not (isbigint(B1) and isbigint(B2)) then
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
  if not (isbigint(B1) and isbigint(B2)) then
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


--- B1 < B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return True if the first value is less then the second one.
bigint.__lt = function (B1, B2)
  if not (isbigint(B1) and isbigint(B2)) then
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
  if B1._sign < B2._sign then return true end
  local b1, b2 = B1._, B2._
  if #b1 == #b2 then   -- equal length
    for i = #b1, 1, -1 do
      if b1[i] ~= b2[i] then
        return (B1._sign > 0 and b1[i] < b2[i]) or
               (B1._sign < 0 and b1[i] > b2[i])
      end
    end
    return false
  else                 -- different length
    return (B1._sign > 0 and #b1 < #b2) or (B1._sign < 0 and #b1 > #b2)
  end
end


--- B1 % B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Remainder object.
bigint.__mod = function (B1, B2)
  if not (isbigint(B1) and isbigint(B2)) then
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
  if not (isbigint(B1) and isbigint(B2)) then
    local p = Cross.convert(B1, B2)
    if p then
      return B1 * p
    else
      p = Cross.convert(B2, B1)
      return p and (p * B2) or (Cross.float(B1) * Cross.float(B2))
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
    local p = Cross.convert(B1, B2)
    if p then
      return B1 ^ p
    else
      p = Cross.convert(B2, B1)
      return p and (p ^ B2) or (Cross.float(B1) ^ Cross.float(B2))
    end
  end
  if B2._sign < 0 then error('Negative power!') end
  local y, x = bigint._1, B1
  if bigint._isZero(B2) then
    assert(not bigint._isZero(B1), "Error: 0^0!")
    return bigint._1
  end
  local dig, mul, rest = {}, bigint._mul, nil
  for i = 1, #B2._ do dig[i] = B2._[i] end
  while #dig > 1 or dig[1] > 1 do
    dig, rest = bigint._divBase(dig, BASE, 2)
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
    local p = Cross.convert(B1, B2)
    if p then
      return B1 - p
    else
      p = Cross.convert(B2, B1)
      return p and (p - B2) or (Cross.float(B1) - Cross.float(B2))
    end
  end
  if B1._sign > 0 then
    return (B2._sign > 0) and bigint._sub(B1, B2) or bigint._sum(B1, B2)
  else
    return (B2._sign > 0) and -bigint._sum(B1, B2) or bigint._sub(B2, B1)
  end
end


--- - B
--  @param B Bigint object.
--  @return Opposite value.
bigint.__unm = function (B) return bigint._newTable(B._, -B._sign) end


--- String representation.
--  @param B Bigint object.
--  @return String object.
bigint.__tostring = function (B)
  local t = {B._sign < 0 and '-' or ''}
  for i = #B._, 1, -1 do
    t[#t+1] = tonumber(B._[i])
  end
  return table.concat(t, '')
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


--- Try to convert object into bigint.
--  @param v Source objec.
--  @return Int object.
bigint._convert = function (v)
  return Ver.isInteger(v) and bigint._newNumber(v) or nil
end


--- Deep copy of the object.
--  @param B Original bigint object.
--  @return Deep copy.
bigint._copy = function (B)
  local c, b = {}, B._
  for i = 1, #b do c[i] = b[i] end
  return bigint._newTable(c, B._sign)
end


--- In-place decrement for number.
--  @param B Number to decrease by 1.
--  @param forced Flag to skip sign check.
bigint._decr = function (B, forced)
  local b = B._
  if not forced and (B._sign < 0 or #b == 1 and b[1] == 0) then
    B._sign = -1
    return bigint._incr(B, true)
  end
  for i = 1, math.huge do
    local v = b[i] - 1
    if v < 0 then
      v = BASE - 1
    else
      b[i] = v
      break
    end
    b[i] = v
  end
  if #b > 1 and b[#b] <= 0 then
    table.remove(b)
  elseif #b == 1 and b[1] == 0 then
    B._sign = 1
  end
end


--- Main algorithm for division.
--  @param B1 First number representation.
--  @param B2 Second number representation.
--  @return The quotient and remainder.
bigint._div = function (B1, B2)
  local b1, b2 = B1._, B2._
  if bigint._isZero(B2) then error("Divide by 0!") end
  local res = bigint._newTable({0}, 1)
  if #b1 < #b2 then  -- too short
    return res, B1
  end
  local k = #b1 - #b2 + 1
  local rem = bigint._newTable({}, 1)
  Ver.move(b1, k+1, #b1, 1, rem._)  -- copy last elements
  local v2, den, acc = B2:float(), B2:abs(), {}
  for i = k, 1, -1 do
    table.insert(rem._, 1, b1[i])
    if rem >= den then
      local n = math.modf(rem:float() / v2)  -- estimate
      local tmp = rem - den * bigint._newTable({n}, 1)
      if tmp._sign < 0 then
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
  return bigint._isZero(B1) and B2 or bigint._gcd(B2 % B1, B1)
end


--- Check if the first number is greater then the second
--  @param B1 First number.
--  @param B2 Second number.
--  @return True if B1 > B2.
bigint._gt = function (B1, B2)
  if B1._sign > B2._sign then return true end
  local b1, b2 = B1._, B2._
  if #b1 == #b2 then
    for i = #b1, 1, -1 do
      if b1[i] ~= b2[i] then
        return (B1._sign > 0 and b1[i] > b2[i]) or
               (B1._sign < 0 and b1[i] < b2[i])
      end
    end
    return false
  else
    return (B1._sign > 0 and #b1 > #b2) or (B1._sign < 0 and #b1 < #b2)
  end
end


--- In-place increment.
--  @param B Number to increase by 1.
--  @param forced Flag to skip sign check.
bigint._incr = function (B, forced)
  if not forced and (B._sign < 0) then
    return bigint._decr(B, true)
  end
  local b = B._
  for i = 1, math.huge do
    local v = (b[i] or 0) + 1
    if v == BASE then
      v = 0
    else
      b[i] = v
      break
    end
    b[i] = v
  end
end


--- Check for zero value.
--  @param B Bigint number.
--  @return true when B == 0
bigint._isZero = function (B) return #B._ == 1 and B._[1] == 0 end


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
    rest = math.modf(si / BASE)
    s[i] = si - rest * BASE
  end
  if rest > 0 then s[#s+1] = rest end
  return sum
end


--- Create new object, set metatable.
--  @param num Integer, string or table.
--  @return Bigint object.
bigint._new = function (num)
  -- prepare
  if type(num) == 'table' then
    assert(#num > 0, "Wrong input")
    local base = num.base
    assert(base and base > 0 and Ver.isInteger(base), "Wrong base")
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
  elseif type(num) == 'number' and Ver.isInteger(num) then
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
      acc[#acc+1] = assert(mt_digits.mapChar[dig])
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
  _, rest = div(mul(x,y), B3)
  return rest
end


--- Check if it prime using the Fermat theorem.
--  @param B Number.
--  @return true if prime.
bigint._primeFermat = function (B)
  for i = 1, 5 do
    local a = nil
    repeat
      a = bigint:random(B)
    until a:float() >= 2
    local v1 = bigint._powm(a, B, B)
    local _, v2 = bigint._div(a, B)
    if v1 ~= v2 then return false end
  end
  return true
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
  for i, v in ipairs(t) do dig[i] = v end
  repeat
    dig, res[#res+1] = bigint._divBase(dig, Nfrom, Nto)
  until #dig == 0
  return res
end


--- Estimate square root using Babylonian method.
--  @param B Bigint object.
--  @return Estimation of sqrt(B).
bigint._sqrt = function (B)
  local ai = bigint._1
  local sum, sub = bigint._sum, bigint._sub
  repeat
    local aii, _ = bigint._div(B, ai)
    aii._ = bigint._divBase(sum(ai, aii)._, BASE, 2)
    ai, aii = aii, sub(aii, ai)
  until #aii._ == 1 and (aii._[1] <= 1)   -- TODO: check and decrease if need
  return ai
end


--- Get subtraction for two positive bigint numbers.
--  @param B1 First bigint object.
--  @param B2 Second bigint object.
--  @return Difference of the values.
bigint._sub = function (B1, B2)
  local r = 1
  local res = bigint._newTable({0}, 1)
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
      rr[i], sub = v + BASE, 1
    else
      rr[i], sub = v, 0
    end
  end
  -- simplify
  while #rr > 1 and rr[#rr] == 0 do rr[#rr] = nil end
  res._sign = r
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
  while #sq._ > #n._ or not bigint._gt(n, sq) do
    local v1, v2 = bigint._div(B, n)
    if bigint._isZero(v2) then 
      return n, v1
    end
    bigint._incr(n)
  end
  return nil  -- not found
end


--- Common return values
bigint._1 = bigint._newTable({1}, 1)
bigint._0 = bigint._newTable({0}, 1)


--- Absolute value of number.
--  @param B Bigint or integer number.
--  @return Absolute value.
bigint.abs = function (B) return bigint._newTable(B._, 1) end
about[bigint.abs] = {"B:abs() --> num", "Return module of arbitrary long number."}


--- Get sign of the number.
--  @return sign in form -1/0/+1.
bigint.sign = function (self) return self._sign end
about[bigint.sign] = {"B:sign() --> int", "Return +1/0/-1."}


--- Change current numeric base.
--  @param B Bigint object.
--  @param N New base.
--  @return Table with digits of the found number.
bigint.to = function (B, N)
  N = N or BASE
  assert(Ver.isInteger(N) and N > 0, "Wrong base")
  local b = B._
  local res = bigint._rebase(b, BASE, N)
  res.sign = B._sign
  return setmetatable(res, mt_digits)
end
about[bigint.to] = {"B:to(N) --> tbl", "Convert number to the new numeric base."}


--- Find number of combinations.
--  @param self Do nothing.
--  @param n Total number of elements.
--  @param k Group size.
--  @return Bigint for combination number.
bigint.C = function (self, n, k)
  n, k = bigint._args(n, k)
  return bigint.ratF(n, k) / bigint.F(n-k)
end
about[bigint.C] = {":C(n, k) --> combinations_B",
  "Number of combinations C(n,k).", COMB}


--- a == b.
--  In Lua v == 0 is always false because in the case of number
--  the program tries to convert everything into number.
--  For two bigint objects using of <code>==</code> is also possible.
--  @param B1 First bigint object or integer.
--  @param B2 Second bigint object or integer.
--  @return True if numbers have the same values and signs.
bigint.eq = function (B1, B2)
  if not (isbigint(B1) and isbigint(B2)) then
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
  if #b1 == #b2 and B1._sign == B2._sign then
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
--  Use the fact that n*(n-1)*...*2*1 = (n*1)*((n-1)*2)*...
--  @param B Bigint object.
--  @return Factorial of the number as bigint object.
bigint.F = function (B)
  assert(B._sign > 0, "Non-negative value is expected!")
  local N = B:float()
  if     N <= 1 then return bigint._1
  elseif N == 2 then return B
  end
  local n, m = math.modf((N-2) * 0.5)
  local S, d, acc = B, B:_copy(), B
  for i = 1, n do
    bigint._decr(d)
    bigint._decr(d)
    S = bigint._sum(S, d)
    acc = bigint._mul(acc, S)
  end
  if m > 1E-3 then   -- i.e. m > 0
    acc = bigint._mul(acc, bigint._newNumber(n + 2))
  end
  return acc
end
about[bigint.F] = {"B:F() --> B!",
  "Return factorial of non-negative integer B.", COMB}


--- Find multipliers for the number.
--  @param B Integer number.
--  @return List of prime numbers.
bigint.factorize = function (B)
  local v, res = B, {}
  if B._sign < 0 then res[1] = -1 end
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
  local b, res = B._, 0
  if #b > 1 then
    res = (b[#b]*BASE + b[#b-1]) * BASE^(#b-2)
  else
    res = b[1]
  end
  return B._sign >= 0 and res or (-res)
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
  local v1, _ = bigint._trivialSearch(B)
  return v1 == nil
end
about[bigint.isPrime] = {"B:isPrime([method_s]) --> bool",
  "Check if the number is prime. Set 'Fermat' method to use the small Fermat theorem.",
  NUMB}


--- Permutations without repetition.
--  @param self Do nothing.
--  @param n Number of elements.
--  @param k Size of group.
--  @return Number of permutations.
bigint.P = function (self, n, k)
  n, k = bigint._args(n, k)
  return bigint.ratF(n, n-k)
end
about[bigint.P] = {":P(n, k) --> permutaions_B",
  "Find permutations without repetition.", COMB}


--- Generate random number.
--  @param self Do nothing.
--  @param B Upper limit.
--  @return Number from 0 to B.
bigint.random = function (self, B)
  B = isbigint(B) and B or bigint._new(B)
  local set, v = false, 0
  local res = bigint._newTable({0}, B._sign)
  local n = math.random(1, #B._)
  local b, rr = B._, res._
  local any = (n ~= #b)
  for i = n, 1, -1 do
    -- generate
    if any then
      v = math.random(1, BASE) - 1
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


--- Find ratio of factorials n!/k!
--  @param B Numerator.
--  @param B2 Denominator.
--  @return Bigint for ration.
bigint.ratF = function (B, B2)
  assert(B._sign > 0 and B2._sign > 0, "Non-negative expected")
  local N1, N2 = B:float(), B2:float()
  if N1 < N2 then return bigint._0 end
  if N1 == N2 then return bigint._1 end
  if N1 == N2 + 1 then return B end
  local acc = B * (B2 + bigint._1)
  if N1 == N2+2 then return acc end
  local S, diff = acc, B - B2
  local n, m = math.modf((N1+N2-2) * 0.5)
  for i = N2+1, n do
    bigint._decr(diff)
    bigint._decr(diff)
    S = bigint._sum(S, diff)
    acc = bigint._mul(acc, S)
  end
  if m > 1E-3 then   -- i.e. m > 0
    acc = bigint._mul(acc, bigint._newNumber(n+2))
  end
  return acc
end
about[bigint.ratF] = {":ratF(num_B, denom_B) --> num!/denom!",
  "Find ratio of factorials num!/denom!.", COMB}


-- simplify constructor call
setmetatable(bigint, {
__call = function (self, v)
  return bigint._new(v)
end})
about[bigint] = {" (var) --> new_B",
  "Create number from integer, string or table.", help.STATIC}


-- Comment to remove descriptions
bigint.about = about

return bigint

--=================================
-- TODO check F and ratF for different float() 
