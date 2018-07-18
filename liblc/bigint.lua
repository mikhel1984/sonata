--[[      liblc/bigint.lua 

--- Operations with arbitrary long integer numbers.
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

            module 'bigint'
--]]

---------------- Tests -----------------
--[[!!
-- import 'bigint'
Big = require 'liblc.bigint'

-- from integer
a = Big(123)         
ans = a:tonumber()           --> 123

-- from string
b = Big('456')        
ans = b:tonumber()           --> 456

-- arithmetical operations
ans = Big.tonumber(a+b)      --> 579

ans = Big.tonumber(a-b)      --> -333

ans = Big.tonumber(a*Big(2)) --> 246

ans = Big.tonumber(b/2)      --> 228

ans = Big.tonumber(b%a)      --> 87

ans = Big.tonumber(a^3)      --> 1860867

-- absolute value
ans = Big.tonumber(Big.abs('-25')) --> 25

-- factorial
c = Big(10):fact()
ans = Big.tonumber(c)  --> 3628800

-- make copy, comparision
d = a:copy()
ans = (a == d)               --> true

ans = (a > b)                --> false

ans = (a == b)               --> false

-- compare with number
ans = a:eq(123)              --> true

-- number of digits
-- in Lua5.1 use 'digits' method
ans = #a                     --> 3

-- simple print
print(a)

-- more friendly representation
print(c:str())
]]

--	LOCAL

local Ver = require "liblc.versions"

-- Check object type.
local function isbigint(v) return type(v) == 'table' and v.isbigint end

-- Convert integer into bigint.
local function fromInt(n) 
   n = assert(Ver.toInteger(n), 'Integer is expected')
   -- return value and sign
   if n < 0 then 
      return -n, -1
   else
      return n, 1
   end
end

-- Convert string into bigint.
local function fromStr(s)
   local m,d = string.match(s, '^([+-]?)(%d+)$')               -- get sign and value
   assert(m and d, 'Wrong number!')
   return d, (m == '-' and -1 or 1)
end

-- Reduce front zeros. Convert '00123' to '123'.
local function simplify (tdigits)
   local i = #tdigits
   while i > 1 and tdigits[i] == 0 do
      tdigits[i] = nil
      i = i - 1
   end
end

local ZERO = string.byte('0')

--	INFO 

local help = lc_version and (require "liblc.help") or {new=function () return {} end}

--	MODULE

local bigint = {
-- mark
type='bigint', isbigint=true,
-- description
about = help:new("Operations with arbitrary long integers."),
}

bigint.__index = bigint
-- the radix
bigint.BASE = 10        
bigint.about[bigint.BASE] = {'BASE', "The radix of big integer representation.", help.OTHER}

--- Create new object, set metatable.
--    @param a Integer as number or string.
--    @return Bigint object.
bigint.new = function (self, a)
   local s, sign
   if     type(a) == 'string' then
      s, sign = fromStr(a)
   elseif type(a) == 'number' then 
      s, sign = fromInt(a)
   else
      error("Expected integer or string!")
   end
   return setmetatable({value = string.reverse(s), sign = sign}, self)
end


-- Correct function arguments if need.
bigint._args = function (a, b)   
   a = isbigint(a) and a or bigint:new(a)
   if b then
      b = isbigint(b) and b or bigint:new(b)
   end
   return a, b
end

-- Main algorithm for division.
bigint._div_ = function (a,b)
   a,b = bigint._args(a,b)
   local num = string.reverse(a.value)             -- numerator as string
   local acc = {}                                  -- result
   local k = #b.value                              -- index of the last character
   local rest = bigint:new(string.sub(num, 1, k))  -- current part the of numerator
   local denom = bigint.abs(b)                     -- denominator
   local last = #denom.value                       -- length of denominator
   local q = string.sub(denom.value, last) -- first digit
   -- read the string
   while k <= #num do                             
      if rest >= denom then
         -- get ratio
	 local p = string.sub(rest.value, last):reverse()
	 local n = math.modf(p/q)
	 local prod = bigint.__mul(n,denom)        -- n * denom
	 -- save result
	 if bigint.__le(prod, rest) then           -- prod <= rest
	    acc[#acc+1] = n
	    rest = bigint.__sub(rest, prod)        -- rest - prod
	 else
	    acc[#acc+1] = n-1
	    rest = bigint.__sub(rest,prod)+denom   -- rest-prod+denom
	 end
      else
         if #acc > 0 then acc[#acc+1] = 0 end
      end
      k = k+1
      -- update current numerator
      rest.value = string.sub(num, k,k) .. rest.value
   end
   -- convert result
   local result = bigint:new(a.sign*b.sign)
   result.value = (#acc > 0) and string.reverse(table.concat(acc)) or '0'
   return result, rest   
end

-- Get sum of the two positive bigint numbers.
bigint._sum = function (big1,big2)
   local acc, base = {}, bigint.BASE
   -- calculate sum
   for i = 1, math.max(#big1.value,#big2.value) do
      local ai = string.byte(big1.value, i) or ZERO
      local bi = string.byte(big2.value, i) or ZERO      
      acc[i] = (acc[i] or 0) + (ai-ZERO) + (bi-ZERO)
      if acc[i] >= base then
         acc[i] = acc[i] - base
         acc[i+1] = 1
      end      
   end
   simplify(acc)   -- remove zeros
   local res = bigint:new(0)
   res.value = table.concat(acc)
   return res
end

-- Get subtraction for two positive bigint numbers.
bigint._sub_ = function (big1,big2)
   -- find the biggest
   local p,q,r = big1,big2,1
   if bigint.abs(big1) < bigint.abs(big2) then
      p,q,r = q,p,-1
   end
   local acc = {}
   -- calculate sub
   for i = 1, #p.value do
      local pi = string.byte(p.value, i) or ZERO
      local qi = string.byte(q.value, i) or ZERO
      acc[i] = (acc[i] or 0) + pi - qi    -- (pi-zero)-(qi-zero)
      if acc[i] < 0 then
         acc[i] = acc[i] + bigint.BASE
         acc[i+1] = -1
      end
   end
   simplify(acc)   -- remove zeros
   local res = bigint:new(r)
   res.value = table.concat(acc)
   return res
end

--- Absolute value of number.
--    @param v Bigint or integer number.
--    @return Absolute value.
bigint.abs = function (v)
   local a = isbigint(v) and bigint.copy(v) or bigint:new(v)
   a.sign = 1 
   return a
end
bigint.about[bigint.abs] = {"abs(v)", "Return module of arbitrary long number."}

--- Copy of the object.
--    @param v Original bigint object.
--    @return Deep copy.
bigint.copy = function (big)
   local c = bigint:new(big.sign)
   c.value = big.value
   return c
end
bigint.about[bigint.copy] = {"copy(v)", "Return copy of given number.", help.OTHER}

-- a + b
bigint.__add = function (a,b)
   a,b = bigint._args(a,b) 
   if a.sign > 0 then
      return (b.sign > 0) and bigint._sum(a,b) or bigint._sub_(a,b)
   else 
      return (b.sign > 0) and bigint._sub_(b,a) or -bigint._sum(a,b)
   end
end

-- -v
bigint.__unm = function (big)
   local res = bigint:new(-big.sign)
   res.value = big.value
   return res
end

-- a - b
bigint.__sub = function (a, b)
   a,b = bigint._args(a,b)
   if a.sign > 0 then
      return (b.sign > 0) and bigint._sub_(a,b) or bigint._sum(a,b)
   else
      return (b.sign > 0) and -bigint._sum(a,b) or bigint._sub_(b,a)
   end
end

-- a * b
bigint.__mul = function (a, b) 
   a,b = bigint._args(a,b)
   local sum = {}
   -- get products   
   for i = 1, #a.value do
      local ai = string.byte(a.value, i) - ZERO
      for j = 1, #b.value do
	 local pos = i+j-1
	 sum[pos] = (sum[pos] or 0) + ai*(string.byte(b.value,j)-ZERO)
      end
   end
   -- back
   local d = 0
   for i = 1, #sum do
      sum[i] = sum[i] + d
      d = math.floor(sum[i] / bigint.BASE)
      sum[i] = math.floor(sum[i] % bigint.BASE)
   end
   sum[#sum+1] = d
   simplify(sum)
   -- save
   local res = bigint:new(a.sign*b.sign)
   res.value = table.concat(sum)
   return res   
end

-- a / b
bigint.__div = function (a, b)
   local res,_ = bigint._div_(a,b)
   return res
end

-- a % b
bigint.__mod = function (a, b)
   local _,res = bigint._div_(a,b)
   return res
end

--- a == b.
--    In Lua v == 0 is always <code>false</code> because in case of number
--    the program tries to convert everything into number.
--    For two bigint objects using of <code>==</code> is also possible.
--    @param a First bigint object or integer.
--    @param b Second bigint object or integer.
--    @return <code>true</code> if numbers have the same values and signs.
bigint.eq = function (a,b)
   a,b = bigint._args(a,b)
   return a.sign == b.sign and a.value == b.value
end
bigint.about[bigint.eq] = {"eq(a,b)", "Check equality of two values.", help.OTHER}
-- redefine equality
bigint.__eq = bigint.eq

-- a < b
bigint.__lt = function (a,b)
   a,b = bigint._args(a,b)
   if a.sign < b.sign then return true end
   if #a.value == #b.value then         -- equial length
      local va, vb = string.reverse(a.value), string.reverse(b.value)
      return (a.sign > 0 and va < vb) or (a.sign < 0 and va > vb)
   else                                 -- different length
      return (a.sign > 0 and #a.value < #b.value) or (a.sign < 0 and #a.value > #b.value) 
   end
end

-- a <= b
bigint.__le = function (a,b)
   a,b = bigint._args(a,b)
   return bigint.__eq(a,b) or bigint.__lt(a,b)
end

--- #a 
--    @param v Bigint object.
--    @return Number of digits.
bigint.digits = function (v) return #v.value end
bigint.about[bigint.digits] = {"digits(v)", "Number of digits, the same as #v.", help.OTHER}
bigint.__len = bigint.digits

-- a ^ b
bigint.__pow = function (a,b)
   a,b = bigint._args(a,b)
   assert(b.sign >= 0, "Power must be non negative!")
   local res = bigint:new(1)
   if b.value == '0' then 
      assert(a.value ~= '0', "Error: 0^0!")
      return res
   end
   local aa, bb, q = bigint.copy(a), bigint.copy(b)
   local h, two = 1, bigint:new(2)
   while true do
      bb,q = bigint._div_(bb,two)
      if q.value ~= '0' then res = bigint.__mul(res,aa) end
      if bb.value ~= '0' then 
         aa = bigint.__mul(aa,aa)
      else break end
   end
   return res
end

bigint.arithmetic = 'arithmetic'
bigint.about[bigint.arithmetic] = {bigint.arithmetic, "a+b, a-b, a*b, a/b, a%b, a^b, -a, #a", help.META}
bigint.comparison = 'comparison'
bigint.about[bigint.comparison] = {bigint.comparison, "a<b, a<=b, a>b, a>=b, a==b, a~=b", help.META}

-- String representation.
bigint.__tostring = function (v)
   return (v.sign < 0 and '-' or '') .. string.reverse(v.value)
end

--- More convenient string representation
--    @param v Bigint object.
--    @return String representation where each 3 digits are separated.
bigint.str = function (v)
   local value = string.gsub(v.value, '(...)', '%1 ')
   return (v.sign < 0 and '-' or '') .. string.reverse(value)
end
bigint.about[bigint.str] = {"str(v)", "More readable string representation of the number", help.OTHER}

--- Float number representation.
--    @param v Bigint object.
--    @return Integer if possible, otherwise float point number.
bigint.tonumber = function (v)
   return tonumber(tostring(v))
end
bigint.about[bigint.tonumber] = {"tonumber(v)", "Represent current big integer as number if it possible."}

--- m!
--    @param m Bigint object or integer.
--    @return Factorial of the number as bigint object.
bigint.fact = function (m)
   local n = isbigint(m) and m:copy() or bigint:new(m)
   assert(n.sign > 0, "Non-negative value is expected!")
   local res = bigint:new(1)   
   local one = res:copy()
   while n.value ~= '0' do   
      res = bigint.__mul(res, n)
      n = bigint.__sub(n, one)
   end
   return res
end
bigint.about[bigint.fact] = {"fact(n)", "Return factorial of non-negative integer n."}

--[[
bigint.base = function (big, base)
   local letterA = string.byte('A')
   local res = {}
   local value = string.reverse(big.value)
   local acc = 0
   for i = 1,#value do
      local d = string.byte(value,i)
      if d >= letterA then d = d - letterA + 10 else d = d - ZERO end
   end
end
]]

-- simplify constructor call
setmetatable(bigint, {__call = function (self, v) return bigint:new(v) end})
bigint.Big = 'Big'
bigint.about[bigint.Big] = {"Big(v)", "Create big number from integer or string.", help.NEW}

--- Bigint serialization.
--    @param obj Bigint object.
--    @return String, suitable for exchange.
bigint.serialize = function (obj)
   local s = {}
   s[#s+1] = string.format("value='%s'", obj.value)
   s[#s+1] = "sign=" .. obj.sign
   s[#s+1] = "metatablename='Big'"
   s[#s+1] = "modulename='bigint'"
   return string.format("{%s}", table.concat(s, ','))
end
bigint.about[bigint.serialize] = {"serialize(obj)", "Save internal representation of bigint object.", help.OTHER}

-- free memory if need
if not lc_version then bigint.about = nil end

return bigint

--=================================
-- TODO: work with different bases
