--[[      liblc/bigint.lua 

--- Operations with arbitrary long integer numbers.
--
--  Object structure:           </br> 
--  <code> {SIGN, VALUE} </code></br>
--  where <code>SIGN</code> is +1/-1 and <code>VALUE</code> is a string, each character correspons to one digit.
--  Besides, digits have inverted sequence. For example, number <code>123</code> is represented as <code>"321"</code>.
--  
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

local SIGN, VALUE = 1, 2

--- Check object type.
--  @param v Object.
--  @return True if the object is a big integer.
local function isbigint(v) return type(v) == 'table' and v.isbigint end

--- Convert integer into bigint.
--  @param nVal Integer number.
--  @return Absolute value and sign of the number.
local function fromInt(nVal) 
   nVal = assert(Ver.toInteger(nVal), 'Integer is expected')
   -- return value and sign
   if nVal < 0 then 
      return -nVal, -1
   else
      return nVal, 1
   end
end

--- Convert string into bigint.
--  @param sVal String with integer number.
--  @return Absolute value and sign of the number.
local function fromStr(sVal)
   local m,d = string.match(sVal, '^([+-]?)(%d+)$')               -- get sign and value
   assert(m and d, 'Wrong number!')
   return d, (m == '-' and -1 or 1)
end

--- Reduce front zeros in place. Convert '00123' to '123'.
--  @param tDigits Table with digits.
local function simplify (tDigits)
   local i = #tDigits
   while i > 1 and tDigits[i] == 0 do
      tDigits[i] = nil
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
--  @param num Integer as number or string.
--  @return Bigint object.
bigint.new = function (self, num)
   local s, sign
   if     type(num) == 'string' then
      s, sign = fromStr(num)
   elseif type(num) == 'number' then 
      s, sign = fromInt(num)
   else
      error("Expected integer or string!")
   end
   return setmetatable({sign, string.reverse(s)}, self)
end

--- Correct function arguments if need.
--  @param num1 First number representation.
--  @param num2 Secnod number representation (optional).
--  @return Bigint objects.
bigint._args = function (num1, num2)   
   num1 = isbigint(num1) and num1 or bigint:new(num1)
   if num2 then
      num2 = isbigint(num2) and num2 or bigint:new(num2)
   end
   return num1, num2
end

--- Main algorithm for division.
--  @param num1 First number representation.
--  @param num2 Second number representation.
--  @return The quotient and remainder.
bigint._div_ = function (num1,num2)
   num1,num2 = bigint._args(num1,num2)
   local num = string.reverse(num1[2])          -- numerator as string
   local acc = {}                                  -- result
   local k = #num2[2]                           -- index of the last character
   local rest = bigint:new(string.sub(num, 1, k))  -- current part the of numerator
   local denom = bigint.abs(num2)                  -- denominator
   local last = #denom[2]                       -- length of denominator
   local q = string.sub(denom[2], last)         -- first digit
   -- read the string
   while k <= #num do                             
      if rest >= denom then
         -- get ratio
	 local p = string.sub(rest[2], last):reverse()
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
      rest[2] = string.sub(num, k,k) .. rest[2]
   end
   -- convert result
   local result = bigint:new(num1[1]*num2[1])
   result[2] = (#acc > 0) and string.reverse(table.concat(acc)) or '0'
   return result, rest   
end

--- Get sum of the two positive big numbers.
--  @param B1 First bigint object.
--  @param B2 Second bigint object.
--  @return Sum of the values.
bigint._sum = function (B1,B2)
   local acc, base = {}, bigint.BASE
   -- calculate sum
   for i = 1, math.max(#B1[2],#B2[2]) do
      local ai = string.byte(B1[2], i) or ZERO
      local bi = string.byte(B2[2], i) or ZERO      
      acc[i] = (acc[i] or 0) + (ai-ZERO) + (bi-ZERO)
      if acc[i] >= base then
         acc[i] = acc[i] - base
         acc[i+1] = 1
      end      
   end
   simplify(acc)   -- remove zeros
   local res = bigint:new(0)
   res[2] = table.concat(acc)
   return res
end

--- Get subtraction for two positive bigint numbers.
--  @param B1 First bigint object.
--  @param B2 Second bigint object.
--  @return Difference of the values.
bigint._sub_ = function (B1,B2)
   -- find the biggest
   local p,q,r = B1,B2,1
   if bigint.abs(B1) < bigint.abs(B2) then
      p,q,r = q,p,-1
   end
   local acc = {}
   -- calculate sub
   for i = 1, #p[VALUE] do
      local pi = string.byte(p[2], i) or ZERO
      local qi = string.byte(q[2], i) or ZERO
      acc[i] = (acc[i] or 0) + pi - qi    -- (pi-zero)-(qi-zero)
      if acc[i] < 0 then
         acc[i] = acc[i] + bigint.BASE
         acc[i+1] = -1
      end
   end
   simplify(acc)   -- remove zeros
   local res = bigint:new(r)
   res[2] = table.concat(acc)
   return res
end

--- Absolute value of number.
--  @param v Bigint or integer number.
--  @return Absolute value.
bigint.abs = function (val)
   local a = isbigint(val) and bigint.copy(val) or bigint:new(val)
   a[1] = 1 
   return a
end
bigint.about[bigint.abs] = {"abs(v)", "Return module of arbitrary long number."}

--- Copy of the object.
--  @param v Original bigint object.
--  @return Deep copy.
bigint.copy = function (B)
   local c = bigint:new(B[SIGN])
   c[VALUE] = B[VALUE]
   return c
end
bigint.about[bigint.copy] = {"copy(v)", "Return copy of given number.", help.OTHER}

--- B1 + B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Sum object.
bigint.__add = function (B1,B2)
   B1,B2 = bigint._args(B1,B2) 
   if B1[1] > 0 then
      return (B2[1] > 0) and bigint._sum(B1,B2) or bigint._sub_(B1,B2)
   else 
      return (B2[1] > 0) and bigint._sub_(B2,B1) or -bigint._sum(B1,B2)
   end
end

--- - B
--  @param B Bigint object.
--  @return Opposite value.
bigint.__unm = function (B)
   local res = bigint:new(-B[1])
   res[2] = B[2]
   return res
end

--- B1 - B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Difference object.
bigint.__sub = function (B1, B2)
   B1,B2 = bigint._args(B1,B2)
   if B1[1] > 0 then
      return (B2[1] > 0) and bigint._sub_(B1,B2) or bigint._sum(B1,B2)
   else
      return (B2[1] > 0) and -bigint._sum(B1,B2) or bigint._sub_(B2,B1)
   end
end

-- a * b
bigint.__mul = function (a, b) 
   a,b = bigint._args(a,b)
   local sum = {}
   -- get products   
   for i = 1, #a[VALUE] do
      local ai = string.byte(a[2], i) - ZERO
      for j = 1, #b[VALUE] do
	 local pos = i+j-1
	 sum[pos] = (sum[pos] or 0) + ai*(string.byte(b[2],j)-ZERO)
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
   local res = bigint:new(a[1]*b[1])
   res[2] = table.concat(sum)
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
   return a[1] == b[1] and a[2] == b[2]
end
bigint.about[bigint.eq] = {"eq(a,b)", "Check equality of two values.", help.OTHER}
-- redefine equality
bigint.__eq = bigint.eq

-- a < b
bigint.__lt = function (a,b)
   a,b = bigint._args(a,b)
   if a[1] < b[1] then return true end
   if #a[VALUE] == #b[VALUE] then         -- equial length
      local va, vb = string.reverse(a[2]), string.reverse(b[2])
      return (a[1] > 0 and va < vb) or (a[1] < 0 and va > vb)
   else                                 -- different length
      return (a[1] > 0 and #a[VALUE] < #b[VALUE]) or (a[1] < 0 and #a[VALUE] > #b[VALUE]) 
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
bigint.digits = function (v) return #v[VALUE] end
bigint.about[bigint.digits] = {"digits(v)", "Number of digits, the same as #v.", help.OTHER}
bigint.__len = bigint.digits

-- a ^ b
bigint.__pow = function (a,b)
   a,b = bigint._args(a,b)
   assert(b[1] >= 0, "Power must be non negative!")
   local res = bigint:new(1)
   if b[2] == '0' then 
      assert(a[2] ~= '0', "Error: 0^0!")
      return res
   end
   local aa, bb, q = bigint.copy(a), bigint.copy(b)
   local h, two = 1, bigint:new(2)
   while true do
      bb,q = bigint._div_(bb,two)
      if q[2] ~= '0' then res = bigint.__mul(res,aa) end
      if bb[2] ~= '0' then 
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
   return (v[SIGN] < 0 and '-' or '') .. string.reverse(v[VALUE])
end

--- More convenient string representation
--    @param v Bigint object.
--    @return String representation where each 3 digits are separated.
bigint.str = function (v)
   local value = string.gsub(v[VALUE], '(...)', '%1 ')
   return (v[SIGN] < 0 and '-' or '') .. string.reverse(value)
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
   assert(n[1] > 0, "Non-negative value is expected!")
   local res = bigint:new(1)   
   local one = res:copy()
   while n[2] ~= '0' do   
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
   local value = string.reverse(big[VALUE])
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
   s[#s+1] = string.format("value='%s'", obj[VALUE])
   s[#s+1] = "sign=" .. obj[SIGN]
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
