--[[      sonatalib/bigint.lua 

--- Operations with arbitrary long integer numbers.
--
--  Object structure:           </br> 
--  <code> {SIGN, VALUE} </code></br>
--  where <code>SIGN</code> is +1/-1 and <code>VALUE</code> is a string, each character corresponds to one digit.
--  Besides, digits have inverted sequence. For example, number <code>123</code> is represented as <code>"321"</code>.
--  
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonatalib</a> collection, 2017-2019.

            module 'bigint'
--]]

---------------- Tests -----------------
--[[TEST
-- import 'bigint'
Big = require 'sonatalib.bigint'

-- from integer
a = Big(123)         
ans = a:val()           --> 123

-- from string
b = Big('456')        
ans = b:val()           --> 456

-- arithmetical operations
ans = Big.val(a+b)      --> 579

ans = Big.val(a-b)      --> -333

ans = Big.val(a*Big(2)) --> 246

ans = Big.val(b/2)      --> 228

ans = Big.val(b%a)      --> 87

ans = Big.val(a^3)      --> 1860867

-- absolute value
ans = Big.val(Big.abs('-25')) --> 25

-- factorial
c = Big(10):fact()
ans = Big.val(c)  --> 3628800

-- make copy, comparison
d = a:copy()
ans = (a == d)               --> true

ans = (a > b)                --> false

ans = (a == b)               --> false

-- compare with number
ans = a:eq(123)              --> true

-- number of digits
-- (the same as #a) 
ans = a:size()               --> 3

-- simple print
print(a)

-- more friendly representation
print(c:str())
-- set number of digits in group
print(c:str(6))

--]]

--	LOCAL

local Ver = require "sonatalib.versions"

local SIGN, VALUE = 1, 2

local ZERO = string.byte('0')

--- Check object type.
--  @param v Object.
--  @return True if the object is a big integer.
local function isbigint(v) return type(v) == 'table' and v.isbigint end

--- Reduce front zeros in place. Convert '00123' to '123'.
--  @param tDigits Table with digits.
local function simplify (tDigits)
   local i = #tDigits
   while i > 1 and tDigits[i] == 0 do
      tDigits[i] = nil
      i = i - 1
   end
end

--	INFO 

local help = LC_DIALOG and (require "sonatalib.help") or {new=function () return {} end}

--	MODULE

local bigint = {
-- mark
type='bigint', isbigint=true,
-- description
about = help:new("Operations with arbitrary long integers."),
}

bigint.__index = bigint
-- the basis
bigint.BASE = 10        

--- Create new object, set metatable.
--  @param num Integer as number or string.
--  @return Bigint object.
bigint.new = function (self, num)
   local s, sign
   -- prepare
   if type(num) == 'string' then
      sign, s = string.match(num, '^([+-]?)(%d+)$')
      sign = (sign == '-') and -1 or 1
   elseif type(num) == 'number' then 
      s = Ver.toInteger(num)
      sign = (num < 0) and -1 or 1
   end
   -- check result
   if not s then
      error('Wrong number '..tostring(num))
   end
   return setmetatable({sign, string.reverse(s)}, self)
end

--- Correct function arguments if need.
--  @param num1 First number representation.
--  @param num2 Second number representation (optional).
--  @return Bigint objects.
bigint._args_ = function (num1, num2)   
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
   num1,num2 = bigint._args_(num1,num2)
   local num = string.reverse(num1[2])          -- numerator as string
   local acc = {}                                  -- result
   local k = #num2[2]                           -- index of the last character
   local rest = bigint:new(string.sub(num, 1, k))  -- current part the of numerator
   local denom = bigint.abs(num2)                  -- denominator
   local last = #denom[2]                       -- length of denominator
   local q = string.sub(denom[2], last)         -- first digit
   local mul, sub, le = bigint.__mul, bigint.__sub, bigint.__le
   -- read the string
   while k <= #num do                             
      if rest >= denom then
         -- get ratio
	 local p = string.sub(rest[2], last):reverse()
	 local n = math.modf(p/q)
	 local prod = mul(n,denom)                 -- n * denom
	 -- save result
	 if le(prod, rest) then                    -- prod <= rest
	    acc[#acc+1] = n
	    rest = sub(rest, prod)                 -- rest - prod
	 else
	    acc[#acc+1] = n-1
	    rest = sub(rest,prod)+denom            -- rest-prod+denom
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
bigint._sum_ = function (B1,B2)
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
   local acc, base = {}, bigint.BASE
   -- calculate sub
   for i = 1, #p[VALUE] do
      local pi = string.byte(p[2], i) or ZERO
      local qi = string.byte(q[2], i) or ZERO
      acc[i] = (acc[i] or 0) + pi - qi    -- (pi-zero)-(qi-zero)
      if acc[i] < 0 then
         acc[i] = acc[i] + base
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
   B1,B2 = bigint._args_(B1,B2) 
   if B1[1] > 0 then
      return (B2[1] > 0) and bigint._sum_(B1,B2) or bigint._sub_(B1,B2)
   else 
      return (B2[1] > 0) and bigint._sub_(B2,B1) or -bigint._sum_(B1,B2)
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
   B1,B2 = bigint._args_(B1,B2)
   if B1[1] > 0 then
      return (B2[1] > 0) and bigint._sub_(B1,B2) or bigint._sum_(B1,B2)
   else
      return (B2[1] > 0) and -bigint._sum_(B1,B2) or bigint._sub_(B2,B1)
   end
end

--- B1 * B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Product object.
bigint.__mul = function (B1, B2) 
   B1,B2 = bigint._args_(B1,B2)
   local sum = {}
   -- get products   
   for i = 1, #B1[VALUE] do
      local ai = string.byte(B1[2], i) - ZERO
      for j = 1, #B2[VALUE] do
	 local pos = i+j-1
	 sum[pos] = (sum[pos] or 0) + ai*(string.byte(B2[2],j)-ZERO)
      end
   end
   -- back
   local d, base = 0, bigint.BASE
   for i = 1, #sum do
      sum[i] = sum[i] + d
      d = math.floor(sum[i] / base)
      sum[i] = math.floor(sum[i] % base)
   end
   sum[#sum+1] = d
   simplify(sum)
   -- save
   local res = bigint:new(B1[1]*B2[1])
   res[2] = table.concat(sum)
   return res   
end

--- B1 / B1
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Ratio object.
bigint.__div = function (B1, B2)
   local res,_ = bigint._div_(B1,B2)
   return res
end

--- B1 % B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Remainder object.
bigint.__mod = function (B1, B2)
   local _,res = bigint._div_(B1,B2)
   return res
end

--- a == b.
--  In Lua v == 0 is always <code>false</code> because in case of number
--  the program tries to convert everything into number.
--  For two bigint objects using of <code>==</code> is also possible.
--  @param B1 First bigint object or integer.
--  @param B2 Second bigint object or integer.
--  @return <code>true</code> if numbers have the same values and signs.
bigint.eq = function (B1,B2)
   B1,B2 = bigint._args_(B1,B2)
   return B1[1] == B2[1] and B1[2] == B2[2]
end
bigint.about[bigint.eq] = {"eq(a,b)", "Check equality of two values.", help.OTHER}
-- redefine equality
bigint.__eq = bigint.eq

--- B1 < B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return True if the first value is less then the second one.
bigint.__lt = function (B1,B2)
   B1,B2 = bigint._args_(B1,B2)
   if B1[1] < B2[1] then return true end
   if #B1[VALUE] == #B2[VALUE] then         -- equal length
      local va, vb = string.reverse(B1[2]), string.reverse(B2[2])
      return (B1[1] > 0 and va < vb) or (B1[1] < 0 and va > vb)
   else                                 -- different length
      return (B1[1] > 0 and #B1[VALUE] < #B2[VALUE]) or (B1[1] < 0 and #B1[VALUE] > #B2[VALUE]) 
   end
end

--- a <= b
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return True if the first value is less or equal to the second.
bigint.__le = function (B1,B2)
   B1,B2 = bigint._args_(B1,B2)
   return bigint.__eq(B1,B2) or bigint.__lt(B1,B2)
end

--- #a 
--  @param B Bigint object.
--  @return Number of digits.
bigint.size = function (B) return #B[VALUE] end
bigint.about[bigint.size] = {"size(B)", "Number of digits, the same as #B.", help.OTHER}
bigint.__len = bigint.size

--- B1 ^ B2
--  @param B1 First bigint or integer.
--  @param B2 Second bigint or integer.
--  @return Power of the number.
bigint.__pow = function (B1,B2)
   B1,B2 = bigint._args_(B1,B2)
   if B2[1] < 0 then error('Negative power!') end
   local res = bigint:new(1)
   if B2[2] == '0' then 
      assert(B1[2] ~= '0', "Error: 0^0!")
      return res
   end
   local aa, bb, q = bigint.copy(B1), bigint.copy(B2)
   local h, two = 1, bigint:new(2)
   local div, mul = bigint._div_, bigint.__mul
   while true do
      bb,q = div(bb,two)
      if q[2] ~= '0' then res = mul(res,aa) end
      if bb[2] ~= '0' then 
         aa = mul(aa,aa)
      else break end
   end
   return res
end

bigint.arithmetic = 'arithmetic'
bigint.about[bigint.arithmetic] = {bigint.arithmetic, "a+b, a-b, a*b, a/b, a%b, a^b, -a, #a", help.META}
bigint.comparison = 'comparison'
bigint.about[bigint.comparison] = {bigint.comparison, "a<b, a<=b, a>b, a>=b, a==b, a~=b", help.META}

--- String representation.
--  @param B Bigint object.
--  @return String object.
bigint.__tostring = function (B)
   return (B[SIGN] < 0 and '-' or '') .. string.reverse(B[VALUE])
end

--- More convenient string representation
--  @param B Bigint object.
--  @param n Number of digits in group (default is 3).
--  @return String representation where each n digits are separated.
bigint.str = function (B,n)
   n = n or 3
   local templ = string.format('(%s)', string.rep('.',n))
   local value = string.gsub(B[VALUE], templ, '%1 ')
   return (B[SIGN] < 0 and '-' or '') .. string.reverse(value)
end
bigint.about[bigint.str] = {"str(B[,n=3])", "More readable string representation of the number. Optional argument defines number of digits in a group.", help.OTHER}

--- Float number representation.
--  @param v Bigint object.
--  @return Integer if possible, otherwise float point number.
bigint.val = function (B) return tonumber(tostring(B)) end
bigint.about[bigint.val] = {"val(N)", "Represent current big integer as number if it possible.", help.OTHER}

--- B!
--  @param B Bigint object or integer.
--  @return Factorial of the number as bigint object.
bigint.fact = function (B)
   local n = isbigint(B) and B:copy() or bigint:new(B)
   assert(n[1] > 0, "Non-negative value is expected!")
   local res, one = bigint:new(1), bigint:new(1)   
   local mul, sub = bigint.__mul, bigint.__sub
   while n[2] ~= '0' do   
      res = mul(res, n)
      n   = sub(n, one)
   end
   return res
end
bigint.about[bigint.fact] = {"fact(B)", "Return factorial of non-negative integer n."}

-- simplify constructor call
setmetatable(bigint, {__call = function (self, v) return bigint:new(v) end})
bigint.Big = 'Big'
bigint.about[bigint.Big] = {"Big(v)", "Create big number from integer or string.", help.NEW}

-- free memory if need
if not LC_DIALOG then bigint.about = nil end

return bigint

--=================================
--TODO: work with different bases
