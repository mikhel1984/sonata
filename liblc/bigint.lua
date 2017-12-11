--[[      liblc/bigint.lua 

--- Operations with arbitrary long integer numbers.
--  @author Stanislav Mikhel, 2017
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection.

            module 'bigint'
--]]

---------------- Tests -----------------
--[[!!
Big = require 'liblc.bigint'

a = Big(123)         
ans = a:tonumber()           --> 123

b = Big('456')        
ans = b:tonumber()           --> 456

ans = Big.tonumber(a+b)      --> 579

ans = Big.tonumber(a-b)      --> -333

ans = Big.tonumber(a*Big(2)) --> 246

ans = Big.tonumber(b/2)      --> 228

ans = Big.tonumber(b%a)      --> 87

ans = Big.tonumber(a^3)      --> 1860867

ans = Big.tonumber(Big.abs('-25')) --> 25

c = Big(10)
ans = Big.tonumber(c:factorial())  --> 3628800

d = a:copy()
ans = (a == d)               --> true

ans = (a > b)                --> false

ans = (a == b)               --> false

ans = a:eq(123)              --> true

ans = #a                     --> 3

print(a)
]]

-----------------------------------------
-- @class table
-- @name bigint
-- @field type Define object type string.
-- @field about Function description collection.
-- @field BASE Radix value. Default is 10.


local bigint = {}
bigint.__index = bigint
-- mark of type
bigint.type = 'bigint'  
bigint.isbigint = true
-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
bigint.about = help:new("Operations with arbitraty long integers.")
-- the radix
bigint.BASE = 10        
bigint.about[bigint.BASE] = {'BASE', "The radix of big integer representation.", help.OTHER}

--- Absolute value of the integer.
--    <i>Private function.</i>
--    @param v Integer number.
--    @return Absolute value.
local function iabs(v) return (v < 0) and (-v) or v end

--- Check object type.
--    <i>Private function.</i>
--    @param t Object for checking.
--    @return True if table is a bigint.
local function isbigint(v) return type(v) == 'table' and v.isbigint end

--- Convert integer into bigint.
--    <i>Private function.</i>
--    @param v Integer value.
--    @return String representation of the number and sign (1 or -1).
local function fromint(v) 
   v = assert(math.tointeger(v), 'Integer is expected')
   return tostring(iabs(v)), (v < 0) and -1 or 1               -- return value and sing
end

--- Convert string into bigint.
--    <i>Private function.</i>
--    @param s Integer as string.
--    @return String representation of the absolute value and the sign (1 or -1).
local function fromstr(s)
   local m,d = string.match(s, '^([+-]?)(%d+)$')               -- get sign and value
   assert(m and d, 'Wrong number!')
   return d, (m == '-' and -1 or 1)
end

--- Create new object, set metatable.
--    @param a Integer as number or string.
--    @return Bigint object.
function bigint:new(a)
   local s, sign
   if     type(a) == 'string' then
      s, sign = fromstr(a)
   elseif type(a) == 'number' then 
      s, sign = fromint(a)
   else
      error("Expected integer or string")
   end
   local o = {value = string.reverse(s), sign = sign}
   setmetatable(o, self)
   return o
end

--- Reduce front zeros.
--    Convert '00123' to '123'.
--    <i>Private function.</i>
--    @param v Table with digits.
local function simplify (v)
   local i = #v
   while i > 1 and v[i] == 0 do
      v[i] = nil
      i = i - 1
   end
end

--- Correct function arguments if need.
--    <i>Private function.</i>
--    @param a Bigint or integer.
--    @param b Bigint or integer or <code>nil</code>.
--    @return Both arguments as bigint objects (or <code>nil</code>).
local function args (a, b)   
   a = isbigint(a) and a or bigint:new(a)
   if b then
      b = isbigint(b) and b or bigint:new(b)
   end
   return a, b
end

--- Main algorithm for division.
--    <i>Private function.</i>
--    @param a First bigint object.
--    @param b Second bigint object.
--    @return Ratio and rest as bigint objects.
local function div(a,b)
   a,b = args(a,b)
   local num = string.reverse(a.value)             -- numerator as string
   local acc = {}                                  -- result
   local k = #b.value                              -- index of the last character
   local rest = bigint:new(string.sub(num, 1, k))  -- current part the of numerator
   local denom = bigint.abs(b)                     -- denominator
   local q = string.sub(denom.value, #denom.value) -- first digit
   -- read the string
   while k <= #num do                             
      if rest >= denom then
         -- get ratio
	 local n, p = 1, string.sub(rest.value, #denom.value):reverse()
	 n = math.tointeger(p // q)
	 local prod = n*denom
	 -- save result
	 if prod <= rest then
	    acc[#acc+1] = n
	    rest = rest-prod
	 else
	    acc[#acc+1] = n-1
	    rest = rest-prod+denom
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

--- Get sum of the two positive bigint numbers.
--    <i>Private function.</i>
--    @param a First bigint object.
--    @param b Second bigint object.
--    @return Summ.
local function sum(a,b)
   local acc = {}
   local zero = string.byte('0')
   -- calculate sum
   for i = 1, math.max(#a.value,#b.value) do
      local ai = string.byte(a.value, i) or zero
      local bi = string.byte(b.value, i) or zero      
      acc[i] = (acc[i] or 0) + (ai-zero) + (bi-zero)
      if acc[i] >= bigint.BASE then
         acc[i] = acc[i] - bigint.BASE
         acc[i+1] = 1
      end      
   end
   simplify(acc)   -- remove zeros
   local res = bigint:new(0)
   res.value = table.concat(acc)
   return res
end

--- Get subsruction for two positive bigint numbers.
--    <i>Private function.</i>
--    @param a First bigint object.
--    @param b Second bigint object.
--    @return Substraction.
local function sub(a,b)
   -- find the biggest
   local p,q,r = a,b,1
   if bigint.abs(a) < bigint.abs(b) then
      p,q,r = q,p,-1
   end
   local acc = {}
   local zero = string.byte('0')
   -- calculate sub
   for i = 1, #p.value do
      local pi = string.byte(p.value, i) or zero
      local qi = string.byte(q.value, i) or zero
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

--- Module of number.
--    @param v Bigint or integer number.
--    @return Absolute value.
bigint.abs = function (v)
   local a = (type(v) == 'number' or type(v) == 'string') and bigint:new(v) or bigint.copy(v)
   if a.sign < 0 then a.sign = -a.sign end
   return a
end
bigint.about[bigint.abs] = {"abs(v)", "Return module of arbitrary long number.", help.BASE}

--- Copy of the object.
--    @param v Original bigint object.
--    @return Deep copy.
bigint.copy = function (v)
   local c = bigint:new(v.sign)
   c.value = v.value
   return c
end
bigint.about[bigint.copy] = {"copy(v)", "Return copy of given number.", help.OTHER}

--- a + b
--    @param a First bigint object or integer.
--    @param b Second bigint object or integer.
--    @return Summ as bigint object.
bigint.__add = function (a,b)
   a,b = args(a,b) 
   if a.sign > 0 then
      return (b.sign > 0) and sum(a,b) or sub(a,b)
   else 
      return (b.sign > 0) and sub(b,a) or -sum(a,b)
   end
end

--- -v
--    @param v Bigint object.
--    @return Negative value of the object.
bigint.__unm = function (v)
   local res = bigint:new(-v.sign)
   res.value = v.value
   return res
end
--- a - b
--    @param a First bigint object or integer.
--    @param b Second bigint object or integer.
--    @return Difference as bigint object.
bigint.__sub = function (a, b)
   a,b = args(a,b)
   if a.sign > 0 then
      return (b.sign > 0) and sub(a,b) or sum(a,b)
   else
      return (b.sign > 0) and -sum(a,b) or sub(b,a)
   end
end

--- a * b
--    @param a First bigint object or integer.
--    @param b Second bigint object or integer.
--    @return Product as bigint object.
bigint.__mul = function (a, b) 
   a,b = args(a,b)
   local zero = string.byte('0')
   local sum = {}
   -- get products   
   for i = 1, #a.value do
      local ai = string.byte(a.value, i) - zero
      for j = 1, #b.value do
         local bj = string.byte(b.value, j) - zero
	 local pos = i+j-1
	 sum[pos] = (sum[pos] or 0) + ai*bj
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

--- a / b
--    @param a First bigint object or integer.
--    @param b Second bigint object or integer.
--    @return Integer part of the ratio as bigint object.
bigint.__div = function (a, b)
   local res,_ = div(a,b)
   return res
end

--- a % b
--    @param a First bigint object or integer.
--    @param b Second bigint object or integer.
--    @return Rest of the ratio as bigint object.
bigint.__mod = function (a, b)
   local _,res = div(a,b)
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
   a,b = args(a,b)
   return a.sign == b.sign and a.value == b.value
end
bigint.about[bigint.eq] = {"eq(a,b)", "Check equality of two values.", help.OTHER}
-- redefine equality
bigint.__eq = bigint.eq

--- a < b
--    @param a First bigint object or integer.
--    @param b Second bigint object or integer.
--    @return Relation between two numbers.
bigint.__lt = function (a,b)
   a,b = args(a,b)
   if a.sign < b.sign then return true end
   if #a.value == #b.value then         -- equial length
      local va, vb = string.reverse(a.value), string.reverse(b.value)
      return (a.sign > 0 and va < vb) or (a.sign < 0 and va > vb)
   else                                 -- different length
      return (a.sign > 0 and #a.value < #b.value) or (a.sign < 0 and #a.value > #b.value) 
   end
end

--- a <= b
--    @param a First bigint object or integer.
--    @param b Second bigint object or integer.
--    @return Relation between two numbers.
bigint.__le = function (a,b)
   a,b = args(a,b)
   return bigint.__eq(a,b) or bigint.__lt(a,b)
end

--- #a 
--    @param v Bigint object.
--    @return Number of digits.
bigint.__len = function (v)
   return #v.value
end

--- a ^ b
--    @param a Bigint object or integer.
--    @param b Positive bigint object or integer.
--    @return Result of power operation.
bigint.__pow = function (a,b)
   a,b = args(a,b)
   assert(b.sign >= 0, "Power must be non negative")
   if b.value == '0' then 
      assert(a.value ~= '0', "Error: 0^0")
      return bigint:new(1) 
   end
   local aa, bb, q = bigint.copy(a), bigint.copy(b), nil
   local res = bigint:new(1)
   local h = 1
   while bb > 0 do
      bb,q = div(bb,2)
      if q.value ~= '0' then res = res*aa end
      if bb.value ~= '0' then aa = aa*aa end
   end
   return res
end

bigint.arithmetic = 'arithmetic'
bigint.about[bigint.arithmetic] = {bigint.arithmetic, "a+b, a-b, a*b, a/b, a%b, a^b, -a, #a", help.BASE}
bigint.comparation = 'comparation'
bigint.about[bigint.comparation] = {bigint.comparation, "a<b, a<=b, a>b, a>=b, a==b, a~=b", help.BASE}

--- String representation.
--    @param v Bigint object.
--    @return Number as string.
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
   return tonumber(bigint.__tostring(v))
end
bigint.about[bigint.tonumber] = {"tonumber(v)", "Represent current big integer as number if it possible.", help.BASE}

--- m!
--    @param m Bigint object or integer.
--    @return Factorial of the number as bigint object.
bigint.factorial = function (m)
   assert(m >= 0, "Nonnegative value is expected!")
   local n = bigint.abs(m)
   local res = bigint:new(1)   
   local one = res:copy()
   while n.value ~= '0' do   
      res = res * n
      n = n - one
   end
   return res
end
bigint.about[bigint.factorial] = {"factorial(n)", "Return factorial of nonnegative integer n.", help.BASE}

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
