---------- bigint.lua -------------
--
-- Operations with arbitrary long integer numbers.
--
-- This file is a part of liblc collection. 
-- Stanislav Mikhel, 2017.
----------------------------------------

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

local bigint = {}
bigint.__index = bigint

bigint.type = 'bigint'  -- mark of type

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
bigint.about = help:new("Operations with arbitraty long integers")

bigint.BASE = 10        -- the radix
bigint.about[bigint.BASE] = {'BASE', "The radix of big integer representation", help.OTHER}

-- absolute value of integer
local function iabs(v) return (v < 0) and (-v) or v end

local function isbigint(v) return type(v) == 'table' and v.type == bigint.type end

-- convert integer into bigint
local function fromint(v) 
   v = math.tointeger(v)
   assert(v, 'Integer is expected')
   return tostring(iabs(v)), (v < 0) and -1 or 1   -- return value and sing
end

-- convert string into bigint
local function fromstr(s)
   assert(string.find(s, '^[+-]?%d+$'), "Wrong string number")                       -- check format
   return string.match(s, '%d+'), (string.byte(s) == string.byte('-')) and -1 or 1   -- return value and sing
end

-- constructor
function bigint:new(a)
   local s, sign
   if     type(a) == 'number' then 
      s, sign = fromint(a)
   elseif type(a) == 'string' then
      s, sign = fromstr(a)
   else
      error("Expected integer or string")
   end
   local o = {value = string.reverse(s), sign = sign}
   setmetatable(o, self)
   return o
end

-- reduce front zeros
local function simplify (v)
   local i = #v
   while i > 1 and v[i] == 0 do
      v[i] = nil
      i = i - 1
   end
end

-- correct function arguments if need
local function args (a, b)   
   a = isbigint(a) and a or bigint:new(a)
   if b then
      b = isbigint(b) and b or bigint:new(b)
   end
   return a, b
end

-- main function for division
local function div(a,b)
   a,b = args(a,b)
   local num = string.reverse(a.value)            -- numerator as string
   local acc = {}                                 -- result
   local k = #b.value                             -- index of the last character
   local rest = bigint:new(string.sub(num, 1, k)) -- current part of numerator
   local denom = bigint.abs(b)                    -- denominator
   local q = string.sub(denom.value, #denom.value)
   -- read the string
   while k <= #num do                             
      if rest >= denom then
         -- get ratio
	 local n, p = 1, string.sub(rest.value, #denom.value):reverse()
	 n = p // q
	 local prod = n*denom
	 -- save result
	 if prod <= rest then
	    acc[#acc+1] = math.tointeger(n)
	    rest = rest-prod
	 else
	    acc[#acc+1] = math.tointeger(n-1)
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

-- get sum for two bigint
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

-- get subsruction for two bigint
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

-- module of number
bigint.abs = function (v)
   local a = (type(v) == 'number' or type(v) == 'string') and bigint:new(v) or bigint.copy(v)
   if a.sign < 0 then a.sign = -a.sign end
   return a
end
bigint.about[bigint.abs] = {"abs(v)", "Return module of arbitrary long number.", help.BASE}

-- get copy
bigint.copy = function (v)
   local c = bigint:new(v.sign)
   c.value = v.value
   return c
end
bigint.about[bigint.copy] = {"copy(v)", "Return copy of given number.", help.OTHER}

-- a + b
bigint.__add = function (a,b)
   a,b = args(a,b) 
   if     a.sign > 0 and b.sign > 0 then return sum(a,b)
   elseif a.sign < 0 and b.sign < 0 then return -sum(a,b)
   elseif a.sign > 0 and b.sign < 0 then return sub(a,b)
   else                                  return sub(b,a)
   end   
end

-- -v
bigint.__unm = function (v)
   local res = bigint:new(-v.sign)
   res.value = v.value
   return res
end

-- a - b
bigint.__sub = function (a, b)
   a,b = args(a,b)
   if     a.sign > 0 and b.sign > 0 then return sub(a,b)
   elseif a.sign > 0 and b.sign < 0 then return sum(a,b)
   elseif a.sign < 0 and b.sign > 0 then return -sum(a,b)
   else                                  return sub(b,a)
   end
end

-- a * b
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
   if d ~= 0 then sum[#sum+1] = d end
   simplify(sum)
   -- save
   local res = bigint:new(a.sign*b.sign)
   res.value = table.concat(sum)
   return res   
end

-- a / b
bigint.__div = function (a, b)
   local res, _ = div(a,b)
   return res
end

-- a % b
bigint.__mod = function (a, b)
   local _, res = div(a,b)
   return res
end

-- a == b
bigint.eq = function (a,b)
   a,b = args(a,b)
   return a.sign == b.sign and a.value == b.value
end
bigint.about[bigint.eq] = {"eq(a,b)", "Check equality of two values", help.OTHER}

bigint.__eq = bigint.eq

-- a < b
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

-- a <= b
bigint.__le = function (a,b)
   a,b = args(a,b)
   return bigint.__eq(a,b) or bigint.__lt(a,b)
end

-- #v
bigint.__len = function (v)
   return #v.value
end

-- a^b
bigint.__pow = function (a,b)
   a,b = args(a,b)
   assert(b.sign >= 0, "Power must be non negative")
   if b.value == '0' then 
      assert(a ~= 0, "Error: 0^0")
      return bigint:new(1) 
   end
   local aa, bb, q = bigint.copy(a), bigint.copy(b), nil
   local res = bigint:new(1)
   local h = 1
   while bb > 0 do
      bb,q = div(bb,2)
      if q.value ~= '0' then
         res = res * aa
      end
      aa = aa * aa      
   end
   return res
end

bigint.arithmetic = 'arithmetic'
bigint.about[bigint.arithmetic] = {bigint.arithmetic, "a+b, a-b, a*b, a/b, a%b, a^b, -a, #a", help.BASE}
bigint.comparation = 'comparation'
bigint.about[bigint.comparation] = {bigint.comparation, "a<b, a<=b, a>b, a>=b, a==b, a~=b", help.BASE}

-- string representation
bigint.__tostring = function (v)
   return (v.sign < 0 and '-' or '') .. string.reverse(v.value)
end

-- float number representation
bigint.tonumber = function (v)
   return tonumber(bigint.__tostring(v))
end
bigint.about[bigint.tonumber] = {"tonumber(v)", "Represent current big integer as number if it possible.", help.BASE}

-- m!
bigint.factorial = function (m)
   assert(m >= 0, "Nonnegative value is expected!")
   local n = bigint.abs(m)
   local res = bigint:new(1)   
   while n.value ~= '0' do   
      res = res * n
      n = n - 1      
   end
   return res
end
bigint.about[bigint.factorial] = {"factorial(n)", "Return factorial of nonnegative integer n.", help.BASE}

-- simplify constructor call
setmetatable(bigint, {__call = function (self, v) return bigint:new(v) end})
bigint.Big = 'Big'
bigint.about[bigint.Big] = {"Big(v)", "Create big number from integer or string", help.NEW}

-- object serialization
bigint.serialize = function (obj)
   local s = {}
   s[#s+1] = string.format("value='%s'", obj.value)
   s[#s+1] = "sign=" .. obj.sign
   s[#s+1] = "metatablename='Big'"
   s[#s+1] = "modulename='bigint'"
   return string.format("{%s}", table.concat(s, ','))
end
bigint.about[bigint.serialize] = {"serialize(obj)", "Save internal representation of bigint object.", help.OTHER}

return bigint
