
local bigint = {}
bigint.__index = bigint

bigint.BASE = 10

local function iabs(v) return (v < 0) and (-v) or v end

local function fromint(v) 
   v = math.tointeger(v)
   assert(v, 'Integer is expected')
   return tostring(iabs(v)), (v < 0) and -1 or 1   
end

local function fromstr(s)
   assert(string.find(s, '^[+-]?%d+$'), "Wrong string number")
   return string.match(s, '%d+'), (string.byte(s) == string.byte('-')) and -1 or 1
end

function bigint:new(a)
   local s, sign
   if type(a) == 'number' then 
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

bigint.__add = function (a,b)
   a = (type(a) == 'number' or type(a) == 'string') and bigint:new(a) or a
   b = (type(b) == 'number' or type(b) == 'string') and bigint:new(b) or b
   local zero = string.byte('0')
   local sum = {}
   local ci, d = 0, 0
   for i = 1, math.max(#a.value, #b.value) do
      local ai = string.byte(a.value, i) or zero
      local bi = string.byte(b.value, i) or zero
      ci = a.sign * (ai - zero) + b.sign * (bi - zero) + d
      if ci >= bigint.BASE then 
         sum[i] = ci - bigint.BASE
         d = 1
      elseif ci <= -bigint.BASE then
         sum[i] = -ci - bigint.BASE
	 d = -1
      else
         sum[i] = iabs(ci)
	 d = 0
      end
   end
   if d ~= 0 then sum[#sum+1] = 1 end
   local res = bigint:new(0)
   res.sign = (ci < 0) and -1 or 1
   res.value = table.concat(sum)
   return res
end

bigint.__unm = function (v)
   local res = bigint:new(-v.sign)
   res.value = v.value
   return res
end

bigint.__sub = function (a, b)
   b = (type(b) == 'number' or type(b) == 'string') and bigint:new(b) or b
   return bigint.__add(a, -b)
end

bigint.__mul = function (a, b) 
   a = (type(a) == 'number' or type(a) == 'string') and bigint:new(a) or a
   b = (type(b) == 'number' or type(b) == 'string') and bigint:new(b) or b
   local zero = string.byte('0')
   local sum = {}   
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
      d = math.floor(sum[i]/bigint.BASE)
      sum[i] = math.floor(sum[i] % bigint.BASE)
   end
   if d ~= 0 then sum[#sum+1] = d end
   -- save
   local res = bigint:new(a.sign*b.sign)
   res.value = table.concat(sum)
   return res   
end

bigint.__eq = function (a,b)
   a = (type(a) == 'number' or type(a) == 'string') and bigint:new(a) or a
   b = (type(b) == 'number' or type(b) == 'string') and bigint:new(b) or b
   return a.sign == b.sign and a.value == b.value
end

bigint.__lt = function (a,b)
   a = (type(a) == 'number' or type(a) == 'string') and bigint:new(a) or a
   b = (type(b) == 'number' or type(b) == 'string') and bigint:new(b) or b
   return a.sign < b.sign or 
         -- use string comparision
         (a.sign > 0 and a.value < b.value) or
	 (a.sign < 0 and a.value > b.value)
end

bigint.__le = function (a,b)
   a = (type(a) == 'number' or type(a) == 'string') and bigint:new(a) or a
   b = (type(b) == 'number' or type(b) == 'string') and bigint:new(b) or b
   return bigint.__eq(a,b) or bigint.__lt(a,b)
end

bigint.__len = function (v)
   return #v.value
end

bigint.__tostring = function (v)
   return (v.sign < 0 and '-' or '') .. string.reverse(v.value)
end
--[[
t = bigint:new('1233456732369988007')
print(t)
a = bigint:new(123)
b = bigint:new(-456)

c = a - b
print(c)
]]
p = bigint:new(25)
q = bigint:new(35)
r = p<=q
print(r)
