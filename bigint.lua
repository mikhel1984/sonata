
local bigint = {}
bigint.__index = bigint

bigint.BASE = 10
bigint.type = 'bigint'

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

local function simplify (v)
   local i = #v
   while i > 1 and v[i] == 0 do
      v[i] = nil
      i = i - 1
   end
end

local function args (a, b)
   --[[
   a = (type(a) == 'number' or type(a) == 'string') and bigint:new(a) or a
   if b then
      b = (type(b) == 'number' or type(b) == 'string') and bigint:new(b) or b
   end
   ]]
   a = (type(a) == 'table' and a.type == bigint.type) and a or bigint:new(a)
   if b then
      b = (type(b) == 'table' and b.type == bigint.type) and b or bigint:new(b)
   end
   return a, b
end


local function div(a,b)
   a,b = args(a,b)
   local num = string.reverse(a.value)
   local acc = {}
   local k = #b.value
   local rest = bigint:new(string.sub(num, 1, k))
   local denom = bigint.abs(b)
   while k <= #num do
      if rest >= denom then
         local n = bigint.BASE
	 local prod
	 repeat
	    n = n-1
	    prod = denom*n
	 until prod <= rest
	 acc[#acc+1] = n
	 rest = rest-prod
      else
         if #acc > 0 then acc[#acc+1] = 0 end
      end
      k = k+1
      rest.value = string.sub(num, k,k) .. rest.value
   end
   local result = bigint:new(a.sign*b.sign)
   result.value = #acc > 0 and string.reverse(table.concat(acc)) or '0'
   return result, rest   
end

bigint.abs = function (v)
   local a = (type(v) == 'number' or type(v) == 'string') and bigint:new(v) or bigint.copy(v)
   if a.sign < 0 then a.sign = -a.sign end
   return a
end

bigint.copy = function (v)
   local c = bigint:new(v.sign)
   c.value = v.value
   return c
end

bigint.__add = function (a,b)
   a,b = args(a,b)
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
   simplify(sum)
   -- save
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
   b = args(b)
   return bigint.__add(a, -b)
end

bigint.__mul = function (a, b) 
   a,b = args(a,b)
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
   simplify(sum)
   -- save
   local res = bigint:new(a.sign*b.sign)
   res.value = table.concat(sum)
   return res   
end

bigint.__div = function (a, b)
   local res, _ = div(a,b)
   return res
end

bigint.__mod = function (a, b)
   local _, res = div(a,b)
   return res
end

bigint.__eq = function (a,b)
   a,b = args(a,b)
   return a.sign == b.sign and a.value == b.value
end

bigint.__lt = function (a,b)
   a,b = args(a,b)
   if a.sign < b.sign then return true end
   if #a.value == #b.value then
      local va, vb = string.reverse(a.value), string.reverse(b.value)
      return (a.sign > 0 and va < vb) or (a.sign < 0 and va > vb)
   else
      return (a.sign > 0 and #a.value < #b.value) or (a.sign < 0 and #a.value > #b.value) 
   end
end

bigint.__le = function (a,b)
   a,b = args(a,b)
   return bigint.__eq(a,b) or bigint.__lt(a,b)
end

bigint.__len = function (v)
   return #v.value
end

bigint.__pow = function (a,b)
   a,b = args(a,b)
   assert(b.sign >= 0, "Power must be non negative")
   if b.value == 0 then 
      assert(a ~= 0, "Error: 0^0")
      return bigint:new(1) 
   end
   local aa, bb = bigint.copy(a), bigint.copy(b)
   local res = bigint:new(1)
   local h = 1
   while bb > 0 do
      local p,q = div(bb,2)
      if q.value ~= '0' then
         res = res * aa
      end
      aa = aa * aa
      bb = p
   end
   return res
end

bigint.__tostring = function (v)
   return (v.sign < 0 and '-' or '') .. string.reverse(v.value)
end

bigint.tonumber = function (v)
   return tonumber(bigint.__tostring(v))
end

a = bigint:new(25)
b = bigint:new(2)

print(a ^ 36 )




