

local polynom = {}
polynom.__index = polynom

polynom.type = 'polynom'

local function ispolynom(x) return type(x) == 'table' and x.type == polynom.type end

local function args(a,b)
   a = ispolynom(a) and a or polynom.new(a)
   b = ispolynom(b) and b or polynom.new(b)
   return a, b
end

function polynom:init(t)
   setmetatable(t, self)
   return t
end

function polynom.new(...)
   local o = {...}
   assert(#o > 0, "Unexpected number of coefficients!")
   for i = 1, #o do
      assert(type(o[i]) == 'number', "Wrong coefficient type")
   end
   return polynom:init(o)
end

polynom.val = function (p,x)
   local res = 0
   for i = 1, #p do
      res = res * (i > 1 and x or 1)
      res = res + p[i]
   end
   return res
end

polynom.copy = function (p)
   local cp = {}
   for i = 1, #p do cp[i] = p[i] end
   return polynom:init(cp)
end

polynom.__add = function (a,b)
   a, b = args(a,b)
   local t = {}
   local i,j = #a, #b
   for k = math.max(i,j),1,-1 do
      t[k] = (a[i] or 0) + (b[j] or 0)
      i = i-1
      j = j-1
   end
   return polynom:init(t)
end

polynom.__unm = function (p)
   local res = {}
   for i = 1, #p do res[i] = -p[i] end
   return polynom:init(res)
end

polynom.__sub = function (a,b)
   return a + (-b)
end

polynom.__mul = function (a,b)
   a,b = args(a,b)
   local res = polynom:init({0})
   for j = 1, #b do
      local tmp = {}
      for i = 1, #a do tmp[i] = a[i]*b[j] end
      local pos = #b-j
      while pos > 0 do
         table.insert(tmp, 0)
	 pos = pos-1
      end
      res = res + polynom:init(tmp)
   end
   return res
end

polynom.__pow = function (p,n)
   n = assert(math.tointeger(n), "Integer power is expected!")
   assert(n > 0, "Positive power is expected!")
   local res, acc = polynom:init({1}), polynom.copy(p)
   while n > 0 do
      if n%2 == 1 then res = res*acc end
      if n ~= 1 then acc = acc * acc end
      n = n // 2
   end
   return res
end

polynom.__eq = function (a,b)
   if type(a) ~= type(b) or a.type ~= b.type then return false end
   if #a ~= #b then return false end
   for i = 1, #a do
      if a[i] ~= b[i] then return false end
   end
   return true
end

polynom.__lt = function (a,b)
   a,b = args(a,b)
   return #a < #b or (#a == #b and a[1] < b[1])
end

polynom.__le = function (a,b)
   return a == b or a < b
end

polynom.der = function (p,x)
   local der, pow = {}, #p
   for i = 1, #p-1 do
      table.insert(der, p[i]*(pow-i))
   end
   der = polynom:init(der)
   if x then x = polynom.val(der,x) end
   return der, x
end

polynom.int = function (p,x)
   x = x or 0
   local int, pow = {}, #p+1
   for i = 1, #p do
      table.insert(int, p[i]/(pow-i))
   end
   table.insert(int, x)
   return polynom:init(int)
end

polynom.__tostring = function (p)
   return string.format('[%s]', table.concat(p, ','))
end


----------------------

a = polynom.new(1,1)

print(a^10)
