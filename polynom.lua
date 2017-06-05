

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

a = polynom.new(1,2,3)
b = polynom.new(4,5)

print(a-b)
