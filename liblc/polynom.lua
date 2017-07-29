--[[     polynom.lua
Manipulations with polynomials.

---------- Examples -------------

Poly = require 'liblc.polynom'

a = Poly(1,2,4,3)            --> [1,2,4,3]
b = Poly(1,1)                --> [1,1]
a:val(0)                     --> 3

a + b                        --> [1,2,5,4]
a - b                        --> [1,2,3,2]
a * a                        --> [1,2,1]
b / a
b % a
a ^ 3                        --> [1,3,3,1]

a:int()                     
a:der()                     
Poly.coef(1,-1)              --> [1,0,-1]

c = a:copy()

This file is a part of liblc collection. 
Stanislav Mikhel, 2017.
]]

--TODO: polyroot, polyfit

local polynom = {}
polynom.__index = polynom

polynom.type = 'polynom'

local help = lc_version and (require "liblc.help") or {new=function () return {} end}
polynom.about = help:new("Operations with polynomials")

-- check type
local function ispolynom(x) return type(x) == 'table' and x.type == polynom.type end

-- correct arguments if need
local function args(a,b)
   a = ispolynom(a) and a or polynom.new(a)
   b = ispolynom(b) and b or polynom.new(b)
   return a, b
end

-- initialize polynom from table
function polynom:init(t)
   if #t == 0 then t[1] = 0 end
   setmetatable(t, self)
   return t
end

-- create polynom from list of coefficients
function polynom.new(...)
   local o = {...}
   return polynom:init(o)
end

-- remove zeros from begin
local function reduce (p)
   while p[1] == 0 and #p > 1 do
      table.remove(p, 1)
   end
   return p
end

-- calculate ratio and rest of 2 polynomials
local function div(a,b)
   if #a < #b then return polynom.new(), a end
   local rest, res = polynom.copy(a), {}
   -- get part for working with
   local num = polynom:init(table.move(a,1,#b,1,{}))
   while #num >= #b do
      -- calculate next coefficient
      local t = num[1]/b[1]
      table.insert(res, t)
      -- prepare difference
      num = num - t*b
      reduce(num)
      table.remove(rest, 1)
      if rest[#b] then table.insert(num, rest[#b]) end
   end
   return polynom:init(res), num
end

-- calculate value
polynom.val = function (p,x)
   local res = 0
   for i = 1, #p do
      res = res * (i > 1 and x or 1)
      res = res + p[i]
   end
   return res
end
polynom.about[polynom.val] = {"val(p,x)", "Get value of polinom p in point x.", help.BASE}

-- create copy of object
polynom.copy = function (p)
   local cp = {}
   for i = 1, #p do cp[i] = p[i] end
   return polynom:init(cp)
end
polynom.about[polynom.copy] = {"copy(p)", "Get copy of polynom.", help.OTHER}

-- a + b
polynom.__add = function (a,b)
   a, b = args(a,b)
   local t = {}
   -- get summ of equal powers
   local i,j = #a, #b
   for k = math.max(i,j),1,-1 do
      t[k] = (a[i] or 0) + (b[j] or 0)
      i = i-1
      j = j-1
   end
   return polynom:init(t)
end

-- -a
polynom.__unm = function (p)
   local res = {}
   for i = 1, #p do res[i] = -p[i] end
   return polynom:init(res)
end

-- a - b
polynom.__sub = function (a,b)
   return reduce(a + (-b))
end

-- a * b
polynom.__mul = function (a,b)
   a,b = args(a,b)
   local res = polynom:init({0})
   for j = 1, #b do
      -- sum of crossproduct
      local tmp = {}
      for i = 1, #a do tmp[i] = a[i]*b[j] end
      local pos = #b-j
      -- add zeros to increase power
      while pos > 0 do
         table.insert(tmp, 0)
	 pos = pos-1
      end
      res = res + polynom:init(tmp)
   end
   return reduce(res)
end

-- a / b
polynom.__div = function (a,b)
   local res, _ = div(args(a,b))
   return res
end

-- a % b
polynom.__mod = function (a,b)
   local _, res = div(args(a,b))
   return res
end

-- a ^ n
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

-- a == b
polynom.__eq = function (a,b)
   if type(a) ~= type(b) or a.type ~= b.type then return false end
   if #a ~= #b then return false end
   for i = 1, #a do
      if a[i] ~= b[i] then return false end
   end
   return true
end

-- a < b
polynom.__lt = function (a,b)
   a,b = args(a,b)
   return #a < #b or (#a == #b and a[1] < b[1])
end

-- a <= b
polynom.__le = function (a,b)
   return a == b or a < b
end

polynom.arithmetic = 'arithmetic'
polynom.about[polynom.arithmetic] = {polynom.arithmetic, "a+b, a-b, a*b, a/b, a^n, -a", help.BASE}

polynom.comparation = 'comparation'
polynom.about[polynom.comparation] = {polynom.comparation, "a<b, a<=b, a>b, a>=b, a==b, a~=b", help.BASE}

-- get derivative
polynom.der = function (p,x)
   assert(ispolynom(p), "Polinom is expected!")
   local der, pow = {}, #p
   for i = 1, #p-1 do
      table.insert(der, p[i]*(pow-i))
   end
   der = polynom:init(der)
   if x then x = polynom.val(der,x) end
   return der, x
end
polynom.about[polynom.der] = {"der(p[,x])", "Calculate derivative of polynom, and its value, if need.", help.BASE}

-- get integral
polynom.int = function (p,x)
   assert(ispolynom(p), "Polynom is expected!")
   x = x or 0
   local int, pow = {}, #p+1
   for i = 1, #p do
      table.insert(int, p[i]/(pow-i))
   end
   table.insert(int, x)
   return polynom:init(int)
end
polynom.about[polynom.int] = {"int(p[,x0])", "Calculate integral, x0 - free coefficient.", help.BASE}

-- get polynom from roots
polynom.coef = function (...)
   local args = {...}
   local res = polynom:init({1})
   for i = 1, #args do
      res = res * polynom:init({1,-args[i]})
   end
   return res
end
polynom.about[polynom.coef] = {"coef(...)", "Return polynom with given roots.", help.OTHER}

-- string representation
polynom.__tostring = function (p)
   return table.concat(p,' ')
end

setmetatable(polynom, {__call = function (self, ...) return polynom.new(...) end})
polynom.Poly = 'Poly'
polynom.about[polynom.Poly] = {"Poly(...)", "Create a polynom", help.NEW}

-- polynom serialization
polynom.serialize = function (obj)
   local s = {}
   for i = 1, #obj do s[#s+1] = string.format("%a", obj[i]) end
   s[#s+1] = "metatablename='Poly'"
   s[#s+1] = "modulename='polynom'"
   return string.format("{%s}", table.concat(s, ','))
end
polynom.about[polynom.serialize] = {"serialize(obj)", "Save polynom internal representation.", help.OTHER}

return polynom
