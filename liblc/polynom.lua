--[[      liblc/polynom.lua 

--- Manipulations with polynomials.
--  @author Stanislav Mikhel, 2017
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection.

            module 'polynom'
--]]

-------------------- Tests -------------------
--[[!!
Poly = require 'liblc.polynom'

a = Poly(1,2,4,3)            
b = Poly(1,1)                
ans = a:val(0)                --> 3

ans = a(0)                    --> 3

ans = a + b                   --> Poly(1,2,5,4)

ans = a - b                   --> Poly(1,2,3,2)

ans = b * b                   --> Poly(1,2,1)

ans = a / b                   --> Poly(1,1,3)

ans = a % b                   --> Poly(0)

ans = b ^ 3                   --> Poly(1,3,3,1)

ans = b:int()                 --> Poly(0.5,1,0)

_,ans = a:der(1)              --> 11

ans = Poly.coef(1,-1)         --> Poly(1,0,-1)

c = a:copy()
ans = (a == c)                --> true

ans = (b == c)                --> false

d = Poly(2,-2,1)
ans = d:equation('s')         --> '2*s^2-2*s+1'

e = a:real()
ans = e[1]                    --~ -1.00

print(a)
]]


----------------------------------------------
-- @class table
-- @name polynom
-- @field type Define object type string.
-- @field about Function description collection.
local polynom = {}
polynom.__index = polynom
-- marker
polynom.type = 'polynom'
polynom.ispolynom = true
-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
polynom.about = help:new("Operations with polynomials")

--- Check object type.
--    <i>Private function.</i>
--    @param x Object for checking.
--    @return True if table is a polynom.
local function ispolynom(x) return type(x) == 'table' and x.ispolynom end

--- Correct arguments if need.
--    <i>Private function.</i>
--    @param a First object
--    @param b Second object.
--    @return Two polinomial objects.
local function args(a,b)
   a = ispolynom(a) and a or polynom.new(a)
   b = ispolynom(b) and b or polynom.new(b)
   return a, b
end

--- Initialize polynom from table.
--    @param t Table of coefficients.
--    @return Polynom object.
function polynom:init(t)
   if #t == 0 then t[1] = 0 end
   setmetatable(t, self)
   return t
end

--- Create polynom from list of coefficients.
--    Arguments are a list of coefficients.
--    @return Polynom object.
function polynom.new(...)
   local o = {...}
   return polynom:init(o)
end

--- Simplify polynom, remove zeros from the begining.
--    <i>Private function.</i>
--    @param p Table of coefficients.
--    @return Simplified polynom.
local function reduce (p)
   while p[1] == 0 and #p > 1 do
      table.remove(p, 1)
   end
   return p
end

--- Calculate ratio and rest of 2 polynomials.
--    <i>Private function.</i>
--    @param a First polynom.
--    @param b Second polynom.
--    @return Ratio and the rest.
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

--- Polinom value.
--    Can be called with ().
--    @param p Polynom.
--    @param x Variable.
--    @return Value in the given point.
polynom.val = function (p,x)
   local res = 0
   for i = 1, #p do
      res = res * (i > 1 and x or 1)
      res = res + p[i]
   end
   return res
end
polynom.about[polynom.val] = {"val(p,x)", "Get value of polinom p in point x.", help.BASE}

polynom.__call = function (p,x) return polynom.val(p,x) end

--- Create copy of object.
--    @param p Initial polynom.
--    @return Deep copy.
polynom.copy = function (p)
   return polynom:init(table.move(p,1,#p,1,{}))
end
polynom.about[polynom.copy] = {"copy(p)", "Get copy of polynom.", help.OTHER}

--- a + b
--    @param a First polynom.
--    @param b Second polynom.
--    @return Summ of the objects.
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

--- -a
--    @param p Source polynom.
--    @return Negative value.
polynom.__unm = function (p)
   local res = {}
   for i = 1, #p do res[i] = -p[i] end
   return polynom:init(res)
end

--- a - b
--    @param a First polynom.
--    @param b Second polynom.
--    @return Substraction of the objects.
polynom.__sub = function (a,b)
   return reduce(a + (-b))
end

--- a * b
--    @param a First polynom.
--    @param b Second polynom.
--    @return Multiplication of the objects.
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

--- a / b
--    @param a First polynom.
--    @param b Second polynom.
--    @return Ratio of the objects.
polynom.__div = function (a,b)
   local res, _ = div(args(a,b))
   return res
end

--- a % b
--    @param a First polynom.
--    @param b Second polynom.
--    @return Rest from ratio of the objects.
polynom.__mod = function (a,b)
   local _, res = div(args(a,b))
   return res
end

--- a ^ n
--    @param a Polynom.
--    @param n Integer value.
--    @return Power of the polynom.
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

--- a == b
--    @param a Polynom.
--    @param n Integer value.
--    @return <code>true</code> if the polynoms are equial.
polynom.__eq = function (a,b)
   if type(a) ~= type(b) or a.type ~= b.type then return false end
   if #a ~= #b then return false end
   for i = 1, #a do
      if a[i] ~= b[i] then return false end
   end
   return true
end

--- a < b
--    @param a Frist polynom.
--    @param b Second polynom.
--    @return <code>true</code> if first polynom is less then second.
polynom.__lt = function (a,b)
   a,b = args(a,b)
   return #a < #b or (#a == #b and a[1] < b[1])
end

--- a <= b
--    @param a Frist polynom.
--    @param b Second polynom.
--    @return <code>true</code> if first polynom is less or equial then second.
polynom.__le = function (a,b)
   return a == b or a < b
end

polynom.arithmetic = 'arithmetic'
polynom.about[polynom.arithmetic] = {polynom.arithmetic, "a+b, a-b, a*b, a/b, a^n, -a", help.BASE}

polynom.comparation = 'comparation'
polynom.about[polynom.comparation] = {polynom.comparation, "a<b, a<=b, a>b, a>=b, a==b, a~=b", help.BASE}

--- Get derivative.
--    @param p Initial polynom.
--    @param x Variable (can be omitted).
--    @return Derivative or its value.
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

--- Get integral.
--    @param p Initial polynom.
--    @param x Free coefficient.
--    @return Integral.
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

--- Get polynom from roots.
--    Arguments are a sequance of roots.
--    @return Polynom object.
polynom.coef = function (...)
   local args = {...}
   local res = polynom:init({1})
   for i = 1, #args do
      res = res * polynom:init({1,-args[i]})
   end
   return res
end
polynom.about[polynom.coef] = {"coef(...)", "Return polynom with given roots.", help.OTHER}

--- String representation.
--    @param p Source polynom.
--    @return Simplified string representation of coefficients.
polynom.__tostring = function (p)
   return table.concat(p,' ')
end

--- Represent polynom in natural form.
--    @param p Source polynom.
--    @param l String variable (default is <code>x</code>).
--    @return String with traditional form of equation.
polynom.equation = function (p,l)
   l = l or 'x'
   local res,pow,mult = {}, #p-1
   res[1] = string.format('%s%s%s', tostring(p[1]), (pow > 0 and '*'..l or ''), (pow > 1 and '^'..pow or ''))
   for i = 2,#p do
      pow = #p-i
      res[i] = string.format('%s%s%s', (p[i] > 0 and '+'..p[i] or tostring(p[i])), (pow > 0 and '*'..l or ''), (pow > 1 and '^'..pow or ''))
   end
   return table.concat(res)
end

--- Find closest root of using Newton-Rapson technique
--    <i>Private function.</i>
--    @param p Source polynom.
--    @param x Initial value of the root (optional).
--    @param epx Tolerance
--    @return Found value and flag about its correctness.
local function NewtonRapson(p,x,eps)
   eps = eps or 1e-6
   x = x or math.random()
   -- prepare variables
   local dp,n,max = p:der(), 0, 30
   while true do
      -- polynom value
      local dx = p(x) / dp(x) 
      if math.abs(dx) <= eps or n > max then break
      else
         -- update root value and number of iterations
         x = x - dx
	 n = n+1
      end
   end
   return x, (n < max)
end

--- Find real roots of the polynom.
--    @param p Source polynom.
--    @return Table with real roots.
polynom.real = function (p)
   local res = {}
   local pp = p:copy()
   -- zeros
   while pp[#pp] == 0 do
      pp[#pp] = nil
      res[#res+1] = 0
   end
   -- if could have roots
   while #pp > 1 do
      local x, root = NewtonRapson(pp)
      if root then 
         -- save and remove the root
         res[#res+1] = x
	 pp = pp / polynom:init({1,-x})
      else break
      end
   end
   return res
end
polynom.about[polynom.real] = {"real(p)", "Find real roots of the polynom.", help.OTHER}

setmetatable(polynom, {__call = function (self, ...) return polynom.new(...) end})
polynom.Poly = 'Poly'
polynom.about[polynom.Poly] = {"Poly(...)", "Create a polynom.", help.NEW}

--- Polynom serialization.
--    @param obj Polynom object.
--    @return String, suitable for exchange.
polynom.serialize = function (obj)
   local s = {}
   for i = 1, #obj do s[#s+1] = string.format("%a", obj[i]) end
   s[#s+1] = "metatablename='Poly'"
   s[#s+1] = "modulename='polynom'"
   return string.format("{%s}", table.concat(s, ','))
end
polynom.about[polynom.serialize] = {"serialize(obj)", "Save polynom internal representation.", help.OTHER}

-- free memory if need
if not lc_version then polynom.about = nil end

return polynom

--===========================
--TODO: polyroot, polyfit

