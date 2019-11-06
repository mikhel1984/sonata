--[[      sonatalib/polynom.lua 

--- Manipulations with polynomials.
--
--  Object structure:        </br>
--  <code>{pn ... p0} </code></br>
--  where each element <i>pk</i> corresponds to coefficient of <i>x^k</i>.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonatalib</a> collection, 2017-2019.

            module 'polynom'
--]]

-------------------- Tests -------------------
--[[TEST

-- import 'polynom'
Poly = require 'sonatalib.polynom'

-- coefficients in ascendant order
a = Poly {1,2,4,3}           
b = Poly {1,1}   
-- polynomial value for x=0             
ans = a:val(0)                --> 3

-- simplified call
ans = a(0)                    --> 3

-- arithmetic
ans = a + b                   --> Poly {1,2,5,4}

ans = a - b                   --> Poly {1,2,3,2}

ans = b * b                   --> Poly {1,2,1}

ans = a / b                   --> Poly {1,1,3}

ans = a % b                   --> Poly {0}

ans = b ^ 3                   --> Poly {1,3,3,1}

-- integration
-- free coefficient is 0
ans = b:int()                 --> Poly {0.5,1,0}

-- derivative
ader = a:der()
-- and its value for x=1
ans = ader(1)                 --> 11

-- build polynomial using roots
ans = Poly.build(1,-1)         --> Poly {1,0,-1}

-- make copy and compare
c = a:copy()
ans = (a == c)                --> true

-- not equal
ans = (b == c)                --> false

-- find real roots
e = a:real()
ans = e[1]                    --~ -1.00

-- fit curve with polynomial
-- of order 2
A={0,1,2,3}
B={-3,2,11,24}
p = Poly.fit(A,B,2)
ans = p(10)                   --0> 227.0

-- simple print
print(a)

-- human-friendly print
-- with variable 's' (default is 'x')
d = Poly {2,-2,1}
ans = d:str('s')              --> '2*s^2 -2*s +1'

--]]

--	LOCAL

local Ver = require "sonatalib.versions"

-- Check object type.
local function ispolynom(x) return type(x) == 'table' and x.ispolynom end

--- Simplify polynomial, remove zeros from the begin.
--  @param P Table of coefficients.
--  @return Simplified polynomial.
local function reduce (P)
   while #P > 1 and  P[1] == 0 do table.remove(P, 1) end
   return P
end

--- Get sum of the table elements.
--  Use product if need.
--  @param P1 First table.
--  @param P2 Second table (optional).
--  @return Sum of elements.
local function getSum(P1,P2)
   local res = 0
   if P2 then
      for i = 1, #P1 do res = res + P1[i]*P2[i] end
   else 
      for i = 1, #P1 do res = res + P1[i] end
   end
   return res
end

--	INFO

local help = LC_DIALOG and (require "sonatalib.help") or {new=function () return {} end}

--	MODULE

local polynom = {
-- marker
type = 'polynomial', ispolynom = true,
-- description
about = help:new("Operations with polynomials."),
}
polynom.__index = polynom

-- dependencies
local done, M = pcall(require,'sonatalib.matrix')
if done then
   polynom.lc_matrix = M 
else
   print('WARNING >> Not available: fit()')
end

--- Correct arguments if need.
--  @param a First object
--  @param b Second object.
--  @return Two polynomial objects.
polynom._args_ = function (a,b)
   a = ispolynom(a) and a or polynom:init({a})
   b = ispolynom(b) and b or polynom:init({b})
   return a, b
end

--- Initialize polynomial from table.
--  @param self Parent object.
--  @param t Table of coefficients.
--  @return Polynomial object.
polynom.init = function (self, t)
   if #t == 0 then t[1] = 0 end
   return setmetatable(t, self)
end

--- Calculate ratio and rest of 2 polynomials.
--  @param P1 First polynomial.
--  @param P2 Second polynomial.
--  @return Ratio and the rest.
polynom._div_ = function (P1,P2)
   local rest, res = polynom.copy(P1), polynom:init({})
   -- update coefficients
   for k = 1,(#P1-#P2+1) do
      local tmp = rest[1]/P2[1]
      res[k] = tmp
      for j = 1,#P2 do rest[j] = rest[j] - tmp*P2[j] end
      -- remove zero element
      table.remove(rest,1)
   end
   return res, rest
end

--- Polynomial value.
--  Can be called with ().
--  @param P Polynomial.
--  @param x Variable.
--  @return Value in the given point.
polynom.val = function (P,x)
   local res = P[1] or 0
   for i = 2, #P do res = res * x + P[i] end
   return res
end
polynom.about[polynom.val] = {"val(P,x)", "Get value of polynomial P in point x."}

-- simplify call
polynom.__call = function (p,x) return polynom.val(p,x) end

--- Create copy of object.
--  @param P Initial polynomial.
--  @return Deep copy.
polynom.copy = function (P) return polynom:init(Ver.move(P,1,#P,1,{})) end
polynom.about[polynom.copy] = {"copy(P)", "Get copy of the polynomial.", help.OTHER}

--- P1 + P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @return Sum.
polynom.__add = function (P1,P2)
   P1, P2 = polynom._args_(P1,P2)
   local t = {}
   -- get sum of equal powers
   local i,j = #P1, #P2
   for k = math.max(i,j),1,-1 do
      t[k] = (P1[i] or 0) + (P2[j] or 0)
      i, j = i-1, j-1
   end
   return polynom:init(t)
end

--- -P
--  @param P Polynomial object.
--  @return Polynomial with inverted signs.
polynom.__unm = function (P)
   local res = {}
   for i = 1, #P do res[i] = -P[i] end
   return polynom:init(res)
end

--- P1 - P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @return Difference.
polynom.__sub = function (P1,P2) return reduce(P1 + (-P2)) end

--- P1 * P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @return Product.
polynom.__mul = function (P1,P2)
   P1,P2 = polynom._args_(P1,P2)
   local res = polynom:init({})
   -- get sum of coefficients
   for i = 1, #P1 do
      for j = 1, #P2 do
         local k = i+j-1
         res[k] = (res[k] or 0) + P1[i]*P2[j]
      end
   end
   return res
end

--- P1 / P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @param Ratio.
polynom.__div = function (P1,P2)
   local res, _ = polynom._div_(polynom._args_(P1,P2))
   return res
end

--- P1 % P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @return Rest.
polynom.__mod = function (P1,P2)
   local _, res = polynom._div_(polynom._args_(P1,P2))
   return res
end

--- P ^ n
--  @param P Polynomial object.
--  @param n Positive integer power.
--  @return Polynomial in given power.
polynom.__pow = function (P,n)
   n = assert(Ver.toInteger(n), "Integer power is expected!")
   if n <= 0 then error("Positive power is expected!") end
   local res, acc = polynom:init({1}), polynom.copy(P)
   local mul = polynom.__mul
   while n > 0 do
      if n%2 == 1 then res = mul(res,acc) end
      if n ~= 1 then acc = mul(acc,acc) end
      n = math.modf(n/2)
   end
   return res
end

--- P1 == P2
--  @param P1 First (polynomial) object.
--  @param P2 Second (polynomial) object.
--  @return True if the objects are equal
polynom.__eq = function (P1,P2)
   if type(P1) ~= type(P2) or P1.type ~= P2.type then return false end
   if #P1 ~= #P2 then return false end
   for i = 1, #P1 do
      if P1[i] ~= P2[i] then return false end
   end
   return true
end

polynom.arithmetic = 'arithmetic'
polynom.about[polynom.arithmetic] = {polynom.arithmetic, "a+b, a-b, a*b, a/b, a^n, -a", help.META}

polynom.comparison = 'comparison'
polynom.about[polynom.comparison] = {polynom.comparison, "a==b, a~=b", help.META}

--- Get derivative.
--  @param P Initial polynomial.
--  @return Derivative polynomial (and its value).
polynom.der = function (P)
   if not ispolynom(P) then error("Polynomial is expected!") end
   local der, pow = {}, #P
   for i = 1, #P-1 do
      table.insert(der, P[i]*(pow-i))
   end
   return polynom:init(der)
end
polynom.about[polynom.der] = {"der(P)", "Calculate derivative of polynomial."}

--- Get integral.
--  @param P Initial polynomial.
--  @param x0 Free coefficient.
--  @return Integral.
polynom.int = function (P,x0)
   if not ispolynom(P) then error("Polynomial is expected!") end
   x0 = x0 or 0
   local int, pow = {}, #P+1
   for i = 1, #P do
      table.insert(int, P[i]/(pow-i))
   end
   table.insert(int, x0)
   return polynom:init(int)
end
polynom.about[polynom.int] = {"int(P[,x0=0])", "Calculate integral, x0 - free coefficient."}

--- Get polynomial from roots.
--  Arguments are a sequence of roots.
--  @param ... List of roots.
--  @return Polynomial object.
polynom.build = function (...)
   local args = {...}
   local res = polynom:init({1})
   local mul = polynom.__mul
   for i = 1, #args do
      res = mul(res, polynom:init({1,-args[i]}))
   end
   return res
end
polynom.about[polynom.build] = {"build(root1,root2,...)", "Return polynomial with given roots.", help.OTHER}

--- String representation.
--  @param P Polynomial object.
--  @return String with coefficients.
polynom.__tostring = function (P) return table.concat(P,' ') end

--- Represent polynomial in "natural" form.
--  @param P Source polynomial.
--  @param var String variable (default is <code>x</code>).
--  @return String with traditional form of equation.
polynom.str = function (P,var)
   var = var or 'x'
   local res, pow = {}, #P-1
   for i = 1,#P-1 do
      local a,b = P[i], P[i+1]
      if a ~= 0 then                                                      -- ignore coefficient = 0
         if a ~= 1 then res[#res+1] = tostring(a)..'*' end                -- eliminate coefficient = 1
         res[#res+1] = var                                                -- variable name
         if pow > 1 then res[#res+1] = '^'..tostring(pow) end             -- eliminate power=1
         res[#res+1] = ' '
      end 
      if type(b) ~= 'number' or b > 0 then res[#res+1] = '+' end
      pow = pow-1  
   end
   local c = P[#P]
   if type(c) ~= 'number' or c ~= 0 then res[#res+1] = tostring(c) end     -- free coefficient
      
   return table.concat(res)
end

--- Find closest root using Newton-Rapson technique
--  @param P Source polynomial.
--  @param x0 Initial value of the root (optional).
--  @param epx Tolerance
--  @return Found value and flag about its correctness.
polynom._NewtonRapson_ = function (P,x0,eps)
   eps = eps or 1e-6
   x0 = x0 or math.random()
   -- prepare variables
   local dp,n,max = P:der(), 0, 30
   local val = polynom.val
   while true do
      -- polynomial value
      local dx = val(P,x0) / val(dp,x0) 
      if math.abs(dx) <= eps or n > max then break
      else
         -- update root value and number of iterations
         x0 = x0 - dx
	 n = n+1
      end
   end
   return x0, (n < max)
end

--- Find real roots of the polynomial.
--  @param P Source polynomial.
--  @return Table with real roots.
polynom.real = function (P)
   local pp, res = polynom.copy(P), {}
   -- zeros
   while pp[#pp] == 0 do
      pp[#pp] = nil
      res[#res+1] = 0
   end
   -- if could have roots
   local n_r = polynom._NewtonRapson_ 
   while #pp > 1 do
      local x, root = n_r(pp)
      if root then 
         -- save and remove the root
         res[#res+1] = x
	 -- divide by (1-x)
	 for i = 2,#pp-1 do pp[i] = pp[i] + x*pp[i-1] end
	 pp[#pp] = nil
      else break
      end
   end
   return res
end
polynom.about[polynom.real] = {"real(p)", "Find real roots of the polynomial.", help.OTHER}

--- Find the best polynomial approximation for the line.
--  @param X Set of independent variables.
--  @param Y Set of dependent variables.
--  @param ord Polynomial order.
--  @return Polynomial object.
polynom.fit = function (X,Y,ord)
   if not (ord > 0 and Ver.mathType(ord) == 'integer') then error('Wrong order!') end
   if #X ~= #Y then error('Wrong data size!') end
   if #X <= ord then error('Too few data points!') end
   -- find sums
   local acc = Ver.move(X,1,#X,1,{})       -- accumulate powers
   local sX, sY = {}, {}                     -- accumulate sums
   local nY = ord
   sY[nY+1] = getSum(Y) 
   for nX = 2*ord,1,-1 do
      sX[nX] = getSum(acc)
      if nY > 0 then sY[nY] = getSum(acc, Y); nY = nY-1 end
      if nX > 1 then
         for i = 1,#acc do acc[i] = acc[i]*X[i] end
      end -- if
   end -- for
   sX[#sX+1] = #X
   -- prepare matrix, reuse accumulator
   for k = 1,ord+1 do
      local mk = {}
      for j = 1, ord+1 do mk[j] = sX[k+j-1] end
      acc[k] = mk
   end
   for k = ord+2,#acc do acc[k] = nil end
   -- add sums to the last "column"
   for i = 1,#acc do table.insert(acc[i],sY[i]) end
   -- solve
   local mat = polynom.lc_matrix
   local gaus = mat.rref(mat(acc))
   local res = {}
   for i = 1,ord+1 do res[i] = gaus:get(i,-1) end
   return polynom:init(res)
end
polynom.about[polynom.fit] = {"fit(X,Y,ord)", "Find polynomial approximation for the line.", help.OTHER}

setmetatable(polynom, {__call = function (self, t) return polynom:init(Ver.move(t,1,#t,1,{})) end})
polynom.Poly = 'Poly'
polynom.about[polynom.Poly] = {"Poly(...)", "Create a polynomial.", help.NEW}

-- free memory if need
if not LC_DIALOG then polynom.about = nil end

return polynom

--===========================
--TODO: polyroot

