--[[		sonata/lib/polynomial.lua 

--- Manipulations with polynomials.
--
--  Object structure: </br>
--  <code>{[0]=p0, ... pn} </code></br>
--  where each element <i>pk</i> corresponds to coefficient of <i>x^k</i>.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2021.

	module 'polynomial'
--]]

-------------------- Tests -------------------
--[[TEST

-- use 'polynomial'
Poly = require 'lib.polynomial'
-- external dependencies, can be loaded implicitly
require 'lib.matrix'         -- in Poly.fit
Comp = require 'lib.complex' -- for complex roots

-- coefficients in ascendant order
a = Poly {1,2,4,3}
b = Poly {1,1}  
-- polynomial value for x=0
ans = Poly.val(a,0)           --> 3

-- simplified call
ans = a(0)                    --> 3

-- coefficient for x^3
ans = a[3]                    --> 1

-- arithmetic
ans = a + b                   --> Poly {1,2,5,4}

ans = a - b                   --> Poly {1,2,3,2}

ans = b * b                   --> Poly {1,2,1}

ans = a / b                   --> Poly {1,1,3}

ans = a % b                   --> 0

ans = b ^ 3                   --> Poly {1,3,3,1}

-- integration
-- free coefficient is 0
ans = b:int()                 --> Poly {0.5,1,0}

-- derivative
ader = a:der()
-- and its value for x=1
ans = ader(1)                 --> 11

-- build polynomial using roots
ans = Poly.build(1,-1)        --> Poly {1,0,-1}

-- use complex roots
-- don't add conjugated toots
ans = Poly.build(1, Comp(2,3))  --> Poly {1, -5, 17, -13}

-- make copy and compare
c = a:copy()
ans = (a == c)                --> true

-- not equal
ans = (b == c)                --> false

-- find real roots
e = a:real()
ans = e[1]                   --1> -1.00

-- find all roots
g = Poly.build(2, Comp(3,4))
e = g:roots()
ans = e[2].Re                --1> 3

-- fit curve with polynomial
-- of order 2
A={0,1,2,3}
B={-3,2,11,24}
p = Poly.fit(A,B,2)
ans = p(10)                  --0> 227.0

-- simple print
print(a)

-- human-friendly print
-- with variable 's' (default is 'x')
d = Poly {2,-2,1}
ans = d:str('s')              --> '2*s^2-2*s+1'

-- Lagrange polynomial 
-- for tx(x)
X = {-1.5, -0.75, 0, 0.75, 1.5}
Y = {-14.101,-0.931596,0,0.931596,14.101}
p = Poly.lagrange(X,Y)
ans = p[3]                   --3> 4.83485

-- Taylor series
-- for exp(x) near 0
p = Poly.taylor(0, 1, 1, 1, 1)
ans = p(0.3)                 --2> math.exp(0.3)

-- linear interpolation
-- use constant values out the interval
p = Poly.lin(X,Y, Y[1], Y[#Y]) 
y1, n = Poly.ppval(p, 0.5)
ans = y1                     --2> 0.621

-- polynomial index
ans = n                       --> 4

-- simplify call when index is known
ans = Poly.ppval(p, 0.5, n)  --2> y1

-- cubic spline 
p = Poly.spline(X, Y)
-- can be called without 'ppval'
ans = p(0.5)                 --2> -0.512

--]]

--	LOCAL

local Ver = require("lib.utils").versions

-- Check object type.
local function ispolynomial(v) return type(v) == 'table' and v.ispolynomial end

-- Return number in trivial case.
local function numpoly(P) return #P == 0 and P[0] or P end

--- Simplify polynomial, remove zeros from the begin.
--  @param P Table of coefficients.
--  @return Simplified polynomial.
local function reduce (P)
  while #P > 0 and P[#P] == 0 do table.remove(P) end
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

local help = SonataHelp and (require "core.help") or {new=function () return {} end}

local FIT = 'approximation'

--	MODULE

local polynomial = {
-- marker
type = 'polynomial', ispolynomial = true,
-- description
about = help:new("Operations with polynomials."),
}
polynomial.__index = polynomial

--- Correct arguments if need.
--  @param a First object
--  @param b Second object.
--  @return Two polynomial objects.
polynomial._args_ = function (a,b)
  a = ispolynomial(a) and a or polynomial:_init_({[0]=a})
  b = ispolynomial(b) and b or polynomial:_init_({[0]=b})
  return a, b
end

--- Initialize polynomial from table.
--  @param self Parent object.
--  @param t Table of coefficients, from highest to lowest power.
--  @return Polynomial object.
polynomial._init_ = function (self, t)
  return setmetatable(t, self)
end

--- Change order of coefficients and make new polynomial.
--  @param t Table of coefficients, from lowest to highest.
--  @return Polynomial object.
polynomial._reorder_ = function (t)
  local p, k = {[0]=0}, 0
  for i = #t,1,-1 do
    p[k] = t[i]  -- save in reverse order
    k = k + 1
  end
  return polynomial:_init_(p)
end

--- Calculate ratio and rest of 2 polynomials.
--  @param P1 First polynomial.
--  @param P2 Second polynomial.
--  @return Ratio and the rest.
polynomial._div_ = function (P1,P2)
  local rest, res = polynomial.copy(P1), {}
  local pmax = P2[#P2]
  -- update coefficients
  for k = #P1, #P2, -1 do
    local tmp = rest[#rest] / pmax
    res[#res+1] = tmp
    for j = #P2, 0, -1 do
      local n = k - #P2 + j
      rest[n] = rest[n] - tmp*P2[j]
    end
    table.remove(rest)
  end
  return numpoly(polynomial._reorder_(res)), numpoly(reduce(rest))
end

--- Polynomial value.
--  Can be called with ().
--  @param P Polynomial.
--  @param x Variable.
--  @return Value in the given point.
polynomial.val = function (P,x)
  local res = P[#P] 
  for i = #P-1, 0, -1 do res = res * x + P[i] end
  return res
end
polynomial.about[polynomial.val] = {"val(P,x)", "Get value of polynomial P in point x."}

-- simplify call
polynomial.__call = function (p,x) return polynomial.val(p,x) end

--- Create copy of object.
--  @param P Initial polynomial.
--  @return Deep copy.
polynomial.copy = function (P) return polynomial:_init_(Ver.move(P,0,#P,0,{})) end
polynomial.about[polynomial.copy] = {"copy(P)", "Get copy of the polynomial.", help.OTHER}

--- P1 + P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @return Sum.
polynomial.__add = function (P1,P2)
  P1, P2 = polynomial._args_(P1,P2)
  local t = {}
  -- get sum of equal powers
  for i = 0, math.max(#P1,#P2) do
    t[i] = (P1[i] or 0) + (P2[i] or 0)
  end
  return numpoly(reduce(polynomial:_init_(t)))
end

--- -P
--  @param P Polynomial object.
--  @return Polynomial with inverted signs.
polynomial.__unm = function (P)
  local res = {}
  for i = 0, #P do res[i] = -P[i] end
  return polynomial:_init_(res)
end

--- P1 - P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @return Difference.
polynomial.__sub = function (P1,P2) 
  return numpoly(reduce(P1 + (-P2)))
end

--- P1 * P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @return Product.
polynomial.__mul = function (P1,P2)
  P1,P2 = polynomial._args_(P1,P2)
  local res = polynomial:_init_({[0]=0})
  -- get sum of coefficients
  for i = 0, #P1 do
    local pi = P1[i]
    for j = 0, #P2 do
      local k = i+j
      res[k] = (res[k] or 0) + pi*P2[j]
    end
  end
  return numpoly(reduce(res))
end

--- P1 / P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @param Ratio.
polynomial.__div = function (P1,P2)
  local res, _ = polynomial._div_(polynomial._args_(P1,P2))
  return res
end

--- P1 % P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @return Rest.
polynomial.__mod = function (P1,P2)
  local _, res = polynomial._div_(polynomial._args_(P1,P2))
  return res
end

--- P ^ n
--  @param P Polynomial object.
--  @param n Positive integer power.
--  @return Polynomial in given power.
polynomial.__pow = function (P,n)
  n = assert(Ver.toInteger(n), "Integer power is expected!")
  if n <= 0 then error("Positive power is expected!") end
  local res, acc = polynomial:_init_({[0]=1}), polynomial.copy(P)
  while n >= 1 do
    if n % 2 == 1 then res = polynomial.__mul(res,acc) end
    if n ~= 1 then acc = polynomial.__mul(acc,acc) end
    n = math.modf(n / 2)
  end
  return res
end

--- P1 == P2
--  @param P1 First (polynomial) object.
--  @param P2 Second (polynomial) object.
--  @return True if the objects are equal
polynomial.__eq = function (P1,P2)
  if type(P1) ~= type(P2) or P1.type ~= P2.type then return false end
  if #P1 ~= #P2 then return false end
  for i = 0, #P1 do
    if P1[i] ~= P2[i] then return false end
  end
  return true
end

polynomial.arithmetic = 'arithmetic'
polynomial.about[polynomial.arithmetic] = {polynomial.arithmetic, "a+b, a-b, a*b, a/b, a^n, -a", help.META}

polynomial.comparison = 'comparison'
polynomial.about[polynomial.comparison] = {polynomial.comparison, "a==b, a~=b", help.META}

--- Get derivative.
--  @param P Initial polynomial.
--  @return Derivative polynomial (and its value).
polynomial.der = function (P)
  local der = {[0]=0}
  for i = 1, #P do
    der[i-1] = i * P[i]
  end
  return numpoly(polynomial:_init_(der))
end
polynomial.about[polynomial.der] = {"der(P)", "Calculate derivative of polynomial."}

--- Get integral.
--  @param P Initial polynomial.
--  @param x0 Free coefficient.
--  @return Integral.
polynomial.int = function (P,x0)
  local int = {[0] = (x0 or 0)}
  for i = 1, #P+1 do
    int[i] = P[i-1] / i
  end
  return polynomial:_init_(int)
end
polynomial.about[polynomial.int] = {"int(P[,x0=0])", "Calculate integral, x0 - free coefficient."}

--- Simplify call P * (x - v), inplace
--  @param P Polynomial object.
--  @param v New root.
polynomial._multXv_ = function (P,v)
  local prev, cur = 0
  for i = 0, #P do
    cur = P[i]
    P[i] = prev - cur * v
    prev = cur
  end
  P[#P+1] = prev
end

--- Get polynomial from roots.
--  Arguments are a sequence of roots.
--  @param ... List of roots.
--  @return Polynomial object.
polynomial.build = function (...)
  local res = polynomial:_init_({[0]=1})
  for _,v in ipairs({...}) do
    if type(v) == 'table' then
      local p = polynomial:_init_({[0] = v.Re^2 + v.Im^2, -2*v.Re, 1})
      res = polynomial.__mul(res, p)
    else
      polynomial._multXv_(res, v)
    end
  end
  return res
end
polynomial.about[polynomial.build] = {"build(root1,root2,...)", "Return polynomial with given roots.", help.OTHER}

--- String representation.
--  @param P Polynomial object.
--  @return String with coefficients.
polynomial.__tostring = function (P) 
  local t = {}
  for i = #P, 0, -1 do table.insert(t, P[i]) end
  return table.concat(t,' ') 
end

--- Represent polynomial in "natural" form.
--  @param P Source polynomial.
--  @param var String variable (default is <code>x</code>).
--  @return String with traditional form of equation.
polynomial.str = function (P,var)
  var = var or 'x'
  local res, a, b = {}
  for i = #P, 1, -1 do
    a, b = P[i], P[i-1]
    if a ~= 0 then
      if a ~= 1 then res[#res+1] = tostring(a)..'*' end
      res[#res+1] = var
      if i > 1 then res[#res+1] = '^'..tostring(i) end
    end
    if type(b) ~= 'number' or b > 0 then res[#res+1] = '+' end
  end
  if type(b) ~= 'number' or b ~= 0 then res[#res+1] = tostring(b) end
  return table.concat(res)
end

--- Find closest root using Newton-Rapson technique
--  @param P Source polynomial.
--  @param x0 Initial value of the root (optional).
--  @param epx Tolerance
--  @return Found value and flag about its correctness.
polynomial._NR_ = function (P, x0, eps)
  -- prepare variables
  local dp, max = polynomial.der(P), 30
  local val = polynomial.val
  for i = 1, max do
    local dx = val(P,x0) / val(dp,x0) 
    if (type(dx) == 'number' and math.abs(dx) or dx:abs()) <= eps then
      return true, x0
    else
      -- next approximation
      x0 = x0 - dx
    end
  end
  return false
end

--- Find real roots of the polynomial.
--  @param P Source polynomial.
--  @return Table with real roots.
polynomial.real = function (P)
  local pp, res = polynomial.copy(P), {}
  -- zeros
  while #pp > 0 and pp[0] == 0 do
    pp[0] = table.remove(pp,1)
    res[#res+1] = 0
  end
  -- if could have roots
  local p0 = polynomial.copy(pp)
  while #pp > 0 do
    -- rough estimate
    local root, x = polynomial._NR_(pp, math.random(), 0.1)
    if root then 
      -- correction 
      root, x = polynomial._NR_(p0, x, 1E-6)
      if not root then break end
      -- save and remove the root
      res[#res+1] = x
      -- divide by (1-x)
      for i = #pp-1,1,-1 do pp[i] = pp[i] + x*pp[i+1] end
      pp[0] = table.remove(pp,1)
    else break
    end
  end
  return res, pp
end
polynomial.about[polynomial.real] = {"real(p)", "Find real roots of the polynomial.", help.OTHER}

--- Find roots of 2nd order polynomial.
--  @param P Source polynomial.
--  @return Table with roots.
polynomial._roots2_ = function (P)
  local a, b = P[2], P[1]
  local sD = polynomial.ext_complex.sqrt(b*b - 4*a*P[0])
  return {(-b-sD)/(2*a), (-b+sD)/(2*a)}
end

--- Find roots of 2nd order polynomial.
--  Use Cardano's formula.
--  @param P Source polynomial.
--  @return Table with roots.
polynomial._roots3_ = function (P)
  local t = P[3]
  local a, b, c = P[2]/t, P[1]/t, P[0]/t
  local Q, R = (a*a - 3*b)/9, (2*a^3 - 9*a*b + 27*c)/54
  t = Q^3
  if R*R < t then
    -- only real roots
    t = math.acos(R / math.sqrt(t)) / 3
    Q = -2*math.sqrt(Q)   -- reuse
    return {Q*math.cos(t)-a/3, Q*math.cos(t+2*math.pi/3)-a/3, Q*math.cos(t-2*math.pi/3)-a/3}
  else
    -- can have complex roots
    local A = (R > 0 and -1 or 1) * math.pow(math.abs(R) + math.sqrt(R*R-t), 1/3)
    local B = (A == 0 and 0 or Q/A)
    t = polynomial.ext_complex(0, math.sqrt(3)/2 * (A-B))
    Q = A + B            -- reuse
    return {Q-a/3, -Q/2-a/3 + t, -Q/2-a/3 - t}
  end
end

--- Find all the polynomial roots.
--  @param P Source polynomial.
--  @return Table with roots.
polynomial.roots = function (P)
  polynomial.ext_complex = polynomial.ext_complex or require('lib.complex')
  -- exact solution
  if #P == 1 then 
    return {-P[0] / P[1]}
  elseif #P == 2 then
    return polynomial._roots2_(P)
  elseif #P == 3 then 
    return polynomial._roots3_(P)
  end
  -- approximation
  local r, pp = polynomial.real(P)
  if #r == #P then  -- all roots are real
    return r 
  end
  -- find complex roots
  local comp = polynomial.ext_complex
  while #pp > 0 do
    local root, x = polynomial._NR_(pp, comp(math.random(), math.random()), 0.1)
    if root then
      _, x = polynomial._NR_(P, x, 1E-6)
      r[#r+1] = x
      r[#r+1] = x:conj()
      pp = pp / polynomial.build(x)
    else 
      break 
    end
  end
  return r
end
polynomial.about[polynomial.roots] = {"roots(P)", "Find all the polynomial roots.", help.OTHER}

--- Find the best polynomial approximation for the line.
--  @param X Set of independent variables.
--  @param Y Set of dependent variables.
--  @param ord Polynomial order.
--  @return Polynomial object.
polynomial.fit = function (X,Y,ord)
  if not (ord > 0 and Ver.mathType(ord) == 'integer') then error('Wrong order!') end
  if #X ~= #Y then error('Wrong data size!') end
  if #X <= ord then error('Too few points!') end
  -- find sums
  local acc = Ver.move(X,1,#X,1,{})     -- accumulate powers
  local sX, sY = {}, {}              -- accumulate sums
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
  polynomial.ext_matrix = polynomial.ext_matrix or require('lib.matrix')
  local mat = polynomial.ext_matrix
  local gaus = mat.rref(mat(acc))
  local res = {}
  for i = 1,ord+1 do res[i] = gaus:get(i,-1) end
  return polynomial._reorder_(res)
end
polynomial.about[polynomial.fit] = {"fit(X,Y,ord)", "Find polynomial approximation for the line.", FIT}

--- Find interpolation polinomial in the Lagrange form.
--  @param X Set of variables.
--  @param Y Set of variables.
--  @return Interpolation polynomial.
polynomial.lagrange = function (X,Y)
  if #X ~= #Y then error('Wrong data size!') end
  local res = polynomial:_init_({[0]=0})
  for i = 1,#X do
    -- find basis polynomial
    local p = polynomial:_init_({[0]=1})
    local den, v = 1, X[i]
    for j = 1,#Y do
      if i ~= j then
        polynomial._multXv_(p, X[j])
        den = den * (v - X[j])
      end
    end
    -- add
    v = Y[i] / den
    for j = 0,#p do
      res[j] = (res[j] or 0) + p[j]*v
    end
  end
  return numpoly(reduce(res))
end
polynomial.about[polynomial.lagrange] = {"lagrange(X,Y)", "Find interpolation polynomial in the Lagrange form.", FIT}

--- Find Taylor series.
--  @param x Argument value.
--  @param f Function value in x.
--  @param ... Sequence of derivatives f', f'' etc.
--  @return Corresponding polynomial.
polynomial.taylor = function (x,f,...)
  local res = polynomial:_init_({[0]=f})
  local p, k = polynomial:_init_({[0]=1}), 1
  for i,v in ipairs({...}) do
    polynomial._multXv_(p, x)
    k = k * i
    local w = v / k
    for j = 0, #p do
      res[j] = (res[j] or 0) + w * p[j]
    end
  end
  return numpoly(res)
end
polynomial.about[polynomial.taylor] = {"taylor(x,f[,f',f''..])", "Get Taylor series.", FIT}

--- Cubic spline data interpolation.
--  Use 'natural' boundary conditions. 
--  @param tX Sequence of independent values.
--  @param tY Sequence of dependent values.
--  @return Table with polynomials for each interval.
polynomial.spline = function (tX, tY)
  polynomial.ext_matrix = polynomial.ext_matrix or require('lib.matrix')
  local mat, N = polynomial.ext_matrix, #tX-1
  local h, A = {}, mat:_init_(N-1, N+2, {})
  -- prepare matrix
  h[1] = tX[2]-tX[1] 
  for i = 2, N do
    h[i] = tX[i+1] - tX[i]
    local p = i-1
    local row, hp, hi = A[p], h[p], h[i]
    row[p] = hp; row[i] = 2*(hp+hi); row[i+1] = hi
    row[N+2] = 3*(tY[i+1]-tY[i])/hi - 3*(tY[i]-tY[p])/hp
  end
  -- "remove" penultimate column
  for i = 1, A.rows do A[i][N+1] = A[i][N+2] end
  -- "remove" frist column
  for i = 1, A.rows do 
    local row = A[i]
    for j = 1, N do row[j] = row[j+1] end
  end
  A.cols = N  -- resize matrix
  -- solve 
  A = mat.rref(A)
  -- prepare 'b' elements
  local b = {0}   -- "natural" condition, b1 = bn = 0
  for i = 1, A.rows do b[#b+1] = A[i][N] end
  b[#b+1] = 0
  -- make polynomials
  local res = {}
  for i = 1, N do
    local hi, bi, di, xi = h[i], b[i], tY[i], tX[i]
    local ai = (b[i+1] - bi) / (3 * hi)
    local ci = (tY[i+1] - di) / hi - hi * (2*bi + b[i+1]) / 3
    res[i] = {tX[i+1], polynomial:_init_({ 
       [0] = ((-ai*xi + bi)*xi - ci)*xi + di,
       xi*(3*ai*xi - 2*bi) + ci, -3*ai*xi + bi, ai })
    }
  end
  return setmetatable(res, polynomial._metappval_)
end
polynomial.about[polynomial.spline] = {"spline(tX,tY)", "Cubic spline data interpolation. Return table with polynomials.", FIT}

--- Linear data interpolation.
--  @param tX Sequence of independent values.
--  @param tY Sequence of dependent values.
--  @return Table with polynomials for each interval.
polynomial.lin = function (tX, tY, v0, vN)
  local res = {}
  local xp, yp = tX[1], tY[1]
  if v0 then res[1] = { xp, polynomial:_init_({[0] = v0}) } end
  for i = 2,#tX do
    xi, yi = tX[i], tY[i]
    local k = (yi-yp)/(xi-xp)
    res[#res+1] = { xi, polynomial:_init_({[0]=yp-k*xp, k}) }
    xp, yp = xi, yi
  end
  if v0 then res[#res+1] = { xp+1, polynomial:_init_({[0] = vN or v0}) } end
  return setmetatable(res, polynomial._metappval_)
end
polynomial.about[polynomial.lin] = {"lin(tX,tY[,v0=0,vN=v0])", "Linear data interpolation. Return table with polynomials.", FIT}

--- Evaluate value for table of polynomials (piecewise polynomial).
--  @param tP Table of polynomials in form {{x1, p1}, {x2, p2} ...}.
--  @param x Query point.
--  @param n Index of polynomial in the table (optional).
--  @return Found value and the polynomial index.
polynomial.ppval = function (tP, x, n)
  if n then
    return tP[n][2](x), n
  else
    -- find index n
    local up, low = #tP-1, 1
    if x <= tP[low][1] then 
      n = 1
    elseif x > tP[up][1] then
      n = #tP
    else
      repeat
        n = math.ceil((up+low)*0.5) 
        if x >= tP[n][1] then low = n else up = n end
      until up - low <= 1
      n = up
    end
    return polynomial.ppval(tP, x, n)
  end
end
polynomial.about[polynomial.ppval] = {"ppval(tP,x[,n]", "Return value of a piecewise polynomial in the point and the polynomial index.", FIT} 

-- Simplify ppval call.
polynomial._metappval_ = {__call = polynomial.ppval}

setmetatable(polynomial, {__call = function (self, t) return polynomial._reorder_(t) end})
polynomial.Poly = 'Poly'
polynomial.about[polynomial.Poly] = {"Poly(...)", "Create a polynomial.", help.NEW}

-- Uncomment to remove descriptions
--polynomial.about = nil

return polynomial

--===========================
--TODO: other types of splines
--TODO: other conditions for cubic spline
