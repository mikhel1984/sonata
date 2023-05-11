--[[		sonata/lib/polynomial.lua

--- Manipulations with polynomials.
--
--  Object structure: </br>
--  <code>{[0]=p0, ... pn} </code></br>
--  where each element <i>pk</i> corresponds to coefficient of <i>x^k</i>.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2023.

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
ans = a:val(0)                --> 3

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
ans = Poly:build(1,-1)        --> Poly {1,0,-1}

-- use complex roots
-- don't add conjugated toots
ans = Poly:build(1, Comp(2,3))  --> Poly {1, -5, 17, -13}

-- make copy and compare
c = a:copy()
ans = (a == c)                --> true

-- not equal
ans = (b == c)                --> false

-- find real roots
e = a:real()
ans = e[1]                   --1> -1.00

-- find all roots
g = Poly:build(2, Comp(3,4))
e = g:roots()
ans = e[2]:re()              --1> 3

-- fit curve with polynomial
-- of order 2
A={0,1,2,3}
B={-3,2,11,24}
p = Poly:fit(A,B,2)
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
p = Poly:lagrange(X,Y)
ans = p[3]                   --3> 4.83485

-- Taylor series
-- for exp(x) near 0
p = Poly:taylor(0, 1, 1, 1, 1)
ans = p(0.3)                 --2> math.exp(0.3)

-- linear interpolation
-- use constant values out the interval
p = Poly:lin(X,Y, Y[1], Y[#Y])
y1, n = Poly:ppval(p, 0.5)
ans = y1                     --2> 0.621

-- polynomial index
ans = n                       --> 4

-- simplify call when index is known
ans = Poly:ppval(p, 0.5, n)  --2> y1

-- cubic spline
p = Poly:spline(X, Y)
-- can be called without 'ppval'
ans = p(0.5)                 --2> -0.512

--]]

--	LOCAL

local Ver = require("lib.utils")
local Utils = Ver.utils
local Cross = Ver.cross
Ver = Ver.versions




-- Return number in trivial case.
local function numpoly(P) return #P == 0 and Cross.simp(P[0]) or P end


--- Simplify polynomial, remove zeros from the begin.
--  @param t Table of coefficients.
--  @return Simplified polynomial.
local function reduce (t)
  while #t > 0 and Cross.eq(t[#t], 0) do table.remove(t) end
  return t
end


--- Get sum of the table elements.
--  Use product if need.
--  @param P1 First table.
--  @param P2 Second table (optional).
--  @return Sum of elements.
local function getSum(P1, P2)
  local res = 0
  if P2 then
    for i = 1, #P1 do res = res + P1[i]*P2[i] end
  else
    for i = 1, #P1 do res = res + P1[i] end
  end
  return res
end


--- Condition for the root sorting.
--  @param v1 First element.
--  @param v2 Second element.
--  @return True when v1 goes before v2.
local function sortRoots (v1, v2)
  -- move complex back
  if type(v1) == 'table' and v1.iscomplex
        and (type(v2) ~= 'table' or not v2.iscomplex) then
    return false
  elseif type(v2) == 'table' and v2.iscomplex
        and (type(v1) ~= 'table' or not v1.iscomplex) then
    return true
  else
    return Cross.norm(v1) > Cross.norm(v2)
  end
end


--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Operations with polynomials."
}


local FIT = 'approximation'


--	MODULE

local polynomial = {
-- marker
type = 'polynomial',
-- simplification
_simp = numpoly
}


-- Check object type.
local function ispolynomial(v) return getmetatable(v) == polynomial end


-- Simplify ppval call.
local mt_ppval = {__call = function (...) return polynomial.ppval(nil, ...) end}


--- P1 + P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @return Sum.
polynomial.__add = function (P1, P2)
  if not (ispolynomial(P1) and ispolynomial(P2)) then
    local p = Cross.convert(P1, P2)
    if p then
      return P1 + p
    else
      return Cross.convert(P2, P1) + P2
    end
  end
  local t = {}
  -- get sum of equal powers
  for i = 0, math.max(#P1, #P2) do
    t[i] = (P1[i] or 0) + (P2[i] or 0)
  end
  return numpoly(reduce(polynomial._init(t)))
end


--- P1 / P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @param Ratio.
polynomial.__div = function (P1, P2)
  if not (ispolynomial(P1) and ispolynomial(P2)) then
    local p = Cross.convert(P1, P2)
    if p then
      return P1 / p
    else
      return Cross.convert(P2, P1) / P2
    end
  end
  local res, _ = polynomial._div(P1, P2)
  return res
end


--- P1 == P2
--  @param P1 First (polynomial) object.
--  @param P2 Second (polynomial) object.
--  @return True if the objects are equal
polynomial.__eq = function (P1, P2)
  if ispolynomial(P1) then
    if ispolynomial(P2) then
      if #P1 ~= #P2 then return false end
      for i = 0, #P1 do
        if not Cross.eq(P1[i], P2[i]) then return false end
      end
      return true
    else
      return polynomial.__eq(P2, P1)
    end
  else
    -- only free coefficient
    return #P2 == 0 and P2[0] == P1
  end
end


-- methametods
polynomial.__index = polynomial


--- P1 % P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @return Rest.
polynomial.__mod = function (P1, P2)
  if not (ispolynomial(P1) and ispolynomial(P2)) then
    local p = Cross.convert(P1, P2)
    if p then
      return P1 % p
    else
      return Cross.convert(P2, P1) % P2
    end
  end
  local _, res = polynomial._div(P1, P2)
  return res
end


--- P1 * P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @return Product.
polynomial.__mul = function (P1, P2)
  if not (ispolynomial(P1) and ispolynomial(P2)) then
    local p = Cross.convert(P1, P2)
    if p then
      return P1 * p
    else
      return Cross.convert(P2, P1) * P2
    end
  end
  local res = polynomial._init({[0]=0})
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


--- P ^ n
--  @param P Polynomial object.
--  @param N Positive integer power.
--  @return Polynomial in given power.
polynomial.__pow = function (P, N)
  N = assert(Ver.toInteger(N), "Integer power is expected!")
  if N <= 0 then error("Positive power is expected!") end
  local res, acc = polynomial._init({[0]=1}), polynomial.copy(P)
  while N >= 1 do
    if N % 2 == 1 then res = polynomial.__mul(res, acc) end
    if N ~= 1 then acc = polynomial.__mul(acc, acc) end
    N = math.modf(N / 2)
  end
  return res
end


--- P1 - P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @return Difference.
polynomial.__sub = function (P1, P2)
  return P1 + (-P2)
end


--- String representation.
--  @param P Polynomial object.
--  @return String with coefficients.
polynomial.__tostring = function (P)
  local t = {}
  for i = #P, 0, -1 do
    local v = P[i]
    table.insert(t, type(v) == 'number' and Utils.numstr(v) or tostring(v))
  end
  return table.concat(t, ' ')
end


--- -P
--  @param P Polynomial object.
--  @return Polynomial with inverted signs.
polynomial.__unm = function (P)
  local res = {}
  for i = 0, #P do res[i] = -P[i] end
  return polynomial._init(res)
end


about['_ar'] = {"arithmetic: a+b, a-b, a*b, a/b, a^n, -a", nil, help.META}
about['_cmp'] = {"comparison: a==b, a~=b", nil, help.META}


polynomial._convert = function (v)
  return (type(v) == 'number' or
          type(v) == 'table' and v.__add and v.__mul and v.__div)
          and polynomial._init({[0]=v})
end


--- Calculate ratio and rest of 2 polynomials.
--  @param P1 First polynomial.
--  @param P2 Second polynomial.
--  @return Ratio and the rest.
polynomial._div = function (P1, P2)
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
  rest[0] = rest[0] or 0
  return numpoly(polynomial._reorder(res)), numpoly(reduce(rest))
end


--- Initialize polynomial from table.
--  @param t Table of coefficients, from highest to lowest power.
--  @return Polynomial object.
polynomial._init = function (t)
  return setmetatable(t, polynomial)
end


--- Simplify call P * (x - v), inplace
--  @param P Polynomial object.
--  @param v New root.
polynomial._multXv = function (P, v)
  local prev, cur = 0, nil
  for i = 0, #P do
    cur = P[i]
    P[i] = prev - cur * v
    prev = cur
  end
  P[#P+1] = prev
end


--- Find closest root using Newton-Rapson technique
--  @param P Source polynomial.
--  @param d0 Initial value of the root (optional).
--  @param de Tolerance
--  @return Found value and flag about its correctness.
polynomial._nr = function (P, d0, de)
  -- prepare variables
  local dp, max = polynomial.der(P), 30
  local val = polynomial.val
  for i = 1, max do
    local dx = val(P, d0) / val(dp, d0)
    if (type(dx) == 'number' and math.abs(dx) or dx:abs()) <= de then
      return true, d0
    else
      -- next approximation
      d0 = d0 - dx
    end
  end
  return false
end


--- Change order of coefficients and make new polynomial.
--  @param t Table of coefficients, from lowest to highest.
--  @return Polynomial object.
polynomial._reorder = function (t)
  local p, k = {[0]=0}, 0
  for i = #t, 1, -1 do
    p[k] = t[i]  -- save in reverse order
    k = k + 1
  end
  return polynomial._init(p)
end


--- Find roots of 2nd order polynomial.
--  @param P Source polynomial.
--  @return Table with roots.
polynomial._roots2 = function (P)
  local a, b = P[2], P[1]
  local sD = polynomial.ext_complex.sqrt(b*b - 4*a*P[0])
  local res = {(-b-sD)/(2*a), (-b+sD)/(2*a)}
  table.sort(res, sortRoots)
  return res
end


--- Find roots of 2nd order polynomial.
--  Use Cardano's formula.
--  @param P Source polynomial.
--  @return Table with roots.
polynomial._roots3 = function (P)
  local t = P[3]
  local a, b, c = P[2]/t, P[1]/t, P[0]/t
  local Q, R = (a*a - 3*b)/9, (2*a^3 - 9*a*b + 27*c)/54
  t = Q^3
  local res = nil
  if R*R < t then
    -- only real roots
    t = math.acos(R / math.sqrt(t)) / 3
    Q = -2*math.sqrt(Q)   -- reuse
    res = {
      Q*math.cos(t)-a/3, Q*math.cos(t+2*math.pi/3)-a/3,
      Q*math.cos(t-2*math.pi/3)-a/3}
  else
    -- can have complex roots
    local A = (R > 0 and -1 or 1)
      * (math.abs(R) + math.sqrt(R*R-t))^(1/3)
    local B = (A == 0 and 0 or Q/A)
    t = polynomial.ext_complex(0, math.sqrt(3)/2 * (A-B))
    Q = A + B            -- reuse
    res = {Q-a/3, -Q/2-a/3 + t, -Q/2-a/3 - t}
  end
  table.sort(res, sortRoots)
  return res
end


--- Get polynomial from roots.
--  Arguments are a sequence of roots.
--  @param self Do nothing.
--  @param ... List of roots.
--  @return Polynomial object.
polynomial.build = function (self, ...)
  local res = polynomial._init({[0]=1})
  for _, v in ipairs({...}) do
    if type(v) == 'table' and v.iscomplex then
      local p = polynomial._init({[0] = v:re()^2 + v:im()^2, -2*v:re(), 1})
      res = polynomial.__mul(res, p)
    else
      polynomial._multXv(res, v)
    end
  end
  return res
end
about[polynomial.build] = {":build(root1, [root2,..]) --> P",
  "Return polynomial with given roots.", help.OTHER}


--- Find characteristic polinomial for the matrix.
--  @param self Do nothing.
--  @param M Source matrix.
--  @return Characteristic polynomial.
polynomial.char = function (self, M)
  local m = M:copy()
  for i = 1, m:cols() do
    m[i][i] = polynomial._init({[0]=m[i][i], -1})
  end
  return m:minor(0, 0)
end
about[polynomial.char] = {":char(M) --> P",
  "Return characteristic polinomial for the given matrix."}


--- Create copy of object.
--  @param P Initial polynomial.
--  @return Deep copy.
polynomial.copy = function (P)
  local res = {}
  for i = 0, #P do
    res[i] = Cross.copy(P[i])
  end
  return polynomial._init(res)
end
about[polynomial.copy] = {"P:copy() --> cpy_P",
  "Get copy of the polynomial.", help.OTHER}


--- Get derivative.
--  @param P Initial polynomial.
--  @return Derivative polynomial (and its value).
polynomial.der = function (P)
  local der = {[0]=0}
  for i = 1, #P do
    der[i-1] = i * P[i]
  end
  return numpoly(polynomial._init(der))
end
about[polynomial.der] = {"P:der() --> der_P",
  "Calculate derivative of polynomial."}


--- Find the best polynomial approximation for the line.
--  @param self Do nothing.
--  @param X Set of independent variables.
--  @param Y Set of dependent variables.
--  @param ord Polynomial order.
--  @return Polynomial object.
polynomial.fit = function (self, tX, tY, N)
  if not (N > 0 and Ver.mathType(N) == 'integer') then
    error('Wrong order!')
  end
  if #tX ~= #tY then error('Wrong data size!') end
  if #tX <= N then error('Too few points!') end
  -- find sums
  local acc = Ver.move(tX, 1, #tX, 1, {})     -- accumulate powers
  local sX, sY = {}, {}              -- accumulate sums
  local nY = N
  sY[nY+1] = getSum(tY)
  for nX = 2*N, 1, -1 do
    sX[nX] = getSum(acc)
    if nY > 0 then sY[nY] = getSum(acc, tY); nY = nY-1 end
    if nX > 1 then
      for i = 1, #acc do acc[i] = acc[i]*tX[i] end
    end -- if
  end -- for
  sX[#sX+1] = #tX
  -- prepare matrix, reuse accumulator
  for k = 1, N+1 do
    local mk = {}
    for j = 1, N+1 do mk[j] = sX[k+j-1] end
    acc[k] = mk
  end
  for k = N+2, #acc do acc[k] = nil end
  -- add sums to the last "column"
  for i = 1, #acc do table.insert(acc[i], sY[i]) end
  -- solve
  polynomial.ext_matrix = polynomial.ext_matrix or require('lib.matrix')
  local mat = polynomial.ext_matrix
  local gaus = mat(acc):rref()
  local res = {}
  for i = 1, N+1 do res[i] = gaus(i, -1) end
  return polynomial._reorder(res)
end
about[polynomial.fit] = {":fit(xs_t, ys_t, order_N) --> P",
  "Find polynomial approximation for the line.", FIT}


--- Get integral.
--  @param P Initial polynomial.
--  @param x0 Free coefficient.
--  @return Integral.
polynomial.int = function (P, d0)
  local int = {[0] = (d0 or 0)}
  for i = 1, #P+1 do
    int[i] = P[i-1] / i
  end
  return polynomial._init(int)
end
about[polynomial.int] = {"P:int(x0_d=0) --> int_P",
  "Calculate integral, d0 - free coefficient."}


--- Find interpolation polinomial in the Lagrange form.
--  @param self Do nothing.
--  @param X Set of variables.
--  @param Y Set of variables.
--  @return Interpolation polynomial.
polynomial.lagrange = function (self, tX, tY)
  if #tX ~= #tY then error('Wrong data size!') end
  local res = polynomial._init({[0]=0})
  for i = 1, #tX do
    -- find basis polynomial
    local p = polynomial._init({[0]=1})
    local den, v = 1, tX[i]
    for j = 1, #tY do
      if i ~= j then
        polynomial._multXv(p, tX[j])
        den = den * (v - tX[j])
      end
    end
    -- add
    v = tY[i] / den
    for j = 0, #p do
      res[j] = (res[j] or 0) + p[j]*v
    end
  end
  return numpoly(reduce(res))
end
about[polynomial.lagrange] = {":lagrange(xs_t, ys_t) --> P",
  "Find interpolation polynomial in the Lagrange form.", FIT}


--- Linear data interpolation.
--  @param self Do nothing
--  @param tX Sequence of independent values.
--  @param tY Sequence of dependent values.
--  @return Table with polynomials for each interval.
polynomial.lin = function (self, tX, tY, v0, vN)
  local res = {}
  local xp, yp = tX[1], tY[1]
  if v0 then res[1] = { xp, polynomial._init({[0] = v0}) } end
  for i = 2, #tX do
    local xi, yi = tX[i], tY[i]
    local k = (yi-yp)/(xi-xp)
    res[#res+1] = { xi, polynomial._init({[0]=yp-k*xp, k}) }
    xp, yp = xi, yi
  end
  if v0 then res[#res+1] = { xp+1, polynomial._init({[0] = vN or v0}) } end
  return setmetatable(res, mt_ppval)
end
about[polynomial.lin] = {":lin(xs_t, ys_t, yBefore_d=0, yAfter_d=y0) --> P",
  "Linear data interpolation. Return table with polynomials.", FIT}


--- Evaluate value for table of polynomials (piecewise polynomial).
--  @param self Do nothing.
--  @param tP Table of polynomials in form {{x1, p1}, {x2, p2} ...}.
--  @param x Query point.
--  @param n Index of polynomial in the table (optional).
--  @return Found value and the polynomial index.
polynomial.ppval = function (self, tP, d, N)
  if N then
    return tP[N][2](d), N
  else
    -- find index n
    local up, low = #tP-1, 1
    if d <= tP[low][1] then
      N = 1
    elseif d > tP[up][1] then
      N = #tP
    else
      repeat
        N = math.ceil((up+low)*0.5)
        if d >= tP[N][1] then low = N else up = N end
      until up - low <= 1
      N = up
    end
    return polynomial:ppval(tP, d, N)
  end
end
about[polynomial.ppval] = {":ppval(Ps_t, x_d, [index_N]) --> num",
  "Return value of a piecewise polynomial in the point and the polynomial index.",
  FIT}


--- Find real roots of the polynomial.
--  @param P Source polynomial.
--  @return Table with real roots.
polynomial.real = function (P)
  local pp, res = polynomial.copy(P), {}
  -- zeros
  while #pp > 0 and pp[0] == 0 do
    pp[0] = table.remove(pp, 1)
    res[#res+1] = 0
  end
  -- if could have roots
  local p0 = polynomial.copy(pp)
  while #pp > 0 do
    -- rough estimate
    local root, x = polynomial._nr(pp, math.random(), 0.1)
    if root then
      -- correction
      root, x = polynomial._nr(p0, x, 1E-6)
      if not root then break end
      -- save and remove the root
      res[#res+1] = x
      -- divide by (1-x)
      for i = #pp-1, 1, -1 do pp[i] = pp[i] + x*pp[i+1] end
      pp[0] = table.remove(pp, 1)
    else break
    end
  end
  table.sort(res, function (a, b) return math.abs(a) > math.abs(b) end)
  return res, pp
end
about[polynomial.real] = {"P:real() --> roots_t",
  "Find real roots of the polynomial.", help.OTHER}


--- Find all the polynomial roots.
--  @param P Source polynomial.
--  @return Table with roots.
polynomial.roots = function (P)
  polynomial.ext_complex = polynomial.ext_complex or require('lib.complex')
  -- exact solution
  if #P == 1 then
    return {-P[0] / P[1]}
  elseif #P == 2 then
    return polynomial._roots2(P)
  elseif #P == 3 then
    return polynomial._roots3(P)
  end
  -- approximation
  local r, pp = polynomial.real(P)
  if #r == #P then  -- all roots are real
    return r
  end
  -- find complex roots
  local comp = polynomial.ext_complex
  while #pp > 0 do
    local root, x = polynomial._nr(pp, comp(math.random(), math.random()), 0.1)
    if root then
      _, x = polynomial._nr(P, x, 1E-6)
      r[#r+1] = x
      r[#r+1] = x:conj()
      pp = pp / polynomial.build(x)
    else
      break
    end
  end
  table.sort(r, sortRoots)
  return r
end
about[polynomial.roots] = {"P:roots() --> roots_t",
  "Find all the polynomial roots.", help.OTHER}


--- Cubic spline data interpolation.
--  Use 'natural' boundary conditions.
--  @param self Do nothing.
--  @param tX Sequence of independent values.
--  @param tY Sequence of dependent values.
--  @return Table with polynomials for each interval.
polynomial.spline = function (self, tX, tY)
  polynomial.ext_matrix = polynomial.ext_matrix or require('lib.matrix')
  local mat, N = polynomial.ext_matrix, #tX-1
  local h, A = {}, mat._init(N-1, N+2, {})
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
  for i = 1, A:rows() do A[i][N+1] = A[i][N+2] end
  -- "remove" frist column
  for i = 1, A:rows() do
    local row = A[i]
    for j = 1, N do row[j] = row[j+1] end
  end
  A._cols = N  -- resize matrix
  -- solve
  A = A:rref()
  -- prepare 'b' elements
  local b = {0}   -- "natural" condition, b1 = bn = 0
  for i = 1, A:rows() do b[#b+1] = A[i][N] end
  b[#b+1] = 0
  -- make polynomials
  local res = {}
  for i = 1, N do
    local hi, bi, di, xi = h[i], b[i], tY[i], tX[i]
    local ai = (b[i+1] - bi) / (3 * hi)
    local ci = (tY[i+1] - di) / hi - hi * (2*bi + b[i+1]) / 3
    res[i] = {tX[i+1], polynomial._init({
       [0] = ((-ai*xi + bi)*xi - ci)*xi + di,
       xi*(3*ai*xi - 2*bi) + ci, -3*ai*xi + bi, ai })
    }
  end
  return setmetatable(res, mt_ppval)
end
about[polynomial.spline] = {":spline(xs_t, ys_t) --> Ps_t",
  "Cubic spline data interpolation. Return table with polynomials.", FIT}


--- Represent polynomial in "natural" form.
--  @param P Source polynomial.
--  @param s String variable (default is 'x').
--  @return String with traditional form of equation.
polynomial.str = function (P, s)
  s = s or 'x'
  local res, a, b = {}, 0, 0
  for i = #P, 1, -1 do
    a, b = P[i], P[i-1]
    if a ~= 0 then
      if a ~= 1 then res[#res+1] = tostring(a)..'*' end
      res[#res+1] = s
      if i > 1 then res[#res+1] = '^'..tostring(i) end
    end
    if type(b) ~= 'number' or b > 0 then res[#res+1] = '+' end
  end
  if type(b) ~= 'number' or b ~= 0 then res[#res+1] = tostring(b) end
  return table.concat(res)
end
about[polynomial.str] = {"P:str(char_s='x') --> str",
  "Pretty print for polynomial.", help.OTHER}


--- Find Taylor series.
--  @param self Do nothing.
--  @param v Argument value.
--  @param vF Function value in v.
--  @param ... Sequence of derivatives fn', fn'' etc.
--  @return Corresponding polynomial.
polynomial.taylor = function (self, v, vF, ...)
  local res = polynomial._init({[0]=vF})
  local p, k = polynomial._init({[0]=1}), 1
  for i, x in ipairs({...}) do
    polynomial._multXv(p, v)
    k = k * i
    local w = x / k
    for j = 0, #p do
      res[j] = (res[j] or 0) + w * p[j]
    end
  end
  return numpoly(res)
end
about[polynomial.taylor] = {":taylor(x_d, fx_d, [fx'_d, fx''_d,..]) --> P",
  "Get Taylor series.", FIT}


--- Polynomial value.
--  Can be called with ().
--  @param P Polynomial.
--  @param v Variable.
--  @return Value in the given point.
polynomial.val = function (P, v)
  local res = P[#P]
  for i = #P-1, 0, -1 do res = res * v + P[i] end
  return res
end
about[polynomial.val] = {"P:val(x) --> y",
  "Get value of polynomial P in point x."}
-- simplify call
polynomial.__call = function (p, x) return polynomial.val(p, x) end


setmetatable(polynomial, {
__call = function (self, t)
  for _, v in ipairs(t) do
    if not (type(v) == 'number' or
            type(v) == 'table' and v.__add and v.__mul) then
      error("Wrong coefficient "..tostring(v))
    end
  end
  return polynomial._reorder(t)
end})
about[polynomial] = {" {.., v1, v0} --> new_P", "Create a polynomial.", help.NEW}


-- Comment to remove descriptions
polynomial.about = about

return polynomial

--===========================
--TODO: other types of splines
--TODO: other conditions for cubic spline
--TODO: remove Poly:ppval
--TODO: Newton polynomial
