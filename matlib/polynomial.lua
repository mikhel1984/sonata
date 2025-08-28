--[[		sonata/lib/polynomial.lua

--- Manipulations with polynomials.
--
--  Object structure: </br>
--  <code>{[0]=p0, ... pn} </code></br>
--  where each element <i>pk</i> corresponds to coefficient of <i>x^k</i>.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2025.

	module 'polynomial'
--]]


-------------------- Tests -------------------
--[[TEST_IT

-- use 'polynomial'
Poly = require 'matlib.polynomial'
-- external dependencies, can be loaded implicitly
Comp = require 'matlib.complex' -- for complex roots
-- make matrix
Mat = require 'matlib.matrix'   
-- for pack/unpack
D = require 'matlib.data'

-- coefficients in ascendant order
a = Poly {1,2,4,3}
b = Poly {1,1}
-- polynomial value for x=0
ans = a:val(0)                -->  3

-- simplified call
ans = a(0)                    -->  3

-- coefficient for x^3
ans = a[3]                    -->  1

-- arithmetic
ans = a + b                   -->  Poly {1,2,5,4}

ans = a - b                   -->  Poly {1,2,3,2}

ans = b * b                   -->  Poly {1,2,1}

ans = a / b                   -->  Poly {1,1,3}

ans = a % b                   -->  0

ans = b ^ 3                   -->  Poly {1,3,3,1}

-- not all powers
x = Poly:x()
ans = x^3-2*x+1               --> Poly {1,0,-2,1}

-- integration
-- free coefficient is 0
ans = b:int()                 -->  Poly {0.5,1,0}

-- derivative
ader = a:der()
-- and its value for x=1
ans = ader(1)                 -->  11

-- build polynomial using roots
ans = Poly:R{1,-1}            -->  Poly {1,0,-1}

-- use complex roots
i = Comp:i()
ans = Poly:R{1, 2+3*i, 2-3*i}  -->  Poly {1, -5, 17, -13}

-- make copy and compare
c = a:copy()
ans = (a == c)                -->  true

-- not equal
ans = (b == c)                -->  false

-- find all roots
g = Poly:R{1, 2, 3+4*i, 3-4*i}
e = g:roots()
ans = e[3]:re()             --.1>  3

-- fit curve with polynomial
-- of order 2
A={0,1,2,3}
B={-3,2,11,24}
p = Poly:fit(A,B,2)
ans = p(10)                 --.0>  227.0

-- simple print
print(a)

-- human-friendly print
-- with variable 's' (default is 'x')
d = Poly {2,-2,1}
ans = d:str('s')              -->  '2*s^2-2*s+1'

-- Lagrange polynomial
-- for tx(x)
X = {-1.5, -0.75, 0, 0.75, 1.5}
Y = {-14.101,-0.931596,0,0.931596,14.101}
p = Poly:lagrange(X,Y)
ans = p[3]                  --.3>  4.83485

-- Taylor series
-- for exp(x) near 0
p = Poly:taylor(0, 1, 1, 1, 1)
ans = p(0.3)                --.2>  math.exp(0.3)

-- linear interpolation
-- use constant values out the interval
p = Poly:lin(X,Y, Y[1], Y[#Y])
y1, n = p:val(0.5)
ans = y1                    --.2>  0.621

-- polynomial index
ans = n                       -->  4

-- simplify call when index is known
ans = p(0.5, n)             --.2>  y1

-- cubic spline
p = Poly:spline(X, Y)
ans = p(0.5)                --.2>  -0.512

-- characteristic polynomial
m = Mat {{1,2}, {3,4}}
p = Poly:char(m)
ans = #p                     -->  2

ans = p[0]                 --.3>  m:det()

-- object pack
t = D:pack(p)
ans = type(t)                -->  'string'

-- unpack
ans = D:unpack(t)            -->  p

--]]

--	LOCAL

local _ext = {
  utils = require("matlib.utils"),
  -- matrix = require("matlib.matrix"),
  -- complex = require("matlib.complex"),
}

local _utils = _ext.utils.utils
local _ver = _ext.utils.versions
local _cross = _ext.utils.cross


-- Return number in trivial case.
local function _numpoly(P) return #P == 0 and _cross.simp(P[0]) or P end


--- Simplify polynomial, remove zeros from the begin.
--  @param t Table of coefficients.
--  @return Simplified polynomial.
local function _reduce (t)
  while #t > 0 and _cross.isZero(t[#t]) do table.remove(t) end
  return t
end


--- Get sum of the table elements.
--  Use product if need.
--  @param P1 First table.
--  @param P2 Second table (optional).
--  @return Sum of elements.
local function _getSum(P1, P2)
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
local function _sortRoots (v1, v2)
  local is1 = type(v1) == 'table'
  local is2 = type(v2) == 'table'
  -- move complex back
  if is1 and v1.iscomplex and not (is2 and v2.iscomplex) then
    return false
  elseif is2 and v2.iscomplex and not (is1 and v1.iscomplex) then
    return true
  else
    return _cross.norm(v1) > _cross.norm(v2)
  end
end


--	INFO

local _help = SonataHelp or {}
-- description
local _about = {
__module__ = "Operations with polynomials."
}


local FIT = 'approximation'


--	MODULE

local polynomial = {
-- marker
type = 'polynomial',
-- simplification
_simp = _numpoly,
-- parameters
--STRIP = 1E-12,
}


-- Check object type.
local function _ispolynomial(v) return getmetatable(v) == polynomial end


-- List of polynomials
local mt_ppval = {}
mt_ppval.__index = mt_ppval


--- Copy of the spline.
--  @return deep copy
mt_ppval.copy = function (self)
  local res = {}
  for i, p in ipairs(self) do res[i] = {p[1], p[2]:copy()} end
  return setmetatable(res, mt_ppval)
end


--- Find derivatives for all the polynomials.
--  @return list of derivatives.
mt_ppval.der = function (self)
  local res = {}
  for i, p in ipairs(self) do res[i] = {p[1], polynomial(p[2]:der())} end
  return setmetatable(res, mt_ppval)
end


--- Find integrals for all the polynomials.
--  @return list of integrals.
mt_ppval.int = function (self)
  local res = {}
  for i, p in ipairs(self) do
    local curr = p[2]:int()
    if i > 1 then
      local prev = res[i-1][2]
      curr[0] = prev(p[1]) - curr(p[1])
    end
    res[i] = {p[1], curr}
  end
  return setmetatable(res, mt_ppval)
end


--- Evaluate value for table of polynomials (piecewise polynomial).
--  @param d Query point.
--  @param N Index of polynomial in the table (optional).
--  @return Found value and the polynomial index.
mt_ppval.val = function (self, d, N)
  if N then
    return self[N][2]:val(d), N
  else
    -- find index n
    local up, low = #self-1, 1
    if d <= self[low][1] then
      N = 1
    elseif d > self[up][1] then
      N = #self
    else
      repeat
        N = math.ceil((up + low)*0.5)
        if d >= self[N][1] then low = N else up = N end
      until up - low <= 1
      N = up
    end
    return self[N][2]:val(d), N
  end
end

-- Simplify call
mt_ppval.__call = mt_ppval.val


--- Find roots of 2nd order polynomial.
--  @return Table with roots.
local function _roots2 (self)
  local a, b, c = self[2], self[1], self[0]
  local sD = _ext.complex.sqrt(b*b - 4*a*c)
  local sgn = b > 0 and 1 or b < 0 and -1 or 0
  sD = -0.5*(b + sgn*sD)
  return {sD/a, c/sD}
end


--- Find roots of 3rd order polynomial.
--  Use Cardano's formula.
--  @return Table with roots.
local function _roots3 (self)
  local t = self[3]
  local a, b, c = self[2]/t, self[1]/t, self[0]/t
  local Q, R = (a*a - 3*b)/9, (2*a^3 - 9*a*b + 27*c)/54
  t = Q^3
  local res = nil
  if R*R < t then
    -- only real roots
    t = math.acos(R / math.sqrt(t))/3
    Q = -2*math.sqrt(Q)   -- reuse
    res = {
      Q*math.cos(t) - a/3, Q*math.cos(t + 2*math.pi/3) - a/3,
      Q*math.cos(t - 2*math.pi/3) - a/3}
  else
    -- can have complex roots
    local A = (R > 0 and -1 or 1)
      * (math.abs(R) + math.sqrt(R*R - t))^(1/3)
    local B = (A == 0 and 0 or Q/A)
    t = _ext.complex(0, math.sqrt(3)/2*(A - B))
    Q = A + B            -- reuse
    res = {Q - a/3, -Q/2 - a/3 + t, -Q/2 - a/3 - t}
  end
  return res
end


--- Check if there are exact roots.
--  @return found roots or nil
local function _exact (self)
  if #self == 1 then
    return {-self[0] / self[1]}
  elseif #self == 2 then
    return _roots2(self)
  elseif #self == 3 then
    return _roots3(self)
  end
  return nil
end


--- Simplify call P * (x - v), in-place
--  @param P Polynomial to update.
--  @param v New root.
local function _multXv (P, v)
  local prev, cur = 0, nil
  for i = 0, #P do
    cur = P[i]
    P[i] = prev - cur*v
    prev = cur
  end
  P[#P+1] = prev
end


--- Find closest root using Newton-Rapson technique
--  @param d0 Initial value of the root (optional).
--  @param tol Tolerance
--  @return found value or nil.
local function _nr (self, d0, tol)
  -- prepare variables
  local dp, max = polynomial.der(self), 30
  local val = polynomial.val
  for i = 1, max do
    local der = _ispolynomial(dp) and val(dp, d0) or dp
    local dx = val(self, d0) / der
    if _cross.norm(dx) <= tol then
      return d0
    else
      -- next approximation
      d0 = d0 - dx
    end
  end
  return nil
end


--- Change order of coefficients and make new polynomial.
--  @param t Table of coefficients, from lowest to highest.
--  @return Polynomial object.
local function _reorder (t)
  local p, k = {[0]=0}, 0
  for i = #t, 1, -1 do
    p[k] = t[i]  -- save in reverse order
    k = k + 1
  end
  return polynomial._init(p)
end


--- P1 + P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @return Sum.
polynomial.__add = function (P1, P2)
  if not _ispolynomial(P2) then
    local v = polynomial._convert(P2)
    return v and P1 + v or P2.__add(P1, P2)
  elseif not _ispolynomial(P1) then
    local v = polynomial._convert(P1)
    return v and v + P2 or error('Not def')
  end
  local t = {}
  -- get sum of equal powers
  for i = 0, math.max(#P1, #P2) do
    t[i] = (P1[i] or 0) + (P2[i] or 0)
  end
  return _numpoly(_reduce(polynomial._init(t)))
end


--- P1 / P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @param Ratio.
polynomial.__div = function (P1, P2)
  if not _ispolynomial(P2) then
    local v = polynomial._convert(P2)
    return v and P1 / v or P2.__div(P1, P2)
  elseif not _ispolynomial(P1) then
    local v = polynomial._convert(P1)
    return v and v / P2 or error('Not def')
  end
  local res, _ = polynomial._div(P1, P2)
  return res
end


--- P1 == P2
--  @param P1 First (polynomial) object.
--  @param P2 Second (polynomial) object.
--  @return True if the objects are equal
polynomial.__eq = function (P1, P2)
  if _ispolynomial(P1) then
    if _ispolynomial(P2) then
      if #P1 ~= #P2 then return false end
      for i = 0, #P1 do
        if not _cross.eq(P1[i], P2[i]) then return false end
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
  if not _ispolynomial(P2) then
    local v = polynomial._convert(P2)
    return v and P1 % v or P2.__mod(P1, P2)
  elseif not _ispolynomial(P1) then
    local v = polynomial._convert(P1)
    return v and v % P2 or error('Not def')
  end
  local _, res = polynomial._div(P1, P2)
  return res
end


--- P1 * P2
--  @param P1 First polynomial or number.
--  @param P2 Second polynomial or number.
--  @return Product.
polynomial.__mul = function (P1, P2)
  if not _ispolynomial(P2) then
    local v = polynomial._convert(P2)
    return v and P1 * v or P2.__mul(P1, P2)
  elseif not _ispolynomial(P1) then
    local v = polynomial._convert(P1)
    return v and v * P2 or error('Not def')
  end
  local res = polynomial._init({[0]=0})
  -- get sum of coefficients
  for i = 0, #P1 do
    local pi = P1[i]
    for j = 0, #P2 do
      local k = i + j
      res[k] = (res[k] or 0) + pi*P2[j]
    end
  end
  return _numpoly(_reduce(res))
end


--- P ^ n
--  @param N Positive integer power.
--  @return Polynomial in given power.
polynomial.__pow = function (self, N)
  N = assert(_ver.toInteger(N), "Integer power is expected!")
  if N <= 0 then error("Positive power is expected!") end
  if #self == 1 and self[1] == 1 and self[0] == 0 then  -- simplified calc
    local res = {}
    for i = 0, N-1 do res[i] = 0 end
    res[N] = 1
    return polynomial._init(res)
  end
  -- general case
  local res, acc = polynomial._init({[0]=1}), polynomial.copy(self)
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
--  @return String with coefficients.
polynomial.__tostring = function (self)
  local t = {}
  for i = #self, 0, -1 do
    local v = self[i]
    table.insert(t, type(v) == 'number' and _utils.numstr(v) or tostring(v))
  end
  return table.concat(t, ' ')
end


--- -P
--  @return Polynomial with inverted signs.
polynomial.__unm = function (self)
  local res = {}
  for i = 0, #self do res[i] = -self[i] end
  return polynomial._init(res)
end


_about['_ar'] = {"arithmetic: a+b, a-b, a*b, a/b, a^n, -a", nil, _help.META}
_about['_cmp'] = {"comparison: a==b, a~=b", nil, _help.META}


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
  return _numpoly(_reorder(res)), _numpoly(_reduce(rest))
end



--- Initialize polynomial from table.
--  @param t Table of coefficients, from highest to lowest power.
--  @return Polynomial object.
polynomial._init = function (t)
  if polynomial.STRIP then
    for i = 0, #t do
      if _cross.norm(t[i]) < polynomial.STRIP then t[i] = 0 end
    end
  end
  return setmetatable(t, polynomial)
end


--- Check if the polynomial is zero constant
--  @return true when P==0
polynomial._isZero = function (self)
  return #self == 0 and _cross.isZero(self[0])
end


--- Dump to binary string.
--  @param acc Accumulator table.
--  @return String with object representation.
polynomial._pack = function (self, acc)
  local spack = string.pack
  local n = #self
  local t = {spack('B', acc['polynomial']), spack('I2', n)}
  t[#t+1] = _utils.pack_seq(self, 0, n, acc)
  return table.concat(t)
end


--- Find real or exact roots of the polynomial.
--  @return Table with real roots and the rest polynomial.
polynomial._real = function (self)
  local pp, res = polynomial.copy(self), {}
  -- zeros
  while #pp > 0 and _cross.isZero(pp[0]) do
    pp[0] = table.remove(pp, 1)
    res[#res+1] = 0
  end
  -- looking for roots
  while #pp > 0 do
    local exact = _exact(pp)
    if exact then
      _ver.move(exact, 1, #exact, #res+1, res)
      pp = 0  -- rest is zero
      break
    end
    -- rough estimate
    local x = _nr(pp, math.random(), 0.1)
    if x then
      -- correction
      x = _nr(self, x, 1E-6)
      if not x then break end
      -- save and remove the root
      res[#res+1] = x
      -- divide by (1-x)
      for i = #pp-1, 1, -1 do pp[i] = pp[i] + x*pp[i+1] end
      pp[0] = table.remove(pp, 1)
    else
      break
    end
  end
  return res, pp
end


--- Strip coefficients of the polynomial.
--  @param tol Required tolerance.
--  @return stripped object.
polynomial._round = function (self, tol)
  for i = 0, #self do
    self[i] = _cross.round(self[i], tol)
  end
  return self
end


--- Undump from binary string.
--  @param src Source string.
--  @param pos Start position.
--  @param acc Accumulator table.
--  @param ver Pack algorithm version.
--  @return Polynomial object.
polynomial._unpack = function (src, pos, acc, ver)
  local t, ord = {}, nil
  ord, pos = string.unpack('I2', src, pos)
  t, pos = _utils.unpack_seq(ord+1, src, pos, acc, ver)
  t = table.move(t, 1, #t, 0, {})
  return polynomial._init(t), pos
end


--- Get polynomial from roots.
--  Arguments are a sequence of roots.
--  @param t List of roots.
--  @return Polynomial object.
polynomial.R = function (_, t)
  local lst = _ver.move(t, 1, #t, 1, {})
  local res = polynomial._init({[0]=1})
  while #lst > 0 do
    local v = table.remove(lst, 1)
    if type(v) == 'table' and v.iscomplex then
      -- looking for pair
      local ind, re, im = nil, v:re(), v:im()
      for i, u in ipairs(lst) do
        if type(u) == 'table' and u.iscomplex and u:re() == re and u:im() == -im
        then
          ind = i; break
        end
      end
      if not ind then
        error ('No pair for '..tostring(v))
      end
      table.remove(lst, ind)
      res = polynomial.__mul(res,
        polynomial._init({[0]= re*re + im*im, -2*re, 1}))
    else
      _multXv(res, v)
    end
  end
  return res
end
_about[polynomial.R] = {":R(roots_t) --> P",
  "Return polynomial with given roots.", _help.OTHER}


--- Find characteristic polinomial for the matrix.
--  @param M Source matrix.
--  @return Characteristic polynomial.
polynomial.char = function (_, M)
  local m = M:copy()
  for i = 1, m:cols() do
    m[i][i] = polynomial._init({[0]=m[i][i], -1})
  end
  return m:minor(0, 0)
end
_about[polynomial.char] = {":char(M) --> P",
  "Return characteristic polinomial for the given matrix."}


--- Create copy of object.
--  @return Deep copy.
polynomial.copy = function (self)
  local res = {}
  for i = 0, #self do
    res[i] = _cross.copy(self[i])
  end
  return polynomial._init(res)
end
_about[polynomial.copy] = {"P:copy() --> cpy_P",
  "Get copy of the polynomial.", _help.OTHER}


--- Get derivative.
--  @return Derivative polynomial (and its value).
polynomial.der = function (self)
  local der = {[0]=0}
  for i = 1, #self do
    der[i-1] = i*self[i]
  end
  return #der == 0 and _cross.simp(der[0]) or polynomial._init(der)
end
_about[polynomial.der] = {"P:der() --> der_P",
  "Calculate derivative of polynomial."}


--- Find the best polynomial approximation for the line.
--  @param X Set of independent variables.
--  @param Y Set of dependent variables.
--  @param ord Polynomial order.
--  @return Polynomial object.
polynomial.fit = function (_, tX, tY, N)
  if not (N > 0 and _ver.mathType(N) == 'integer') then
    error 'Wrong order!'
  end
  if #tX ~= #tY then
    error 'Wrong data size!'
  end
  if #tX <= N then
    error 'Too few points!'
  end
  -- find sums
  local acc = _ver.move(tX, 1, #tX, 1, {})     -- accumulate powers
  local sX, sY = {}, {}              -- accumulate sums
  local nY = N
  sY[nY+1] = _getSum(tY)
  for nX = 2*N, 1, -1 do
    sX[nX] = _getSum(acc)
    if nY > 0 then
      sY[nY] = _getSum(acc, tY)
      nY = nY - 1
    end
    if nX > 1 then
      for i = 1, #acc do acc[i] = acc[i]*tX[i] end
    end
  end
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
  _ext.matrix = _ext.matrix or require('matlib.matrix')
  local mat = _ext.matrix
  local gaus = mat(acc):rref()
  local res = {}
  for i = 1, N+1 do res[i] = gaus(i, -1) end
  return _reorder(res)
end
_about[polynomial.fit] = {":fit(xs_t, ys_t, order_N) --> P",
  "Find polynomial approximation for the line.", FIT}


--- Get integral.
--  @param x0 Free coefficient.
--  @return Integral.
polynomial.int = function (self, d0)
  local int = {[0]= d0 or 0}
  for i = 1, #self+1 do
    int[i] = self[i-1]/i
  end
  return polynomial._init(int)
end
_about[polynomial.int] = {"P:int(x0_d=0) --> int_P",
  "Calculate integral, define free coefficient if need."}


--- Find interpolation polinomial in the Lagrange form.
--  @param X Set of variables.
--  @param Y Set of variables.
--  @return Interpolation polynomial.
polynomial.lagrange = function (_, tX, tY)
  if #tX ~= #tY then
    error 'Wrong data size!'
  end
  local res = polynomial._init({[0]=0})
  for i = 1, #tX do
    -- find basis polynomial
    local p = polynomial._init({[0]=1})
    local den, v = 1, tX[i]
    for j = 1, #tY do
      if i ~= j then
        _multXv(p, tX[j])
        den = den*(v - tX[j])
      end
    end
    -- add
    v = tY[i]/den
    for j = 0, #p do
      res[j] = (res[j] or 0) + p[j]*v
    end
  end
  return _numpoly(_reduce(res))
end
_about[polynomial.lagrange] = {":lagrange(xs_t, ys_t) --> P",
  "Find interpolation polynomial in the Lagrange form.", FIT}


--- Linear data interpolation.
--  @param tX Sequence of independent values.
--  @param tY Sequence of dependent values.
--  @param v0 Value before the range.
--  @param vN Value after the range.
--  @return Table with polynomials for each interval.
polynomial.lin = function (_, tX, tY, v0, vN)
  local res = {}
  local xp, yp = tX[1], tY[1]
  if v0 then res[1] = { xp, polynomial._init({[0]=v0}) } end
  for i = 2, #tX do
    local xi, yi = tX[i], tY[i]
    local k = (yi - yp)/(xi - xp)
    res[#res+1] = {xi, polynomial._init({[0]=(yp - k*xp), k})}
    xp, yp = xi, yi
  end
  if v0 then res[#res+1] = {xp + 1, polynomial._init({[0]= vN or v0}) } end
  return setmetatable(res, mt_ppval)
end
_about[polynomial.lin] = {
  ":lin(xs_t, ys_t, before_d=nil, after_d=before_d) --> Ps_t",
  "Linear data interpolation. Return table with polynomials.", FIT}


--- Find all the polynomial roots.
--  @return table with roots.
polynomial.roots = function (self)
  _ext.complex = _ext.complex or require('matlib.complex')
  -- exact solution or real roots
  local res, pp = polynomial._real(self)
    -- find complex roots
  local Z = _ext.complex
  while _ispolynomial(pp) and #pp > 0 do
    local exact = _exact(pp)
    if exact then
      _ver.move(exact, 1, #exact, #res + 1, res)
      break
    end
    local x = _nr(pp, Z(math.random(), math.random()), 0.1)
    if x then
      x = assert(_nr(self, x, 1E-6))
      res[#res+1] = x
      res[#res+1] = x:conj()
      pp = polynomial._div(pp, polynomial:R({x, x:conj()}))
    else
      break
    end
  end
  table.sort(res, _sortRoots)
  return res
end
_about[polynomial.roots] = {"P:roots() --> roots_t",
  "Find all the polynomial roots.", _help.OTHER}


--- Cubic spline data interpolation.
--  Use 'natural' boundary conditions.
--  @param tX Sequence of independent values.
--  @param tY Sequence of dependent values.
--  @return Table with polynomials for each interval.
polynomial.spline = function (_, tX, tY)
  _ext.matrix = _ext.matrix or require('matlib.matrix')
  local mat, N = _ext.matrix, #tX - 1
  local h, A = {}, mat:zeros(N - 1, N + 2)
  -- prepare matrix
  h[1] = tX[2] - tX[1]
  for i = 2, N do
    h[i] = tX[i+1] - tX[i]
    local p = i - 1
    local row, hp, hi = A[p], h[p], h[i]
    row[p] = hp; row[i] = 2*(hp + hi); row[i+1] = hi
    row[N+2] = 3*(tY[i+1] - tY[i])/hi - 3*(tY[i] - tY[p])/hp
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
    local ai = (b[i+1] - bi)/(3 * hi)
    local ci = (tY[i+1] - di)/hi - hi*(2*bi + b[i+1])/3
    res[i] = {tX[i+1], polynomial._init({
       [0]= ((-ai*xi + bi)*xi - ci)*xi + di,
       xi*(3*ai*xi - 2*bi) + ci, -3*ai*xi + bi, ai})
    }
  end
  return setmetatable(res, mt_ppval)
end
_about[polynomial.spline] = {":spline(xs_t, ys_t) --> Ps_t",
  "Cubic spline data interpolation. Return table with polynomials.", FIT}


--- Represent polynomial in "natural" form.
--  @param s String variable (default is 'x').
--  @return String with traditional form of equation.
polynomial.str = function (self, s)
  s = s or 'x'
  local res, a, b = {}, 0, 0
  for i = #self, 1, -1 do
    a, b = self[i], self[i-1]
    if not _cross.isZero(a) then
      if not _cross.eq(a, 1) then
        res[#res+1] = (type(a) == 'number' and _utils.numstr(a) or tostring(a))..'*'
      end
      res[#res+1] = s
      if i > 1 then res[#res+1] = '^'..tostring(i) end
    end
    if type(b) ~= 'number' or b > 0 then res[#res+1] = '+' end
  end
  if type(b) ~= 'number' or not _cross.isZero(b) then
    res[#res+1] = (type(b) == 'number' and _utils.numstr(b) or tostring(b))
  end
  return table.concat(res)
end
_about[polynomial.str] = {"P:str(char_s='x') --> str",
  "Pretty print for polynomial.", _help.OTHER}


--- Find Taylor series.
--  @param v Argument value.
--  @param vF Function value in v.
--  @param ... Sequence of derivatives fn', fn'' etc.
--  @return Corresponding polynomial.
polynomial.taylor = function (_, v, vF, ...)
  local res = polynomial._init({[0]=vF})
  local p, k = polynomial._init({[0]=1}), 1
  for i, x in ipairs({...}) do
    _multXv(p, v)
    k = k * i
    local w = x/k
    for j = 0, #p do
      res[j] = (res[j] or 0) + w*p[j]
    end
  end
  return _numpoly(res)
end
_about[polynomial.taylor] = {":taylor(x_d, fx_d, [fx'_d, fx''_d,..]) --> P",
  "Get Taylor series.", FIT}


--- Polynomial value.
--  Can be called with ().
--  @param v Variable.
--  @return Value in the given point.
polynomial.val = function (self, v)
  local res = self[#self]
  for i = #self-1, 0, -1 do res = res*v + self[i] end
  return res
end
_about[polynomial.val] = {"P:val(x) --> y",
  "Get value of polynomial P in point x. Equat to P(x)."}
-- simplify call
polynomial.__call = function (p, x) return polynomial.val(p, x) end


--- Get object to write polynomial in 'classical' form.
--  @return Polynomial object.
polynomial.x = function (_)
  return polynomial._init({[0]=0, 1})
end
_about[polynomial.x] = {":x() --> P",
  "Get object to define polynomial as a sum of k*x^n"}


setmetatable(polynomial, {
__call = function (_, t)
  if _ispolynomial(t) then
    return t
  elseif type(t) == 'number' then
    return polynomial._init({[0]=t})
  end
  for _, v in ipairs(t) do
    if not (type(v) == 'number' or
            type(v) == 'table' and v.__add and v.__mul) then
      error("Wrong coefficient "..tostring(v))
    end
  end
  return _reorder(t)
end})
_about[polynomial] = {" {.., v1, v0} --> new_P", "Create a polynomial.", _help.NEW}


-- Comment to remove descriptions
polynomial.about = _about

return polynomial

--===========================
--TODO: other types of splines
--TODO: other conditions for cubic spline
--TODO: Newton polynomial
