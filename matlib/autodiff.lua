--[[		sonata/matlib/autodiff.lua

--- Automatic differentiation based computation of the
--  1st, 2nd and cross derivative.
--  Based on <a href="https://github.com/tisimst/ad">ad</a> Python library.
--
--  Object structure </br>
--  <code> {_={value, name}, _der={1st_der, 2nd_der, cross_der}}  </code>
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2026.

	module 'autodiff'
--]]


--[[TEST_IT

-- use 'autodiff'
Ad = require 'matlib.autodiff'
-- for jacobian/hessian
require 'matlib.matrix'

-- init
x = Ad(3, "x")
y = Ad(4)  -- name is optional
-- get value
ans = x()                     -->  3

-- arithmetic operation
s = x + y
-- first derivative
ans = s:d(x)                --.2>  1

-- name can be used
ans = s:d "x"               --.2>  1

-- second derivative
ans = s:d2(y)               --.2>  0

s = x * y
ans = s:d(x)                --.2>  y()

-- jacobian (gradient)
v = s:grad {x, y}  -- get vector
ans = v(1)                  --.2>  y()

-- hessian
v = s:hess {x, y}
ans = v(1,2)                --.2>  1

-- ds^2/dxdy
ans = s:d2(x, y)            --.2>  1.0

ans = (y - x):d(x)          --.2>  -1.0

s = x / y
ans = s:d(y)                --.2>  -x()/y()^2

ans = s:d2(y, x)            --.2>  -1/y()^2

s = x^y
-- equal to s:d2(x)
ans = s:d2(x, x)            --.2>  y()*(y()-1)*x()^(y()-2)

ans = s:d(y)                --.2>  x()^y()*math.log(x())

ans = x:sin():d(x)          --.2>  math.cos(x())

ans = x:cos():d2(x)         --.2>  -math.cos(x())

ans = (x*y):tan():d(x)      --.2>  y()/math.cos(x()*y())^2

ans = y:sqrt():d(y)         --.2>  1/(2*math.sqrt(y()))

ans = x:exp():d2(x)         --.2>  math.exp(x())

ans = x:log():d(x)          --.2>  1/x()

--]]


--	LOCAL

-- Sonata dependencies
local _ext = {
  utils = require("matlib.utils"),
  complex = require("matlib.complex"),
  -- matrix = require("matlib.matrix"),
}

-- Inter-module functionality
local _cross = _ext.utils.cross
-- Additional math functions
local _calc = _ext.utils.calc
-- complex square root
local _sqrt = _ext.complex.sqrt

-- Constants
local _EMPTY, _ZEROS = {}, {0, 0}

-- Function info
local FUNCTIONS = 'functions'


--- Find reference by name.
--  @param var Variable.
--  @param nm Name string.
--  @return variable or nil.
local function _byName (var, nm)
  for k in pairs(var._der[1]) do
    if k[2] == nm then return k end
  end
end


--- Find derivatives using chain rule.
--  @param ads List of autodiff objects.
--  @param vars List of variables.
--  @param lins List of coefficients for the 1st derivative.
--  @param quads List of coefficients for the 2nd derivative.
--  @param cross List of coefficients for cross derivatives.
--  @return tables with values for 1st, 2nd and cross derivatives.
local function _chainRule (ads, vars, lins, quads, cross)
  -- initialization
  local linVars, quadVars, crossVars = {}, {}, {}
  for i, v in ipairs(vars) do
    linVars[v] = 0
    quadVars[v] = 0
    local t = {}
    for j = i+1, #vars do t[ vars[j] ] = 0 end
    crossVars[v] = t
  end

  -- chain rule
  for i = 1, #vars do
    local v1 = vars[i]
    for j = i, #vars do
      local v2 = vars[j]
      for k = 1, #lins do
        local der, dh, d2h = ads[k]._der, lins[k], quads[k]
        local fv1 = der[1][v1] or 0  -- 1sd derivative
        if i == j then
          -- first order terms
          linVars[v1] = linVars[v1] + dh*fv1
          -- pure second order terms
          quadVars[v1] = quadVars[v1] + dh*(der[2][v1] or 0) + d2h*fv1*fv1
        else
          local d3 = der[3]
          local fdc = d3[v1] and d3[v1][v2] or d3[v2] and d3[v2][v1] or 0
          -- cross product of second order terms
          local tmp = dh*fdc + d2h*fv1*(der[1][v2] or 0)
          local t = crossVars[v1]
          t[v2] = t[v2] + tmp
        end
      end
      -- update quadratic and cross product terms
      if #ads > 1 then
        local d1, d2 = ads[1]._der[1], ads[2]._der[1]
        local tmp = (d1[v2] or 0) * (d2[v1] or 0)
        if i == j then
          quadVars[v1] = quadVars[v1] + 2*cross*tmp
        else
          tmp = tmp + (d1[v1] or 0) * (d2[v2] or 0)
          local t = crossVars[v1]
          t[v2] = t[v2] + cross*tmp
        end
      end
    end
  end
  return linVars, quadVars, crossVars
end


--- Choose function to apply, standard of module specific.
--  @param x Function argument.
--  @param nm Function name.
--  @param f Standard implementation.
--  @return function value.
local function _fun (x, nm, f)
  return type(x) == "number" and f(x)
      or x[nm] and x[nm](x)
      or f(x:float())
end


--- Extract list of variables.
--  @param a First autodiff object.
--  @param b Second autodiff object (or nil).
--  @return list of variables.
local function _getVars (a, b)
  local res = {}
  for k in pairs(a) do res[#res+1] = k end
  if b then
    for k in pairs(b) do
      if not a[k] then res[#res+1] = k end
    end
  end
  return res
end


--- Check compatibility and return argument value.
--  @param v Object to check.
--  @return float number or complex or nil
local function _value (v)
  return type(v) == "number" and v
    or type(v) == "table" and
      (v.float and v:float() or v._norm and v)
    or nil   -- either number or complex/quaternion
end


--	INFO

local _help = SonataHelp or {}  -- optional
-- description
local _about = {
__module__ = "Automatic differentiation."
}


--	MODULE

local autodiff = {
-- mark
type = 'autodiff',
}
-- methametods
autodiff.__index = autodiff


--- Check object type.
--  @param v Object.
--  @return True if the object is autodiff.
local function _isautodiff(v) return getmetatable(v) == autodiff end


--- A1 + A2
--  @param A1 autodiff object or number.
--  @param A2 autodiff object or number.
--  @return sum of numbers.
autodiff.__add = function (A1, A2)
  if not _isautodiff(A2) then
    local w = autodiff._convert(A2)
    return w and A1 + w or A2.__add(A1, A2)
  elseif not _isautodiff(A1) then
    local w = autodiff._convert(A1)
    return w and w + A2 or error("Not def")
  end
  -- process
  local x, y = A1._[1], A2._[1]
  local vars = _getVars(A1._der[1], A2._der[1])
  local f = x + y
  if #vars == 0 then return f end
  local linVars, quadVars, crossVars = _chainRule(
    {A1, A2}, vars, {1, 1}, _ZEROS, 0)
  return autodiff._init(f, linVars, quadVars, crossVars)
end


--- Get object value.
--  @return value of an autodiff number/function.
autodiff.__call = function (self) return self._[1] end


--- A1 / A2
--  @param A1 autodiff object or number.
--  @param A2 autodiff object or number.
--  @return ratio of numbers.
autodiff.__div = function (A1, A2)
  if not _isautodiff(A2) then
    local w = autodiff._convert(A2)
    return w and A1 / w or A2.__div(A1, A2)
  elseif not _isautodiff(A1) then
    local w = autodiff._convert(A1)
    return w and w / A2 or error("Not def")
  end
  -- process
  local x, y = A1._[1], A2._[1]
  local vars = _getVars(A1._der[1], A2._der[1])
  local f, yy = x/y, 1.0/(y*y)
  if #vars == 0 then return f end
  local linVars, quadVars, crossVars = _chainRule(
    {A1, A2}, vars, {1/y, -f/y}, {0, 2*f*yy}, -yy)
  return autodiff._init(f, linVars, quadVars, crossVars)
end


--- A1 == A2
--  @param A1 autodiff object or number.
--  @param A2 autodiff object or number.
--  @return equality of numbers.
autodiff.__eq = function (A1, A2)
  if not (_isautodiff(A1) and _isautodiff(A2)) then
    local a = _cross.convert(A1, A2)
    if a then
      return A1 == a
    else
      return _cross.convert(A2, A1) == A2
    end
  end
  return _cross.eq(A1._[1], A2._[1])
end


--- A1 < A2
--  @param A1 autodiff object or number.
--  @param A2 autodiff object or number.
--  @return true when first argument is less then the second one.
autodiff.__lt = function (A1, A2)
  if not (_isautodiff(A1) and _isautodiff(A2)) then
    local a = _cross.convert(A1, A2)
    if a then
      return A1 < a
    else
      return _cross.convert(A2, A1) < A2
    end
  end
  return A1._[1] < A2._[1]
end


--- A1 <= A2
--  @param A1 autodiff object or number.
--  @param A2 autodiff object or number.
--  @return true when first argument is less then the second one.
autodiff.__le = function (A1, A2)
  if not (_isautodiff(A1) and _isautodiff(A2)) then
    local a = _cross.convert(A1, A2)
    if a then
      return A1 <= a
    else
      return _cross.convert(A2, A1) <= A2
    end
  end
  return A1._[1] <= A2._[1]
end


--- A1 * A2
--  @param A1 autodiff object or number.
--  @param A2 autodiff object or number.
--  @return product of numbers.
autodiff.__mul = function (A1, A2)
  if not _isautodiff(A2) then
    local w = autodiff._convert(A2)
    return w and A1 * w or A2.__mul(A1, A2)
  elseif not _isautodiff(A1) then
    local w = autodiff._convert(A1)
    return w and w * A2 or error("Not def")
  end
  -- process
  local x, y = A1._[1], A2._[1]
  local vars = _getVars(A1._der[1], A2._der[1])
  local f = x * y
  if #vars == 0 then return f end
  local linVars, quadVars, crossVars = _chainRule(
    {A1, A2}, vars, {y, x}, _ZEROS, 1)
  return autodiff._init(f, linVars, quadVars, crossVars)
end


--- A1 ^ A2
--  @param A1 autodiff object or number.
--  @param A2 autodiff object or number.
--  @return power of numbers.
autodiff.__pow = function (A1, A2)
  if not _isautodiff(A2) then
    local w = autodiff._convert(A2)
    return w and A1 ^ w or A2.__pow(A1, A2)
  elseif not _isautodiff(A1) then
    local w = autodiff._convert(A1)
    return w and w ^ A2 or error("Not def")
  end
  -- process
  local x, y = A1._[1], A2._[1]
  local vars = _getVars(A1._der[1], A2._der[1])
  local f = x ^ y
  if #vars == 0 then return f end
  local lins = {y*x^(y-1), 0}
  local quads = {y*(y-1)*x^(y-2), 0}
  local cross = 0
  local istblx = type(x) == "table"
  local norm = istblx and _cross.norm(x)
           or (x > 0) and x
           or 0
  if norm > 0 and (not istblx and type(y) ~= "table" or A2._der[1][A2._] ~= 0)
  then
    local lx = istblx and x:log() or math.log(x)
    lins[2] = x^y * lx
    quads[2] = x^y * lx*lx
    cross = x^y * (y*lx + 1)/x
  end
  local linVars, quadVars, crossVars = _chainRule(
    {A1, A2}, vars, lins, quads, cross)
  return autodiff._init(f, linVars, quadVars, crossVars)
end


--- A1 - A2
--  @param A1 autodiff object or number.
--  @param A2 autodiff object or number.
--  @return difference of numbers.
autodiff.__sub = function (A1, A2)
  if not _isautodiff(A2) then
    local w = autodiff._convert(A2)
    return w and A1 - w or A2.__sub(A1, A2)
  elseif not _isautodiff(A1) then
    local w = autodiff._convert(A1)
    return w and w - A2 or error("Not def")
  end
  -- process
  local x, y = A1._[1], A2._[1]
  local vars = _getVars(A1._der[1], A2._der[1])
  local f = x - y
  if #vars == 0 then return f end
  local linVars, quadVars, crossVars = _chainRule(
    {A1, A2}, vars, {1, -1}, _ZEROS, 0)
  return autodiff._init(f, linVars, quadVars, crossVars)
end


--- String representation.
--  @return object value as string.
autodiff.__tostring = function (self)
  local v, n = self._[1], self._[2]
  return string.format("Ad(%s%s%s)",
    n or "",
    n and "=" or "",
    type(v) == "number" and _ext.utils.utils.numstr(v) or tostring(v))
end


--- -A
--  @return negation of number.
autodiff.__unm = function (self)
  local x = self._[1]
  local vars = _getVars(self._der[1])
  local linVars, quadVars, crossVars = _chainRule(
    {self}, vars, {-1}, _ZEROS, 0)
  return autodiff._init(-x, linVars, quadVars, crossVars)
end


-- Metamethods
_about['_ar'] = {"arithmetic: a+b, a-b, a*b, a/b, a^b, -a", nil, _help.META}
_about['_cmp'] = {"comparison: a==b, a~=b, a < b, a <= b", nil, _help.META}


--- Transform constant value to autodiff object.
--  @param v Number to transform.
--  @return autodiff object.
autodiff._convert = function (v)
  local w = _value(v)
  return w and autodiff._init(w, _EMPTY, _EMPTY, _EMPTY)
end


--- Make autodiff object.
--  @param v Numeric value.
--  @param dv Table with 1st derivatives.
--  @param ddv Table with 2nd derivatives.
--  @param cross Table with cross derivatives.
--  @param name (=nil) Variable name.
--  @return new autodiff object.
autodiff._init = function (v, dv, ddv, cross, name)
  local o = {
    _ = {v, name},
    _der = {dv, ddv, cross},
  }
  return setmetatable(o, autodiff)
end


--- Check if the object value is zero.
--  For compatibility with other modules.
--  @return true when equal to zero.
autodiff._isZero = function (self) return _cross.isZero(self._[1]) end


--- Find absolute value of an object.
--  For compatibility with other modules.
--  @return absolute value.
autodiff._norm = function (self) return _cross.norm(self._[1]) end


--- Round value to specific tolerance.
--  @param tol Tolerance value.
--  @return rounded number.
autodiff._round = function (self, tol) return _cross.round(self._[1], tol) end


--- Try to get numeric value.
--  @return float value or nil.
autodiff._simp = function (self) return _cross.simp(self._[1]) end


--- acos(x)
--  @return inverse cosine.
autodiff.acos = function (self)
  local x = self._[1]
  local vars = _getVars(self._der[1])
  local f = _ext.complex(x):acos():_simp()
  local sq = _sqrt(1 - x*x)
  local linVars, quadVars, crossVars = _chainRule(
    {self}, vars, {-1/sq}, {x/(sq*(x*x-1))}, 0)
  return autodiff._init(f, linVars, quadVars, crossVars)
end
_about[autodiff.acos] = {"A:acos() --> y_A", "Inverse cosine.", FUNCTIONS}


--- acosh(x)
--  @return hyperbolic inverse cosine.
autodiff.acosh = function (self)
  local x = self._[1]
  local vars = _getVars(self._der[1])
  local f = _ext.complex(x):acosh():_simp()
  local sq = _sqrt(x*x - 1)
  local linVars, quadVars, crossVars = _chainRule(
    {self}, vars, {1/sq}, {-x/(sq*(x*x-1))}, 0)
  return autodiff._init(f, linVars, quadVars, crossVars)
end
_about[autodiff.acosh] = {"A:acosh() --> y_A",
  "Inverse hyperbolic cosine.", FUNCTIONS}


--- asin(x)
--  @return inverse sine.
autodiff.asin = function (self)
  local x = self._[1]
  local vars = _getVars(self._der[1])
  local f = _ext.complex(x):asin():_simp()
  local sq = _sqrt(1 - x*x)
  local linVars, quadVars, crossVars = _chainRule(
    {self}, vars, {1/sq}, {-x/(sq*(x*x-1))}, 0)
  return autodiff._init(f, linVars, quadVars, crossVars)
end
_about[autodiff.asin] = {"A:asin() --> y_A", "Inverse sine.", FUNCTIONS}


--- asinh(x)
--  @return hyperbolic inverse sine.
autodiff.asinh = function (self)
  local x = self._[1]
  local f = _fun(x, "asinh", _calc.asinh)
  local vars = _getVars(self._der[1])
  local sq = _fun(x*x + 1, "sqrt", math.sqrt)
  local linVars, quadVars, crossVars = _chainRule(
    {self}, vars, {1/sq}, {-x/(sq*(x*x+1))}, 0)
  return autodiff._init(f, linVars, quadVars, crossVars)
end
_about[autodiff.asinh] = {"A:asinh() --> y_A",
  "Inverse hyperbolic sine.", FUNCTIONS}


--- atan(x)
--  @return inverse tangent.
autodiff.atan = function (self)
  local x = self._[1]
  local f = _fun(x, "atan", math.atan)
  local vars = _getVars(self._der[1])
  local xx = x*x + 1
  local linVars, quadVars, crossVars = _chainRule(
    {self}, vars, {1/xx}, {-2*x/(xx*xx)}, 0)
  return autodiff._init(f, linVars, quadVars, crossVars)
end
_about[autodiff.atan] = {"A:atan() --> y_A", "Inverse tangent.", FUNCTIONS}


--- atanh(x)
--  @return hyperbolic inverse tangent.
autodiff.atanh = function (self)
  local x = self._[1]
  local f = _fun(x, "atanh", _calc.atanh)
  local vars = _getVars(self._der[1])
  local xx = x*x - 1
  local linVars, quadVars, crossVars = _chainRule(
    {self}, vars, {-1/xx}, {2*x/(xx*xx)}, 0)
  return autodiff._init(f, linVars, quadVars, crossVars)
end
_about[autodiff.atanh] = {"A:atanh() --> y_A",
  "Inverse hyperbolic tangent.", FUNCTIONS}


--- Deep copy of an autodiff object.
--  @return copy of the object.
autodiff.copy = function (self)
  local d1, d2, d3 = {}, {}, {}
  for k, v in pairs(self._der[1]) do d1[k] = v end
  for k, v in pairs(self._der[2]) do d2[k] = v end
  for k, v in pairs(self._der[3]) do
    local t = {}
    for kk, vv in pairs(v) do t[kk] = vv end
    d3[k] = t
  end
  return autodiff._init(self._[1], d1, d2, d3, self._[2])
end


--- cos(x)
--  @return cosine.
autodiff.cos = function (self)
  local x = self._[1]
  local f = _fun(x, "cos", math.cos)
  local vars = _getVars(self._der[1])
  local s = _fun(x, "sin", math.sin)
  local linVars, quadVars, crossVars = _chainRule(
    {self}, vars, {-s}, {-f}, 0)
  return autodiff._init(f, linVars, quadVars, crossVars)
end
_about[autodiff.cos] = {"A:cos() --> y_A", "Cosine value.", FUNCTIONS}


--- cosh(x)
--  @return hyperbolic cosine.
autodiff.cosh = function (self)
  local x = self._[1]
  local f = _fun(x, "cosh", math.cosh)
  local vars = _getVars(self._der[1])
  local s = _fun(x, "sinh", math.sinh)
  local linVars, quadVars, crossVars = _chainRule(
    {self}, vars, {s}, {f}, 0)
  return autodiff._init(f, linVars, quadVars, crossVars)
end
_about[autodiff.cosh] = {"A:cosh() --> y_A",
  "Hypebolic cosine value.", FUNCTIONS}


--- Get 1st derivative.
--  @param x Autodiff object or variable name.
--  @return 1st derivative value.
autodiff.d = function (self, x)
  if not x then
    error "Variable expected"
  end
  return _isautodiff(x) and self._der[1][x._]
      or type(x) == "string" and self._der[1][_byName(self, x)]
      or 0
end
_about[autodiff.d] = {"A:d(var) --> df/dx_A",
  "First derivative wrt the given variable (object or name)."}


--- Get 2nd derivative.
--  @param x Autodiff object or variable name.
--  @param y (=x) Autodiff object or variable name.
--  @return 2nd derivative or cross derivative.
autodiff.d2 = function (self, x, y)
  if not x then
    error "Variable expected"
  end
  y = y or x
  -- second derivative
  local kx = _isautodiff(x) and x._ or type(x) == "string" and _byName(self, x)
  if x == y then
    return self._der[2][kx] or 0
  end
  -- cross derivative
  local ky = _isautodiff(y) and y._ or type(y) == "string" and _byName(self, y)
  local dc = self._der[3]
  return dc[kx] and dc[kx][ky]
      or dc[ky] and dc[ky][kx]
      or 0.0
end
_about[autodiff.d2] = {"A:d2(var, [var2=var]) --> d2f/dxdy_A",
  "Second derivative wrt the given variables (object or name)."}


--- exp(x)
--  @return exponent value.
autodiff.exp = function (self)
  local x = self._[1]
  local f = _fun(x, "exp", math.exp)
  local vars = _getVars(self._der[1])
  local linVars, quadVars, crossVars = _chainRule(
    {self}, vars, {f}, {f}, 0)
  return autodiff._init(f, linVars, quadVars, crossVars)
end
_about[autodiff.exp] = {"A:exp() --> y_A", "Exponent value.", FUNCTIONS}


--- Get variable as float value when possible.
autodiff.float = function (self) return _cross.float(self._[1]) end


--- Find gradient vector.
--  @param vars List of variables.
--  @return vector of partial derivatives.
autodiff.grad = function (self, vars)
  _ext.matrix = _ext.matrix or require("matlib.matrix")
  local t = {}
  for i, v in ipairs(vars) do t[i] = autodiff.d(self, v) end
  return _ext.matrix:V(t)
end
_about[autodiff.grad] = {"A:grad(vars_t) --> grad_V",
  "Find Jacobian vector of expression wrt to the given list of variables."}


--- Find hessian matrix.
--  @param vars List of variables.
--  @return matrix of cross derivatives.
autodiff.hess = function (self, vars)
  _ext.matrix = _ext.matrix or require("matlib.matrix")
  local m = _ext.matrix:zeros(#vars)
  for i = 1, #vars do
    local vi, t = vars[i], m[i]
    for j = 1, i-1 do t[j] = m[j][i] end
    for j = i, #vars do
      t[j] = autodiff.d2(self, vi, vars[j])
    end
  end
  return m
end
_about[autodiff.hess] = {"A:hess(vars_t) --> hess_M",
  "Find Hessian matrix of expression wrt to the given list of variables."}


--- log(x)
--  @param base (=e) logarithm base.
--  @return logarithm value.
autodiff.log = function (self, base)
  local x = self._[1]
  local f = _fun(x, "log", math.log)
  local vars = _getVars(self._der[1])
  local lbase = base and math.log(base) or 1
  local linVars, quadVars, crossVars = _chainRule(
    {self}, vars, {1/(x*lbase)}, {-1/(x*x*lbase)}, 0)
  return autodiff._init(f, linVars, quadVars, crossVars)
end
_about[autodiff.log] = {"A:log([base=e]) --> y_A", "Logarithm value.", FUNCTIONS}


--- sin(x)
--  @return sine value.
autodiff.sin = function (self)
  local x = self._[1]
  local f = _fun(x, "sin", math.sin)
  local vars = _getVars(self._der[1])
  local c = _fun(x, "cos", math.cos)
  local linVars, quadVars, crossVars = _chainRule(
    {self}, vars, {c}, {-f}, 0)
  return autodiff._init(f, linVars, quadVars, crossVars)
end
_about[autodiff.sin] = {"A:sin() --> y_A", "Sine value.", FUNCTIONS}


--- sinh(x)
--  @return hyperbolic sine value.
autodiff.sinh = function (self)
  local x = self._[1]
  local f = _fun(x, "sinh", math.sinh)
  local vars = _getVars(self._der[1])
  local c = _fun(x, "cosh", math.cosh)
  local linVars, quadVars, crossVars = _chainRule(
    {self}, vars, {c}, {f}, 0)
  return autodiff._init(f, linVars, quadVars, crossVars)
end
_about[autodiff.sinh] = {"A:sinh() --> y_A",
  "Hyperbolic sine value.", FUNCTIONS}


--- sqrt(x)
--  @return square root value.
autodiff.sqrt = function (self)
  local x = self._[1]
  local f = _sqrt(x)
  local vars = _getVars(self._der[1])
  local linVars, quadVars, crossVars = _chainRule(
    {self}, vars, {1/(2*f)}, {-1/(4*x*f)}, 0)
  return autodiff._init(f, linVars, quadVars, crossVars)
end
_about[autodiff.sqrt] = {"A:sqrt() --> y_A", "Square root value.", FUNCTIONS}


--- tan(x)
--  @return tangent value.
autodiff.tan = function (self)
  local x = self._[1]
  local s = _fun(x, "sin", math.sin)
  local c = _fun(x, "cos", math.cos)
  local vars = _getVars(self._der[1])
  local cc = c*c
  local linVars, quadVars, crossVars = _chainRule(
    {self}, vars, {1/(cc)}, {2*s/(cc*c)}, 0)
  return autodiff._init(s/c, linVars, quadVars, crossVars)
end
_about[autodiff.tan] = {"A:tan() --> y_A", "Tangent value.", FUNCTIONS}


--- tanh(x)
--  @return hyperbolic tangent.
autodiff.tanh = function (self)
  local x = self._[1]
  local s = _fun(x, "sinh", math.sinh)
  local c = _fun(x, "cosh", math.cosh)
  local vars = _getVars(self._der[1])
  local cc = c*c
  local linVars, quadVars, crossVars = _chainRule(
    {self}, vars, {1/(cc)}, {-2*s/(cc*c)}, 0)
  return autodiff._init(s/c, linVars, quadVars, crossVars)
end
_about[autodiff.tanh] = {"A:tanh() --> y_A",
  "Hyperbolic tangent value.", FUNCTIONS}


-- simplify constructor call
setmetatable(autodiff, {
__call = function (_, v, name)
  if _isautodiff(v) then
    if name then v._[2] = name end
    return v
  end
  local w = assert(_value(v), "Wrong data type")
  local t = autodiff._init(w, {}, {}, _EMPTY, name)
  local key = t._
  t._der[1][key] = 1.0
  t._der[2][key] = 0.0
  return t
end })
_about[autodiff] = {" (value, [name]) --> new_A",
  "Create a new autodiff object.", _help.STATIC}


-- Comment to remove descriptions
autodiff.about = _about

return autodiff

--======================================
