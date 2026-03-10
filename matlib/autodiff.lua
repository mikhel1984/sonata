--[[		sonata/matlib/autodiff.lua

--- Automatic differentiation.
--
--  </br></br><b>Authors</b>: Your Name

	module 'autodiff'
--]]

-- Define here your tests, save results to 'ans',
-- use --> for the strict equality
-- and --.n> for the n-digit precision in the case of floating numbers.
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
ans = v(1)                  --.2> y()

-- hessian
v = s:hess {x, y}
ans = v(1,2)                --.2> 1

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

local _ext = {
  utils = require("matlib.utils"),
  -- matrix = require("matlib.matrix"),
}

local _cross = _ext.utils.cross
local _calc = _ext.utils.calc

local _empty, _zeros = {}, {0, 0}


local function _by_name (var, nm)
  for k in pairs(var._der[1]) do
    if k[2] == nm then return k end
  end
end


local function _chain_rule (fns, vars, lins, quads, cross)
  -- initialization
  local lin_vars, quad_vars, cross_vars = {}, {}, {}
  for i, v in ipairs(vars) do
    lin_vars[v] = 0
    quad_vars[v] = 0
    local t = {}
    for j = i+1, #vars do
      t[ vars[j] ] = 0
    end
    cross_vars[v] = t
  end

  -- chain rule
  for i = 1, #vars do
    local v1 = vars[i]
    for j = i, #vars do
      local v2 = vars[j]
      for k = 1, #lins do
        local der, dh, d2h = fns[k]._der, lins[k], quads[k]
        local fv1 = der[1][v1] or 0  -- 1sd derivative
        if i == j then
          -- first order terms
          lin_vars[v1] = lin_vars[v1] + dh*fv1
          -- pure second order terms
          quad_vars[v1] = quad_vars[v1] + dh*(der[2][v1] or 0) + d2h*fv1*fv1
        else
          local d3 = der[3]
          local fdc = d3[v1] and d3[v1][v2] or d3[v2] and d3[v2][v1] or 0
          -- cross product of second order terms
          local tmp = dh*fdc + d2h*fv1*(der[1][v2] or 0)
          local t = cross_vars[v1]
          t[v2] = t[v2] + tmp
        end
      end
      -- update quadratic and cross product terms
      if #fns > 1 then
        local d1, d2 = fns[1]._der[1], fns[2]._der[1]
        local tmp = (d1[v2] or 0) * (d2[v1] or 0)
        if i == j then
          quad_vars[v1] = quad_vars[v1] + 2*cross*tmp
        else
          tmp = tmp + (d1[v1] or 0) * (d2[v2] or 0)
          local t = cross_vars[v1]
          t[v2] = t[v2] + cross*tmp
        end
      end
    end
  end
  return lin_vars, quad_vars, cross_vars
end

local function _get_vars (a, b)
  local res = {}
  for k in pairs(a) do res[#res+1] = k end
  if b then
    for k in pairs(b) do
      if not a[k] then res[#res+1] = k end
    end
  end
  return res
end


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

autodiff.__add = function (v1, v2)
  if not _isautodiff(v2) then
    local w = autodiff._convert(v2)
    return w and v1 + w or v2.__add(v1, v2)
  elseif not _isautodiff(v1) then
    local w = autodiff._convert(v1)
    return w and w + v2 or error("Not def")
  end
  --
  local x, y = v1._[1], v2._[1]
  local vars = _get_vars(v1._der[1], v2._der[1])
  local f = x+y
  if #vars == 0 then return f end
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {v1, v2}, vars, {1, 1}, _zeros, 0)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end

autodiff.__call = function (self)
  return self._[1]
end

autodiff.__div = function (v1, v2)
  if not _isautodiff(v2) then
    local w = autodiff._convert(v2)
    return w and v1 / w or v2.__div(v1, v2)
  elseif not _isautodiff(v1) then
    local w = autodiff._convert(v1)
    return w and w / v2 or error("Not def")
  end
  --
  local x, y = v1._[1], v2._[1]
  local vars = _get_vars(v1._der[1], v2._der[1])
  local f, yy = x/y, 1.0/(y*y)
  if #vars == 0 then return f end
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {v1, v2}, vars, {1/y, -f/y}, {0, 2*f*yy}, -yy)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end

autodiff.__mul = function (v1, v2)
  if not _isautodiff(v2) then
    local w = autodiff._convert(v2)
    return w and v1 * w or v2.__mul(v1, v2)
  elseif not _isautodiff(v1) then
    local w = autodiff._convert(v1)
    return w and w * v2 or error("Not def")
  end
  --
  local x, y = v1._[1], v2._[1]
  local vars = _get_vars(v1._der[1], v2._der[1])
  local f = x*y
  if #vars == 0 then return f end
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {v1, v2}, vars, {y, x}, _zeros, 1)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end

autodiff.__pow = function (v1, v2)
  if not _isautodiff(v2) then
    local w = autodiff._convert(v2)
    return w and v1 ^ w or v2.__pow(v1, v2)
  elseif not _isautodiff(v1) then
    local w = autodiff._convert(v1)
    return w and w ^ v2 or error("Not def")
  end
  --
  local x, y = v1._[1], v2._[1]
  local vars = _get_vars(v1._der[1], v2._der[1])
  local f = x^y
  if #vars == 0 then return f end
  local lins = {y*x^(y-1), 0}
  local quads = {y*(y-1)*x^(y-2), 0}
  local cross = 0
  local istblx = type(x) == "table"
  local norm = istblx and _cross.norm(x)
           or (x > 0) and x
           or 0
  if norm > 0 and (not istblx and type(y) ~= "table" or v2._der[1][v2._] ~= 0)
  then
    local lx = istblx and x:log() or math.log(x)
    lins[2] = x^y * lx
    quads[2] = x^y * lx*lx
    cross = x^y * (y*lx + 1)/x
  end
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {v1, v2}, vars, lins, quads, cross)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end


autodiff.__sub = function (v1, v2)
  if not _isautodiff(v2) then
    local w = autodiff._convert(v2)
    return w and v1 - w or v2.__sub(v1, v2)
  elseif not _isautodiff(v1) then
    local w = autodiff._convert(v1)
    return w and w - v2 or error("Not def")
  end
  --
  local x, y = v1._[1], v2._[1]
  local vars = _get_vars(v1._der[1], v2._der[1])
  local f = x-y
  if #vars == 0 then return f end
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {v1, v2}, vars, {1, -1}, _zeros, 0)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end


autodiff.__unm = function (v)
  local x = v._[1]
  local vars = _get_vars(v._der[1])
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {v}, vars, {-1}, _zeros, 0)

  return autodiff._init(-x, lin_vars, quad_vars, cross_vars)
end



autodiff.__tostring = function (self)
  local v, n = self._[1], self._[2]
  return string.format("Ad(%s%s%s)",
    n or "",
    n and "=" or "",
    type(v) == "number" and _ext.utils.utils.numstr(v) or tostring(v))
end

autodiff._convert = function (v)
  local w = _value(v)
  return w and autodiff._init(w, _empty, _empty, _empty)
end

--- Method example.
--  It is good idea to define method for the copy creation.
--  @return Copy of the object.
autodiff._copy = function (self)
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


autodiff._init = function (v, dv, ddv, cross, name)
  local o = {
    _ = {v, name},
    _der = {dv, ddv, cross},
  }
  return setmetatable(o, autodiff)
end


autodiff._isZero = function (self)
  return _cross.isZero(self._[1])
end

autodiff._norm = function (self)
  v = self._[1]
  return type(v) == "number" and math.abs(v) or
    v._norm and v:_norm() or nil
end

local function _fun (x, nm, f)
  return type(x) == "number" and f(x)
      or x[nm] and x[nm](x)
      or f(x:float())
end

autodiff.acos = function (self)
  local x = self._[1]
  local f = _fun(x, "acos", math.acos)
  local vars = _get_vars(self._der[1])
  local sq = _fun(1-x*x, "sqrt", math.sqrt)
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {self}, vars, {-1/sq}, {x/(sq*(x*x-1))}, 0)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end

autodiff.acosh = function (self)
  local x = self._[1]
  local f = _fun(x, "acosh", _calc.acosh)
  local vars = _get_vars(self._der[1])
  local sq = _fun(x*x-1, "sqrt", math.sqrt)
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {self}, vars, {1/sq}, {-x/(sq*(x*x-1))}, 0)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end

autodiff.asin = function (self)
  local x = self._[1]
  local f = _fun(x, "asin", math.asin)
  local vars = _get_vars(self._der[1])
  local sq = _fun(1-x*x, "sqrt", math.sqrt)
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {self}, vars, {1/sq}, {-x/(sq*(x*x-1))}, 0)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end

autodiff.asinh = function (self)
  local x = self._[1]
  local f = _fun(x, "asinh", _calc.asinh)
  local vars = _get_vars(self._der[1])
  local sq = _fun(x*x+1, "sqrt", math.sqrt)
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {self}, vars, {1/sq}, {-x/(sq*(x*x+1))}, 0)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end

autodiff.atan = function (self)
  local x = self._[1]
  local f = _fun(x, "atan", math.atan)
  local vars = _get_vars(self._der[1])
  local xx = x*x + 1
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {self}, vars, {1/xx}, {-2*x/(xx*xx)}, 0)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end

autodiff.atanh = function (self)
  local x = self._[1]
  local f = _fun(x, "atanh", _calc.atanh)
  local vars = _get_vars(self._der[1])
  local xx = x*x - 1
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {self}, vars, {-1/xx}, {2*x/(xx*xx)}, 0)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end

autodiff.cos = function (self)
  local x = self._[1]
  local f = _fun(x, "cos", math.cos)
  local vars = _get_vars(self._der[1])
  local s = _fun(x, "sin", math.sin)
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {self}, vars, {-s}, {-f}, 0)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end

autodiff.cosh = function (self)
  local x = self._[1]
  local f = _fun(x, "cosh", _calc.cosh)
  local vars = _get_vars(self._der[1])
  local s = _fun(x, "sinh", _calc.sinh)
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {self}, vars, {s}, {f}, 0)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end

autodiff.exp = function (self)
  local x = self._[1]
  local f = _fun(x, "exp", math.exp)
  local vars = _get_vars(self._der[1])
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {self}, vars, {f}, {f}, 0)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end

autodiff.log = function (self, base)
  local x = self._[1]
  local f = _fun(x, "log", math.log)
  local vars = _get_vars(self._der[1])
  local lbase = base and math.log(base) or 1
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {self}, vars, {1/(x*lbase)}, {-1/(x*x*lbase)}, 0)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end

autodiff.sin = function (self)
  local x = self._[1]
  local f = _fun(x, "sin", math.sin)
  local vars = _get_vars(self._der[1])
  local c = _fun(x, "cos", math.cos)
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {self}, vars, {c}, {-f}, 0)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end

autodiff.sinh = function (self)
  local x = self._[1]
  local f = _fun(x, "sinh", _calc.sinh)
  local vars = _get_vars(self._der[1])
  local c = _fun(x, "cosh", _calc.cosh)
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {self}, vars, {c}, {f}, 0)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end

autodiff.sqrt = function (self)
  local x = self._[1]
  local f = _fun(x, "sqrt", math.sqrt)
  local vars = _get_vars(self._der[1])
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {self}, vars, {1/(2*f)}, {-1/(4*x*f)}, 0)

  return autodiff._init(f, lin_vars, quad_vars, cross_vars)
end

autodiff.tan = function (self)
  local x = self._[1]
  local s = _fun(x, "sin", math.sin)
  local c = _fun(x, "cos", math.cos)
  local vars = _get_vars(self._der[1])
  local cc = c*c
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {self}, vars, {1/(cc)}, {2*s/(cc*c)}, 0)

  return autodiff._init(s/c, lin_vars, quad_vars, cross_vars)
end

autodiff.tanh = function (self)
  local x = self._[1]
  local s = _fun(x, "sinh", _calc.sinh)
  local c = _fun(x, "cosh", _calc.cosh)
  local vars = _get_vars(self._der[1])
  local cc = c*c
  local lin_vars, quad_vars, cross_vars = _chain_rule(
    {self}, vars, {1/(cc)}, {-2*s/(cc*c)}, 0)

  return autodiff._init(s/c, lin_vars, quad_vars, cross_vars)
end


--autodiff.float = function (self)
--  local v = self._[1]
--  return type(v) == "number" and v or
--    v.float and v:float() or nil
--end

autodiff.d = function (self, var)
  if not var then
    error "Variable expected"
  end
  return _isautodiff(var) and self._der[1][var._] 
      or type(var) == "string" and self._der[1][_by_name(self, var)]
      or 0
end

autodiff.d2 = function (self, x, y)
  if not x then
    error "Variable expected"
  end
  y = y or x
  -- second derivative
  local kx = _isautodiff(x) and x._ or type(x) == "string" and _by_name(self, x)
  if x == y then
    return self._der[2][kx] or 0
  end
  -- cross derivative
  local ky = _isautodiff(y) and y._ or type(y) == "string" and _by_name(self, y)
  local dc = self._der[3]
  return dc[kx] and dc[kx][ky]
      or dc[ky] and dc[ky][kx]
      or 0.0
end

autodiff.grad = function (self, vars)
  _ext.matrix = _ext.matrix or require("matlib.matrix")
  local t = {}
  for i, v in ipairs(vars) do t[i] = autodiff.d(self, v) end
  return _ext.matrix:V(t)
end

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


autodiff.val = function (self) return self._[1] end


-- simplify constructor call
setmetatable(autodiff, {
__call = function (_, v, name)
  if _isautodiff(v) then
    if name then v._[2] = name end
    return v
  end
  local w = assert(_value(v), "Wrong data type")
  local t = autodiff._init(w, {}, {}, _empty, name)
  local key = t._
  t._der[1][key] = 1.0
  t._der[2][key] = 0.0
  return t
end })
_about[autodiff] = {" (t) --> A", "Create new autodiff object.", _help.NEW}
-- begin from ' ' to get 'Ad ()'


-- Comment to remove descriptions
autodiff.about = _about

return autodiff

--======================================

