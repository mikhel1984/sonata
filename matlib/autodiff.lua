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

-- example
a = Ad()
-- check equality
ans = a.type                  -->  'autodiff'

-- check relative equality ( ~10^(-2) )
ans = math.pi               --.2> 355/113

--]]


--	LOCAL

local _ext = {
  utils = require("matlib.utils"),
  -- matrix = require("matlib.matrix"),
}

local _cross = _ext.utils.cross

local _empty, _zeros = {}, {0, 0}


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



autodiff.v = function (self) return self._[1] end


autodiff.float = function (self)
  local v = self._[1]
  return type(v) == "number" and v or
    v.float and v:float() or nil
end

autodiff.d = function (self, var)
  if var then
    return _isautodiff(var) and self._der[1][var._] or 0
  end
  return self._der[1]
end

autodiff.d2 = function (self, var)
  if var then
    return _isautodiff(var) and self._der[2][var._] or 0
  end
  return self._der[2]
end

autodiff.d2c = function (self, x, y)
  local dc = self._der[3]
  if x and y then
    if x == y then return self:d2(x) end
    if _isautodiff(x) and _isautodiff(y) then
      local kx, ky = x._, y._
      return dc[kx] and dc[kx][ky] 
          or dc[ky] and dc[ky][kx]
          or 0.0
    end
  elseif not x and not y then
    return dc
  end
  return 0.0
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


-- simplify constructor call
setmetatable(autodiff, {
__call = function (_, v, name) 
  if _isautodiff(v) then
    if name then v._[2] = name end
    return v
  end
  local w = assert(_value(v), "Wrong data type")
  local t = autodiff._init(w, {}, {}, {}, name)
  local key = t._
  t._der[1][key] = 1.0
  t._der[2][key] = 0.0
  return t
end })
_about[autodiff] = {" (t) --> A", "Create new autodiff.", _help.NEW}
-- begin from ' ' to get 'Ad ()'


-- Comment to remove descriptions
autodiff.about = _about

--return autodiff

-- TODO const tables to local variables
--======================================
--TODO: write new functions

local v1 = autodiff(3, "x")
local v2 = autodiff(2, "y")

print(v2)

local s = v1 * 2
print(s)
print(s:d(v1))
