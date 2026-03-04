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

local _foo = 42

--	INFO

local _help = SonataHelp or {}  -- optional
-- description
local _about = {
__module__ = "Automatic differentiation."
}


local function _chain_rule (fns, vars, lins, quads, cross)
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
  
  for i = 1, #vars do
    local v1 = vars[i]
    for j = i, #vars do
      local v2 = vars[j]
      for k = 1, #lins do
        local f, dh, d2h = fns[k], lins[k], quads[k]
        if i == j then 
          local fv1 = f:d(v1)
          lin_vars[v1] = lin_vars[v1] + dh*fv1
          quad_vars[v1] = quad_vars[v1] + dh*f:d2(v1) + d2h*fv1*fv1
        else
          local tmp = dh*f:d2c(v1, v2) + d2h*f:d(v1)*f:d(v2)
          local t = cross_vars[v1]
          t[v2] = t[v2] + tmp
        end
      end
      
      if #fns > 1 then
        if i == j then
          local tmp = 2*cross * fns[1]:d(v1) * fns[2]:d(v1)
          quad_vars[v1] = quad_vars[v1] + tmp          
        else 
          local tmp = cross*( fns[1]:d(v1) * fns[2]:d(v2) + fns[1]:d(v2) * fns[2]:d(v1) )
          local t = cross_vars[v1]
          t[v2] = t[v2] + tmp
        end
      end
    end
  end
  return lin_vars, quad_vars, cross_vars
end


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


autodiff._init = function (v, dv, ddv, cross, name)
  local o = {[0]=v, dv, ddv, name=name}
  return setmetatable({_=o}, autodiff)
end


autodiff.d = function (self, var)
  return var and _isautodiff(var) and self._[1][var.name] or 0
end

autodiff.d2 = function (self, var)
  return var and _isautodiff(var) and self._[2][var.name] or 0
end


--- Constructor example.
--  @param t Some value.
--  @return New object of autodiff.
autodiff.new = function(self, t)
  local o = {}
  -- your logic
  -- return object
  return setmetatable(o, self)
end
_about[autodiff.new] = {":new(t) --> A", "Explicit constructor.", _help.NEW}
-- begin from ':' to get 'Ad:new(t)'


-- simplify constructor call
setmetatable(autodiff, {__call = function (self, v) return autodiff:new(v) end})
_about[autodiff] = {" (t) --> A", "Create new autodiff.", _help.NEW}
-- begin from ' ' to get 'Ad ()'


--- Method example.
--  It is good idea to define method for the copy creation.
--  @return Copy of the object.
autodiff.copy = function (self)
  -- some logic
  return autodiff:new(argument)
end
_about[autodiff.copy] = {"A:copy() --> cpy_A",
  "Create a copy of the object."} -- third element is optional, default is 'base'
-- begin from A implicitly

-- Comment to remove descriptions
autodiff.about = _about

return autodiff

--======================================
--TODO: write new functions
