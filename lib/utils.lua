--[[		sonata/lib/versions.lua

--- Auxilary functions.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2022.

	module 'utils'
--]]

--	MODULE

--================ New version ================
local versions = {
  -- Arctangent with sign
  atan2     = math.atan,
  -- Check if the number is integer
  isInteger = math.tointeger,
  -- Execute string code.
  loadStr   = load,
  -- Check type of the number.
  mathType  = math.type,
  -- Move elements to new position (and table)
  move      = table.move,
  -- Return integer number or nil
  toInteger = math.tointeger,
  -- Extract table values.
  unpack    = table.unpack,
}

--=============== Previous versions =======================
if _VERSION < 'Lua 5.3' then 
  -- Arctangent with sign
  versions.atan2 = math.atan2
  -- Check if the number is integer
  versions.isInteger = function (x)
    if type(x) == 'string' then x = tonumber(x) end
    if not x then return false end
    local v,p = math.modf(x)
    return p == 0.0 and v >= -1E9 and v <= 1E9
  end
  -- Execute string code
  versions.loadStr = loadstring
  -- Check type of the number
  versions.mathType = function (x)
    local n = tonumber(x)
    if not n then return nil end
    local _,p = math.modf(n)
    return (p == 0.0) and 'integer' or 'float'
  end
  -- Move elements to new position (and table)
  versions.move = function (src,sfrom,sto,dfrom,dest)
    if dest and dest ~= src then
      for i = sfrom, sto do
        dest[dfrom] = src[i]
        dfrom = dfrom + 1
      end
    else
      local temp = versions.move(src,sfrom,sto,sfrom,{})
      dest = versions.move(temp,sfrom,sto,dfrom,src)
    end
    return dest
  end
  -- Return integer number or nil
  versions.toInteger = function (x)
    if type(x) == 'string' then x = tonumber(x) end
    local p,q = math.modf(x)
    return (q == 0.0) and p or nil
  end
  -- Extract table values
  versions.unpack = unpack
end

--============= Cross-module functionality =========

local cross = {
--NUMBER = 1, TABLE = 2, STRING = 3, OTHER = 4,
}

--- Compare equality of two objects.
--  @param v1 First object.
--  @param v2 Second object.
--  @return True if the objects are equal.
cross.eq = function (v1,v2)
  if     type(v1) == 'table' and v1.__eq then
    return v1:__eq(v2) 
  elseif type(v2) == 'table' and v2.__eq then
    return v2:__eq(v1) 
  else
    return v1 == v2
  end
end

--- Convert slave into type of master.
--  @param vMaster Master object.
--  @param vSlave Slave object.
--  @return Converted slave of nil.
cross.convert = function (vMaster, vSlave)
  return type(vMaster) == 'table' and vMaster._convert_ and vMaster._convert_(vSlave)
end

--- Get the object copy.
--  @param v Object.
--  @return Deep copy when possible.
cross.copy = function (v)
  return type(v) == 'table' and v.copy and v:copy() or v
end

--- Get float value when possible.
--  @param v Some object.
--  @return Float number or nil.
cross.float = function (v)
  return type(v) == 'number' and v or type(v) == 'table' and v:float() or nil
end

--- Norm of numeric object.
--  @param v Some object.
--  @return Norm or nil.
cross.norm = function (v)
  if type(v) == 'number' then
    return math.abs(v)
  elseif type(v) == 'table' then
    return v.float and math.abs(v:float()) or v._norm_ and v:_norm_() or nil
  end
  return nil
end

--- Apply simplification if possible 
--  @param v Sonata object.
--  @return Number or the object itself.
cross.simp = function (v)
  return type(v) == 'table' and v._simp_ and v:_simp_() or v
end

--============== Utils ================

local utils = {}

--- Generate function from string.
--  @param sExpr Expression for execution.
--  @param iArg Number of arguments.
--  @return Function based on the expression.
utils.Fn = function (sExpr,iArg)
  local arg = {}
  for i = 1, iArg do arg[i] = string.format("x%d", i) end
  local fn = versions.loadStr(
    string.format("return function (%s) return %s end", table.concat(arg,','), sExpr)
  )
  return fn()
end

--- 'Smart' number to string conversation.
--  @param d Number. 
--  @return String representation.
utils.numstr = function (d)
  local int, frac = math.modf(d)
  local a = math.abs(int)
  -- short integer
  if frac == 0 then 
    if a < 1E5 then
      return string.format('%d', int) 
    end
  elseif int == 0 then
    if math.abs(frac) >= 0.01 then
      return string.format("%.3f", d)
    end
  elseif a < 10 then
    return string.format('%.3f', d)
  elseif a < 100 then 
    return string.format('%.2f', d)
  elseif a < 1000 then 
    return string.format('%.1f', d)
  end
  return string.format('%.2E', d)
end

return {
  versions = versions,
  cross = cross,
  utils = utils,
}

--===================================================
--TODO setup for number of digits
