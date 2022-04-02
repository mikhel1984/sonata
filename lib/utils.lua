--[[		sonata/lib/versions.lua

--- Auxilary functions for compatiblity with Lua 5.1 and 5.2.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2021.

	module 'versions'
--]]

--	MODULE

local versions = {}

if _VERSION < 'Lua 5.3' 
then --==================== Previous versions =======================

-- Check if the number is integer
versions.isInteger = function (x)
  if type(x) == 'string' then x = tonumber(x) end
  if not x then return false end
  local v,p = math.modf(x)
  return p == 0.0 and v >= -1E9 and v <= 1E9
end
-- Return integer number or nil
versions.toInteger = function (x)
  if type(x) == 'string' then x = tonumber(x) end
  local p,q = math.modf(x)
  return (q == 0.0) and p or nil
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
-- Execute string code
versions.loadStr = loadstring
-- Check type of the number
versions.mathType = function (x)
  local n = tonumber(x)
  if not n then return nil end
  local _,p = math.modf(n)
  return (p == 0.0) and 'integer' or 'float'
end
-- Extract table values
versions.unpack = unpack
-- Arctangent with sign
versions.atan2 = math.atan2

else --================ New version ================

-- Check if the number is integer
versions.isInteger = math.tointeger
-- Return integer number or nil
versions.toInteger = math.tointeger
-- Move elements to new position (and table)
versions.move = table.move
-- Execute string code.
versions.loadStr = load
-- Check type of the number.
versions.mathType = math.type
-- Extract table values.
versions.unpack = table.unpack
-- Arctangent with sign
versions.atan2 = math.atan

end




--============= Cross-module functionality =========

local cross = {
--NUMBER = 1, TABLE = 2, STRING = 3, OTHER = 4,
}

-- norm of numeric objects
cross.norm = function (v)
  if type(v) == 'number' then
    return math.abs(v)
  elseif type(v) == 'table' and v._norm_ then
    return v:_norm_()
  end
  return nil
end

-- compare equality of two objects
cross.eq = function (v1,v2)
  if     type(v1) == 'table' and v1.__eq then
    return v1:__eq(v2) 
  elseif type(v2) == 'table' and v2.__eq then
    return v2:__eq(v1) 
  else
    return v1 == v2
  end
end

-- apply simplification if possible 
cross.simp = function (v)
  return type(v) == 'table' and v._simp_ and v:_simp_() or v
end

-- get the object copy
cross.copy = function (v)
  return type(v) == 'table' and v.copy and v:copy() or v
end

-- get float value when possible
cross.float = function (v)
  return type(v) == 'number' and v or type(v) == 'table' and v:float() or nil
end

-- check if the types are the same
cross.same = function (v1,v2)
  return type(v1) == type(v2) and (type(v1) ~= 'table' or v1.type == v2.type)
end

-- convert slave into type of master
cross.convert = function (vMaster, vSlave)
  return type(vMaster) == 'table' and vMaster._convert_ and vMaster._convert_(vSlave)
end

return {
  versions = versions,
  cross = cross,
}

--===================================================
