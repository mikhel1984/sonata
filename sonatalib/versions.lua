--[[      sonatalib/versions.lua 

--- Auxilary functions for compatiblity with Lua 5.1 and 5.2.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonatalib</a> collection, 2017-2019.

            module 'versions'
--]]

--	MODULE

local versions = {}

if _VERSION < 'Lua 5.3' 
then --==================== Previous versions =======================

-- Check if the number is integer.
versions.isInteger = function (x) 
      if type(x) == 'string' then x = tonumber(x) end
      if not x then return false end
      local v,p = math.modf(x) 
      return p == 0.0 and v <= 1E9
   end
   
-- Return integer number or nil.
versions.toInteger = function (x) 
      if type(x) == 'string' then x = tonumber(x) end
      local p,q = math.modf(x) 
      return (q == 0.0) and p or nil
   end

-- Move elements to new position (and table).
versions.move = function (src,sfrom,sto,dfrom,dest)
      if dest then
         local k = 0
         for i = sfrom,sto do dest[dfrom+k] = src[i]; k=k+1 end
      else
         local tmp = versions.move(src,sfrom,sto,sfrom,{})
	 local dest = versions.move(tmp,sfrom,sto,dfrom,src)
	 if dfrom > sfrom then 
	    for i = sfrom,dfrom-1 do dest[i] = nil end
	 else
	    for i = dfrom+sto-sfrom+1,sto do dest[i] = nil end
	 end
      end
      return dest
   end
-- Execute string code.
versions.loadStr = loadstring
-- Check type of the number.
versions.mathType = function (x)
      local n = tonumber(x)
      if not n then return nil end
      local _,p = math.modf(n)
      return (p == 0.0) and 'integer' or 'float'
   end
-- Extract table values.
versions.unpack = unpack
-- Arctangent with sign
versions.atan2 = math.atan2

else --================ New version ================

-- Check if the number is integer.
versions.isInteger = math.tointeger
-- Return integer number or nil.
versions.toInteger = math.tointeger
-- Move elements to new position (and table).
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

return versions
