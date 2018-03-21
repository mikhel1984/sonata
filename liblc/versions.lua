--[[      liblc/versions.lua 

--- Auxilary functions for compatiblity with Lua 5.1 and 5.2.
--  @author <a href="mailto:vpsys@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

            module 'versions'
--]]

local versions = {}

if _VERSION < 'Lua 5.3' then

versions.isinteger = function (x) 
      if type(x) == 'string' then x = tonumber(x) end
      if not x then return false end
      local _,p = math.modf(x) 
      return p == 0.0 
   end
versions.tointeger = function (x) 
      if type(x) == 'string' then x = tonumber(x) end
      local p,q = math.modf(x) 
      return (q == 0.0) and p or nil
   end
versions.move = function (src,sfrom,sto,dfrom,dest)
      if dest then
         local k = 0
         for i = sfrom,sto do dest[dfrom+k] = src[i]; k=k+1 end
      else
         local tmp = versions.move(src,sfrom,sto,dfrom,{})
	 local dest = versions.move(tmp,sfrom,sto,dfrom,src)
	 if dfrom > sfrom then 
	    for i = sfrom,dfrom-1 do dest[i] = nil end
	 else
	    for i = dfrom+sto-sfrom+1,sto do dest[i] = nil end
	 end
      end
      return dest
   end
versions.loadstr = loadstring
versions.mtype = function (x)
      local _,p = math.modf(x)
      return (p == 0.0) and 'integer' or 'float'
   end

versions.unpack = unpack

else -------- Lua 5.3 and newer ---------

versions.isinteger = math.tointeger
versions.tointeger = math.tointeger
versions.move = table.move
versions.loadstr = load
versions.mtype = math.type
versions.unpack = table.unpack

end

return versions
