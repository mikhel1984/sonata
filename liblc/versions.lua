--[[      liblc/versions.lua 

--- Auxilary functions for compatiblity with Lua 5.1 and 5.2.
--  @author <a href="mailto:vpsys@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

            module 'versions'
--]]

local versions = {}

if _VERSION < 'Lua 5.3' then

versions.isinteger = function (x) _,p = math.modf(x); return p == 0.0 end
versions.move = function (src,sfrom,sto,dfrom,dest)
      k = 0
      for i = sfrom,sto do dest[dfrom+k] = src[i]; k=k+1 end
      return dest
   end
versions.loadstr = loadstring

else -------- Lua 5.3 and newer ---------

versions.isinteger = math.tointeger
versions.move = table.move
versions.loadstr = load

end

return versions
