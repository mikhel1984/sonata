-- Help management

local help = {}
help.__index = help

-- internal parameters
local TITLE, DESCRIPTION, CATEGORY, MODULE = 1, 2, 3, 4

-- constant strings
help.BASE = 'base'
help.TRIG = 'trigonometry'
help.HYP = 'hyperbolic'
help.CONST = 'constants'
help.OTHER = 'other'
help.NEW = 'constructor'

-- constructor
function help:new(str)
   assert(str and type(str) == 'string', "Constructor must include description!")
   local o = {}
   o[o] = {link=o, str}        -- save link to itself
   setmetatable(o, self)
   return o
end

-- create list of functions, sort by module and cathegory
local function funclist(tbl)
   local res = {}
   for k, v in pairs(tbl) do
      if not v.link then
         local category = v[CATEGORY] or ""
	 local module = v[MODULE] or "Default"
	 res[module] = res[module] or {}
         res[module][category] = res[module][category] or {}
         table.insert(res[module][category], v[TITLE])
      end
   end
   return res
end

-- print information about function or list of all possible functions
function help:print(fn)
   if fn then
      local v = self[fn]
      if v.link then                  -- common description
         print(v[1])
	 return v.link:print()
      else                            -- function description
         print(string.format("  :%s\n%s", v[TITLE], v[DESCRIPTION]))
      end
   else                               -- function list
      local lst = funclist(self)
      for mod, t in pairs(lst) do
         print(string.format("\t%s", mod))
         for cat, n in pairs(t) do
            print(string.format("  :%s", cat))
	    for i, v in ipairs(n) do
	       io.write(v, (i ~= #n and ', ' or ''))
	    end
	    print()                   -- new line
         end
      end
   end
end

-- include content of the other help table into current one
function help:add(tbl, nm)
   assert(nm, "Module name is required!")
   for k, v in pairs(tbl) do 
      if not v.link then table.insert(v, nm) end -- function description doesn't contain 'link' element
      self[k] = v 
   end
   print("Use '" .. nm .. "' to get access.")
end

return help

