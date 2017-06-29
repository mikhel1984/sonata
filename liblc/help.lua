--[[     help.lua
Function description management.

about = help:new("Module description*)      -- create new help object

Each function description is represented as table:
about[function] = 
{ 
  function_name,
  function_description,
  function_category
}

If there are more then 1 module, use 
  about:add(table, module_name)
to concatenate descriptions. In this way 4-th entry will be added 
to sort help list according the module name.

To use language localisation, create text file with Lua table in format
{
  module_name1 = {
    __main__ = "Main module description",
    ["function_name1"] = "Function 1 description",
    ["function_name2"] = "Function 2 description",
       ...
  },
  module_name2 = {
  },
  etc.
}
Use about:localisation("file_name") to load it.

This file is a part of liblc collection. 
Stanislav Mikhel, 2017.
]]

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
   local mt = getmetatable(self)
   local lng = mt.locale and mt.locale[nm]
   for k, v in pairs(tbl) do 
      if not v.link then table.insert(v, nm) end -- function description doesn't contain 'link' element
      -- set localisation
      if lng then
         if v.link then
	    v[1] = lng.__main__ or v[1]
	 else
            v[DESCRIPTION] = lng[v[TITLE]] or v[DESCRIPTION]
	 end
      end
      self[k] = v 
   end
   --if lng then mt.locale[nm] = nil end -- free memory
end

-- read file with localisation data and update main module
function help:localisation(fname)
   local f = io.open(fname)
   if f then
      local lng_fn = assert(load("return " .. f:read("*a")))
      f:close()
      local lng = lng_fn()
      getmetatable(self).locale = lng            -- save into metatable
      -- update functions in calc.lua
      local lc = lng.Calc
      for k,v in pairs(self) do
         if v.link then
	    self[k][1] = lc.__main__ or v[1]
	 else
	    self[k][DESCRIPTION] = lc[v[TITLE]] or v[DESCRIPTION]
	 end
      end
   else
      print("File " .. fname .. " wasn't found.")
   end
end

-- intro text
function help:intro()
   local mt = getmetatable(self)
   local lng = mt.locale and mt.locale.Calc and mt.locale.Calc.intro
   local str = [[
Print 'import(module) to expand functionality.
Print 'help([function]) to get help.
Print 'quit()' for exit.
]]
   return lng or str
end

-- text for 'available modules'
function help:modules()
   local mt = getmetatable(self)
   local m = mt.locale and mt.locale.Calc and mt.locale.Calc.modules
   return m or 'Available modules:'
end

return help

