--[[          help.lua 

--- Function description management.
--  <i>This file is a part of 
--  <a href="https://github.com/mikhel1984/lc">liblc</a>
--  collection.</i>
--  @copyright 2017, Stanislav Mikhel

            module 'help'
--]]

--[[     == About help system ==

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
--]]

-- internal parameters
local TITLE, DESCRIPTION, CATEGORY, MODULE = 1, 2, 3, 4
local MAIN = 1

--- English version of some interface strings.
--    @class table
--    @name eng
local eng = {
intro = [[
Print 'import(module) to expand functionality.
Print 'help([function]) to get help.
Print 'quit()' for exit.
]],
modules = 'Available modules:',
done = 'Done.',
alias = "Use alias '%s' for access to the module '%s'.",
use_import = [[

Use
  import 'module' ['module2' 'module3' ...]
to get additional modules.]],
}

-------------------------------------------- 
-- @class table
-- @name help
-- @field BASE Constant.
-- @field TRIG Constant.
-- @field HYP Constant.
-- @field CONST Constant.
-- @field OTHER Constant.
-- @field NEW Constant.
local help = {}
help.__index = help
-- constant strings
help.BASE = 'base'
help.TRIG = 'trigonometry'
help.HYP = 'hyperbolic'
help.CONST = 'constants'
help.OTHER = 'other'
help.NEW = 'constructor'

--- Create new object, set metatable.
--    @param str Module description.
--    @return Array object.
function help:new(str)
   assert(str and type(str) == 'string', "Constructor must include description!")
   local o = {}
   o[o] = {link=o, str}        -- save link to itself
   setmetatable(o, self)
   return o
end

--- Create list of functions, sort by module and cathegory.
--    <i>Private function.</i>
--    @param tbl Table with descriptions.
--    @return Sorted table.
local function funclist(tbl)
   local res = {}
   for k, v in pairs(tbl) do
      -- only main description contains 'link'
      if not v.link then
         local category = v[CATEGORY] or ""
	 local module = v[MODULE] or "Default"
	 res[module] = res[module] or {}                      -- create table for each module
         res[module][category] = res[module][category] or {}  -- add table for each category
         table.insert(res[module][category], v[TITLE])        -- insert function fitle into this table
      end
   end
   return res
end

--- Print information about function or list of all possible functions.
--    @param fn Function or module for getting manual.
function help:print(fn)
   if fn then
      -- expected module or functoin description
      local v = self[fn]
      if v.link then                  
         -- module common description
         print(v[MAIN])
	 -- details
	 return v.link:print()
      else                           
         -- function description
         print(string.format("  :%s\n%s", v[TITLE], v[DESCRIPTION]))
      end
   else                               
      -- sort functions
      local lst = funclist(self)
      for mod, t in pairs(lst) do                   -- for each module
         print(string.format("\t%s", mod))
         for cat, n in pairs(t) do                  -- for each category
            print(string.format("  :%s", cat))
	    for i, v in ipairs(n) do                -- for each function
	       io.write(v, (i ~= #n and ', ' or ''))
	    end
	    print()                   -- new line
         end
      end
   end
end

--- Include content of the other help table into current one.
--    @param tbl Table to add.
--    @param nm Name of the added module.
function help:add(tbl, nm)
   assert(nm, "Module name is required!")
   -- localisation data
   local mt = getmetatable(self)
   local lng = mt.locale and mt.locale[nm]
   -- prepare new 
   for k, v in pairs(tbl) do 
      if not v.link then table.insert(v, nm) end -- function description doesn't contain 'link' element
      -- set localisation
      if lng then
         if v.link then
	    -- common description
	    v[MAIN] = lng.__main__ or v[MAIN]
	 else
	    -- details
            v[DESCRIPTION] = lng[v[TITLE]] or v[DESCRIPTION]
	    v[CATEGORY] = self:get(v[CATEGORY])
	 end
      end
      self[k] = v                                -- add to base description table 
   end
   if lng then mt.locale[nm] = nil end -- free memory
end

--- Read file with localisation data and update main module.
--    @param fname Name of the file with translated text.
function help:localisation(fname)
   local f = io.open(fname)
   if f then
      -- read from file and represent as Lua table
      local lng_fn = assert(load("return " .. f:read("*a")))
      f:close()
      local lng = lng_fn()
      -- save into metatable
      getmetatable(self).locale = lng           
      -- update functions in calc.lua
      local lc = lng.Calc
      for k,v in pairs(self) do
         if v.link then
	    -- common description
	    self[k][MAIN] = lc.__main__ or v[MAIN]
	 else
	    -- details
	    self[k][DESCRIPTION] = lc[v[TITLE]] or v[DESCRIPTION]
	    self[k][CATEGORY] = self:get(v[CATEGORY])
	 end
      end
   else
      print("File " .. fname .. " wasn't found.")
   end
end


--- Get translated string if possible.
--    @param txt Text to seek.
--    @return Translated or initial text.
function help:get(txt)
   local mt = getmetatable(self)
   local lng = mt.locale and mt.locale.Calc and mt.locale.Calc[txt]  -- check in localisation table
   return lng or eng[txt] or txt
end

return help

