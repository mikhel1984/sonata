--[[      liblc/help.lua 

--- Function description management.
--  @author <a href="mailto:vpsys@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

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
    ["__main__"] = "Main module description.",
    ["function_name1"] = "Function 1 description.",
    ["function_name2"] = "Function 2 description.",
       ...
  },
  module_name2 = {
  },
  etc.
}
Use about:localisation("file_name") to load it.
--]]

-- directory with language files
local LOCALE = 'locale'
local LIB = 'liblc'
-- internal parameters
local TITLE, DESCRIPTION, CATEGORY, MODULE = 1, 2, 3, 4
local MAIN = 1
local _MAIN_ = '__main__'

--- English version of some interface strings.
--    @class table
--    @name eng
local eng = {
intro = [[
Print 'import(module)' to expand functionality.
Print 'help([function])' to get help.
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

help.SEP = string.sub(package.config,1,1)

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

--- Create list of functions, sort by module and category.
--    <i>Private function.</i>
--    @param tbl Table with descriptions.
--    @return Sorted table.
local function funclist(tbl)
   local res = {}
   for k, v in pairs(tbl) do
      -- only main description contains 'link'
      if not v.link then
         local category = v[CATEGORY] or help.BASE
	 local module = v[MODULE] or "Default"
	 res[module] = res[module] or {}                      -- create table for each module
         res[module][category] = res[module][category] or {}  -- add table for each category
         table.insert(res[module][category], v[TITLE])        -- insert function file into this table
      end
   end
   return res
end

--- Print information about function or list of all possible functions.
--    @param fn Function or module for getting manual.
function help:print(fn)
   if fn then
      -- expected module or function description
      local v = self[fn]
      if not v then print('No help for :',fn); return end
      if v.link then                  
         -- module common description
         print('\n'..v[MAIN], '\n')
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
            print(string.format("  /%s", cat))
	    for i, v in ipairs(n) do                -- for each function
	       io.write(v, (i ~= #n and ', ' or ''))
	    end
	    print()                   -- new line
         end
	 print()
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
      if not v.link then v[MODULE] = nm end -- function description doesn't contain 'link' element
      -- set localisation
      if lng then
         if v.link then
	    -- common description
	    v[MAIN] = lng[_MAIN_] or v[MAIN]
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
   fname = LOCALE..help.SEP..fname
   -- call method of the 'files' module
   help.lc_files = help.lc_files or require('liblc.files')
   local lng = help.lc_files.tblimport(fname)
   if lng then
      getmetatable(self).locale = lng           
      -- update functions in calc.lua
      local lc = lng.Main
      for k,v in pairs(self) do
         if v.link then
	    -- common description
	    self[k][MAIN] = lc[_MAIN_] or v[MAIN]
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

--- Add table 'about' into the 'eng' for saving into localisation file.
--    <i>Private function.</i>
local function eng2about()
   eng.about = {}
   for k,v in pairs(eng) do
      eng.about[k] = {k,v}
   end
   eng.about.about = nil
end

--- Prepare strings with help information of the given module.
--    <i>Private function.</i>
--    @param module Module name or table.
--    @param alias Alias of the module name.
--    @param lng Localisation table from existing file.
--    @return String representation of all help information of the module.
local function helplines(module, alias, lng)
   -- get table and name
   local m = (type(module) == 'string') and require('liblc.' .. module) or module
   local mname = (type(module) == 'string') and (module .. '.lua') or 'dialog'
   -- choose existing data
   local lng_t = lng and lng[alias] or {}
   -- write to table
   local res = {}
   res[#res+1] = string.format('%s %s %s', string.rep('-',10), mname, string.rep('-',10))
   res[#res+1] = string.format('%s = {', alias)
   -- for all descriptions
   for _, elt in pairs(m.about) do
      -- save
      local title = elt.link and _MAIN_ or elt[TITLE]
      local pos = elt.link and MAIN or DESCRIPTION
      local stitle = string.format('["%s"]', title)
      local line = string.format('%-24s = [[%s]],', stitle, lng_t[title] or elt[pos])
      -- comment if this data are new
      if not lng_t[title] then
         line = string.format((line:find('%c') and '--[=[%s]=]' or '--%s'), line)
      end
      if elt.link then
         -- set main description after the bracket
         table.insert(res,3,line)
      else
         res[#res+1] = line
      end
   end
   res[#res+1] = '},'
   res[#res+1] = ''
   return table.concat(res, '\n')
end

--- Prepare and save localisation data.
--    @param fname Language name, for example 'en' or 'it'.
--    @param modules Table with the list of existing modules.
function help.prepare(fname, modules)
   fname = string.format('%s%s%s.lng', LOCALE, help.SEP, fname)
   -- call method of the 'files' module
   help.lc_files = help.lc_files or require('liblc.files')
   -- prepare new file
   local lng = help.lc_files.tblimport(fname)
   local f = io.open(fname, 'w')
   -- save descriptions
   f:write(string.rep('-',10), string.format(' %s ', fname), string.rep('-',10), '\n')
   f:write('{\n')
   -- language and authors
   f:write((lng and lng.language) and string.format("language =\t '%s',", lng.language) or "-- language =\t 'English',", '\n')
   f:write((lng and lng.authors) and string.format("authors = [[%s]],", lng.authors) or '-- authors = [[Your Name]],', '\n')
   -- dialog elements
   eng2about()
   f:write(helplines(eng,'Dialog',lng))
   -- main functions
   f:write(helplines('main','Main',lng))
   -- other modules
   for k,v in pairs(modules) do
      f:write(helplines(k,v,lng))
   end
   f:write('}')
   f:close()
   print('File '..fname..' is saved!')
end

--- Get translated string if possible.
--    @param txt Text to seek.
--    @return Translated or initial text.
function help:get(txt)
   local mt = getmetatable(self)
   local lng = mt.locale and mt.locale.Dialog and mt.locale.Dialog[txt]  -- check in localisation table
   return lng or eng[txt] or txt
end

function help.newmodule (mname, alias, description)
   if not (mname and alias) then
      print('Both module name and alias are expected!'); return
   end
   local fname = string.format('%s%s%s.lua', LIB, help.SEP, mname)
   -- check existence
   local f = io.open(fname) 
   if f then 
      f:close()
      print('File '..fname..' is already exist!'); return
   end
   -- write new file
   description = description or "This is my cool module!"
   local txt = 
[=[--[[       WORD1

3L WORD5
--  @author My Name

           WORD4 'WORD2'
--]]

3L3L3L3L3L Tests 3L3L3L3L
-- Define here your tests, save results to 'ans', use --> for equality and --~ for estimation.
--[[!!
WORD3 = require 'liblc.WORD2'

-- example
a = WORD3()
ans = a.type                   --> 'WORD2'
]]

3L3L3L3L3L3L3L3L3L3L3L
-- @class table
-- @name WORD2
-- @field about Description of functions.
local WORD2 = {}
WORD2.__index = WORD2

-- mark
WORD2.type = 'WORD2'
WORD2.isWORD2 = true
local function isWORD2(t) return type(t)=='table' and t.isWORD2 end

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
WORD2.about = help:new("WORD5")

3L Constructor example
--    @param t Some value.
--    @return New object of WORD2.
function WORD2:new(t)
   local o = {}
   -- some logic
   setmetatable(o,self)
   return o
end

-- simplify constructor call
setmetatable(WORD2, {__call = function (self,v) return WORD2:new(v) end})
WORD2.WORD3 = 'WORD3'
WORD2.about[WORD2.WORD3] = {"WORD3(t)", "Create new WORD2.", help.NEW}

3L Method example
--   It is good idea to define method for the copy creation.
--   @param t Initial object.
--   @return Copy of the object.
WORD2.copy = function (t)
   -- some logic
   return WORD2:new(argument)
end
WORD2.about[WORD2.copy] = {"copy(t)", "Create a copy of the object.", help.BASE}

-- free memory in case of standalone usage
if not lc_version then WORD2.about = nil end

return WORD2
]=]
   -- correct text
   txt = string.gsub(txt, '3L', '---')            -- protect from creating failed documentation
   txt = string.gsub(txt, '(WORD%d)', {WORD1=fname, WORD2=mname, WORD3=alias, WORD4='module', WORD5=description})
   -- save
   f = io.open(fname, 'w')
   f:write(txt)
   f:close()

   print('File '..fname..' is written.')
end

return help

--==========================================
-- TODO: localise error messages
-- TODO: problem with the common names of different objects in the same module
-- TODO: add help -h for program
