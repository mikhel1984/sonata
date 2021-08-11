--[[		sonata/core/help.lua 

--- Function description management.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata</a> collection, 2021.

	module 'help'
--]]

--[[    == About help system ==

about = help:new("Module description*)  -- create new help object

Each function description is represented as a table:
about[function] =
{
  function_name,
  function_description,
  function_category (can be skipped)
}

If there are several modules, use
  about:add(table, module_name)
to concatenate descriptions. In this case the 4-th entry will be added
to sort help list according the module name.

--]]

--	LOCAL

-- directory with language files
local LOCALE = (LC_ADD_PATH or '')..'locale'
local LIB   = (LC_ADD_PATH or '')..'lib'

-- internal parameters
local TITLE, DESCRIPTION, CATEGORY, MODULE = 1, 2, 3, 4
local MAIN = 1
local _MAIN_ = '__module__'

-- English version of some interface strings
local eng = {
intro = [[
------- help([function]) = get help -------------
------- dofile(filename) = execute Lua script ---
------------ use(module) = expand functionality -
----------------- quit() = exit -----------------
]],
--modules = 'Available modules:',
done = 'Done.',
alias = "Use alias '%s' for access to the '%s' module.",
use_import = [[

Call
  use 'module' OR use {'moduleA','moduleB' ...}
to load modules.]],
}

--- Add table 'about' into the 'eng' for saving into localization file.
local function eng2about()
  eng.about = {}
  for k,v in pairs(eng) do
    eng.about[k] = {k,v}
  end
  eng.about.about = nil
end

local loadStr = (_VERSION < 'Lua 5.3') and loadstring or load

--	MODULE

local help = {
-- constant strings
BASE = 'base',
CONST = 'constants',
OTHER = 'other',
NEW = 'constructor',
META = 'methods',
-- file name separator
SEP = string.sub(package.config,1,1),
-- colors
CMAIN = '',
CHELP = '',
CRESET = '',
CBOLD = '',
CNBOLD = '',
CERROR = '',
}
-- metamethods
help.__index = help

--- Auxiliary function, which define colors for text elements.
--  @param bUse Boolean flag of usage.
help.useColors = function (bUse)
  if bUse then
    help.CMAIN  = '\x1B[32m' 
    help.CHELP  = '\x1B[33m' 
    help.CRESET = '\x1B[0m'
    help.CBOLD  = '\x1B[1m'
    help.CNBOLD = '\x1B[22m'
    help.CERROR = '\x1B[31m'
  end
end

--================== Function help system ==================

--- Create new object, set metatable.
--  @param self Parent table.
--  @param str Module description.
--  @return Help object.
help.new = function (self,str)
  assert(type(str) == 'string', "Constructor must have a description!")
  local o = {}
  o[o] = {link=o, str}      -- save link to itself
  return setmetatable(o, self)
end

--- Create list of functions, sort by module and category.
--  @param tbl Table of pairs 'function - description'.
--  @return Grouped descriptions.
help._funcList_ = function (tbl)
  local res = {}
  for k, v in pairs(tbl) do
    -- only main description contains 'link'
    if not v.link then
      local category = v[CATEGORY] or help.BASE
      local module = v[MODULE] or "Default"
      res[module] = res[module] or {}               -- create table for each module
      res[module][category] = res[module][category] or {}  -- add table for each category
      table.insert(res[module][category], v[TITLE])      -- insert function file into this table
    end
  end
  return res
end

--- Print information about function or list of all possible functions.
--  @param self Parent object.
--  @param fn Function or module for getting manual.
help.print = function (self,fn)
  io.write(help.CHELP)
  if fn then
    -- expected module or function description
    local v = self[fn]
    if not v then 
      return print('No help for :',fn) 
    end
    if v.link then
      -- module common description
      io.write('\n',v[MAIN],'\n\n')
      -- details
      v.link:print()
    else
      -- function description
      print(string.format("  :%s\n%s", v[TITLE], v[DESCRIPTION]))
    end
  else
    -- sort functions
    local lst = help._funcList_(self)
    for mod, t in pairs(lst) do             -- for each module
      io.write(help.CBOLD, '\t', mod, '\n', help.CNBOLD)
      for cat, n in pairs(t) do            -- for each category
        io.write("    |", cat, ':\n')
        for i, v in ipairs(n) do           -- for each function
          io.write(v, (i ~= #n and ', ' or '\n'))
        end
      end 
      io.write('\n')
    end -- for
  end -- if
end

--- Include content of the other help table into current one.
--  @param self Parent object.
--  @param tbl Table to add.
--  @param nm Name of the added module.
help.add = function (self,tbl,nm)
  assert(nm, "Module name is required!")
  -- localization data
  local mt = getmetatable(self)
  local lng = mt.locale and mt.locale[nm]
  -- prepare new 
  for k, v in pairs(tbl) do 
    if not v.link then v[MODULE] = nm end    -- no 'link' element in the function description
    if lng then
      -- set localization
      if v.link then
        -- common description
        v[MAIN] = lng[_MAIN_] or v[MAIN]
      else
        -- details
        v[DESCRIPTION] = lng[v[TITLE]] or v[DESCRIPTION]
        v[CATEGORY] = self:get(v[CATEGORY])
      end 
    end -- lng
    self[k] = v                       -- add to the base table 
  end
  if lng then mt.locale[nm] = nil end -- free memory
end

--================== Localization template =================

--- Prepare strings with help information of the given module.
--  @param module Module name or table.
--  @param alias Alias of the module name.
--  @param lng Localization table from existing file.
--  @return String representation of all help information of the module.
local function helpLines(module, alias, lng)
  -- get table and name
  local m = (type(module) == 'string') 
             and require((module == 'main' and 'core.' or 'lib.') .. module)
             or module
  local mName = (type(module) == 'string') and (module .. '.lua') or 'dialog'
  -- choose existing data
  local lng_t = lng and lng[alias] or {}
  -- write to table
  local res = {
    string.format('%s %s %s', string.rep('-',10), mName, string.rep('-',10)),
    string.format('%s = {', alias)
  }
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

--- Read file with localization data and update main module.
--  @param self Parent object.
--  @param fName Name of the file with translated text.
help.localization = function (self,fName)
  fName = LOCALE..help.SEP..fName
  local lng = help.tblImport(fName)
  if lng then
    getmetatable(self).locale = lng   -- save translation
    -- update functions in main.lua
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
    io.write("File ", fName, " wasn't found.\n")
  end
end

--- Prepare and save localization data.
--  @param fName Language name, for example 'en' or 'it'.
--  @param tModules Table with the list of existing modules.
help.prepare = function(fName, tModules)
  fName = string.format('%s%s%s.lng', LOCALE, help.SEP, fName)
  -- prepare new file
  local lng = help.tblImport(fName)
  local f = io.open(fName, 'w')
  -- save descriptions
  f:write(string.rep('-',10), string.format(' %s ', fName), string.rep('-',10), '\n')
  f:write('{\n')
  -- language and authors
  f:write(string.format("language = '%s',", lng and lng.language or 'English'), '\n')
  f:write(string.format("authors  = [[%s]],", lng and lng.authors or 'Your Name'), '\n')
  -- dialog elements
  eng2about()
  f:write(helpLines(eng,'Dialog',lng))
  -- main functions
  f:write(helpLines('main','Main',lng))
  -- other modules
  for k,v in pairs(tModules) do
    f:write(helpLines(k,v,lng))
  end
  f:write('}')
  f:close()
  io.write('File ', fName, ' is saved!\n')
end

--- Get translated string if possible.
--  @param self Parent object.
--  @param txt Text to seek.
--  @return Translated or initial text.
help.get = function (self,txt)
  local mt = getmetatable(self)
  local lng = mt.locale and mt.locale.Dialog and mt.locale.Dialog[txt]  -- check in localization table
  return lng or eng[txt] or txt
end

--================== Module template =======================

--- Generate template for new module.
--  @param mName Module name.
--  @param alias Module short name.
--  @param description Short module description.
help.newModule = function (mName, alias, description)
  if not (mName and alias) then 
    return print('Expected: --new "name" "Alias" ["description"]')
  end
  local fName = string.format('%s%s%s.lua', LIB, help.SEP, mName)
  -- check existence
  local f = io.open(fName) 
  if f then 
    f:close()
    return print('File '..fName..' is already exists!')
  end
  -- write new file
  description = description or "The module of your dream!"
  local txt = 
[=[--[[		WORD1

3L WORD5
--  @author My Name

	WORD4 'WORD2'
--]]

-- Define here your tests, save results to 'ans', use --> for the strict equality and --2> for the two digit precision (for example).
--[[TEST

-- use 'WORD2'
WORD3 = require 'lib.WORD2'

-- example
a = WORD3()
ans = a.type   -->  'WORD2'

ans = math.pi  --2> 355/113

--]]

--	LOCAL

3L Check object type.
--  @param t Object.
--  @return True if the object is WORD2.
local function isWORD2(t) return type(t)=='table' and t.isWORD2 end

--	INFO

local help = LC_DIALOG and (require "core.help") or {new=function () return {} end}

--	MODULE

local WORD2 = {
-- mark
type = 'WORD2', isWORD2 = true,
-- description
about = help:new("WORD5"),
}
-- methametods
WORD2.__index = WORD2

3L Constructor example.
--  @param t Some value.
--  @return New object of WORD2.
WORD2.new = function(self,t)
  local o = {}
  -- your logic 
  -- return object
  return setmetatable(o,self)
end

-- simplify constructor call
setmetatable(WORD2, {__call = function (self,v) return WORD2:new(v) end})
WORD2.WORD3 = 'WORD3'
WORD2.about[WORD2.WORD3] = {"WORD3(t)", "Create new WORD2.", help.NEW}

3L Method example.
--  It is good idea to define method for the copy creation.
--  @param t Initial object.
--  @return Copy of the object.
WORD2.copy = function (t)
  -- some logic
  return WORD2:new(argument)
end
WORD2.about[WORD2.copy] = {"copy(t)", "Create a copy of the object."} -- third element is optional, default is 'base'

-- Uncomment to remove descriptions
--WORD2.about = nil

return WORD2

--======================================
--TODO: write new functions
]=]
  -- correct text
  txt = string.gsub(txt, '3L', '---')     -- protect from creating failed documentation
  txt = string.gsub(txt, '(WORD%d)', {WORD1=fName, WORD2=mName, WORD3=alias, WORD4='module', WORD5=description})
  -- save
  f = io.open(fName, 'w')
  f:write(txt)
  f:close()
  io.write('File ', fName, ' is ready.\n')
end

--================== HTML documentation ====================

--- Prepare strings with help information for html generation.
--  @param module Module name or table.
--  @param alias Alias of the module name.
--  @param lng Localization table from existing file.
--  @return String representation of all help information of the module.
local function docLines(module, alias, lng)
  local m = require((module == 'main' and 'core.' or 'lib.')..module)
  local lng_t = lng and lng[alias] or {}
  -- collect
  local fn, description = {}
  for _, elt in pairs(m.about) do
    if elt.link then
      description = lng_t[_MAIN_] or elt[MAIN]
    else
      local title = elt[TITLE]
      local desc = lng_t[title] or elt[DESCRIPTION]
      fn[#fn+1] = {title, string.gsub(desc, '\n', '<br>\n')}
    end
  end
  -- sort
  table.sort(fn, function (a,b) return a[1] < b[1] end)
  -- format
  for i,v in ipairs(fn) do
    fn[i] = string.format("<b>%s</b> - %s<br>", v[1], v[2])
  end
  return table.concat(fn, "\n"), description
end

--- Prepare module example for html generation.
--  @param str Example text.
--  @return String representation with tags.
local function docExample (str)
  if not str then return nil end
  return string.format('<pre class="example">%s</pre>', str)
end

--- Generate html file with documentation.
--  @param locName Name of locale.
--  @param tModules Table with description for all modules.
help.generateDoc = function (locName, tModules)
  -- prepare text
  local res = {
    "<html><head>",
    '<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">',
    "<title>Sonata LC Help</title>",
    "</head><body>",
    '<style type="text/css">',
    'H3 {text-align:center;}',
    'P {line-height:1.3;}',
    'UL {column-count: 4; border: 2px solid blue; border-radius: 5px; padding: 5px 5px 5px 30px;}',
    'DIV {margin-left: 10px;}',
    '.EXAMPLE {border: 2px solid blue; border-radius: 5px; background: lightgrey; padding: 15px; margin: 10px 0px; color:navy; }',
    '.DESCRIPT {text-align:center; font-style:italic; }',
    '</style>',
    '<a name="Top"></a>',
    '<div><h1 align="center">Sonata Lua Calculus</h1>',
  }
  -- prepare module list
  local sortedModules = {}
  for k,v in pairs(tModules) do sortedModules[#sortedModules+1] = {k,v} end
  table.sort(sortedModules, function (a,b) return a[1] < b[1] end)
  -- add content
  res[#res+1] = '<ul>'
  for _,val in ipairs(sortedModules) do
    res[#res+1] = string.format('<li><a href="#%s">%s</a></li>', val[2], val[1])
  end
  res[#res+1] = '</ul></div>'
  res[#res+1] = '<div><h3># About #</h3>'
  -- program description
  local base = string.gsub(lc._arghelp_(), '\n', '<br>\n')
  base = string.gsub(base, '(%u%u%u+)', '<b>%1</b>')
  res[#res+1] = string.format('<p>%s</p>', base)
  res[#res+1] = '<p><a href="https://github.com/mikhel1984/sonata/wiki">Project Wiki</a></p></div>'

  local fName = string.format('%s%s%s', LOCALE, help.SEP, locName)
  -- call method of the 'files' module
  help.lc_test = help.lc_test or require('core.test')
  -- prepare new file
  local lng = help.tblImport(fName)

  eng2about()
  
  res[#res+1] = '<div><a name="Main"></a>'
  res[#res+1] = '<h3># Main (main) #</h3>'
  local functions, description = docLines('main','Main',lng)
  res[#res+1] = string.format('<p class="descript">%s</p>', description)
  res[#res+1] = string.format('<p>%s</p>', functions)
  local fstr = help.readAll(string.format('%score%smain.lua', (LC_ADD_PATH or ''), help.SEP))
  res[#res+1] = docExample(help.lc_test._getCode_(fstr))
  res[#res+1] = '<a href="#Top">Top</a></div>'
  -- other modules
  for _, val in ipairs(sortedModules) do
    local k,v = val[1], val[2]
    res[#res+1] = string.format('<div><a name="%s"></a>', v)
    res[#res+1] = string.format('<h3># %s (%s) #</h3>', v, k)
    functions, description = docLines(k, v, lng)
    res[#res+1] = string.format('<p class="descript">%s</p>', description)
    res[#res+1] = string.format('<p>%s</p>', functions)
    fstr = help.readAll(string.format('%s%s%s.lua', LIB, help.SEP, k))
    res[#res+1] = docExample(help.lc_test._getCode_(fstr))
    res[#res+1] = '<a href="#Top">Top</a></div>'
  end

  res[#res+1] = '<div><p align="center"><i>2021, Stanislav Mikhel</i></p></div>'
  res[#res+1] = '</body></html>'

  -- save
  local f = io.open('help.html','w')
  f:write(table.concat(res,'\n'))
  f:close()
  io.write("File 'help.html' is saved!\n")
end

--================== Files ===================

--- Returns text of the file.
--  @param fName
--  @return String or nil.
help.readAll = function (fName)
  local f, str = io.open(fName, 'r')
  if f then
    str = f:read('*a')
    f:close()
  end
  return str
end

--- Load Lua table from file.
--  @param fName File name.
--  @return Lua table or nil.
help.tblImport = function (fName)
  local str,f = help.readAll(fName)
  -- use Lua default import
  if str then f = loadStr('return '..str) end
  return f and f() or nil
end

return help

--==========================================
--TODO: localize error messages (?)
