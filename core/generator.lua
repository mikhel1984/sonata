--[[		sonata/core/generator.lua

--- Functions for a file generation.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2022.

	module 'generator'
--]]

local Help = require('core.help')

--	LOCAL 

local TITLE, DESCRIPTION, CATEGORY, MODULE = 1, 2, 3, 4
local MAIN = 1
local _MAIN_ = '__module__'

local LIB = (SONATA_ADD_PATH or '')..'lib'

--- Add table 'about' into the 'eng' for saving into localization file.
local function eng2about()
  Help.english.about = {}
  for k,v in pairs(Help.english) do
    Help.english.about[k] = {k,v}
  end
  Help.english.about.about = nil
end

--================== Module template =======================

local generator = {}

--- Generate template for new module.
--  @param mName Module name.
--  @param alias Module short name.
--  @param description Short module description.
generator.module = function (mName, alias, description)
  if not (mName and alias) then 
    return print('Expected: --new "name" "Alias" ["description"]')
  end
  local fName = string.format('%s%s%s.lua', LIB, Help.SEP, mName)
  -- check existence
  local f = io.open(fName) 
  if f then 
    f:close()
    return print('File '..fName..' is already exists!')
  end
  -- write new file
  description = description or "The module of your dream!"
  local txt = 
[=[--[[		sonata/WORD1

3L WORD5
--  @author Your Name

	WORD4 'WORD2'
--]]

-- Define here your tests, save results to 'ans', use --> for the strict equality 
-- and --n> for the n-digit precision in the case of floating numbers.
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
--  @param v Object.
--  @return True if the object is WORD2.
local function isWORD2(v) return type(v)=='table' and v.isWORD2 end

--	INFO

local help = SonataHelp or {new=function () return {} end}
-- description
local about = help:new("WORD5")

--	MODULE

local WORD2 = {
-- mark
type = 'WORD2', isWORD2 = true,
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
about[WORD2.WORD3] = {"WORD3(t)", "Create new WORD2.", help.NEW}

3L Method example.
--  It is good idea to define method for the copy creation.
--  @param t Initial object.
--  @return Copy of the object.
WORD2.copy = function (t)
  -- some logic
  return WORD2:new(argument)
end
about[WORD2.copy] = {"copy(t)", "Create a copy of the object."} -- third element is optional, default is 'base'

-- Comment to remove descriptions
WORD2.about = about

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
  io.write('File ', fName, " is ready. Add it to the 'use' table in 'sonata.lua'.\n")
end

--================== HTML documentation ====================

--- Prepare strings with help information for html generation.
--  @param module Module name or table.
--  @param alias Alias of the module name.
--  @param lng Localization table from existing file.
--  @return String representation of all help information of the module.
local function docLines(module, alias, lng)
  local m = require('lib.' .. module)
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
generator.doc = function (locName, tModules)
  local Test = require('core.test')
  -- prepare text
  local res = {
    "<html><head>",
    '<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">',
    "<title>Sonata Help</title>",
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
  local base = string.gsub(Sonata._arghelp_(), '\n', '<br>\n')
  base = string.gsub(base, '(%u%u%u+)', '<b>%1</b>')
  res[#res+1] = string.format('<p>%s</p>', base)
  res[#res+1] = '<p><a href="https://github.com/mikhel1984/sonata/wiki">Project Wiki</a></p></div>'

  local fName = string.format('%s%s%s', Help.LOCALE, Help.SEP, locName)
  -- prepare new file
  local lng = Help.tblImport(fName)

  eng2about()
  
  res[#res+1] = '<div><a name="Main"></a>'
  res[#res+1] = '<h3># Main (main) #</h3>'
  local functions, description = docLines('main','Main',lng)
  res[#res+1] = string.format('<p class="descript">%s</p>', description)
  res[#res+1] = string.format('<p>%s</p>', functions)
  local fstr = Help.readAll(string.format('%slib%smain.lua', (SONATA_ADD_PATH or ''), Help.SEP))
  res[#res+1] = docExample(Test._getCode_(fstr))
  res[#res+1] = '<a href="#Top">Top</a></div>'
  -- other modules
  for _, val in ipairs(sortedModules) do
    local k,v = val[1], val[2]
    res[#res+1] = string.format('<div><a name="%s"></a>', v)
    res[#res+1] = string.format('<h3># %s (%s) #</h3>', v, k)
    functions, description = docLines(k, v, lng)
    res[#res+1] = string.format('<p class="descript">%s</p>', description)
    res[#res+1] = string.format('<p>%s</p>', functions)
    fstr = Help.readAll(string.format('%s%s%s.lua', LIB, Help.SEP, k))
    res[#res+1] = docExample(Test._getCode_(fstr))
    res[#res+1] = '<a href="#Top">Top</a></div>'
  end

  res[#res+1] = '<div><p align="center"><i>2017-2022, Stanislav Mikhel</i></p></div>'
  res[#res+1] = '</body></html>'

  -- save
  local f = io.open('help.html','w')
  f:write(table.concat(res,'\n'))
  f:close()
  io.write("File 'help.html' is saved!\n")
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
             and require('lib.' .. module)
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



--- Prepare and save localization data.
--  @param fName Language name, for example 'en' or 'it'.
--  @param tModules Table with the list of existing modules.
generator.lang = function(fName, tModules)
  fName = string.format('%s%s%s.lng', Help.LOCALE, Help.SEP, fName)
  -- prepare new file
  local lng = Help.tblImport(fName)
  local f = io.open(fName, 'w')
  -- save descriptions
  f:write(string.rep('-',10), string.format(' %s ', fName), string.rep('-',10), '\n')
  f:write('{\n')
  -- language and authors
  f:write(string.format("language = '%s',", lng and lng.language or 'English'), '\n')
  f:write(string.format("authors  = [[%s]],", lng and lng.authors or 'Your Name'), '\n')
  -- dialog elements
  eng2about()
  f:write(helpLines(Help.english,'Dialog',lng))
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

return generator

--=======================================
--TODO: update Help.english once, on import
