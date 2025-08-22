--[[		sonata/core/generator.lua

--- Functions for a file generation.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2025.

	module 'generator'
--]]


--	LOCAL

local Help = require('core.help')

local TITLE, DESCRIPTION, CATEGORY, EXTEND = 1, 2, 3, 4
local S10 = string.rep('-', 10)

local LIB = (SONATA_ADD_PATH or '')..'matlib'

local sformat, sgsub = string.format, string.gsub
local tconcat = table.concat

local generator = {}


--================== HTML documentation ====================

--- Prepare strings with help information for html generation.
--  @param module Module name or table.
--  @param alias Alias of the module name.
--  @param lng Localization table from existing file.
--  @return String representation of all help information of the module.
local function docLines(module, alias, lng)
  local m = require('matlib.' .. module)
  local lng_t = lng[module] or {}
  -- collect
  local fn, description = {}, nil
  for k, elt in pairs(m.about) do
    if k == '__module__' then
      description = lng_t[k] or elt
    else
      local title = elt[TITLE]
      local desc = lng_t[title] or elt[DESCRIPTION]
      if desc then
        fn[#fn+1] = {Help._toExtend(title, alias), sgsub(desc, '\n', '<br>\n')}
      end
    end
  end
  -- sort
  table.sort(fn, function (a, b) return a[1] < b[1] end)
  -- format
  for i, v in ipairs(fn) do
    fn[i] = sformat("<b>%s</b> - %s<br>", v[1], v[2])
  end
  return tconcat(fn, "\n"), description
end


--- Prepare module example for html generation.
--  @param str Example text.
--  @return String representation with tags.
local function docExample (str)
  if not str then return nil end
  return sformat('<pre class="example">%s</pre>', str)
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
  for k, v in pairs(tModules) do sortedModules[#sortedModules+1] = {k, v} end
  table.sort(sortedModules, function (a, b) return a[1] < b[1] end)
  -- add content
  res[#res+1] = '<ul>'
  for _, val in ipairs(sortedModules) do
    res[#res+1] = sformat('<li><a href="#%s">%s</a></li>', val[2], val[1])
  end
  res[#res+1] = '</ul></div>'
  res[#res+1] = '<div><h3># About #</h3>'
  -- program description
  local base = sgsub(Sonata._arghelp(), '\n', '<br>\n')
  base = sgsub(base, '(%u%u%u+)', '<b>%1</b>')
  res[#res+1] = sformat('<p>%s</p>', base)
  res[#res+1] =
    '<p><a href="https://github.com/mikhel1984/sonata/wiki">Project Wiki</a></p></div>'

  local fName = sformat('%s%s%s', Help.LOCALE, Help.SEP, locName)
  -- prepare new file
  local lng = Help.lngImport(fName) or {}

  -- modules
  for _, val in ipairs(sortedModules) do
    local k, v = val[1], val[2]
    res[#res+1] = sformat('<div><a name="%s"></a>', v)
    res[#res+1] = sformat('<h3># %s (%s) #</h3>', v, k)
    local functions, description = docLines(k, v, lng)
    res[#res+1] = sformat('<p class="descript">%s</p>', description)
    res[#res+1] = sformat('<p>%s</p>', functions)
    local fstr = Help.readAll(sformat('%s%s%s.lua', LIB, Help.SEP, k))
    res[#res+1] = docExample(Test.getCode(fstr))
    res[#res+1] = '<a href="#Top">Top</a></div>'
  end

  res[#res+1] =
    '<div><p align="center"><i>2017-2024, Stanislav Mikhel</i></p></div>'
  res[#res+1] = '</body></html>'

  -- save
  local f = assert(io.open('help.html', 'w'), "Can't save help file")
  f:write(tconcat(res, '\n'))
  f:close()
  io.write("File 'help.html' is saved!\n")
end


--- Generate markdown file with documentation.
--  @param tModule Table with description for all modules.
generator.md = function (tModule)
  local res = {
    "# Sonata Lua Calculus",
    "",
  }
  -- prepare module list
  local sortedModules = {}
  for k, v in pairs(tModule) do sortedModules[#sortedModules+1] = {k, v} end
  table.sort(sortedModules, function (a, b) return a[1] < b[1] end)
  -- modules
  local sym = {["<b>"]="**", ["</b>"]="**", ["<br>"]="\n"}
  for _, val in ipairs(sortedModules) do
    local k, v = val[1], val[2]
    res[#res+1] = sformat('## %s (%s)', v, k)
    local functions, description = docLines(k, v, {})
    res[#res+1] = description
    res[#res+1] = ""
    res[#res+1] = sgsub(functions, "(<.->)", sym)
    res[#res+1] = ""
  end

  local f = assert(io.open('help.md', 'w'), "Can't save help file")
  f:write(tconcat(res, '\n'))
  f:close()
  io.write("File 'help.md' is saved!\n")
end

--================== Localization template =================

--- Find translation.
--  @param tLang Table with translations.
--  @param key Function signature.
--  @return translation or nil.
local findVal = function (tLang, key)
  local val = tLang[key]
  if not val then
    -- check if signature is modified
    local name = string.match(key, "(%w+)%(")
    if name then
      name = '[^%a]?'..name..'%('    -- make template
      for k, v in pairs(tLang) do
        if string.find(k, name) then return v end
      end
    end
  end
  return val
end


--- Prepare text for dialog.
--  @param tbl Table with descriptions.
--  @param tLang Table with translation.
--  @return Text with description.
local makeDialog = function (tbl, tLang)
  local lng = tLang['Dialog'] or {}
  local res = {
    sformat('%s dialog %s', S10, S10), 'Dialog = {',
  }
  for k, v in pairs(tbl) do
    local title = sformat('["%s"]', k)
    local line = sformat('%-26s = [[%s]],', title, lng[k] or v)
    if not lng[k] then
      line = sformat((line:find('%c') and '--[=[%s]=]' or '--%s'), line)
    end
    res[#res+1] = line
  end
  res[#res+1] = '},\n'
  return tconcat(res, '\n')
end


--- Prepare text for module.
--  @param sName Module name.
--  @param tLang Table with translation.
--  @return Text with description.
local makeModule = function (sName, tLang)
  local m = require('matlib.'..sName)
  local lng = tLang[sName] or {}
  local res = {
    sformat('%s %s.lua %s', S10, sName, S10),
    sformat('%s = {', sName),
  }
  local new = {}
  for k, v in pairs(m.about) do
    local elt, desc = '', ''
    if k == '__module__' then
      elt, desc = k, v
    else
      elt, desc = v[TITLE], v[DESCRIPTION]
    end
    local title = sformat('["%s"]', elt)
    local value = findVal(lng, elt)
    local line = sformat('%-26s = [[%s]],', title, value or desc)
    if elt and desc then
      if value then   -- found translation
        res[#res+1] = line
      else
        new[#new+1] = sformat(
          (line:find('%c') and '--[=[%s]=]' or '--%s'), line)
      end
    end
  end
  for _, v in ipairs(new) do res[#res+1] = v end
  res[#res+1] = '},\n'
  return tconcat(res, '\n')
end


--- Prepare and save localization data.
--  @param fName Language name, for example 'en' or 'it'.
--  @param tModules Table with the list of existing modules.
generator.lang = function(fName, tModules)
  fName = sformat('%s%s%s.lua', Help.LOCALE, Help.SEP, fName)
  -- prepare new file
  local lng = Help.lngImport(fName) or {}
  local f = assert(io.open(fName, 'w'), "Can't save file")
  -- save descriptions
  f:write(S10, sformat(' %s ', fName), S10, '\n\n')
  f:write('return {\n')
  f:write('----------\n')
  -- language and authors
  f:write(
    sformat("language = '%s',", lng.language or 'English'), '\n')
  f:write(
    sformat("authors  = [[%s]],", lng.authors or 'Your Name'),
    '\n')
  -- dialog elements
  f:write(makeDialog(Help.english, lng))
  -- modules
  local modules = {}
  for k, _ in pairs(tModules) do table.insert(modules, k) end
  table.sort(modules)
  for _, v in ipairs(modules) do
    f:write(makeModule(v, lng))
  end
  f:write('}')
  f:close()
  io.write('File ', fName, ' is saved!\n')
end


--================== Module template =======================

--- Generate template for new module.
--  @param mName Module name.
--  @param alias Module short name.
--  @param description Short module description.
generator.module = function (mName, alias, description)
  if not mName then
    return io.write('Expected: --new "name" ["Alias"] ["description"]\n')
  end
  local fName = sformat('%s%s%s.lua', LIB, Help.SEP, mName)
  -- check existence
  local f = io.open(fName)
  if f then
    f:close()
    return io.write('File ', fName, ' is already exists!\n')
  end
  -- write new file
  local txt =
[=[--[[		sonata/WORD1

3L WORD5.
--
--  </br></br><b>Authors</b>: Your Name

	WORD4 'WORD2'
--]]

-- Define here your tests, save results to 'ans',
-- use --> for the strict equality
-- and --n> for the n-digit precision in the case of floating numbers.
--[[TEST_IT

-- use 'WORD2'
WORD3 = require 'matlib.WORD2'

-- example
a = WORD3()
-- check equality
ans = a.type                  -->  'WORD2'

-- check relative equality ( ~10^(-2) )
ans = math.pi                --2> 355/113

--]]


--	LOCAL

local FOO = 42

--	INFO

local help = SonataHelp or {}  -- optional
-- description
local about = {
__module__ = "WORD5"
}


--	MODULE

local WORD2 = {
-- mark
type = 'WORD2',
}
-- methametods
WORD2.__index = WORD2


3L Check object type.
--  @param v Object.
--  @return True if the object is WORD2.
local function isWORD2(v) return getmetatable(v) == WORD2 end


3L Constructor example.
--  @param t Some value.
--  @return New object of WORD2.
WORD2.new = function(self, t)
  local o = {}
  -- your logic
  -- return object
  return setmetatable(o, self)
end
about[WORD2.new] = {":new(t) --> WORD6", "Explicit constructor.", help.NEW}
-- begin from ':' to get 'WORD3:new(t)'


-- simplify constructor call
setmetatable(WORD2, {__call = function (self, v) return WORD2:new(v) end})
about[WORD2] = {" (t) --> WORD6", "Create new WORD2.", help.NEW}
-- begin from ' ' to get 'WORD3 ()'


3L Method example.
--  It is good idea to define method for the copy creation.
--  @return Copy of the object.
WORD2.copy = function (self)
  -- some logic
  return WORD2:new(argument)
end
about[WORD2.copy] = {"WORD6:copy() --> cpy_WORD6",
  "Create a copy of the object."} -- third element is optional, default is 'base'
-- don't modify since start from letter


-- Comment to remove descriptions
WORD2.about = about

return WORD2

--======================================
--TODO: write new functions
]=]
  -- correct text
  -- protect from creating failed documentation
  txt = sgsub(txt, '3L', '---')
  txt = sgsub(txt, '(WORD%d)', {
    WORD1=fName, WORD2=mName, WORD3=alias, WORD4='module',
    WORD5=description, WORD6=alias:sub(1,1)})
  -- save
  f = assert(io.open(fName, 'w'), "Can't save template")
  f:write(txt)
  f:close()
  io.write(
    'File ', fName, " is ready. Add it to the 'use' table in 'sonata.lua'.\n")
end


return generator

--=======================================
--FIX: lost white space in 'use_import' on file update
