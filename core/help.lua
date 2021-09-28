--[[		sonata/core/help.lua 

--- Function description management.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2021.

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

-- internal parameters
local TITLE, DESCRIPTION, CATEGORY, MODULE = 1, 2, 3, 4
local MAIN = 1
local _MAIN_ = '__module__'

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
-- path
LOCALE = (SONATA_ADD_PATH or '')..'locale',
}
-- metamethods
help.__index = help

-- English version of some interface strings
help.english = {
intro = [[
------- help([function]) = get help -------------
---------- use([module]) = expand functionality -
----------------- quit() = exit -----------------
]],
done = 'Done.',
use_import = [[

Call
  use 'module' OR use {'moduleA','moduleB' ...}
to load new modules.]],
}

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
      local module = v[MODULE] or "Main"
      res[module] = res[module] or {}               -- create table for each module
      res[module][category] = res[module][category] or {}  -- add table for each category
      table.insert(res[module][category], v[TITLE])      -- insert function file into this table
    end
  end
  return res
end

--- Prepare information about function or list of all possible functions.
--  @param self Parent object.
--  @param fn Function or module for getting manual.
help.make = function (self,fn)
  if fn then
    -- expected module or function description
    local v = assert(self[fn], "No help for "..tostring(fn))
    if v.link then
      -- module common description
      local res = {SONATA_INFO=true, '\n', v[MAIN], '\n'}
      -- details
      for _,elt in ipairs(v.link:make()) do res[#res+1] = elt end
      return res
    else
      -- function description
      return {SONATA_INFO=true, '  :', Sonata.FORMAT_V1, v[TITLE], '\n', v[DESCRIPTION]}
    end
  else
    local res = {SONATA_INFO=true}
    -- sort functions
    local lst = help._funcList_(self)
    for mod, t in pairs(lst) do             -- for each module
      res[#res+1] = '\n\t'; res[#res+1] = Sonata.FORMAT_V2
      res[#res+1] = mod ; res[#res+1] = '\n'
      for cat, n in pairs(t) do            -- for each category
        res[#res+1] = '    |'; res[#res+1] = Sonata.FORMAT_V1
        res[#res+1] = cat    ; res[#res+1] = ':\n'
        for i, v in ipairs(n) do           -- for each function
          res[#res+1] = v; res[#res+1] = (i ~= #n and ', ' or '\n')
        end
      end 
      --res[#res+1] = '\n'
    end -- for
    return res
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

--- Read file with localization data and update main module.
--  @param self Parent object.
--  @param fName Name of the file with translated text.
help.localization = function (self,fName)
  fName = help.LOCALE..help.SEP..fName
  local lng = help.tblImport(fName)
  if lng then
    getmetatable(self).locale = lng   -- save translation
    -- update functions in main.lua
    local Sn = lng.Main
    for k,v in pairs(self) do
      if v.link then
        -- common description
        self[k][MAIN] = Sn[_MAIN_] or v[MAIN]
      else
        -- details
        self[k][DESCRIPTION] = Sn[v[TITLE]] or v[DESCRIPTION]
        self[k][CATEGORY] = self:get(v[CATEGORY])
      end
    end
  else
    io.write("File ", fName, " wasn't found.\n")
  end
end

--- Get translated string if possible.
--  @param self Parent object.
--  @param txt Text to seek.
--  @return Translated or initial text.
help.get = function (self,txt)
  local mt = getmetatable(self)
  local lng = mt.locale and mt.locale.Dialog and mt.locale.Dialog[txt]  -- check in localization table
  return lng or help.english[txt] or txt
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
