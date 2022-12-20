--[[		sonata/core/help.lua

--- Function description management.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2022.

	module 'help'
--]]

--[[    == About help system ==

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

Function arguments are typically start from a letter according the convention:
v - any value
t - table
d - any number
f - float point number
i - integer number
b - boolean
s - string
fn - function
N - natural number
capital - object

--]]

--	LOCAL
local Win = SONATA_WIN_CODE and require('core.win') or nil

-- internal parameters
local TITLE, DESCRIPTION, CATEGORY, EXTEND = 1, 2, 3, 4
local COLON = string.byte(':', 1, 1)

local loadStr = (_VERSION < 'Lua 5.3') and loadstring or load

--	MODULE

local help = {
-- constant strings
BASE = 'base',
CONST = 'constants',
OTHER = 'other',
NEW = 'constructor',
META = 'methods',
STATIC = 'common',
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
to load new modules.
]],
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

--- Extend function name if need.
--  @param nm Name.
--  @param alias Module alias name.
--  @return 'name' or 'alias:name'
help._toExtend = function(nm, alias)
  return string.byte(nm, 1, 1) == COLON and alias..nm or nm
end

--- Include content of the other help table into current one.
--  @param dst Table to store data.
--  @param tbl Source table.
--  @param nm Name of the module.
help.add = function (dst, tbl, nm, alias)
  dst._modules[nm] = tbl
  -- update
  local lng = dst._locale[nm] or {}
  for k, v in pairs(tbl) do
    if k == '__module__' then
      tbl[k] = lng.__module__ or v              -- translate module description
    else
      local title = v[TITLE]
      v[DESCRIPTION] = lng[title] or v[DESCRIPTION] -- translate description
      v[CATEGORY] = v[CATEGORY] or help.BASE      -- update category
      v[EXTEND] = help._toExtend(title, alias)
    end
  end
  if lng then dst._locale[nm] = nil end  -- free memory
end

--- Look for object description in the stored data.
--  @param tbl Table with info.
--  @param obj Something that we would like to find.
--  @param tGlob Table with aliases.
--  @return Description or nil.
help.findObject = function (tbl, obj, tGlob)
  -- check module
  local hlp_module = (type(obj) == 'table') and obj.about
  for nm, mod in pairs(tbl._modules) do
    if mod == hlp_module then
      -- module description
      return help.makeModule(mod, tGlob[nm])
    elseif mod[obj] then
      -- function description
      local t = mod[obj]
      Sonata.info {'  ', Sonata.FORMAT_V1, t[EXTEND], '\n', t[DESCRIPTION]}
    end
  end
  return nil
end

--- Get translated string if possible.
--  @param self Parent object.
--  @param txt Text to seek.
--  @return Translated or initial text.
help.get = function (tbl, txt)
  local lng = tbl._locale.Dialog and tbl._locale.Dialog[txt]
  return lng or help.english[txt] or txt
end

--- Prepare main table for help info.
--  @return New table.
help.init = function ()
  return setmetatable({_locale={}, _modules={}}, help)
end

--- Read file with localization data and update main module.
--  @param self Parent object.
--  @param fName Name of the file with translated text.
help.localization = function (dst, fName)
  fName = help.LOCALE..help.SEP..fName
  local lng = Win and help.tblImportWin(fName) or help.tblImport(fName)
  if lng then
    dst._locale = lng
  else
    io.write("File ", fName, " not found.\n")
  end
end

--- Collect information for all modules.
--  @param t Table with all modules.
--  @param tGlob Table with aliases.
--  @return List of strings.
help.makeFull = function (t, tGlob)
  local res = Sonata.info {}
  for nm, mod in pairs(t._modules) do
    local acc = help.makeModule(mod, tGlob[nm])
    for _, v in ipairs(acc) do
      res[#res+1] = v
    end
  end
  return res
end

--- Prepare description for module.
--  @param t Table with functions.
--  @param nm Module name.
--  @return List of strings.
help.makeModule = function (t, nm)
  -- sort by categories
  local acc, txt = {}, ''
  for k, v in pairs(t) do
    if k == '__module__' then
      txt = v
    else
      local cat = v[CATEGORY]
      acc[cat] = acc[cat] or {}
      table.insert(acc[cat], v[EXTEND])
    end
  end
  -- output
  local res = Sonata.info {'\n\t', Sonata.FORMAT_V2, nm,'\n',
    txt, '\n'}
  for cat, n in pairs(acc) do          -- for each category
    res[#res+1] = '    |'; res[#res+1] = Sonata.FORMAT_V1
    res[#res+1] = cat    ; res[#res+1] = ':\n'
    for i, v in ipairs(n) do           -- for each function
      res[#res+1] = v; res[#res+1] = (i ~= #n and ', ' or '\n')
    end
  end
  return res
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
  local str,f = help.readAll(fName), nil
  -- use Lua default import
  if str then f = loadStr(str) end
  return f and f() or nil
end

--- Load and encode Lua table from file.
--  @param fName File name.
--  @return Lua table or nil
help.tblImportWin = function (fName)
  local str,f = help.readAll(fName), nil
  if str then
    str = Win.convert(str)
    f = loadStr(str)
  end
  return f and f() or nil
end

return help

--==========================================
--TODO: localize error messages (?)
