--[[		sonata/core/help.lua

--- Function description management.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2025.

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

--]]


--	LOCAL

-- internal parameters
local TITLE, DESCRIPTION, CATEGORY, EXTEND, TEST = 1, 2, 3, 4, 5

-- metamethods signes
local meta_op = {
  __add="+", __sub="-", __mul="*", __div="/", __pow="^", __mod="%", __unm="-obj",
  __idiv="//", __band="&", __bor="|", __bnot="~", __shl="<<", __shr=">>",
  __concat="..", __len="#", __eq="==", __lt="<", __le="<=", __call="obj()"
}


--	MODULE

local help = {
-- constant strings
BASE = 'base',
CONST = 'constants',
OTHER = 'other',
NEW = 'constructor',
META = 'methods',
STATIC = 'common',
-- default language
DEFAULT = 'default',
-- file name separator
SEP = string.sub(package.config, 1, 1),
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
---------- use([module]) = load module ----------
----------------- quit() = exit -----------------
]],
done = 'Done.',
-- commands
cmd_clear = "Clear global variables",
cmd_help = "Show this help",
cmd_log = "Turn on/off logging",
cmd_ls = "Show list of blocks for execution",
cmd_N = "Go to N-th block",
cmd_o = "Open note-file",
cmd_rm = "Clear list of notes",
cmd_q = "Quit",
cmd_show = "Print the next or the given note",
cmd_time = "Estimate average time",
cmd_trace = "Profiling for the function",
cmd_set = "Set short alias for module method",
cmd_shell = "Execute command in shell",
cmd_w = "Write output to 'pipe'",
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
  return (string.find(nm, '^[%a_]') == nil) and alias..nm or nm
end


--- Include content of the other help table into current one.
--  @param dst Table to store data.
--  @param tbl Source table.
--  @param nm Name of the module.
help.add = function (dst, tbl, nm, alias)
  dst._modules[nm] = tbl
  dst._als[nm] = alias
end


--- Look for object description in the stored data.
--  @param tbl Table with info.
--  @param obj Something that we would like to find.
--  @param tGlob Table with aliases.
--  @return Description or nil.
help.findObject = function (tbl, obj, tGlob, lang)
  lang = lang or help.DEFAULT
  -- check module
  local str = type(obj) == 'string'
  for nm, mod in pairs(tbl._modules) do
    if str and (nm == obj or tGlob[nm] == obj) then
      -- module description
      return help.makeModule(tbl, nm, lang)
    elseif mod[obj] then
      -- init description
      if not tbl._info[lang][nm] then
        local tlang = tbl._locales[lang][nm]
        tbl._info[lang][nm] = help.prepareModule(mod, tlang, tbl._als[nm])
      end
      -- function description
      local t = tbl._info[lang][nm][obj]
      local res = {'  ', Sonata.FORMAT_V1, t[EXTEND], Sonata.FORMAT_CLR,
        '\n', t[DESCRIPTION]}
      -- extract examples from unit tests
      help.ext_test = help.ext_test or require('core.test')
      if not t[TEST] then
        local fname = string.format('%smatlib/%s.lua', (SONATA_ADD_PATH or ''), nm)
        local text = assert(help.readAll(fname), "Unable to load '"..fname.."'")
        t[TEST] = help.ext_test.getCode(text)
      end
      local examples = help.ext_test.examples(t[TEST], t[EXTEND])
      if #examples > 0 then
        res[#res+1] = '\n\n  Example\n'
        -- add one example
        res[#res+1] = examples[1]
      end
      return Sonata.info (res)
    end
  end
  return nil
end


--- Get translated string if possible.
--  @param tbl Parent object.
--  @param txt Text to seek.
--  @return Translated or initial text.
help.get = function (tbl, txt, lang)
  lang = lang or help.DEFAULT
  local src = tbl._locales[lang]
  local lng = src.Dialog and src.Dialog[txt]
  return lng or help.english[txt] or txt
end


--- Prepare main table for help info.
--  @return New table.
help.init = function ()
  local o = {
    _locales={}, _modules={}, _als={}, _info = {}
  }
  return setmetatable(o, help)
end


help.prepareLang = function (self, lang)
  lang = lang or help.DEFAULT
  self._locales[lang] = self._locales[lang] or {}
  self._info[lang] = self._info[lang] or {}
end


--- Read file with localization data and update main module.
--  @param self Parent object.
--  @param fName Name of the file with translated text.
help.localization = function (dst, fName)
  local path = help.LOCALE..help.SEP..fName
  local lng = help.lngImport(path)
  if lng then
    dst._locales[fName] = lng
    help.prepareLang(dst, fName)
  else
    io.write("File ", fName, " not found.\n")
  end
end


--- Collect information for all modules.
--  @param t Module info storage.
--  @param tGlob Table with aliases.
--  @return List of strings.
help.makeFull = function (t, tGlob, lang)
  lang = lang or help.DEFAULT
  local res = Sonata.info {}
  for nm, mod in pairs(t._modules) do
    local acc = help.makeModule(t, nm, lang)
    for _, v in ipairs(acc) do
      res[#res+1] = v
    end
  end
  return res
end


--- Generate module description for the given language.
--  @param mod Source module description.
--  @param lang Table with translations.
--  @param alias Module alias name.
--  @return table with function info.
help.prepareModule = function (mod, tlang, alias)
  local acc = {}
  for k, v in pairs(mod) do
    local t = nil
    if k == '__module__' then
      t = tlang.__module__ or v              -- translate module description
    else
      local title = v[TITLE]
      t = {}
      t[TITLE] = title 
      t[DESCRIPTION] = tlang[title] or v[DESCRIPTION] -- translate description
      t[CATEGORY] = v[CATEGORY] or help.BASE      -- update category
      t[EXTEND] = help._toExtend(title, alias)
    end
    acc[k] = t
  end
  return acc
end


--- Prepare description for module.
--  @param store Function info storage.
--  @param nm Module name.
--  @return List of strings.
help.makeModule = function (store, nm, lang)
  local t = store._info[lang][nm]
  if not t then
    local mod = store._modules[nm]
    local tlang = store._locales[lang][nm]
    store._info[lang][nm] = help.prepareModule(mod, tlang, store._als[nm])
    t = store._info[lang][nm]
  end
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
  local res = Sonata.info {'\n\t', Sonata.FORMAT_V2, store._als[nm], Sonata.FORMAT_CLR,
    '\n', txt, '\n'}
  for cat, n in pairs(acc) do          -- for each category
    res[#res+1] = '\t::'
    res[#res+1] = Sonata.FORMAT_V1; res[#res+1] = cat
    res[#res+1] = Sonata.FORMAT_CLR; res[#res+1] = '::\n'
    table.sort(n)
    res[#res+1] = table.concat(n, '\n')
    res[#res+1] = '\n'
  end
  return res
end


--- Collect information about object.
--  @param var Some object.
--  @return table with descriptions.
help.objectInfo = function (var)
  local mt = getmetatable(var)
  local t = {
    string.format('<%s>', mt and mt.type or type(var)),
    '\n', tostring(var)}
  if mt then
    local acc = {}
    for k, v in pairs(mt) do
      if type(k) == 'string' and type(v) == 'function' then
        if string.sub(k, 1, 1) == '_' then
          if meta_op[k] then acc[#acc+1] = meta_op[k] end
        else
          acc[#acc+1] = k
        end
      end
    end
    if #acc > 0 then
      table.sort(acc, function (a, b) return #a < #b end)
      t[#t+1] = '\n'
      t[#t+1] = 'Methods: ' .. table.concat(acc, ', ')
    end
  end
  return t
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


--- Load localization tables from file,
--  decode if need.
--  @param fName File path and name.
--  @return Lua table or nil.
help.lngImport = function (fName)
  local ok, res = pcall(dofile, fName)
  return ok and res or nil
end


return help

--==========================================
--TODO: localize error messages (?)
