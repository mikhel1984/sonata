--[[		sonata/core/load.lua

--- Prepare the main functionality of the program.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2023.

	module 'load'
]]

-- Prepare help module.
SonataHelp = require("core.help")
-- Collect all descriptions
About = SonataHelp.init()
-- Text colors
SonataHelp.useColors(SONATA_USE_COLOR)

-- Command evaluation.
Sonata = require('core.evaluate')

-- current version
Sonata.version = '0.9.36'

-- Quit the program
quit = Sonata.exit


-- Import actions
Sonata.doimport = function (tbl, name)
  local var = tbl[name]
  if not var then
    -- try alias
    if not Sonata.alias then
      Sonata.alias = {}
      for k, v in pairs(use) do Sonata.alias[v] = k end
    end
    var = name
    name = assert(Sonata.alias[name], "Wrong module name: "..name.."!")
  end
  if not _G[var] then
    local lib = require('lib.'..name)
    _G[var] = lib
    -- add description
    if lib.about then About:add(lib.about, name, var) end
  end
end


-- Add modules
setmetatable(use,
{ -- load modules
  __call = function (self, name)
    if not name then
      local lst = Sonata.info {
        Sonata.FORMAT_V1,
        string.format("\n%-12s%-9s%s\n\n", "MODULE", "ALIAS", "USED")}
      -- show loaded modules
      for k, v in pairs(use) do
        lst[#lst+1] = string.format("%-12s%-10s", k, v)
        if _G[v] then
          lst[#lst+1] = Sonata.FORMAT_V1
          lst[#lst+1] = '++\n'
        else
          lst[#lst+1] = '\n'
        end
      end
      return Sonata.inLua and Sonata._toText(lst) or lst
    elseif name == 'all' then
      -- load all modules
      for k, _ in  pairs(self) do Sonata.doimport(self, k) end
    elseif type(name) == 'table' then
      -- load group of modules
      for _, v in ipairs(name) do Sonata.doimport(self, v) end
    else
      -- load module
      Sonata.doimport(self, name)
    end
  end,
})


--- Print SonataHelp information.
--  @param v Function, module or nil.
help = function(v)
  local res = nil
  v = v or 'main'
  if v == 'all' then
    res = About:makeFull(use)
  else
    res = About:findObject(v, use) or Sonata.info {
      string.format('<%s>', Type and Type(v) or type(v)), '\n', tostring(v) }
  end
  return Sonata.inLua and Sonata._toText(res) or res
end


-- command line arguments of Sonata and their processing
local _args = {

-- run tests
['--test'] = {
description = 'Run unit tests for a given module. Execute for all modules if no name is specified.',
example = '--test array',
process = function (args)
  local Test = require('core.test')
  if args[2] then
    Test.module(
      string.format('%slib/%s.lua', (SONATA_ADD_PATH or ''), args[2]))
  else
    for m in pairs(use) do
      Test.module(string.format('%slib/%s.lua', (SONATA_ADD_PATH or ''), m))
    end
  end
  Test.summary()
end,
exit = true},

-- localization file
['--lang'] = {
description = 'Generation or updating of the localization file.',
example = '--lang eo',
process = function (args)
  if args[2] then
    local Gen = require('core.generator')
    Gen.lang(args[2], use)
  else
    print('Current localization file: ', SONATA_LOCALIZATION)
  end
end,
exit = true},

-- generate 'help.html'
['--doc'] = {
description = 'Generating a documentation page.',
example = '--doc ru',
process = function (args)
  local Gen = require('core.generator')
  if args[2] then
    SONATA_LOCALIZATION = args[2]..'.lng'
  end
  Gen.doc(SONATA_LOCALIZATION, use)
end,
exit = true},

-- new module
['--new'] = {
description = 'Generating a template for a new module.',
example = '--new  matrices  Mat  "Matrix operations."',
process = function (args)
  local Gen = require('core.generator')
  Gen.module(args[2], args[3], args[4])
end,
exit = true},

-- command line evaluation
['-e'] = {
process = function (args)
  local _, _, ans = coroutine.resume(Sonata.evalThread(), args[2] or '')
  print(ans)
end,
exit = true},

-- process files
['default'] = {
--description = 'Evaluate file(s).',
process = function (args)
  if SONATA_LOCALIZATION then
    About:localization(SONATA_LOCALIZATION)
  end
  for i = 1, #args do
    if string.find(args[i], '%.note$') then
      local blk = Sonata._toBlocks(args[i])
      Sonata.cli(blk)
      --Sonata:note(args[i])
    else
      dofile(args[i])
    end
  end
end,
exit = true},
}

-- show help
_args['-h'] = {
process = function () print(Sonata._arghelp()) end,
exit = true}

-- string representation of the help info
Sonata._arghelp = function ()
  local txt = {
    "\nSonata is a Lua based program for mathematical calculations.",
    "",
    "USAGE:",
    "\tlua [-i] sonata.lua [flag] [arg1 arg2 ...]",
    "(option '-i' can be used for working in native Lua interpreter)",
    "",
    "FLAGS:",
    "\t-h\tGet this help message.",
    "\t-e\tEvaluate command line expression.",
    '\t\te.g. -e "2+2"',
    "",
  }
  for k, v in pairs(_args) do
    if v.description then
      txt[#txt+1] = string.format('\t%s\t%s', k, v.description)
      if v.example then
        txt[#txt+1] = string.format('\t\te.g. %s', v.example)
      end
    end
  end
  txt[#txt+1] = "\t No flag  - Evaluate file(s)."
  txt[#txt+1] = "\nVERSION: "..Sonata.version
  txt[#txt+1] = ""
  local modules = {}
  for k in pairs(use) do modules[#modules+1] = k end
  txt[#txt+1] = string.format("MODULES: %s.\n", table.concat(modules, ', '))
  txt[#txt+1] = "BUGS: mail to 'mikhel.sk@gmail.com'\n"
  return table.concat(txt, '\n')
end


--================== EXECUTION =================

-- Read localization file and update descriptions
if SONATA_LOCALIZATION then
  About:localization(SONATA_LOCALIZATION)
end


-- Try to import base functions
pcall(use, 'main')


-- Process command line arguments
if #arg > 0 then
  local command = _args[arg[1]]
  if not command then command = _args['default'] end
  command.process(arg)
  if command.exit then os.exit() end
end


-- Run!!!

io.write(SonataHelp.CMAIN, '\n',
"   # #      --=====  so/\\/ata  =====--       # #\n",
"    # #        --==== ", Sonata.version, " ====--        # #\n\n",
SonataHelp.CHELP)
print(About:get('intro'), SonataHelp.CRESET)

-- Import default modules
if SONATA_DEFAULT_MODULES then
  use(SONATA_DEFAULT_MODULES)
end

-- choose interpreter
if arg[-1] ~= '-i' then
  Sonata.cli()
else
  Sonata.inLua = true
end

--===============================================
--note: all methods in _args require exit after execution...
