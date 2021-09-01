#!/usr/local/bin/lua
-- Lua based calculator 
-- This file is a part of 'sonata.lib' collection, 2021.

--================= CONFIGURATION ====================

--	Uncomment to set the localization file
--SONATA_LOCALIZATION = "ru.lng"

--	Text coloring
--SONATA_USE_COLOR = true

--	Load after start (optional)
--SONATA_DEFAULT_MODULES = {'matrix','numeric'}

--	Path (optional, for bash alias, e.g. sonata='path/to/sonata.lua')
--SONATA_ADD_PATH = path/to/dir/

--=====================  CODE  ========================

-- Environment
Main_local = { version = '0.9.24' }

-- Add path to the libraries
if SONATA_ADD_PATH then
  package.path = string.format("%s;%s?.lua", package.path, SONATA_ADD_PATH)
end

-- Table for program variables. Import base functions 
Main = require('core.main')

-- Text colors 
Main_help.useColors(SONATA_USE_COLOR) 

-- Quit the program
quit = Main._exit_

-- Update random seed
math.randomseed(os.time())

-- Modules 
use = {
--  name     alias
  array     = "Arr",
  bigint    = "Int",
  complex   = "Comp",
  const     = "_C",
  geodesy   = "Geo",
  gnuplot   = "Gp",
  graph     = "Graph",
  matrix    = "Mat",
  numeric   = "Num",
  polynom   = "Poly",
  quaternion = "Quat",
  rational  = "Rat",
  special   = "Spec",
  stat      = "Stat",
  units     = "Unit",
}

-- Import actions 
function Main_local.doimport(tbl,name)
  local var = tbl[name]
  if not var then
    -- try alias
    if not Main_local.alias then 
      Main_local.alias = {}
      for k,v in pairs(use) do Main_local.alias[v] = k end
    end
    var = name
    name = assert(Main_local.alias[name], "Wrong module name: "..name.."!")
  end
  if not _G[var] then
    local lib = require('lib.'..name)
    _G[var] = lib
    -- add description 
    if lib.about then about:add(lib.about, var) end
    -- do additional actions
    if lib.onImport then lib.onImport() end
  end
  return var, name
end

-- Add modules 
setmetatable(use,
{ -- load modules
  __call = function (self, name)
    if not name then
      -- show loaded modules
      io.write('\n', Main_help.CHELP, string.format("%-12s%-9s%s", "MODULE", "ALIAS", "USED"), '\n')
      for k,v in pairs(use) do
        io.write(string.format("%-13s%-10s%s", k, v, (_G[v] and 'v' or '-')),'\n')
      end
      io.write(about:get('use_import'), Main_help.CRESET, '\n\n')
    elseif name == 'all' then
      -- load all modules
      for k,v in pairs(self) do Main_local.doimport(self,k) end
    elseif type(name) == 'table' then
      -- load group of modules
      for _,v in ipairs(name) do use(v) end
    else
      -- load module
      local var, nm = Main_local.doimport(self,name)
      if SONATA_DIALOG then
        io.write(Main_help.CHELP)
        print(string.format(about:get('alias'), Main_help.CBOLD..var..Main_help.CNBOLD, nm))
      end
    end
  end,
})

--- Print Main_help information.
--  @param fn Function name.
help = function(fn)
  if fn then 
    if fn == use then
      use()
    else
      about:print(type(fn)=='table' and fn.about or fn) 
    end
  else
    about:print(about)
  end
  io.write(Main_help.CRESET)
end

-- command line arguments of Sonata and their processing
local _args_ = {

-- run tests
['--test'] = {
description = 'Apply the unit tests to the desired module. Call all modules if the name is not defined.',
example = '--test array',
process = function (args)
  local Test = require('core.test')
  if args[2] then
    if args[2] == 'main' then
      Test.module(string.format('%score/main.lua', (SONATA_ADD_PATH or '')))
    else
      Test.module(string.format('%slib/%s.lua', (SONATA_ADD_PATH or ''), args[2]))
    end
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
description = 'Creating/updating a file for localization.',
example = '--lang eo',
process = function (args)
  if args[2] then
    SONATA_DIALOG = true -- load help info
    local Gen = require('core.generator')
    Gen.lang(args[2], use)
  else 
    print('Current localization file: ', SONATA_LOCALIZATION)
  end
end,
exit = true},

-- generate 'help.html'
['--doc'] = {
description = 'Creating/updating a documentation file.',
example = '--doc ru',
process = function (args)
  SONATA_DIALOG = true   -- load help info
  local Gen = require('core.generator')
  if args[2] then
    SONATA_LOCALIZATION = args[2]..'.lng'
  end
  Gen.doc(SONATA_LOCALIZATION, use) 
end,
exit = true},

-- new module
['--new'] = {
description = 'Create a template for a new module.',
example = '--new  signal  Sig  "Signal processing functions."',
process = function (args)
  local Gen = require('core.generator')
  Gen.module(args[2],args[3],args[4]) 
end,
exit = true},

-- process files
['default'] = {
--description = 'Evaluate file(s).',
process = function (args) 
  for i = 1,#args do 
    if string.find(args[i], '%.note$') then
      SONATA_DIALOG = true
      Main.evalNote(args[i])
    else
      dofile(args[i]) 
    end
  end 
end,
exit = true},
}

-- show help
_args_['-h'] = {
process = function () print(Main_local._arghelp_()) end,
exit = true}

-- string representation of the help info
Main_local._arghelp_ = function ()
  local txt = {  
    "\n'Sonata' is a Lua based program for mathematical calculations.",
    "",
    "USAGE:",
    "\tlua [-i] sonata.lua [flag] [arg1 arg2 ...]",
    "(option '-i' can be used for working in native Lua interpreter)",
    "",
    "FLAGS:",
    "\t-h - Get this help message.",
    "\t\t{Development}",
  }
  for k,v in pairs(_args_) do 
    if v.description then
      txt[#txt+1] = string.format('\t%-8s - %s', k, v.description)
      if v.example then
        txt[#txt+1] = string.format('\t  (e.g. %s)', v.example)
      end
    end
  end
  txt[#txt+1] = "\t No flag  - Evaluate file(s)."
  txt[#txt+1] = "\nVERSION: "..Main_local.version
  txt[#txt+1] = ""
  local modules = {}
  for k in pairs(use) do modules[#modules+1] = k end
  txt[#txt+1] = string.format("MODULES: %s.\n", table.concat(modules,', '))
  txt[#txt+1] = "BUGS: mail to 'sonatalc@yandex.ru'\n"
  return table.concat(txt,'\n')
end

--================== EXECUTION =================

-- Process command line arguments
if #arg > 0 then
  local command = _args_[arg[1]]
  if not command then command = _args_['default'] end
  command.process(arg)
  if command.exit then os.exit() end
end

-- Prepare for dialog mode
SONATA_DIALOG = true

-- Read localization file and update descriptions
if SONATA_LOCALIZATION then 
  about:localization(SONATA_LOCALIZATION) 
end

-- Run! 
io.write(Main_help.CMAIN, '\n',
"   # #       --=====  Sonata  =====--       # #\n",
"    # #        --==== ", Main_local.version, " ====--        # #\n\n",
Main_help.CHELP)
print(about:get('intro'), Main_help.CRESET)

-- Import default modules
if SONATA_DEFAULT_MODULES then
  use(SONATA_DEFAULT_MODULES)
end

if arg[-1] ~= '-i' then
  Main.evalDialog()
end

--===============================================
--note: all methods in _args_ require exit after execution...
--TODO: fix help(Main)
