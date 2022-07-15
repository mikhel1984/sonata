--[[		sonata/core/load.lua

--- Prepare the main functionality of the program.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2022.

	module 'load'
]]

-- Prepare help module.
SonataHelp = require "core.help"
About = SonataHelp:new("Lua based mathematics.")
-- Text colors
SonataHelp.useColors(SONATA_USE_COLOR)

-- Command evaluation.
Sonata = require('core.evaluate')
Sonata.version = '0.9.30'

-- Quit the program
quit = Sonata.exit

-- Import actions
function Sonata.doimport(tbl,name)
  local var = tbl[name]
  if not var then
    -- try alias
    if not Sonata.alias then
      Sonata.alias = {}
      for k,v in pairs(use) do Sonata.alias[v] = k end
    end
    var = name
    name = assert(Sonata.alias[name], "Wrong module name: "..name.."!")
  end
  if not _G[var] then
    local lib = require('lib.'..name)
    _G[var] = lib
    -- add description
    if lib.about then About:add(lib.about, var) end
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
      local lst = {SONATA_INFO=true, Sonata.FORMAT_V1, string.format("\n%-12s%-9s%s\n\n", "MODULE", "ALIAS", "USED")}
      -- show loaded modules
      for k,v in pairs(use) do
        lst[#lst+1] = string.format("%-12s%-10s", k, v)
        if _G[v] then
          lst[#lst+1] = Sonata.FORMAT_V1
          lst[#lst+1] = '+\n'
        else
          lst[#lst+1] = '\n'
        end
      end
      lst[#lst+1] = Sonata.FORMAT_V1
      lst[#lst+1] = About:get('use_import')
      return Sonata.inLua and Sonata._toText(lst) or lst
    elseif name == 'all' then
      -- load all modules
      for k,v in pairs(self) do Sonata.doimport(self,k) end
    elseif type(name) == 'table' then
      -- load group of modules
      for _,v in ipairs(name) do Sonata.doimport(self,v) end
    else
      -- load module
      local var, nm = Sonata.doimport(self,name)
    end
  end,
})

--- Print SonataHelp information.
--  @param fn Function, module or nil.
help = function(fn)
  if fn then
    if fn == use then
      return use()
    else
      local res = About:make(type(fn)=='table' and fn.about or fn)
      return Sonata.inLua and Sonata._toText(res) or res
    end
  else
    local res = About:make(About)
    return Sonata.inLua and Sonata._toText(res) or res
  end
end

--- Session logging.
--  @param flat Value 'on'/true to start and 'off'/false to stop.
Log = function (flag)
  if flag == 'on' then
    if not Sonata._logFile_ then
      Sonata._logFile_ = io.open(Sonata.LOGNAME, 'a')
      local d = os.date('*t')
      Sonata._logFile_:write(string.format('\n--\tSession\n-- %d-%d-%d %d:%d\n\n', d.day, d.month, d.year, d.hour, d.min))
      Sonata._logFile_:write('-- ')  -- prepare comment for 'logging on'
    end
  elseif flag == 'off' then
    if Sonata._logFile_ then
      Sonata._logFile_:close()
      Sonata._logFile_ = nil
    end
  else
    io.write('Unexpected argument!\n')
  end
end
About[Log] = {'Log(sFlag)', "Save session into the log file. Use 'on'/'off' to start/stop logging.", SonataHelp.OTHER}


-- command line arguments of Sonata and their processing
local _args_ = {

-- run tests
['--test'] = {
description = 'Apply the unit tests to the desired module. Call all modules if the name is not defined.',
example = '--test array',
process = function (args)
  local Test = require('core.test')
  if args[2] then
    Test.module(string.format('%slib/%s.lua', (SONATA_ADD_PATH or ''), args[2]))
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

-- command line evaluation
['-e'] = {
process = function (args)
  Sonata:eval(args[2] or '', false)
  print(Sonata._ans)
end,
exit = true},

-- process files
['default'] = {
--description = 'Evaluate file(s).',
process = function (args)
  if SONATA_LOCALIZATION then
    About:localization(SONATA_LOCALIZATION)
  end
  for i = 1,#args do
    if string.find(args[i], '%.note$') then
      Sonata:note(args[i])
    else
      dofile(args[i])
    end
  end
end,
exit = true},
}

-- show help
_args_['-h'] = {
process = function () print(Sonata._arghelp_()) end,
exit = true}

-- string representation of the help info
Sonata._arghelp_ = function ()
  local txt = {
    "\n'Sonata' is a Lua based program for mathematical calculations.",
    "",
    "USAGE:",
    "\tlua [-i] sonata.lua [flag] [arg1 arg2 ...]",
    "(option '-i' can be used for working in native Lua interpreter)",
    "",
    "FLAGS:",
    "\t-h - Get this help message.",
    "\t-e - Evaluate command line expression.",
    '\t  (e.g. -e "2+2")',
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
  txt[#txt+1] = "\nVERSION: "..Sonata.version
  txt[#txt+1] = ""
  local modules = {}
  for k in pairs(use) do modules[#modules+1] = k end
  txt[#txt+1] = string.format("MODULES: %s.\n", table.concat(modules,', '))
  txt[#txt+1] = "BUGS: mail to 'mikhel.sk@gmail.com'\n"
  return table.concat(txt,'\n')
end

--================== EXECUTION =================

-- Try to import base functions
_, Main = pcall(require, 'lib.main')

-- Process command line arguments
if #arg > 0 then
  local command = _args_[arg[1]]
  if not command then command = _args_['default'] end
  command.process(arg)
  if command.exit then os.exit() end
end

-- Read localization file and update descriptions
if SONATA_LOCALIZATION then
  About:localization(SONATA_LOCALIZATION)
end

-- Run!!!

io.write(SonataHelp.CMAIN, '\n',
"   # #       --=====  Sonata  =====--       # #\n",
"    # #        --==== ", Sonata.version, " ====--        # #\n\n",
SonataHelp.CHELP)
print(About:get('intro'), SonataHelp.CRESET)

-- Import default modules
if SONATA_DEFAULT_MODULES then
  use(SONATA_DEFAULT_MODULES)
end

if arg[-1] ~= '-i' then
  Sonata:cli()
else
  Sonata.inLua = true
end

--===============================================
--note: all methods in _args_ require exit after execution...
--TODO: fix help(Main)
