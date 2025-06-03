--[[		sonata/core/load.lua

--- Prepare the main functionality of the program.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2025.

	module 'load'
]]

-- Prepare help module.
SonataHelp = require("core.help")
-- Collect all descriptions
About = SonataHelp.init()
-- Text colors
SonataHelp.useColors(SONATA_USE_COLOR)
-- Input function
local reader = nil

-- Command evaluation.
Sonata = require('core.evaluate')

-- Quit the program
quit = Sonata.exit

-- Get list of aliases
for k, v in pairs(use) do Sonata.alias[v] = k end
-- protect names
setmetatable(_G, {
-- protect aliases
__newindex = function (t, k, v)
  if SONATA_PROTECT_ALIAS and Sonata.alias[k] then
    error(string.format('%s is reserved for %s module', k, Sonata.alias[k]))
  end
  rawset(t, k, v)
end,
-- store into the hidden table
__index = function (t, k)
  if Sonata.alias[k] then
    use(k)
    return Sonata._modules[k]
  end
end
})


-- Import actions
Sonata.doimport = function (tbl, name)
  local var = tbl[name]
  if not var then
    -- try alias
    var = name
    name = assert(Sonata.alias[name], "Wrong module name: "..name.."!")
  end
  if not Sonata._modules[var] then
    local lib = require('matlib.'..name)
    Sonata._modules[var] = lib
    if not SONATA_PROTECT_ALIAS then _G[var] = lib end
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
        string.format("\n%-12s%-9s%s\n\n", "MODULE", "ALIAS", "USED"),
        Sonata.FORMAT_CLR}
      -- show loaded modules
      for k, v in pairs(use) do
        lst[#lst+1] = string.format("%-12s%-10s", k, v)
        if Sonata._modules[v] then
          lst[#lst+1] = Sonata.FORMAT_V1
          lst[#lst+1] = '++\n'
          lst[#lst+1] = Sonata.FORMAT_CLR
        else
          lst[#lst+1] = '\n'
        end
      end
      return Sonata.inLua and Sonata._toText(lst) or lst
    elseif name == '*' then
      -- load all modules
      for k, _ in  pairs(self) do Sonata.doimport(self, k) end
    elseif type(name) == 'table' then
      -- load group of modules
      for i, v in ipairs(name) do Sonata.doimport(self, v) end
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
  if v == '*' then
    res = About:makeFull(use)
  else
    res = About:findObject(v, use) or Sonata.info {
      string.format('<%s>', getmetatable(v) and getmetatable(v).type or type(v)),
      '\n', tostring(v) }
  end
  return Sonata.inLua and Sonata._toText(res) or res
end


-- command line arguments of Sonata and their processing
local _args = {

-- run tests
['--test'] = {
description = 'Run unit tests.',
example = {
  'e.g. --test          # all tests', 
  '     --test array    # tests for the given module'},
process = function (args)
  local Test = require('core.test')
  SONATA_PROTECT_ALIAS = false
  if args[2] then
    Test.module(
      string.format('%smatlib/%s.lua', (SONATA_ADD_PATH or ''), args[2]))
  else
    for m in pairs(use) do
      Test.module(string.format('%smatlib/%s.lua', (SONATA_ADD_PATH or ''), m))
    end
  end
  Test.summary()
end,
exit = true},

-- localization file
['--lang'] = {
description = 'Generation or updating of the localization file.',
example = {
  'e.g. --lang          # show current language', 
  '     --lang eo       # file for Esperanto localization'},
process = function (args)
  if args[2] then
    local Gen = require('core.generator')
    Gen.lang(args[2], use)
  else
    io.write('Current localization file:\t', SONATA_LOCALIZATION or 'en', '\n')
  end
end,
exit = true},

-- generate 'help.html'
['--doc'] = {
description = 'Generating a documentation page.',
example = {
  'e.g. --doc           # original documentation', 
  '     --doc ru        # translated documentation'},
process = function (args)
  local Gen = require('core.generator')
  if args[2] == 'md' then
    Gen.md(use)
  else
    if args[2] then SONATA_LOCALIZATION = args[2]..'.lng' end
    Gen.doc(SONATA_LOCALIZATION, use)
  end
end,
exit = true},

-- new module
['--new'] = {
description = 'Generating a template for a new module.',
example = {'e.g. --new  matrices  Mat  "Matrix operations."'},
process = function (args)
  local Gen = require('core.generator')
  Gen.module(args[2], args[3] or args[2], args[4] or args[2])
end,
exit = true},

-- define method for input (and output)
['--io'] = {
description = "Method for interaction.",
example = {
  'e.g. --io readline   # keep command history', 
  '     --io tcp 23456  # server mode', 
  '     --io w          # output window mode'},
process = function (args)
  if args[2] == 'readline' then
    local readline = require("core.io_readline")
    reader = readline.reader  -- set implementation
    Sonata.set_local_env = readline.set_local_env
  elseif args[2] == 'tcp' then
    local server = require("core.io_socket")
    local port = assert(tonumber(args[3]), 'Expected: --io tcp port')
    server:new('*', port)
    server:repl()
  elseif args[2] == 'w' then
    -- expected Unix
    local fname = Sonata._pipeFile()
    os.execute('> ' .. fname)  -- clear/create file
    os.execute('tail -F ' .. fname)
  else
    print("Unknown option:", args[2])
  end
end,
exit = (arg[2] ~= "readline")},

-- command line evaluation
['-e'] = {
process = function (args)
  local _, _, ans = coroutine.resume(Sonata.evalThread(), args[2] or io.read())
  io.write(ans or "", "\n")
end,
exit = true},

-- process files
['default'] = {
process = function (args)
  if SONATA_LOCALIZATION then
    About:localization(SONATA_LOCALIZATION)
  end
  for i = 1, #args do
    if string.find(args[i], '%.note$') then
      local blk = Sonata._toBlocks(args[i])
      Sonata.repl(blk)
    else
      dofile(args[i])
    end
  end
end,
exit = true},
}

-- show help
_args['-h'] = {
process = function () io.write(Sonata._arghelp(), "\n") end,
exit = true}

-- string representation of the help info
Sonata._arghelp = function ()
  local txt = {
    "\nSonata is a Lua program for mathematical calculations.",
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
      for i = 1, #v.example do
        txt[#txt+1] = string.format('\t\t%s', v.example[i])
      end
    end
  end
  txt[#txt+1] = ""
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


-- Call default module
use('main')


-- Process command line arguments
if #arg > 0 then
  local command = _args[arg[1]]
  if not command then command = _args['default'] end
  command.process(arg)
  if command.exit then os.exit() end
end


-- Run!!!
io.write(SonataHelp.CMAIN, '\n', Sonata.TITLE, '\n', SonataHelp.CHELP)
io.write(About:get('intro'), SonataHelp.CRESET, "\n")


-- choose interpreter
if arg[-1] ~= '-i' then
  Sonata.repl(nil, reader)  -- use Sonata REPL
else
  Sonata.inLua = true       -- use Lua REPL
end

--===============================================
