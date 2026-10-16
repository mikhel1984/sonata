--[[		sonata/core/load.lua

--- Prepare the main functionality of the program.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2026.

	module 'load'
]]


-- Global state.
Sonata = {
  -- version
  MAJOR_V = 1, MINOR_V = 4,
  -- keep modules
  alias = {}, _modules = {},
}


-- Prepare help module.
SonataHelp = require("core.help")
-- Collect all descriptions
About = SonataHelp.init()
About:prepareLang()


-- Main definitions.
local _base = require("core.io_base")
-- Show table if need.
tprint = _base.doShowTable
-- Quit the program.
quit = os.exit
-- Show warning
Sonata.warning = _base.warning
-- Ask question
Sonata.ask = _base.ask


--- Make title string.
--  @return title string.
Sonata.getTitle = function ()
  return string.format(
[[   # #      --=====  so/\/ata  =====--     # #
    # #         --==== %d.%02d ====--        # #
]], Sonata.MAJOR_V, Sonata.MINOR_V)
end


-- Get list of aliases
for k, v in pairs(use) do Sonata.alias[v] = k end

-- protect names
setmetatable(_G, {
-- store into the hidden table
__index = function (t, k)
  if Sonata.alias[k] then
    use(k)
    return Sonata._modules[k]
  end
end,
-- protect aliases
__newindex = function (t, k, v)
  if SONATA_PROTECT_ALIAS and Sonata.alias[k] then
    error(string.format("%s is reserved for %s module", k, Sonata.alias[k]))
  end
  rawset(t, k, v)
end,
})


-- Import actions
local function _doimport (tbl, name)
  local var = tbl[name]
  if not var then
    -- try alias
    var = name
    name = assert(Sonata.alias[name], "Wrong module name: "..name.."!")
  end
  if not Sonata._modules[var] then
    local lib = require("matlib."..name)
    Sonata._modules[var] = lib
    if not SONATA_PROTECT_ALIAS then _G[var] = lib end
    -- add description
    if lib.about then About:add(lib.about, name, var) end
  end
end


-- Add modules
setmetatable(use, { 
-- load modules
__call = function (self, name)
  if not name then
    local lst = _base.formatted {
      _base.FORMAT_V1,
      string.format("\n%-12s%-9s%s\n\n", "MODULE", "ALIAS", "USED"),
      _base.FORMAT_CLR}
    -- show loaded modules
    for k, v in pairs(use) do
      lst[#lst+1] = string.format("%-12s%-10s", k, v)
      if Sonata._modules[v] then
        lst[#lst+1] = _base.FORMAT_V1
        lst[#lst+1] = "++\n"
        lst[#lst+1] = _base.FORMAT_CLR
      else
        lst[#lst+1] = "\n"
      end
    end
    return Sonata.inLua and _base.simpleString(lst) or lst
  elseif name == "*" then
    -- load all modules
    for k, _ in  pairs(self) do _doimport(self, k) end
  elseif type(name) == "table" then
    -- load group of modules
    for i, v in ipairs(name) do _doimport(self, v) end
  else
    -- load module
    _doimport(self, name)
  end
end
})


--- Print SonataHelp information.
--  @param v Function, module or nil.
help = function(v)
  v = v or "main"
  local res = nil
  if v == "*" then
    res = About:makeFull(use, SONATA_LOCALIZATION)
  else
    res = About:findObject(v, use, SONATA_LOCALIZATION) or
          _base.formatted(SonataHelp.objectInfo(v))
  end
  return Sonata.inLua and _base.simpleString(res) or res
end


-- "Quick" evaluation or a user input
if arg[1] == "-e" then
  Sonata.inLua = true
  use("main")
  local ok, ans = _base.doEval(arg[2] or io.read())
  if ok then print(_base.simpleString(ans)) end
  os.exit()
end


-- Command line arguments of Sonata and their processing
local _args = {

-- Run tests
["--test"] = {
description = "Run unit tests.",
example = {
  "e.g. --test          # all tests",
  "     --test array    # tests for the given module"},
process = function (args)
  local Test = require("core.test")
  SONATA_PROTECT_ALIAS = false
  if args[2] then
    Test.module(
      string.format("%smatlib/%s.lua", (SONATA_ADD_PATH or ""), args[2]))
  else
    for m in pairs(use) do
      Test.module(string.format("%smatlib/%s.lua", (SONATA_ADD_PATH or ""), m))
    end
  end
  Test.summary()
end,
exit = true},

-- Localization file
["--lang"] = {
description = "Generation or updating of the localization file.",
example = {
  "e.g. --lang          # show current language",
  "     --lang eo       # file for Esperanto localization"},
process = function (args)
  if args[2] then
    local Gen = require("core.generator")
    Gen.lang(args[2], use)
  else
    io.write("Current localization file:\t", SONATA_LOCALIZATION or "en", "\n")
  end
end,
exit = true},

-- Generate 'help.html'
["--doc"] = {
description = "Generating a documentation page.",
example = {
  "e.g. --doc           # original documentation",
  "     --doc ru        # translated documentation"},
process = function (args)
  local Gen = require("core.generator")
  if args[2] == "md" then
    Gen.md(use)
  else
    if args[2] then SONATA_LOCALIZATION = args[2]..".lng" end
    Gen.doc(SONATA_LOCALIZATION, use)
  end
end,
exit = true},

-- New module
["--new"] = {
description = "Generating a template for a new module.",
example = {'e.g. --new  matrices  Mat  "Matrix operations."'},
process = function (args)
  local Gen = require("core.generator")
  Gen.module(args[2], args[3] or args[2], args[4] or args[2])
end,
exit = true},

-- Define method for input (and output)
["--io"] = {
description = "Method for interaction.",
example = {
  "e.g. --io tcp 23456  # server mode",
  "     --io w          # output window mode"},
process = function (args)
  if args[2] == "tcp" then
    local server = require("core.io_tcp")
    local port = assert(tonumber(args[3]), "Expected: --io tcp port")
    server:new("*", port)
    pcall(server.repl, server)
  elseif args[2] == "w" then
    -- expected Unix
    local fname = _base.pipeFile()
    os.execute("> " .. fname)  -- clear/create file
    os.execute("tail -F " .. fname)
  else
    print("Unknown option:", args[2])
  end
end,
exit = true},

-- Generate Lua binding
["--make"] = {
description = "Wrap code into Lua.",
example = {
  "e.g. --make mylib.h mylib.c"},
process = function (args)
  local gen = require("maker.cgen")
  local aa = {}; for i = 2, #args do aa[#aa+1] = args[i] end
  gen.makeBin(aa)
end,
exit = true},

-- ['-e'] defined outside

-- process files
["default"] = {
process = function (args)
  if SONATA_LOCALIZATION then
    About:localization(SONATA_LOCALIZATION)
  end
  local loop = require("core.io_terminal")
  loop.replFiles(args)
end,
exit = true},
}

-- show help
_args["-h"] = {
process = function () io.write(Sonata._arghelp(), "\n") end,
exit = true}

-- string representation of the help info
Sonata._arghelp = function ()
  local blank = ""
  local txt = {
    "\nSonata is a Lua program for mathematical calculations.",
    blank,
    "USAGE:",
    "\tlua [-i] sonata.lua [flag] [arg1 arg2 ...]",
    "(option '-i' can be used for working in native Lua interpreter)",
    blank,
    "FLAGS:",
    "\t-h\tGet this help message.",
    "\t-e\tEvaluate command line expression.",
    '\t\te.g. -e "2+2"',
    "\t-i\tContinue interaction after file evaluation.",
    "\t\te.g. -i file1.note file2.note",
  }
  for k, v in pairs(_args) do
    if v.description then
      txt[#txt+1] = string.format("\t%s\t%s", k, v.description)
      for i = 1, #v.example do
        txt[#txt+1] = string.format("\t\t%s", v.example[i])
      end
    end
  end
  txt[#txt+1] = "\t No flag  - Evaluate file(s)."
  txt[#txt+1] = string.format("\nVERSION: %d.%02d", Sonata.MAJOR_V, Sonata.MINOR_V)
  txt[#txt+1] = blank
  local modules = {}
  for k in pairs(use) do modules[#modules+1] = k end
  txt[#txt+1] = string.format("MODULES: %s.\n", table.concat(modules, ", "))
  txt[#txt+1] = "BUGS: mail to 'mikhel.sk@gmail.com'\n"
  return table.concat(txt, "\n")
end


--================== EXECUTION =================

-- Set default language
if SONATA_LOCALIZATION then
  About:localization(SONATA_LOCALIZATION)
end


-- Call default module
use("main")


-- Process command line arguments
if #arg > 0 then
  local command = _args[arg[1]]
  if not command then command = _args["default"] end
  command.process(arg)
  if command.exit then os.exit() end
end


-- Run!!!
io.write(SonataHelp.CMAIN, "\n", Sonata.getTitle(), "\n", SonataHelp.CHELP)
io.write(About:get("intro", SONATA_LOCALIZATION), SonataHelp.CRESET, "\n")


-- choose interpreter
if arg[-1] ~= "-i" then
  local loop = require("core.io_terminal")
  loop.repl() -- use Sonata REPL
else
  Sonata.inLua = true       -- use Lua REPL
end

--===============================================

