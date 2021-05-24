#!/usr/local/bin/lua
-- Lua based calculator 
-- This file is a part of 'sonatalib' collection, 2021.

--================= CONFIGURATION ====================

--	Uncomment to set the localization file
--LC_LOCALIZATION = "ru.lng"

--	Text coloring
--LC_USE_COLOR = true

--	Load after start (optional)
--LC_DEFAULT_MODULES = {'matrix','numeric'}

--	Path (optional, for bash alias lc='path/to/sonata.lua')
--LC_ADD_PATH = path/to/dir/

--=====================  CODE  ========================

-- Environment
lc_local = { version = '0.9.19' }

-- Add path to the libraries
if LC_ADD_PATH then
  package.path = string.format("%s;%s?.lua", package.path, LC_ADD_PATH)
end

-- Table for program variables. Import base functions 
lc = require('sonatalib.main')

-- Text colors 
lc_help.useColors(LC_USE_COLOR) 

-- Quit the program
quit = lc._exit_

-- Update random seed
math.randomseed(os.time())

-- Modules 
use = {
--  name     alias
  array     = "Arr",
  bigint    = "Big",
  complex   = "Comp",
  const     = "_C",
  files     = "File",
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
function lc_local.doimport(tbl,name)
  local var = tbl[name]
  if not var then
    -- try alias
    if not lc_local.alias then 
      lc_local.alias = {}
      for k,v in pairs(use) do lc_local.alias[v] = k end
    end
    var = name
    name = assert(lc_local.alias[name], "Wrong module name: "..name.."!")
  end
  if not _G[var] then
    local lib = require('sonatalib.'..name)
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
     io.write('\n', lc_help.CHELP, string.format("%-12s%-9s%s", "MODULE", "ALIAS", "USED"), '\n')
     for k,v in pairs(use) do
       io.write(string.format("%-13s%-10s%s", k, v, (_G[v] and 'v' or '-')),'\n')
     end
     io.write(about:get('use_import'), lc_help.CRESET, '\n\n')
   elseif name == 'all' then
     -- load all modules
     for k,v in pairs(self) do lc_local.doimport(self,k) end
   elseif type(name) == 'table' then
     -- load group of modules
     for _,v in ipairs(name) do use(v) end
   else
     -- load module
     local var, nm = lc_local.doimport(self,name)
     if LC_DIALOG then
       io.write(lc_help.CHELP)
       print(string.format(about:get('alias'), lc_help.CBOLD..var..lc_help.CNBOLD, nm))
     end
   end
  end,
})

--- Print lc_help information.
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
  io.write(lc_help.CRESET)
end

-- Process command line arguments
if #arg > 0 then
  local command = lc._args_[arg[1]]
  if type(command) == 'string' then command = lc._args_[command] end
  if not command then command = lc._args_['default'] end
  command.process(arg)
  if command.exit then os.exit() end
end

-- Prepare for dialog mode
LC_DIALOG = true

-- Read localization file and update descriptions 
if LC_LOCALIZATION then 
  about:localization(LC_LOCALIZATION) 
end

-- Run! 
io.write(lc_help.CMAIN, '\n',
"   # #       --===== Sonata LC =====--       # #\n",
"    # #        --==== ", lc_local.version, " ====--        # #\n\n",
lc_help.CHELP)
print(about:get('intro'), lc_help.CRESET)

-- Import default modules
if LC_DEFAULT_MODULES then
  use(LC_DEFAULT_MODULES)
end

if arg[-1] ~= '-i' then
  lc.evalDialog()
end

--===============================================
