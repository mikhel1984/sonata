#!/usr/local/bin/lua
-- Lua based calculator 
-- This file is a part of 'sonatalib' collection, 2017-2019.

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

-- Version
lc_version = '0.9.16'

-- Add path to the libraries
if LC_ADD_PATH then
   package.path = string.format("%s;%s?.lua", package.path, LC_ADD_PATH)
end

-- Table for program variables. Import base functions 
lc = require('sonatalib.main')
-- Environment
lc_local = {}

-- Text colors 
lc_help.useColors(LC_USE_COLOR) 

-- Quit the program
quit = lc._exit_

-- Load file
run = dofile

-- Update random seed
math.randomseed(os.time())

-- Modules 
import = {
--   name       alias
   array      = "Arr",
   bigint     = "Big",
   complex    = "Comp",
   const      = "_C",
   files      = "File",
   gnuplot    = "Gnu",
   graph      = "Graph",
   matrix     = "Mat",
   numeric    = "Num",
   polynom    = "Poly",
   quaternion = "Quat",
   rational   = "Rat",
   special    = "Spec",
   stat       = "Stat",
   struct     = "DS",
   units      = "Unit",
}

-- Update help information about imported modules 
function lc_local.import_state_update()
   local m = {lc_help.CHELP, string.format("%-12s%-9s%s", "MODULE", "ALIAS", "LOADED")}
   for k,v in pairs(import) do
      m[#m+1] = string.format("%-13s%-10s%s", k, v, (_G[v] and 'v' or '-'))
   end
   m[#m+1] = about:get('use_import')
   m[#m+1] = lc_help.CRESET
   return table.concat(m, '\n')
end

-- Import actions 
function lc_local.doimport(tbl,name)
   local var = tbl[name]
   if not var then
      if not lc_local.alias then 
         lc_local.alias = {}
	 for k,v in pairs(import) do lc_local.alias[v] = k end
      end
      var = name
      name = assert(lc_local.alias[name], "Wrong module name: "..name.."!")
   end
   if not _G[var] then
      local lib = require('sonatalib.'..name)
      _G[var] = lib
      -- add description if need
      if lib.about then about:add(lib.about, var) end
      -- do additional actions
      if lib.onImport then lib.onImport() end
   end
   return var, name
end

-- Add modules 
setmetatable(import, 
{ -- last recursive call 
  __tostring = function (x) io.write(lc_help.CHELP); return about:get('done')..lc_help.CRESET end,
  -- load modules
  __call = function (self, name) 
    if not name then
       print(lc_local.import_state_update())
    elseif name == 'all' then 
       for k,v in pairs(self) do lc_local.doimport(self,k) end    
    elseif type(name) == 'table' then
       for _,v in ipairs(name) do import(v) end
    else
       local var, nm = lc_local.doimport(self,name)
       if LC_DIALOG then
          io.write(lc_help.CHELP)
          print(string.format(about:get('alias'), lc_help.CBOLD..var..lc_help.CNBOLD, nm))          
       end       
    end
    return import
  end,
})

--- Print lc_help information.
--  @param fn Function name.
help = function(fn)   
   if fn then 
      if fn == import then
         print(lc_local.import_state_update())
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


-- save references for "global" methods
lc_local.import = import
lc_local.help = help            
lc_local.quit = quit

-- Run! 
io.write(lc_help.CMAIN)
print("\n   # #       --===== Sonata LC =====--       # #\n    # #         --==== "..lc_version.." ====--        # #\n")
io.write(lc_help.CHELP)
print(about:get('intro'), lc_help.CRESET)

-- Import default modules
if LC_DEFAULT_MODULES then
   import(LC_DEFAULT_MODULES)  
end

if arg[-1] ~= '-i' then
   lc.evalDialog(lc._logFile_)
end

--===============================================
