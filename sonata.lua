#!/usr/local/bin/lua -i
-- Lua based calculator 
-- This file is a part of 'sonatalib' collection, 2017-2018.

--================= CONFIGURATION ====================

--	Uncomment to set the localization file
--LC_LOCALIZATION = "ru.lng"

--	Text coloring
LC_USE_COLOR = true

--	Load after start (optional)
--LC_DEFAULT_MODULES = {'matrix','numeric'}

--	Path (optional, for bash alias lc='path/to/sonata.lua') 
--LC_ADD_PATH = path/to/?.lua

--=====================  CODE  ========================

-- Version
lc_version = '0.9.7'

-- Add path to the libraries
if LC_ADD_PATH then
   package.path = package.path..';'..LC_ADD_PATH
end

-- Table for program variables. Import base functions 
lc = require('sonatalib.main')
lc_local = {}

-- Text colors 
lc_help.useColors(LC_USE_COLOR) 

-- Quick exit 
quit = function () print(lc_help.CMAIN.."\n              --======= Buy! =======--\n"..lc_help.CRESET); os.exit() end

-- Modules 
import = {
--   name     alias
   array    = "Arr",
   bigint   = "Big",
   complex  = "Comp",
   const    = "_C",
   files    = "File",
   gnuplot  = "Gnu",
   graph    = "Graph",
   matrix   = "Mat",
   numeric  = "Num",
   polynom  = "Poly",
   rational = "Rat",
   special  = "Spec",
   stat     = "Stat",
   struct   = "DS",
   units    = "Unit",
}
about[import] = {"import", ""}

-- Update help information about imported modules 
function lc_local.import_state_update()
   local m = {string.format("%-12s%-9s%s", "MODULE", "ALIAS", "LOADED")}
   for k,v in pairs(import) do
      m[#m+1] = string.format("%-13s%-10s%s", k, v, (_G[v] and 'v' or '-'))
   end
   m[#m+1] = about:get('use_import')
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
      _G[var] = require('sonatalib.'..name)
      about:add(_G[var].about, var)
      if _G[var].onImport then _G[var].onImport() end
   end
   return var, name
end

-- Add modules 
setmetatable(import, 
{ __tostring = function (x) io.write(lc_help.CHELP); return about:get('done') end,
  __call = function (self, name) 
    if name == 'all' then 
       for k,v in pairs(self) do lc_local.doimport(self,k) end
    elseif type(name) == 'table' then
       for _,v in ipairs(name) do import(v) end
    else
       local var, nm = lc_local.doimport(self,name)
       if lc_local.dialog then
          io.write(lc_help.CHELP)
          print(string.format(about:get('alias'), lc_help.CBOLD..var..lc_help.CNBOLD, nm))
       end
    end
    about[import][2] = lc_local.import_state_update()
    return import
  end,
})

-- Process command line arguments
if #arg > 0 then
   local command = lc._args[arg[1]]
   if type(command) == 'string' then command = lc._args[command] end
   if not command then command = lc._args['no flags'] end
   command.process(arg)
   if command.exit then os.exit() end
end

-- Show additional information
lc_local.dialog = true

-- Read localization file and update descriptions 
if LC_LOCALIZATION then 
   about:localization(LC_LOCALIZATION) 
end
about[import][2] = lc_local.import_state_update()

-- Run! 
io.write(lc_help.CMAIN)
print("\n   # #       --===== Sonata LC =====--       # #\n    # #         --==== "..lc_version.." ====--         # #\n")
io.write(lc_help.CHELP)
print(about:get('intro'))

_PROMPT = lc_help.CMAIN..'lc:'..lc_help.CRESET..' '
_PROMPT2= lc_help.CMAIN..'..:'..lc_help.CRESET..' '

-- Import default modules
if LC_DEFAULT_MODULES then
   import(LC_DEFAULT_MODULES)  
end

--===============================================
-- TODO: rename functions to matlab style
