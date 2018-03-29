#!/usr/local/bin/lua -i
-- Lua based calculator 
-- This file is a part of 'liblc' collection, 2017-2018.

--================= CONFIGURATION ====================

--	Uncomment to set the localization file.
LC_LOCALIZATION = "ru.lng"

--	Text coloring.
LC_USE_COLOR = true

--	Load after start
--LC_DEFAULT_MODULES = {'matrix','numeric'}

--	Optional (for bash alias lc='path/to/calc.lua') 
--LC_ADD_PATH = path/to/?.lua

--====================  CODE  ========================

-- Version
lc_version = '0.8.2'

-- Add path to the libraries
if LC_ADD_PATH then
   package.path = package.path..';'..LC_ADD_PATH
end

-- Table for program variables. Import base functions. 
liblc = {main=require('liblc.main')}

-- Text colors 
lc_help.usecolors(LC_USE_COLOR) 

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
   set      = "Set",
   special  = "Spec",
   stat     = "Stat",
   struct   = "DS",
   units    = "Unit",
}
about[import] = {"import", ""}

-- Update help information about imported modules, 
function liblc.import_state_update()
   local m = {string.format("%-12s%-9s%s", "MODULE", "ALIAS", "LOADED")}
   for k,v in pairs(import) do
      m[#m+1] = string.format("%-13s%-10s%s", k, v, (_G[v] and 'v' or '-'))
   end
   m[#m+1] = about:get('use_import')
   return table.concat(m, '\n')
end

-- Import actions 
function liblc.doimport(tbl,name)
   local var = assert(tbl[name], 'Wrong module name: '..name..'!')
   if not _G[var] then
      _G[var] = require('liblc.'..name)
      about:add(_G[var].about, var)
      if _G[var].onimport then _G[var].onimport() end
   end
   return var
end

-- Add modules 
setmetatable(import, 
{ __tostring = function (x) io.write(lc_help.CHELP); return about:get('done') end,
  __call = function (self, name) 
    if name == 'all' then 
       for k,v in pairs(self) do liblc.doimport(self,k) end
    elseif type(name) == 'table' then
       for _,v in ipairs(name) do import(v) end
    else
       local var = liblc.doimport(self,name)
       io.write(lc_help.CHELP)
       print(string.format(about:get('alias'), lc_help.CBOLD..var..lc_help.CNBOLD, name))
    end
    about[import][2] = liblc.import_state_update()
    return import
  end,
})

-- Check arguments 
local arg1 = arg[1]
if #arg > 0 then
   -- test modules
   if arg1 == '-t' or arg1 == '--test' then
      local Test = require 'liblc.test'
      if arg[2] then
         Test.module(string.format('liblc/%s.lua',arg[2]))
      else
         for m in pairs(import) do
	    Test.module(string.format('liblc/%s.lua',m))
	 end
      end
      Test.summary()
   -- calculate
   elseif arg1 == '-e' or arg1 == '--eval' then
      local tmp = table.move(arg,2,#arg,1,{})
      liblc.main.evalstr(table.concat(tmp,' '))
   -- update localization file
   elseif arg1 == '-l' or arg1 == '--lang' then
      if arg[2] then
	 lc_help.prepare(arg[2], import)
      else 
         print('Current localization file: ', LC_LOCALIZATION)
      end
   -- prepare new module
   elseif arg1 == '-n' or arg1 == '--new' then
      lc_help.newmodule(arg[2],arg[3],arg[4])
   -- execute all the files from the argument list
   else
      for i = 1,#arg do dofile(arg[i]) end
   end
   os.exit()
end

-- Read localization file and update descriptions. 
if LC_LOCALIZATION then 
   about:localization(LC_LOCALIZATION) 
end
about[import][2] = liblc.import_state_update()

-- Run! 
io.write(lc_help.CMAIN)
print("\n           --==== LuaCalculus "..lc_version.." ====--\n")
io.write(lc_help.CHELP)
print(about:get('intro'))

_PROMPT = lc_help.CMAIN..'lc:'..lc_help.CRESET..' '
_PROMPT2= lc_help.CMAIN..'..:'..lc_help.CRESET..' '

-- Import default modules
if LC_DEFAULT_MODULES then
   import(LC_DEFAULT_MODULES)  
end

--===============================================
-- TODO: use alias or part of the name for import
-- TODO: rename program, for example, 'Balu LC', 'Sigma LC', 'Sonata LC'...
-- TODO: add step-by-step execution
-- TODO: optimize for LuaJIT or write C objects
-- TODO: add 'evaluatable' expressions in text files
