#!/usr/local/bin/lua -i
-- Lua based calculator
-- This file is a part of 'liblc' collection, 2017-2018.
lc_version = '0.7.2'

-- Uncomment to set the localisation file.
-- LOCALISATION_FILE = "ru.lng"

-- Optional (for bash alias lc='~/.lc/calc.lua')
-- package.path = package.path .. ';./.lc/?.lua'

-- Table for program variables
liblc = {}

-- base functionality
liblc.main = require 'liblc.main'
-- Quick exit
quit = function () print("\n              --======= Buy! =======--\n"); os.exit() end

-- modules
import = {
   array    = "Arr",
   bigint   = "Big",
   complex  = "Cmp",
   const    = "_C",
   gnuplot  = "Gnu",
   matrix   = "Mat",
   numeric  = "Num",
   polynom  = "Poly",
   rational = "Rat",
   set      = "Set",
   special  = "Spec",
   stat     = "Stat",
   units    = "Unit",
}
about[import] = {"import", ""}

-- update help information about imported modules
function liblc.import_state_update()
   local m = {string.format("%-12s%-9s%s", "MODULE", "ALIAS", "LOADED")}
   for k,v in pairs(import) do
      m[#m+1] = string.format("%-13s%-10s%s", k, v, (_G[v] and 'v' or '-'))
   end
   m[#m+1] = about:get('use_import')
   return table.concat(m, '\n')
end

-- import actions
function liblc.doimport(tbl,name)
   local var = assert(tbl[name], 'Wrong module name: '..name..'!')
   if not _G[var] then
      _G[var] = require('liblc.'..name)
      about:add(_G[var].about, var)
      if _G[var].onimport then _G[var].onimport() end
   end
   return var
end

-- add modules
setmetatable(import, 
{ __tostring = function (x) return about:get('done') end,
  __call = function (self, name) 
    if name == 'all' then 
       for k,v in pairs(self) do liblc.doimport(self,k) end
    else
       local var = liblc.doimport(self,name)
       print(string.format(about:get('alias'), var, name))
    end
    about[import][2] = liblc.import_state_update()
    return import
  end,
})

-- check arguments
if #arg > 0 then
   -- test modules
   if arg[1] == '-test' then
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
   elseif arg[1] == '-e' or arg[1] == '-eval' then
      local tmp = table.move(arg,2,#arg,1,{})
      liblc.main.evalstr(table.concat(tmp,' '))
   -- update localisation file
   elseif arg[1] == '-lang' then
      if arg[2] then
	 mhelp.prepare(arg[2], import)
      else 
         print('Current localization file: ', LOCALISATION_FILE)
      end
   -- prepare new module
   elseif arg[1] == '-new' then
      mhelp.newmodule(arg[2],arg[3])
   -- execute all the files from the argument list
   else
      for i = 1,#arg do dofile(arg[i]) end
   end
   os.exit()
end

-- read localisation file and update descriptions
if LOCALISATION_FILE then 
   about:localisation(LOCALISATION_FILE) 
end
about[import][2] = liblc.import_state_update()

-- Run!
print("\n           --==== LuaCalculus "..lc_version.." ====--\n")
print(about:get('intro'))

_PROMPT='lc: '
_PROMPT2='..: '

--===============================================
-- TODO: use alias or part of the name for import

