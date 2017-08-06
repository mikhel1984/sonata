#!/usr/local/bin/lua -i

-- Lua based calculator
lc_version = '0.5.4'

--LOCALISATION_FILE = "locale/lng.ru"

-- help
help = require "liblc.help"
about = help:new("Lua based calculator")

-- Common
abs = math.abs;    about[abs] = {"abs(x)", "Absolut value.", help.BASE}
exp = math.exp;    about[exp] = {"exp(x)", "Exponenta.", help.BASE}
ln = math.log;     about[ln] = {"ln(x)", "Natural logarithm.", help.BASE}
lg = math.log10;   about[lg] = {"lg(x)", "Decimal logarithm.", help.BASE}
pow = math.pow;    about[pow] = {"pow(a,b)", "Return a^b.", help.BASE}
sqrt = math.sqrt;  about[sqrt] = {"sqrt(a)", "Square root.", help.BASE}
max = math.max;    about[max] = {"max(...)", "Maximum number.", help.BASE}
min = math.min;    about[min] = {"min(...)", "Minimum number.", help.BASE}
-- Trigonometrical
sin = math.sin;    about[sin] = {"sin(x)", "Sinus x.", help.TRIG}
cos = math.cos;    about[cos] = {"cos(x)", "Cosinus x.", help.TRIG}
tan = math.tan;    about[tan] = {"tan(x)", "Tangent x.", help.TRIG}
asin = math.asin;  about[asin] = {"asin(x)", "Arcsinus x.", help.TRIG}
acos = math.acos;  about[acos] = {"acos(x)", "Arccosinus x.", help.TRIG}
atan = math.atan;  about[atan] = {"atan(y[,x])", "Arctangent y. In case of 2 parameters calculate y/x with signs.", help.TRIG}
-- Hyperbolic
ch = math.cosh;    about[ch] = {"ch(x)", "Hyperbolic cosinus.", help.HYP}
sh = math.sinh;    about[sh] = {"sh(x)", "Hyperbolic sinus.", help.HYP}
th = math.tanh;    about[th] = {"th(x)", "Hyperbolic tangent.", help.HYP}
-- Angles 
deg = math.deg;    about[deg] = {"deg(x)", "Radians to degrees.", help.BASE}
rad = math.rad;    about[rad] = {"rad(x)", "Degrees to radians.", help.BASE}
-- Constants
_pi = math.pi;     about[_pi] = {"_pi", "Number pi", help.CONST}
_e = math.exp(1)   about[_e] = {"_e", "Euler number", help.CONST}
--_i = nil         -- import 'complex' to use it
-- Quick exit
quit = function () print("\n                --==== Buy! ====--\n"); os.exit() end

-- modules
import = {
   array    = "Arr",
   bigint   = "Big",
   complex  = "Cmp",
   gnuplot  = "Gnu",
   matrix   = "Mat",
   numeric  = "Num",
   polynom  = "Poly",
   rational = "Rat",
   set      = "Set",
   stat     = "Stat",
   units    = "Unit",
}
about[import] = {"import", "", help.BASE}

function import_state_update()
   local m = {string.format("%-12s%-6s%s", "MODULE", "ALIAS", "LOADED")}
   for k,v in pairs(import) do
      m[#m+1] = string.format("%-13s%-7s%s", k, v, (_G[v] and 'v' or '-'))
   end
   m[#m+1] = about:get('use_import')
   return table.concat(m, '\n')
end

-- add modules
setmetatable(import, 
{ __tostring = function (x) return about:get('done') end,
  __call = function (self, name) 
   local var = assert(self[name], "Wrong module name!")
   if not _G[var] then
      _G[var] = require('liblc.'..name)
      about:add(_G[var].about, var)
      if _G[var].onimport then _G[var].onimport() end
   end
   print(string.format(about:get('alias'), var, name))
   about[import][2] = import_state_update()
   return import
end })

-- Additional functions --
function rand() return math.random() end
about[rand] = {"rand()", "Random number between 0 and 1.", help.BASE}
-- hyperbolic arcsinus
function ash(x)
   return math.log(x+math.sqrt(x*x+1))
end
about[ash] = {"ash(x)", "Hyperbolic arcsinus.", help.HYP}
-- hyperbolic arccosinus
function ach(x)
   return math.log(x+math.sqrt(x*x-1))
end
about[ach] = {"ach(x)", "Hyperbolic arccosinus.", help.HYP}
-- hyperbolic arctangenth
function ath(x)
   return 0.5*math.log((1+x)/(1-x))
end
about[ath] = {"ath(x)", "Hyperbolic arctangent.", help.HYP}

-- create function f(x) from string
function fx(str)
   return assert(load("return function (x) return " .. str .. " end"))()
end
about[fx] = {"fx(str)", "Create Lua function f(x) from string.", help.OTHER}

-- plot string function
function plot(str, a, b)
   assert(type(str) == 'string', 'Expected string expression!')  
   -- prepare command 
   local graph = 'gnuplot -p -e "f(x)= ' .. str .. '; plot'
   if a and type(a) == 'number' and b and type(b) == 'number' then
      graph = graph .. ' [' .. a .. ':' .. b .. ']'
   end
   graph = graph .. ' f(x)"'   
   os.execute(graph)   
end
about[plot] = {"plot(str[,a,b])", "Quick plot function in Gnuplot. Use bounds if they are defined.  Variable must be 'x'.", help.OTHER}

-- calculate function for range of values
function eval(fn, x1, xn, step)
   xn = xn or x1
   step = step or 1
   for k = x1, xn, step do print("x="..k.."\tres="..fn(k)) end
end
about[eval] = {"eval(fn,x1[,xn[,step]])", "Evalueate function for given value or interval and print result.", help.OTHER}

-- read localisation file and update descriptions
if LOCALISATION_FILE then about:localisation(LOCALISATION_FILE) end

about[import][2] = import_state_update()

-- Print help information
function help(fn)   
   if fn then 
      about:print(type(fn)=='table' and fn.about or fn) 
   else
      about:print(about)
      print("\t" .. about:get('modules'))
      local t = {}; for k in pairs(import) do t[#t+1] = k end
      print(table.concat(t, ', '))
   end
end

-- read object from its serialization
function deserialize(obj_str)
   local f = assert(load("return " .. obj_str)) 
   local o = f()
   assert(_G[o.metatablename], "Module '" .. o.modulename .. "' is required")
   setmetatable(o, _G[o.metatablename])
   o.modulename = nil; o.metatablename = nil
   return o
end

-- check arguments
if #arg > 0 then
   if arg[1] == '-test' then
      Test = require 'liblc.test'
      if arg[2] then
         Test.module(string.format('liblc/%s.lua',arg[2]))
      else
         for m in pairs(import) do
	    Test.module(string.format('liblc/%s.lua',m))
	 end
      end
      Test.summary()
      os.exit()
   end
end

-- Run!
print("\n             --==== LuaCalc "..lc_version.." ====--\n")
print(about:get('intro'))

_PROMPT='lc: '
_PROMPT2='..: '
