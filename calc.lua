#!/usr/local/bin/lua -i

-- Lua based calculator

-- help
help = require "liblc.help"
about = help:new("Lua based calculator")
about:localisation("lng.ru")
--about[about] = {link=about, "Lua based calculator"}

-- Common
abs = math.abs;    about[abs] = {"abs(x)", "Absolut value", help.BASE}
exp = math.exp;    about[exp] = {"exp(x)", "Exponenta", help.BASE}
ln = math.log;     about[ln] = {"ln(x)", "Natural logarithm", help.BASE}
lg = math.log10;   about[lg] = {"lg(x)", "Decimal logarithm", help.BASE}
pow = math.pow;    about[pow] = {"pow(a,b)", "Return a^b", help.BASE}
sqrt = math.sqrt;  about[sqrt] = {"sqrt(a)", "Square root", help.BASE}
max = math.max;    about[max] = {"max(...)", "Maximum number", help.BASE}
min = math.min;    about[min] = {"min(...)", "Minimum number", help.BASE}
-- Trigonometrical
sin = math.sin;    about[sin] = {"sin(x)", "Sinus", help.TRIG}
cos = math.cos;    about[cos] = {"cos(x)", "Cosinus", help.TRIG}
tan = math.tan;    about[tan] = {"tan(x)", "Tangent", help.TRIG}
asin = math.asin;  about[asin] = {"asin(x)", "Arcsinus", help.TRIG}
acos = math.acos;  about[acos] = {"acos(x)", "Arccosinus", help.TRIG}
atan = math.atan;  about[atan] = {"atan(x)", "Arctangent", help.TRIG}
-- Hyperbolic
ch = math.cosh;    about[ch] = {"ch(x)", "Hyperbolic cosinus", help.HYP}
sh = math.sinh;    about[sh] = {"sh(x)", "Hyperbolic sinus", help.HYP}
th = math.tanh;    about[th] = {"th(x)", "Hyperbolic tangent", help.HYP}
-- Angles 
deg = math.deg;    about[deg] = {"deg(x)", "Radians to degrees.", help.BASE}
rad = math.rad;    about[rad] = {"rad(x)", "Degrees to radians.", help.BASE}
-- Constants
_pi = math.pi;     about[_pi] = {"_pi", "Number pi", help.CONST}
_e = math.exp(1)   about[_e] = {"_e", "Euler number", help.CONST}
_i = nil         -- import 'complex' to use it
EPS = 0.0001;      about[EPS] = {"EPS", "Value of tolerance for solving equations.", help.CONST}
-- Quick exit
quit = function () print("\n                --==== Buy! ====--\n"); os.exit() end

-- modules
import = {
   rational = "Rat",
   complex  = "Cmp",
   bigint   = "Big",
   matrix   = "Mat",
   polynom  = "Poly",
   set      = "Set",
   gnuplot  = "Gnu",
}
about[import] = {"import 'module_name'", "", help.BASE}
function import_state_update()
   local m = {string.format("%-12s%-6s%s", "Module", "Alias", "Loaded")}
   for k,v in pairs(import) do
      m[#m+1] = string.format("%-13s%-7s%s", k, v, (_G[v] and '+' or '-'))
   end
   return table.concat(m, '\n')
end
about[import][2] = import_state_update()
-- add modules
setmetatable(import, 
{ __tostring = function (x) return "Done" end,
  __call = function (self, name) 
   local var = assert(self[name], "Wrong module name!")
   if not _G[var] then
      _G[var] = require('liblc.'..name)
      about:add(_G[var].about, var)
   end
   print(string.format("Use alias '%s' for access to the module '%s'", var, name))
   about[import][2] = import_state_update()
   return import
end })

-- Additional functions --
function rand() return math.random() end
about[rand] = {"rand()", "Random number between 0 and 1", help.BASE}
-- hyperbolic arcsinus
function ash(x)
   return math.log(x+math.sqrt(x*x+1))
end
about[ash] = {"ash(x)", "Hyperbolic arcsinus", help.HYP}
-- hyperbolic arccosinus
function ach(x)
   return math.log(x+math.sqrt(x*x-1))
end
about[ach] = {"ach(x)", "Hyperbolic arccosinus", help.HYP}
-- hyperbolic arctangenth
function ath(x)
   return 0.5*math.log((1+x)/(1-x))
end
about[ath] = {"ath(x)", "Hyperbolic arctangent", help.HYP}

-- create function f(x) from string
function fcreate(str)
   return assert(loadstring("return function (x) return " .. str .. " end"))()
end
about[fcreate] = {"fcreate(str)", "Create Lua function from string", help.BASE}

-- find root of f(x)
function fsolve(fn, a, b)
   fn0 = fn(a)
   fn1 = fn(b)
   assert(fn0*fn1 < 0, "Boundary values must have different sign")   
   repeat 
      b = b-fn1/(fn1-fn0)*(b-a)
      fn1 = fn(b)
   until math.abs(fn1) < EPS
   return b
end 
about[fsolve] = {"fsolve(fn, l_bound, r_bound)", "Find the root of a function at given interval", help.OTHER}

-- find root of string defined function
function solve(str, a, b)
   return fsolve(fcreate(str), a, b)
end
about[solve] = {"solve(str, l_bound, r_bound)", " Find the root of function (represented as string) at given interval", help.OTHER}

-- plot string function
function plot(str, a, b)
   assert(type(str) == 'string', 'Expected string expression!')  
   -- prepare command 
   graph = 'gnuplot -p -e "f(x)= ' .. str .. '; plot'
   if a and type(a) == 'number' and b and type(b) == 'number' then
      graph = graph .. ' [' .. a .. ':' .. b .. ']'
   end
   graph = graph .. ' f(x)"'   
   os.execute(graph)   
end
about[plot] = {"plot(str [,a,b])", " Plot function in Gnuplot. Use bounds if they are defined.  Variable must be 'x'.", help.OTHER}

-- Print help information
function help(fn)   
   if fn then 
      about:print(type(fn)=='table' and fn.about or fn) 
   else
      about:print(about)
      print("\t" .. about:modules())
      local t = {}
      for k in pairs(import) do t[#t+1] = k end
      print(table.concat(t, ', ') .. '.')
   end
end

-- Run!
print("\n             --==== LuaCalc 0.4 ====--\n")
print(about:intro())

_PROMPT='lc: '
_PROMPT2='..: '
