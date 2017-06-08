#!/usr/local/bin/lua -i

-- Lua based calculator

-- help
help = require "help"
about = help:new("Lua based calculator")
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
quit = os.exit

-- modules
Rat = nil        -- rational
Cmp = nil        -- complex
Big = nil        -- bigint
Mat = nil        -- matrix
Poly = nil       -- polynom
Set = nil        -- set

local MODULE_LIST = "complex, rational, bigint, matrix, polynom, set"

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

-- Additional modules
function import(modname)   
   if modname == "rational" then
      if not Rat then
         Rat = require(modname); about:add(Rat.about, "Rat")                
      end
   elseif modname == "complex" then
      if not Cmp then
         Cmp = require(modname); about:add(Cmp.about, "Cmp")
         _i = Cmp._i;    print("- add imaginary unit '_i'")
      end
   elseif modname == "bigint" then
      if not Big then
         Big = require(modname); about:add(Big.about, "Big")         
      end
   elseif modname == 'matrix' then
      if not Mat then
         Mat = require(modname); about:add(Mat.about, "Mat")
      end
   elseif modname == 'polynom' then
      if not Poly then
         Poly = require(modname); about:add(Poly.about, "Poly")
      end
   elseif modname == 'set' then
      if not Set then
         Set = require(modname); about:add(Set.about, "Set")
      end
   else
      print("No such module: " .. modname); return
   end
   print("Module '" .. modname .. "' is imported")
end

-- Print help information
function help(fn)   
   if fn then 
      about:print(type(fn)=='table' and fn.about or fn) 
   else
      about:print(about)
      print("\tAvailable modules:\n" .. MODULE_LIST .. "\nUse 'import name' to load it.")
   end
end

-- Run!
print("\nLuaCalc ver 0.2.")
print("Print 'quit()' for exit!")
print("Print 'help([fn])' to get help.\n")

_PROMPT='lc: '
_PROMPT2='..: '
