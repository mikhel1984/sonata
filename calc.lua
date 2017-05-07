#!/usr/local/bin/lua -i

-- Lua based calculator

-- Common
abs = math.abs
exp = math.exp
ln = math.log
lg = math.log10
pow = math.pow
sqr = math.sqrt
max = math.max
min = math.min
-- Trigonometrical
sin = math.sin
cos = math.cos
tan = math.tan
asin = math.asin
acos = math.acos
atan = math.atan
-- Hyperbolic
ch = math.cosh
sh = math.sinh
th = math.tanh
-- Angles 
deg = math.deg
rad = math.rad
-- Constants
_pi = math.pi
_e = math.exp(1)
_i = nil         -- import 'complex' to use it
EPS = 0.0001
-- Quick exit
quit = os.exit

-- help
about = {}

-- modules
Rat = nil        -- rational
Cmp = nil        -- complex

-- Additional functions --
function rand() return math.random() end
-- hyperbolic arcsinus
function ash(x)
   return math.log(x+math.sqrt(x*x+1))
end
-- hyperbolic arccosinus
function ach(x)
   return math.log(x+math.sqrt(x*x-1))
end
-- hyperbolic arctangenth
function ath(x)
   return 0.5*math.log((1+x)/(1-x))
end

-- create function f(x) from string
function fcreate(str)
   return assert(loadstring("return function (x) return " .. str .. " end"))()
end
about[fcreate] = [[
   : fcreate(str)
Create Lua function from string.
]]

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
about[fsolve] = [[
   : fsolve(function, left_bound, right_bound)
Find the root of a function at given interval.
]]

-- find root of string defined function
function solve(str, a, b)
   return fsolve(fcreate(str), a, b)
end
about[solve] = [[
   : solve(string, left_bound, right_bound)
Find the root of function (represented as string) at given interval.
]]

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
about[plot] = [[
   : plot(string [,a,b])
Plot function in Gnuplot. Use bounds if they are defined. 
Variable must be 'x'.
]]

about.list = [[
   : Base 
abs(x), exp(x), ln(x), lg(x), pow(x), sqrt(x), rand(), 
max(...), min(...), deg(x), rad(x)
   : Trigonometrical 
sin(x), cos(x), tan(x), asin(x), acos(x), atan(x)
   : Hyperbolic 
sh(x), ch(x), th(x), ash(x), ach(x), ath(x)
   : Solving equation 
fsolve(f, a, b), solve(str, a, b)
   : Other 
plot(str [,a,b]), fcreate(str)
   : Modules for 'lc'
import(modname)
'rational', 'complex'   
   : Constants
_pi, _e
]]

-- Print help information
function help(fn)
   if fn == nil then
      print(about.list)
   else
      description = about[fn]
      print(description and description or "Not defined")
   end
end

-- Additional modules
function import(modname)   
   if modname == "rational" then
      if not Rat then
         Rat = require(modname)
         for k,v in pairs(Rat.about) do about[k] = v end 
         print("Use 'Rat' to work with rational numbers")       
      end
   elseif modname == "complex" then
      if not Cmp then
         Cmp = require(modname)
         for k, v in pairs(Cmp.about) do about[k] = v end
         _i = Cmp._i;    print("- add imaginary unit '_i'")
         sqr = Cmp.sqrt; print("- redefine sqr(x) for complex numbers")
         print("Use 'Cmp' to work with complex numbers")
      end
   else
      print("No such module: " .. modname)
      return
   end
   print("Module '" .. modname .. "' is imported")
end
about[import] = [[
   : import modname
Load module with given name.
]]

-- Run!
print("\nPrint 'quit()' for exit!")
print("Print 'help([fn])' to get help.\n")

_PROMPT='lc: '
_PROMPT2='..: '
