--[[      liblc/main.lua 

--- Define aliases for standard operations and add some new common functions.
--  @author Stanislav Mikhel, 2017
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection.

            module 'main'
--]]

local main = {}

-- mhelp
mhelp = require "liblc.help"
about = mhelp:new("Lua based calculator.")

-- Common
abs = math.abs;    about[abs] = {"abs(x)", "Absolut value.", mhelp.BASE}
exp = math.exp;    about[exp] = {"exp(x)", "Exponenta.", mhelp.BASE}
ln = math.log;     about[ln] = {"ln(x)", "Natural logarithm.", mhelp.BASE}
lg = math.log10;   about[lg] = {"lg(x)", "Decimal logarithm.", mhelp.BASE}
pow = math.pow;    about[pow] = {"pow(a,b)", "Return a^b.", mhelp.BASE}
sqrt = math.sqrt;  about[sqrt] = {"sqrt(a)", "Square root.", mhelp.BASE}
max = math.max;    about[max] = {"max(...)", "Maximum number.", mhelp.BASE}
min = math.min;    about[min] = {"min(...)", "Minimum number.", mhelp.BASE}
-- Trigonometrical
sin = math.sin;    about[sin] = {"sin(x)", "Sinus x.", mhelp.TRIG}
cos = math.cos;    about[cos] = {"cos(x)", "Cosinus x.", mhelp.TRIG}
tan = math.tan;    about[tan] = {"tan(x)", "Tangent x.", mhelp.TRIG}
asin = math.asin;  about[asin] = {"asin(x)", "Arcsinus x.", mhelp.TRIG}
acos = math.acos;  about[acos] = {"acos(x)", "Arccosinus x.", mhelp.TRIG}
atan = math.atan;  about[atan] = {"atan(y[,x])", "Arctangent y. In case of 2 parameters calculate y/x with signs.", mhelp.TRIG}
-- Hyperbolic
ch = math.cosh;    about[ch] = {"ch(x)", "Hyperbolic cosinus.", mhelp.HYP}
sh = math.sinh;    about[sh] = {"sh(x)", "Hyperbolic sinus.", mhelp.HYP}
th = math.tanh;    about[th] = {"th(x)", "Hyperbolic tangent.", mhelp.HYP}
-- Angles 
deg = math.deg;    about[deg] = {"deg(x)", "Radians to degrees.", mhelp.BASE}
rad = math.rad;    about[rad] = {"rad(x)", "Degrees to radians.", mhelp.BASE}
-- Rounding
floor = math.floor; about[floor] = {"floor(x)", "Return largest integer less or equial to x.", mhelp.OTHER}
ceil = math.ceil;  about[ceil] = {"ceil(x)", "Return smallest integer more or equial to x.", mhelp.OTHER}
-- Constants
_pi = math.pi;     about[_pi] = {"_pi", "Number pi", mhelp.CONST}
_e = math.exp(1)   about[_e] = {"_e", "Euler number", mhelp.CONST}

-- Additional functions --
function rand() return math.random() end
about[rand] = {"rand()", "Random number between 0 and 1.", mhelp.BASE}
-- hyperbolic arcsinus
function ash(x)
   return math.log(x+math.sqrt(x*x+1))
end
about[ash] = {"ash(x)", "Hyperbolic arcsinus.", mhelp.HYP}
-- hyperbolic arccosinus
function ach(x)
   return math.log(x+math.sqrt(x*x-1))
end
about[ach] = {"ach(x)", "Hyperbolic arccosinus.", mhelp.HYP}
-- hyperbolic arctangenth
function ath(x)
   return 0.5*math.log((1+x)/(1-x))
end
about[ath] = {"ath(x)", "Hyperbolic arctangent.", mhelp.HYP}

-- round to closest integer
function round(x)
   local p,q = math.modf(x)
   if q >= 0.5 then 
      return p+1
   elseif q <= -0.5 then 
      return p-1
   end
   return p
end
about[round] = {'round(x)', 'Round value to closest integer.', mhelp.OTHER}

-- create function f(x) from string
function fx(str)
   return assert(load("return function (x) return " .. str .. " end"))()
end
about[fx] = {"fx(str)", "Create Lua function f(x) from string.", mhelp.OTHER}

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
about[plot] = {"plot(str[,a,b])", "Quick plot function in Gnuplot. Use bounds if they are defined.  Variable must be 'x'.", mhelp.OTHER}

-- calculate function for range of values
function eval(fn, x1, xn, step)
   xn = xn or x1
   step = step or 1
   for k = x1, xn, step do print("x="..k.."\tres="..fn(k)) end
end
about[eval] = {"eval(fn,x1[,xn[,step]])", "Evalueate function for given value or interval and print result.", mhelp.OTHER}

-- Print examples from test part of module
function example(nm)
   assert(type(nm) == 'string', 'Module name is expected!')
   local fname = 'liblc/'..nm..'.lua'
   local f = io.open(fname, 'r')
   if not f then print("Can't open file '"..fname.."'"); return end
   local test = require('liblc.test')
   local txt = f:read('*a')
   f:close()   
   txt = test.getcode(txt)
   if txt then
      print(txt)
   else
      print("No examples found in '"..fname.."'")
   end
end
about[example] = {"example(name)", "Show examples for given module, which used to test.", mhelp.BASE}


-- read object from its serialization
function deserialize(obj_str)
   local f = assert(load("return " .. obj_str)) 
   local o = f()
   assert(_G[o.metatablename], "Module '" .. o.modulename .. "' is required")
   setmetatable(o, _G[o.metatablename])
   o.modulename = nil; o.metatablename = nil
   return o
end
about[deserialize] = {"deserialize(obj_str)", "Transform string with serialization into LuaCalculus object.", mhelp.OTHER}

-- Print the contents of a table
function flip(t,N)
   assert(type(t) == 'table', 'Talbe is expected!')
   N = N or 10
   local count = 1
   -- dialog
   local function continue(n)
            io.write(n, ' - continue? (y/n) ')
	    return string.lower(io.read()) == 'y'
         end
   print('{')
   -- keys/values
   for k,v in pairs(t) do
      if math.type(k) ~= 'integer' or k < 1 then
         if count % N == 0 and not continue(count) then break end
	 print(k..' = '..v)
	 count = count + 1
      end
   end
   -- numbers
   for i,v in ipairs(t) do
      io.write(v,', ')
      if i % N == 0 then
         print()
	 if not continue(i) then break end
      end
   end
   print(#t > 0 and '\n}' or '}')
end
about[flip] = {"flip(t[,N])", "Print Lua table in user-friendly form. Ask about continuation after each N elements (default is 10).", mhelp.OTHER}

-- Print mhelp information
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

main.about = about

return main

--===============================
