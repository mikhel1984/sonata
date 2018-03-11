--[[      liblc/main.lua 

--- Define aliases for standard operations and add some new common functions.
--  @author <a href="mailto:vpsys@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

            module 'main'
--]]

---------------- Tests ---------------------
--[[!!
require 'liblc.main'

-- constants starts from _
ans = _pi                        --> math.pi

-- round number
ans = round(0.9)                 --> 1.0

-- 'small' factorial - int
ans = fact(12)                   --> 479001600

-- 'big' factorial - float
ans = fact(50)                   --~ 3.0414E+64

-- get object type
ans = lctype(25)                 --> 'integer'

-- show table components
a = {a=1,b=2;3,4,5}
flip(a)
]]

local main = {}

-- lc_help
lc_help = require "liblc.help"
about = lc_help:new("Lua based calculator.")

-- first 11 factorials
local factorials = {[0] = 1, 1,2,6,24,120,720,5040,40320,362880,3628800}

-- Common
abs = math.abs;    about[abs] = {"abs(x)", "Absolute value.", }
exp = math.exp;    about[exp] = {"exp(x)", "Exponent.", }
ln = math.log;     about[ln] = {"ln(x)", "Natural logarithm.", }
pow = math.pow;    about[pow] = {"pow(a,b)", "Return a^b.", }
sqrt = math.sqrt;  about[sqrt] = {"sqrt(a)", "Square root.", }
max = math.max;    about[max] = {"max(...)", "Maximum number.", }
min = math.min;    about[min] = {"min(...)", "Minimum number.", }
-- Trigonometrical
sin = math.sin;    about[sin] = {"sin(x)", "Sinus x.", lc_help.TRIG}
cos = math.cos;    about[cos] = {"cos(x)", "Cosine x.", lc_help.TRIG}
tan = math.tan;    about[tan] = {"tan(x)", "Tangent x.", lc_help.TRIG}
asin = math.asin;  about[asin] = {"asin(x)", "Arcsine x.", lc_help.TRIG}
acos = math.acos;  about[acos] = {"acos(x)", "Arc cosine x.", lc_help.TRIG}
atan = math.atan;  about[atan] = {"atan(y[,x])", "Arctangent y. In case of 2 parameters calculate y/x with signs.", lc_help.TRIG}
-- Hyperbolic
ch = math.cosh;    about[ch] = {"ch(x)", "Hyperbolic cosine.", lc_help.HYP}
sh = math.sinh;    about[sh] = {"sh(x)", "Hyperbolic sinus.", lc_help.HYP}
th = math.tanh;    about[th] = {"th(x)", "Hyperbolic tangent.", lc_help.HYP}
-- Angles 
deg = math.deg;    about[deg] = {"deg(x)", "Radians to degrees.", }
rad = math.rad;    about[rad] = {"rad(x)", "Degrees to radians.", }
-- Rounding
floor = math.floor; about[floor] = {"floor(x)", "Return largest integer less or equal to x.", lc_help.OTHER}
ceil = math.ceil;  about[ceil] = {"ceil(x)", "Return smallest integer more or equal to x.", lc_help.OTHER}
-- Constants
_pi = math.pi;     about[_pi] = {"_pi", "Number pi", lc_help.CONST}
_e = math.exp(1.0) about[_e] = {"_e", "Euler number", lc_help.CONST}

-- Additional functions --
main.LOG10 = math.log(10)

function lg(x) return math.log(x)/main.LOG10 end
about[lg] = {"lg(x)", "Decimal logarithm.", }

function rand() return math.random() end
about[rand] = {"rand()", "Random number between 0 and 1.", }
-- hyperbolic arcsine
function ash(x)
   return math.log(x+math.sqrt(x*x+1))
end
about[ash] = {"ash(x)", "Hyperbolic arcsine.", lc_help.HYP}
-- hyperbolic arc cosine
function ach(x)
   return math.log(x+math.sqrt(x*x-1))
end
about[ach] = {"ach(x)", "Hyperbolic arc cosine.", lc_help.HYP}
-- hyperbolic arctangent
function ath(x)
   return 0.5*math.log((1+x)/(1-x))
end
about[ath] = {"ath(x)", "Hyperbolic arctangent.", lc_help.HYP}

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
about[round] = {'round(x)', 'Round value to closest integer.', lc_help.OTHER}

-- calculate function for range of values
function main.eval(fn, x1, xn, step)
   xn = xn or x1
   step = step or 1
   for k = x1, xn, step do print("x="..k.."\tres="..fn(k)) end
end

function fact(n)
   assert(n >= 0 and math.type(n) == 'integer', 'Expected positive integer value!')
   local tmp = factorials[n]
   if tmp and tmp < math.huge then return tmp end
   -- calculate
   local maxint, isint = math.maxinteger / 100, true
   for i = #factorials,n do
      tmp = factorials[i]*(i+1)
      assert(tmp < math.huge, "Too big value! Try Big.fact(n) or Spec.gammaln(n+1).")
      if isint and tmp > maxint then tmp = tmp * 1.0; isint = false end
      factorials[i+1] = tmp
   end
   return factorials[n]
end

-- Print examples from test part of module
function example(nm)
   assert(type(nm) == 'string', 'Module name is expected!')
   local fname = 'liblc'..lc_help.SEP..nm..'.lua'
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
about[example] = {"example(name)", "Show examples for given module, which used to test.", }


-- read object from its serialization
function deserialize(obj_str)
   local f = assert(load("return " .. obj_str)) 
   local o = f()
   assert(_G[o.metatablename], "Module '" .. o.modulename .. "' is required")
   setmetatable(o, _G[o.metatablename])
   o.modulename = nil; o.metatablename = nil
   return o
end
about[deserialize] = {"deserialize(obj_str)", "Transform string with serialization into LuaCalculus object.", lc_help.OTHER}

-- Print the contents of a table
function flip(t,N)
   assert(type(t) == 'table', 'Table is expected!')
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
	 print(tostring(k)..' = '..tostring(v))
	 count = count + 1
      end
   end
   -- numbers
   for i,v in ipairs(t) do
      io.write(tostring(v),', ')
      if i % N == 0 then
         print()
	 if not continue(i) then break end
      end
   end
   print(#t > 0 and '\n}' or '}')
end
about[flip] = {"flip(t[,N])", "Print Lua table in user-friendly form. Ask about continuation after each N elements (default is 10).", lc_help.OTHER}

-- Show type of the object.
function lctype(t)
   local v = type(t)
   if v == 'table' then
      v = t.type or v
   elseif v == 'number' then
      v = math.type(t) 
   end
   return v
end
about[lctype] = {'lctype(t)', 'Show type of the object.', lc_help.OTHER}

-- Print lc_help information
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

-- Evaluate string equation, print result
function main.evalstr (s)
   local T = require 'liblc.files'
   for c in T.split(s,';') do
      if not (c:find('=') or c:find('print')) then c = string.format('print(%s)',c) end
      local fn = load(c)
      if fn then fn() end
   end
end

main.about = about

return main

--===============================

-- TODO: round() for different number of digits
