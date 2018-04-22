--[[      liblc/main.lua 

--- Define aliases for standard operations and add some new common functions.
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
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

-- save 2 digits
ans = round(math.pi, 2)          --> 3.142

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
ch = function (x) return 0.5*(math.exp(x)+math.exp(-x)) end
about[ch] = {"ch(x)", "Hyperbolic cosine.", lc_help.HYP}
sh = function (x) return 0.5*(math.exp(x)-math.exp(-x)) end   
about[sh] = {"sh(x)", "Hyperbolic sinus.", lc_help.HYP}
th = function (x) t = math.exp(2*x); return (t-1)/(t+1) end
about[th] = {"th(x)", "Hyperbolic tangent.", lc_help.HYP}
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
function round(x,n)
   k = 10^(n or 0)
   local p,q = math.modf(x*k)
   if q >= 0.5 then 
      p = p+1
   elseif q <= -0.5 then 
      p = p-1
   end
   return p / k
end
about[round] = {'round(x[,n])', 'Round value, define number of decimal digits.', lc_help.OTHER}

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
about[deserialize] = {"deserialize(obj_str)", "Transform string with serialization into Sonata LC object.", lc_help.OTHER}

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

-- Return 'scintific' representation of the number
function sci(x)
   print(string.format('%.2E',x))
end
about[sci] = {"sci(x)", "'Scintific' representation of the number.", lc_help.OTHER}

-- Show type of the object.
function lc_type(t)
   local v = type(t)
   if v == 'table' then
      v = t.type or v
   elseif v == 'number' then
      v = math.type(t) 
   end
   return v
end
about[lc_type] = {'lc_type(t)', 'Show type of the object.', lc_help.OTHER}

-- Wait for press button
function lc_pause(s)
   if s then io.write(s) end
   io.read()
end
about[lc_pause] = {'lc_pause([str])', 'Wait for button press, print text if need.', lc_help.OTHER}

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

-- Simple implementation of 'the life'
-- prepare your initial board in form of matrix
function lc_life(board)
   assert(board.type == 'matrix', 'Matrix is expected!')
   local rows,cols = board:size() 
   local src = board
   local gen = 0
   -- make decision about current cell
   local islive = function (r,c)
            local n = src[r-1][c-1] + src[r][c-1] + src[r+1][c-1] + src[r-1][c] + src[r+1][c] + src[r-1][c+1] + src[r][c+1] + src[r+1][c+1]	    
	    return (n==3 or n==2 and src[r][c]==1) and 1 or 0
         end
   -- evaluate
   repeat
      local new = board:zeros()    -- empty matrix of the same size
      gen = gen+1
      -- update 
      for r = 1,rows do
         for c = 1,cols do 
	    new[r][c] = gen > 1 and islive(r,c) or src[r][c] ~= 0 and 1 or 0
	    io.write(new[r][c] == 1 and '*' or ' ')
	 end
	 print('|')
      end
      if gen > 1 and new == src then print('~~ Game Over ~~'); break end
      src = new
      io.write(string.format('#%-3d continue? (y/n) ', gen))
   until 'n' == io.read()
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

main.args = {
-- run tests
['-t'] = '--test',
['--test'] = {
description = 'Apply unit tests to desired module, or all modules if the name is not defined.',
process = function (args)
   local Test = require 'liblc.test'
   if args[2] then
      Test.module(string.format('liblc/%s.lua',args[2]))
   else
      for m in pairs(import) do
	 Test.module(string.format('liblc/%s.lua',m))
      end
   end
   Test.summary()
end,
exit = true},
-- evaluate expressions
['-e'] = '--eval',
['--eval'] = {
description = 'Evaluate command line expression(s).',
process = function (args) main.evalstr(args[2]) end,
exit = true},
-- localisation file
['-l'] = '--lng',
['--lng'] = {
description = 'Create/update file for localisation.',
process = function (args)
   if args[2] then
      lc_help.prepare(args[2], import)
   else 
      print('Current localization file: ', LC_LOCALIZATION)
   end
end,
exit = true},
-- new module
['-n'] = '--new',
['--new'] = {
description = 'Generate template for a new module.',
process = function (args)
   lc_help.newmodule(args[2],args[3],args[4])
end,
exit = true},
-- run code for debugging 
['--dbg'] = {
description = "Run file 'dbg.lua'",
process = function (args) dofile('dbg.lua') end,
exit = false},
-- process files
['no flags'] = {
description = 'Evaluate file(s).',
process = function (args)
   for i = 1,#args do dofile(args[i]) end
end,
exit = true},
-- 
['-h'] = '--help',
}
-- show help
main.args['--help'] = {
description = 'Get this help message.',
process = function ()
   print "\n'Sonata LC' is a Lua based program for mathematical calculations.\n"
   print "USAGE:\n\tlua -i sonata.lua [flag [arg1 arg2 ...]]"
   print "(option '-i' could be omitted when the program is in non-interractive mode)\n\nFLAGS:"
   for k,v in pairs(main.args) do 
      if type(v) == 'string' then
         local ref = main.args[v]         
         print(string.format('\t%s, %s - %s', k, v, ref.description))
      end
   end
   print("\nVERSION: "..lc_version)
   local modules = {}
   for k in pairs(import) do modules[#modules+1] = k end
   print(string.format("Available modules: %s.\n", table.concat(modules,', ')))
   print "BUGS: mail to 'sonatalc@yandex.ru'\n"
end,
exit = true}

return main

--===============================

