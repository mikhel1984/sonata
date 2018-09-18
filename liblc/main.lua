--[[      liblc/main.lua 

--- Define aliases for standard operations and add some new common functions.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

            module 'main'
--]]

---------------- Tests ---------------------
--[[!!
lc = require 'liblc.main'

-- constants starts from _
ans = _pi                        --> math.pi

-- round number
ans = round(0.9)                 --> 1.0

-- save 2 digits
ans = round(math.pi, 2)          --> 3.14

-- 'small' factorial - int
ans = factorial(12)              --> 479001600

-- 'big' factorial - float
ans = factorial(50)              --~ 3.0414E+64

-- get object type
ans = lc.type(25)                --> 'integer'

-- show table components
a = {a=1,b=2;3,4,5}
lc.flip(a)

-- generate 'vector'
b = lc.range(3)
ans = b[3]                      --> 3

-- full form
b = lc.range(2,10,2)
ans = b[2]                      --> 4

-- append a to b
lc.append(b,a)
ans = b.b                       --> 2
]]

--	LOCAL

local TRIG = 'trigonometry'
local HYP = 'hyperbolic'

-- first 11 factorials
local factorials = {[0] = 1, 1,2,6,24,120,720,5040,40320,362880,3628800}

--	INFO

-- lc_help
lc_help = require "liblc.help"
about = lc_help:new("Lua based calculator.")

--	MODULE

local main = {_LOG10=math.log(10)}

-- Common
abs = math.abs;    about[abs] = {"abs(x)", "Absolute value."}
exp = math.exp;    about[exp] = {"exp(x)", "Exponent."}
log = math.log;    about[log] = {"log(x)", "Natural logarithm."}
sqrt = math.sqrt;  about[sqrt] = {"sqrt(a)", "Square root."}
max = math.max;    about[max] = {"max(...)", "Maximum number."}
min = math.min;    about[min] = {"min(...)", "Minimum number."}
-- Trigonometrical
sin = math.sin;    about[sin] = {"sin(x)", "Sinus x.", TRIG}
cos = math.cos;    about[cos] = {"cos(x)", "Cosine x.", TRIG}
tan = math.tan;    about[tan] = {"tan(x)", "Tangent x.", TRIG}
asin = math.asin;  about[asin] = {"asin(x)", "Arcsine x.", TRIG}
acos = math.acos;  about[acos] = {"acos(x)", "Arc cosine x.", TRIG}
atan = math.atan;  about[atan] = {"atan(x)", "Arctangent x.", TRIG}
atan2 = function (y,x) return math.atan(y,x) end 
about[atan2] = {"atan2(y,x)", "Arctangent of y/x, use signes.", TRIG}
-- Hyperbolic
cosh = function (x) return 0.5*(math.exp(x)+math.exp(-x)) end
about[cosh] = {"cosh(x)", "Hyperbolic cosine.", HYP}
sinh = function (x) return 0.5*(math.exp(x)-math.exp(-x)) end   
about[sinh] = {"sinh(x)", "Hyperbolic sinus.", HYP}
tanh = function (x) t = math.exp(2*x); return (t-1)/(t+1) end
about[tanh] = {"tanh(x)", "Hyperbolic tangent.", HYP}
-- Angles 
rad2deg = math.deg;    about[rad2deg] = {"rad2deg(x)", "Convert radians to degrees."}
deg2rad = math.rad;    about[deg2rad] = {"deg2rad(x)", "Convert degrees to radians."}
-- Rounding
floor = math.floor; about[floor] = {"floor(x)", "Return largest integer less or equal to x.", lc_help.OTHER}
ceil = math.ceil;   about[ceil] = {"ceil(x)", "Return smallest integer more or equal to x.", lc_help.OTHER}
-- Constants
_pi = math.pi;      about[_pi] = {"_pi", "Number pi", lc_help.CONST}
_e = math.exp(1.0); about[_e] = {"_e", "Euler number", lc_help.CONST}


log10 = function (x) return math.log(x)/main._LOG10 end
about[log10] = {"log10(x)", "Decimal logarithm."}

rand = function () return math.random() end
about[rand] = {"rand()", "Random number between 0 and 1."}
randi = function (N) return math.random(1,N) end
about[randi] = {"randi(N)", "Random integer in range from 1 to N."}
-- hyperbolic arcsine
asinh = function (x) return math.log(x+math.sqrt(x*x+1)) end
about[asinh] = {"asinh(x)", "Hyperbolic arcsine.", HYP}
-- hyperbolic arc cosine
acosh = function (x) return math.log(x+math.sqrt(x*x-1)) end
about[acosh] = {"acosh(x)", "Hyperbolic arc cosine.", HYP}
-- hyperbolic arctangent
atanh = function (x) return 0.5*math.log((1+x)/(1-x)) end
about[atanh] = {"atanh(x)", "Hyperbolic arctangent.", HYP}

-- round to closest integer
round = function (x,n)
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
main.eval = function (fn, x1, xn, step)
   xn = xn or x1
   step = step or 1
   for k = x1, xn, step do print("x="..k.."\tres="..fn(k)) end
end

factorial = function (n)
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
about[factorial] = {'factorial(n)', 'Evaluate factorial.'}

-- read object from its serialization
main.deserialize = function (obj_str)
   local f = assert(load("return " .. obj_str)) 
   local o = f()
   assert(_G[o.metatablename], "Module '" .. o.modulename .. "' is required")
   setmetatable(o, _G[o.metatablename])
   o.modulename = nil; o.metatablename = nil
   return o
end
about[main.deserialize] = {"deserialize(obj_str)", "Transform string with serialization into Sonata LC object.", lc_help.OTHER}

-- Print the contents of a table
main.flip = function (t,N)
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
about[main.flip] = {"lc.flip(t[,N])", "Print Lua table in user-friendly form. Ask about continuation after each N elements (default is 10).", lc_help.OTHER}

-- Return 'scintific' representation of the number
main.sci = function (x)
   print(string.format('%.2E',x))
end
about[main.sci] = {"lc.sci(x)", "'Scintific' representation of the number.", lc_help.OTHER}

-- Show type of the object.
function main.type(t)
   local v = type(t)
   if v == 'table' then
      v = t.type or v
   elseif v == 'number' then
      v = math.type(t) 
   end
   return v
end
about[main.type] = {'lc.type(t)', 'Show type of the object.', lc_help.OTHER}

-- Wait for press button
main.pause = function (s)
   if s then io.write(s) end
   io.read()
end
about[main.pause] = {'lc.pause([str])', 'Wait for button press, print text if need.', lc_help.OTHER}

-- Generate sequence of values
main.range = function (from,to,step)
   step = step or 1
   if not to then to = from; from = 1 end
   assert((to-from)*step > 0)
   local res = {}
   for i = from,to,step do res[#res+1] = i end
   return res
end
about[main.range] = {'lc.range([from,]to[,step])','Generate table with sequence of numbers.', lc_help.OTHER}

-- append to table a value or elements of other table
main.append = function (t,val)
   if type(val) == 'table' then
      for k,v in pairs(val) do 
         if type(k) == 'number' then
	    t[#t+1] = v
	 else
            t[k] = v 
	 end
      end
   else
      t[#t+1] = val
   end
end
about[main.append] = {'lc.append(tbl,val)','Append value or table to the given table.', lc_help.OTHER}

-- Print lc_help information
help = function(fn)   
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
main.life = function (board)
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

main.evalText = function (src,dst)
   local F = require('liblc.files')
   local txt = assert(F.read(src), 'No such file: '..tostring(src))
   local res = string.gsub(txt, '##(.-)##', function (s)
                  for expr in F.split(s,';') do
                     if string.find(expr, '=') or string.find(expr,'import') then
                        local fn = load(expr)
                        if fn then fn() end
                     else
                        local fn = load('return '..expr)
                        if fn then return tostring(fn()) end
                     end
                  end
                  return ''
               end)
   if dst then
      out = assert(io.open(dst, 'w'), 'Cannot create file: '..tostring(dst))
      out:write(res)
      out:close()
      print('Done')
   else
      print(res)
   end   
end

main.about = about

main._args = {
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
-- generate 'help.html'
['-d'] = '--doc',
['--doc'] = {
description = 'Create/update documentation file.',
process = function () lc_help.generateDoc(LC_LOCALIZATION, import) end,
exit = true},
-- new module
['-n'] = '--new',
['--new'] = {
description = 'Generate template for a new module.',
process = function (args)
   lc_help.newModule(args[2],args[3],args[4])
end,
exit = true},
-- evaluate code in text file
['-e'] = '--eval',
['--eval'] = {
description = 'Read text file and evaluate expressions in ##..##.',
process = function (args)
   main.evalText(args[2],args[3])
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
main._args['--help'] = {
description = 'Get this help message.',
process = function () print(main._args.text()) end,
exit = true}
-- string representation of the help info
main._args.text = function ()
   local txt = {   
      "\n'Sonata LC' is a Lua based program for mathematical calculations.",
      "",
      "USAGE:",
      "\tlua -i sonata.lua [flag [arg1 arg2 ...]]",
      "(option '-i' could be omitted when the program is in non-interractive mode)",
      "",
      "FLAGS:"
   }
   for k,v in pairs(main._args) do 
      if type(v) == 'string' then
         --local ref = main._args[v]         
         txt[#txt+1] = string.format('\t%s, %s - %s', k, v, main._args[v].description)
      end
   end
   txt[#txt+1] = "\nVERSION: "..lc_version
   txt[#txt+1] = ""
   local modules = {}
   for k in pairs(import) do modules[#modules+1] = k end
   txt[#txt+1] = string.format("MODULES: %s.\n", table.concat(modules,', '))
   txt[#txt+1] = "BUGS: mail to 'sonatalc@yandex.ru'\n"
   
   return table.concat(txt,'\n')   
end

return main

--===============================
-- TODO: add step-by-step execution
-- TODO: define API for each module
-- TODO: add constant parameters
