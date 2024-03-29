--[[		sonata/core/main.lua

--- Default functions and objects.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2023.

	module 'main'
--]]


---------------- Tests ---------------------
--[[TEST_IT

require 'matlib.main'

-- standard functions
ans = exp(0)+sin(PI/2)+cosh(0)  --1>  3.0

-- round number
ans = Round(0.9)              -->  1.0

-- save 2 digits
ans = Round(math.pi, 2)       -->  3.14

-- use tolerance
ans = Round(math.pi, 0.01)    -->  3.14

-- calculate function values
c = Map(sin, {2,4,6,8,10})
ans = c[1]                   --3>  0.909

-- use Lua functions if need
ans = math.deg(PI)           --2>  180.0

--]]


--	LOCAL

local TRIG = 'trigonometry'
local HYP = 'hyperbolic'
local AUX = 'auxiliary'


-- compatibility
local Ver = require("matlib.utils")
local Cross = Ver.cross
local Utils = Ver.utils
local Calc = Ver.calc
Ver = Ver.versions


--- Call default or module-specific function.
--  @param fn Function.
--  @param s Function name.
--  @return Function equal to fn(x).
local _call = function (fn, s)
  return function (v)
    if type(v) == 'table' then
      local method = v[s]
      return method and method(v) or fn(v:float())
    else
      return fn(v)
    end
  end
end


--	INFO

-- description
local about = {
__module__ = "Lua based mathematics."
}


--	MODULE

local main = {}


-- Commonly used methods
abs = _call(math.abs, 'abs')
about[abs] = {"abs(x) --> num", "Absolute value."}
exp = _call(math.exp, 'exp')
about[exp] = {"exp(x) --> y", "Exponent."}
log = _call(math.log, 'log')
about[log] = {"log(x) --> y", "Natural logarithm."}
sqrt = _call(math.sqrt, 'sqrt')
about[sqrt] = {"sqrt(x) --> y", "Square root."}

-- Trigonometrical
sin = _call(math.sin, 'sin')
about[sin] = {"sin(x) --> y", "Sine.", TRIG}
cos = _call(math.cos, 'cos')
about[cos] = {"cos(x) --> y", "Cosine.", TRIG}
tan = _call(math.tan, 'tan')
about[tan] = {"tan(x) --> y", "Tangent.", TRIG}
asin = _call(math.asin, 'asin')
about[asin] = {"asin(x) --> y", "Inverse sine.", TRIG}
acos = _call(math.acos, 'acos')
about[acos] = {"acos(x) --> y", "Inverse cosine x.", TRIG}
atan = _call(math.atan, 'atan')
about[atan] = {"atan(x) --> y", "Inverse tangent x.", TRIG}
atan2 = Ver.atan2
about[atan2] = {"atan2(y_d, x_d) --> num", "Inverse tangent of dy/dx, use signs.", TRIG}

-- Hyperbolic
cosh = _call(Calc.cosh, 'cosh')
about[cosh] = {"cosh(x) --> y", "Hyperbolic cosine.", HYP}
sinh = _call(Calc.sinh, 'sinh')
about[sinh] = {"sinh(x) --> y", "Hyperbolic sinus.", HYP}
tanh = _call(Calc.tanh, 'tanh')
about[tanh] = {"tanh(x) --> y", "Hyperbolic tangent.", HYP}

-- Hyperbolic inverse
asinh = _call(Calc.asinh, 'asinh')
about[asinh] = {"asinh(x) --> y", "Hyperbolic inverse sine.", HYP}
acosh = _call(Calc.acosh, 'acosh')
about[acosh] = {"acosh(x) --> y", "Hyperbolic arc cosine.", HYP}
atanh = _call(Calc.atanh, 'atanh')
about[atanh] = {"atanh(x) --> y", "Hyperbolic inverse tangent.", HYP}

-- Constants
PI = math.pi
about[PI] = {"PI --> 3.14", "Number pi.", AUX}


--- Wrap function to simplify call if need.
--  @param obj Sonata object.
--  @param name Function name.
--  @return clojure of the method.
Bind = function (obj, name)
  return function (...) return obj[name](obj, ...) end
end
about[Bind] = {"Bind(obj, fn_name) --> fn", "Wrap function to call it without object.", AUX}


--- Generate list of function values.
--  @param fn Function to apply.
--  @param t Table with arguments.
--  @return Table with result of evaluation.
Map = function (fn, t)
  if type(t) == 'table' then
    if t.map then return t:map(fn) end
    local res = {}
    for i, v in ipairs(t) do res[i] = fn(v) end
    return res
  end
  return nil
end
about[Map] = {'Map(fn, in_t) --> out_t','Evaluate function for each table element.', AUX}


--- Round to some precision.
--  @param f Real number.
--  @param d Number of decimal digits or precision.
--  @return Rounded number.
Round = function (f, v)
  v = v or 0
  if not (0 < v and v < 1) then
    v = 10^(-v)
  end
  return Cross.round(f, v)
end
about[Round] = {'Round(v, d=0) --> v', 
  'Round value, define number of decimal digits or tolerance.', AUX}


-- Sonata specific functions
if Sonata then  --( 

about[use] = {'use([module_s]) --> str|nil',
  "Call use('module') or use{'module1','module2'} to load new functions.", AUX}
about[help] = {"help(fn='main') --> str", "Show information about the function.", AUX}
about[quit] = {'quit()', "Quit the program.", AUX}

end             --) if Sonata


-- save link to help info
main.about = about

return main

--===============================
