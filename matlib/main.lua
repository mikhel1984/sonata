--[[		sonata/core/main.lua

--- Default functions and objects.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2025.

	module 'main'
--]]


---------------- Tests ---------------------
--[[TEST_IT

require 'matlib.main'
-- for bind example
D = require('matlib.data')

-- standard functions
ans = exp(0)+sin(PI/2)+cosh(0)  --.1>  3.0

-- hypotenuse
ans = hypot(3, 4)           --.2>  5.0

-- lazy function definition
-- equal to function (x, y) return x^2 - y^3 end
f = Fn "x, y -> x^2 - y^2"
ans = f(2, 3)                 --> -5.0

-- round number
ans = Round(0.9)              -->  1.0

-- save 2 digits
ans = Round(math.pi, 2)       -->  3.14

-- use tolerance
ans = Round(math.pi, 0.01)    -->  3.14

-- calculate function values
c = Map(sin, {2,4,6,8,10})
ans = c[1]                  --.3>  0.909

-- simplified asciiplot call
-- use table to change range
xrng, yrng = {-3, 3}, {-1, 1}
s = Plot(math.cos, 'cos', xrng, yrng, 'range correct')
print(s)

-- use Lua functions if need
ans = math.deg(PI)          --.2>  180.0

--]]


--	LOCAL

local _tag = { TRIG='trigonometry', HYP='hyperbolic', AUX='auxiliary' }


-- compatibility
local _ext = {
  utils = require("matlib.utils"),
  -- ap = require("matlib.asciiplot"),
}

local _calc = _ext.utils.calc
local _round = _ext.utils.cross.round
local _fn = _ext.utils.utils.Fn


--- Call default or module-specific function.
--  @param fn Function.
--  @param s Function name.
--  @return Function equal to fn(x).
local function _call (fn, s)
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
local _about = {
__module__ = "Lua based mathematics."
}


--	MODULE

local main = {}


-- Commonly used methods
abs = _call(math.abs, 'abs')
_about[abs] = {"abs(x) --> y", "Absolute value."}
exp = _call(math.exp, 'exp')
_about[exp] = {"exp(x) --> y", "Exponent."}
log = _call(math.log, 'log')
_about[log] = {"log(x) --> y", "Natural logarithm."}
sqrt = _call(math.sqrt, 'sqrt')
_about[sqrt] = {"sqrt(x) --> y", "Square root."}

-- Trigonometrical
sin = _call(math.sin, 'sin')
_about[sin] = {"sin(x) --> y", "Sine.", _tag.TRIG}
cos = _call(math.cos, 'cos')
_about[cos] = {"cos(x) --> y", "Cosine.", _tag.TRIG}
tan = _call(math.tan, 'tan')
_about[tan] = {"tan(x) --> y", "Tangent.", _tag.TRIG}
asin = _call(math.asin, 'asin')
_about[asin] = {"asin(x) --> y", "Inverse sine.", _tag.TRIG}
acos = _call(math.acos, 'acos')
_about[acos] = {"acos(x) --> y", "Inverse cosine x.", _tag.TRIG}
atan = _call(math.atan, 'atan')
_about[atan] = {"atan(x) --> y", "Inverse tangent x.", _tag.TRIG}
atan2 = _ext.utils.versions.atan2
_about[atan2] = {"atan2(y_d, x_d) --> num",
  "Inverse tangent of y/x, use signs.", _tag.TRIG}

-- Hyperbolic
cosh = _call(_calc.cosh, 'cosh')
_about[cosh] = {"cosh(x) --> y", "Hyperbolic cosine.", _tag.HYP}
sinh = _call(_calc.sinh, 'sinh')
_about[sinh] = {"sinh(x) --> y", "Hyperbolic sinus.", _tag.HYP}
tanh = _call(_calc.tanh, 'tanh')
_about[tanh] = {"tanh(x) --> y", "Hyperbolic tangent.", _tag.HYP}

-- Hyperbolic inverse
asinh = _call(_calc.asinh, 'asinh')
_about[asinh] = {"asinh(x) --> y", "Hyperbolic inverse sine.", _tag.HYP}
acosh = _call(_calc.acosh, 'acosh')
_about[acosh] = {"acosh(x) --> y", "Hyperbolic arc cosine.", _tag.HYP}
atanh = _call(_calc.atanh, 'atanh')
_about[atanh] = {"atanh(x) --> y", "Hyperbolic inverse tangent.", _tag.HYP}


--- Check equality.
--  @param x First number or object.
--  @param y Second number or object.
--  @return true when objects are equal.
eq = function (x, y)
  if type(x) == 'table' and x.__eq then
    return x:__eq(y)
  elseif type(y) == 'table' and y.__eq then
    return y:__eq(x)
  end
  return x == y
end
_about[eq] = {"eq(x, y) --> bool", "Check equality of two objects."}


--- Convert object to float number if possible.
--  @param x Number or object.
--  @return float point number.
float = function (x)
  if type(x) == 'number' then
    return x
  elseif type(x) == 'table' then
    return x:float()
  else
    return tonumber(x)
  end
end
_about[float] = {"float(obj) --> x", "Convert object to float point number."}


--- Find hypotenuse.
--  @return square root for the sum of squares.
hypot = function (...)
  local s = 0
  for _, v in ipairs {...} do s = s + v*v end
  return math.sqrt(s)
end
_about[hypot] = {"hypot(...)", "Hypotenuse."}


--- Convert object to integer number if possible.
--  @param x Number or object.
--  @return integer number.
int = function (x) return math.floor(float(x)) end
_about[int] = {"int(obj) --> x", "Convert object to integer number."}


-- Constants
PI = math.pi
_about[PI] = {"PI --> 3.14", "Number pi.", _tag.AUX}


--- Generate function from string in form 'args -> expression'.
--  For single argument named 'x' only 'expression' can be defined.
--  @param sExpr Definition in form 'args -> expr'.
--  @return Function based on the expression.
Fn = _fn
_about[Fn] = {"Fn(expr_s) --> fn",
  "Generate function from expression 'args -> value'", _tag.AUX}


--- Generate list of function values.
--  @param fn Function to apply.
--  @param t Table with arguments.
--  @return Table with result of evaluation.
Map = function (fn, t)
  if type(fn) == 'string' then
    fn = _fn(fn)
  end
  if type(t) == 'table' then
    if t.map then return t:map(fn) end
    local res = {}
    for i, v in ipairs(t) do res[i] = fn(v) end
    return res
  end
  return nil
end
_about[Map] = {'Map(fn, in_t) --> out_t',
  'Evaluate function for each table element.', _tag.AUX}


--- Use 'asciiplot' for simplified print.
--  @param ... Objects to plot.
--  @return figure as text.
Plot = function (...)
  _ext.ap = _ext.ap or require('matlib.asciiplot')
  local f = _ext.ap()
  f._x:setRange({-5, 5})
  f:plot(...)
  return tostring(f)
end
_about[Plot] = {"Plot(...) --> str",
    "Plot arguments in form 't', 't1,t1', 'fn,nm', 'fn1,fn2' etc.", _tag.AUX}


--- Round to some precision.
--  @param f Real number.
--  @param d Number of decimal digits or precision.
--  @return Rounded number.
Round = function (f, v)
  v = v or 0
  if not (0 < v and v < 1) then
    v = 10^(-v)
  end
  return _round(f, v)
end
_about[Round] = {'Round(v, decimal_N=0) --> round_v',
  'Round value, define number of decimal digits or tolerance.', _tag.AUX}


if Sonata
then  --~~~~~~~~~~~~~~~~~~~~~~

  -- Sonata specific functions
  _about[use] = {'use([module_s]) --> str|nil',
    "Call use('module') or use{'module1','module2'} to load new functions.", _tag.AUX}

  _about[help] = {"help(fn='main') --> str",
    "Show information about the function.", _tag.AUX}

  _about[quit] = {'quit()', "Quit the program.", _tag.AUX}

end   --~~~~~~~~~~~~~~~~~~~~~~


-- save link to help info
main.about = _about
-- clear load data
_tag = nil

return main

--===============================
