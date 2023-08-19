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
ans = exp(0)+sin(pi/2)+cosh(0)  --1>  3.0

-- round number
ans = Round(0.9)              -->  1.0

-- save 2 digits
ans = Round(math.pi, 2)       -->  3.14

-- get type
-- "knows" Sonata objects
ans = Type(25)                -->  'integer'

-- modified print function
a = {a=1,b=2, 3,4,5}
Print(a, 0.123)


-- calculate function values
c = Map(sin, {2,4,6,8,10})
ans = c[1]                   --3>  0.909

-- use Lua functions if need
ans = math.deg(pi)           --2>  180.0

--]]


--	LOCAL

local TRIG = 'trigonometry'
local HYP = 'hyperbolic'
local AUX = 'auxiliary'


-- compatibility
local Ver = require("matlib.utils")
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
pi = math.pi
about[pi] = {"pi", "Number pi.", AUX}


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
--  @param N Number of decimal digits.
--  @return Rounded number.
Round = function (f, N)
  N = N or 0
  return Utils.round(f, 10^(-N))
end
about[Round] = {
  'Round(x_d, N=0) --> num', 'Round value, define number of decimal digits.', AUX}


--- Show type of the object.
--  @param v Some Lua or Sonata object.
--  @return String with type value.
Type = function (v)
  local u = type(v)
  if u == 'table' then
    u = v.type or u
  elseif u == 'number' then
    u = Ver.mathType(v)
  end
  return u
end
about[Type] = {'Type(x) --> str', 'Show type of the object.', AUX}


-- "In the game of life the strong survive..." (Scorpions) ;)
--  board - matrix with 'ones' as live cells
main.life = function (board)
  assert(board.type == 'matrix', 'Matrix is expected!')
  local rows, cols = board:size()
  local src = board
  local gen = 0
  -- make decision about current cell
  local function islive (r, c)
    local n = src[r-1][c-1] + src[r][c-1] + src[r+1][c-1] + src[r-1][c]
      + src[r+1][c] + src[r-1][c+1] + src[r][c+1] + src[r+1][c+1]
    return (n==3 or n==2 and src[r][c]==1) and 1 or 0
  end
  -- evaluate
  repeat
    local new = board:zeros()   -- empty matrix of the same size
    gen = gen+1
    -- update
    for r = 1, rows do
      for c = 1, cols do
        new[r][c] = gen > 1 and islive(r, c) or src[r][c] ~= 0 and 1 or 0
        io.write(new[r][c] == 1 and '*' or ' ')
      end
      io.write('|\n')
    end
    if gen > 1 and new == src then
      return print('~~ Game Over ~~')
    end
    src = new
    io.write(string.format('#%-3d continue? (y/n) ', gen))
  until 'n' == io.read()
end


-- Sonata specific functions

if Sonata then  -- SPECIFIC

about[use] = {'use([module_s]) --> str|nil',
  "Call use('module') or use{'module1','module2'} to load new functions.", AUX}
about[help] = {"help(fn='main') --> str", "Show information about the function.", AUX}
about[quit] = {'quit()', "Quit the program.", AUX}

end  -- SPECIFIC


-- save link to help info
main.about = about

return main

--===============================
