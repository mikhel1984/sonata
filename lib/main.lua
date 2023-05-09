--[[		sonata/core/main.lua

--- Default functions and objects.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2023.

	module 'main'
--]]


---------------- Tests ---------------------
--[[TEST

require 'lib.main'

-- constants starts from '_'
ans = _pi                     --> math.pi

-- standard functions
ans = exp(0)+sin(_pi/2)+cosh(0)  --1> 3.0

-- round number
ans = Round(0.9)              --> 1.0

-- save 2 digits
ans = Round(math.pi, 2)       --> 3.14

-- get type
-- "knows" Sonata objects
ans = Type(25)                --> 'integer'

-- modified print function
a = {a=1,b=2, 3,4,5}
Print(a, 0.123)

-- generate 'sequence'
b = Range(1,3)
ans = b[3]                    --> 3

-- even numbers
b = Range(2,10,2)
ans = b[2]                    --> 4

-- linear transformations
-- with range Range objects
b2 = 2*b + 4
ans = b2[1]                   --> 8

-- calculate function values
c = Map(sin, b)
ans = c[1]                   --3> 0.909

-- use Lua functions if need
ans = math.deg(_pi)          --2> 180.0

--]]


--	LOCAL

local TRIG = 'trigonometry'
local HYP = 'hyperbolic'
local AUX = 'auxiliary'


-- compatibility
local Ver = require("lib.utils")
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


--- Print element, use 'scientific' form for float numbers.
--  @param v Value to print.
main._showElt = function (v)
  return type(v) == 'number' and Utils.numstr(v) or tostring(v)
end


--- Show elements of the table.
--  @param t Table to print.
main._showTable = function (t)
  local N, nums, out = 10, {}, {'{ '}
  -- dialog
  local function continue(n, res)
    local txt = tostring(n) .. ' continue? (y/n/) '
    if Sonata then
      txt = Sonata.ask(txt, res)
    else
      io.write(res, '\n', txt)
      txt = io.read()
    end
    return string.lower(txt) == 'y'
  end
  -- list elements
  for i, v in ipairs(t) do
    out[#out+1] = main._showElt(v); out[#out+1] = ', '
    nums[i] = true
    if i % N == 0 then
      out[#out+1] = '\n'
      local data = table.concat(out)
      out = {'\n'}
      if not continue(i, data) then break end
    end
  end
  -- hash table elements
  local count = 0
  for k, v in pairs(t) do
    if not nums[k] then
      out[#out+1] = string.format('\n%s = %s, ', tostring(k), main._showElt(v))
      count = count + 1
      if count % N == 0 then
        local data = table.concat(out)
        out = {'\n'}
        if not continue('', data) then break end
      end
    end
  end
  out[#out+1] = ' }\n'
  return table.concat(out)
end


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
_pi = math.pi;         about[_pi] = {"_pi", "Number pi.", SonataHelp.CONST}
_e  = 2.718281828459;  about[_e]  = {"_e", "Euler number.", SonataHelp.CONST}


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


--- Show table content and scientific form of numbers.
--  @param ... List of arguments.
Print = function (...)
  local out = {}
  for i, v in ipairs({...}) do
    if type(v) == 'table' then
      local mt = getmetatable(v)
      if mt and mt.__tostring then
        -- has representation
        local tmp = tostring(v)
        if string.find(tmp, '\n') then
          out[#out+1] = '\n'
        end
        out[#out+1] = tmp
        out[#out+1] = '\t'
      else
        -- require representation
        out[#out+1] = main._showTable(v)
      end
    else
      -- show value
      out[#out+1] = main._showElt(v)
      out[#out+1] = '\t'
    end
  end
  local res = table.concat(out)
  if Sonata then Sonata.say(res) else print(res) end
end
about[Print] = {"Print(...) --> nil",
  "Extenden print function, it shows elements of tables and scientific form of numbers.",
  AUX}


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


-- Methametods for the range of numbers.
local mt_range = { type = 'range' }


--- Initialize range object.
--  @param dBeg First value.
--  @param dEnd Last value.
--  @param dStep Step value.
--  @param iN Number of elements.
--  @return Range object.
mt_range._init = function (dBeg, dEnd, dStep, iN, fn)
  return setmetatable({_beg=dBeg, _end=dEnd, _step=dStep, _N=iN, _fn=fn}, mt_range)
end


--- Add number (shift range).
--  @param d Any number.
--  @param R Range object.
--  @return Shifted range table.
mt_range.__add = function (d, R)
  if type(R) == 'number' then
    return mt_range.__add(R, d)
  else
    return mt_range._init(d+R._beg, d+R._end, R._step, R._N)
  end
end


--- Substract number.
--  @param R Range object.
--  @param d Any number.
--  @return Shifted range table.
mt_range.__sub = function (R, d)
  if type(R) == 'number' then   -- d is range
    return R + (-1)*d
  else
    return mt_range._init(R._beg-d, R._end-d, R._step, R._N)
  end
end


--- Multiply to number (expand range).
--  @param d Any number.
--  @param R Range object.
--  @return Expanded range table.
mt_range.__mul = function (d, R)
  if type(R) == 'number' then
    return mt_range.__mul(R, d)
  else
    return mt_range._init(d*R._beg, d*R._end, d*R._step, R._N)
  end
end


--- Pretty print.
--  @param R Range object.
--  @return String with the table representation.
mt_range.__tostring = function (self)
  return string.format("%s{%g, %g .. %g}", self._fn and "fn" or "",
    self._beg, self._beg+self._step, self._end)
end


--- Get number of elements.
--  @param self Range object.
--  @return Element number.
mt_range.__len = function (self)
  return self._N
end


--- Get i-th element.
--  @param self Range object.
--  @param i Element index.
--  @return Number.
mt_range.__index = function (self, i)
  if Ver.isInteger(i) and i > 0 and i <= self._N then
    local v = 0
    if i < self._N then
      v = self._beg + (i-1)*self._step
    else
      v = self._end
    end
    return v and self._fn and self._fn(v) or v
  else
    return mt_range[i]
  end
end


-- Don't set new elements
mt_range.__newindex = function (self, k, v) end


--- Apply function to range of numbers.
--  @param fn Function f(x).
--  @return modified range of numbers.
mt_range.map = function (self, fn)
  if self._fn then
    local fn1 = function (x) return fn(self._fn(x)) end  -- combine functions
    return mt_range._init(self._beg, self._end, self._step, self._N, fn1)
  else
    return mt_range._init(self._beg, self._end, self._step, self._N, fn)
  end
end


--- Generate sequence of values.
--  @param dBegin Beginning of range.
--  @param dEnd End of range.
--  @param dStep Step value (default is 1).
--  @return Table with numbers, Range object.
Range = function (dBegin, dEnd, dStep)
  dStep = dStep or (dEnd > dBegin) and 1 or -1
  local diff = dEnd - dBegin
  assert(diff * dStep > 0, "Wrong range or step")
  -- check size
  local n, _ = math.modf(diff / dStep)
  if math.abs(n*dStep - dEnd) >= math.abs(dStep * 0.1) then n = n + 1 end
  -- result
  return mt_range._init(dBegin, dEnd, dStep, n)
end
about[Range] = {'Range(begin_d, end_d, [step_d]) --> new_R', 'Generate range object.', AUX}


-- Sonata specific functions

if Sonata then  -- SPECIFIC

about[use] = {'use([module_s]) --> str|nil',
  "Call use('module') or use{'module1','module2'} to load new functions.", AUX}
about[help] = {"help(fn='main') --> str", "Show information about the function.", AUX}
about[quit] = {'quit() --> nil', "Quit the program.", AUX}

end  -- SPECIFIC


-- save link to help info
main.about = about

return main

--===============================
