--[[		sonata/core/main.lua

--- Default functions and objects.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2022.

	module 'main'
--]]

---------------- Tests ---------------------
--[[TEST

require 'lib.main'

-- constants starts from _
ans = _pi                     --> math.pi

-- standard functions
ans = exp(0)+sin(_pi/2)+cosh(0)  --1> 3.0

-- round number
ans = Round(0.9)              --> 1.0

-- save 2 digits
ans = Round(math.pi, 2)       --> 3.14

-- random between 0 and 1
p = rand()
ans = (p >= 0) and (p <= 1)   --> true

-- random integer (1 to 10)
p = randi(10)
ans = (p >= 1) and (p <= 10)  --> true

-- normal distributed random
print(randn())

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
ans = math.floor(_pi)

ans = math.deg(_pi)

--]]

--	LOCAL

local TRIG = 'trigonometry'
local HYP = 'hyperbolic'
local AUX = 'auxiliary'

-- compatibility
local Ver = require("lib.utils")
local Utils = Ver.utils
Ver = Ver.versions

--	INFO 

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Lua based mathematics."
}
--	MODULE

local main = {}

--- Print element, use 'scientific' form for float numbers.
--  @param v Value to print.
main._showElt = function (v)
  if type(v) == 'number' then
    return Utils.numstr(v)
  else
    return tostring(v)
  end
end

--- Show elements of the table.
--  @param t Table to print.
main._showTable = function (t)
  local N, nums = 10, {}
  -- dialog
  local function continue(n)
    io.write(n, ' continue? (y/n) ')
    return string.lower(io.read()) == 'y'
  end
  io.write('\n{ ')
  -- list elements
  for i,v in ipairs(t) do
    io.write(main._showElt(v), ', ')
    nums[i] = true
    if i % N == 0 then
      io.write('\n')
      if not continue(i) then break end
    end
  end
  -- hash table elements
  local count = 0
  for k,v in pairs(t) do
    if not nums[k] then
      io.write('\n', tostring(k), ' = ', main._showElt(v), ', ')
      count = count + 1
      if count % N == 0 and not continue("") then break end
    end
  end
  io.write(' }\n')
end

-- Commonly used methods
abs = math.abs;   about[abs] =  {"abs(d)", "Absolute value."}
exp = math.exp;   about[exp] =  {"exp(d)", "Exponent."}
log = math.log;   about[log] =  {"log(dPos)", "Natural logarithm."}
sqrt = math.sqrt;  about[sqrt] = {"sqrt(dPos)", "Square root."}

-- Trigonometrical
sin = math.sin;   about[sin] =  {"sin(d)", "Sinus x.", TRIG}
cos = math.cos;   about[cos] =  {"cos(d)", "Cosine x.", TRIG}
tan = math.tan;   about[tan] =  {"tan(d)", "Tangent x.", TRIG}
asin = math.asin;  about[asin] = {"asin(d)", "Inverse sine x.", TRIG}
acos = math.acos;  about[acos] = {"acos(d)", "Inverse cosine x.", TRIG}
atan = math.atan;  about[atan] = {"atan(d)", "Inverse tangent x.", TRIG}

atan2 = Ver.atan2
about[atan2] = {"atan2(dy,dx)", "Inverse tangent of dy/dx, use signs.", TRIG}

-- Hyperbolic
cosh = function (x) return 0.5*(math.exp(x)+math.exp(-x)) end
about[cosh] = {"cosh(d)", "Hyperbolic cosine.", HYP}

sinh = function (x) return 0.5*(math.exp(x)-math.exp(-x)) end
about[sinh] = {"sinh(d)", "Hyperbolic sinus.", HYP}

tanh = function (x) t = math.exp(2*x); return (t-1)/(t+1) end
about[tanh] = {"tanh(d)", "Hyperbolic tangent.", HYP}

-- Hyperbolic inverse
asinh = function (x) return math.log(x+math.sqrt(x*x+1)) end
about[asinh] = {"asinh(x)", "Hyperbolic inverse sine.", HYP}

acosh = function (x) return math.log(x+math.sqrt(x*x-1)) end
about[acosh] = {"acosh(x)", "Hyperbolic arc cosine.", HYP}

atanh = function (x) return 0.5*math.log((1+x)/(1-x)) end
about[atanh] = {"atanh(x)", "Hyperbolic inverse tangent.", HYP}

-- Constants
_pi = math.pi;   about[_pi] = {"_pi", "Number pi.", SonataHelp.CONST}
_e  = 2.718281828459;   about[_e]  = {"_e", "Euler number.", SonataHelp.CONST}
-- result
_ans = 0;
about[_ans] = {"_ans", "Result of the last operation.", SonataHelp.OTHER}

-- random
math.randomseed(os.time()) -- comment to get repeatable 'random' numbers

rand = function () return math.random() end
about[rand] = {"rand()", "Random number between 0 and 1."}

randi = function (N) return math.random(1,N) end
about[randi] = {"randi(N)", "Random integer in range from 1 to N."}

randn = function (dMean, dev)
  dMean = dMean or 0
  dev = dev or 1
  -- use Box-Muller transform
  local u,v,s
  repeat
    u = 2*math.random()-1
    v = 2*math.random()-1
    s = u*u + v*v
  until s <= 1 and s > 0
  local norm = u * math.sqrt(-2*math.log(s)/s)
  return norm * dev + dMean
end
about[randn] = {"randn([dMean=0,dev=1])",
  "Normal distributed random value with the given mean and deviation."}


--- Generate list of function values.
--  @param fn Function to apply.
--  @param t Table with arguments.
--  @return Table with result of evaluation.
Map = function (fn, t)
  if type(t) == 'table' then
    if t.map then return t:map(fn) end
    local res = {}
    for i,v in ipairs(t) do res[i] = fn(v) end
    return res
  end
  return nil
end
about[Map] = {'Map(fn,t)','Evaluate function for each table element.', AUX}

--- Show table content and scientific form of numbers.
--  @param ... List of arguments.
Print = function (...)
  for i,v in ipairs({...}) do
    if type(v) == 'table' then
      local mt = getmetatable(v)
      if mt and mt.__tostring then
        -- has representation
        local tmp = tostring(v)
        if string.find(tmp,'\n') then
          io.write('\n', tmp, '\n')
        else
          io.write(tmp, '\t')
        end
      else
        -- require representation
        main._showTable(v)
      end
    else
      -- show value
      io.write(main._showElt(v), '\t')
    end
  end
  io.write('\n')
end
about[Print] = {"Print(...)",
  "Extenden print function, it shows elements of tables and scientific form of numbers.",
  AUX}

--- Round to some precision.
--  @param f Real number.
--  @param N Number of decimal digits.
--  @return Rounded number.
Round = function (f,N)
  N = N or 0
  return Utils.round(f, 10^(-N))
end
about[Round] = {
  'Round(f,[N=0])', 'Round value, define number of decimal digits.', AUX}

--- Execute file inside the interpreter.
--  @param sFile Lua or note file name.
Run = function (sFile, bInt)
  if string.find(sFile, '%.lua$') then
    dofile(sFile)
  elseif string.find(sFile, '%.note$') then
    Sonata:note(sFile, bInt==true)
  else
    io.write('Expected .lua or .note!\n')
  end
end
about[Run] = {'Run(sFile,[bInt=false])',
  "Execute lua- or note- file. Set bInt for interaction.", AUX}

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
about[Type] = {'Type(v)', 'Show type of the object.', AUX}

-- "In the game of life the strong survive..." (Scorpions) ;)
--  board - matrix with 'ones' as live cells
main.life = function (board)
  assert(board.type == 'matrix', 'Matrix is expected!')
  local rows,cols = board:size()
  local src = board
  local gen = 0
  -- make decision about current cell
  local islive = function (r,c)
      local n = src[r-1][c-1] + src[r][c-1] + src[r+1][c-1] + src[r-1][c]
        + src[r+1][c] + src[r-1][c+1] + src[r][c+1] + src[r+1][c+1]
      return (n==3 or n==2 and src[r][c]==1) and 1 or 0
    end
  -- evaluate
  repeat
    local new = board:zeros()   -- empty matrix of the same size
    gen = gen+1
    -- update
    for r = 1,rows do
      for c = 1,cols do
        new[r][c] = gen > 1 and islive(r,c) or src[r][c] ~= 0 and 1 or 0
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
mt_range._init = function (dBeg,dEnd,dStep,iN)
  return setmetatable({_beg=dBeg, _end=dEnd, _step=dStep, _N=iN}, mt_range)
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
  if Ver.isInteger(i) then
    local v
    if i > 0 and i < self._N then
      v = self._beg + (i-1)*self._step
    elseif i == self._N then
      v = self._end
    end
    return v and self._fn and self._fn(v) or v
  end
  return mt_range[i]
end

-- Block setting operation.
mt_range.__newindex = function (self, k, v)
  -- do nothing
end

mt_range.map = function (self, fn)
  return setmetatable(
    {_beg=self._beg, _end=self._end, _step=self._step, _N=self._N, _fn=fn},
    mt_range)
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
about[Range] = {'Range(dBegin,dEnd,[dStep])','Generate range object.', AUX}

-- save link to help info
main.about = about

--- Update help reference when redefine function.
--  @param fnNew New function.
--  @param fnOld Old function.
main._updateHelp = function (fnNew, fnOld)
  main.about[fnNew] = main.about[fnOld]
  main.about[fnOld] = nil
end

return main

--===============================

--TODO don't print result when ; in the end
