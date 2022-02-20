--[[		sonata/core/main.lua 

--- Define aliases for standard operations and add some new common functions.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2021.

	module 'main'
--]]

---------------- Tests ---------------------
--[[TEST

require 'core.main'

-- constants starts from _
ans = _pi                     --> math.pi

-- standard functions 
ans = exp(0)+sin(_pi/2)+cosh(0)  --1> 3.0

-- round number
ans = Round(0.9)           --> 1.0

-- save 2 digits
ans = Round(math.pi, 2)    --> 3.14

-- random between 0 and 1
p = rand()
ans = (p >= 0) and (p <= 1)   --> true

-- random integer (1 to 10)
p = randi(10)
ans = (p >= 1) and (p <= 10)  --> true

-- normal distributed random
print(randn())

-- get object type
-- "knows" types for Sonata objects
ans = Type(25)             --> 'integer'

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

-- prepare file name
nm = os.tmpname()

-- dsv write 
-- separate elements with ';'
t = {{1,2,3},{4,5,6}}
DsvWrite(nm, t, ';')

-- dsv read
-- with separator ';'
tt = DsvRead(nm, ';')
ans = tt[2][2]                --> 5

-- csv write by columns
DsvWrite(nm, t, ',', true)

-- csv read by columns
tt = DsvRead(nm, ',', true)
ans = tt[1][3]                --> 3

-- read table from file
f = io.open(nm,'w')
f:write("{1,2.0,a='pqr',b={3,4,c='abc'}}")
f:close()
aa = TblImport(nm)
ans = aa.b.c                  --> 'abc'

--]]

--	LOCAL

local TRIG = 'trigonometry'
local HYP = 'hyperbolic'
local FILES = 'files'

-- compatibility
local Ver = {}
if _VERSION < 'Lua 5.3' then
  Ver.atan2 = math.atan2
  Ver.mathType = function (x)
    local n = tonumber(x)
    if not n then return nil end
    local _,p = math.modf(n)
    return (p == 0.0) and 'integer' or 'float'
  end
else
  Ver.atan2 = math.atan
  Ver.mathType = math.type
end

--	MODULE

local main = {}

-- Commonly used methods
abs = math.abs;   About[abs] =  {"abs(x)", "Absolute value."}
exp = math.exp;   About[exp] =  {"exp(x)", "Exponent."}
log = math.log;   About[log] =  {"log(x)", "Natural logarithm."}
sqrt = math.sqrt;  About[sqrt] = {"sqrt(a)", "Square root."}

-- Trigonometrical
sin = math.sin;   About[sin] =  {"sin(x)", "Sinus x.", TRIG}
cos = math.cos;   About[cos] =  {"cos(x)", "Cosine x.", TRIG}
tan = math.tan;   About[tan] =  {"tan(x)", "Tangent x.", TRIG}
asin = math.asin;  About[asin] = {"asin(x)", "Inverse sine x.", TRIG}
acos = math.acos;  About[acos] = {"acos(x)", "Inverse cosine x.", TRIG}
atan = math.atan;  About[atan] = {"atan(x)", "Inverse tangent x.", TRIG}

atan2 = Ver.atan2 
About[atan2] = {"atan2(y,x)", "Inverse tangent of y/x, use signs.", TRIG}

-- Hyperbolic
cosh = function (x) return 0.5*(math.exp(x)+math.exp(-x)) end
About[cosh] = {"cosh(x)", "Hyperbolic cosine.", HYP}

sinh = function (x) return 0.5*(math.exp(x)-math.exp(-x)) end  
About[sinh] = {"sinh(x)", "Hyperbolic sinus.", HYP}

tanh = function (x) t = math.exp(2*x); return (t-1)/(t+1) end
About[tanh] = {"tanh(x)", "Hyperbolic tangent.", HYP}

-- Hyperbolic inverse 
asinh = function (x) return math.log(x+math.sqrt(x*x+1)) end
About[asinh] = {"asinh(x)", "Hyperbolic inverse sine.", HYP}

acosh = function (x) return math.log(x+math.sqrt(x*x-1)) end
About[acosh] = {"acosh(x)", "Hyperbolic arc cosine.", HYP}

atanh = function (x) return 0.5*math.log((1+x)/(1-x)) end
About[atanh] = {"atanh(x)", "Hyperbolic inverse tangent.", HYP}

-- Constants
_pi = math.pi;   About[_pi] = {"_pi", "Number pi.", SonataHelp.CONST}
_e  = 2.718281828459;   About[_e]  = {"_e", "Euler number.", SonataHelp.CONST}
-- result 
_ans = 0;   About[_ans] = {"_ans", "Result of the last operation.", SonataHelp.OTHER}

-- random
math.randomseed(os.time()) -- comment to get repeatable 'random' numbers

rand = function () return math.random() end
About[rand] = {"rand()", "Random number between 0 and 1."}

randi = function (N) return math.random(1,N) end
About[randi] = {"randi(N)", "Random integer in range from 1 to N."}

randn = function () 
  -- use Box-Muller transform
  local u,v,s
  repeat
    u = 2*math.random()-1
    v = 2*math.random()-1
    s = u*u + v*v
  until s <= 1 and s > 0
  return u * math.sqrt(-2*math.log(s)/s)
end
About[randn] = {"randn()", "Normal distributed random value with 0 mean and variance 1."}

--- Round to closest integer.
--  @param f Real number.
--  @param N Number of decimal digits.
--  @return Rounded number.
Round = function (f,N)
  local k = 10^(N or 0)
  local p,q = math.modf(f*k)
  if q >= 0.5 then 
    p = p+1
  elseif q <= -0.5 then 
    p = p-1
  end
  return p / k
end
About[Round] = {'Round(f[,N=0])', 'Round value, define number of decimal digits.', SonataHelp.OTHER}

--- Print element, use 'scientific' form for float numbers.
--  @param v Value to print.
main._showElt_ = function (v)
  local tp = Ver.mathType(v)
  if tp == 'float' or tp == 'integer' and math.abs(v) >= 1000 then
    return string.format('%.2E', v)  -- 'scientific' format
  else
    return tostring(v)
  end
end

--- Show elements of the table.
--  @param t Table to print.
main._showTable_ = function (t)
  local N, nums = 10, {}
  -- dialog
  local function continue(n)
    io.write(n, ' continue? (y/n) ')
    return string.lower(io.read()) == 'y'
  end
  io.write('\n{ ')
  -- list elements
  for i,v in ipairs(t) do
    io.write(main._showElt_(v), ', ')
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
      io.write('\n', tostring(k), ' = ', main._showElt_(v), ', ')
      count = count + 1
      if count % N == 0 and not continue("") then break end
    end
  end
  io.write(' }\n')
end

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
        main._showTable_(v)
      end
    else
      -- show value
      io.write(main._showElt_(v), '\t')
    end
  end
  io.write('\n')
end
About[Print] = {"Print(...)", "Extenden print function, it shows elements of tables and scientific form of numbers.", SonataHelp.OTHER}

--- Show type of the object.
--  @param v Some Lua or Sonata object.
--  @return String with type value.
function Type(v)
  local u = type(v)
  if u == 'table' then
    u = v.type or u
  elseif u == 'number' then
    u = Ver.mathType(v) 
  end
  return u
end
About[Type] = {'Type(v)', 'Show type of the object.', SonataHelp.OTHER}

-- Methametods for the range of numbers.
local metharange = { type = 'range' }
metharange.__index = metharange

--- Add number (shift range).
--  @param d Any number.
--  @param R Range object.
--  @return Shifted range table.
metharange.__add = function (d, R)
  if type(R) == 'number' then
    return metharange.__add(R, d)
  else
    local res = {}
    for _,v in ipairs(R) do res[#res+1] = v + d end
    return setmetatable(res, metharange)
  end
end

--- Multiply to number (expand range).
--  @param d Any number.
--  @param R Range object.
--  @return Expanded range table.
metharange.__mul = function (d, R)
  if type(R) == 'number' then 
    return metharange.__mul(R, d)
  else 
    local res = {}
    for _, v in ipairs(R) do res[#res+1] = v * d end
    return setmetatable(res, metharange)
  end
end

--- Pretty print.
--  @param R Range object.
--  @return String with the table representation.
metharange.__tostring = function (R)
  return string.format("{%s}", 
    (#R <= 3) and table.concat(R, ',') 
               or string.format("%d,%d..%d", R[1], R[2], R[#R]))
end

--- Generate sequence of values.
--  @param dBegin Beginning of range.
--  @param dEnd End of range.
--  @param dStep Step value (default is 1).
--  @return Table with numbers, Range object.
Range = function (dBegin, dEnd, dStep)
  dStep = dStep or (dEnd > dBegin) and 1 or -1
  assert((dEnd - dBegin) * dStep > 0)
  local res = {}
  for t = dBegin,dEnd,dStep do res[#res+1] = t end
  if math.abs(res[#res] - dEnd) >= math.abs(dStep)*0.1 then
    res[#res+1] = dEnd 
  end
  return setmetatable(res, metharange)
end
About[Range] = {'Range(dBegin,dEnd[,dStep])','Generate table with sequence of numbers.', SonataHelp.OTHER}

--- Generate list of function values.
--  @param fn Function to apply.
--  @param t Table with arguments.
--  @return Table with result of evaluation.
Map = function (fn, t)
  local res = {}
  for _,v in ipairs(t) do res[#res+1] = fn(v) end
  return res
end
About[Map] = {'Map(fn,t)','Evaluate function for each table element.', SonataHelp.OTHER}

-- "In the game of life the strong survive..." (Scorpions) ;)
--  board - matrix with 'ones' as live cells
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

--- Save Lua table in file, use given delimiter.
--  @param sFile File name.
--  @param t Lua table.
--  @param char Delimiter, default is coma.
--  @param bCol Flag, reading elements by columns.
DsvWrite = function (sFile, t, char, bCol)
  local f = assert(io.open(sFile,'w'))
  char = char or ','
  if bCol then
    -- by columns
    if type(t[1]) == 'table' then
      for r = 1,#t[1] do
        local tmp = {}
        for c = 1,#t do tmp[#tmp+1] = t[c][r] end
        f:write(table.concat(tmp,char),'\n')
      end
    else
      for r = 1,#t do f:write(t[i],'\n') end
    end
  else
    -- by rows
    for _,v in ipairs(t) do
      if type(v) == 'table' then v = table.concat(v,char) end
      f:write(v,'\n')
    end
  end
  f:close()
  io.write('Done\n')
end
About[DsvWrite] = {"DsvWrite(sFile,t[,char=',',bCol=false])", "Save Lua table as delimiter separated data into file.", FILES}

--- Import data from text file, use given delimiter.
--  @param sFile File name.
--  @param char Delimiter, default is coma.
--  @param bCol Flag, reading elements by columns.
--  @return Lua table with data.
DsvRead = function (sFile, char, bCol)
  local f = assert(io.open(sFile, 'r'))
  char = char or ','
  local templ = '([^'..char..']+)'
  local res = {}
  for s in f:lines('l') do
    -- read data
    if char ~= '#' then
      s = string.match(s, '([^#]+)')    -- skip comments
    end
    s = string.match(s,'^%s*(.*)%s*$')  -- strip line
    if #s > 0 then
      local tmp = {}
      -- parse string
      for p in string.gmatch(s, templ) do
        tmp[#tmp+1] = tonumber(p) or p
      end
      -- save
      if bCol then
        if #res == 0 then  -- initialize
          for i = 1,#tmp do res[#res+1] = {} end
        end
        for i = 1, #tmp do table.insert(res[i], tmp[i]) end
      else
        res[#res+1] = tmp
      end
    end
  end
  f:close()
  return res
end
About[DsvRead] = {"DsvRead(sFile[,delim=',',bCol=false])", "Read delimiter separated data as Lua table.", FILES}

TblImport = SonataHelp.tblImport
About[TblImport] = {"TblImport(sFile)", "Import Lua table, saved into file.", FILES}


--- Execute file inside the interpreter.
--  @param sFile Lua or note file name.
Run = function (sFile)
  if string.find(sFile, '%.lua$') then
    dofile(sFile)
  elseif string.find(sFile, '%.note$') then
    Sonata:note(sFile, false)
  else
    io.write('Expected .lua or .note!\n')
  end
end
About[Run] = {'Run(sFile)', "Execute lua- or note-file.", SonataHelp.OTHER}

-- save link to help info
main.about = About

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
