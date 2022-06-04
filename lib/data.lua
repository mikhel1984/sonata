--[[		sonata/lib/data.lua 

--- Data processing and statistics.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2022.

	module 'data'
--]]

-------------------- Tests -------------------
--[[TEST

-- use 'data'
Data = require 'lib.data'
-- external dependencies, can be loaded implicitly
require 'lib.special'
require 'lib.matrix'

-- initial data (tables)
X = {3,2,5,6,3,4,3,1}
-- weight
w = {1,1,0}
-- enought to define w[i] ~= 1
w[5] = 2; w[6] = 2
-- average
ans = Data.mean(X)           --3> 3.375

-- standard deviation
ans, tmp = Data.std(X,W)     --3> 1.495

-- variance
ans = tmp                    --3> 2.234

-- covariance for two vectors
Y = {0,2,1,3,7,5,8,4}
a = Data.cov2(X,Y)
ans = a                      --3> -0.65625

-- covariance matrix 
tmp = Data.cov({X,Y})
ans = tmp[1][2]              --3> a

-- maximum element and index
_,ans = Data.max(X)           --> 4 

-- median
ans = Data.median(X)          --> 3

-- table of frequencies
tmp = Data.freq(X)
ans = tmp[3]                  --> 3

-- central moment
ans = Data.moment(2,X)       --3> 2.234

-- summ of elements
ans = Data.sum(X)             --> 27

-- minimum value
ans = Data.min(X)             --> 1

-- geometrical mean
ans = Data.geomean(X)        --3> 2.995

-- harmonic mean
ans = Data.harmmean(X,W)     --3> 2.567

-- check if X[i] > 2
a = Data.is(X, Data.xGt(2))
ans = a[1]                    --> 1

-- get elements X[i] > 2
tmp = Data.filter(X, a)
ans = tmp[1]                  --> X[1]

-- filtration using explicit function 
fn = function (x) return x > 2 end
tmp = Data.filter(X, fn)
ans = tmp[1]                  --> X[1]

-- generate new list
-- use 'lazy' function definition
fn = Data.Fn "{x1-x2,x1+x2}"
tmp = Data.zip(fn, X, Y)
ans = tmp[1][2]               --> X[1]+Y[1]

-- find histogram
a,b = Data.histcounts(X, 3)
ans = b[1]                    --> 2.25

-- define edges 
a,b = Data.histcounts(X,{2,4,7})
ans = a[1]                    --> 1

-- table range reference 
a = Data.ref(X, 3, 6)
ans = #a                      --> 4

ans = a[1]                    --> X[3]

-- Student cdf and pdf
ans = Data.tcdf(4, 2.5)      --3> 0.9805

ans = Data.tpdf(2, 3.3)      --3> 0.0672

-- dsv write 
nm = os.tmpname()
-- separate elements with ';'
t = {{1,2,3},{4,5,6}}
Data.csvwrite(nm, t, ';')

-- dsv read
-- with separator ';'
tt = Data.csvread(nm, ';')
ans = tt[2][2]                --> 5

-- csv write by columns
Data.csvwrite(nm, t, ',', true)

-- csv read by columns
tt = Data.csvread(nm, ',', true)
ans = tt[1][3]                --> 3

--]]

--	LOCAL

local Ver = require("lib.utils")
local Cross = Ver.cross
Ver = Ver.versions

local STAT = 'statistics'
local FILES = 'files'
local FILTER = 'filter'

--	INFO

local help = SonataHelp or {new=function () return {} end}
-- description
local about = help:new("Data processing and statistics.")

--	MODULE

local data = {}

--- Estimate covariance for two vectors.
--  @param t1 First data vector.
--  @param t2 Second data vector.
--  @return Covariance value.
data.cov2 = function (t1,t2)
  if #t1 ~= #t2 then error("Different vector size") end
  local m1 = data.mean(t1)
  local m2 = data.mean(t2) 
  local s = 0
  for i = 1, #t1 do
    s = s + (t1[i] - m1)*(t2[i] - m2)
  end
  return s / #t1
end
about[data.cov2] = {"cov2(t1,t2)", "Find covariance value for two vectors.", STAT}

--- Estimate covariance matrix.
--  @param t Table of data vectors.
--  @return Covariance matrix.
data.cov = function (t)
  local N = #t
  if N == 0 then error("Expected list of vectors") end
  data.ext_matrix = data.ext_matrix or require('lib.matrix')
  local m = data.ext_matrix.zeros(N, N) 
  for i = 1, N do
    local ti = t[i]
    m[i][i] = data.cov2(ti, ti)
    for j = i+1, N do
      m[i][j] = data.cov2(ti, t[j])
      m[j][i] = m[i][j]
    end
  end
  return m
end
about[data.cov] = {"cov(t)", "Find covariance matrix for list of vectors.", STAT}

--- Save Lua table in file, use given delimiter.
--  @param sFile File name.
--  @param t Lua table.
--  @param char Delimiter, default is coma.
--  @param bCol Flag, reading elements by columns.
data.csvwrite = function (sFile, t, char, bCol)
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
about[data.csvwrite] = {"csvwrite(sFile,t,[char=',',bCol=false])", "Save Lua table as delimiter separated data into file.", FILES}

--- Import data from text file, use given delimiter.
--  @param sFile File name.
--  @param char Delimiter, default is coma.
--  @param bCol Flag, reading elements by columns.
--  @return Lua table with data.
data.csvread = function (sFile, char, bCol)
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
about[data.csvread] = {"csvread(sFile,[delim=',',bCol=false])", "Read delimiter separated data as Lua table.", FILES}

--- Generate function from string.
--  @param sExpr Expression for execution.
--  @param iArg Number of arguments (optional).
--  @return Function based on the expression.
data.Fn = function (sExpr,iArg)
  local arg = {}
  for i = 1, (iArg or 2) do arg[i] = string.format("x%d", i) end
  local fn = Ver.loadStr(
    string.format("return function (%s) return %s end", table.concat(arg,','), sExpr)
  )
  return fn()
end
about[data.Fn] = {"Fn(sExpr,[iArg=2])", "Generate function from expression of x1, x2 etc.", help.OTHER}

--- Find elements using condition.
--  @param t Table with data.
--  @param vCond Either boolean function or table of weights.
--  @return Table with the filtered elements.
data.filter = function (t, vCond)
  local res = {}
  if type(vCond) == 'function' then
    -- boolean function
    for i = 1, #t do
      local v = t[i] 
      if fn(v) then res[#res+1] = v end
    end
  elseif type(vCond) == 'table' then
    -- weights 
    for i = 1, #t do
      if vCond[i] ~= 0 then res[#res+1] = t[i] end
    end
  end
  return res
end
about[data.filter] = {"filter(t,vCond)","Get result of the table filtering. Condition is either boolean function or table of weights.", FILTER}

--- Frequency of elements.
--  @param t Table of numbers.
--  @return Table where keys are elements and values are their frequencies.
data.freq = function (t)
  local tmp = {}
  for _, v in ipairs(t) do
    tmp[v] = (tmp[v] or 0) + 1
  end
  return tmp
end
about[data.freq] = {"freq(t)", "Return table with frequencies of elements.", STAT}

--- Geometrical mean.
--  @param t Table of numbers.
--  @param tw Table of weights, optional.
--  @return Geometrical mean.
data.geomean = function (t, tw)
  if tw then
    local st, sw = 0, 0
    for i = 1,#t do
      local w = tw[i] or 1
      st = st + w*math.log(t[i])
      sw = sw + w
    end
    return math.exp(st / sw)
  else
    local p = 1
    for i = 1, #t do p = p * t[i] end
    return p^(1/#t)
  end
end
about[data.geomean] = {"geomean(t,[tw])", "Geometrical mean.", STAT}

--- Harmonic mean.
--  @param t Table of numbers.
--  @param tw Table of weights. Can be omitted.
--  @return Harmonic mean.
data.harmmean = function (t, tw)
  if tw then
    local st, sw = 0, 0
    for i = 1,#t do
      local w = tw[i]
      st = st + w/t[i]
      sw = sw + w
    end
    return sw / st
  else
    local h = 0
    for i = 1, #t do h = h + 1/t[i] end
    return #t / h
  end
end
about[data.harmmean] = {"harmmean(t,[tw])", "Harmonic mean.", STAT}

--- Number of elements in each bin.
--  @param t Data table.
--  @param rng Number of bins or table with edges.
--  @return Two tables, with sum and edges.
data.histcounts = function (t, rng)
  rng = rng or 10 
  local bins
  -- make copy and sort
  local y = Ver.move(t,1,#t,1,{})
  table.sort(y)
  -- prepare edges
  if type(rng) == 'number' then
    local vMin, vMax = y[1], y[#y]
    local wid = (vMax - vMin) / (rng - 1)
    bins = {}
    for v = vMin + 0.5*wid, vMax, wid do bins[#bins+1] = v end
  elseif type(rng) == 'table' then    
    bins = rng
  else
    error("Expected number or table")
  end
  -- check order
  for i = 2,#bins do 
    if bins[i] <= bins[i-1] then error("Wrong order") end
  end  
  -- fill result
  local res = {}
  for i = 1,#bins+1 do res[i] = 0 end
  local p, i = 1, 1 
  while i <= #y do
    local v = y[i]
    if p > #bins or v < bins[p] then
        res[p] = res[p] + 1
        i = i + 1
    else
      p = p + 1
    end
  end
  return res, bins
end
about[data.histcounts] = {"histcounts(X,[rng=10])","Calculate amount of bins. Edges can be either number or table.", STAT}

--- Find weights (1/0) based on condition.
--  @param t Data table.
--  @param fn Condition, boolean function.
--  @return Table of 1 and 0.
data.is = function (t, fn)
  local res = {}
  for i = 1, #t do
    res[i] = fn(t[i]) and 1 or 0
  end
  return res
end
about[data.is] = {"is(t,fn)", "Find weights using boolean function.", FILTER}

--- Find weights (1/0) based on inverted condition.
--  @param t Data table.
--  @param fn Condition, boolean function.
--  @return Table of 1 and 0.
data.isNot = function (t, fn)
  local res = {}
  for i = 1, #t do
    res[i] = fn(t[i]) and 0 or 1
  end
  return res
end
about[data.isNot] = {"isNot(t,fn)", "Find inverted weights using boolean function.", FILTER}

--- Maximum value.
--  @param t Table of numbers.
--  @return Maximum value and its index.
data.max = function (t)
  local m, k = t[1], 1
  for i = 2, #t do
    if t[i] > m then m, k = t[i], i end
  end
  return m,k
end
about[data.max] = {"max(t)", "Maximal element and its index.", STAT}

--- Average value.
--  @param t Table with numbers.
--  @param tw Table with weight. Can be omitted.
--  @return Average.
data.mean = function (t, tw)
  if tw then
    local st, sw = 0, 0
    for i = 1, #t do
      local w = tw[i] or 1
      st = st + t[i]*w
      sw = sw + w
    end
    return st / sw
  else
    return data.sum(t) / #t
  end
end
about[data.mean] = {"mean(t,[tw])", "Calculate average value. Weights can be used.", STAT}

--- Find median.
--  @param t Table of numbers.
--  @return Value of median.
data.median = function (t)
  local len = #t
  local y = Ver.move(t,1,len,1,{})
  table.sort(y)
  if len % 2 == 1 then 
    return y[(len+1)/2]
  else
    len = len / 2
    return (y[len] + y[len+1]) * 0.5
  end
end
about[data.median] = {"median(t)", "Median of the list.", STAT}

--- Minimum value.
--  @param t Table of numbers.
--  @return Minimum value and its index.
data.min = function (t)
  local m, k = t[1], 1
  for i = 2, #t do
    if t[i] < m then m, k = t[i], i end
  end
  return m,k
end
about[data.min] = {"min(t)", "Minimal element and its index.", STAT}

--- Central moment.
--  @param N Order of the moment.
--  @param t Table of numbers.
--  @param tw Table of weights. Can be omitted.
--  @return Central moment value.
data.moment = function (N, t, tw)
  local m, n = 0, 0
  for i = 1,#t do
    local w = tw and tw[i] or 1
    m = m + w * t[i]
    n = n + w
  end
  m = m / n
  local mu = 0
  for i = 1,#t do
    mu = mu + (tw and tw[i] or 1) * (t[i]-m)^N
  end
  return mu / n
end
about[data.moment] = {"moment(N,t,[tw])", "Central moment of t order N, tw is a list of weights.", STAT}


--- Sum of all elements.
--  @param t Table with numbers.
--  @return Sum.
data.sum = function (t)
  local s = 0
  for i = 1, #t do s = s+t[i] end
  return s
end
about[data.sum] = {"sum(t)", "Get sum of all elements.", help.OTHER}

--- Standard deviation and variance.
--  @param t Table of numbers.
--  @param tw Table of weights.
--  @return Standard deviation, variance.
data.std = function (t, tw)
  local mean = data.mean(t,tw)
  local disp = 0
  if tw then
    local sw = 0
    for i = 1,#t do
      local w = tw[i] or 1
      disp = disp + w*(t[i]-mean)^2 
      sw = sw + w
    end
    disp = disp / sw
  else
    for i = 1,#t do disp = disp + (t[i]-mean)^2 end
    disp = disp / #t
  end
  return math.sqrt(disp), disp 
end
about[data.std] = {"std(t,[tw])", "Standard deviation and variance. Weights can be used.", STAT}

--- Student's cumulative distribution
--   @param d Value.
--   @param N Degree of freedom.
--   @return Cumulative value.
data.tcdf = function (d,N)
  data.ext_special = data.ext_special or require('lib.special')
  local tmp = N/(N+d*d)
  return 1-0.5*data.ext_special.betainc(tmp,0.5*N,0.5)
end
about[data.tcdf] = {"tcdf(d,N)", "Student's cumulative distribution.", help.OTHER}

--- Student's density function.
--   @param d Value.
--   @param N Degree of freedom.
--   @return Density value.
data.tpdf = function (d,N)
  data.ext_special = data.ext_special or require('lib.special')
  local tmp = math.sqrt(N)*data.ext_special.beta(0.5,0.5*N)
  return (1+d*d/N)^(-0.5*(N+1))/tmp
end
about[data.tpdf] = {"tpdf(d,N)", "Student's distribution density.", help.OTHER}

--- Condition: x == d.
--  @param d Some value.
--  @return Boolean function.
data.xEq = function (d)
  return (function (x) return Cross.eq(x,d) end)
end
about[data.xEq] = {"xEq(d)", "Return function for condition x == d.", FILTER}

--- Condition: x > d
--  @param d Lower limit.
--  @return Boolean function.
data.xGt = function (d)
  return (function (x) return x > d end)
end
about[data.xGt] = {"xGt(d)", "Return function for condition x > d.", FILTER}

--- Condition: d1 <= x <= d2.
--  @param d1 Lower limit.
--  @param d2 Upper limit.
--  @return Boolean function.
data.xIn = function (d1, d2)
  return (function (x) return d1 <= x and x <= d2 end)
end
about[data.xIn] = {"xIn(d1,d2)", "Return function for condition d1 <= x <= d2.", FILTER}

--- Condition: x < d
--  @param d Upper limit. 
--  @return Boolean function.
data.xLt = function (d)
  return (function (x) return x < d end)
end
about[data.xLt] = {"xLt(d)", "Return function for condition x < d.", FILTER}

--- Apply function of n arguments to n lists.
--  @param fn Function of multiple arguments.
--  @param ... Sequence of lists.
--  @return List of values fn(...).
data.zip = function (fn, ...)
  local ag, res = {...}, {}
  local x, stop = {}, false
  for i = 1, math.huge do
    for j = 1, #ag do
      local v = ag[j][i]
      if v ~= nil then
        x[j] = v
      else 
        stop = true
        break 
      end
    end
    if not stop then 
      res[i] = fn(Ver.unpack(x))
    else 
      break 
    end
  end
  return res 
end
about[data.zip] = {"zip(fn,...)", "Sequentially apply function to list of tables.", help.OTHER}

-- Get reference to data range in other table
local metaref = { type = 'ref' }

--- Number of elements in range.
--  @param self Ref object.
--  @return Length of the reference table.
metaref.__len = function (self)
  return self._end - self._beg
end

--- Get i-th element.
--  @param self Ref object.
--  @param i Element index.
--  @return Table value.
metaref.__index = function (self, i)
  if Ver.isInteger(i) then
    local n = i + self._beg 
    if n > self._beg and n <= self._end then
      return self._t[n]
    end
  end
  return metaref[i]
end

-- Block setting
metaref.__newindex = function (self, k, v)
  -- do nothing
end

--- Create reference to other table.
--  @param t Source table.
--  @param iBeg Index of the first element.
--  @param iEnd Index of the last element.
--  @return Reference object. 
data.ref = function (t, iBeg, iEnd)
  iBeg = iBeg or 1
  iEnd = iEnd or #t
  assert(Ver.isInteger(iBeg) and Ver.isInteger(iEnd), "Wrong index type")
  return setmetatable({_beg=iBeg-1, _end=iEnd, _t=t}, metaref)
end
about[data.ref] = {'ref(t,[iBeg=1,iEnd=#t])', 'Return reference to the range of elements.', help.OTHER}

-- Comment to remove descriptions
data.about = about

return data

--====================================
--TODO: add tinv
