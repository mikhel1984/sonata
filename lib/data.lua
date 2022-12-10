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
_D = require 'lib.data'
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
ans = _D:mean(X)           --3> 3.375

-- standard deviation
ans, tmp = _D:std(X,W)     --3> 1.495

-- variance
ans = tmp                    --3> 2.234

-- covariance for two vectors
Y = {0,2,1,3,7,5,8,4}
a = _D:cov2(X,Y)
ans = a                      --3> -0.65625

-- covariance matrix
tmp = _D:cov({X,Y})
ans = tmp[1][2]              --3> a

-- maximum element and index
_,ans = _D:max(X)           --> 4

-- median
ans = _D:median(X)          --> 3

-- table of frequencies
tmp = _D:freq(X)
ans = tmp[3]                  --> 3

-- central moment
ans = _D:moment(2,X)       --3> 2.234

-- summ of elements
ans = _D:sum(X)             --> 27

-- minimum value
ans = _D:min(X)             --> 1

-- geometrical mean
ans = _D:geomean(X)        --3> 2.995

-- harmonic mean
ans = _D:harmmean(X,W)     --3> 2.567

-- check if X[i] > 2
a = _D:is(X, _D:xGt(2))
ans = a[1]                    --> 1

-- get elements X[i] > 2
tmp = _D:filter(X, a)
ans = tmp[1]                  --> X[1]

-- filtration using explicit function
fn = function (x) return x > 2 end
tmp = _D:filter(X, fn)
ans = tmp[1]                  --> X[1]

-- generate new list
-- use 'lazy' function definition
tmp = _D:zip("{x1-x2,x1+x2}", X, Y)
ans = tmp[1][2]               --> X[1]+Y[1]

-- find histogram
a,b = _D:histcounts(X, 3)
ans = b[1]                    --> 2.25

-- define edges
a,b = _D:histcounts(X,{2,4,7})
ans = a[1]                    --> 1

-- table range reference
a = _D:ref(X, 3, 6)
ans = #a                      --> 4

ans = a[1]                    --> X[3]

-- Student cdf and pdf
ans = _D:tcdf(4, 2.5)      --3> 0.9805

ans = _D:tpdf(2, 3.3)      --3> 0.0672

-- dsv write
nm = os.tmpname()
-- separate elements with ';'
t = {{1,2,3},{4,5,6}}
_D:csvwrite(nm, t, ';')

-- dsv read
-- with separator ';'
tt = _D:csvread(nm, ';')
ans = tt[2][2]                --> 5

-- csv write by columns
_D:csvwrite(nm, t, ',', true)

-- csv read by columns
tt = _D:csvread(nm, ',', true)
ans = tt[1][3]                --> 3

--]]

--	LOCAL

local Ver = require("lib.utils")
local Cross = Ver.cross
local Utils = Ver.utils
Ver = Ver.versions

local STAT = 'statistics'
local FILES = 'files'
local FILTER = 'filter'

--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Data processing and statistics."
}

--	MODULE

local data = {}

--- Estimate covariance for two vectors.
--  @param self Do nothing.
--  @param t1 First data vector.
--  @param t2 Second data vector.
--  @return Covariance value.
data.cov2 = function (self,t1,t2)
  if #t1 ~= #t2 then error("Different vector size") end
  local m1 = data:mean(t1)
  local m2 = data:mean(t2)
  local s = 0
  for i = 1, #t1 do
    s = s + (t1[i] - m1)*(t2[i] - m2)
  end
  return s / #t1
end
about[data.cov2] = {
  "_D:cov2(t1,t2)", "Find covariance value for two vectors.", STAT}

--- Estimate covariance matrix.
--  @param self Do nothing.
--  @param t Table of data vectors.
--  @return Covariance matrix.
data.cov = function (self, t)
  local N = #t
  if N == 0 then error("Expected list of vectors") end
  data.ext_matrix = data.ext_matrix or require('lib.matrix')
  local m = data.ext_matrix.zeros(N, N)
  for i = 1, N do
    local ti = t[i]
    m[i][i] = data:cov2(ti, ti)
    for j = i+1, N do
      m[i][j] = data:cov2(ti, t[j])
      m[j][i] = m[i][j]
    end
  end
  return m
end
about[data.cov] = {
  "_D:cov(t)", "Find covariance matrix for list of vectors.", STAT}

--- Save Lua table in file, use given delimiter.
--  @param self Do nothing.
--  @param sFile File name.
--  @param t Lua table.
--  @param char Delimiter, default is coma.
--  @param bCol Flag, reading elements by columns.
data.csvwrite = function (self,sFile, t, char, bCol)
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
about[data.csvwrite] = {"_D:csvwrite(sFile,t,[char=',',bCol=false])",
  "Save Lua table as delimiter separated data into file.", FILES}

--- Import data from text file, use given delimiter.
--  @param self Do nothing.
--  @param sFile File name.
--  @param char Delimiter, default is coma.
--  @param bCol Flag, reading elements by columns.
--  @return Lua table with data.
data.csvread = function (self,sFile, char, bCol)
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
about[data.csvread] = {"_D:csvread(sFile,[delim=',',bCol=false])",
  "Read delimiter separated data as Lua table.", FILES}

--- Generate function from string.
--  @param self Do nothing.
--  @param sExpr Expression for execution.
--  @param iArg Number of arguments (optional).
--  @return Function based on the expression.
data.Fn = function (self,sExpr,iArg) return Utils.Fn(sExpr, iArg or 2) end
about[data.Fn] = {"_D:Fn(sExpr,[iArg=2])",
  "Generate function from expression of x1, x2 etc.", help.OTHER}

--- Find elements using condition.
--  @param self Do nothing.
--  @param t Table with data.
--  @param vCond Either boolean function or table of weights.
--  @return Table with the filtered elements.
data.filter = function (self, t, vCond)
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
about[data.filter] = {"_D:filter(t,vCond)",
  "Get result of the table filtering. Condition is either boolean function or table of weights.",
  FILTER}

--- Frequency of elements.
--  @param self Do nothing.
--  @param t Table of numbers.
--  @return Table where keys are elements and values are their frequencies.
data.freq = function (self, t)
  local tmp = {}
  for _, v in ipairs(t) do
    tmp[v] = (tmp[v] or 0) + 1
  end
  return tmp
end
about[data.freq] = {
  "_D:freq(t)", "Return table with frequencies of elements.", STAT}

--- Geometrical mean.
--  @param self Do nothing.
--  @param t Table of numbers.
--  @param tw Table of weights, optional.
--  @return Geometrical mean.
data.geomean = function (self, t, tw)
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
about[data.geomean] = {"_D:geomean(t,[tw])", "Geometrical mean.", STAT}

--- Harmonic mean.
--  @param self Do nothing.
--  @param t Table of numbers.
--  @param tw Table of weights. Can be omitted.
--  @return Harmonic mean.
data.harmmean = function (self, t, tw)
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
about[data.harmmean] = {"_D:harmmean(t,[tw])", "Harmonic mean.", STAT}

--- Number of elements in each bin.
--  @param self Do nothing.
--  @param t Data table.
--  @param rng Number of bins or table with edges.
--  @return Two tables, with sum and edges.
data.histcounts = function (self, t, rng)
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
about[data.histcounts] = {"_D:histcounts(X,[rng=10])",
  "Calculate amount of bins. Edges can be either number or table.", STAT}

--- Find weights (1/0) based on condition.
--  @param self Do nothing.
--  @param t Data table.
--  @param fn Condition, boolean function.
--  @return Table of 1 and 0.
data.is = function (self, t, fn)
  local res = {}
  for i = 1, #t do
    res[i] = fn(t[i]) and 1 or 0
  end
  return res
end
about[data.is] = {
  "_D:is(t,fn)", "Find weights using boolean function.", FILTER}

--- Find weights (1/0) based on inverted condition.
--  @param self Do nothing.
--  @param t Data table.
--  @param fn Condition, boolean function.
--  @return Table of 1 and 0.
data.isNot = function (self, t, fn)
  local res = {}
  for i = 1, #t do
    res[i] = fn(t[i]) and 0 or 1
  end
  return res
end
about[data.isNot] = {
  "_D:isNot(t,fn)", "Find inverted weights using boolean function.", FILTER}

--- Maximum value.
--  @param self Do nothing.
--  @param t Table of numbers.
--  @return Maximum value and its index.
data.max = function (self, t)
  local m, k = t[1], 1
  for i = 2, #t do
    if t[i] > m then m, k = t[i], i end
  end
  return m,k
end
about[data.max] = {"_D:max(t)", "Maximal element and its index.", STAT}

--- Average value.
--  @param self Do nothing.
--  @param t Table with numbers.
--  @param tw Table with weight. Can be omitted.
--  @return Average.
data.mean = function (self, t, tw)
  if tw then
    local st, sw = 0, 0
    for i = 1, #t do
      local w = tw[i] or 1
      st = st + t[i]*w
      sw = sw + w
    end
    return st / sw
  else
    return data:sum(t) / #t
  end
end
about[data.mean] = {
  "_D:mean(t,[tw])", "Calculate average value. Weights can be used.", STAT}

--- Find median.
--  @param self Do nothing.
--  @param t Table of numbers.
--  @return Value of median.
data.median = function (self, t)
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
about[data.median] = {"_D:median(t)", "Median of the list.", STAT}

--- Minimum value.
--  @param self Do nothing.
--  @param t Table of numbers.
--  @return Minimum value and its index.
data.min = function (self, t)
  local m, k = t[1], 1
  for i = 2, #t do
    if t[i] < m then m, k = t[i], i end
  end
  return m,k
end
about[data.min] = {"_D:min(t)", "Minimal element and its index.", STAT}

--- Central moment.
--  @param self Do nothing.
--  @param N Order of the moment.
--  @param t Table of numbers.
--  @param tw Table of weights. Can be omitted.
--  @return Central moment value.
data.moment = function (self, N, t, tw)
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
about[data.moment] = {"_D:moment(N,t,[tw])",
  "Central moment of t order N, tw is a list of weights.", STAT}


--- Sum of all elements.
--  @param self Do nothing.
--  @param t Table with numbers.
--  @return Sum.
data.sum = function (self,t)
  local s = 0
  for i = 1, #t do s = s+t[i] end
  return s
end
about[data.sum] = {"_D:sum(t)", "Get sum of all elements.", help.OTHER}

--- Standard deviation and variance.
--  @param self Do nothing.
--  @param t Table of numbers.
--  @param tw Table of weights.
--  @return Standard deviation, variance.
data.std = function (self, t, tw)
  local mean = data:mean(t,tw)
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
about[data.std] = {"_D:std(t,[tw])",
  "Standard deviation and variance. Weights can be used.", STAT}

--- Student's cumulative distribution
--  @param self Do nothing.
--  @param d Value.
--  @param N Degree of freedom.
--  @return Cumulative value.
data.tcdf = function (self,d,N)
  data.ext_special = data.ext_special or require('lib.special')
  local tmp = N/(N+d*d)
  return 1-0.5*data.ext_special:betainc(tmp,0.5*N,0.5)
end
about[data.tcdf] = {"_D:tcdf(d,N)",
  "Student's cumulative distribution.", help.OTHER}

--- Student's density function.
--  @param self Do nothing.
--  @param d Value.
--  @param N Degree of freedom.
--  @return Density value.
data.tpdf = function (self,d,N)
  data.ext_special = data.ext_special or require('lib.special')
  local tmp = math.sqrt(N)*data.ext_special:beta(0.5,0.5*N)
  return (1+d*d/N)^(-0.5*(N+1))/tmp
end
about[data.tpdf] = {
  "_D:tpdf(d,N)", "Student's distribution density.", help.OTHER}

--- Condition: x == d.
--  @param self Do nothing.
--  @param d Some value.
--  @return Boolean function.
data.xEq = function (self,d)
  return (function (x) return Cross.eq(x,d) end)
end
about[data.xEq] = {
  "_D:xEq(d)", "Return function for condition x == d.", FILTER}

--- Condition: x > d
--  @param self Do nothing.
--  @param d Lower limit.
--  @return Boolean function.
data.xGt = function (self,d)
  return (function (x) return x > d end)
end
about[data.xGt] = {"_D:xGt(d)", "Return function for condition x > d.", FILTER}

--- Condition: d1 <= x <= d2.
--  @param self Do nothing.
--  @param d1 Lower limit.
--  @param d2 Upper limit.
--  @return Boolean function.
data.xIn = function (self, d1, d2)
  return (function (x) return d1 <= x and x <= d2 end)
end
about[data.xIn] = {
  "_D:xIn(d1,d2)", "Return function for condition d1 <= x <= d2.", FILTER}

--- Condition: x < d
--  @param self Do nothing.
--  @param d Upper limit.
--  @return Boolean function.
data.xLt = function (self,d)
  return (function (x) return x < d end)
end
about[data.xLt] = {"_D:xLt(d)", "Return function for condition x < d.", FILTER}

--- Apply function of n arguments to n lists.
--  @param self Do nothing.
--  @param fn Function of multiple arguments or string.
--  @param ... Sequence of lists.
--  @return List of values fn(...).
data.zip = function (self,fn, ...)
  local ag, res = {...}, {}
  if type(fn) == 'string' then fn = Utils.Fn(fn, #ag) end
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
about[data.zip] = {"_D:zip(fn,...)",
  "Sequentially apply function to list of tables.", help.OTHER}

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
--  @param self Do nothing.
--  @param t Source table.
--  @param iBeg Index of the first element.
--  @param iEnd Index of the last element.
--  @return Reference object.
data.ref = function (self, t, iBeg, iEnd)
  iBeg = iBeg or 1
  iEnd = iEnd or #t
  assert(Ver.isInteger(iBeg) and Ver.isInteger(iEnd), "Wrong index type")
  return setmetatable({_beg=iBeg-1, _end=iEnd, _t=t}, metaref)
end
about[data.ref] = {'_D:ref(t,[iBeg=1,iEnd=#t])',
  'Return reference to the range of elements.', help.OTHER}

-- Comment to remove descriptions
data.about = about

return data

--====================================
--TODO: add tinv
