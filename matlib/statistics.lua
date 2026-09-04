--[[		sonata/matlib/statistics.lua

--- Elements of statistics.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2026.

	module 'statistics'
--]]

-------------------- Tests -------------------
--[[TEST_IT

-- use 'statistics'
Stat = require 'matlib.statistics'
-- external dependencies, can be loaded implicitly
require 'matlib.matrix'
require 'matlib.asciiplot'

-- initial data (tables)
X = {3,2,5,6,3,4,3,1}
-- weight
W = {1,1,0}
-- enought to define w[i] ~= 1
W[5] = 2; W[6] = 2
-- average
ans = Stat:mean(X)          --.3>  3.375

-- standard deviation
ans = Stat:std(X,W)         --.3>  1.314

-- covariance matrix
Y = {0,2,1,3,7,5,8,4}
tmp = Stat:cov({X,Y})
ans = tmp[1][2]             --.2>  -0.656

-- correlation
ans = Stat:corr(X, Y)       --.2>  -0.166

-- maximum element and index
_,ans = Stat:max(X)           -->  4

-- median
ans = Stat:median(X)          -->  3

-- table of frequencies
tmp = Stat:freq(X)
ans = tmp[3]                  -->  3

-- central moment
ans = Stat:moment(X,2)      --.3>  2.234

-- summ of elements
ans = Stat:sum(X)             -->  27

-- minimum value
ans = Stat:min(X)             -->  1

-- geometrical mean
ans = Stat:geomean(X)       --.3>  2.995

-- harmonic mean
ans = Stat:harmmean(X,W)    --.3>  2.571

-- find histogram
a,b = Stat:histcounts(X, 3)
ans = b[1]                    -->  2.25

-- define edges
a,b = Stat:histcounts(X,{2,4,7})
ans = a[1]                    -->  1

-- show histogram
print(Stat:histPlot(X, {2, 4, 7}))


--]]


--	LOCAL

-- dependencies
local _ext = {
  utils = require("matlib.utils"),
  -- matrix = require("matlib.matrix"),  -- covariance
  -- ap = require("matlib.asciiplot"),  -- histPlot
}

local _ver = _ext.utils.versions

--	INFO

local _help = SonataHelp or {}  -- optional
-- description
local _about = {
__module__ = "Elements of statistics."
}


--	MODULE

local statistics = { }


--- Estimate covariance for two vectors.
--  @param ii First list index.
--  @param jj Second list index.
--  @param ts List of lists.
--  @param ms List of means.
--  @return Covariance value.
statistics._cov2 = function (_, ii, jj, ts, ms)
  local t1, t2 = ts[ii], ts[jj]
  if #t1 ~= #t2 then
    error "Different vector size"
  end
  if #t1 == 0 then
    error "Empty vector"
  end
  local m1, m2, s = ms[ii], ms[jj], 0
  for i = 1, #t1 do
    s = s + (t1[i] - m1)*(t2[i] - m2)
  end
  return s / #t1
end


--- Estimate correlation for two lists.
--  @param t1 First data list.
--  @param t2 Second data list.
--  @return correlation value.
statistics.corr = function(_, t1, t2)
  if #t1 ~= #t2 then
    error "Different vector size"
  end
  local m1 = statistics:mean(t1)
  local m2 = statistics:mean(t2)
  local s12, s11, s22 = 0, 0, 0
  for i = 1, #t1 do
    local d1, d2 = t1[i]-m1, t2[i]-m2
    s12 = s12 + d1*d2
    s11 = s11 + d1*d1
    s22 = s22 + d2*d2
  end
  return s12 / math.sqrt(s11*s22)
end
_about[statistics.corr] = {":corr(xs_t, ys_t) --> float",
  "Find correlation for two vectors."}



--- Estimate covariance matrix.
--  @param t Table of data vectors.
--  @return Covariance matrix.
statistics.cov = function (_, t)
  local N = #t
  if N == 0 then
    error "Expected list of vectors"
  end
  _ext.matrix = _ext.matrix or require("matlib.matrix")
  local m = _ext.matrix:zeros(N, N)
  local avg = {}
  for i = 1, N do avg[i] = statistics:mean(t[i]) end
  for i = 1, N do
    m[i][i] = statistics:_cov2(i, i, t, avg)
    for j = i+1, N do
      m[i][j] = statistics:_cov2(i, j, t, avg)
      m[j][i] = m[i][j]
    end
  end
  return m
end
_about[statistics.cov] = {":cov(data_t) --> cov_M",
  "Find covariance matrix for list of vectors."}


--- Frequency of elements.
--  @param t Table of numbers.
--  @return Table where keys are elements and values are their frequencies.
statistics.freq = function (_, t)
  local tmp = {}
  for _, v in ipairs(t) do
    tmp[v] = (tmp[v] or 0) + 1
  end
  return tmp
end
_about[statistics.freq] = {":freq(data_t) --> tbl",
  "Return table with frequencies of elements."}


--- Geometrical mean.
--  @param t Table of numbers.
--  @param tw Table of weights, optional.
--  @return Geometrical mean.
statistics.geomean = function (_, t, tw)
  if tw then
    local st, sw = 0, 0
    for i = 1, #t do
      local w = tw[i] or 1
      st = st + w*math.log(w > 0 and t[i] or 1)
      sw = sw + w
    end
    return math.exp(st / sw)
  else
    local p = 1
    for i = 1, #t do p = p * t[i] end
    return p^(1/#t)
  end
end
_about[statistics.geomean] = {":geomean(data_t, weigh_t=nil) --> num",
  "Geometrical mean."}


--- Harmonic mean.
--  @param t Table of numbers.
--  @param tw Table of weights. Can be omitted.
--  @return Harmonic mean.
statistics.harmmean = function (_, t, tw)
  if tw then
    local st, sw = 0, 0
    for i = 1, #t do
      local w = tw[i] or 1
      st = st + w/(w > 0 and t[i] or 1)
      sw = sw + w
    end
    return sw / st
  else
    local h = 0
    for i = 1, #t do h = h + 1/t[i] end
    return #t / h
  end
end
_about[statistics.harmmean] = {":harmmean(data_t, weigh_t=nil) --> num",
  "Harmonic mean."}


--- Number of elements in each bin.
--  @param t Data table.
--  @param rng Number of bins or table with edges.
--  @return Two tables, with sum and edges.
statistics.histcounts = function (_, t, rng)
  rng = rng or 10
  local bins = nil
  -- make copy and sort
  local y = _ver.move(t, 1, #t, 1, {})
  table.sort(y)
  -- prepare edges
  if type(rng) == "number" then
    local vMin, vMax = y[1], y[#y]
    local wid = (vMax - vMin) / (rng - 1)
    bins = {}
    for v = vMin + 0.5*wid, vMax, wid do bins[#bins+1] = v end
  elseif type(rng) == "table" then
    bins = rng
  else
    error "Expected number or table"
  end
  -- check order
  for i = 2, #bins do
    if bins[i] <= bins[i-1] then error ("Wrong order") end
  end
  -- fill result
  local res = {}
  for i = 1, #bins+1 do res[i] = 0 end
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
_about[statistics.histcounts] = {":histcounts(data_t, edges_t|N=10) --> sum_t, edges_t",
  "Calculate amount of bins. Edges can be either number or table."}


--- Show histogram with asciiplot.
--  @param t Data table.
--  @param rng Number of bins or table with edges.
--  @return asciiplot object.
statistics.histPlot = function (_, t, rng)
  _ext.ap = _ext.ap or require("matlib.asciiplot")
  local res, bins = statistics.histcounts(_, t, rng)
  bins[#bins+1] = "rest"
  local m = statistics.max(_, res)
  local fig = _ext.ap()
  fig:setX {view="min", range={0, m}, fix=true}
  fig:setY {size=#res}
  fig:bar(bins, res)
  return fig
end
_about[statistics.histPlot] = {":histPlot(data_t, edges_t|N=10) --> fig",
  "Find and show histogram."}


--- Maximum value.
--  @param t Table of numbers.
--  @return Maximum value and its index.
statistics.max = function (_, t)
  local m, k = t[1], 1
  for i = 2, #t do
    if t[i] > m then m, k = t[i], i end
  end
  return m, k
end
_about[statistics.max] = {":max(data_t) --> var, ind_N",
  "Maximal element and its index."}


--- Average value.
--  @param t Table with numbers.
--  @param tw Table with weight. Can be omitted.
--  @return Average.
statistics.mean = function (_, t, tw)
  if tw then
    local st, sw = 0, 0
    for i = 1, #t do
      local w = tw[i] or 1
      st = st + t[i]*w
      sw = sw + w
    end
    return st / sw
  else
    return statistics:sum(t) / #t
  end
end
_about[statistics.mean] = {":mean(data_t, wight_t=nil) --> num",
  "Calculate average value. Weights can be used."}


--- Find median.
--  @param t Table of numbers.
--  @return Value of median.
statistics.median = function (_, t)
  local len = #t
  local y = _ver.move(t, 1, len, 1, {})
  table.sort(y)
  if len % 2 == 1 then
    return y[(len+1)/2]
  else
    len = len / 2
    return (y[len] + y[len+1]) * 0.5
  end
end
_about[statistics.median] = {":median(data_t) --> num",
  "Median of the list."}


--- Minimum value.
--  @param t Table of numbers.
--  @return Minimum value and its index.
statistics.min = function (_, t)
  local m, k = t[1], 1
  for i = 2, #t do
    if t[i] < m then m, k = t[i], i end
  end
  return m, k
end
_about[statistics.min] = {":min(data_t) --> var, ind_N",
  "Minimal element and its index."}


--- Central moment.
--  @param t Table of numbers.
--  @param N Order of the moment.
--  @param tw Table of weights. Can be omitted.
--  @return Central moment value.
statistics.moment = function (_, t, N, tw)
  local m, n = 0, 0
  for i = 1, #t do
    local w = tw and tw[i] or 1
    m = m + w * t[i]
    n = n + w
  end
  m = m / n
  local mu = 0
  for i = 1, #t do
    mu = mu + (tw and tw[i] or 1) * (t[i]-m)^N
  end
  return mu / n
end
_about[statistics.moment] = {":moment(data_t, order_N, weigth_t=nil) --> num",
  "Central moment of order N, weights can be defined."}


--- Sum of all elements.
--  @param t Table with numbers.
--  @return Sum.
statistics.sum = function (_, t)
  local s = 0
  for i = 1, #t do s = s+t[i] end
  return s
end
_about[statistics.sum] = {":sum(data_t) --> var",
  "Get sum of all elements."}


--- Standard deviation and variance.
--  @param t Table of numbers.
--  @param tw Table of weights.
--  @return Standard deviation, variance.
statistics.std = function (_, t, tw)
  local mean = statistics:mean(t, tw)
  local disp = 0
  if tw then
    local sw = 0
    for i = 1, #t do
      local w = tw[i] or 1
      disp = disp + w*(t[i]-mean)^2
      sw = sw + w
    end
    disp = disp / sw
  else
    for i = 1, #t do disp = disp + (t[i]-mean)^2 end
    disp = disp / #t
  end
  return math.sqrt(disp)
end
_about[statistics.std] = {":std(data_t, weight_t=nil) --> num",
  "Standard deviation. Weights can be used."}


-- Comment to remove descriptions
statistics.about = _about

return statistics

--======================================

