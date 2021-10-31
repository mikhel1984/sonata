--[[		sonata/lib/stat.lua 

--- Some statistical functions.
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2021.

	module 'stat'
--]]

-------------------- Tests -------------------
--[[TEST

-- use 'stat'
Stat = require 'lib.stat'
-- external dependencies, can be loaded implicitly
require 'lib.special'

-- initial data (tables)
X = {3,2,5,6,3,4,3,1}
w = {1,1,0,1,2,2,1,1}
-- average
ans = Stat.mean(X)           --3> 3.375

-- standard deviation
ans, tmp = Stat.std(X,W)     --3> 1.495

-- variance
ans = tmp                    --3> 2.234

-- maximum element and index
_,ans = Stat.max(X)           --> 4 

-- median
ans = Stat.median(X)          --> 3

-- table of frequencies
tmp = Stat.freq(X)
ans = tmp[3]                  --> 3

-- central moment
ans = Stat.moment(2,X)       --3> 2.234

-- summ of elements
ans = Stat.sum(X)             --> 27

-- minimum value
ans = Stat.min(X)             --> 1

-- geometrical mean
ans = Stat.geomean(X)        --3> 2.995

-- harmonic mean
ans = Stat.harmmean(X,W)     --3> 2.567

-- find histogram
a,b = Stat.histcounts(X, 3)
ans = b[1]                    --> 1

-- define edges 
a,b = Stat.histcounts(X,{0,4,7})  
ans = a[1]                    --> 5 

-- Student cdf and pdf
ans = Stat.tcdf(4, 2.5)      --3> 0.9805

ans = Stat.tpdf(2, 3.3)      --3> 0.0672

--]]

--	LOCAL

local Ver = require("lib.utils").versions

local DISTRIB = 'distribution'

--	INFO

local help = SonataHelp and (require "core.help") or {new=function () return {} end}

--	MODULE

local stat = {
-- description
about = help:new("Statistical calculations. Data set must be a Lua table.")
}

--- Sum of all elements.
--  @param t Table with numbers.
--  @return Sum.
stat.sum = function (t)
  local s = 0
  for i = 1, #t do s = s+t[i] end
  return s
end
stat.about[stat.sum] = {"sum(t)", "Get sum of all elements.", help.OTHER}

--- Average value.
--  @param t Table with numbers.
--  @param w Table with weight. Can be omitted.
--  @return Average.
stat.mean = function (t, w)
  if w then
    local st, sw = 0, 0
    for i = 1, #t do
      st = st + t[i]*w[i]
      sw = sw + w[i]
    end
    return st / sw
  else
    return stat.sum(t) / #t
  end
end
stat.about[stat.mean] = {"mean(t[,w])", "Calculate average value. Weights can be used.", }

--- Standard deviation and variance.
--  @param t Table of numbers.
--  @param w Table of weights.
--  @return Standard deviation, variance.
stat.std = function (t, w)
  local mean = stat.mean(t,w)
  local disp = 0
  if w then
    local sw = 0
    for i = 1,#t do
      disp = disp + w[i]*(t[i]-mean)^2 
      sw = sw + w[i]
    end
    disp = disp / sw
  else
    for i = 1,#t do disp = disp + (t[i]-mean)^2 end
    disp = disp / #t
  end
  return math.sqrt(disp), disp 
end
stat.about[stat.std] = {"std(t[,w])", "Standard deviation and variance. Weights can be used.", }

--- Maximum value.
--  @param t Table of numbers.
--  @return Maximum value and its index.
stat.max = function (t)
  local m, k = t[1], 1
  for i = 2, #t do
    if t[i] > m then m, k = t[i], i end
  end
  return m,k
end
stat.about[stat.max] = {"max(t)", "Maximal element and its index.", help.OTHER}

--- Minimum value.
--  @param t Table of numbers.
--  @return Minimum value and its index.
stat.min = function (t)
  local m, k = t[1], 1
  for i = 2, #t do
    if t[i] < m then m, k = t[i], i end
  end
  return m,k
end
stat.about[stat.min] = {"min(t)", "Minimal element and its index.", help.OTHER}

--- Geometrical mean.
--  @param t Table of numbers.
--  @param w Table of weights. Can be omitted.
--  @return Geometrical mean.
stat.geomean = function (t, w)
  if w then
    local st, sw = 0, 0
    for i = 1,#t do 
      st = st + w[i]*math.log(t[i])
      sw = sw + w[i]
    end
    return math.exp(st / sw)
  else
    local p = 1
    for i = 1, #t do p = p * t[i] end
    return p^(1/#t)
  end
end
stat.about[stat.geomean] = {"geomean(t[,w])", "Geometrical mean.", help.OTHER}

--- Harmonic mean.
--  @param t Table of numbers.
--  @param w Table of weights. Can be omitted.
--  @return Harmonic mean.
stat.harmmean = function (t, w)
  if w then
    local st, sw = 0, 0
    for i = 1,#t do
      st = st + w[i]/t[i]
      sw = sw + w[i]
    end
    return sw / st
  else
    local h = 0
    for i = 1, #t do h = h + 1/t[i] end
    return #t / h
  end
end
stat.about[stat.harmmean] = {"harmmean(t[,w])", "Harmonic mean.", help.OTHER}

--- Find median.
--  @param p Table of numbers.
--  @return Value of median.
stat.median = function (p)
  local len = #p
  local t = Ver.move(p,1,len,1,{})
  table.sort(t)
  if len % 2 == 1 then 
    return t[(len+1)/2]
  else
    len = len / 2
    return (t[len] + t[len+1]) * 0.5
  end
end
stat.about[stat.median] = {"median(t)", "Median of the list."}

--- Frequency of elements.
--  @param t Table of numbers.
--  @return Table where keys are elements and values are their frequencies.
stat.freq = function (t)
  local tmp = {}
  for _, v in ipairs(t) do
    tmp[v] = (tmp[v] or 0) + 1
  end
  return tmp
end
stat.about[stat.freq] = {"freq(t)", "Return table with frequencies of elements.", }

--- Central moment.
--  @param n Order of the moment.
--  @param x Table of numbers.
--  @param p Table of weights. Can be omitted.
--  @return Central moment value.
stat.moment = function (n, x, p)
  local pk, m = 1/#x, 0
  for i = 1,#x do m = m + x[i]*(p and p[i] or pk) end
  local mu = 0
  for i = 1,#x do mu = mu + (x[i]-m)^n*(p and p[i] or pk) end
  return mu
end
stat.about[stat.moment] = {"moment(n,x[,p])", "Central moment of x order n, p is a list of weights.", }

--- Number of elements in each bin.
--  @param x Data table.
--  @param rng Number of bins or table with edges.
--  @return Two tables, with sum and edges.
stat.histcounts = function (x, rng)
  rng = rng or 10 
  local bins
  -- make copy and sort
  local t = Ver.move(x,1,#x,1,{})
  table.sort(t)
  -- prepare edges
  if type(rng) == 'number' then
    local vMin, vMax = t[1], t[#t]
    bins = {}
    for v = vMin, vMax, (vMax-vMin)/rng do bins[#bins+1] = v end  
    bins[#bins] = vMax+(vMax-vMin)*0.001      
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
  for i = 1,#bins-1 do res[i] = 0 end
  --for i = 1, #bins-1 do res[i] = 0 end
  local p,upper = 1, bins[2]
  for _,v in ipairs(t) do    
    if v < bins[1] then
      -- just skip
    elseif v < upper then
      res[p] = res[p]+1
    else
      -- next bin
      while bins[p+1] and v >= bins[p+1] do p = p+1 end
      if p >= #bins then break end
      upper = bins[p+1]
      res[p] = res[p]+1
    end
  end
  return res, bins
end
stat.about[stat.histcounts] = {"histcounts(X[,rng=10])","Calculate amount of bins. Edges can be either number or table."}


--- Student's cumulative distribution
--   @param x Value.
--   @param nu Degree of freedom.
--   @return Cumulative value.
stat.tcdf = function (x,nu)
  stat.ext_special = stat.ext_special or require('lib.special')
  local tmp = nu/(nu+x*x)
  return 1-0.5*stat.ext_special.betainc(tmp,0.5*nu,0.5)
end
stat.about[stat.tcdf] = {"tcdf(x,nu)", "Student's cumulative distribution.", DISTRIB}

--- Student's density function.
--   @param x Value.
--   @param nu Degree of freedom.
--   @return Density value.
stat.tpdf = function (x,nu)
  stat.ext_special = stat.ext_special or require('lib.special')
  local tmp = math.sqrt(nu)*stat.ext_special.beta(0.5,0.5*nu)
  return (1+x*x/nu)^(-0.5*(nu+1))/tmp
end
stat.about[stat.tpdf] = {"tpdf(x,nu)", "Student's distribution density.", DISTRIB}

-- Uncomment to remove descriptions
--stat.about = nil

return stat

--====================================
--TODO: add tinv
