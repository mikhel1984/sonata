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

-- find histogram
a,b = Data.histcounts(X, 3)
ans = b[1]                    --> 1

-- define edges 
a,b = Data.histcounts(X,{0,4,7})  
ans = a[1]                    --> 5

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
Data.writeCsv(nm, t, ';')

-- dsv read
-- with separator ';'
tt = Data.readCsv(nm, ';')
ans = tt[2][2]                --> 5

-- csv write by columns
Data.writeCsv(nm, t, ',', true)

-- csv read by columns
tt = Data.readCsv(nm, ',', true)
ans = tt[1][3]                --> 3

--]]

--	LOCAL

local Ver = require("lib.utils").versions

local DISTRIB = 'distribution'
local FILES = 'files'

--	INFO

local help = SonataHelp or {new=function () return {} end}
-- description
local about = help:new("Data processing and statistics.")

--	MODULE

local data = {}

--- Sum of all elements.
--  @param t Table with numbers.
--  @return Sum.
data.sum = function (t)
  local s = 0
  for i = 1, #t do s = s+t[i] end
  return s
end
about[data.sum] = {"sum(t)", "Get sum of all elements.", help.OTHER}

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
about[data.mean] = {"mean(t[,tw])", "Calculate average value. Weights can be used.", }

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
about[data.std] = {"std(t[,tw])", "Standard deviation and variance. Weights can be used.", }

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
about[data.max] = {"max(t)", "Maximal element and its index.", help.OTHER}

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
about[data.min] = {"min(t)", "Minimal element and its index.", help.OTHER}

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
about[data.geomean] = {"geomean(t[,tw])", "Geometrical mean.", help.OTHER}

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
about[data.harmmean] = {"harmmean(t[,tw])", "Harmonic mean.", help.OTHER}

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
about[data.median] = {"median(t)", "Median of the list."}

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
about[data.freq] = {"freq(t)", "Return table with frequencies of elements.", }

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
about[data.moment] = {"moment(N,t[,tw])", "Central moment of t order N, tw is a list of weights.", }

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
  for _,v in ipairs(y) do    
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
about[data.histcounts] = {"histcounts(X[,rng=10])","Calculate amount of bins. Edges can be either number or table."}


--- Student's cumulative distribution
--   @param d Value.
--   @param N Degree of freedom.
--   @return Cumulative value.
data.tcdf = function (d,N)
  data.ext_special = data.ext_special or require('lib.special')
  local tmp = N/(N+d*d)
  return 1-0.5*data.ext_special.betainc(tmp,0.5*N,0.5)
end
about[data.tcdf] = {"tcdf(d,N)", "Student's cumulative distribution.", DISTRIB}

--- Student's density function.
--   @param d Value.
--   @param N Degree of freedom.
--   @return Density value.
data.tpdf = function (d,N)
  data.ext_special = data.ext_special or require('lib.special')
  local tmp = math.sqrt(N)*data.ext_special.beta(0.5,0.5*N)
  return (1+d*d/N)^(-0.5*(N+1))/tmp
end
about[data.tpdf] = {"tpdf(d,N)", "Student's distribution density.", DISTRIB}

--- Save Lua table in file, use given delimiter.
--  @param sFile File name.
--  @param t Lua table.
--  @param char Delimiter, default is coma.
--  @param bCol Flag, reading elements by columns.
data.writeCsv = function (sFile, t, char, bCol)
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
about[data.writeCsv] = {"writeCsv(sFile,t,[char=',',bCol=false])", "Save Lua table as delimiter separated data into file.", FILES}

--- Import data from text file, use given delimiter.
--  @param sFile File name.
--  @param char Delimiter, default is coma.
--  @param bCol Flag, reading elements by columns.
--  @return Lua table with data.
data.readCsv = function (sFile, char, bCol)
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
about[data.readCsv] = {"readCsv(sFile,[delim=',',bCol=false])", "Read delimiter separated data as Lua table.", FILES}

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
  return setmetatable({_beg=iBeg-1, _end=iEnd, _t=t}, metaref)
end
about[data.ref] = {'ref(t,[iBeg=1,iEnd=#t])', 'Return reference to the range of elements.', SonataHelp.NEW}

-- Comment to remove descriptions
data.about = about

return data

--====================================
--TODO: add tinv
