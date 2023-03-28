--[[		sonata/lib/data.lua

--- Data processing and statistics.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2023.

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
tmp = _D:zip("{x1-x2, x1+x2}", X, Y)
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
ans = _D:tcdf(4, 2.5)        --3> 0.9805

ans = _D:tpdf(2, 3.3)        --3> 0.0672

-- dsv write
nm = os.tmpname()
-- separate elements with ';'
t = {{1,2,3},{4,5,6}}
_D:csvwrite(nm, t, ';')

-- dsv read
-- with separator ';'
tt = _D:csvread(nm, ';')
ans = tt[2][2]                --> t[2][2]

-- reference to 'transposed' table
b = _D:T(t)
ans = b[3][2]                 --> t[2][3]

-- pretty print for a table
print(_D:table(t)) 

-- add column names and some processing 
fn = function (v)
  return {v[1]^2, 0.5*(v[2]+v[2])}
end
c = _D:table(t, {'sq', 'avg'}, fn)
print(c)

--]]

--	LOCAL

local Ver = require("lib.utils")
local Cross = Ver.cross
local Utils = Ver.utils
Ver = Ver.versions

local STAT = 'statistics'
local FILES = 'files'
local FILTER = 'filter'
local REF = 'reference'

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
data.cov2 = function (self, t1, t2)
  if #t1 ~= #t2 then error("Different vector size") end
  local m1 = data:mean(t1)
  local m2 = data:mean(t2)
  local s = 0
  for i = 1, #t1 do
    s = s + (t1[i] - m1)*(t2[i] - m2)
  end
  return s / #t1
end
about[data.cov2] = {":cov2(xs_t, ys_t) --> float", 
  "Find covariance value for two vectors.", STAT}

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
about[data.cov] = {":cov(data_t) --> cov_M", 
  "Find covariance matrix for list of vectors.", STAT}

--- Save Lua table in file, use given delimiter.
--  @param self Do nothing.
--  @param sFile File name.
--  @param t Lua table.
--  @param char Delimiter, default is coma.
data.csvwrite = function (self, sFile, t, char)
  local f = assert(io.open(sFile, 'w'))
  char = char or ','
  for _, v in ipairs(t) do
    if type(v) == 'table' then v = table.concat(v, char) end
    f:write(v, '\n')
  end
  f:close()
  io.write('Done\n')
end
about[data.csvwrite] = {":csvwrite(file_s, data_t, char=',') --> nil",
  "Save Lua table as delimiter separated data into file.", FILES}

--- Import data from text file, use given delimiter.
--  @param self Do nothing.
--  @param sFile File name.
--  @param char Delimiter, default is coma.
--  @return Lua table with data.
data.csvread = function (self, sFile, char)
  local f = assert(io.open(sFile, 'r'))
  char = char or ','
  local templ = '([^'..char..']+)'
  local res = {}
  for s in f:lines('l') do
    -- read data
    if char ~= '#' then
      s = string.match(s, '([^#]+)')    -- skip comments
    end
    s = string.match(s, '^%s*(.*)%s*$')  -- strip line
    if #s > 0 then
      local tmp = {}
      -- parse string
      for p in string.gmatch(s, templ) do
        tmp[#tmp+1] = tonumber(p) or p
      end
      -- save
      res[#res+1] = tmp
    end
  end
  f:close()
  return res
end
about[data.csvread] = {":csvread(file_s, delim_s=',') --> tbl",
  "Read delimiter separated data as Lua table.", FILES}

--- Generate function from string.
--  @param self Do nothing.
--  @param sExpr Expression for execution.
--  @param iArg Number of arguments (optional).
--  @return Function based on the expression.
data.Fn = function (self, sExpr, iArg) return Utils.Fn(sExpr, iArg or 2) end
about[data.Fn] = {":Fn(expr_s, arg_N=2) --> fn",
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
about[data.filter] = {":filter(in_t, condition) --> out_t",
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
about[data.freq] = {":freq(data_t) --> tbl", 
  "Return table with frequencies of elements.", STAT}

--- Geometrical mean.
--  @param self Do nothing.
--  @param t Table of numbers.
--  @param tw Table of weights, optional.
--  @return Geometrical mean.
data.geomean = function (self, t, tw)
  if tw then
    local st, sw = 0, 0
    for i = 1, #t do
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
about[data.geomean] = {":geomean(data_t, [weigh_t]) --> num", 
  "Geometrical mean.", STAT}

--- Harmonic mean.
--  @param self Do nothing.
--  @param t Table of numbers.
--  @param tw Table of weights. Can be omitted.
--  @return Harmonic mean.
data.harmmean = function (self, t, tw)
  if tw then
    local st, sw = 0, 0
    for i = 1, #t do
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
about[data.harmmean] = {":harmmean(data_t, [weigh_t]) --> num", 
  "Harmonic mean.", STAT}

--- Number of elements in each bin.
--  @param self Do nothing.
--  @param t Data table.
--  @param rng Number of bins or table with edges.
--  @return Two tables, with sum and edges.
data.histcounts = function (self, t, rng)
  rng = rng or 10
  local bins = nil
  -- make copy and sort
  local y = Ver.move(t, 1, #t, 1, {})
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
  for i = 2, #bins do
    if bins[i] <= bins[i-1] then error("Wrong order") end
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
about[data.histcounts] = {":histcounts(data_t, rng_v=10) --> sum_t, edges_t",
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
about[data.is] = {":is(data_t, cond_fn) --> yesno_t",
  "Find weights using boolean function.", FILTER}

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
about[data.isNot] = {":isNot(data_t, cond_fn) --> yesno_t", 
  "Find inverted weights using boolean function.", FILTER}

--- Maximum value.
--  @param self Do nothing.
--  @param t Table of numbers.
--  @return Maximum value and its index.
data.max = function (self, t)
  local m, k = t[1], 1
  for i = 2, #t do
    if t[i] > m then m, k = t[i], i end
  end
  return m, k
end
about[data.max] = {":max(data_t) --> var, ind_N", 
  "Maximal element and its index.", STAT}

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
about[data.mean] = {":mean(data_t, [wight_t]) --> num", 
  "Calculate average value. Weights can be used.", STAT}

--- Find median.
--  @param self Do nothing.
--  @param t Table of numbers.
--  @return Value of median.
data.median = function (self, t)
  local len = #t
  local y = Ver.move(t, 1, len, 1, {})
  table.sort(y)
  if len % 2 == 1 then
    return y[(len+1)/2]
  else
    len = len / 2
    return (y[len] + y[len+1]) * 0.5
  end
end
about[data.median] = {":median(data_t) --> num", 
  "Median of the list.", STAT}

--- Minimum value.
--  @param self Do nothing.
--  @param t Table of numbers.
--  @return Minimum value and its index.
data.min = function (self, t)
  local m, k = t[1], 1
  for i = 2, #t do
    if t[i] < m then m, k = t[i], i end
  end
  return m, k
end
about[data.min] = {":min(data_t) --> var, ind_N", 
  "Minimal element and its index.", STAT}

--- Central moment.
--  @param self Do nothing.
--  @param N Order of the moment.
--  @param t Table of numbers.
--  @param tw Table of weights. Can be omitted.
--  @return Central moment value.
data.moment = function (self, N, t, tw)
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
about[data.moment] = {":moment(order_N, data_t, [weigth_t]) --> num",
  "Central moment of t order N, tw is a list of weights.", STAT}


--- Sum of all elements.
--  @param self Do nothing.
--  @param t Table with numbers.
--  @return Sum.
data.sum = function (self, t)
  local s = 0
  for i = 1, #t do s = s+t[i] end
  return s
end
about[data.sum] = {":sum(data_t) --> var", 
  "Get sum of all elements.", help.OTHER}

--- Standard deviation and variance.
--  @param self Do nothing.
--  @param t Table of numbers.
--  @param tw Table of weights.
--  @return Standard deviation, variance.
data.std = function (self, t, tw)
  local mean = data:mean(t, tw)
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
  return math.sqrt(disp), disp
end
about[data.std] = {":std(data_t, [weight_t]) --> dev_f, var_f",
  "Standard deviation and variance. Weights can be used.", STAT}

--- Show data in Markdown-like table form.
--  @param self Do nothing.
--  @param data_t Table of form {row1, row2, etc.}.
--  @param names_t Table of column names (optional).
--  @param fn Table that generates new column from the given (optional).
--  @return String with table representation.
data.table = function (self, data_t, names_t, fn)
  local acc, line, head, len = {}, {}, {}, {}
  -- data to stings
  for i, v in ipairs(data_t) do
    local row = {}
    for j, w in ipairs(fn and fn(v) or v) do
      row[j] = tostring(w)
      len[j] = math.max(len[j] or 0, #row[j])
    end
    acc[i] = row
  end
  -- names
  if names_t then
    assert(#names_t == #acc[1], "Wrong column number")
    for j, w in ipairs(names_t) do
      head[j] = tostring(w)
      len[j] = math.max(len[j], #head[j])
    end
  end
  -- collect
  for j = 1, #acc[1] do
    local templ = string.format('%%-%ds', len[j])
    for i = 1, #acc do acc[i][j] = string.format(templ, acc[i][j]) end
    line[j] = string.rep('-', len[j])
    if names_t then head[j] = string.format(templ, head[j]) end
  end
  local res, templ = {}, '| %s |'
  if names_t then res[1] = string.format(templ, table.concat(head, ' | ')) end
  res[#res+1] = string.format('|-%s-|', table.concat(line, '-|-'))
  for _, v in ipairs(acc) do
    res[#res+1] = string.format(templ, table.concat(v, ' | ')) 
  end
  return table.concat(res, '\n')
end
about[data.table] = {":table(data_t, names_t=nil, row_fn=nil) --> str", 
  "Markdown-like table representation. Rows can be processed using function row_fn(t)-->t."}

--- Student's cumulative distribution
--  @param self Do nothing.
--  @param d Value.
--  @param N Degree of freedom.
--  @return Cumulative value.
data.tcdf = function (self, d, N)
  data.ext_special = data.ext_special or require('lib.special')
  local tmp = N/(N+d*d)
  return 1-0.5*data.ext_special:betainc(tmp, 0.5*N, 0.5)
end
about[data.tcdf] = {":tcdf(x_d, deg_N) --> num",
  "Student's cumulative distribution.", help.OTHER}

--- Student's density function.
--  @param self Do nothing.
--  @param d Value.
--  @param N Degree of freedom.
--  @return Density value.
data.tpdf = function (self, d, N)
  data.ext_special = data.ext_special or require('lib.special')
  local tmp = math.sqrt(N)*data.ext_special:beta(0.5, 0.5*N)
  return (1+d*d/N)^(-0.5*(N+1))/tmp
end
about[data.tpdf] = {":tpdf(x_d, deg_N) --> num", 
  "Student's distribution density.", help.OTHER}

--- Condition: x == d.
--  @param self Do nothing.
--  @param d Some value.
--  @return Boolean function.
data.xEq = function (self, d)
  return (function (x) return Cross.eq(x, d) end)
end
about[data.xEq] = {":xEq(num) --> cond_fn",
  "Return function for condition x == d.", FILTER}

--- Condition: x > d
--  @param self Do nothing.
--  @param d Lower limit.
--  @return Boolean function.
data.xGt = function (self, d)
  return (function (x) return x > d end)
end
about[data.xGt] = {":xGt(num) --> cond_fn", "Return function for condition x > d.", FILTER}

--- Condition: d1 <= x <= d2.
--  @param self Do nothing.
--  @param d1 Lower limit.
--  @param d2 Upper limit.
--  @return Boolean function.
data.xIn = function (self, d1, d2)
  return (function (x) return d1 <= x and x <= d2 end)
end
about[data.xIn] = {":xIn(num1, num2) --> cond_fn", 
  "Return function for condition d1 <= x <= d2.", FILTER}

--- Condition: x < d
--  @param self Do nothing.
--  @param d Upper limit.
--  @return Boolean function.
data.xLt = function (self, d)
  return (function (x) return x < d end)
end
about[data.xLt] = {":xLt(num) --> cond_fn", "Return function for condition x < d.", FILTER}

--- Apply function of n arguments to n lists.
--  @param self Do nothing.
--  @param fn Function of multiple arguments or string.
--  @param ... Sequence of lists.
--  @return List of values fn(...).
data.zip = function (self, fn, ...)
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
about[data.zip] = {":zip(fn,...) --> tbl",
  "Sequentially apply function to list of tables.", help.OTHER}

-- Get reference to data range in other table
local mt_ref = { type = 'ref' }

--- Number of elements in range.
--  @param self Ref object.
--  @return Length of the reference table.
mt_ref.__len = function (self)
  return self._end - self._beg
end

--- Get i-th element.
--  @param self Ref object.
--  @param i Element index.
--  @return Table value.
mt_ref.__index = function (self, i)
  if Ver.isInteger(i) then
    local n = i + self._beg
    if self._beg < n and n <= self._end then
      return self._t[n]
    end
  end
  return mt_ref[i]
end

--- Set k-th value.
--  @param self Ref object.
--  @param k Index.
--  @param v Value.
mt_ref.__newindex = function (self, k, v)
  if Ver.isInteger(k) and self._beg < k and k <= self._end then
    self._t[self._beg + k] = v
  end
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
  return setmetatable({_beg=iBeg-1, _end=iEnd, _t=t}, mt_ref)
end
about[data.ref] = {':ref(src_t, begin_N=1, end_N=#src_t) --> new_R',
  'Return reference to the range of elements.', REF}

-- Get reference to 'transposed' data
-- i.e. t[i][j] returns t[j][i]
local mt_transpose = {type = 'transpose'}

--- Get access to k-th element.
--  @param self T-ref object. 
--  @param k Table index.
--  @return Empty table with mt_transpose_k metatable.
mt_transpose.__index = function (self, k)
  self._tbl._n = k
  return self._tbl
end

--- Get table 'length'.
--  @param self T-ref object.
--  @return Expected table length.
mt_transpose.__len = function (self)
  return #self._tbl._src[1] 
end

--- Transform ref object into pure table.
--  @param self T-ref object.
--  @return Lua table.
mt_transpose._table = function (self)
  local res, src = {}, self._tbl._src
  for i = 1, #src[1] do
    local row = {}
    for j = 1, #src do row[j] = src[j][i] end
    res[i] = row
  end
  return res 
end

-- Metatable for internal table.
local mt_transpose_k = {}

--- Get table element.
--  @param self Internal T-ref object.
--  @param k Element index.
mt_transpose_k.__index = function (self, k)
  local v = self._src[k]
  return v and v[self._n] or nil
end

--- Set table element.
--  @param self Internal T-ref object.
--  @param k Element index.
--  @param v New value.
mt_transpose_k.__newindex = function (self, k, v)
  self._src[k][self._n] = v
end

--- Get length of 'internal' table.
--  @param self Internal T-ref object.
--  @return Table length.
mt_transpose_k.__len = function (self) return #self._src end

--- Get reference to 'transposed' table.
--  @param self Do nothing.
--  @param src Source table.
--  @return Reference object.
data.T = function (self, src)
  if #src == 0 or type(src[1]) ~= 'table' then
    return src  -- don't need in reference
  end
  local o = {
    _tbl = setmetatable({_src = src, _n = 0}, mt_transpose_k),
  }
  return setmetatable(o, mt_transpose)
end
about[data.T] = {":T(src_t) --> TR", "Get reference to 'transposed' table.", REF}

-- Comment to remove descriptions
data.about = about

return data

--====================================
--TODO: add tinv
