--[[		sonata/lib/data.lua

--- Data processing and statistics.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2025.

	module 'data'
--]]


-------------------- Tests -------------------
--[[TEST_IT

-- use 'data'
D = require 'matlib.data'
-- external dependencies, can be loaded implicitly
require 'matlib.special'
require 'matlib.matrix'

-- initial data (tables)
X = {3,2,5,6,3,4,3,1}
-- weight
W = {1,1,0}
-- enought to define w[i] ~= 1
W[5] = 2; W[6] = 2
-- average
ans = D:mean(X)              --3>  3.375

-- standard deviation
ans = D:std(X,W)             --3>  1.314

-- covariance matrix
Y = {0,2,1,3,7,5,8,4}
tmp = D:cov({X,Y})
ans = tmp[1][2]              --2>  -0.656

-- correlation
ans = D:corr(X, Y)           --2> -0.166

-- maximum element and index
_,ans = D:max(X)              -->  4

-- median
ans = D:median(X)             -->  3

-- table of frequencies
tmp = D:freq(X)
ans = tmp[3]                  -->  3

-- central moment
ans = D:moment(X,2)          --3>  2.234

-- summ of elements
ans = D:sum(X)                -->  27

-- minimum value
ans = D:min(X)                -->  1

-- geometrical mean
ans = D:geomean(X)           --3>  2.995

-- harmonic mean
ans = D:harmmean(X,W)        --3>  2.571

-- check if X[i] > 2
a = D:is(X, "x1 > 2")
ans = a[1]                    -->  1

-- get elements X[i] > 2
tmp = D:filter(X, a)
ans = tmp[1]                  -->  X[1]

-- filtration using explicit function
fn = function (x) return x > 2 end
tmp = D:filter(X, fn)
ans = tmp[1]                  -->  X[1]

-- reverse elements
q = {1, 2, 3, 4, 5}
D:reverse(q)
ans = q[1]                   --> 5

-- sort
D:sort(q, "x1 < x2")
ans = q[1]                   --> 1

-- binary search
ans, _ = D:binsearch(q, 2)   --> 2

-- generate new list
-- use 'lazy' function definition
tmp = D:zip("{x1-x2, x1+x2}", X, Y)
ans = tmp[1][2]               -->  X[1]+Y[1]

-- generator with condition
-- squares or even elements
tmp = D:gen(X, "x1^2", "x2 % 2 == 0")
ans = tmp[2]                  --> X[4]*X[4]

-- make array 2x3
zs = D:zeros(2, 3)
ans = #zs[2]                  --> 3

-- find histogram
a,b = D:histcounts(X, 3)
ans = b[1]                    -->  2.25

-- define edges
a,b = D:histcounts(X,{2,4,7})
ans = a[1]                    -->  1

-- table range reference
a = D:ref(X, 3, 6)
ans = #a                      -->  4

ans = a[1]                    -->  X[3]

-- sequential processing
a = D(X)
ans = a:filter("x1 > 3"):sum()  --> 15

-- dsv write
nm = os.tmpname()
-- separate elements with ';'
t = {{1,2,3},{4,5,6}}
D:csvwrite(t, nm, ';')

-- dsv read
-- with separator ';'
tt = D:csvread(nm, ';')
ans = tt[2][2]                -->  t[2][2]

-- pack
bin = D:pack(t)
ans = #bin                    --> 30

-- unpack
t2 = D:unpack(bin)
ans = t2[1][2]                --> t[1][2]

-- Markdown-like print for a table
print(D:md(t))

-- add column names and some processing
fn = function (v)
  return {v[1]^2, 0.5*(v[2]+v[3])}
end
c = D:md(t, {'sq', 'avg'}, fn)
print(c)

-- even numbers
b = D:range(2,10,2)
ans = b[2]                    -->  4

-- linear transformations
-- with range Range objects
b2 = 2*b + 4
ans = b2[1]                   -->  8

-- apply function
c = b:map(math.sin)
ans = c[1]                   --3>  0.909

--]]


--	LOCAL

local Ver = require("matlib.utils")
local Cross = Ver.cross
local Utils = Ver.utils
Ver = Ver.versions


local STAT = 'statistics'
local FILES = 'in/out'
local LIST = 'lists'
local REF = 'reference'
local AUX = 'auxiliary'


--- Add string bytes.
--  @param s Source string.
--  @param n (=nil) Last character position.
--  @return byte sum % 8.
local function _byteSum (s, n)
  local v = 0
  for i = 1, (n or #s) do v = v + string.byte(s, i, i) end
  return v % 8
end


--- Make copy of an object or list.
--  @param v Source object.
--  @return deep copy.
local function _copyObj(v)
  if type(v) == 'table' then
    if v.copy then
      return v:copy()
    else
      local lst = {}
      for i = 1, #v do lst[i] = _copyObj(v[i]) end
      return lst
    end
  else
    return v
  end
end


--- Recursive making or array with given value.
--  @param val Value to set.
--  @param n Current dimentions.
--  @param ... Rest of dimentions.
--  @return table or value.
local function _fillRest (val, n, ...)
  if n then
    if n <= 0 then error('expected positive size') end
    local res = {}
    for i = 1, n do res[i] = _fillRest(val, ...) end
    return res
  end
  return val
end


--- Convert list to binary string.
--  @param src Source table.
--  @param acc Data type accumulator.
--  @return binary string.
local function _listPack (src, acc)
  local t = {string.pack('B', acc['#'])}
  for _, v in ipairs(src) do
    if type(v) == 'table' then
      t[#t+1] = v._pack and v:_pack(acc) or _listPack(v, acc)
    elseif type(v) == 'number' then
      t[#t+1] = Utils.pack_num(v, acc)
    else
      error "Unable to pack"
    end
  end
  t[#t+1] = '\0'
  return table.concat(t)
end


--- Convert binary string to list.
--  @param src Source string.
--  @param pos Start position.
--  @param acc Data type accumulator.
--  @param ver Version of the pack algorithm.
--  @return obtained object and the next position.
local function _listUnpack (src, pos, acc, ver)
  local t, n = {}, nil
  while string.byte(src, pos) ~= 0 do
    n, pos = string.unpack('B', src, pos)
    local key = acc[n]
    if type(key) == "string" then
      if key == '#' then
        t[#t+1], pos = _listUnpack(src, pos, acc, ver)
      elseif string.byte(key, 1) == 0x26 then  -- &
        t[#t+1], pos = Utils.unpack_num(src, pos, key, ver)
      else   -- Sonata object
        acc[n] = require('matlib.'..key)
        t[#t+1], pos = acc[n]._unpack(src, pos, acc, ver)
      end
    else  -- library table
      t[#t+1], pos = key._unpack(src, pos, acc, ver)
    end
  end
  return t, pos+1  -- skip last \0
end


--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Data processing and statistics."
}


--	MODULE

local data = {}


-- List wrapper.
local mt_list = {
  type="list",
  -- methods
  __newindex = function (self, k, v) self._tbl[k] = v end,
  __len = function (self) return #self._tbl end,
  __tostring = function (self)
    return string.format("<list %s>", tostring(self._tbl))
  end,
}


--- Make function that returns result of source method call.
--  @param f Source method.
--  @return function call result.
local function _wrapCall (f)
  return function (self, ...) return f(nil, self, ...) end
end


--- Make function that calls source method and saves result.
--  @param f Source method.
--  @return list wrapper.
local function _wrapList (f)
  return function (self, ...)
    return setmetatable({_tbl=f(nil, self, ...)}, mt_list)
  end
end


--- Access to the wrapped list.
--  @param k Index name.
--  @return data value.
mt_list.__index = function (self, k)
  if k == 'data' then return self._tbl end
  return mt_list[k] or self._tbl[k]
end


--- Estimate covariance for two vectors.
--  @param t1 First data vector.
--  @param t2 Second data vector.
--  @return Covariance value.
data._cov2 = function (_, t1, t2)
  if #t1 ~= #t2 then
    error "Different vector size"
  end
  local m1 = data:mean(t1)
  local m2 = data:mean(t2)
  local s = 0
  for i = 1, #t1 do
    s = s + (t1[i] - m1)*(t2[i] - m2)
  end
  return s / #t1
end
data.cov2 = data._cov2  -- DEPRECATED


--- Binary search in sorted list.
--  @param t Sorted list of elements.
--  @param val Value to search.
--  @param fn (=nil) Function to extract data.
--  @return index and value.
data.binsearch = function (_, t, val, fn)
  local i, u = Utils.binsearch(t, val, fn)
  if u == val then
    return i, u  -- only when found
  end
end
mt_list.binsearch = _wrapCall(data.binsearch)
about[data.binsearch] = {":binsearch(sorted_t, value, [extract_fn]) --> index_i, value",
  "Find position of element in sorted list using binary search.", LIST}


--- Make copy of a list wrapper.
--  @return copy object.
mt_list.copy = function (self)
  local t = {}
  for i = 1, #self._tbl do t[i] = self._tbl[i] end
  return setmetatable({_tbl=t}, mt_list)
end


--- Estimate correlation for two lists.
--  @param t1 First data list.
--  @param t2 Second data list.
--  @return correlation value.
data.corr = function(_, t1, t2)
  if #t1 ~= #t2 then
    error "Different vector size"
  end
  local m1 = data:mean(t1)
  local m2 = data:mean(t2)
  local s12, s11, s22 = 0, 0, 0
  for i = 1, #t1 do
    local d1, d2 = t1[i]-m1, t2[i]-m2
    s12 = s12 + d1*d2
    s11 = s11 + d1*d1
    s22 = s22 + d2*d2
  end
  return s12 / math.sqrt(s11*s22)
end
about[data.corr] = {":corr(xs_t, ys_t) --> float",
  "Find correlation for two vectors.", STAT}


--- Make copy of an object or list.
--  @param v Source object.
--  @return deep copy.
data.copy = function (_, v) return _copyObj(v) end
about[data.copy] = {":copy(t) --> copy_t",
  "Make deep copy of the table.", help.OTHER}



--- Estimate covariance matrix.
--  @param t Table of data vectors.
--  @return Covariance matrix.
data.cov = function (_, t)
  local N = #t
  if N == 0 then
    error "Expected list of vectors"
  end
  data.ext_matrix = data.ext_matrix or require('matlib.matrix')
  local m = data.ext_matrix:zeros(N, N)
  for i = 1, N do
    local ti = t[i]
    m[i][i] = data:_cov2(ti, ti)
    for j = i+1, N do
      m[i][j] = data:_cov2(ti, t[j])
      m[j][i] = m[i][j]
    end
  end
  return m
end
mt_list.cov = _wrapCall(data.cov)
about[data.cov] = {":cov(data_t) --> cov_M",
  "Find covariance matrix for list of vectors.", STAT}


--- Save Lua table in file, use given delimiter.
--  @param t Lua table.
--  @param sFile File name.
--  @param char Delimiter, default is coma.
data.csvwrite = function (_, t, sFile, char)
  local f = assert(io.open(sFile, 'w'))
  char = char or ','
  for _, v in ipairs(t) do
    if type(v) == 'table' then v = table.concat(v, char) end
    f:write(v, '\n')
  end
  f:close()
end
mt_list.csvwrite = _wrapCall(data.csvwrite)
about[data.csvwrite] = {":csvwrite(data_t, file_s, delim_s=',')",
  "Save Lua table as delimiter separated data into file.", FILES}


--- Import data from text file, use given delimiter.
--  @param sFile File name.
--  @param char Delimiter, default is coma.
--  @return Lua table with data.
data.csvread = function (_, sFile, char)
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


--- Find elements using condition.
--  @param t Table with data.
--  @param vCond Either boolean function or table of weights.
--  @return Table with the filtered elements.
data.filter = function (_, t, vCond)
  local res = {}
  if type(vCond) == 'string' then vCond = Utils.Fn(vCond, 1) end
  if type(vCond) == 'function' then
    -- boolean function
    for i = 1, #t do
      local v = t[i]
      if vCond(v) then res[#res+1] = v end
    end
  elseif type(vCond) == 'table' then
    -- weights
    for i = 1, #t do
      if vCond[i] ~= 0 then res[#res+1] = t[i] end
    end
  end
  return res
end
mt_list.filter = _wrapList(data.filter)
about[data.filter] = {":filter(in_t, fn|str|tbl) --> out_t",
  "Get result of the table filtering. Condition is boolean function, string or table of weights.",
  LIST}


--- Frequency of elements.
--  @param t Table of numbers.
--  @return Table where keys are elements and values are their frequencies.
data.freq = function (_, t)
  local tmp = {}
  for _, v in ipairs(t) do
    tmp[v] = (tmp[v] or 0) + 1
  end
  return tmp
end
mt_list.freq = _wrapCall(data.freq)
about[data.freq] = {":freq(data_t) --> tbl",
  "Return table with frequencies of elements.", STAT}


--- Apply given function to elements of the list when condition is true.
--  @param t Table of numbers.
--  @param fn Transformation function or string.
--  @param cond Condition function f(v,i) or string.
--  @return obtained list.
data.gen = function (_, t, fn, cond)
  if type(fn) == 'string' then fn = Utils.Fn(fn, 1) end
  -- condition function f(index, value)
  if cond and type(cond) == 'string' then cond = Utils.Fn(cond, 2) end
  local q = {}
  if cond then
    for i, v in ipairs(t) do
      if cond(v, i) then q[#q+1] = fn(v) end
    end
  else
    for i, v in ipairs(t) do q[i] = fn(v) end
  end
  return q
end
mt_list.gen = _wrapList(data.gen)
about[data.gen] = {":gen(in_t, fn|str, [cond_fn|cond_str=nil]) --> out_t",
  "Make new list using given transformation. Optional condition function of the form f(value,index).",
  LIST}


--- Geometrical mean.
--  @param t Table of numbers.
--  @param tw Table of weights, optional.
--  @return Geometrical mean.
data.geomean = function (_, t, tw)
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
mt_list.geomean = _wrapCall(data.geomean)
about[data.geomean] = {":geomean(data_t, weigh_t=nil) --> num",
  "Geometrical mean.", STAT}


--- Harmonic mean.
--  @param t Table of numbers.
--  @param tw Table of weights. Can be omitted.
--  @return Harmonic mean.
data.harmmean = function (_, t, tw)
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
mt_list.harmmean = _wrapCall(harmmean)
about[data.harmmean] = {":harmmean(data_t, weigh_t=nil) --> num",
  "Harmonic mean.", STAT}


--- Number of elements in each bin.
--  @param t Data table.
--  @param rng Number of bins or table with edges.
--  @return Two tables, with sum and edges.
data.histcounts = function (_, t, rng)
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
mt_list.histcounts = _wrapCall(data.histcounts)
about[data.histcounts] = {":histcounts(data_t, edges_t|N=10) --> sum_t, edges_t",
  "Calculate amount of bins. Edges can be either number or table.", STAT}


--- Find weights (1/0) based on condition.
--  @param t Data table.
--  @param fn Condition, boolean function or string.
--  @return Table of 1 and 0.
data.is = function (_, t, fn)
  if type(fn) == 'string' then fn = Utils.Fn(fn, 1) end
  local res = {}
  for i = 1, #t do
    res[i] = fn(t[i]) and 1 or 0
  end
  return res
end
mt_list.is = _wrapList(data.is)
about[data.is] = {":is(data_t, fn|str) --> weigh_t",
  "Find weights using condition (boolean function or string).", LIST}


--- Find weights (1/0) based on inverted condition.
--  @param t Data table.
--  @param fn Condition, boolean function.
--  @return Table of 1 and 0.
data.isNot = function (_, t, fn)
  if type(fn) == 'string' then fn = Utils.Fn(fn, 1) end
  local res = {}
  for i = 1, #t do
    res[i] = fn(t[i]) and 0 or 1
  end
  return res
end
mt_list.isNot = _wrapList(data.isNot)
about[data.isNot] = {":isNot(data_t, fn|str) --> weigh_t",
  "Find inverted weights using condition (boolean function or string).", LIST}


--- Maximum value.
--  @param t Table of numbers.
--  @return Maximum value and its index.
data.max = function (_, t)
  local m, k = t[1], 1
  for i = 2, #t do
    if t[i] > m then m, k = t[i], i end
  end
  return m, k
end
mt_list.max = _wrapCall(data.max)
about[data.max] = {":max(data_t) --> var, ind_N",
  "Maximal element and its index.", STAT}


--- Show data in Markdown-like table form.
--  @param data_t Table of form {row1, row2, etc.}.
--  @param names_t Table of column names (optional).
--  @param fn Table that generates new column from the given (optional).
--  @return String with table representation.
data.md = function (_, data_t, names_t, fn)
  local acc, line = {}, {}
  -- data to stings
  for i, v in ipairs(data_t) do
    local row = {}
    for j, w in ipairs(fn and fn(v) or v) do
      row[j] = tostring(w)
    end
    acc[i] = row
  end
  -- names
  if names_t then
    local head = {}
    for j = 1, #acc[1] do head[j] = tostring(names_t[j] or '') end
    acc[#acc+1] = head  -- temporary add
  end
  -- save
  local len = Utils.align(acc)
  for j = 1, #len do line[j] = string.rep('-', len[j]) end
  local res, templ = {}, '| %s |'
  if names_t then
    res[1] = string.format(templ, table.concat(acc[#acc], ' | '))
    acc[#acc] = nil
  end
  res[#res+1] = string.format('|-%s-|', table.concat(line, '-|-'))
  for _, v in ipairs(acc) do
    res[#res+1] = string.format(templ, table.concat(v, ' | '))
  end
  return table.concat(res, '\n')
end
mt_list.md = _wrapCall(data.md)
about[data.md] = {":md(data_t, names_t=nil, row_fn=nil) --> str",
  "Markdown-like table representation. Rows can be processed using function row_fn(t)-->t.",
  help.OTHER}


--- Average value.
--  @param t Table with numbers.
--  @param tw Table with weight. Can be omitted.
--  @return Average.
data.mean = function (_, t, tw)
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
mt_list.mean = _wrapCall(data.mean)
about[data.mean] = {":mean(data_t, wight_t=nil) --> num",
  "Calculate average value. Weights can be used.", STAT}


--- Find median.
--  @param t Table of numbers.
--  @return Value of median.
data.median = function (_, t)
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
mt_list.median = _wrapCall(data.median)
about[data.median] = {":median(data_t) --> num",
  "Median of the list.", STAT}


--- Minimum value.
--  @param t Table of numbers.
--  @return Minimum value and its index.
data.min = function (_, t)
  local m, k = t[1], 1
  for i = 2, #t do
    if t[i] < m then m, k = t[i], i end
  end
  return m, k
end
mt_list.min = _wrapCall(data.min)
about[data.min] = {":min(data_t) --> var, ind_N",
  "Minimal element and its index.", STAT}


--- Central moment.
--  @param t Table of numbers.
--  @param N Order of the moment.
--  @param tw Table of weights. Can be omitted.
--  @return Central moment value.
data.moment = function (_, t, N, tw)
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
mt_list.moment = _wrapCall(data.moment)
about[data.moment] = {":moment(data_t, order_N, weigth_t=nil) --> num",
  "Central moment of order N, weights can be defined.", STAT}


--- Apply reduction rule to the list elements.
--  @param t List.
--  @param fn Function of 2 elements.
--  @param val Initial value (optional).
--  @return Result of reduction.
data.reduce = function (_, t, fn, val)
  local i0 = 1
  if not val then
    val, i0 = t[1], 2
  end
  if type(fn) == 'string' then fn = Utils.Fn(fn, 2) end
  for i = i0, #t do val = fn(val, t[i]) end
  return val
end
mt_list.reduce = _wrapCall(data.reduce)
about[data.reduce] = {":reduce(data, fn|str, initial=datadata_t[1]_t[1]) --> var",
  "Apply function to its previous result and next element.", LIST}


--- Reverse list in place.
--  @param t List of elements.
data.reverse = function (_, t)
  local n, m = math.floor(#t / 2), #t+1
  for i = 1, n do
    t[i], t[m-i] = t[m-i], t[i]
  end
  return dst
end
mt_list.reverse = function (self) data.reverse(nil, self._tbl); return self end
about[data.reverse] = {":reverse(data_t)",
  "Reverse table elements.", LIST}


--- Sort elements in place.
--  @param t List of elements.
--  @param fn Comparison function or string.
data.sort = function (_, t, fn)
  if type(fn) == "string" then fn = Utils.Fn(fn, 2) end
  table.sort(t, fn)
end
mt_list.sort = function (self, fn) data.sort(nil, self._tbl, fn); return self end
about[data.sort] = {":sort(data_t, fn|str)",
  "Sort elements of the list", LIST}


--- Sum of all elements.
--  @param t Table with numbers.
--  @return Sum.
data.sum = function (_, t)
  local s = 0
  for i = 1, #t do s = s+t[i] end
  return s
end
mt_list.sum = _wrapCall(data.sum)
about[data.sum] = {":sum(data_t) --> var",
  "Get sum of all elements.", STAT}


--- Standard deviation and variance.
--  @param t Table of numbers.
--  @param tw Table of weights.
--  @return Standard deviation, variance.
data.std = function (_, t, tw)
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
  return math.sqrt(disp)
end
mt_list.std = _wrapCall(data.std)
about[data.std] = {":std(data_t, weight_t=nil) --> num",
  "Standard deviation. Weights can be used.", STAT}


--- Make table of given size filled with zeros.
--  @param ... Size list.
--  @return table with zeros.
data.zeros = function (_, ...) return _fillRest(0, ...) end
about[data.zeros] = {":zeros(n1, [n2,..]) --> tbl", 
  "Make table with zeros.", help.OTHER}


--- Apply function of n arguments to n lists.
--  @param fn Function of multiple arguments or string.
--  @param ... Sequence of lists.
--  @return List of values fn(...).
data.zip = function (_, fn, ...)
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
about[data.zip] = {":zip(fn|str, ...) --> tbl",
  "Sequentially apply function to list of tables.", LIST}


-- Constructor for the list wrapper.
setmetatable(data, {
__call = function (self, t)
  return setmetatable({_tbl=t}, mt_list)
end })
about[data] = {" (data_t) --> new_L",
  "Create list wrapper.", REF}


-- Methametods for the range of numbers.
local mt_range = { 
  type = 'range',
  -- methods 
  __len = function (self) return self._N end,
  __newindex = function (self, k, v) end,  -- can't modify
}


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


-- Range methods
about["_rng"] = {"range: -R, R+x, R-x, k*R, R|fn", nil, AUX}


--- Initialize range object.
--  @param dBeg First value.
--  @param dEnd Last value.
--  @param dStep Step value.
--  @param iN Number of elements.
--  @return Range object.
mt_range._init = function (dBeg, dEnd, dStep, iN, fn)
  return setmetatable({_beg=dBeg, _end=dEnd, _step=dStep, _N=iN, _fn=fn}, mt_range)
end


--- Make reversed range object.
--  @return new object.
mt_range.reverse = function (self)
  return mt_range._init(self._end, self._beg, -self._step, self._N, self._fn)
end


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
mt_range.__bor = mt_range.map  -- allow  rng | fn1 | fn2


--- Generate sequence of values.
--  @param dBegin Beginning of range.
--  @param dEnd End of range.
--  @param dStep Step value (default is 1 or -1).
--  @return Table with numbers, Range object.
data.range = function (_, dBegin, dEnd, dStep)
  dStep = dStep or (dEnd > dBegin) and 1 or -1
  local diff = dEnd - dBegin
  assert(diff * dStep > 0, "Wrong range or step")
  -- check size
  local n, _ = math.modf(diff / dStep)
  if math.abs(n*dStep - dEnd) >= math.abs(dStep * 0.1) then n = n + 1 end
  -- result
  return mt_range._init(dBegin, dEnd, dStep, n)
end
about[data.range] = {':range(begin_d, end_d, step_d=±1) --> new_R',
  'Generate range object.', AUX}


-- Get reference to data range in other table
local mt_ref = {
  type = 'ref' ,
  -- methods
  __len = function (t) return t._end - t._beg end,
  __tostring = function (t) return string.format("<ref %s>", tostring(t._tbl)) end,
}


--- Get i-th element.
--  @param self Ref object.
--  @param i Element index.
--  @return Table value.
mt_ref.__index = function (self, i)
  if Ver.isInteger(i) then
    local n = i + self._beg
    if self._beg < n and n <= self._end then
      return self._tbl[n]
    end
  end
  return mt_ref[i] or mt_list[i]
end


--- Set k-th value.
--  @param self Ref object.
--  @param k Index.
--  @param v Value.
mt_ref.__newindex = function (self, k, v)
  if Ver.isInteger(k) and 0 < k and (self._beg + k) <= self._end then
    k = k + self._beg
    if getmetatable(v) == mt_ref then
      -- copy data
      local i0 = k - 1
      local n = math.min(#v, self._end - i0)
      for i = 1, n do self._tbl[i0 + i] = v[i] end
    else
      -- set value
      self._tbl[k] = v
    end
  end
end


--- Create reference to other table.
--  @param t Source table.
--  @param iBeg Index of the first element.
--  @param iEnd Index of the last element.
--  @return Reference object.
data.ref = function (_, t, iBeg, iEnd)
  iBeg = iBeg or 1
  iEnd = iEnd or #t
  assert(Ver.isInteger(iBeg) and Ver.isInteger(iEnd), "Wrong index type")
  if getmetatable(t) == mt_ref then
    return setmetatable({_beg=t._beg+iBeg-1, _end=t._beg+iEnd, _tbl=t._tbl}, mt_ref)
  end
  return setmetatable({_beg=iBeg-1, _end=iEnd, _tbl=t}, mt_ref)
end
about[data.ref] = {':ref(data_t, begin_N=1, end_N=#src_t) --> new_R',
  'Return reference to the range of elements.', REF}


-- Column reference
local mt_col = {
  type='column',
  -- methods
  __len = function (self) return #self._tbl end,
}


--- Get column element.
--  @param k Key.
--  @return found value.
mt_col.__index = function (self, k)
  local tmp = mt_col[k] or mt_list[k]
  if tmp then  -- for sequential transformations
    return tmp
  end
  local row = self._tbl[k]
  return row and row[self._n]
end

--- Set element or group of elements.
--  @param k Key, 'data' for group set.
--  @param v Value, scalar or list.
mt_col.__newindex = function (self, k, v)
  if k == 'data' then
    if type(v) ~= 'table' or #v ~= #self._tbl then
      error "unable to set elements"
    end
    local t, n = self._tbl, self._n
    for i = 1, #t do t[i][n] = v[i] end
  else
    local row = self._tbl[k]
    if row then row[self._n] = v end
  end
end


--- Make column reference.
--  @param t Source table.
--  @param n Column number.
--  @return column reference.
data.col = function (_, t, n)
  assert(Ver.isInteger(n) and t[1][n], "Out of range")
  return setmetatable({_tbl=t, _n=n}, mt_col)
end
about[data.col] = {":col(src_t, col_N) --> ref_Col",
  "Make column reference.", REF}


-- Row reference
local mt_row = {
  type='row',
  -- methods
  __len = function (self) return #self._row end,
}


--- Get row element.
--  @param k Key.
--  @return found value.
mt_row.__index = function (self, k)
  return mt_row[k] or mt_list[k] or self._row[k]
end


--- Set element or group of elements.
--  @param k Key, 'data' for group set.
--  @param v Value, scalar or list.
mt_row.__newindex = function (self, k, v)
  if k == 'data' then
    if type(v) ~= 'table' or #v ~= #self._row then
      error "unable to set elements"
    end
    local t = self._row
    for i = 1, #t do t[i] = v[i] end
  else
    if self._row[k] then self._row[k] = v end
  end
end


--- Make row reference.
--  @param t Source table.
--  @param n Column number.
--  @return column reference.
data.row = function (_, t, n)
  local row = t[n]
  assert(Ver.isInteger(n) and row, "Out of range")
  return setmetatable({_tbl=t, _row=row, _n=n}, mt_row)
end
about[data.row] = {":row(src_t, row_N) --> ref_Row",
  "Make row reference.", REF}


-- Collect data types for packing.
local mt_accum = {
  -- Save new element and return its index.
  __index = function (t, k)
    table.insert(t._nm, k)
    local n = #t._nm
    rawset(t, k, n)
    return n
  end
}


--- Convert object to binary string.
--  @param v Source object.
--  @return binary string representation.
data.pack = function (self, v)
  local ver = Sonata and (100*Sonata.MAJOR_V + Sonata.MINOR_V) or 100
  local t = {'/\\/', string.pack('I2', ver), '\0',}
  local acc, bin = setmetatable({_nm={}}, mt_accum), nil
  if type(v) == 'table' then
    bin = v._pack and v:_pack(acc) or _listPack(v, acc)
  else
    error "No rules to pack it"
  end
  -- make 'vocabulary'
  for _, nm in ipairs(acc._nm) do
    t[#t+1] = string.pack('B', #nm)
    t[#t+1] = nm
  end
  t[#t+1] = '\0'  -- end marker
  t[#t+1] = bin
  -- check sum
  local sum = 0
  for i = 1, #t do sum = sum + _byteSum(t[i]) end
  t[#t+1] = string.pack('B', sum % 8)
  return table.concat(t)
end
about[data.pack] = {":pack(obj) --> bin_s",
  "Pack object to binary string.", FILES}


--- Convert binary string to Sonata object.
--  @param v Source string.
--  @return object.
data.unpack = function (self, v)
  if type(v) ~= 'string' or string.sub(v, 1, 3) ~= '/\\/' then
    error 'Unknown data type'
  end
  if _byteSum(v, #v-1) ~= string.byte(v, #v) then
    error 'Wrong check sum'
  end
  local ver, pos = string.unpack('I2', v, 4)
  pos = pos + 1  -- skip zero
  -- restore vocabulary
  local types, n = {}, 0
  while string.byte(v, pos) ~= 0 do
    n, pos = string.unpack('B', v, pos)
    types[#types+1] = string.sub(v, pos, pos+n-1)
    pos = pos + n
  end
  pos = pos + 1
  if types[1] == '#' then  -- lua table
    return _listUnpack(v, pos+1, types, ver)
  else
    types[1] = require('matlib.'..types[1])
    return types[1]._unpack(v, pos+1, types, ver)
  end
end
about[data.unpack] = {":unpack(bin_s) --> obj",
  "Unpack object from binary string.", FILES}


-- Comment to remove descriptions
data.about = about

return data

--====================================
