--[[		sonata/lib/data.lua

--- Data rading and processing.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2026.

	module 'data'
--]]


-------------------- Tests -------------------
--[[TEST_IT

-- use 'data'
D = require 'matlib.data'

-- initial data
X = {3,2,5,6,3,4,3,1}
-- check if X[i] > 2
a = D:is(X, "x > 2")
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
ans = q[1]                    -->  5

-- merge sort
D:sort(q, "x,y -> x < y")
ans = q[1]                    -->  1

-- binary search
ans, _ = D:binsearch(q, 2)    -->  2

-- generate new list
Y = {0,2,1,3,7,5,8,4}
-- use 'lazy' function definition
tmp = D:zip("x1,x2 -> {x1-x2, x1+x2}", X, Y)
ans = tmp[1][2]               -->  X[1]+Y[1]

-- generator with condition
-- squares or even elements
tmp = D:gen(X,
  "x^2",                 -- rule
  "x, n -> n % 2 == 0")  -- condition
ans = tmp[2]                  -->  X[4]*X[4]

-- sum of squares
ans = D:reduce(q, "acc,x -> acc+x^2", 0)  --> 55

-- make array 2x3
zs = D:zeros(2, 3)
ans = #zs[2]                  -->  3

ans = zs[1][1]                -->  0

-- table range reference
a = D:ref(X, 3, 6)
ans = #a                      -->  4

ans = a[1]                    -->  X[3]

-- shift reference range
-- can be called with << and >>
a1 = a:shift(-1)
-- show new borders
print(a:range())

-- sequential processing
a = D(X)
ans = a:filter("x > 3"):reduce("x,y -> x+y")  -->  15

-- dsv write
nm = os.tmpname()
-- separate elements with ';'
t = {{1,2,3},{4,5,6}}
D:csvwrite(t, nm, ';')

-- dsv read
-- with separator ';'
tt = D:csvread(nm, ';')
ans = tt[2][2]                -->  t[2][2]

-- get column
vc = D:col(t, 2)
ans = vc[2]                   -->  t[2][2]

-- get row
vr = D:row(t, 1)
ans = vr[3]                   -->  t[1][3]

-- pack
bin = D:pack(t)
ans = #bin                    -->  31

-- unpack
t2 = D:unpack(bin)
ans = t2[1][2]                -->  t[1][2]

-- Markdown-like print for a table
print(D:md(t))

-- add column names and some processing
fn = function (v)
  return {v[1]^2, 0.5*(v[2]+v[3])}
end
c = D:md(t, {'sq', 'avg'}, fn)
print(c)

-- iterate over combinations
q = {1,2,3}
n = 0
for _ in D:icomb(q, 2) do n = n+1 end
ans = n                       -->  3

-- iterate over permutations
n = 0
for _ in D:iperm(q) do n = n+1 end
ans = n                       -->  6

-- nested iterators
-- get all permutations of N elements
seq = {
  -- wrap to remove 'self' from arguments
  function (...) return D:icomb(...) end,
  function (...) return D:iperm(...) end,
}
n = 0
for _ in D:inest(seq, q, 2) do n = n+1 end
ans = n                       -->  6

-- iterate over all combination length
inum = function (A)
  local i = -1
  return function ()
    i = i+1  -- from 0 to #A
    if i > #A then return end
    return A, i
  end
end
seq = {inum, function (...) return D:icomb(...) end}
for v in D:inest(seq, q) do
  print('{'..table.concat(v, ', ')..'}')
end

-- even numbers
b = D:range(2, 10, 2)
ans = b[2]                    -->  4

-- reverse range
b1 = b:reverse()
ans = b1[1]                   -->  10

-- linear transformations
-- with range Range objects
b2 = 2*b + 4
ans = b2[1]                   -->  8

-- apply function
c = b:map(math.sin)
ans = c[1]                  --.3>  0.909

--]]


--	LOCAL

local _ext = {
  utils = require("matlib.utils"),
}

local _utils = _ext.utils.utils
local _ver = _ext.utils.versions

local _tag = {
  FILES="in/out", LIST="lists",
  REF="reference", AUX="auxiliary",
}


--- Add string bytes.
--  @param s Source string.
--  @param n (=nil) Last character position.
--  @return byte sum % 8.
local function _byteSum (s, n)
  local v = 0
  for i = 1, (n or #s) do v = v + string.byte(s, i, i) end
  return v % 256
end


--- Make copy of an object or list.
--  @param v Source object.
--  @return deep copy.
local function _copyObj(v)
  if type(v) == "table" then
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
    if n <= 0 then
      error "expected positive size"
    end
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
  local t = {string.pack("B", acc["#"])}
  for _, v in ipairs(src) do
    if type(v) == "table" then
      t[#t+1] = v._pack and v:_pack(acc) or _listPack(v, acc)
    elseif type(v) == "number" then
      t[#t+1] = _utils.packNum(v, acc)
    elseif type(v) == "string" then
      t[#t+1] = _utils.packStr(v, acc)
    else
      error "Unable to pack"
    end
  end
  t[#t+1] = "\0"
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
    n, pos = string.unpack("B", src, pos)
    local key = acc[n]
    if type(key) == "string" then
      if key == "#" then
        t[#t+1], pos = _listUnpack(src, pos, acc, ver)
      elseif string.byte(key, 1) == 0x26 then  -- &
        t[#t+1], pos = _utils.unpackNum(src, pos, key, ver)
      elseif string.byte(key, 1) == 0x22 then  -- "
        t[#t+1], pos = _utils.unpackStr(src, pos, key, ver)
      else   -- Sonata object
        acc[n] = require("matlib."..key)
        t[#t+1], pos = acc[n]._unpack(src, pos, acc, ver)
      end
    else  -- library table
      t[#t+1], pos = key._unpack(src, pos, acc, ver)
    end
  end
  return t, pos+1  -- skip last \0
end


--- Merge sort algorithm.
--  @param up Source list.
--  @param down Buffer.
--  @param left Start index.
--  @param right End index.
--  @param fn Comparison method.
--  @return Sorted list.
local function _mergeSort(up, down, left, right, fn)
  if left == right then
    down[left] = up[left]
    return down
  end
  local middle = left + math.floor(0.5*(right - left))
  -- divide
  local lbuf = _mergeSort(up, down, left, middle, fn)
  local rbuf = _mergeSort(up, down, middle+1, right, fn)
  local target = (lbuf == up) and down or up
  local lcur, rcur = left, middle+1
  -- merge
  for i = left, right do
    local lv, rv = lbuf[lcur], rbuf[rcur]
    if lcur <= middle and rcur <= right then
      if fn(lv, rv) then
        target[i] = lv
        lcur = lcur + 1
      else
        target[i] = rv
        rcur = rcur + 1
      end
    elseif lcur <= middle then
      target[i] = lv
      lcur = lcur + 1
    else
      target[i] = rv
      rcur = rcur + 1
    end
  end
  return target
end


--	INFO

local _help = SonataHelp or {}
-- description
local _about = {
__module__ = "Data rading and processing."
}


--	MODULE

local data = {}


-- List wrapper.
local mtList = {
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
    return setmetatable({_tbl=f(nil, self, ...)}, mtList)
  end
end


--- Access to the wrapped list.
--  @param k Index name.
--  @return data value.
mtList.__index = function (self, k)
  if k == "data" then return self._tbl end
  return mtList[k] or self._tbl[k]
end



--- Binary search in sorted list.
--  @param t Sorted list of elements.
--  @param val Value to search.
--  @param fn (=nil) Function to extract data.
--  @return index and value.
data.binsearch = function (_, t, val, fn)
  local i, u = _utils.binsearch(t, val, fn)
  if u == val then
    return i, u  -- only when found
  end
end
mtList.binsearch = _wrapCall(data.binsearch)
_about[data.binsearch] = {":binsearch(sorted_t, value, [extract_fn]) --> index_i, value",
  "Find position of element in sorted list using binary search.", _tag.LIST}


--- Make copy of a list wrapper.
--  @return copy object.
mtList.copy = function (self)
  local t = {}
  for i = 1, #self._tbl do t[i] = self._tbl[i] end
  return setmetatable({_tbl=t}, mtList)
end


--- Make copy of an object or list.
--  @param v Source object.
--  @return deep copy.
data.copy = function (_, v) return _copyObj(v) end
_about[data.copy] = {":copy(t) --> copy_t",
  "Make deep copy of the table.", _help.OTHER}


--- Save Lua table in file, use given delimiter.
--  @param t Lua table.
--  @param sFile File name.
--  @param char Delimiter, default is coma.
data.csvwrite = function (_, t, sFile, char)
  local f = assert(io.open(sFile, "w"))
  char = char or ","
  for _, v in ipairs(t) do
    if type(v) == "table" then v = table.concat(v, char) end
    f:write(v, "\n")
  end
  f:close()
end
mtList.csvwrite = _wrapCall(data.csvwrite)
_about[data.csvwrite] = {":csvwrite(data_t, file_s, delim_s=',')",
  "Save Lua table as delimiter separated data into file.", _tag.FILES}


--- Import data from text file, use given delimiter.
--  @param sFile File name.
--  @param char Delimiter, default is coma.
--  @return Lua table with data.
data.csvread = function (_, sFile, char)
  local f = assert(io.open(sFile, "r"))
  char = char or ","
  local templ = "([^"..char.."]+)"
  local res = {}
  for s in f:lines("l") do
    -- read data
    if char ~= "#" then
      s = string.match(s, "([^#]+)")    -- skip comments
    end
    s = string.match(s, "^%s*(.*)%s*$")  -- strip line
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
_about[data.csvread] = {":csvread(file_s, delim_s=',') --> tbl",
  "Read delimiter separated data as Lua table.", _tag.FILES}


--- Find elements using condition.
--  @param t Table with data.
--  @param vCond Either boolean function or table of weights.
--  @return Table with the filtered elements.
data.filter = function (_, t, vCond)
  local res = {}
  if type(vCond) == "string" then vCond = _utils.Fn(vCond) end
  if type(vCond) == "function" then
    -- boolean function
    for i = 1, #t do
      local v = t[i]
      if vCond(v) then res[#res+1] = v end
    end
  elseif type(vCond) == "table" then
    -- weights
    for i = 1, #t do
      if vCond[i] ~= 0 then res[#res+1] = t[i] end
    end
  end
  return res
end
mtList.filter = _wrapList(data.filter)
_about[data.filter] = {":filter(in_t, fn|str|tbl) --> out_t",
  "Get result of the table filtering. Condition is boolean function, string or table of weights.",
  _tag.LIST}



--- Apply given function to elements of the list when condition is true.
--  @param t Table of numbers.
--  @param fn Transformation function or string.
--  @param cond Condition function f(v,i) or string.
--  @return obtained list.
data.gen = function (_, t, fn, cond)
  if type(fn) == "string" then fn = _utils.Fn(fn) end
  -- condition function f(value, index)
  if type(cond) == "string" then cond = _utils.Fn(cond) end
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
mtList.gen = _wrapList(data.gen)
_about[data.gen] = {":gen(in_t, fn|str, [cond_fn|cond_str=nil]) --> out_t",
  "Make new list using given transformation. Optional condition function of the form f(value,index).",
  _tag.LIST}


--- Iterate over all n-length combinations for elements from the list t.
--  @param t Source list.
--  @param n Length of combination.
--  @return iterator over combinations.
data.icomb = function (_, t, n)
  local ind, w = {0}, #t - n
  -- iterator
  return function ()
    -- check index
    while #ind > 0 and ind[#ind] - #ind >= w do
      table.remove(ind)
    end
    if #ind == 0 then return end
    -- next index
    local p = table.remove(ind)
    while #ind < n do
      p = p+1
      table.insert(ind, p)
    end
    -- make result
    local res = {}
    for i = 1, n do res[i] = t[ ind[i] ] end
    return res
  end
end
mtList.icomb = _wrapCall(data.icomb)
_about[data.icomb] = {":icomb(list_t, N) --> fn()->t",
  "Iterate over all n-length combinations of elements from the source list."}


--- Make iterator as a sequence of nested iterators.
--  Each next iterator takes argument from the previous one.
--  @param t Sequence of iterators.
--  @param ... Arguments of the first iterator.
--  @return complex iterator.
data.inest = function (_, t, ...)
  local stack = {t[1](...)}  -- init first
  -- iterator
  return function ()
    local p = _ver.pack( stack[#stack]() )
    -- remove completed
    while #p == 0 and #stack > 1 do
      table.remove(stack)
      p = _ver.pack( stack[#stack]() )  -- new element for iteration
    end
    -- update iterators
    while #p > 0 and #stack < #t do
      stack[#stack+1] = t[#stack+1]( _ver.unpack(p) )
      p = _ver.pack( stack[#stack]() )  -- iterate
    end
    return _ver.unpack(p)
  end
end
_about[data.inest] = {":inest(iterators_t, ...) --> fn()->t",
  "Combine sequence of nested iterators, each previous iterator generates agrument for the next one."}


--- Iterate over all permutations for the given list elements.
--  @param t Source list.
--  @return iterator over permutations.
data.iperm = function (_, t)
  local ind, n = {}, #t
  -- init
  local p, loop = n, n
  for i = 1, n do ind[i] = i end
  -- iterator
  return function ()
    if loop > 1 or p > 1 then  -- ignore last permutation
      local res = {}
      for i = 1, n do res[i] = t[ ind[i] ] end
      -- next index
      if p == 1 then
        loop = loop - 1
        p = n
      end
      -- swap
      local q = p - 1
      ind[p], ind[q] = ind[q], ind[p]
      p = q
      return res
    end
  end
end
mtList.iperm = _wrapCall(data.iperm)
_about[data.iperm] = {":iperm(list_t) --> fn()->t",
  "Iterate over all permutations of the source list."}


--- Find weights (1/0) based on condition.
--  @param t Data table.
--  @param fn Condition, boolean function or string.
--  @return Table of 1 and 0.
data.is = function (_, t, fn)
  if type(fn) == "string" then fn = _utils.Fn(fn) end
  local res = {}
  for i = 1, #t do
    res[i] = fn(t[i]) and 1 or 0
  end
  return res
end
mtList.is = _wrapList(data.is)
_about[data.is] = {":is(data_t, fn|str) --> weigh_t",
  "Find weights using condition (boolean function or string).", _tag.LIST}


--- Find weights (1/0) based on inverted condition.
--  @param t Data table.
--  @param fn Condition, boolean function.
--  @return Table of 1 and 0.
data.isNot = function (_, t, fn)
  if type(fn) == "string" then fn = _utils.Fn(fn) end
  local res = {}
  for i = 1, #t do
    res[i] = fn(t[i]) and 0 or 1
  end
  return res
end
mtList.isNot = _wrapList(data.isNot)
_about[data.isNot] = {":isNot(data_t, fn|str) --> weigh_t",
  "Find inverted weights using condition (boolean function or string).", _tag.LIST}



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
    for j = 1, #acc[1] do head[j] = tostring(names_t[j] or "") end
    acc[#acc+1] = head  -- temporary add
  end
  -- save
  local len = _utils.align(acc)
  for j = 1, #len do line[j] = string.rep("-", len[j]) end
  local res, templ = {}, "| %s |"
  if names_t then
    res[1] = string.format(templ, table.concat(acc[#acc], " | "))
    acc[#acc] = nil
  end
  res[#res+1] = string.format("|-%s-|", table.concat(line, "-|-"))
  for _, v in ipairs(acc) do
    res[#res+1] = string.format(templ, table.concat(v, " | "))
  end
  return table.concat(res, "\n")
end
mtList.md = _wrapCall(data.md)
_about[data.md] = {":md(data_t, names_t=nil, row_fn=nil) --> str",
  "Markdown-like table representation. Rows can be processed using function row_fn(t)-->t.",
  _help.OTHER}




--- Apply reduction rule to the list elements.
--  @param t List.
--  @param fn Function of 2 elements.
--  @param val (=0) Initial value.
--  @return Result of reduction.
data.reduce = function (_, t, fn, val)
  val = val or 0
  if type(fn) == "string" then fn = _utils.Fn(fn) end
  for i = 1, #t do val = fn(val, t[i]) end
  return val
end
mtList.reduce = _wrapCall(data.reduce)
_about[data.reduce] = {":reduce(data, fn|str, initial=datadata_t[1]_t[1]) --> var",
  "Apply function to its previous result and next element.", _tag.LIST}


--- Reverse list in place.
--  @param t List of elements.
data.reverse = function (_, t)
  local n, m = math.floor(#t / 2), #t + 1
  for i = 1, n do
    t[i], t[m-i] = t[m-i], t[i]
  end
end
mtList.reverse = function (self) data.reverse(nil, self._tbl); return self end
_about[data.reverse] = {":reverse(data_t)",
  "Reverse table elements.", _tag.LIST}


--- Sort elements in place.
--  @param t List of elements.
--  @param fn Comparison function or string.
data.sort = function (_, t, fn)
  if type(fn) == "string" then fn = _utils.Fn(fn) end
  return _mergeSort(t, {}, 1, #t, fn)
end
mtList.sort = function (self, fn) data.sort(nil, self._tbl, fn); return self end
_about[data.sort] = {":sort(data_t, fn|str)",
  "Sort elements of the list", _tag.LIST}



--- Make table of given size filled with zeros.
--  @param ... Size list.
--  @return table with zeros.
data.zeros = function (_, ...) return _fillRest(0, ...) end
_about[data.zeros] = {":zeros(n1, [n2,..]) --> tbl",
  "Make table with zeros.", _help.OTHER}


--- Apply function of n arguments to n lists.
--  @param fn Function of multiple arguments or string.
--  @param ... Sequence of lists.
--  @return List of values fn(...).
data.zip = function (_, fn, ...)
  local ag, res = {...}, {}
  if type(fn) == "string" then fn = _utils.Fn(fn) end
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
      res[i] = fn(_ver.unpack(x))
    else
      break
    end
  end
  return res
end
_about[data.zip] = {":zip(fn|str, ...) --> tbl",
  "Sequentially apply function to list of tables.", _tag.LIST}


-- Constructor for the list wrapper.
setmetatable(data, {
__call = function (_, t)
  return setmetatable({_tbl=t}, mtList)
end })
_about[data] = {" (data_t) --> new_L",
  "Create list wrapper.", _tag.REF}


-- Methametods for the range of numbers.
local mtRange = {
  type = "range",
  -- methods
  __len = function (self) return self._N end,
  __newindex = function (self, k, v) end,  -- can't modify
}


--- Add number (shift range).
--  @param d Any number.
--  @param R Range object.
--  @return Shifted range table.
mtRange.__add = function (d, R)
  if type(R) == "number" then
    return mtRange.__add(R, d)
  else
    return mtRange._init(d + R._beg, d + R._end, R._step, R._N)
  end
end


--- Substract number.
--  @param R Range object.
--  @param d Any number.
--  @return Shifted range table.
mtRange.__sub = function (R, d)
  if type(R) == "number" then   -- d is range
    return R + (-1)*d
  else
    return mtRange._init(R._beg - d, R._end - d, R._step, R._N)
  end
end


--- Multiply to number (expand range).
--  @param d Any number.
--  @param R Range object.
--  @return Expanded range table.
mtRange.__mul = function (d, R)
  if type(R) == "number" then
    return mtRange.__mul(R, d)
  else
    return mtRange._init(d*R._beg, d*R._end, d*R._step, R._N)
  end
end


--- Pretty print.
--  @param R Range object.
--  @return String with the table representation.
mtRange.__tostring = function (self)
  return string.format("%s{%g, %g .. %g}", self._fn and "fn" or "",
    self._beg, self._beg+self._step, self._end)
end


--- Get i-th element.
--  @param self Range object.
--  @param i Element index.
--  @return Number.
mtRange.__index = function (self, i)
  if _ver.toInteger(i) ~= nil and i > 0 and i <= self._N then
    local v = 0
    if i < self._N then
      v = self._beg + (i - 1)*self._step
    else
      v = self._end
    end
    return v and self._fn and self._fn(v) or v
  else
    return mtRange[i]
  end
end


-- Range methods
_about["_rng"] = {"range: -R, R+x, R-x, k*R, R|fn", nil, _tag.AUX}


--- Initialize range object.
--  @param dBeg First value.
--  @param dEnd Last value.
--  @param dStep Step value.
--  @param iN Number of elements.
--  @return Range object.
mtRange._init = function (dBeg, dEnd, dStep, iN, fn)
  return setmetatable({_beg=dBeg, _end=dEnd, _step=dStep, _N=iN, _fn=fn}, mtRange)
end


--- Make reversed range object.
--  @return new object.
mtRange.reverse = function (self)
  return mtRange._init(self._end, self._beg, -self._step, self._N, self._fn)
end


--- Apply function to range of numbers.
--  @param fn Function f(x).
--  @return modified range of numbers.
mtRange.map = function (self, fn)
  if self._fn then
    local fn1 = function (x) return fn(self._fn(x)) end  -- combine functions
    return mtRange._init(self._beg, self._end, self._step, self._N, fn1)
  else
    return mtRange._init(self._beg, self._end, self._step, self._N, fn)
  end
end
mtRange.__bor = mtRange.map  -- allow  rng | fn1 | fn2


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
  return mtRange._init(dBegin, dEnd, dStep, n)
end
_about[data.range] = {":range(begin_d, end_d, step_d=±1) --> new_R",
  "Generate range object.", _tag.AUX}


--- Generate powers or 10.
--  @param dBegin Beginning of range.
--  @param dEnd End of range.
--  @param dStep Step value (default is 1 or -1).
--  @return table of 10^x, Range object.
data.logrange = function (_, dBeg, dEnd, dStep)
  local range = data.range(_, dBeg, dEnd, dStep)
  return range:map(function (x) return 10^x end)
end
_about[data.logrange] = {":logrange(begin_d, end_d, step_d=±1) --> new_R)",
  "Generate logarithmic range.", _tag.AUX}


-- Get reference to data range in other table
local mtRef = {
  type = "ref" ,
  -- methods
  __len = function (t) return t._end - t._beg end,
  __tostring = function (t) return string.format("<ref %s>", tostring(t._tbl)) end,
}


--- Get i-th element.
--  @param self Ref object.
--  @param i Element index.
--  @return Table value.
mtRef.__index = function (self, i)
  if _ver.toInteger(i) ~= nil then
    local n = (i >= 0) and (i + self._beg) or (i + 1 + self._end)
    if self._beg < n and n <= self._end then
      return self._tbl[n]
    end
  end
  return mtRef[i] or mtList[i]
end


--- Set k-th value.
--  @param self Ref object.
--  @param k Index.
--  @param v Value.
mtRef.__newindex = function (self, k, v)
  if _ver.toInteger(k) ~= nil and 0 < k and (self._beg + k) <= self._end then
    k = k + self._beg
    if getmetatable(v) == mtRef then
      -- copy data
      local i0 = k - 1
      local n = math.min(#v, self._end - i0)
      for i = 1, n do self._tbl[i0+i] = v[i] end
    else
      -- set value
      self._tbl[k] = v
    end
  end
end


--- Get reference range.
--  @return Beginning and the end of data range.
mtRef.range = function (self)
  return self._beg + 1, self._end
end


--- Change bounds or the reference.
--  @param a Left shift.
--  @param b Right shift.
--  @return New ref object.
mtRef.shift = function (self, a, b)
  b = b or a
  return setmetatable({
    _beg=math.max(math.min(self._beg + a, #self._tbl - 1), 0),
    _end=math.max(math.min(self._end + b, #self._tbl), 1),
    _tbl=self._tbl
  }, mtRef)
end
mtRef.__shl = function (ref, n) return mtRef.shift(ref, -n, -n) end
mtRef.__shr = function (ref, n) return mtRef.shift(ref, n, n) end


--- Create reference to other table.
--  @param t Source table.
--  @param iBeg Index of the first element.
--  @param iEnd Index of the last element.
--  @return Reference object.
data.ref = function (_, t, iBeg, iEnd)
  iBeg = iBeg or 1
  iEnd = iEnd or #t
  assert(_ver.toInteger(iBeg) and _ver.toInteger(iEnd), "Wrong index type")
  if getmetatable(t) == mtRef then
    return setmetatable({_beg=t._beg + iBeg - 1, _end=t._beg + iEnd, _tbl=t._tbl}, mtRef)
  end
  return setmetatable({_beg=iBeg - 1, _end=iEnd, _tbl=t}, mtRef)
end
_about[data.ref] = {":ref(data_t, begin_N=1, end_N=#src_t) --> new_R",
  "Return reference to the range of elements.", _tag.REF}


-- Column reference
local mtCol = {
  type="column",
  -- methods
  __len = function (self) return #self._tbl end,
}


--- Get column element.
--  @param k Key.
--  @return found value.
mtCol.__index = function (self, k)
  local tmp = mtCol[k] or mtList[k]
  if tmp then  -- for sequential transformations
    return tmp
  end
  local row = self._tbl[k]
  return row and row[self._n]
end

--- Set element or group of elements.
--  @param k Key, 'data' for group set.
--  @param v Value, scalar or list.
mtCol.__newindex = function (self, k, v)
  if k == "data" then
    if type(v) ~= "table" or #v ~= #self._tbl then
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
  assert(_ver.toInteger(n) and t[1][n], "Out of range")
  return setmetatable({_tbl=t, _n=n}, mtCol)
end
_about[data.col] = {":col(src_t, col_N) --> ref_Col",
  "Make column reference.", _tag.REF}


-- Row reference
local mtRow = {
  type="row",
  -- methods
  __len = function (self) return #self._row end,
}


--- Get row element.
--  @param k Key.
--  @return found value.
mtRow.__index = function (self, k)
  return mtRow[k] or mtList[k] or self._row[k]
end


--- Set element or group of elements.
--  @param k Key, 'data' for group set.
--  @param v Value, scalar or list.
mtRow.__newindex = function (self, k, v)
  if k == "data" then
    if type(v) ~= "table" or #v ~= #self._row then
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
  assert(_ver.toInteger(n) and row, "Out of range")
  return setmetatable({_tbl=t, _row=row, _n=n}, mtRow)
end
_about[data.row] = {":row(src_t, row_N) --> ref_Row",
  "Make row reference.", _tag.REF}


-- Collect data types for packing.
local mtAccum = {
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
  local t = {"/\\/", string.pack("I2", ver), "\0",}
  local acc, bin = setmetatable({_nm={}}, mtAccum), nil
  if type(v) == "table" then
    bin = v._pack and v:_pack(acc) or _listPack(v, acc)
  else
    error "No rules to pack it"
  end
  -- make "vocabulary"
  for _, nm in ipairs(acc._nm) do
    t[#t+1] = string.pack("B", #nm)
    t[#t+1] = nm
  end
  t[#t+1] = "\0"  -- end marker
  t[#t+1] = bin
  -- check sum
  local sum = 0
  for i = 1, #t do sum = sum + _byteSum(t[i]) end
  t[#t+1] = string.pack("B", sum % 256)
  return table.concat(t)
end
_about[data.pack] = {":pack(obj) --> bin_s",
  "Pack object to binary string.", _tag.FILES}


--- Convert binary string to Sonata object.
--  @param v Source string.
--  @return object.
data.unpack = function (_, v)
  if type(v) ~= "string" or string.sub(v, 1, 3) ~= "/\\/" then
    error "Unknown data type"
  end
  if _byteSum(v, #v-1) ~= string.byte(v, #v) then
    error "Wrong check sum"
  end
  local ver, pos = string.unpack("I2", v, 4)
  pos = pos + 1  -- skip zero
  -- restore vocabulary
  local types, n = {}, 0
  while string.byte(v, pos) ~= 0 do
    n, pos = string.unpack("B", v, pos)
    types[#types+1] = string.sub(v, pos, pos+n-1)
    pos = pos + n
  end
  pos = pos + 1
  if types[1] == "#" then  -- lua table
    return _listUnpack(v, pos+1, types, ver)
  else
    types[1] = require("matlib."..types[1])
    return types[1]._unpack(v, pos+1, types, ver)
  end
end
_about[data.unpack] = {":unpack(bin_s) --> obj",
  "Unpack object from binary string.", _tag.FILES}


-- Comment to remove descriptions
data.about = _about

return data

--====================================
-- TODO iterated merge sort
