--[[		sonata/lib/versions.lua

--- Auxilary functions.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2025.

	module 'utils'
--]]


--	MODULE

local _modf = math.modf


--=============== Choose versions =======================

local versions = {
  -- Arctangent with sign
  atan2 = math.atan2 or math.atan,
  -- Execute string code.
  loadStr = loadstring or load,
  -- Extract table values.
  unpack = unpack or table.unpack
}


-- Check type of the number
versions.mathType = math.type or function (x)
  local n = tonumber(x)
  if not n then return nil end
  local _, p = _modf(n)
  return (p == 0.0) and 'integer' or 'float'
end


-- Move elements to new position (and table)
versions.move = table.move or function (src, sfrom, sto, dfrom, dest)
  if dest and (dest ~= src or dfrom < sfrom or dfrom > sto) then
    for i = sfrom, sto do
      dest[dfrom] = src[i]
      dfrom = dfrom + 1
    end
  else
    local temp = versions.move(src, sfrom, sto, sfrom, {})
    dest = versions.move(temp, sfrom, sto, dfrom, src)
  end
  return dest
end


-- Return integer number or nil
versions.toInteger = math.tointeger or function (x)
  if type(x) == 'string' then x = tonumber(x) end
  local p, q = _modf(x)
  return (q == 0.0) and p or nil
end


-- Power as function
versions.pow = math.pow or function (x, y)
  return x^y
end


--============= Cross-module functionality =========
-- Cross methods:
-- _convert, _isZero, _norm, _round, _simp, copy, float

local cross = {}


--- Compare equality of two objects.
--  @param v1 First object.
--  @param v2 Second object.
--  @return True if the objects are equal.
cross.eq = function (v1, v2)
  if     type(v1) == 'table' and v1.__eq then
    return v1:__eq(v2)
  elseif type(v2) == 'table' and v2.__eq then
    return v2:__eq(v1)
  else
    return v1 == v2
  end
end


--- Convert slave into type of master.
--  @param vMaster Master object.
--  @param vSlave Slave object.
--  @return Converted slave of nil.
cross.convert = function (vMaster, vSlave)
  local mt = getmetatable(vMaster)
  return mt and mt._convert and mt._convert(vSlave)
end


--- Get the object copy.
--  @param v Object.
--  @return Deep copy when possible.
cross.copy = function (v)
  local mt = getmetatable(v)
  return mt and mt.copy and mt.copy(v) or v
end


--- Get float value when possible.
--  @param v Some object.
--  @return Float number or nil.
cross.float = function (v)
  local tp = type(v)
  return tp == 'number' and v or tp == 'table' and v:float() or nil
end


--- Check if the number equal 0.
--  @param v Some object.
--  @return true when v == 0
cross.isZero = function (v)
  local mt = getmetatable(v)
  if mt and mt._isZero then
    return mt._isZero(v)
  end
  return v == 0
end


--- Norm of numeric object.
--  @param v Some object.
--  @return Norm or nil.
cross.norm = function (v)
  local tp = type(v)
  if tp == 'number' then
    return math.abs(v)
  elseif tp == 'table' then
    return v.float and math.abs(v:float()) or v._norm and v:_norm() or nil
  end
  return nil
end


--- Round float number for required number of digits.
--  @param v Number for rounding.
--  @param fTol Required tolerance (1E-k)
--  @return Rounded value.
cross.round = function (v, fTol)
  if type(v) == 'number' then
    local p, q = _modf(v / fTol)
    if     q >  0.5 then p = p + 1
    elseif q < -0.5 then p = p - 1
    end
    return p * fTol
  end
  return type(v) == 'table' and v._round and v:_round(fTol) or v
end


--- Apply simplification if possible
--  @param v Sonata object.
--  @return Number or the object itself.
cross.simp = function (v)
  local mt = getmetatable(v)
  return mt and mt._simp and mt._simp(v) or v
end


--============== Utils ================

local NUM_DOT = '%.'
local NUM_DIG = '^%d+$'
local NUM_EXP = '^%d+[eE]%d*$'
local NUM_SGN = '^[+-]$'


local utils = {
  TEMPL = '([%w_]*)%s*([^%w%s_]?)%s*',   -- var and symbol
  -- parse number
  NUM_FSM = {
    [NUM_DIG] = {NUM_DOT},
    [NUM_DOT] = {NUM_DIG, NUM_EXP},      -- use as start
    [NUM_EXP] = {NUM_SGN, NUM_DIG},
    [NUM_SGN] = {NUM_DIG},
  },
}


--- Transform sequence of strings to number.
--  @param t Table with strings.
--  @return Table with numbers when possible.
local function _toNumbers (t)
  local res, num = {}, {}
  local s = NUM_DOT
  for _, v in ipairs(t) do
    -- check expected element
    local found = false
    for _, tmpl in ipairs(utils.NUM_FSM[s]) do
      if string.match(v, tmpl) then
        s, found = tmpl, true
        num[#num+1] = v
        break
      end
    end
    if not found then
      -- save, update state
      if #num > 0 then
        res[#res+1] = tonumber(table.concat(num))  -- TODO process error
        num, s = {}, NUM_DOT
      end
      res[#res+1] = v
    end
  end
  if #num > 0 then
    res[#res+1] = tonumber(table.concat(num))
  end
  return res
end


--- Find element that equal or bigger then the given one.
--  @param t Table with sorted data.
--  @param val Value for search.
--  @param fn Function to extract element from table (optional).
--  @return index and value of the element.
utils.binsearch = function (t, val, fn)
  fn = fn or function (x) return x end
  local istart, iend = 1, #t
  if val <= fn(t[1]) then return 1, t[1] end
  if val >= fn(t[iend]) then return iend, t[iend] end
  while (iend - istart) > 1 do
    local imid = math.floor((istart+iend) * 0.5)
    local vm = fn(t[imid])
    if val < vm then
      iend = imid
    elseif val > vm then
      istart = imid
    else  -- val == vm
      return imid, vm
    end
  end
  return iend, fn(t[iend])
end


--- Generate function from string.
--  @param sExpr Expression for execution.
--  @return Function based on the expression.
utils.Fn = function (sExpr)
  local a, b = string.match(sExpr, "(.*)->(.+)")
  if not a then 
    a, b = 'x', sExpr
  end
  local fn = versions.loadStr(
    string.format("return function (%s) return %s end", a, b))
  return fn and fn()
end


--- 'Smart' number to string conversation.
--  @param d Number.
--  @return String representation.
utils.numstr = function (d)
  local int, frac = _modf(d)
  local a = math.abs(int)
  -- short integer
  if frac == 0 then
    if a < 1E5 then
      return string.format('%d', int)
    end
  elseif int == 0 then
    if math.abs(frac) >= 0.01 then
      return string.format("%.3f", d)
    end
  elseif a < 10 then
    return string.format('%.3f', d)
  elseif a < 100 then
    return string.format('%.2f', d)
  elseif a < 1000 then
    return string.format('%.1f', d)
  end
  return string.format('%.2E', d)
end


--- Simple lexer for algebraic expressions.
--  @param s String to parse.
--  @return List of tokens.
utils.lex = function (s)
  local res = {}
  for x, y in string.gmatch(s, utils.TEMPL) do
    if #x > 0 then res[#res+1] = x end
    if #y > 0 then res[#res+1] = y end
  end
  return _toNumbers(res)
end


--- Check sign if possible.
--  @param d Value to check.
--  @return -1, 0 or 1
utils.sign = function (d)
  local tp = type(d)
  if tp == 'number' or (tp == 'table' and d.__lt) then
    return (d > 0) and 1 or (d < 0) and -1 or 0
  else
    return 1
  end
end


--- Find maximal width for each column, align size.
--  @param tbl Table with lists of strings.
--  @param right Alignment to right.
--  @return list of width.
utils.align = function (tbl, right)
  local base = right and '%%%ds' or '%%-%ds'
  local len = {}
  for _, row in ipairs(tbl) do
    for j, str in ipairs(row) do len[j] = math.max(len[j] or 0, #str) end
  end
  -- align
  for j, d in ipairs(len) do
    local templ = string.format(base, d)
    for _, row in ipairs(tbl) do
      row[j] = string.format(templ, row[j])
    end
  end
  return len
end


--- Define number size and convert it to binary string.
--  @param x Value to pack.
--  @param acc Accumulator table.
--  @return binary string.
utils.pack_num = function (x, acc)
  local v, p = _modf(math.abs(x))
  if p == 0.0 then
    -- integer
    if v < 128 then
      return string.pack('Bb', acc['&b'], x)
    elseif v < 32768 then
      return string.pack('Bi2', acc['&i2'], x)
    elseif v < 2.1E9 then
      return string.pack('Bi4', acc['&i4'], x)
    elseif v < 9.2E18 then
      return string.pack('Bi8', acc['&i8'], x)
    else
      return string.pack('Bd', acc['&d'], x)
    end
  else
    -- float
    if v > 3.4E38 or p < -3.4E38 then
      return string.pack('Bd', acc['&d'], x)
    else
      return string.pack('Bf', acc['&f'], x)
    end
  end
end


--- Pack sequence of numbers or objects.
--  @param src List of numbers/objects.
--  @param i0 Start index.
--  @param ii End index.
--  @param acc Accumulator table.
--  @return binary string.
utils.pack_seq = function (src, i0, ii, acc)
  local t, pack_num = {}, utils.pack_num
  for i = i0, ii do
    local x = src[i]
    if type(x) == 'number' then
      t[#t+1] = pack_num(x, acc)
    elseif type(x) == 'table' and x._pack then
      t[#t+1] = x:_pack(acc)
    else
      error "Unable to pack"
    end
  end
  return table.concat(t)
end


--- Check size and pack string.
--  @param s Source string.
--  @param acc Accumulator table.
--  @return binary string.
utils.pack_str = function (s, acc)
  local n = #s
  if n < 256 then
    return string.pack('BB', acc['"B'], n) .. s
  elseif n < 65536 then
    return string.pack('BI2', acc['"I2'], n) .. s
  elseif n < 16777216 then
    return string.pack('BI3', acc['"I3'], n) .. s
  elseif n < 4294967296 then
    return string.pack('BI4', acc['"I4'], n) .. s
  else
    error "Too big string, max is 4294967296"
  end
end


--- Get number from binary string.
--  @param s Source string.
--  @param pos Start position.
--  @param key Number tag.
--  @param ver Pack version algorithm.
--  @return number and next position.
utils.unpack_num = function (s, pos, key, ver)
  -- remove & to get template
  return string.unpack(string.sub(key, 2), s, pos)
end


--- Get sequence of numbers/objects from binary string.
--  @param s Source string.
--  @param pos Start position.
--  @param acc Accumulator table.
--  @param ver Pack version algorithm.
--  @return table with resutl, next position.
utils.unpack_seq = function (len, s, pos, acc, ver)
  local t, n, unpack_num = {}, nil, utils.unpack_num
  for i = 1, len do
    n, pos = string.unpack('B', s, pos)
    local key = acc[n]
    if type(key) == "string" then
      if string.byte(key, 1) == 0x26 then  -- &
        t[i], pos = unpack_num(s, pos, key, ver)
      else
        acc[n] = require('matlib.'..key)
        t[i], pos = acc[n]._unpack(s, pos, acc, ver)
      end
    else
      t[i], pos = key._unpack(s, pos, acc, ver)
    end
  end
  return t, pos
end


--- Get unpacked string.
--  @param s Source string.
--  @param pos Start position.
--  @param key Number tag.
--  @param ver Pack version algorithm.
--  @return string and next position.
utils.unpack_str = function (s, pos, key, ver)
  local n, pos = string.unpack(string.sub(key, 2), s, pos)
  return string.sub(s, pos, pos+n-1), pos+n
end


--============== Calc ================

local mexp, msqrt, mlog = math.exp, math.sqrt, math.log

-- Additional functions.
local calc = {
  -- hyperbolic cosine
  cosh = function (x) return 0.5*(mexp(x) + mexp(-x)) end,
  -- hyperbolic sine
  sinh = function (x) return 0.5*(mexp(x) - mexp(-x)) end,
  -- inverse hyperbolic sine
  asinh = function (x) return mlog(x + msqrt(x*x+1)) end,
  -- inverse hyperbolic cosine
  acosh = function (x) return mlog(x + msqrt(x*x-1)) end,
  -- inverse hyperbolic tangent
  atanh = function (x) return 0.5*mlog((1 + x)/(1 - x)) end,
  -- hyperbolic tangent
  tanh = function (x)
    local t = mexp(2*x)
    return (t-1)/(t+1)
  end,
}


--============== Queue ================

--- Queue data structure
local queue = {
  -- create object
  new = function () return {{}, {}} end,
  -- add element
  push = function (self, v) table.insert(self[1], v) end,
  -- check content
  isEmpty = function (self) return #self[2] == 0 and #self[1] == 0 end
}


-- Remove element.
queue.pop = function (self)
  local q1, q2 = self[1], self[2]
  if #q2 == 0 then
    while #q1 > 0 do
      table.insert(q2, table.remove(q1))
    end
  end
  return table.remove(q2)
end


--============== Bin Tree ================

local tree = {
  -- create object
  new = function (l, r, v, isleaf) 
    return {left=l, right=r, val=v, isleaf=isleaf} 
  end,
  -- check node
  isNode = function (t) return t.isleaf ~= true end,
  -- check leaf
  isLeaf = function (t) return t.isleaf == true end,
  -- flags
  LEAF = true, NODE = false,
}


-- export
return {
  versions = versions,
  cross = cross,
  utils = utils,
  calc = calc,
  queue = queue,
  tree = tree,
}

--===================================================
--TODO setup for number of digits
