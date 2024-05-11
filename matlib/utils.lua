--[[		sonata/lib/versions.lua

--- Auxilary functions.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2024.

	module 'utils'
--]]


--	MODULE

local mmodf = math.modf


--=============== Choose versions =======================

local versions = {
  -- Arctangent with sign
  atan2 = math.atan2 or math.atan,
  -- Execute string code.
  loadStr = loadstring or load,
  -- Extract table values.
  unpack = unpack or table.unpack
}


-- Check if the number is integer
versions.isInteger = math.tointeger or function (x)
  if type(x) == 'string' then x = tonumber(x) end
  if not x then return false end
  local v, p = mmodf(x)
  return p == 0.0 and v >= -1E9 and v <= 1E9
end


-- Check type of the number
versions.mathType = math.type or function (x)
  local n = tonumber(x)
  if not n then return nil end
  local _, p = mmodf(n)
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
  local p, q = mmodf(x)
  return (q == 0.0) and p or nil
end


-- Power as function
versions.pow = math.pow or function (x, y)
  return x^y
end


--============= Cross-module functionality =========

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
    local p, q = mmodf(v / fTol)
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
utils._toNumbers = function (t)
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
--  @param N Number of arguments.
--  @return Function based on the expression.
utils.Fn = function (sExpr, N)
  local arg = {}
  for i = 1, N do arg[i] = string.format("x%d", i) end
  local fn = versions.loadStr(
    string.format("return function (%s) return %s end",
      table.concat(arg, ','), sExpr))
  return fn and fn()
end


--- 'Smart' number to string conversation.
--  @param d Number.
--  @return String representation.
utils.numstr = function (d)
  local int, frac = mmodf(d)
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
  return utils._toNumbers(res)
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


--============== Calc ================

local calc = {}


--- Hyperbolic cosine.
--  @param x Float number.
--  @return Hyperbolic cosine value.
calc.cosh = function (x) return 0.5*(math.exp(x)+math.exp(-x)) end


--- Hyperbolic sine.
--  @param x Float number.
--  @return Hyperbolic sine value.
calc.sinh = function (x) return 0.5*(math.exp(x)-math.exp(-x)) end


--- Hyperbolic tangent.
--  @param x Float number.
--  @return Hyperbolic tangent value.
calc.tanh = function (x) t = math.exp(2*x); return (t-1)/(t+1) end


--- Inverse hyperbolic sine.
--  @param x Float number.
--  @return Inverse hyperbolic sine value.
calc.asinh = function (x) return math.log(x+math.sqrt(x*x+1)) end


--- Inverse hyperbolic cosine.
--  @param x Float number.
--  @return Inverse hyperbolic cosine value.
calc.acosh = function (x) return math.log(x+math.sqrt(x*x-1)) end


--- Inverse hyperbolic tangent.
--  @param x Float number.
--  @return Inverse hyperbolic tangent value.
calc.atanh = function (x) return 0.5*math.log((1+x)/(1-x)) end


return {
  versions = versions,
  cross = cross,
  utils = utils,
  calc = calc,
}

--===================================================
--TODO setup for number of digits
