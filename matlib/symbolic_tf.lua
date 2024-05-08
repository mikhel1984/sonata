--[[		sonata/lib/symbase.lua

--- Basic symbolical operations.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2024.

	module 'symbase'
--]]


--	LOCAL

local Upack do
  local lib = require('matlib.utils')
  Upack = lib.versions.unpack
end


--- Condition for element sorting.
--  @param S1 Symbolic object.
--  @param S2 Symbolic object.
--  @return true when S1 < S2.
local compList = function (S1, S2) return S1[2]._sign < S2[2]._sign end


--	MODULE

local PARENTS = {}


local symbolic = {
-- mark
type = 'symbolic',
-- main types
_parentList = PARENTS,
}


--- Check object type.
--  @param v Object.
--  @return True if the object is symbolic.
local function issym(v) return getmetatable(v) == symbolic end


-- Combine 'common' methods
local COMMON = {

  -- Do nothing.
  empty = function (S) end,

  --- Check if the value is 1.
  --  @param v Some object.
  --  @return v == 1.
  isOne = function (v) return issym(v) and v._ == 1 or v == 1 end,

  --- Check if the value is 0.
  --  @param v Some object.
  --  @return v == 0
  isZero = function (v) return issym(v) and v._ == 0 or v == 0 end,

  -- Return false always.
  skip = function (S) return false end,

} -- COMMON


--- Copy content of the simbolic object.
--  @param dst Destination object.
--  @param src Source object.
COMMON.copy = function (dst, src)
  dst._parent = src._parent
  dst._ = src._
  dst._sign = src.p_isatom and src._sign or nil
end


--- Check equality for objects based on lists.
--  @param S1 First symbolic object.
--  @param S2 Second symbolic object.
--  @return true when objects are equal.
COMMON.eq = function (S1, S2)
  if S1._sign ~= S2._sign or S1._parent ~= S2._parent or #S1._ ~= #S2._ then
    return false
  end
  for i = 1, #S1._ do
    local si1, si2 = S1._[i], S2._[i]
    if not si1:p_eq(si2) then
      return false
    end
  end
  return true
end


--- Check equality for objects based on lists of pairs.
--  @param S1 First symbolic object.
--  @param S2 Second symbolic object.
--  @return true when objects are equal.
COMMON.eqPairs = function (S1, S2)
  if S1._sign ~= S2._sign or S1._parent ~= S2._parent or #S1._ ~= #S2._ then
    return false
  end
  for i = 1, #S1._ do
    local si1, si2 = S1._[i], S2._[i]
    if not (si1[1] == si2[1] and si1[2]:p_eq(si2[2])) then
      return false
    end
  end
  return true
end


--- Evaluate expression for list.
--  @param S Symbolic object.
--  @param tEnv Table with elements for substitution.
--  @return New object.
COMMON.eval = function (S, tEnv)
  local res = symbolic:_newExpr(S._parent, {})
  for i, v in ipairs(S._) do
    res._[i] = v:p_eval(tEnv)
  end
  return res
end


--- Evaluate expression for list of pairs.
--  @param S Symbolic object.
--  @param tEnv Table with elements for substitution.
--  @return New object.
COMMON.evalPairs = function (S, tEnv)
  local res = symbolic:_newExpr(S._parent, {})
  for i, v in ipairs(S._) do
    res._[i] = {v[1], v[2]:p_eval(tEnv)}
  end
  return res
end


--- Find signature of an object based on list.
--  @param S Symbolic object.
--  @return true when update signature.
COMMON.signature = function (S)
  local found = false
  for i = 1, #S._ do
    found = S._[i]:p_signature() or found
  end
  if S._sign and not found then return false end
  local sum = S.p_id
  for i = 1, #S._ do sum = (sum*8 + S._[i]._sign) % 1000000 end
  S._sign = sum
  return true
end


--- Find signature of an object based on list of pairs.
--  @param S Symbolic object.
--  @return true when update signature.
COMMON.signaturePairs = function (S)
  -- check elements
  local found = false
  for i = 1, #S._ do
    found = S._[i][2]:p_signature() or found
  end
  if S._sign and not found then return false end
  table.sort(S._, compList)
  local sum = S.p_id
  for i = 1, #S._ do sum = (sum*8 + S._[i][2]._sign) % 1000000 end
  S._sign = sum
  return true
end


--- Try to combine (simplify) to pairs {k, S}
--  @param a First pair.
--  @param b Second pair.
--  @param tParent Parent reference.
COMMON.simpPairElements = function (a, b, tParent)
  local a2, b2 = a[2], b[2]
  if a2._parent == PARENTS.const and a2._parent == b2._parent then
    -- c1*c2 + c3*c4  or  c1^c2 * c3^c4
    if tParent == PARENTS.product then
      a[2] = symbolic:_newConst(a[1]*a2._ + b[1]*b2._)
    else  -- PARENTS.power
      a[2] = symbolic:_newConst(a2._^a[1] * b2._^b[1])
    end
    a[1], b[1] = 1, 0
  elseif b2:p_eq(a2) then
    -- c1*v + c2*v  or v^c1 * v^c3
    a[1], b[1] = a[1] + b[1], 0
  end
end


--- Simplify list of pairs (in place).
--  @param S Symbolic object.
--  @param tParent Parent reference.
COMMON.simpPair = function (S, tParent)
  -- be sure that signature is found
  S:p_signature()
  -- update coefficients
  for _, v in ipairs(S._) do
    if v[2]._parent == tParent then
      local k = v[2]:p_getConst()
      if k ~= 1 then
        v[1] = v[1] * k
        v[2]:p_signature()
      end
    end
  end
  -- combine elements
  for i, si in ipairs(S._) do
    if si[1] ~= 0 then
      for j = i+1, #S._ do
        local sj = S._[j]
        if sj[1] ~= 0 then
          COMMON.simpPairElements(si, sj, tParent)
        end
      end
    end
  end
  -- remove zeros
  for i = #S._, 1, -1 do
    if S._[i][1] == 0 then
      table.remove(S._, i)
      S._sign = nil
    end
  end
end


-- ============ CONSTANT ============

PARENTS.const = {
  -- S._ = value
  p_id = 1,
  p_isatom = true,
  p_signature = COMMON.skip,
  p_eq = function (S1, S2) return S1._ == S2._ end,
  p_str = function (S) return tostring(S._) end,
  p_simp = COMMON.empty,
  p_eval = function (S) return S end,
  p_diff = function (S1, S2) return symbolic._0 end,
  p_internal = function (S, n)
    return string.format('%s%s', string.rep(' ', n), tostring(S._))
  end,
}


-- ============ FUNCTION CALL ============

PARENTS.funcValue = {
  -- S._ = {fn, arg1, arg2, ...}
  p_id = 2,
  p_isatom = false,
  p_signature = COMMON.signature,
  p_eq = COMMON.eq,
  p_simp = function (S, bFull)
    for _, v in ipairs(S._) do v:p_simp(bFull) end
  end,
}


PARENTS.funcValue.p_diff = function (S1, S2)
  local res = symbolic._0
  local args = {}
  for i = 2, #S1._ do args[#args+1] = S1._[i] end
  local diffs = S1._[1]:p_diff(S2)
  if diffs then
    -- has predefined derivatives
    if #args ~= #diffs then 
      error 'Wrong arguments number'
    end
    for i, fn in ipairs(diffs) do
      local dx = args[i]:p_diff(S2)
      if not COMMON.isZero(dx) then res = res + fn(Upack(args)) * dx end
    end
  else
    -- derivative not defined
    local df = symbolic:_newExpr(
      PARENTS.funcValue, {symbolic._fnInit.diff, S1._[1], S2})
    for _, v in ipairs(args) do
      local dx = v:p_diff(S2)
      if not COMMON.isZero(dx) then res = res + df * dx end
    end
  end
  return res
end


--- Evaluate function value.
--  @param S Symbolic object.
--  @param tEnv Elements for substitution.
--  @return New object.
PARENTS.funcValue.p_eval = function (S, tEnv)
  local t, val = {S._[1]}, {}
  for i = 2, #S._ do
    local v = S._[i]:p_eval(tEnv)
    t[i] = v
    if v._parent == PARENTS.const then
      val[#val+1] = v._
    end
  end
  local body = symbolic._fnList[S._[1]._].body
  if #val + 1 == #t and body then
    -- evaluate
    return symbolic:_newConst( body(Upack(val)) )
  else
    return symbolic:_newExpr(S._parent, t)
  end
end


--- Function internal struct.
--  @param S Symbolic object.
--  @param n Shift.
--  @return string representation.
PARENTS.funcValue.p_internal = function (S, n)
  local t = {string.format('%sCALL', string.rep(' ', n))}
  for _, v in ipairs(S._) do
    t[#t+1] = v:p_internal(n+2)
  end
  return table.concat(t, '\n')
end


--- Text form.
--  @param S Symbolic object.
--  @return string.
PARENTS.funcValue.p_str = function (S)
  local t = {}
  for i = 2, #S._ do t[#t+1] = S._[i]:p_str() end
  return string.format('%s(%s)', S._[1]._, table.concat(t, ','))
end


-- ============ POWER ============

PARENTS.power = {
  -- S._ = {base, power}
  p_id = 3,
  p_isatom = false,
  p_signature = COMMON.signature,
  p_eq = COMMON.eq,
  p_eval = COMMON.eval,
}


--- Split power to numeric and symbolic parts.
--  @param S Symbolic object.
--  @return Constant value.
PARENTS.power.p_getConst = function (S)
  local v = S._[2]
  if v._parent == PARENTS.const then
    v = v._
    COMMON.copy(S, S._[1])
    return v
  end
  return 1
end


--- Simplify power (in place).
--  @param S Symbolic object.
--  @param bFull Flag for recursive simplification.
PARENTS.power.p_simp = function (S, bFull)
  if bFull then
    S._[1]:p_simp(bFull)
    S._[2]:p_simp(bFull)
  end
  if COMMON.isOne(S._[2]) then       -- v^1
    COMMON.copy(S, S._[1])
  elseif COMMON.isZero(S._[2]) then  -- v^0
    COMMON.copy(S, symbolic._1)
  elseif COMMON.isOne(S._[1]) or COMMON.isZero(S._[1]) then  -- 1^v or 0^v
    COMMON.copy(S, S._[1])
  elseif S._[1]._parent == S._[2]._parent and S._[1]._parent == PARENTS.const 
  then
    COMMON.copy(S, symbolic:_newConst(S._[1]._ ^ S._[2]._))
  end
end


--- Power object to string translation.
--  @param S Symbolic object.
--  @return String representation.
PARENTS.power.p_str = function (S)
  local base = S._[1]:p_str()
  if COMMON.closed[S._[1]._parent] then
    base = string.format('(%s)', base)
  end
  --if COMMON.isOne(S._[2]) then return base end
  local pow = S._[2]:p_str()
  if COMMON.closed[S._[2]._parent] then
    pow = string.format('(%s)', pow)
  end
  return string.format('%s^%s', base, pow)
end


--- Power internal struct.
--  @param S Symbolic object.
--  @param n Shift.
--  @return string representation.
PARENTS.power.p_internal = function (S, n)
  return string.format('%sPOWER:\n%s\n%s',
    string.rep(' ', n), S._[1]:p_internal(n+2), S._[2]:p_internal(n+2))
end


--- Power derivative.
--  @param S1 First object.
--  @param S2 Second object.
--  @return derivative object.
PARENTS.power.p_diff = function (S1, S2)
  local res = nil
  local a, b = S1._[1], S1._[2]
  local dx = a:p_diff(S2)
  -- da/dx
  if not COMMON.isZero(dx) then
    res = b * a ^ (b - symbolic._1) * dx
  end
  dx = b:p_diff(S2)
  if not COMMON.isZero(dx) then
    local prod = S1 * symbolic._fnInit.log(a)
    res = res and (res + prod) or prod
  end
  return res
end


-- ============ PRODUCT ============

PARENTS.product = {
  -- S._ = {{pow1, S1}, {pow2, S2}, ...}
  p_id = 4,
  p_isatom = false,
  p_signature = COMMON.signaturePairs,
  --p_eq = COMMON.eqPairs,
  p_eval = COMMON.evalPairs,
}


PARENTS.product.p_eq = function (S1, S2)
  return COMMON.eqPairs(S1, S2) or
    #S1._ == 1 and S2._parent == PARENTS.power and
    --    S2:p_eq(S1._[1][2] ^ S1._[1][1])
    -- compare power
    S2._[2]._parent == PARENTS.const and S1._[1][1] == S2._[2]._ and
    -- compare base
    S1._[1][2]:p_eq(S2._[1])
end


--- Split product to numeric and symbolic parts.
--  @param S Symbolic object.
--  @return Constant value.
PARENTS.product.p_getConst = function (S)
  local v = S._[1]
  if v[2]._parent == PARENTS.const then
    v = v[1] * v[2]._  -- coefficient * const
    if #S._ == 2 and COMMON.isOne(S._[2][1]) then    -- val^1
      COMMON.copy(S, S._[2][2])
    else
      table.remove(S._, 1)
      S._sign = nil
    end
    return v
  end
  return 1
end


--- Simplify product (in place).
--  @param S Symbolic object.
--  @param bFull Flag for recursive simplification.
PARENTS.product.p_simp = function (S, bFull)
  if bFull then
    COMMON.simpPair(S, PARENTS.power)  -- check for similar terms
    for _, v in ipairs(S._) do v[2]:p_simp(bFull) end
  end
  COMMON.simpPair(S, PARENTS.power)
  -- empty list
  if #S._ == 0 then
    COMMON.copy(S, symbolic._1)
    return
  elseif #S._ > 1 then
    table.sort(S._, compList)
  end
  -- check constant
  if COMMON.isZero(S._[1][2]) then
    COMMON.copy(S, symbolic._0)
    return
  elseif #S._ > 1 and COMMON.isOne(S._[1][2]) then
    table.remove(S._, 1)
    S._sign = nil
  end
  -- change type
  if #S._ == 1 and S._[1][1] == 1 then
    COMMON.copy(S, S._[1][2])
  end
end


--- Sum object to string translation.
--  @param S Symbolic object.
--  @return String representation.
PARENTS.product.p_str = function (S)
  local num, denom = {}, {}
  for _, v in ipairs(S._) do
    local v1, v2 = v[1], v[2]:p_str()
    if #S._ > 1 and COMMON.closed[v[2]._parent] then
      v2 = string.format('(%s)', v2)
    end
    if v1 > 0 then
      num[#num+1] =
        (v1 == 1) and v2 or string.format('%s^%s', v2, tostring(v1))
    else  -- v[1] < 0
      denom[#denom+1] =
        (v1 == -1) and v2 or string.format('%s^%s', v2, tostring(-v1))
    end
  end
  if #denom == 0 then
    return table.concat(num, '*')
  else
    num = #num > 0 and table.concat(num, '*') or '1'  -- reuse
    return string.format(
      #denom > 1 and "%s/(%s)" or "%s/%s", num, table.concat(denom, '*'))
  end
end


PARENTS.product.p_diff = function (S1, S2)
  local res = symbolic._0
  for _, v in ipairs(S1._) do
    local dx = v[2]:p_diff(S2)
    if not COMMON.isZero(dx) then
      local k = v[1]
      v[1] = k - 1  -- reduce power
      res = res + (k * dx) * S1  -- TODO k * dx directly into table
      v[1] = k      -- set back
    end
  end
  return res
end


PARENTS.product.p_internal = function (S, n)
  local t = {string.format('%sPROD:', string.rep(' ', n))}
  local offset = string.rep(' ', n+2)
  for _, v in ipairs(S._) do
    t[#t+1] = v[2]:p_internal(n+2)
    t[#t+1] = string.format('%s[^ %s]', offset, tostring(v[1]))
  end
  return table.concat(t, '\n')
end


-- ============ SUM ============

PARENTS.sum = {
  -- S._ = {{k1, S1}, {k2, S2}, ...} 
  -- i.e. k1*S1 + k2*S2 + ...
  p_id = 5,
  p_isatom = false,
  p_signature = COMMON.signaturePairs,
  p_eq = COMMON.eqPairs,
  p_eval = COMMON.evalPairs,
}


PARENTS.sum.p_diff = function (S1, S2)
  local res = symbolic:_newExpr(PARENTS.sum, {})
  for _, v in ipairs(S1._) do
    table.insert(res._, {v[1], v[2]:p_diff(S2)})
  end
  res:p_simp()
  res:p_signature()
  return res
end


PARENTS.sum.p_internal = function (S, n)
  local t = {string.format('%sSUM:', string.rep(' ', n))}
  local offset = string.rep(' ', n+2)
  for _, v in ipairs(S._) do
    t[#t+1] = string.format('%s[%s *]', offset, tostring(v[1]))
    t[#t+1] = v[2]:p_internal(n+2)
  end
  return table.concat(t, '\n')
end


--- Simplify sum (in place).
--  @param S Symbolic object.
--  @param bFull Flag for recursive simplification.
PARENTS.sum.p_simp = function (S, bFull)
  if bFull then
    COMMON.simpPair(S, PARENTS.product)  -- check for similar terms
    for _, v in ipairs(S._) do v[2]:p_simp(bFull) end
  end
  -- update structure
  COMMON.simpPair(S, PARENTS.product)
  -- empty list
  if #S._ == 0 then
    COMMON.copy(S, symbolic._0)
    return
  end
  -- sort and remove zero constant
  if #S._ > 1 then
    table.sort(S._, compList)
    if COMMON.isZero(S._[1][2]) then
      table.remove(S._, 1)
      S._sign = nil
    end
  end
  -- change type
  if #S._ == 1 then
    local v = S._[1]
    if v[1] ~= 1 then
      COMMON.copy(S, symbolic:_newConst(v[1]) * v[2])
    else
      COMMON.copy(S, v[2])
    end
  end
end


--- Product object to string translation.
--  @param S Symbolic object.
--  @return String representation.
PARENTS.sum.p_str = function (S)
  local plus, minus = {}, {}
  for _, v in ipairs(S._) do
    local v1, v2 = v[1], v[2]:p_str()
    if v1 > 0 then
      plus[#plus+1] =
        (v1 == 1) and v2 or string.format('%s*%s', tostring(v1), v2)
    else
      minus[#minus+1] =
        (v1 == -1) and v2 or string.format('%s*%s', tostring(-v1), v2)
    end
  end
  if #minus == 0 then
    return table.concat(plus, '+')
  else
    return string.format(
      '%s-%s', table.concat(plus, '+'), table.concat(minus, '-'))
  end
end


-- ============ SYMBOL ============

PARENTS.symbol = {
  -- S._ = name
  p_id = 6,
  p_isatom = true,
  p_signature = COMMON.skip,
  p_eq = function (S1, S2) return S1._ == S2._ end,
  p_simp = COMMON.empty,
  p_internal = function (S, n)
    return string.format('%s%s%s',
      string.rep(' ', n), S._, symbolic._fnList[S._] and '()' or '')
  end,
}


PARENTS.symbol.p_diff = function (S1, S2)
  if symbolic._fnList[S1._] then
    return symbolic._fnDiff[S1._] or nil
  else
    return S1._ == S2._ and symbolic._1 or symbolic._0
  end
end


PARENTS.symbol.p_eval = function (S, tEnv)
  local v = tEnv[S._]
  return v and (issym(v) and v or symbolic:_newConst(v)) or S
end


PARENTS.symbol.p_str = function (S, isFull)
  local nm = S._
  local lst = symbolic._fnList[nm]
  if isFull then
    return lst and string.format('%s(%s): %s',
      nm, table.concat(lst.args or {}, ','), tostring(lst.body or '')) or nm
  else
    return lst and nm..'()' or nm
  end
end


--- List of elements for printing in brackets.
COMMON.closed = {
[PARENTS.sum] = true,
[PARENTS.product] = true,
-- [PARENTS.power] = true,
}


--- S1 + S2
--  @param S1 Symbolic object or number.
--  @param S2 Symbolic object or number.
--  @return Sum object.
symbolic.__add = function (S1, S2)
  S1, S2 = symbolic._toSym(S1, S2)
  local res = symbolic:_newExpr(PARENTS.sum, {})
  -- S1
  if S1._parent == PARENTS.sum then
    for _, v in ipairs(S1._) do table.insert(res._, {v[1], v[2]}) end
  else
    table.insert(res._, {1, S1})
  end
  -- S2
  if S2._parent == PARENTS.sum then
    for _, v in ipairs(S2._) do table.insert(res._, {v[1], v[2]}) end
  else
    table.insert(res._, {1, S2})
  end
  res:p_simp()
  res:p_signature()
  return res
end


--- Call symbolic function.
--  @param S Symbolic object.
--  @param ... List of arguments.
--  @return Function value (symbolic object).
symbolic.__call = function (S, ...)
  if S:_isfn() then
    local t = {...}
    local fn = symbolic._fnList[S._]
    if #t ~= #fn.args then
      error(string.format("Expected %d arguments for %s", #fn.args, S._))
    end
    if type(fn.body) == 'function' or fn.body == nil then
      table.insert(t, 1, S)
      return symbolic:_newExpr(PARENTS.funcValue, t)
    elseif issym(fn.body) then
      local env = {}
      for i, k in ipairs(fn.args) do env[k] = t[i] end
      return symbolic.eval(fn.body, env)
    end
  end
end


--- S1 / S2
--  @param S1 Symbolic object or number.
--  @param S2 Symbolic object or number.
--  @return Ratio object.
symbolic.__div = function (S1, S2)
  S1, S2 = symbolic._toSym(S1, S2)
  local res = symbolic:_newExpr(PARENTS.product, {})
  -- check a^x / a^y
  if S1._parent == PARENTS.power and S2._parent == PARENTS.power 
     and S1._[1] == S2._[1] 
  then
     return symbolic.__pow(S1._[1], S1._[2] - S2._[2])
  end
  -- S1
  if S1._parent == PARENTS.product then
    for _, v in ipairs(S1._) do table.insert(res._, {v[1], v[2]}) end
  else
    table.insert(res._, {1, S1})
  end
  -- S2
  if S2._parent == PARENTS.product then
    for _, v in ipairs(S2._) do table.insert(res._, {-v[1], v[2]}) end
  else
    table.insert(res._, {-1, S2})
  end
  res:p_simp()
  res:p_signature()
  return res
end


--- S1 == S2
--  @param S1 Symbolic object or number.
--  @param S2 Symbolic object or number.
--  @return true when objects are equal.
symbolic.__eq = function (S1, S2)
  return issym(S1) and issym(S2) and S1:p_eq(S2)
end


--- Multiple 'inheritance'.
--  @param t Source object.
--  @param k Required key.
--  @return Found method or nil.
symbolic.__index = function (t, k) return symbolic[k] or t._parent[k] end


--- S1 * S2
--  @param S1 Symbolic object or number.
--  @param S2 Symbolic object or number.
--  @return Product object.
symbolic.__mul = function (S1, S2)
  S1, S2 = symbolic._toSym(S1, S2)
  local res = symbolic:_newExpr(PARENTS.product, {})
  -- check a^x * a^y
  if S1._parent == PARENTS.power and S2._parent == PARENTS.power 
     and S1._[1] == S2._[1] 
  then
     return symbolic.__pow(S1._[1], S1._[2] + S2._[2])
  end
  -- S1
  if S1._parent == PARENTS.product then
    for _, v in ipairs(S1._) do table.insert(res._, {v[1], v[2]}) end
  else
    table.insert(res._, {1, S1})
  end
  -- S2
  if S2._parent == PARENTS.product then
    for _, v in ipairs(S2._) do table.insert(res._, {v[1], v[2]}) end
  else
    table.insert(res._, {1, S2})
  end
  res:p_simp()
  res:p_signature()
  return res
end


--- S1 ^ S2
--  @param S1 Symbolic object or number.
--  @param S2 Symbolic object or number.
--  @return Power object.
symbolic.__pow = function (S1, S2)
  S1, S2 = symbolic._toSym(S1, S2)
  local res = symbolic:_newExpr(PARENTS.power)
  if S1._parent == PARENTS.power then
    -- (a^x)^y
    res._ = {S1._[1], S1._[2] * S2}
    res._[2]:p_simp()
  else
    res._ = {S1, S2}
  end
  res:p_simp()
  res:p_signature()
  return res
end


--- S1 - S2
--  @param S1 Symbolic object or number.
--  @param S2 Symbolic object or number.
--  @return Difference object.
symbolic.__sub = function (S1, S2)
  S1, S2 = symbolic._toSym(S1, S2)
  local res = symbolic:_newExpr(PARENTS.sum, {})
  -- S1
  if S1._parent == PARENTS.sum then
    for _, v in ipairs(S1._) do table.insert(res._, {v[1], v[2]}) end
  else
    table.insert(res._, {1, S1})
  end
  -- S2
  if S2._parent == PARENTS.sum then
    for _, v in ipairs(S2._) do table.insert(res._, {-v[1], v[2]}) end
  else
    table.insert(res._, {-1, S2})
  end
  res:p_simp()
  res:p_signature()
  return res
end


--- String representation.
--  @param S Symbolic object.
--  @return String.
symbolic.__tostring = function (S)
  return S._parent.p_str(S, true)  -- true for full version of fn
end


--- -S
--  @param S Symbolic object.
--  @return Negative value.
symbolic.__unm = function (S)
  local res = nil
  if S._parent == PARENTS.sum then
    res = symbolic:_newExpr(PARENTS.sum, {})
    for i, v in ipairs(S._) do res._[i] = {-v[1], v[2]} end
  else
    res = symbolic._m1 * S
  end
  if S._parent == PARENTS.product then
    res:p_simp()
  end
  res:p_signature()
  return res
end


--- Create constant object.
--  @param self Symbolic table.
--  @param v Constant value.
--  @return Symbolic object.
symbolic._newConst = function (self, v)
  local o = {
    _parent = PARENTS.const,
    _sign = 1,
    _ = v,
  }
  return setmetatable(o, self)
end


--- Prepare structure for expression.
--  @param self Symbolic table.
--  @param parent Parent table.
--  @param v Argument.
--  @return Symbolic object.
symbolic._newExpr = function (self, parent, v)
  local o = {
    _parent = parent,
    _ = v,
  }
  return setmetatable(o, self)
end


--- Create symbolic variable.
--  @param self Symbolic table.
--  @param sName Variable name.
--  @return Symbolic object.
symbolic._newSymbol = function (self, sName)
  local sum = 0
  for i = 1, #sName do sum = (sum*8 + string.byte(sName, i, i)) % 100000 end
  local o = {
    _parent = PARENTS.symbol,
    _sign = sum,
    _ = sName,
  }
  return setmetatable(o, self)
end


-- Often used constants
symbolic._m1 = symbolic:_newConst(-1)
symbolic._0 = symbolic:_newConst(0)
symbolic._1 = symbolic:_newConst(1)
symbolic._2 = symbolic:_newConst(2)


-- predefine some functions
local singleArg = {'x'}
symbolic._fnList = {
  sqrt = {args = singleArg, body = math.sqrt},
  log = {args = singleArg, body = math.sin},
  exp = {args = singleArg, body = math.exp},
  sin = {args = singleArg, body = math.sin},
  cos = {args = singleArg, body = math.cos},
  tan = {args = singleArg, body = math.tan},
  asin = {args = singleArg, body = math.asin},
  acos = {args = singleArg, body = math.acos},
  atan = {args = singleArg, body = math.atan},
  --
  diff = {args = {'y','x'}},
}


-- add objects for main functions
symbolic._fnInit = {}
for k, _ in pairs(symbolic._fnList) do
  symbolic._fnInit[k] = symbolic:_newSymbol(k)
end


-- list of derivatives
-- i-th position of a table corresponds to df/dxi
symbolic._fnDiff = {
  sqrt = {
    function (x) return symbolic._1 / symbolic._fnInit.sqrt(x) / symbolic._2 end},
  log = {function (x) return symbolic._1 / x end},
  exp = {function (x) return symbolic._fnInit.exp(x) end},
  sin = {function (x) return symbolic._fnInit.cos(x) end},
  cos = {function (x) return -symbolic._fnInit.sin(x) end},
  tan = {
    function (x) return symbolic._1 / symbolic._fnInit.cos(x)^symbolic._2 end},
  asin = {function (x)
    return symbolic._1 / symbolic._fnInit.sqrt(symbolic._1 - x^symbolic._2) end},
  acos = {function (x)
    return -symbolic._1 / symbolic._fnInit.sqrt(symbolic._1 - x^symbolic._2) end},
  atan = {function (x) return symbolic._1 / (symbolic._1 + x^symbolic._2) end},
}


--- Check if the object is function.
--  @param S Symbolic object.
--  @return True if it is found in function list.
symbolic._isfn = function (S)
  return S._parent == PARENTS.symbol and symbolic._fnList[S._]
end


return symbolic
