--[[		sonata/lib/symbolic.lua

--- Symbolical calculus.
--
--  Object structure <br>
--  <code> {_=components, _parent=parent, _sign=signature} </code><br>
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2022.

	module 'symbolic'
--]]

--[[TEST

-- use 'symbolic'
Sym = require 'lib.symbolic'

-- create variables
x, y = Sym('x'), Sym('y')
ans = (x == y)            --> false

-- sum
ans = x + 2*y - x + y     --> 3*y

-- product
ans = x * y^2 / x * y     --> y^3

-- power
ans = x^y * x^(2*y)       --> x^(3*y)

-- evaluate
S = (x+y)*(x-y)
ans = S:eval{x=2, y=1}    --> Sym(3)

-- define function
foo = Sym:def('foo', {x, y}, x^y)
ans = foo(y, x)           --> y^x

-- numeric value
ans = foo(Sym(2), Sym(3)) --> Sym(8)


--]]

--	LOCAL

local Ver = require('lib.utils')
local Cross = Ver.cross
local Utils = Ver.utils
Ver = Ver.versions

--- Check object type.
--  @param v Object.
--  @return True if the object is symbolic.
local function issymbolic(v) return type(v)=='table' and v.issymbolic end

--- Condition for element sorting.
--  @param S1 Symbolic object.
--  @param S2 Symbolic object.
--  @return true when S1 < S2.
local compList = function (S1, S2) return S1[2]._sign < S2[2]._sign end

--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Symbolical calculations."
}

--	MODULE

local symbolic = {
-- mark
type = 'symbolic', issymbolic = true,
}

-- Combine 'common' methods
local COMMON = {

  --- Copy content of the simbolic object.
  --  @param S Destination object.
  --  @param S0 Source object.
  copy = function (S, S0)
    S._parent = S0._parent
    S._ = S0._
    S._sign = S0.p_isatom and S0._sign or nil
  end,

  -- Do nothing.
  empty = function (S) end,

    --- Check equality for objects based on lists.
  --  @param S1 First symbolic object.
  --  @param S2 Second symbolic object.
  --  @return true when objects are equal.
  eq = function (S1, S2)
    if not (S1._sign == S2._sign and #S1._ == #S2._) then
      return false
    end
    for i = 1, #S1._ do
      local si1, si2 = S1._[i], S2._[i]
      if not si1:p_eq(si2) then
        return false
      end
    end
    return true
  end,

  --- Check equality for objects based on lists of pairs.
  --  @param S1 First symbolic object.
  --  @param S2 Second symbolic object.
  --  @return true when objects are equal.
  eqPairs = function (S1, S2)
    if not (S1._sign == S2._sign and #S1._ == #S2._) then
      return false
    end
    for i = 1, #S1._ do
      local si1, si2 = S1._[i], S2._[i]
      if not (si1[1] == si2[1] and si1[2]:p_eq(si2[2])) then
        return false
      end
    end
    return true
  end,

  --- Evaluate expression for list.
  --  @param S Symbolic object.
  --  @param tEnv Table with elements for substitution.
  --  @return New object.
  eval = function (S, tEnv)
    local res = symbolic:_newExpr(S._parent, {})
    for i, v in ipairs(S._) do
      res._[i] = v:p_eval(tEnv)
    end
    return res
  end,

  --- Evaluate expression for list of pairs.
  --  @param S Symbolic object.
  --  @param tEnv Table with elements for substitution.
  --  @return New object.
  evalPairs = function (S, tEnv)
    local res = symbolic:_newExpr(S._parent, {})
    for i, v in ipairs(S._) do
      res._[i] = {v[1], v[2]:p_eval(tEnv)}
    end
    return res
  end,

  --- Check if the value is 1.
  --  @param v Some object.
  --  @return v == 1.
  isOne = function (v)
    return issymbolic(v) and v._ == 1 or v == 1
  end,

  --- Check if the value is 0.
  --  @param v Some object.
  --  @return v == 0
  isZero = function (v)
    return issymbolic(v) and v._ == 0 or v == 0
  end,

  --- Find signature of an object based on list.
  --  @param S Symbolic object.
  --  @return true when update signature.
  signature = function (S)
    local found = false
    for i = 1, #S._ do
      found = S._[i]:p_signature() or found
    end
    if S._sign and not found then return false end
    local t = {}
    for i = 1, #S._ do t[i] = S._[i]._sign end
    S._sign = string.format('(%s:%s)', S.p_char, table.concat(t, ';'))
    return true
  end,

  --- Find signature of an object based on list of pairs.
  --  @param S Symbolic object.
  --  @return true when update signature.
  signaturePairs = function (S)
    -- check elements
    local found = false
    for i = 1, #S._ do
      found = S._[i][2]:p_signature() or found
    end
    if S._sign and not found then return false end
    table.sort(S._, compList)
    local t = {}
    for i = 1, #S._ do t[i] = S._[i][2]._sign end
    S._sign = string.format('(%s:%s)', S.p_char, table.concat(t, ';'))
    return true
  end,

  -- Return false always.
  skip = function (S) return false end,

} -- COMMON

-- Basic symbolic types.
local PARENTS = {

  -- constant value
  const = {
    -- S._ = value
    p_char = '',
    p_isatom = true,
    p_signature = COMMON.skip,
    p_eq = function (S1, S2) return S1._ == S2._ end,
    p_str = function (S) return tostring(S._) end,
    p_simp = COMMON.empty,
    p_eval = function (S) return S end,
  },

  -- function object
  func = {
    -- S._ = name
    p_char = 'FN',
    p_isatom = true,
    p_signature = COMMON.skip,
    p_eq = function (S1, S2) return S1._ == S2._ end,
    p_str = function (S)
      local nm = S._
      local lst = symbolic._fnList[nm]
      return string.format(
        '%s(%s): %s', nm, table.concat(lst.args, ','), tostring(lst.body))
    end,
    p_simp = COMMON.empty,
  },

  -- expression (function value)
  funcValue = {
    -- S._ = {fn, arg1, arg2, ...}
    p_char = 'FN',
    p_isatom = false,
    p_signature = COMMON.signature,
    p_eq = COMMON.eq,
    p_str = function (S)
      local t = {}
      for i = 2, #S._ do t[#t+1] = S._[i]:p_str() end
      return string.format('%s(%s)', S._[1]._, table.concat(t, ','))
    end,
    p_simp = function (S, bFull)
      for _, v in ipairs(S._) do v:p_simp(bFull) end
    end,
    -- p_eval
  },

  -- expression (power)
  power = {
    -- S._ = {base, power}
    p_char = '^',
    p_isatom = false,
    p_signature = COMMON.signature,
    p_eq = COMMON.eq,
    p_eval = COMMON.eval,
    -- p_simp below
  },

  -- expression (product)
  product = {
    -- S._ = {{pow1, S1}, {pow2, S2}, ...}
    p_sym = '*',
    p_isatom = false,
    p_signature = COMMON.signaturePairs,
    p_eq = COMMON.eqPairs,
    p_eval = COMMON.evalPairs,
    -- p_simp below
  },

  -- expresion (sum)
  sum = {
    -- S._ = {{k1, S1}, {k2, S2}, ...}
    p_char = '+',
    p_isatom = false,
    p_signature = COMMON.signaturePairs,
    p_eq = COMMON.eqPairs,
    p_eval = COMMON.evalPairs,
    -- p_simp below
  },

  -- variable
  symbol = {
    -- S._ = name
    p_char = 'S',
    p_isatom = true,
    p_signature = COMMON.skip,
    p_eq = function (S1, S2) return S1._ == S2._ end,
    p_str = function (S) return S._ end,
    p_simp = COMMON.empty,
    p_eval = function (S, tEnv)
      local v = tEnv[S._]
      return v and (issymbolic(v) and v or symbolic:_newConst(v)) or S
    end,
  },

  --[[
  equation = {
    -- S._ = {lft, rht}
    p_char = 'EQ',
    p_signature = COMMON.signature,
    p_eq = COMMON.eq,
    p_str = function (S)
      return string.format('%s = %s', S._[1]:p_str(), S._[2]:p_str())
    end,
    p_simp = COMMON.simp,
  }
  ]]
} -- PARENTS

-- Parser elements
PARSER = {} 

--- Parse coma separated elements.
--  @param lst List with tokens.
--  @param n Element index.
--  @return Table and next index.
PARSER.args = function (lst, n)
  local t = {}
  t[1], n = PARSER.sum(lst, n)
  while lst[n] == ',' do
    t[#t+1], n = PARSER.sum(lst, n+1) 
  end
  return t, n
end

--- Parse sum or difference.
--  @param lst List with tokens.
--  @param n Element index.
--  @return Table and next index.
PARSER.sum = function (lst, n)
  local res, n = PARSER.prod(lst, n)
  while true do
    if lst[n] == '+' then 
      local tmp, m = PARSER.prod(lst, n+1)
      res, n = res + tmp, m
    elseif lst[n] == '-' then
      local tmp, m = PARSER.prod(lst, n+1)
      res, n = res - tmp, m
    else break end
  end
  return res, n
end

--- Parse product or ratio.
--  @param lst List with tokens.
--  @param n Element index.
--  @return Table and next index.
PARSER.prod = function (lst, n)
  local res, n = PARSER.pow(lst, n)
  while true do 
    if lst[n] == '*' then 
      local tmp, m = PARSER.pow(lst, n+1)
      res, n = res * tmp, m
    elseif lst[n] == '/' then
      local tmp, m = PARSER.pow(lst, n+1)
      res, n = res / tmp, m
    else break end
  end
  return res, n
end

--- Parse power.
--  @param lst List with tokens.
--  @param n Element index.
--  @return Table and next index.
PARSER.pow = function (lst, n)
  local res, n = PARSER.prim(lst, n)
  if lst[n] == '^' then  -- TODO add **
    local tmp, m = PARSER.prim(lst, n+1)
    res, n = res ^ tmp, m
  end
  return res, n
end

--- Parse number, symbol or function.
--  @param lst List with tokens.
--  @param n Element index.
--  @return Table and next index.
PARSER.prim = function (lst, n)
  local v = lst[n]
  if type(v) == 'number' then
    return symbolic:_newConst(v), n+1
  elseif v == '(' then
    local res, n = PARSER.sum(lst, n+1)
    if lst[n] ~= ')' then error("expected ')'") end
    return res, n+1
  elseif v == '-' then
    local res, n = PARSER.prod(lst, n+1)
    return -res, n
  elseif string.find(v, '^[%a_]') ~= nil then
    if lst[n+1] == '(' then
      local t = nil
      if lst[n+2] == ')' then
        t, n = {}, n+2
      else
        t, n = PARSER.args(lst, n+2)
      end
      if lst[n] ~= ')' then error("expected ')'") end
      table.insert(t, 1, symbolic:_newFunc(v))
      return symbolic:_newExpr(PARENTS.funcValue, t), n+1
    else
      return symbolic:_newSymbol(v), n+1
    end
  else
    error("unexpected symbol "..v)
  end
  return nil, n
end



--- List of elements for printing in brackets.
COMMON.closed = {
[PARENTS.sum] = true,
[PARENTS.product] = true,
--[PARENTS.power] = true,
}

--- Simplify list of pairs (in place).
--  @param S Symbolic object.
--  @param tParent Parent reference.
COMMON.simpPair = function (S, tParent)
  -- be sure that signature is found
  S:p_signature()
  -- get coefficients
  for i, v in ipairs(S._) do
    if v[2]._parent == tParent then
      local k, v2 = v[2]:p_getConst()
      if k ~= 1 then
        v[1] = v[1] * k
        v[2]:p_signature()
      end
    end
  end
  -- combine elements
  for i, si in ipairs(S._) do
    if si[1] ~= 0 then
      local iconst = (si[2]._parent == PARENTS.const)
      for j = i+1, #S._ do
        local sj = S._[j]
        if sj[1] ~= 0 then
          if iconst and sj[2]._parent == PARENTS.const then
            -- combine constants
            if tParent == PARENTS.product then
              si[2]._ = si[1] * si[2]._ + sj[1] * sj[2]._
            else -- PARENTS.power
              si[2]._ = si[2]._ ^ si[1] * sj[2]._ ^ sj[1]
            end
            si[1], sj[1] = 1, 0
          elseif si[2]:p_eq(sj[2]) then
            -- combine expressions
            si[1], sj[1] = si[1] + sj[1], 0
          end
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
    return symbolic:_newConst( body(table.unpack(val)) )
  else
    local res = symbolic:_newExpr(S._parent, t)
    return res
  end
end

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
    COMMON.copy(S, symbolic:_newConst(1))
  elseif COMMON.isOne(S._[1]) or COMMON.isZero(S._[1]) then  -- 1^v or 0^v
    COMMON.copy(S, S._[1])
  elseif S._[1]._parent == S._[2]._parent and
         S._[1]._parent == PARENTS.const then
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
    for _, v in ipairs(S._) do v[2]:p_simp(bFull) end
  end
  COMMON.simpPair(S, PARENTS.power)
  -- empty list
  if #S._ == 0 then
    COMMON.copy(S, symbolic:_newConst(1))
    return
  elseif #S._ > 1 then
    table.sort(S._, compList)
  end
  -- check constant
  if COMMON.isZero(S._[1][2]) then
    COMMON.copy(S, symbolic:_newConst(0))
    return
  elseif #S._ > 1 and COMMON.isOne(S._[1][2]) then
    table.remove(S._, 1)
    S._sign = nil
  end
  -- change type
  if #S._ == 1 then
    local v = S._[1]
    if v[1] ~= 1 then
      COMMON.copy(S, v[2] ^ symbolic:_newConst(v[1]))
    else
      COMMON.copy(S, v[2])
    end
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

--- Simplify sum (in place).
--  @param S Symbolic object.
--  @param bFull Flag for recursive simplification.
PARENTS.sum.p_simp = function (S, bFull)
  if bFull then
    for _, v in ipairs(S._) do v[2]:p_simp(bFull) end
  end
  -- update structure
  COMMON.simpPair(S, PARENTS.product)
  -- empty list
  if #S._ == 0 then
    COMMON.copy(S, symbolic:_newConst(0))
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

--- List of 'sybolic' functions.
symbolic._fnList = {
  sin = {args = {'x'}, body = math.sin},
  cos = {args = {'x'}, body = math.cos},
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
    for i, v in ipairs(S1._) do table.insert(res._, {v[1], v[2]}) end
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
  if S._parent == PARENTS.func then
    local t = {...}
    local fn = symbolic._fnList[S._]
    if #t ~= #fn.args then
      error(string.format("Expected %d arguments for %s", #fn.args, S._))
    end
    if type(fn.body) == 'function' or fn.body == nil then
      table.insert(t, 1, S)
      return symbolic:_newExpr(PARENTS.funcValue, t)
    elseif issymbolic(fn.body) then
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
  return issymbolic(S1) and issymbolic(S2) and S1:p_eq(S2)
end

--- Multiple 'inheritance'.
--  @param t Source object.
--  @param k Required key.
--  @return Found method or nil.
symbolic.__index = function (t, k)
  return symbolic[k] or t._parent[k]
end

--- S1 * S2
--  @param S1 Symbolic object or number.
--  @param S2 Symbolic object or number.
--  @return Product object.
symbolic.__mul = function (S1, S2)
  S1, S2 = symbolic._toSym(S1, S2)
  local res = symbolic:_newExpr(PARENTS.product, {})
  if S1._parent == PARENTS.power and S2._parent == PARENTS.power and
     S1._[1] == S2._[1] then
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
  return S._parent.p_str(S)
end

--- -S
--  @param S Symbolic object.
--  @return Negative value.
symbolic.__unm = function (S)
  local res
  if S._parent == PARENTS.sum then
    res = symbolic:_newExpr(PARENTS.sum, {})
    for i, v in ipairs(S._) do res._[i] = {-v[1], v[2]} end
  else
    res = symbolic:_newConst(-1) * S
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
    _sign = '.',
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

--- Create function object.
--  @param self Symbolic table.
--  @param sName Function name.
--  @return Symbolic object.
symbolic._newFunc = function (self, sName)
  local o = {
    _parent = PARENTS.func,
    _ = sName,
    _sign = sName..'()'
  }
  return setmetatable(o, self)
end

--- Create symbolic variable.
--  @param self Symbolic table.
--  @param sName Variable name.
--  @return Symbolic object.
symbolic._newSymbol = function (self, sName)
  local o = {
    _parent = PARENTS.symbol,
    _sign = sName,
    _ = sName,
  }
  return setmetatable(o, self)
end

--- Convert arguments to symbolic if need.
--  @param v2 First value.
--  @param v2 Second value.
--  @return Symbolic objects.
symbolic._toSym = function (v1, v2)
  v1 = issymbolic(v1) and v1 or symbolic:_newConst(v1)
  v2 = issymbolic(v2) and v2 or symbolic:_newConst(v2)
  return v1, v2
end

--- Define (redefine) function.
--  @param self Do nothing.
--  @param sName Function name.
--  @param tArgs List of arguments (symbolic objects).
--  @param S Function body, symbolical expression or Lua function.
--  @return Function object.
symbolic.def = function (self, sName, tArgs, S)
  assert(type(sName) == 'string' and issymbolic(S), "Wrong arguments")
  local t = {}
  for i, v in ipairs(tArgs) do
    if issymbolic(v) then
      if v._parent == PARENTS.symbol then
        t[i] = v._
      else
        error("Wrong arguments")
      end
    elseif type(v) == 'string' then
      t[i] = v
    else
      error("Wrong arguments")
    end
  end
  symbolic._fnList[sName] = { args = t, body = S }
  return symbolic:_newFunc(sName)
end
about[symbolic.def] = {":def(sName,tArgs,S)", 
  "Define symbolical function. S is either symbolical expression or a Lua function."}

--- Find value for the given substitutions.
--  @param S Symbolic object.
--  @param tEnv Table of substitutions (key - value).
--  @return New object.
symbolic.eval = function (S, tEnv)
  local res = S:p_eval(tEnv or {})
  res:p_simp(true)
  res:p_signature()
  return res
end
about[symbolic.eval] = {"eval([tEnv])", "Evaluate symbolical expression with the given environment."}

--- Find function using its name.
--  @param self Do nothing.
--  @param sName Function name.
--  @return Function object or nil.
symbolic.func = function (self, sName)
  return symbolic._fnList[sName] and symbolic:_newFunc(sName) or nil
end
about[symbolic.func] = {":func(sName)", "Return symbolical function if it was defined."}

--- Get name of variable.
--  @param S Symbolic object.
--  @return Variable name.
symbolic.name = function (S)
  return S._parent == PARENTS.symbol and S._ or nil
end

--- Get symbolic expression from string.
--  @param self Do nothing.
--  @param str Expression string.
--  @return One or several symbolic elements.
symbolic.parse = function(self, str)
  local tokens = Utils.lex(str)
  assert(#tokens > 0)
  local res = PARSER.args(tokens, 1)
  if issymbolic(res) then
    return res
  else
    return table.unpack(res)
  end
end
about[symbolic.parse] = {":parse(str)", "Get simbolical expression from string."}

--- Get value of constant.
--  @param S Symbolic object.
--  @return Constant value.
symbolic.value = function (S)
  return S._parent == PARENTS.const and S._ or nil
end

-- simplify constructor call
setmetatable(symbolic, {
__call = function (self,v)
  if type(v) == 'string' then
    return symbolic:_newSymbol(
      assert(v:match('^[_%a]+[_%w]*$'), 'Wrong name'))
  elseif type(v) == 'number' or type(v) == 'table' and v.__mul then   -- TODO other methods?
    return symbolic:_newConst(v)
  end
  error("Wrong argument "..tostring(v))
end})
about[symbolic] = {" (v)", "Create new symbolic variable.", help.NEW}

-- Comment to remove descriptions
symbolic.about = about

return symbolic

--=============================================================
--TODO 'eval' for expression substitution
