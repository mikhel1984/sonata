--[[		sonata/lib/symbolic.lua

--- Symbolical calculus.
--
--  Object structure <br>
--  <code> {_=components, _parent_=parent, _sign_=signature} </code><br>
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

--- Check object type.
--  @param v Object.
--  @return True if the object is symbolic.
local function issymbolic(v) return type(v)=='table' and v.issymbolic end

--- Condition for element sorting.
--  @param S1 Symbolic object.
--  @param S2 Symbolic object.
--  @return true when S1 < S2.
local _comp_lst_ = function (S1, S2) return S1[2]._sign_ < S2[2]._sign_ end

--	INFO

local help = SonataHelp or {new=function () return {} end}
-- description
local about = help:new("Symbolical calculations.")

--	MODULE

local symbolic = {
-- mark
type = 'symbolic', issymbolic = true,
}

-- Combine 'common' methods
local _COMMON_ = {

  --- Copy content of the simbolic object.
  --  @param S Destination object.
  --  @param S0 Source object.
  copy = function (S, S0)
    S._parent_ = S0._parent_
    S._ = S0._
    S._sign_ = S0.p_isatom and S0._sign_ or nil
  end,

  -- Do nothing.
  empty = function (S) end,
  
    --- Check equality for objects based on lists.
  --  @param S1 First symbolic object.
  --  @param S2 Second symbolic object.
  --  @return true when objects are equal.
  eq = function (S1, S2)
    if not (S1._sign_ == S2._sign_ and #S1._ == #S2._) then
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
  eq_pairs = function (S1, S2)
    if not (S1._sign_ == S2._sign_ and #S1._ == #S2._) then
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
    local res = symbolic:_new_expr_(S._parent_, {})
    for i, v in ipairs(S._) do
      res._[i] = v:p_eval(tEnv)
    end
    return res
  end,

  --- Evaluate expression for list of pairs.
  --  @param S Symbolic object.
  --  @param tEnv Table with elements for substitution.
  --  @return New object.
  eval_pairs = function (S, tEnv)
    local res = symbolic:_new_expr_(S._parent_, {})
    for i, v in ipairs(S._) do
      res._[i] = {v[1], v[2]:p_eval(tEnv)}
    end
    return res
  end,
  
  --- Check if the value is 1.
  --  @param v Some object.
  --  @return v == 1.
  isone = function (v)
    return issymbolic(v) and v._ == 1 or v == 1
  end,
  
  --- Check if the value is 0.
  --  @param v Some object.
  --  @return v == 0
  iszero = function (v)
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
    if S._sign_ and not found then return false end
    local t = {}
    for i = 1, #S._ do t[i] = S._[i]._sign_ end
    S._sign_ = string.format('(%s:%s)', S.p_char, table.concat(t, ';'))
    return true
  end,
  
  --- Find signature of an object based on list of pairs.
  --  @param S Symbolic object.
  --  @return true when update signature.
  signature_pairs = function (S)
    -- check elements
    local found = false
    for i = 1, #S._ do
      found = S._[i][2]:p_signature() or found
    end
    if S._sign_ and not found then return false end
    table.sort(S._, _comp_lst_)
    local t = {}
    for i = 1, #S._ do t[i] = S._[i][2]._sign_ end
    S._sign_ = string.format('(%s:%s)', S.p_char, table.concat(t, ';'))
    return true
  end, 
  
  -- Return false always.
  skip = function (S) return false end,

} -- _COMMON_

-- Basic symbolic types.
local _PARENTS_ = {

  -- constant value
  const = {
    -- S._ = value
    p_char = '',
    p_isatom = true,
    p_signature = _COMMON_.skip,
    p_eq = function (S1, S2) return S1._ == S2._ end,
    p_str = function (S) return tostring(S._) end,
    p_simp = _COMMON_.empty,
    p_eval = function (S) return S end,
  },
  
  -- function object
  func = {
    -- S._ = name
    p_char = 'FN',  
    p_isatom = true,
    p_signature = _COMMON_.skip,
    p_eq = function (S1, S2) return S1._ == S2._ end,
    p_str = function (S)
      local nm = S._
      local lst = symbolic._fn_list_[nm]
      return string.format('%s(%s): %s', nm, table.concat(lst.args, ','), tostring(lst.body))
    end,
    p_simp = _COMMON_.empty,
  },
  
  -- expression (function value)
  func_value = {
    -- S._ = {fn, arg1, arg2, ...}
    p_char = 'FN',
    p_isatom = false,
    p_signature = _COMMON_.signature,
    p_eq = _COMMON_.eq,
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
    p_signature = _COMMON_.signature,
    p_eq = _COMMON_.eq,
    p_eval = _COMMON_.eval,
    -- p_simp below
  }, 
 
  -- expression (product)
  product = {
    -- S._ = {{pow1, S1}, {pow2, S2}, ...}
    p_sym = '*',  
    p_isatom = false,
    p_signature = _COMMON_.signature_pairs,
    p_eq = _COMMON_.eq_pairs, 
    p_eval = _COMMON_.eval_pairs,
    -- p_simp below
  },
  
  -- expresion (sum)
  sum = {
    -- S._ = {{k1, S1}, {k2, S2}, ...}
    p_char = '+',
    p_isatom = false,
    p_signature = _COMMON_.signature_pairs,
    p_eq = _COMMON_.eq_pairs,
    p_eval = _COMMON_.eval_pairs,
    -- p_simp below
  },

  -- variable
  symbol = {
    -- S._ = name
    p_char = 'S',
    p_isatom = true,
    p_signature = _COMMON_.skip,
    p_eq = function (S1, S2) return S1._ == S2._ end,
    p_str = function (S) return S._ end,
    p_simp = _COMMON_.empty,
    p_eval = function (S, tEnv)
      local v = tEnv[S._]
      return v and (issymbolic(v) and v or symbolic:_new_const_(v)) or S
    end,
  },

  --[[
  equation = {
    -- S._ = {lft, rht}
    p_char = 'EQ',
    p_signature = _COMMON_.signature,
    p_eq = _COMMON_.eq,
    p_str = function (S)
      return string.format('%s = %s', S._[1]:p_str(), S._[2]:p_str())
    end,
    p_simp = _COMMON_.simp,
  }
  ]]
} -- _PARENTS_

--- List of elements for printing in brackets.
_COMMON_.closed = {
[_PARENTS_.sum] = true,
[_PARENTS_.product] = true,
--[_PARENTS_.power] = true,
}

--- Simplify list of pairs (in place).
--  @param S Symbolic object.
--  @param tParent Parent reference.
_COMMON_.pair_simp = function (S, tParent)
  -- be sure that signature is found
  S:p_signature()
  -- get coefficients
  for i, v in ipairs(S._) do
    if v[2]._parent_ == tParent then
      local k, v2 = v[2]:p_get_const()
      if k ~= 1 then 
        v[1] = v[1] * k
        v[2]:p_signature()
      end
    end
  end
  -- combine elements
  for i, si in ipairs(S._) do
    if si[1] ~= 0 then
      local iconst = (si[2]._parent_ == _PARENTS_.const)
      for j = i+1, #S._ do
        local sj = S._[j]
        if sj[1] ~= 0 then
          if iconst and sj[2]._parent_ == _PARENTS_.const then
            -- combine constants
            if tParent == _PARENTS_.product then 
              si[2]._ = si[1] * si[2]._ + sj[1] * sj[2]._
            else -- _PARENTS_.power 
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
      S._sign_ = nil
    end
  end
end

--- Evaluate function value.
--  @param S Symbolic object.
--  @param tEnv Elements for substitution.
--  @return New object.
_PARENTS_.func_value.p_eval = function (S, tEnv)
  local t, val = {S._[1]}, {}
  for i = 2, #S._ do
    local v = S._[i]:p_eval(tEnv)
    t[i] = v
    if v._parent_ == _PARENTS_.const then
      val[#val+1] = v._
    end
  end
  local body = symbolic._fn_list_[S._[1]._].body
  if #val + 1 == #t and body then
    -- evaluate
    return symbolic:_new_const_( body(table.unpack(val)) )
  else 
    local res = symbolic:_new_expr_(S._parent_, t)
    return res
  end
end

--- Split power to numeric and symbolic parts.
--  @param S Symbolic object.
--  @return Constant value.
_PARENTS_.power.p_get_const = function (S)
  local v = S._[2]
  if v._parent_ == _PARENTS_.const then
    v = v._ 
    _COMMON_.copy(S, S._[1])
    return v
  end
  return 1
end

--- Simplify power (in place).
--  @param S Symbolic object.
--  @param bFull Flag for recursive simplification.
_PARENTS_.power.p_simp = function (S, bFull)
  if bFull then
    S._[1]:p_simp(bFull)
    S._[2]:p_simp(bFull)
  end
  if _COMMON_.isone(S._[2]) then       -- v^1
    _COMMON_.copy(S, S._[1])
  elseif _COMMON_.iszero(S._[2]) then  -- v^0
    _COMMON_.copy(S, symbolic:_new_const_(1))
  elseif _COMMON_.isone(S._[1]) or _COMMON_.iszero(S._[1]) then  -- 1^v or 0^v
    _COMMON_.copy(S, S._[1])
  elseif S._[1]._parent_ == S._[2]._parent_ and S._[1]._parent_ == _PARENTS_.const then
    _COMMON_.copy(S, symbolic:_new_const_(S._[1]._ ^ S._[2]._))
  end
end

--- Power object to string translation.
--  @param S Symbolic object.
--  @return String representation.
_PARENTS_.power.p_str = function (S) 
  local base = S._[1]:p_str()
  if _COMMON_.closed[S._[1]._parent_] then
    base = string.format('(%s)', base)
  end
  --if _COMMON_.isone(S._[2]) then return base end
  local pow = S._[2]:p_str()
  if _COMMON_.closed[S._[2]._parent_] then
    pow = string.format('(%s)', pow)
  end
  return string.format('%s^%s', base, pow)
end

--- Split product to numeric and symbolic parts.
--  @param S Symbolic object.
--  @return Constant value.
_PARENTS_.product.p_get_const = function (S)
  local v = S._[1]
  if v[2]._parent_ == _PARENTS_.const then 
    v = v[1] * v[2]._  -- coefficient * const
    if #S._ == 2 and _COMMON_.isone(S._[2][1]) then    -- val^1
      _COMMON_.copy(S, S._[2][2])
    else
      table.remove(S._, 1)
      S._sign_ = nil
    end
    return v
  end
  return 1
end

--- Simplify product (in place).
--  @param S Symbolic object.
--  @param bFull Flag for recursive simplification.
_PARENTS_.product.p_simp = function (S, bFull)
  if bFull then
    for _, v in ipairs(S._) do v[2]:p_simp(bFull) end
  end
  _COMMON_.pair_simp(S, _PARENTS_.power)
  -- empty list
  if #S._ == 0 then
    _COMMON_.copy(S, symbolic:_new_const_(1))
    return
  elseif #S._ > 1 then
    table.sort(S._, _comp_lst_)
  end
  -- check constant 
  if _COMMON_.iszero(S._[1][2]) then
    _COMMON_.copy(S, symbolic:_new_const_(0))
    return
  elseif #S._ > 1 and _COMMON_.isone(S._[1][2]) then 
    table.remove(S._, 1)
    S._sign_ = nil
  end
  -- change type
  if #S._ == 1 then
    local v = S._[1]
    if v[1] ~= 1 then 
      _COMMON_.copy(S, v[2] ^ symbolic:_new_const_(v[1]))
    else 
      _COMMON_.copy(S, v[2])
    end
  end
end

--- Sum object to string translation.
--  @param S Symbolic object.
--  @return String representation.
_PARENTS_.product.p_str = function (S)
  local num, denom = {}, {}
  for _, v in ipairs(S._) do
    local v1, v2 = v[1], v[2]:p_str()
    if #S._ > 1 and _COMMON_.closed[v[2]._parent_] then 
      v2 = string.format('(%s)', v2)
    end
    if v1 > 0 then
      num[#num+1] = (v1 == 1) and v2 or string.format('%s^%s', v2, tostring(v1))
    else  -- v[1] < 0
      denom[#denom+1] = (v1 == -1) and v2 or string.format('%s^%s', v2, tostring(-v1))
    end
  end
  if #denom == 0 then
    return table.concat(num, '*')
  else
    num = #num > 0 and table.concat(num, '*') or '1'  -- reuse
    return string.format(#denom > 1 and "%s/(%s)" or "%s/%s", num, table.concat(denom, '*'))
  end
end

--- Simplify sum (in place).
--  @param S Symbolic object.
--  @param bFull Flag for recursive simplification.
_PARENTS_.sum.p_simp = function (S, bFull)
  if bFull then
    for _, v in ipairs(S._) do v[2]:p_simp(bFull) end
  end
  -- update structure
  _COMMON_.pair_simp(S, _PARENTS_.product)
  -- empty list
  if #S._ == 0 then
    _COMMON_.copy(S, symbolic:_new_const_(0))
    return
  end
  -- sort and remove zero constant
  if #S._ > 1 then 
    table.sort(S._, _comp_lst_)
    if _COMMON_.iszero(S._[1][2]) then
      table.remove(S._, 1) 
      S._sign_ = nil
    end
  end
  -- change type
  if #S._ == 1 then 
    local v = S._[1]
    if v[1] ~= 1 then 
      _COMMON_.copy(S, symbolic:_new_const_(v[1]) * v[2])
    else 
      _COMMON_.copy(S, v[2])
    end
  end
end

--- Product object to string translation.
--  @param S Symbolic object.
--  @return String representation.
_PARENTS_.sum.p_str = function (S)
  local plus, minus = {}, {}
  for _, v in ipairs(S._) do
    local v1, v2 = v[1], v[2]:p_str()
    if v1 > 0 then 
      plus[#plus+1] = (v1 == 1) and v2 or string.format('%s*%s', tostring(v1), v2)
    else
      minus[#minus+1] = (v1 == -1) and v2 or string.format('%s*%s', tostring(-v1), v2)
    end
  end
  if #minus == 0 then
    return table.concat(plus, '+')
  else
    return string.format('%s-%s', table.concat(plus, '+'), table.concat(minus, '-'))
  end
end

--- List of 'sybolic' functions.
symbolic._fn_list_ = {
  sin = {args = {'x'}, body = math.sin},
  cos = {args = {'x'}, body = math.cos},
}

--- S1 + S2
--  @param S1 Symbolic object or number.
--  @param S2 Symbolic object or number.
--  @return Sum object.
symbolic.__add = function (S1, S2)
  S1, S2 = symbolic._tosym_(S1, S2)
  local res = symbolic:_new_expr_(_PARENTS_.sum, {})
  -- S1
  if S1._parent_ == _PARENTS_.sum then
    for i, v in ipairs(S1._) do table.insert(res._, {v[1], v[2]}) end
  else
    table.insert(res._, {1, S1})
  end
  -- S2
  if S2._parent_ == _PARENTS_.sum then 
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
  if S._parent_ == _PARENTS_.func then
    local t = {...}
    local fn = symbolic._fn_list_[S._]
    if #t ~= #fn.args then 
      error(string.format("Expected %d arguments for %s", #fn.args, S._))
    end
    if type(fn.body) == 'function' or fn.body == nil then
      table.insert(t, 1, S)
      return symbolic:_new_expr_(_PARENTS_.func_value, t)
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
  S1, S2 = symbolic._tosym_(S1, S2)
  local res = symbolic:_new_expr_(_PARENTS_.product, {})
  -- S1
  if S1._parent_ == _PARENTS_.product then 
    for _, v in ipairs(S1._) do table.insert(res._, {v[1], v[2]}) end
  else
    table.insert(res._, {1, S1})
  end
  -- S2
  if S2._parent_ == _PARENTS_.product then 
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
  return symbolic[k] or t._parent_[k]
end

--- S1 * S2
--  @param S1 Symbolic object or number.
--  @param S2 Symbolic object or number.
--  @return Product object.
symbolic.__mul = function (S1, S2)
  S1, S2 = symbolic._tosym_(S1, S2)
  local res = symbolic:_new_expr_(_PARENTS_.product, {})
  if S1._parent_ == _PARENTS_.power and S2._parent_ == _PARENTS_.power and
     S1._[1] == S2._[1] then
     return symbolic.__pow(S1._[1], S1._[2] + S2._[2])
  end
  -- S1
  if S1._parent_ == _PARENTS_.product then 
    for _, v in ipairs(S1._) do table.insert(res._, {v[1], v[2]}) end
  else
    table.insert(res._, {1, S1})
  end
  -- S2
  if S2._parent_ == _PARENTS_.product then 
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
  S1, S2 = symbolic._tosym_(S1, S2)
  local res = symbolic:_new_expr_(_PARENTS_.power)
  if S1._parent_ == _PARENTS_.power then
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
  S1, S2 = symbolic._tosym_(S1, S2)
  local res = symbolic:_new_expr_(_PARENTS_.sum, {})
  -- S1
  if S1._parent_ == _PARENTS_.sum then
    for _, v in ipairs(S1._) do table.insert(res._, {v[1], v[2]}) end
  else
    table.insert(res._, {1, S1})
  end
  -- S2
  if S2._parent_ == _PARENTS_.sum then 
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
  return S._parent_.p_str(S)
end

--- -S
--  @param S Symbolic object.
--  @return Negative value.
symbolic.__unm = function (S)
  local res
  if S._parent_ == _PARENTS_.sum then
    res = symbolic:_new_expr_(_PARENTS_.sum, {})
    for i, v in ipairs(S._) do res._[i] = {-v[1], v[2]} end
  else 
    res = symbolic:_new_const_(-1) * S
  end
  if S._parent_ == _PARENTS_.product then
    res:p_simp()
  end
  res:p_signature()
  return res
end

--- Create constant object.
--  @param self Symbolic table.
--  @param v Constant value.
--  @return Symbolic object.
symbolic._new_const_ = function (self, v)
  local o = {
    _parent_ = _PARENTS_.const,
    _sign_ = '.',
    _ = v,
  }
  return setmetatable(o, self)
end

--- Prepare structure for expression.
--  @param self Symbolic table.
--  @param parent Parent table.
--  @param v Argument.
--  @return Symbolic object.
symbolic._new_expr_ = function (self, parent, v)
  local o = {
    _parent_ = parent,
    _ = v,
  }
  return setmetatable(o, self)
end

--- Create function object.
--  @param self Symbolic table.
--  @param sName Function name.
--  @return Symbolic object.
symbolic._new_func_ = function (self, sName)
  local o = {
    _parent_ = _PARENTS_.func,
    _ = sName,
    _sign_ = sName..'()'
  }
  return setmetatable(o, self)
end

--- Create symbolic variable.
--  @param self Symbolic table.
--  @param sName Variable name.
--  @return Symbolic object.
symbolic._new_symbol_ = function (self, sName)
  local o = {
    _parent_ = _PARENTS_.symbol,
    _sign_ = sName,
    _ = sName,
  }
  return setmetatable(o, self)
end

--- Convert arguments to symbolic if need.
--  @param v2 First value.
--  @param v2 Second value.
--  @return Symbolic objects.
symbolic._tosym_ = function (v1, v2)
  v1 = issymbolic(v1) and v1 or symbolic:_new_const_(v1)
  v2 = issymbolic(v2) and v2 or symbolic:_new_const_(v2)
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
      if v._parent_ == _PARENTS_.symbol then
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
  symbolic._fn_list_[sName] = { args = t, body = S }
  return symbolic:_new_func_(sName)
end

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

--- Find function using its name.
--  @param self Do nothing.
--  @param sName Function name.
--  @return Function object or nil.
symbolic.func = function (self, sName)
  return symbolic._fn_list_[sName] and symbolic:_new_func_(sName) or nil
end

--- Get name of variable.
--  @param S Symbolic object.
--  @return Variable name.
symbolic.name = function (S)
  return S._parent_ == _PARENTS_.symbol and S._ or nil
end

--- Get value of constant.
--  @param S Symbolic object.
--  @return Constant value.
symbolic.value = function (S)
  return S._parent_ == _PARENTS_.const and S._ or nil
end

-- simplify constructor call
setmetatable(symbolic, {
__call = function (self,v) 
  if type(v) == 'string' then
    return symbolic:_new_symbol_(v)  -- TODO check name correctnes
  elseif type(v) == 'number' then    -- TODO apply for other sonata types
    return symbolic:_new_const_(v)
  end
  error("Wrong argument "..tostring(v))
end})
symbolic.Sym = 'Sym'
about[symbolic.Sym] = {"Sym(v)", "Create new symbolic variable.", help.NEW}

-- Comment to remove descriptions
symbolic.about = about

return symbolic

--=============================================================
--TODO 'eval' for expression substitution
