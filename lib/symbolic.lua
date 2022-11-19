--[[		sonata/lib/symbolic.lua

--- Symbolical calculations.
--  @author Your Name

	module 'symbolic'
--]]

-- Define here your tests, save results to 'ans', use --> for the strict equality 
-- and --n> for the n-digit precision in the case of floating numbers.
--[[TEST

-- use 'symbolic'
Sym = require 'lib.symbolic'

-- example
a = Sym()
ans = a.type   -->  'symbolic'

ans = math.pi  --2> 355/113

--]]

--	LOCAL

--- Check object type.
--  @param v Object.
--  @return True if the object is symbolic.
local function issymbolic(v) return type(v)=='table' and v.issymbolic end

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

local _common_ = {

  signature = function (S)
    local found = false
    for i = 1, #S._ do
      found = S._[i]:p_signature() or found
    end
    if S._sign_ and not found then return false end
    local t = {}
    for i = 1, #S do t[i] = S._[i]._sign_ end
    S._sign_ = string.format('(%s:%s)', S.p_char, table.concat(t, ';'))
    return true
  end,

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
    return S._sign_
  end, 
  
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
  
  empty = function (S) end,
  
  simp = function (S, bFull)
    for _, v in ipairs(S._) do v:p_simp(bFull) end
  end,
  
  iszero = function (v)
    return issymbolic(v) and v._ == 0 or v == 0
  end, 
  
  isone = function (v)
    return issymbolic(v) and v._ == 1 or v == 1
  end,

  isconst = function (S)
    return S._parent_ == _parents_.const
  end,
  
  copy = function (S, S0)
    S._parent_ = S0._parent_
    S._ = S0._
    S._name_ = S0._name_
    S._sign_ = nil
  end,



}


local _parents_ = {
const = {
  -- S._ = value
  p_char = '',
  p_signature = function (S)
    if S._sign_ then return false end
    S._sign_ = '.'
    return true
  end,
  p_eq = function (S1, S2) return S1._ == S2._ end,
  p_str = function (S) return tostring(S._) end,
  p_simp = _common_.empty,
},

symbol = {
  -- S._ = name
  p_char = 'S',
  p_signature = function (S)
    if S._sign_ then return false end
    S._sign_ = S._
    return true
  end,
  p_eq = function (S1, S2) return S1._ == S2._ end,
  p_str = function (S) return S._ end,
  p_simp = _common_.empty,
},

sum = {
  -- S._ = {{k1, S1}, {k2, S2}, ...}
  p_char = '+',
  p_signature = _common_.signature_pairs,
  p_eq = _common_.eq_pairs,
  p_str = function (S)
    local t = {}
    for i, v in ipairs(S._) do
      t[i] = string.format('%s*%s', tostring(v[1]), v[2]:p_str())
    end
    return string.format('(%s)', table.concat(t, ' + '))
  end,
  -- p_simp below
},

product = {
  -- S._ = {{pow1, S1}, {pow2, S2}, ...}
  p_sym = '*',  
  p_signature = _common_.signature_pairs,
  p_eq = _common_.eq_pairs, 
  p_str = function (S)
    local t = {}
    for i, v in ipairs(S._) do
      t[i] = string.format('%s^%s', v[2]:p_str(), tostring(v[1]))
    end
    return string.format('(%s)', table.concat(t, ' * '))
  end,
  -- p_simp below
},

power = {
  -- S._ = {base, power}
  p_char = '^',
  p_signature = _common_.signature,
  p_eq = _common_.eq,
  p_str = function (S) 
    return string.format('%s^%s', S._[1]:p_str(), S._[2]:p_str())
  end,
  p_simp = function (S, bFull)
    if bFull then
      S._[1]:p_simp(bFull)
      S._[2]:p_simp(bFull)
    end
    if _common_.isone(S._[2]) then 
      _common_.copy(S, S._[1])
    elseif _common_.iszero(S._[2]) then 
      _common_.copy(S, symbolic:_new_const_(1))
    elseif _common_.isone(S._[1]) or _common_.iszero(S._[1]) then
      _common_.copy(S, S._[1])
    end
  end
}, 

func = {
  -- S._ = {arg1, arg2, ...}
  p_char = 'FN',  
  p_signature = _common_.signature,
  p_eq = function (S1, S2) 
    return S1._name_ == S2._name_ and _common_.eq(S1, S2) 
  end,
  p_str = function (S)
    local t = {}
    for i, v in ipairs(S._) do t[i] = v:p_str() end
    return string.format('%s(%s)', S._name_, table.concat(t, ','))
  end,
  p_simp = _common_.simp,
  
},

--[[
equation = {
  -- S._ = {lft, rht}
  p_char = 'EQ',
  p_signature = _common_.signature,
  p_eq = _common_.eq,
  p_str = function (S)
    return string.format('%s = %s', S._[1]:p_str(), S._[2]:p_str())
  end,
  p_simp = _common_.simp,
}
]]
}

_common_.pair_simp = function (S, tParent)
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
      local iconst = (si[2]._parent_ == _parents_.const)
      for j = i+1, #S._ do
        local sj = S._[j]
        if sj[1] ~= 0 then
          if iconst and sj[2]._parent_ == _parents_.const then
            -- combine constants
            if tParent == _parents_.product then 
              si[2]._ = si[1] * si[2]._ + sj[1] * sj[2]._
            else -- _parents_.power 
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

_parents_.sum.p_simp = function (S, bFull)
  if bFull then
    for _, v in ipairs(S._) do v[2]:p_simp(bFull) end
  end
  -- update structure
  _common_.pair_simp(S, _parents_.product)
  -- empty list
  assert(#S._ > 0)
  -- remove zero constant
  if #S._ > 1 and _common_.iszero(S._[1][2]) then
    table.remove(S._, 1) 
    S._sign_ = nil
  end
  -- change type
  if #S._ == 1 then 
    local v = S._[1]
    if v[1] ~= 1 then 
      _common_.copy(S, symbolic:_new_const_(v[1]) * v[2])
    else 
      _common_.copy(S, v[2])
    end
  end
end

_parents_.product.p_simp = function (S, bFull)
  if bFull then
    for _, v in ipairs(S._) do v[2]:p_simp(bFull) end
  end
  _common_.pair_simp(S, _parents_.power)
  -- empty list
  assert(#S._ > 0)
  -- check constant 
  if _common_.iszero(S._[1][2]) then
    _common_.copy(S, symbolic:_new_const_(0))
    return
  elseif #S._ > 1 and _common_.isone(S._[1][2]) then 
    table.remove(S._, 1)
    S._sign_ = nil
  end
  -- change type
  if #S._ == 1 then
    local v = S._[1]
    if v[1] ~= 1 then 
      _common_.copy(S, v[2] ^ symbolic:_new_const_(v[1]))
    else 
      _common_.copy(S, v[2])
    end
  end
end

_parents_.product.p_get_const = function (S)
  local v = S._[1]
  if v[2]._parent_ == _parent_.const then 
    v = v[1] * v[2]._  -- coefficient * const
    table.remove(S._, 1)
    S._sign_ = nil
    return v
  end
  return 1
end

_parents_.power.p_get_const = function (S)
  local v = S._[2]
  if v._parent_ == _parents_.const then
    v = v._ 
    _common_.copy(S, S._[1])
    return v
  end
  return 1
end


-- methametods
symbolic.__index = function (t, k)
  return symbolic[k] or t._parent_[k]
end

symbolic.__tostring = function (S)
  return S._parent_.p_str(S)
end


symbolic._args_ = function (v1, v2)
  v1 = issymbolic(v1) and v1 or symbolic:_new_const_(v1)
  v2 = issymbolic(v2) and v2 or symbolic:_new_const_(v2)
  return v1, v2
end

symbolic.__add = function (S1, S2)
  S1, S2 = symbolic._args_(S1, S2)
  local res = symbolic:_new_sum_()
  -- S1
  if S1._parent_ == _parents_.sum then
    for i, v in ipairs(S1._) do  -- TODO use move
      table.insert(res._, {v[1], v[2]})
    end
  else
    table.insert(res._, {1, S1})
  end
  -- S2
  if S2._parent_ == _parents_.sum then 
    for _, v in ipairs(S2._) do
      table.insert(res._, {v[1], v[2]})
    end
  else
    table.insert(res._, {1, S2})
  end
  res:p_simp()
  res:p_signature()
  return res
end

symbolic.__sub = function (S1, S2)
  S1, S2 = symbolic._args_(S1, S2)
  local res = symbolic:_new_sum_()
  -- S1
  if S1._parent_ == _parents_.sum then
    for _, v in ipairs(S1._) do  -- TODO use move
      table.insert(res._, {v[1], v[2]})
    end
  else
    table.insert(res._, {1, S1})
  end
  -- S2
  if S2._parent_ == _parents_.sum then 
    for _, v in ipairs(S2._) do table.insert(res._, {-v[1], v[2]}) end
  else
    table.insert(res._, {-1, S2})
  end
  res:p_simp()
  res:p_signature()
  return res
end

symbolic.__mul = function (S1, S2)
  S1, S2 = symbolic._args_(S1, S2)
  local res = symbolic:_new_prod_()
  -- S1
  if S1._parent_ == _parents_.product then 
    for _, v in ipairs(S1._) do  -- TODO use move
      table.insert(res._, {v[1], v[2]})
    end
  else
    table.insert(res._, {1, S1})
  end
  -- S2
  if S2._parent_ == _parents_.product then 
    for _, v in ipairs(S2._) do  -- TODO use move
      table.insert(res._, {v[1], v[2]})
    end
  else
    table.insert(res._, {1, S2})
  end
  res:p_simp()
  res:p_signature()
  return res
end

symbolic.__div = function (S1, S2)
  S1, S2 = symbolic._args_(S1, S2)
  local res = symbolic:_new_prod_()
  -- S1
  if S1._parent_ == _parents_.product then 
    for _, v in ipairs(S1._) do  -- TODO use move
      table.insert(res._, {v[1], v[2]})
    end
  else
    table.insert(res._, {1, S1})
  end
  -- S2
  if S2._parent_ == _parents_.product then 
    for _, v in ipairs(S2._) do table.insert(res._, {-v[1], v[2]}) end
  else
    table.insert(res._, {1, S2})
  end
  res:p_simp()
  res:p_signature()
  return res
end

symbolic.__pow = function (S1, S2)
  S1, S2 = symbolic._args_(S1, S2)
  local res = symbolic:_new_pow_()
  if S1._parent_ == _parents_.power then
    res._[1] = S1._[1]
    res._[2] = S1._[2] * S2
    res._[2]:p_simp()  
  else 
    res._[1] = S1
    res._[2] = S2
  end
  res:p_simp()
  res:p_signature()
  return res
end

symbolic.__unm = function (S)
  local res
  if S._parent_ == _parents_.sum then
    res = symbolic:_new_sum_()
    for i, v in ipairs(S._) do
      res._[i] = {-v[1], v[2]}
    end
  else 
    res = symbolic:_new_const_(-1) * S
  end
  if S._parent_ == _parents_.product then
    res:p_simp()
  end
  res:p_signature()
  return res
end

symbolic.__eq = function (S1, S2)
  return issymbolic(S1) and issymbolic(S2) and S1:p_eq(S2)
end

symbolic._new_const_ = function (self, v)
  local o = {
    _parent_ = _parents_.const,
    _ = v,
  }
  return setmetatable(o, self)
end

symbolic._new_symbol_ = function (self, nm)
  local o = {
    _parent_ = _parents_.symbol,
    _ = nm,
  }
  return setmetatable(o, self)
end

symbolic._new_sum_ = function (self)
  local o = {
    _parent_ = _parents_.sum,
    _ = {},
  }
  return setmetatable(o, self)
end

symbolic._new_prod_ = function (self)
  local o = {
    _parent_ = _parents_.product,
    _ = {},
  }
  return setmetatable(o, self)
end

symbolic._new_pow_ = function (self)
  local o = {
    _parent_ = _parents_.power,
    _ = {1, 1},
  }
end

--- Constructor example.
--  @param t Some value.
--  @return New object of symbolic.
symbolic.new = function(self,t)
  local o = {}
  -- your logic 
  -- return object
  return setmetatable(o,self)
end

-- simplify constructor call
setmetatable(symbolic, {__call = function (self,v) return symbolic:new(v) end})
symbolic.Sym = 'Sym'
about[symbolic.Sym] = {"Sym(t)", "Create new symbolic.", help.NEW}

-- Comment to remove descriptions
symbolic.about = about

--return symbolic
S1 = symbolic:_new_const_(1)
S2 = symbolic:_new_const_(2)
S3 = symbolic:_new_symbol_('c')
S4 = symbolic:_new_symbol_('a')
S5 = symbolic:_new_const_(-1)

A = S1 + S5 
print(A)

