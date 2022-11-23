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

local _COMMON_ = {

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
  
  skip = function (S) return false end,
  
  simp = function (S, bFull)
    for _, v in ipairs(S._) do v:p_simp(bFull) end
  end,
  
  iszero = function (v)
    return issymbolic(v) and v._ == 0 or v == 0
  end, 
  
  isone = function (v)
    return issymbolic(v) and v._ == 1 or v == 1
  end,
  
  
  copy = function (S, S0)
    S._parent_ = S0._parent_
    S._ = S0._
    S._sign_ = S0.p_isatom and S0._sign_ or nil
  end,

  eval_pairs = function (S, tEnv)
    local res = symbolic:_new_obj_(S._parent_, {})
    for i, v in ipairs(S._) do
      res._[i] = {v[1], v[2]:p_eval(tEnv)}
    end
    return res
  end,

  eval = function (S, tEnv)
    local res = symbolic:_new_obj_(S._parent_, {})
    for i, v in ipairs(S._) do
      res._[i] = v:p_eval(tEnv)
    end
    return res
  end,

}


local _PARENTS_ = {

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

sum = {
  -- S._ = {{k1, S1}, {k2, S2}, ...}
  p_char = '+',
  p_isatom = false,
  p_signature = _COMMON_.signature_pairs,
  p_eq = _COMMON_.eq_pairs,
  p_str = function (S)
    local t = {}
    for i, v in ipairs(S._) do
      t[i] = string.format('%s*%s', tostring(v[1]), v[2]:p_str())
    end
    return string.format('(%s)', table.concat(t, ' + '))
  end,
  p_eval = _COMMON_.eval_pairs,
  -- p_simp below
},

product = {
  -- S._ = {{pow1, S1}, {pow2, S2}, ...}
  p_sym = '*',  
  p_isatom = false,
  p_signature = _COMMON_.signature_pairs,
  p_eq = _COMMON_.eq_pairs, 
  p_str = function (S)
    local t = {}
    for i, v in ipairs(S._) do
      t[i] = string.format('%s^%s', v[2]:p_str(), tostring(v[1]))
    end
    return string.format('(%s)', table.concat(t, ' * '))
  end,
  p_eval = _COMMON_.eval_pairs,
  -- p_simp below
},

power = {
  -- S._ = {base, power}
  p_char = '^',
  p_isatom = false,
  p_signature = _COMMON_.signature,
  p_eq = _COMMON_.eq,
  p_str = function (S) 
    return string.format('%s^%s', S._[1]:p_str(), S._[2]:p_str())
  end,
  p_eval = _COMMON_.eval,
  -- p_simp below
}, 

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
  -- no p_eval ?
  
},

func_value = {
  -- S._ = {fn, arg1, arg2, ...}
  p_char = 'FN',
  p_isatom = false,
  p_signature = _COMMON_.signature,
  p_eq = _COMMON_.eq,
  p_str = function (S)
    local t = {}
    for i = 2, #S._ do t[#t+1] = S._[i]:p_str() end
    return string.format('%s(%s)', S._[1]._, table.concat(t, '.'))
  end,
  p_simp = _COMMON_.simp,
  -- p_eval


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
}

symbolic._fn_list_ = {
  sin = {args = {'x'}, body = math.sin},
  cos = {args = {'x'}, body = math.cos},
}

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

_PARENTS_.func_value.p_eval = function (S, tEnv)
  local t, val = {S._[1]}, {}
  for i = 2, #S._ do
    local v = S._[i]:p_eval(tEnv)
    t[i] = v
    if v._parent_ == _PARENTS_.const then
      val[#val+1] = v._
    end
  end
  if #val + 1 == #t then
    local body = symbolic._fn_list_[S._[1]._].body
    -- evaluate
    return symbolic:_new_const_( body(table.unpack(val)) )
  else 
    local res = symbolic:_new_obj_(S._parent_, t)
    return res
  end
end

_PARENTS_.product.p_get_const = function (S)
  local v = S._[1]
  if v[2]._parent_ == _PARENTS_.const then 
    v = v[1] * v[2]._  -- coefficient * const
    table.remove(S._, 1)
    S._sign_ = nil
    return v
  end
  return 1
end

_PARENTS_.power.p_get_const = function (S)
  local v = S._[2]
  if v._parent_ == _PARENTS_.const then
    v = v._ 
    _COMMON_.copy(S, S._[1])
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


symbolic._tosym_ = function (v1, v2)
  v1 = issymbolic(v1) and v1 or symbolic:_new_const_(v1)
  v2 = issymbolic(v2) and v2 or symbolic:_new_const_(v2)
  return v1, v2
end

symbolic.__add = function (S1, S2)
  S1, S2 = symbolic._tosym_(S1, S2)
  local res = symbolic:_new_obj_(_PARENTS_.sum, {})
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

symbolic.__sub = function (S1, S2)
  S1, S2 = symbolic._tosym_(S1, S2)
  local res = symbolic:_new_obj_(_PARENTS_.sum, {})
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

symbolic.__mul = function (S1, S2)
  S1, S2 = symbolic._tosym_(S1, S2)
  local res = symbolic:_new_obj_(_PARENTS_.product, {})
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

symbolic.__div = function (S1, S2)
  S1, S2 = symbolic._tosym_(S1, S2)
  local res = symbolic:_new_obj_(_PARENTS_.product, {})
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

symbolic.__pow = function (S1, S2)
  S1, S2 = symbolic._tosym_(S1, S2)
  local res = symbolic:_new_obj_(_PARENTS_.power)
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

symbolic.__unm = function (S)
  local res
  if S._parent_ == _PARENTS_.sum then
    res = symbolic:_new_obj_(_PARENTS_.sum, {})
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

symbolic.__eq = function (S1, S2)
  return issymbolic(S1) and issymbolic(S2) and S1:p_eq(S2)
end

symbolic._new_const_ = function (self, v)
  local o = {
    _parent_ = _PARENTS_.const,
    _sign_ = '.',
    _ = v,
  }
  return setmetatable(o, self)
end

symbolic._new_symbol_ = function (self, sName)
  local o = {
    _parent_ = _PARENTS_.symbol,
    _sign_ = sName,
    _ = sName,
  }
  return setmetatable(o, self)
end

symbolic._new_obj_ = function (self, parent, v)
  local o = {
    _parent_ = parent,
    _ = v,
  }
  return setmetatable(o, self)
end

--[[
symbolic._new_sum_ = function (self)
  local o = {
    _parent_ = _PARENTS_.sum,
    _ = {},
  }
  return setmetatable(o, self)
end
]]

--[[
symbolic._new_prod_ = function (self)
  local o = {
    _parent_ = _PARENTS_.product,
    _ = {},
  }
  return setmetatable(o, self)
end
]]

--[[
symbolic._new_pow_ = function (self)
  local o = {
    _parent_ = _PARENTS_.power,
    _ = {1, 1},
  }
  return setmetatable(o, self)
end
]]

symbolic._new_func_ = function (self, sName)
  local o = {
    _parent_ = _PARENTS_.func,
    _ = sName,
    _sign_ = sName..'()'
  }
  return setmetatable(o, self)
end

symbolic.eval = function (S, tEnv)
  local res = S:p_eval(tEnv or {})
  res:p_simp(true)
  res:p_signature()
  return res
end

symbolic.name = function (S)
  return S._parent_ == _PARENTS_.symbol and S._ or nil
end

symbolic.value = function (S)
  return S._parent_ == _PARENTS_.const and S._ or nil
end

-- sName - function name
-- tArgs - list of arguments 
-- S - function body, symbolical expression or function
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
  symbolic._fn_list_[sName] = {
    args = t,
    body = S,
  }
  return symbolic:_new_func_(sName)
end

symbolic.func = function (self, sName)
  return symbolic._fn_list_[sName] and symbolic:_new_func_(sName) or nil
end

symbolic.__call = function (S, ...)
  if S._parent_ == _PARENTS_.func then
    local t = {...}
    local fn = symbolic._fn_list_[S._]
    if #t ~= #fn.args then 
      error(string.format("Expected %d arguments for %s", #fn.args, S._))
    end
    if type(fn.body) == 'function' then
      table.insert(t, 1, S)
      return symbolic:_new_obj_(_PARENTS_.func_value, t)
    else -- expression
      local env = {}
      for i, k in ipairs(fn.args) do env[k] = t[i] end
      return symbolic.eval(fn.body, env)
    end
  end
end

-- simplify constructor call
setmetatable(symbolic, {
__call = function (self,v) 
  if type(v) == 'string' then
    return symbolic:_new_symbol_(v)  -- TODO check name correctnes
  elseif type(v) == 'number' then    -- TODO apply for other sonata types
    return symbolic:_new_const_(v)
  end
  error("Wrong symbolic argument "..tostring(v))
end})
symbolic.Sym = 'Sym'
about[symbolic.Sym] = {"Sym(t)", "Create new symbolic variable.", help.NEW}

-- Comment to remove descriptions
symbolic.about = about

--return symbolic
S1 = symbolic(1)
S2 = symbolic(2)
S3 = symbolic('c')
S4 = symbolic('a')
S5 = symbolic(0)

S6 = symbolic('x')
S7 = symbolic('y')

symbolic:def('sum', {S6,'y'}, S6 + S7)
f = symbolic:func('sin')
B = f(S5)
print(B:eval())
