--[[		sonata/lib/symbolic.lua

--- Symbolical calculus.
--
--  Object structure <br>
--  <code> {_=components, _parent=parent, _sign=signature} </code><br>
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2024.

	module 'symbolic'
--]]


--[[TEST_IT

-- use 'symbolic'
Sym = require 'matlib.symbolic'

-- create variables
x, y = Sym('x'), Sym('y')
ans = (x == y)                -->  false

-- sum
ans = x + 2*y - x + y         -->  3*y

-- product
ans = x * y^2 / x * y         -->  y^3

-- power
ans = x^y * x^(2*y)           -->  x^(3*y)

-- evaluate
S = (x+y)*(x-y)
print(S)
ans = S:eval{x=2, y=1}        -->  Sym(3)

-- define function
foo = Sym:def('foo', {x, y}, x^y)
ans = foo(y, x)               -->  y^x

-- numeric value
ans = foo(Sym(2), Sym(3))     -->  Sym(8)


--]]


--	LOCAL

local Ulex, Isint do
  local lib = require('matlib.utils')
  Ulex = lib.utils.lex
  Isint = lib.versions.isInteger
end

local symbolic = require('matlib.symbolic_tf')
local PARENTS = symbolic._parentList


--- Check object type.
--  @param v Object.
--  @return True if the object is symbolic.
local function issymbolic(v) return getmetatable(v) == symbolic end


--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Symbolic calculations."
}


-- Parser elements
local PARSER = {}


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
    return symbolic:_newConst(v), n + 1
  elseif v == '(' then
    local res, n = PARSER.sum(lst, n + 1)
    if lst[n] ~= ')' then error ("expected ')'") end
    return res, n + 1
  elseif v == '-' then
    local res, n = PARSER.prod(lst, n + 1)
    return -res, n
  elseif string.find(v, '^[%a_]') ~= nil then
    if lst[n+1] == '(' then
      local t = nil
      if lst[n+2] == ')' then
        t, n = {}, n + 2
      else
        t, n = PARSER.args(lst, n + 2)
      end
      if lst[n] ~= ')' then error ("expected ')'") end
      -- add function
      if not symbolic._fnList[v] then symbolic._fnList[v] = {} end
      table.insert(t, 1, symbolic:_newSymbol(v))
      return symbolic:_newExpr(PARENTS.funcValue, t), n + 1
    else
      return symbolic:_newSymbol(v), n + 1
    end
  else
    error ("unexpected symbol "..v)
  end
  return nil, n
end


--	MODULE


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
    table.insert(res._, {S1, 1})
  end
  -- S2
  if S2._parent == PARENTS.sum then
    for _, v in ipairs(S2._) do table.insert(res._, {v[1], v[2]}) end
  else
    table.insert(res._, {S2, 1})
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
    table.insert(res._, {S1, 1})
  end
  -- S2
  if S2._parent == PARENTS.product then
    for _, v in ipairs(S2._) do table.insert(res._, {-v[1], v[2]}) end
  else
    table.insert(res._, {S2, -1})
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
    table.insert(res._, {S1, 1})
  end
  -- S2
  if S2._parent == PARENTS.product then
    for _, v in ipairs(S2._) do table.insert(res._, {v[1], v[2]}) end
  else
    table.insert(res._, {S2, 1})
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
    table.insert(res._, {S1, 1})
  end
  -- S2
  if S2._parent == PARENTS.sum then
    for _, v in ipairs(S2._) do table.insert(res._, {-v[1], v[2]}) end
  else
    table.insert(res._, {S2, -1})
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
    for i, v in ipairs(S._) do res._[i] = {v[1], -v[2]} end
  else
    res = symbolic._m1 * S
  end
  if S._parent == PARENTS.product then
    res:p_simp()
  end
  res:p_signature()
  return res
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
--  @param sName Function name.
--  @param tArgs List of arguments (symbolic objects).
--  @param S Function body, symbolical expression or Lua function.
--  @return Function object.
symbolic.def = function (_, sName, tArgs, S)
  assert(type(sName) == 'string' and issymbolic(S), "Wrong arguments")
  local t = {}
  for i, v in ipairs(tArgs) do
    if issymbolic(v) then
      if v._parent == PARENTS.symbol then
        t[i] = v._
      else
        error "Wrong arguments"
      end
    elseif type(v) == 'string' then
      t[i] = v
    else
      error "Wrong arguments"
    end
  end
  symbolic._fnList[sName] = { args = t, body = S }
  return symbolic:_newSymbol(sName)
end
about[symbolic.def] = {":def(name_s, args_t, expr_S) --> fn_S",
  "Define symbolical function. S is either symbolic expression or a Lua function."}


--- Find derivative dS1/dS2.
--  @param S2 Variable.
--  @return Derivative.
symbolic.diff = function (self, S2)
  if self:_isfn() then error('Undefined arguments') end
  return self:p_diff(S2)
end
about[symbolic.diff] = {"S:diff(var_S) --> derivative_S",
  "Find symbolic derivative."}


--- Find value for the given substitutions.
--  @param tEnv Table of substitutions (key - value).
--  @return New object.
symbolic.eval = function (self, tEnv)
  local res = self:p_eval(tEnv or {})
  res:p_simp(true)
  res:p_signature()
  return res
end
about[symbolic.eval] = {"S:eval(env_t={}) --> upd_S|num",
  "Evaluate symbolic expression with the given environment."}


--- Expand product of polynomials.
--  @return expanded expression or the same.
symbolic.expand = function (self)
  local acc, rest = {}, {}
  -- collect elements
  if self._parent == PARENTS.product then
    for _, v in ipairs(self._) do
      local k, x = v[2], v[1]
      if x._parent == PARENTS.sum and k > 0 and Isint(k) then
        acc[#acc+1] = symbolic._binomial(x._, k)
      else
        rest[#rest+1] = v
      end
    end
  elseif self._parent == PARENTS.power then
    if self._[1]._parent == PARENTS.sum and self._[2]._parent == PARENTS.const then
      acc[1] = symbolic._binomial(self._[1]._, self._[2]._)
    end
  end
  -- not found
  if #acc == 0 then return self end
  -- main terms
  local res = nil
  for _, v in ipairs(acc) do
    if res then
      local s = {}
      for _, x in ipairs(res) do
        for _, y in ipairs(v._) do s[#s+1] = {x[1]*y[1], x[2]*y[2]} end
      end
      res = s
    else
      res = v._
    end
  end
  -- multiplier
  if #rest > 0 then
    local k = symbolic:_newExpr(PARENTS.product, rest)
    for _, x in ipairs(res) do x[1] = x[1]*k end
  end
  res = symbolic:_newExpr(PARENTS.sum, res)
  res:p_simp()
  res:p_signature()
  return res
end
about[symbolic.expand] = {"S:expand() --> expanded_S",
  "Expand product of polynomials when possible."}


--- Find function using its name.
--  @param sName Function name.
--  @return Function object or nil.
symbolic.fn = function (_, sName)
  return symbolic._fnInit[sName] or
    symbolic._fnList[sName] and symbolic:_newSymbol(sName) or nil
end
about[symbolic.fn] = {":fn(name_s) --> fn_S|nil",
  "Return symbolic function if it is defined."}


--- Show internal structure of expression.
--  @return String with structure.
symbolic.struct = function (self) return self:p_internal(0) end
about[symbolic.struct] = {"S:struct() --> str", "Show internal structure."}


--- Check if the symbol is function.
--  @return true when it is function.
symbolic.isFn = function (self) return self:_isfn() end
about[symbolic.isFn] = {'S:isFn() --> bool',
  'Return true if the symbol is function.'}


--- Get name of variable.
--  @return Variable name.
symbolic.name = function (self)
  return self._parent == PARENTS.symbol and self._ or nil
end


--- Get symbolic expression from string.
--  @param str Expression string.
--  @return One or several symbolic elements.
symbolic.parse = function(_, str)
  local tokens = Ulex(str)
  assert(#tokens > 0)
  local res = PARSER.args(tokens, 1)
  if issymbolic(res) then
    return res
  end
  return table.unpack(res)
end
about[symbolic.parse] = {":parse(expr_s) --> S1, S2, ..",
  "Get simbolic expression from string."}


--- Get numerator.
--  @return numerator of the ratio.
symbolic.ratNum = function (self) return symbolic._ratGet(self, 1) end
about[symbolic.ratNum] = {"S:ratNum() --> numerator_S", 
  "Get numerator of the expression."}


--- Get denominator.
--  @return denomenator or the ratio.
symbolic.ratDenom = function (self) return symbolic._ratGet(self, -1) end
about[symbolic.ratDenom] = {"S:ratDenom() --> denominator_S",
  "Get denominator of the expression."}


--- Get value of constant.
--  @return Constant value.
symbolic.value = function (self)
  return self._parent == PARENTS.const and self._ or nil
end


-- simplify constructor call
setmetatable(symbolic, {
__call = function (_, v)
  if type(v) == 'string' then
    local s = assert(v:match('^[_%a]+[_%w]*$'), 'Wrong name')
    return symbolic:_newSymbol(s)
  elseif type(v) == 'number' or type(v) == 'table' and v.__mul then   -- TODO other methods?
    return symbolic:_newConst(v)
  end
  error ("Wrong argument "..tostring(v))
end})
about[symbolic] = {" (num|str) --> new_S",
  "Create new symbolic variable.", help.NEW}


-- Comment to remove descriptions
symbolic.about = about

return symbolic

--=============================================================
--TODO 'eval' for expression substitution
