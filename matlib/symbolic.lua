--[[		sonata/lib/symbolic.lua

--- Symbolical calculus.
--
--  Object structure <br>
--  <code> {_=components, _parent=parent, _sign=signature} </code><br>
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2023-2026.

	module 'symbolic'
--]]


--[[TEST_IT

-- use 'symbolic'
Sym = require 'matlib.symbolic'

-- create variables
x, y = Sym('x'), Sym('y')
ans = (x == y)                -->  false

-- parse expressions
e1, e2 = Sym('x+y, x*y')
ans = e1                      -->  y+x

ans = e2                      -->  y*x

-- sum
ans = x + 2*y - x + y         -->  3*y

-- product
ans = x * y^2 / x * y         -->  y^3

-- power
ans = x^y * x^(2*y)           -->  x^(3*y)

-- evaluate
S = (x+y)*(x-y)
ans = S:eval{x=2, y=1}        -->  3

-- define function
foo = function (x, y) return x^y end
ans = foo(y, x)               -->  y^x

-- expand
ans = S:expand()              -->  x*x-y*y

-- derivative
ans = (x^3-Sym:sin(2*x)):diff(x)     -->  3.0*x*x-2*Sym:cos(2*x)

-- partial derivative
ans = foo(x, y):diff(y, 2)    -->  x^y*Sym:log(x)^2

-- parts
S1 = (x-y)/(x+y)
ans = S1:ratNum()             -->  x-y

ans = S1:ratDenom()           -->  x+y

-- internal structure
print(S1:struct())

--]]


--	LOCAL

local _ext = {
  utils = require("matlib.utils"),
  tf = require("matlib.symbolic_tf"),
}

local _lex = _ext.utils.utils.lex
local _tag = { STRUCT="structure", TRANSFORM="transformation" }

local symbolic = _ext.tf
local _parents = symbolic._parentList
local _tinsert = table.insert


--- Check object type.
--  @param v Object.
--  @return True if the object is symbolic.
local function _issymbolic(v) return getmetatable(v) == symbolic end


--- Check function argument. 
--  Raise error when incompatible.
--  @param v Value to check.
local function _checkarg(v)
  if not (_issymbolic(v) 
    or type(v) == "number" 
    or type(v) == "table" and v.__add and v.__mul) 
  then
    error("Unexpected argument "..tostring(v))
  end
end


--- Declare rules for specific function.
--  @param name Function name.
--  @param fn Numeric Lua function.
--  @param deriv List of partial derivatives for each function argument.
--  @return Function that generates symbolic expression.
local function _wrapFn (name, fn, deriv)
  -- register
  symbolic._fnList[name] = fn
  symbolic._fnDiff[name] = deriv
  -- ignore first arg, evaluate the rest
  return function (_, ...)
    local t = {name, ...}
    for i = 2, #t do _checkarg(t[i]) end
    return symbolic:_newExpr(_parents.funcValue, t)
  end
end

symbolic.sin = _wrapFn("sin", math.sin,
  {function (x) return symbolic:cos(x) end})


symbolic.cos = _wrapFn("cos", math.cos,
  {function (x) return -symbolic:sin(x) end})

symbolic.tan = _wrapFn("tan", math.tan,
  {function (x) return 1 / symbolic:cos(x)^2 end})

symbolic.asin = _wrapFn("asin", math.asin,
  {function (x) return 1 / symbolic:sqrt(1 - x*x) end})

symbolic.acos = _wrapFn("acos", math.acos,
  {function (x) return -1 / symbolic:sqrt(1 - x*x) end})

symbolic.atan = _wrapFn("atan", math.atan,
  {function (x) return 1 / (1 + x*x) end})

symbolic.sqrt = _wrapFn("sqrt", math.sqrt, 
{function (x) return 1 / (symbolic:sqrt(x) * 2) end})

symbolic.log = _wrapFn("log", math.log,
  {function (x) return 1 / x end})

symbolic.exp = _wrapFn("exp", math.exp,
  {function (x) return symbolic:exp(x) end})


--	INFO

local _help = SonataHelp or {}
-- description
local _about = {
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
  while lst[n] == "," do
    t[#t+1], n = PARSER.sum(lst, n+1)
  end
  return t, n
end


--- Parse sum or difference.
--  @param lst List with tokens.
--  @param ind Element index.
--  @return Table and next index.
PARSER.sum = function (lst, ind)
  local res, n = PARSER.prod(lst, ind)
  while true do
    if lst[n] == "+" then
      local tmp, m = PARSER.prod(lst, n+1)
      res, n = res + tmp, m
    elseif lst[n] == "-" then
      local tmp, m = PARSER.prod(lst, n+1)
      res, n = res - tmp, m
    else break end
  end
  return res, n
end


--- Parse product or ratio.
--  @param lst List with tokens.
--  @param ind Element index.
--  @return Table and next index.
PARSER.prod = function (lst, ind)
  local res, n = PARSER.pow(lst, ind)
  while true do
    if lst[n] == "*" then
      local tmp, m = PARSER.pow(lst, n+1)
      res, n = res * tmp, m
    elseif lst[n] == "/" then
      local tmp, m = PARSER.pow(lst, n+1)
      res, n = res / tmp, m
    else break end
  end
  return res, n
end


--- Parse power.
--  @param lst List with tokens.
--  @param ind Element index.
--  @return Table and next index.
PARSER.pow = function (lst, ind)
  local res, n = PARSER.prim(lst, ind)
  if lst[n] == "^" then  -- TODO add **
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
  local v, res = lst[n], nil
  if type(v) == "number" then
    return v, n + 1
  elseif v == "(" then
    res, n = PARSER.sum(lst, n + 1)
    if lst[n] ~= ")" then error ("expected ')'") end
    return res, n + 1
  elseif v == "-" then
    res, n = PARSER.prod(lst, n + 1)
    return -res, n
  elseif string.find(v, "^[%a_]") ~= nil then
    if lst[n+1] == "(" then
      local t = nil
      if lst[n+2] == ")" then
        t, n = {}, n + 2
      else
        t, n = PARSER.args(lst, n + 2)
      end
      if lst[n] ~= ")" then error ("expected ')'") end
      -- add function
      if not symbolic._fnList[v] then symbolic._fnList[v] = {} end
      _tinsert(t, 1, v)
      return symbolic:_newExpr(_parents.funcValue, t), n + 1
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
  _checkarg(S1); _checkarg(S2)
  local res = symbolic:_newExpr(_parents.sum, {})
  -- S1
  if _issymbolic(S1) and S1._parent == _parents.sum then
    for _, v in ipairs(S1._) do _tinsert(res._, {v[1], v[2]}) end
  else
    _tinsert(res._, {S1, 1})
  end
  -- S2
  if _issymbolic(S2) and S2._parent == _parents.sum then
    for _, v in ipairs(S2._) do _tinsert(res._, {v[1], v[2]}) end
  else
    _tinsert(res._, {S2, 1})
  end
  res = res:pSimp()
  if _issymbolic(res) then
    res:pSignature()
  end
  return res
end


--- S1 / S2
--  @param S1 Symbolic object or number.
--  @param S2 Symbolic object or number.
--  @return Ratio object.
symbolic.__div = function (S1, S2)
  _checkarg(S1); _checkarg(S2)
  local res = symbolic:_newExpr(_parents.product, {})
  -- S1
  if _issymbolic(S1) and S1._parent == _parents.product then
    for _, v in ipairs(S1._) do _tinsert(res._, {v[1], v[2]}) end
  else
    _tinsert(res._, {S1, 1})
  end
  -- S2
  if _issymbolic(S2) and S2._parent == _parents.product then
    for _, v in ipairs(S2._) do _tinsert(res._, {v[1], -v[2]}) end
  else
    _tinsert(res._, {S2, -1})
  end
  res = res:pSimp()
  if _issymbolic(res) then
    res:pSignature()
  end
  return res
end


--- S1 == S2
--  @param S1 Symbolic object or number.
--  @param S2 Symbolic object or number.
--  @return true when objects are equal.
symbolic.__eq = function (S1, S2)
  return _issymbolic(S1) and _issymbolic(S2) and S1:pEq(S2)
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
  _checkarg(S1); _checkarg(S2)
  local res = symbolic:_newExpr(_parents.product, {})
  -- S1
  if _issymbolic(S1) and S1._parent == _parents.product then
    for _, v in ipairs(S1._) do _tinsert(res._, {v[1], v[2]}) end
  else
    _tinsert(res._, {S1, 1})
  end
  -- S2
  if _issymbolic(S2) and S2._parent == _parents.product then
    for _, v in ipairs(S2._) do _tinsert(res._, {v[1], v[2]}) end
  else
    _tinsert(res._, {S2, 1})
  end
  res = res:pSimp()
  if _issymbolic(res) then
    res:pSignature()
  end
  return res
end


--- S1 ^ S2
--  @param S1 Symbolic object or number.
--  @param S2 Symbolic object or number.
--  @return Power object.
symbolic.__pow = function (S1, S2)
  _checkarg(S1); _checkarg(S2)
  local res = symbolic:_newExpr(_parents.product)
  if _issymbolic(S1) and S1._parent == _parents.product then
    -- (a^x)^y
    local t = {}
    for i, v in ipairs(S1._) do t[i] = {v[1], v[2]*S2} end
    res._ = t
  else
    res._ = {{S1, S2}}
  end
  res = res:pSimp()
  if _issymbolic(res) then
    res:pSignature()
  end
  return res
end


--- S1 - S2
--  @param S1 Symbolic object or number.
--  @param S2 Symbolic object or number.
--  @return Difference object.
symbolic.__sub = function (S1, S2)
  _checkarg(S1); _checkarg(S2)
  local res = symbolic:_newExpr(_parents.sum, {})
  -- S1
  if _issymbolic(S1) and S1._parent == _parents.sum then
    for _, v in ipairs(S1._) do _tinsert(res._, {v[1], v[2]}) end
  else
    _tinsert(res._, {S1, 1})
  end
  -- S2
  if _issymbolic(S2) and S2._parent == _parents.sum then
    for _, v in ipairs(S2._) do _tinsert(res._, {v[1], -v[2]}) end
  else
    _tinsert(res._, {S2, -1})
  end
  res = res:pSimp()
  if _issymbolic(res) then
    res:pSignature()
  end
  return res
end


--- String representation.
--  @param S Symbolic object.
--  @return String.
symbolic.__tostring = function (S) return S._parent.pStr(S) end


--- -S
--  @param S Symbolic object.
--  @return Negative value.
symbolic.__unm = function (S)
  local res = nil
  if S._parent == _parents.sum then
    res = symbolic:_newExpr(_parents.sum, {})
    for i, v in ipairs(S._) do res._[i] = {v[1], -v[2]} end
  else
    res = (-1) * S
  end
  res:pSignature()
  return res
end


-- Metamethods
_about["_ar"] = {"arithmetic: a+b, a-b, a*b, a/b, a^b, -a", nil, _help.META}
_about["_cmp"] = {"comparison: a==b, a~=b", nil, _help.META}


--- Find derivative dS1/dS2.
--  @param S2 Variable.
--  @param n (=1) Order.
--  @return Derivative.
symbolic.diff = function (self, S2, n)
  n = n or 1
  if type(S2) == "string" then S2 = symbolic:_newSymbol(S2) end
  local res = self
  for i = n, 1, -1 do
    res = res:pDiff(S2)
  end
  return res
end
_about[symbolic.diff] = {"S:diff(var_S) --> derivative_S",
  "Find symbolic derivative.", _tag.TRANSFORM}


--- Find value for the given substitutions.
--  @param tEnv Table of substitutions (key - value).
--  @return New object.
symbolic.eval = function (self, tEnv)
  local res = self:pEval(tEnv or {})
  res = res:pSimp(true)
  if _issymbolic(res) then
    res:pSignature()
  end
  return res
end
_about[symbolic.eval] = {"S:eval(env_t={}) --> upd_S|num",
  "Evaluate symbolic expression with the given environment.", _tag.TRANSFORM}


--- Expand product of polynomials.
--  @return expanded expression or the same.
symbolic.expand = function (self)
  local acc, rest = {}, {}
  -- collect elements
  if self._parent == _parents.product then
    local toint = _ext.utils.versions.toInteger
    for _, v in ipairs(self._) do
      local k, x = v[2], v[1]
      if x._parent == _parents.sum and k > 0 and toint(k) ~= nil then
        acc[#acc+1] = symbolic._binomial(x._, k)
      else
        rest[#rest+1] = v
      end
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
    local k = symbolic:_newExpr(_parents.product, rest)
    for _, x in ipairs(res) do x[1] = x[1]*k end
  end
  res = symbolic:_newExpr(_parents.sum, res)
  res = res:pSimp()
  if _issymbolic(res) then
    res:pSignature()
  end
  return res
end
_about[symbolic.expand] = {"S:expand() --> expanded_S",
  "Expand product of polynomials when possible.", _tag.TRANSFORM}


--- Show internal structure of expression.
--  @return String with structure.
symbolic.struct = function (self) return self:pInternal(0) end
_about[symbolic.struct] = {"S:struct() --> str",
  "Show internal structure.", _tag.STRUCT}


--- Get symbolic expression from string.
--  @param str Expression string.
--  @return One or several symbolic elements.
symbolic._parse = function(str)
  local tokens = _lex(str)
  assert(#tokens > 0)
  local res = PARSER.args(tokens, 1)
  if _issymbolic(res) then
    return res
  end
  return _ext.utils.versions.unpack(res)
end
-- Deprecated
symbolic.parse = function (_, str) return symbolic._parse(str) end


--- Get numerator.
--  @return numerator of the ratio.
symbolic.ratNum = function (self) return symbolic._ratGet(self, 1) end
_about[symbolic.ratNum] = {"S:ratNum() --> numerator_S",
  "Get numerator of the expression.", _tag.STRUCT}


--- Get denominator.
--  @return denomenator or the ratio.
symbolic.ratDenom = function (self) return symbolic._ratGet(self, -1) end
_about[symbolic.ratDenom] = {"S:ratDenom() --> denominator_S",
  "Get denominator of the expression.", _tag.STRUCT}


-- simplify constructor call
setmetatable(symbolic, {
__call = function (_, v)
  if type(v) == "string" then
    return symbolic._parse(v)
  elseif type(v) == "number" or type(v) == "table" and v.__mul and v.__add
  then
    return v  -- do nothing
  end
  error ("Wrong argument "..tostring(v))
end})
_about[symbolic] = {" (num|str) --> new_S",
  "Create new symbolic variable.", _help.NEW}


-- Comment to remove descriptions
symbolic.about = _about

return symbolic

--=============================================================
--TODO rawget, rawset
