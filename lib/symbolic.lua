--[[		sonata/lib/symbolic.lua

--- Symbolical calculus.
--
--  Object structure <br>
--  <code> {_=components, _parent=parent, _sign=signature} </code><br>
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2023.

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
print(S)
ans = S:eval{x=2, y=1}    --> Sym(3)

-- define function
foo = Sym:def('foo', {x, y}, x^y)
ans = foo(y, x)           --> y^x

-- numeric value
ans = foo(Sym(2), Sym(3)) --> Sym(8)


--]]

--	LOCAL

--local Ver = require('lib.utils')
--local Cross = Ver.cross
--local Utils = Ver.utils
--Ver = Ver.versions

local symbolic = require('lib.symbase')

--- Check object type.
--  @param v Object.
--  @return True if the object is symbolic.
local function issymbolic(v) return type(v)=='table' and v.issymbolic end

--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Symbolic calculations."
}

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
      return symbolic:_newExpr(symbolic._parentList.funcValue, t), n+1
    else
      return symbolic:_newSymbol(v), n+1
    end
  else
    error("unexpected symbol "..v)
  end
  return nil, n
end

--	MODULE

--- List of 'sybolic' functions.
local fnList = symbolic._fnList
fnList.sin = {args = {'x'}, body = math.sin}
fnList.cos = {args = {'x'}, body = math.cos}

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
      if v._parent == symbolic._parentList.symbol then
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
  "Define symbolical function. S is either symbolic expression or a Lua function."}

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
about[symbolic.eval] = {"eval([tEnv])", "Evaluate symbolic expression with the given environment."}

--- Find function using its name.
--  @param self Do nothing.
--  @param sName Function name.
--  @return Function object or nil.
symbolic.func = function (self, sName)
  return symbolic._fnList[sName] and symbolic:_newFunc(sName) or nil
end
about[symbolic.func] = {":func(sName)", "Return symbolic function if it was defined."}

--- Get name of variable.
--  @param S Symbolic object.
--  @return Variable name.
symbolic.name = function (S)
  return S._parent == symbolic._parentList.symbol and S._ or nil
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
  end
  return table.unpack(res)
end
about[symbolic.parse] = {":parse(str)", "Get simbolic expression from string."}

--- Get value of constant.
--  @param S Symbolic object.
--  @return Constant value.
symbolic.value = function (S)
  return S._parent == symbolic._parentList.const and S._ or nil
end

-- simplify constructor call
setmetatable(symbolic, {
__call = function (self, v)
  if type(v) == 'string' then
    return symbolic:_newSymbol(
      assert(v:match('^[_%a]+[_%w]*$'), 'Wrong name'))
  elseif type(v) == 'number' or type(v) == 'table' and v.__mul then   -- TODO other methods?
    return symbolic:_newConst(v)
  end
  error("Wrong argument "..tostring(v))
end})
about[symbolic] = {" (v)", "Create new symbolic variable.", help.NEW}

symbolic.introspect = function (S)
  return S:p_internal(0)
end

-- Comment to remove descriptions
symbolic.about = about

return symbolic

--=============================================================
--TODO 'eval' for expression substitution
