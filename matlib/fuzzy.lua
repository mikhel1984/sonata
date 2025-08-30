--[[		sonata/matlib/fuzzy.lua

--- Fuzzy logic..
--
--  </br></br><b>Authors</b>: Your Name

	module 'fuzzy'
--]]

-- Define here your tests, save results to 'ans',
-- use --> for the strict equality
-- and --n> for the n-digit precision in the case of floating numbers.
--[[TEST_IT

-- use 'fuzzy'
Fz = require 'matlib.fuzzy'

-- example
a = Fz()
-- check equality
ans = a.type                  -->  'fuzzy'

-- check relative equality ( ~10^(-2) )
ans = math.pi               --.2> 355/113

--]]


--	LOCAL

local ERR_ORDER = "Wrong order"
local ERR_OPERATION = "Unexpected operation"

--	INFO

local _help = SonataHelp or {}  -- optional
-- description
local _about = {
__module__ = "Fuzzy logic."
}


--	MODULE

local _op = { 
  AND='conjunction', OR='disjunction', NOT='complement', 
  fnNot = function (x) return 1 - x end,
}


local mt_set = { 
  type = 'fuzzy_set', 
  _default = 'user_mf',
}
mt_set.__index = mt_set


local function _newSet (s, op, nm)
  local o = {
    set=s,
    op=op,
    name=nm
  }
  return setmetatable(o, mt_set)
end


local function _setArgs (S1, S2)
  if getmetatable(S1) ~= mt_set then S1 = _newSet(S1) end
  if getmetatable(S2) ~= mt_set then S2 = _newSet(S2) end
  return S1, S2
end


mt_set._eval = function (S, x, env)
  if not S.op then
    return S.set(x)
  elseif S.op == _op.AND then
    return env.AND(S.set[1]:_eval(x, env), S.set[2]:_eval(x, env))
  elseif S.op == _op.OR then
    return env.OR(S.set[1]:_eval(x, env), S.set[2]:_eval(x, env))
  elseif S.op == _op.NOT then
    return env.NOT(S.set:_eval(x, env))
  end
  error(ERR_OPERATION)
end


mt_set.__call = function (S, x, env)
  env = env or { AND=math.min, OR=math.max, NOT=_op.fnNot, }
  return mt_set._eval(S, x, env)
end


mt_set._str = function (S)
  if not S.op then
    return S.name or mt_set._default
  elseif S.op == _op.AND then
    return string.format("(%s & %s)", S.set[1]:_str(), S.set[2]:_str())
  elseif S.op == _op.OR then
    return string.format("(%s | %s)", S.set[1]:_str(), S.set[2]:_str())
  elseif S.op == _op.NOT then
    return string.format("~" .. S.set:_str())
  end
  error(ERR_OPERATION)
end


mt_set.__tostring = function (S)
  return string.format("fuzzy %s",  mt_set._str(S))
end


mt_set.andf = function (S1, S2)
  return _newSet({_setArgs(S1, S2)}, _op.AND)
end
mt_set.__band = mt_set.andf


mt_set.orf = function (S1, S2)
  return _newSet({_setArgs(S1, S2)}, _op.OR)
end
mt_set.__bor = mt_set.orf


mt_set.notf = function (S)
  return _newSet(S, _op.NOT)
end
mt_set.__bnot = mt_set.notf






local fuzzy = {
-- mark
type = 'fuzzy',
}
-- methametods
fuzzy.__index = fuzzy

fuzzy.trimf = function (_, a, b, c)
  assert(a <= b and b <= c, ERR_ORDER)
  local k1 = (b > a) and (1/(b-a)) or 0
  local k2 = (c > b) and (-1/(c-b)) or 0
  local fn = function (x)
    if x < a or x > c then
      return 0
    elseif x < b then
      return k1*(x-a)
    end
    return 1 + k2*(x-b)
  end
  return _newSet(fn, nil, 'trimf')
end

fuzzy.trapmf = function (_, a, b, c, d)
  assert(a <= b and b <= c and c <= d, ERR_ORDER)
  local k1 = (b > a) and (1/(b-a)) or 0
  local k2 = (d > c) and (-1/(d-c)) or 0
  local fn = function (x)
    if x < a or x > d then
      return 0
    elseif x < b then
      return k1*(x-a)
    elseif x < c then
      return 1
    end
    return 1 + k2*(x-c)
  end
  return _newSet(fn, nil, 'trapmf')
end

fuzzy.newmf = function (_, fn, name)
  return _newSet(fn, nil, name)
end


--- Check object type.
--  @param v Object.
--  @return True if the object is fuzzy.
local function _isfuzzy(v) return getmetatable(v) == fuzzy end


--- Constructor example.
--  @param t Some value.
--  @return New object of fuzzy.
fuzzy.new = function(self, t)
  local o = {}
  -- your logic
  -- return object
  return setmetatable(o, self)
end
_about[fuzzy.new] = {":new(t) --> F", "Explicit constructor.", _help.NEW}
-- begin from ':' to get 'Fz:new(t)'


-- simplify constructor call
setmetatable(fuzzy, {__call = function (self, v) return fuzzy:new(v) end})
_about[fuzzy] = {" (t) --> F", "Create new fuzzy.", _help.NEW}
-- begin from ' ' to get 'Fz ()'


--- Method example.
--  It is good idea to define method for the copy creation.
--  @return Copy of the object.
fuzzy.copy = function (self)
  -- some logic
  return fuzzy:new(argument)
end
_about[fuzzy.copy] = {"F:copy() --> cpy_F",
  "Create a copy of the object."} -- third element is optional, default is 'base'


-- Comment to remove descriptions
fuzzy.about = _about

--return fuzzy

--======================================
--TODO: write new functions

a = fuzzy:trapmf(0, 1, 2, 3)
b = fuzzy:trimf(-1, 0, 1)
local c = a & b | (~a) & b
print(c)
