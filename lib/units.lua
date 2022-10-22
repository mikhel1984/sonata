--[[		sonata/lib/units.lua 

--- Operations and conversations with units.
--
--  Object structure:  </br>
--  <code>{_value=number, _key=table_of_units}</code></br>
--  Table in represented in form key-power.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2022.

		module 'units'
--]]

-------------------- Tests -------------------
--[[TEST

-- use 'units'
Unit = require 'lib.units'

-- add some rules
Unit:setRule('h', Unit(60,'min'))
Unit:setRule('min', Unit(60,'s'))

-- define variable
a = Unit(1,'m/s')
-- convert to km/h, get only value
ans = a['km/h']              --2> 3.6

-- get numerical value
-- (the save as #a)
ans = a:value()               --> 1

-- get units 
ans = a:key()                 --> 'm/s'

-- make copy
cp = a:copy() 
ans = cp                      --> Unit(1,'m/s')

-- get converted variable
b = a:convert('km/h')
ans = b                       --> Unit(3.6, 'km/h')

-- arithmetic
b = 3 * b
ans = a + b                   --> Unit(4, 'm/s')

ans = b - a                   --> Unit(2, 'm/s')

ans = a * b                   --> Unit(3, 'm^2/s^2')

ans = b / a                   --> Unit(3, '')

ans = (a < b)                 --> true

ans = b ^ 3                   --> Unit(27, 'm^3/s^3')

-- new rule
Unit:setRule('snake', Unit(48, 'parrot'))
-- define variable
c = Unit(2,'snake')
-- convert
ans = c['parrot']             --> 96

-- convert using prefix
ans = c['ksnake']            --3> 0.002

-- another definition syntax
ans = 2 * Unit('N')           --> Unit(2,'N')

-- show result
print(a)
--]]

--	LOCAL

local Ver = require('lib.utils')
local Cross = Ver.cross
local Utils = Ver.utils
Ver = Ver.versions

--- Check object type.
--  @param t Object to check.
--  @return True if the object represents units.
local function isunits(v) return type(v) == 'table' and v.isunits end

--- Combine common elements in tables, add power to the 
--  first table, remove from the second one.
--  @param t1 First key-value table.
--  @param t2 Second key-value table.
--  @param pos True when positive.
local function eliminate(t1, t2, pos)
  for k,v in pairs(t2) do
    if t1[k] then
      local vv = pos and (t1[k] + v) or (t1[k] - v)
      t1[k] = (vv ~= 0) and vv or nil
      t2[k] = nil
    end
  end
end

-- Operations with tables of units.
local op = {
['*'] = function(u1, u2) for k,v in pairs(u2) do u1[k] = (u1[k] or 0) + v end end,
['/'] = function(u1, u2) for k,v in pairs(u2) do u1[k] = (u1[k] or 0) - v end end,
['^'] = function(u, n) for k,v in pairs(u) do u[k] = v*n end end
}

--- Check equality of float point numbers.
--  @param d1 First unit object.
--  @param d2 Second unit object.
--  @return True if the objects are equal.
local function equal(d1,d2)
  return math.abs(d1-d2) <= 1e-3*math.abs(d1)
end

--	INFO

local help = SonataHelp or {new=function () return {} end}
-- description
local about = help:new("Operations and conversations according the units.")

--	MODULE

local units = {
-- mark
type = 'units', isunits = true,
-- save some results
_mem_keys_ = {},
-- rules for unit conversation
_rules_ = {},
}

--- U1 + U2
--  @param U1 First unit object.
--  @param U2 Second unit object.
--  @return Sum of unit objects.
units.__add = function (U1, U2)
  U1, U2 = units._args_(U1,U2)
  local tmp = assert(units._convert_key_(U2, U1._key), "Different units!")
  local res = units.copy(U1)
  res._value = res._value + tmp._value
  return res
end

--- U1 / U2
--  @param U1 First unit object.
--  @param U2 Second unit object.
--  @return Ratio of unit objects.
units.__div = function (U1, U2)
  U1, U2 = units._args_(U1,U2)
  U1, U2 = units._deepcopy_(U1), units._deepcopy_(U2)
  eliminate(U1._key, U2._key, false)
  -- not empty
  if next(U2._key) then
    units._expand_(U1, U2)
    eliminate(U1._key, U2._key, false)
  end
  -- add
  for k, v in pairs(U2._key) do U1._key[k] = -v end
  U1._value = U1._value / U2._value
  return U1
end

--- U1 == U2
--  @param U1 First unit object.
--  @param U2 Second unit object.
--  @return True if the objects are equal.
units.__eq = function (U1,U2)
  if not (isunits(U1) and isunits(U2)) then return false end
  local tmp = units._convert_key_(U2, U1._key)
  if tmp == nil then return false end
  return equal(U1._value, tmp._value)
end

--- Convert using v['new_units'] notation.
--  @param U Initial unit object.
--  @param s New Units.
--  @return Result of conversation of nil.
units.__index = function (U,s)
  if units[s] then
    return units[s]
  elseif type(s) == 'string' then
    local v = units.convert(U,s)
    return v and v._value or nil
  end
end

--- U1 <= U2
--  @param U1 First unit object.
--  @param U2 Second unit object.
--  @return Result of comparison.
units.__le = function (U1,U2)
  assert(isunits(U1) and isunits(U2), 'Not compatible!')
  local tmp = assert(units._convert_key_(U2, U1._key), "Different units!")
  return U1._value <= tmp._value
end

--- U1 < U2
--  @param U1 First unit object.
--  @param U2 Second unit object.
--  @return Result of comparison.
units.__lt = function (U1,U2)
  assert(isunits(U1) and isunits(U2), 'Not compatible!')
  local tmp = assert(units._convert_key_(U2, U1._key), "Different units!")
  return U1._value < tmp._value
end

--- U1 * U2
--  @param U1 First unit object.
--  @param U2 Second unit object.
--  @return Product of unit objects.
units.__mul = function (U1, U2)
  U1, U2 = units._args_(U1,U2)
  U1, U2 = units._deepcopy_(U1), units._deepcopy_(U2)
  eliminate(U1._key, U2._key, true)
  -- not empty
  if next(U2._key) then
    units._expand_(U1, U2)
    eliminate(U1._key, U2._key, true)
  end
  -- add
  for k, v in pairs(U2._key) do U1._key[k] = v end
  U1._value = U1._value * U2._value
  return U1
end

--- U ^ d
--  @param U Unit object.
--  @param d Number.
--  @return Power.
units.__pow = function (U, d)
  d = assert(Cross.float(d), "Wrong power")
  local res = isunits(U) and units._deepcopy_(U) or units:_new_(U,'')
  res._value = res._value ^ d
  op['^'](res._key, d)
  return res
end

--- U1 - U2
--  @param U1 First unit object.
--  @param U2 Second unit object.
--  @return Subtraction of objects with the same units.
units.__sub = function (U1,U2)
  U1, U2 = units._args_(U1,U2)
  local tmp = assert(units._convert_key_(U2, U1._key), "Different units!")
  local res = units.copy(U1)
  res._value = res._value - tmp._value
  return res
end


--- Units representation as string.
--  @param U Unit object.
--  @return Unit value in traditional form.
units.__tostring = function (U)
  return string.format('%s %s', 
    type(U._value) == 'number' and Utils.numstr(U._value) or tostring(U._value), 
    units.key(U))
end

--- -U
--  @param U Units object.
--  @return Object with inverted sign.
units.__unm = function (U)
  local res = units.copy(U)
  res._value = -res._value
  return res
end

units.arithmetic = 'arithmetic'
about[units.arithmetic] = {units.arithmetic, 'U1+U2, U1-U2, U1*U2, U1/U2, U1^N', help.META}

units.comparison = 'comparison'
about[units.comparison] = {units.comparison, 'U1==U2, U1~=U2, U1<U2, U1<=U2, U1>U2, U1>=U2', help.META}

--- Prepare arguments.
--  @param a First unit object or number.
--  @param b Second unit object or number.
--  @return Arguments as units.
units._args_ = function (a,b)
  a = isunits(a) and a or units:_new_(a, '')
  b = isunits(b) and b or units:_new_(b, '')
  return a,b
end

--- Convert object to another units.
--  @param U Unit object.
--  @param t Table with desired units.
--  @return New object or nil.
units._convert_key_ = function (U, t)
  local dst = units:_new_(1, '')
  dst._key = t
  local src = units._deepcopy_(U)
  src._value = 1
  local rat = units.__div(src, dst)
  -- require to check rules
  if next(rat._key) then
    local u2 = units._expand_rules_(rat)
    while u2 do
      rat = units.__mul(rat, u2)
      u2 = units._expand_rules_(rat)
    end
  end
  if next(rat._key) then
    -- can not find transformation
    dst = nil
  else
    dst._value = U._value * rat._value 
  end
  return dst
end

--- Copy all keys from the object.
--  @param U Source object.
--  @return Deep copy.
units._deepcopy_ = function (U)
  local keys = {}
  for k,v in pairs(U._key) do keys[k] = v end
  return setmetatable({_value=U._value, _key=keys}, units)
end

--- Get common part and difference between 2 strings.
--  @param str1 First string.
--  @param str2 Second string.
--  @return First prefix, second prefix, common part.
units._diff_ = function (s1, s2)
  local n1, n2 = #s1, #s2
  while n1 > 0 and n2 > 0 and string.byte(s1, n1) == string.byte(s2, n2) do
    n1 = n1 - 1
    n2 = n2 - 1
  end
  return string.sub(s1, 1, n1), string.sub(s2, 1, n2), string.sub(s1, n1+1)
end

--- Expand prefixes in both objects in-place.
--  @param U1 First unit value.
--  @param U2 Second unit object.
units._expand_ = function (U1, U2)
  local q1, q2, com1, com2 = {}, {}, {}, {}
  -- initialize q2
  for k, v in pairs(U2._key) do q2[k] = v end
  -- find common 
  for k1, v1 in pairs(U1._key) do
    local found = false
    for k2, v2 in pairs(q2) do
      local l, r, base = units._diff_(k1, k2)
      -- apply changes
      if #base > 0 and units.prefix[l] and units.prefix[r] then
        U1._value = U1._value * (units.prefix[l] ^ v1)
        U2._value = U2._value * (units.prefix[r] ^ v2)
        com1[base], com2[base] = v1, v2
        found, q2[k2] = true, nil
        break
      end
    end
    if not found then q1[k1] = v1 end
  end
  -- add common elements
  for k,v in pairs(com1) do q1[k] = v end
  for k,v in pairs(com2) do q2[k] = v end
  U1._key, U2._key = q1, q2
end

--- Apply rule. When found exclude it from 
--  the unit table.
--  @param U Unit object.
--  @return Found rule or nil.
units._expand_rules_ = function (U)
  local v, ku
  for k1, u1 in pairs(units._rules_) do
    -- check
    if U._key[k1] then 
      ku, v = k1, U._key[k1]
    else
      for k2, v2 in pairs(U._key) do
        local l, r, base = units._diff_(k1, k2)
        if #base > 0 and l == '' and units.prefix[r] then
          ku, v = k2, v2
          break
        end
      end
    end
    -- get if it is found
    if ku then
      U._key[ku] = nil
      local res = units._deepcopy_(u1)
      res._value = res._value ^ v
      for a, b in pairs(res._key) do
        res._key[a] = b * v
      end
      return res
    end
  end
  return nil
end

--- Get value or evaluate expression in brackets.
--  @param lst Token list.
--  @param n Initial position.
--  @return Table of units and next position.
units._getExpr_ = function (lst, n)
  local v = lst[n]
  if v == '(' then
    -- parse sub-expression
    local res, m = units._getTerm_(lst, n+1)
    assert(lst[m] == ')')
    return res, m+1
  elseif v == 1 then
    return {}, n+1
  elseif string.find(v, '^%a+$') then 
    return {[v] = 1}, n+1  
  else 
    error("Wrong unit "..v)
  end
end

--- Get number in power.
--  @param lst Token list.
--  @param n Initial position.
--  @return Number and the next position.
units._getNum_ = function (lst, n)
  local v = lst[n]
  if v == '(' then
    local res, m = units._getNum_(lst, n+1)
    assert(lst[m] == ')')
    return res, m+1
  elseif v == '-' then 
    local res, m = units._getNum_(lst, n+1)
    return -res, m
  elseif type(v) ~= 'number' then
    error("Expected number for power")
  end
  return v, n+1
end

--- Evaluate expression a^b.
--  @param lst Token list.
--  @param n Initial position.
--  @return Table of units and next position.
units._getPow_ = function (lst, n)
  local res, m  = units._getExpr_(lst, n)
  if lst[m] == '^' then
    local num
    num, m = units._getNum_(lst, m+1)
    op['^'](res, num)
  end
  return res, m
end

--- Evaluate a * b.
--  @param lst Token list.
--  @param n Initial position.
--  @return Table of units and next position.
units._getTerm_ = function (lst, n)
  local res, m = units._getPow_(lst, n)
  while lst[m] == '*' or lst[m] == '/' do
    -- while get * or / get terms and evaluate
    local sign, tmp = lst[m]
    tmp, m = units._getPow_(lst, m+1)
    op[sign](res, tmp)
  end
  return res, m
end

--- Create new unit object.
--  @param self Parent object.
--  @param v Numerical value. 
--  @param s String of units.
--  @return Unit object.
units._new_ = function (self, v,s)
  if not units._mem_keys_[s] then
    units._mem_keys_[s] = units._parse_(s)
  end
  return setmetatable({_value=v, _key=units._mem_keys_[s]}, self)
end

--- Get absolute value.
--  @param U Unit object.
--  @return Absolute value.
units._norm_ = function (U)
  return Cross.norm(U._value)
end

--- Parse units expression.
--  @param str Initial string with units.
--  @return Reduced list of units.
units._parse_ = function (str)
  if #str == 0 then return {} end
  local tokens = Utils.lex(str)
  assert(#tokens > 0)
  -- get powers
  local res, m = units._getTerm_(tokens, 1)
  if m-1 ~= #tokens then error("Wrong format") end
  return res
end

--- Convert one units to another.
--  @param U Source unit object.
--  @param s String with new units.
--  @return Result of conversation of nil.
units.convert = function (U, s)
  if not units._mem_keys_[s] then
    units._mem_keys_[s] = units._parse_(s)
  end
  return units._convert_key_(U, units._mem_keys_[s])
end
about[units.convert] = {'convert(s)','Convert one units to another, return new object or nil.', }

--- Create copy of the element.
--  @param U Source object.
--  @return Shalow copy.
units.copy = function (U)
  return setmetatable({_value=U._value, _key=U._key}, units)
end
about[units.copy] = {'copy()', 'Create copy of the element.', help.OTHER}

--- Convert table of units into string.
--  @param U Units object.
--  @return String with units.
units.key = function (U)
  local num, denom = {}, {}
  for k, v in pairs(U._key) do
    if v > 0 then
      num[#num+1] = (v ~= 1) and string.format('%s^%s', k, tostring(v)) or k
    else
      denom[#denom+1] = (v ~= -1) and string.format('%s^%s', k, tostring(-v)) or k
    end
  end
  if #num > 0 then
    num = (#num > 1) and string.format(#denom > 0 and '(%s)' or '%s', table.concat(num, '*'))
          or num[1]
  else 
    num = '' 
  end
  if #denom > 0 then
    denom = (#denom > 1) and string.format('(%s)', table.concat(denom, '*')) 
          or denom[1]
    num = (#num > 0) and num..'/' or '1/'
  else 
    denom = ''
  end
  return num .. denom
end
about[units.key] = {'key()', 'Get units.'}

-- prefix list
units.prefix = {
  y = 1e-24,  -- yocto
  z = 1e-21,  -- zepto
  a = 1e-18,  -- atto
  f = 1e-15,  -- femto
  p = 1e-12,  -- pico
  n = 1e-9,   -- nano
  u = 1e-6,   -- micro
  m = 1e-3,   -- milli
  c = 1e-2,   -- centi
  d = 1e-1,   -- deci
  [""] = 1,
  da = 1e+1,  -- deca
  h = 1e+2,   -- hecto
  k = 1e+3,   -- kilo
  M = 1e+6,   -- mega
  G = 1e+9,   -- giga
  T = 1e+12,  -- tera
  P = 1e+15,  -- peta
  E = 1e+18,  -- exa
  Z = 1e+21,  -- zetta
  Y = 1e+24,  -- yotta
}
about[units.prefix] = {'prefix', 'Table of possible prefixes for units.', help.OTHER}

--- Add new rule for unit transformation.
--  @param self Main class.
--  @param s Unit name.
--  @param U Equal unit object.
units.setRule = function (self, s, U)
  assert(isunits(U), 'Units object is expected!')
  self._rules_[s] = U
end
about[units.setRule] = {'Unit:setRule(s,U)', 'Add new rule for conversation.'}

--- Value of the unit object.
--  The same as #U.
--  @param U Unit object.
--  @return Value.
units.value = function (U) return U._value end
about[units.value] = {'value()', 'Get object value. Same as #U.'}
units.__len = units.value

-- simplify constructor call
setmetatable(units, {
__call = function (self,v,s)
  if s then
    assert(Cross.float(v), "Wrong value type")
  else
    v, s = 1, v
  end
  assert(type(s) == 'string', 'Wrong unit type')
  return units:_new_(v, s) 
end})
units.Unit = 'Unit'
about[units.Unit] = {'Unit([v,]s)', 'Create new elements with units.', help.NEW}

-- Comment to remove descriptions
units.about = about

return units

--==================================================

