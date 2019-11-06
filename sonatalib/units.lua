--[[      sonatalib/units.lua 

--- Operations and conversations with units.
--
--  Object structure:                               </br>
--  <code>{value=number, key=string_of_units}</code></br>
--  Key is represented in form "power1..unit1..power2..unit2...etc'.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonatalib</a> units._collect_ion, 2017-2019.

            module 'units'
--]]

-------------------- Tests -------------------
--[[TEST

-- import 'units'
Unit = require 'sonatalib.units'

-- add some rules
Unit.add('h', Unit(60,'min'))
Unit.add('min', Unit(60,'s'))

-- define variable
a = Unit(1,'m/s')
-- convert to km/h, get only value
ans = a['km/h']                    --> 3.6

-- get numerical value
-- (the save as #a)
ans = a:val()                      --> 1

-- make copy
cp = a:copy() 
ans = cp                           --> Unit(1,'m/s')

-- get converted variable
b = a:convert('km/h')
ans = b                            --> Unit(3.6, 'km/h')

-- arithmetic
b = 3 * b
ans = a + b                        --> Unit(4, 'm/s')

ans = b - a                        --> Unit(2, 'm/s')

ans = a * b                        --> Unit(3, 'm^2/s^2')

ans = b / a                        --> Unit(3)

ans = (a < b)                      --> true

ans = b ^ 3                        --> Unit(27, 'm^3/s^3')

-- new rule
Unit.add('snake', Unit(48, 'parrot'))
-- define variable
c = Unit(2,'snake')
-- convert
ans = c['parrot']                  --> 96

-- convert using prefix
ans = c['ksnake']                  --> 0.002

-- complex rule
d = Unit(1,'W')
-- define function for conversation, apply it
lg = function (x) return math.log(x)/math.log(10) end
e = d:convert(function (x) return Unit( 10*lg((x/Unit('mW')):simp()), 'dBm') end)
ans = #e                           --0> 30

-- another definition syntax
ans = 2 * Unit('N')                --> Unit(2,'N')

-- show result
print(a)
--]]

--	LOCAL

local PART = '([^%a]+)(%a+)'     -- highlight value and unit from string

-- possible signs
local C_PROD, C_RAT, C_POW = string.byte('*'), string.byte('/'), string.byte('^')
local BASE, PREFIX = 1, 2

--- Check object type.
--  @param t Object to check.
--  @return True if the object represents units.
local function isunits(t) return type(t) == 'table' and t.isunits end

--- Comparison rule for unit sorting.
--  @param p1 First table of components.
--  @param p2 Second table of components.
--  @return True if p1 > p2.
local function comp (p1,p2) 
   return p1[1] > p2[1] or (p1[1] == p2[1] and string.reverse(p1[2]) < string.reverse(p2[2])) 
end

--- Represent table of units as string.
--  @param t Table of units.
--  @return Key string.
local function toKey(t)
   local s = {}
   for k,v in pairs(t) do s[#s+1] = {v,k} end
   table.sort(s, comp)
   for i,v in ipairs(s) do s[i] = v[1] .. v[2] end
   return table.concat(s)
end

--- Convert internal string representation to the table.
--  @param s Key string.
--  @return Table of units.
local function fromKey(s)
   local t = {}
   for v,k in string.gmatch(s, PART) do t[k] = tonumber(v) end
   return t
end

-- Operations with tables of units.
local op = {
['*'] = function(u,u1) for k,v in pairs(u1) do u[k] = (u[k] or 0) + v end end,
['/'] = function(u,u1) for k,v in pairs(u1) do u[k] = (u[k] or 0) - v end end,
['^'] = function(u,n)  for k,v in pairs(u)  do u[k] = v*n end end
}

--- Remove empty elements from table of units.
--  @param t Table with units.
--  @return Reduced table.
local function reduce(t)
   local tmp = {}
   for k,v in pairs(t) do
      if not(v == 0 or tonumber(k)) then tmp[k] = v end
   end
   return tmp
end

--- Check equality of float point numbers.
--  @param a First unit object.
--  @param b Second unit object.
--  @return True if the objects are equal.
local function equal(a,b)
   return math.abs(a-b) <= 1e-3*math.abs(a)
end

--	INFO

local help = LC_DIALOG and (require "sonatalib.help") or {new=function () return {} end}

--	MODULE

local units = {
-- mark
type = 'units', isunits = true,
-- description
about = help:new("Operations and conversations according the units."),
-- save some results
mem_parts = {},
mem_keys = {},
-- rules for unit conversation
rules = {},
}

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
units.about[units.prefix] = {'prefix', 'Table of possible prefixes for units.', help.OTHER}

--- Get common part and difference between 2 strings.
--  @param str1 First string.
--  @param str2 Second string.
--  @return First prefix, second prefix, common part.
units._diff_ = function (str1, str2)
   -- check memory
   local p1, p2 = units.mem_parts[str1], units.mem_parts[str2]
   if p1 and p2 then
      if p1[BASE] == p2[BASE] then
         return p1[PREFIX], p2[PREFIX], p1[BASE]
      else
         return str1, str2, nil
      end
   end
   -- compare, add to memory 
   local first, base, second = string.match(str1..'|'..str2, '^(.-)(.+)|(.-)%2$')
   if base then
      units.mem_parts[str1] = {base, first}
      units.mem_parts[str2] = {base, second}
      return first, second, base
   else
      return str1, str2, nil
   end
end

--- Prepare arguments.
--  @param a First unit object or number.
--  @param b Second unit object or number.
--  @return Arguments as units.
units._args_ = function (a,b)
   a = isunits(a) and a or units:new(a)
   b = isunits(b) and b or units:new(b)
   return a,b
end

--- Combine elements with common base.
--  @param val Initial value.
--  @param t Table of units.
--  @return New value and reduced table of units.
units._simplify_ = function (val,t)
   local acc,new,res = {}, {}, val
   local udiff = units._diff_
   for k,v in pairs(t) do
      local previous = nil
      local l,r,base
      for _,a in ipairs(acc) do
         l,r,base = udiff(k,a)
	 if base then previous = a; break end
      end
      if previous then
         new[previous] = new[previous] + v
	 res = res * (units.prefix[l]/units.prefix[r])^v
      else
         new[k] = v
	 acc[#acc+1] = k
      end
   end
   return res, new
end

-- ~~~~~~~~~~~~~~~~ Parsing ~~~~~~~~~~~~~~~~~~~~~~~~~~

--- Get value or evaluate expression in brackets.
--  @param str Expression.
--  @return Table of units and string rest.
units._getExpr_ = function (str)
   if #str == 0 then return {}, "" end
   -- check for brackets
   local expr, rest = string.match(str, '^(%b())(.*)')
   if expr then
      -- evaluate
      local res = units._getTerm_(string.match(expr,'%((.+)%)'))
      return res, rest
   else
      -- return table with units
      expr, rest = string.match(str, '([^%(%)%*/%^]+)(.*)')
      return {[expr]=1}, rest
   end
end

--- Evaluate expression a^b.
--  @param str Expression.
--  @return Table of units and string rest.
units._getPow_ = function (str)
   local res, rest, num = units._getExpr_(str)
   if string.byte(rest) == C_POW then
      -- get power and evaluate
      num, rest = string.match(rest, '%^(-?[%d%.]+)(.*)')
      op['^'](res, tonumber(num))
   end
   return res, rest
end

--- Evaluate a * b.
--  @param str Expression.
--  @return Table of units and string rest.
units._getTerm_ = function (str)
   local res, rest = units._getPow_(str)
   while string.byte(rest) == C_PROD or string.byte(rest) == C_RAT do
      -- while get * or / get terms and evaluate
      local sign, tmp
      sign, rest = string.match(rest, "([%*/])(.*)")
      tmp, rest = units._getPow_(rest)
      op[sign](res, tmp)
   end
   return res, rest
end

--- Parse units expression.
--  @param str Initial string with units.
--  @return Reduced list of units.
units._parse_ = function (str)
   local res = units._getTerm_(str)
   return reduce(res)
end

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

--- Create new unit object.
--  @param self Parent object.
--  @param v Numerical value. Can be unit string.
--  @param u String of units. Can be omitted.
--  @return Unit object.
units.new = function (self, v,u)
   if not u then 
      if type(v) == 'string' then v,u = 1,v else u = "" end
   end
   if not units.mem_keys[u] then
      units.mem_keys[u] = toKey(units._parse_(u))
   end
   return setmetatable({value=v, key=units.mem_keys[u]}, self)
end

--- Create copy of the element.
--  @param U Source object.
--  @return Deep copy.
units.copy = function (U)
   local cp = units:new(U.value)
   cp.key = U.key
   return cp
end
units.about[units.copy] = {'copy(U)', 'Create copy of the element.', help.OTHER}

--- Check if 2 elements can be converted to each other.
--  @param k1 First key.
--  @param k2 Second key.
--  @return <code>true</code> if conversation is possible.
units._isCompatible_ = function (k1,k2)
   if k1 == k2 then return true end
   if #k1 == 0 and #k2 ~= 0 then return false end
   local fk2 = string.gmatch(k2, PART)
   local udiff = units._diff_
   for v1,u1 in string.gmatch(k1, PART) do
      local v2,u2 = fk2()
      -- compare powers and units
      if v1 ~= v2 then return false end
      if u1 ~= u2 then 
         local l,r,base = udiff(u1,u2)
	 if not (base and units.prefix[l] and units.prefix[r]) then return false end
      end
   end
   return true
end

--- Calculate value as the result of conversation.
--  @param v Initial value.
--  @param from Initial key.
--  @param to Final key.
--  @return Result value of conversation.
units._valConvert_ = function (v, from, to)
   local f,res = string.gmatch(to, PART), v
   local udiff = units._diff_
   for v1,u1 in string.gmatch(from, PART) do
      local _,u2 = f()
      local l,r = udiff(u1,u2)
      res = res*(units.prefix[l]/units.prefix[r])^tonumber(v1)
   end
   return res
end

--- Simplify units representation.
--  @param U Initial units object.
--  @return Replace all unit to its representation if it is possible.
units._toAtom_ = function (U)
   local t, res = fromKey(U.key), U.value
   local kold, knew = "", U.key
   local usimp, udiff = units._simplify_, units._diff_
   while kold ~= knew do                            -- while can be expanded
      kold = knew
      for v1,u1 in string.gmatch(knew, PART) do     -- for every unit
         local left,right,base,row                  -- get common part and prefixes
         if units.rules[u1] then
	    row = u1
	    left,right,base = '','',u1
	 else
	    for k in pairs(units.rules) do
	       left,right,base = udiff(k,u1)
	       if base and units.prefix[left] and units.prefix[right] then row=k; break end
	    end
	 end
	 if base then                               -- get common part
	    -- replace
	    local tmp = units.rules[row]
	    local add,num = fromKey(tmp.key), tonumber(v1)
	    t[u1] = nil
	    op['^'](add,num)
	    op['*'](t,add)
	    res = res*(tmp.value*units.prefix[right]/units.prefix[left])^num
	    res, t = usimp(res, t)
	    t = reduce(t)
	 end
      end
      knew = toKey(t)                              -- back to string
   end
   return res, knew
end

--- Convert one unit to another using internal representation.
--  @param U Initial unit object,
--  @param toKey Expected key.
--  @return Converted object of <code>nil</nil>.
units._unitConvert_ = function (U, toKey)
   local res = units:new(1)
   res.key = toKey
   if units._isCompatible_(U.key, res.key) then
      res.value = units._valConvert_(U.value, U.key, res.key)
   else
      local v1,u1 = units._toAtom_(U)
      local v2,u2 = units._toAtom_(res)
      if units._isCompatible_(u1,u2) then
         res.value = units._valConvert_(v1/v2, u1, u2)
      else
         res = nil
      end
   end
   return res
end

--- Convert one units to another.
--  @param U Source unit object.
--  @param r Unit string of function f(u).
--  @return Result of conversation of <code>nil</code>.
units.convert = function (U, r)
   if type(r) == 'function' then return r(U)
   else
      local res = units:new(1, r)
      return units._unitConvert_(U, res.key)
   end
end
units.about[units.convert] = {'convert(v, units)','Convert one units to another, return new object or nil.', }

--- -U
--  @param U Units object.
--  @return Object with inverted sign.
units.__unm = function (U)
   local res = units.copy(U)
   res.value = -res.value
   return res
end

--- U1 + U2
--  @param U1 First unit object.
--  @param U2 Second unit object.
--  @return Sum of unit objects.
units.__add = function (U1,U2)
   U1,U2 = units._args_(U1,U2)
   local tmp = assert(units._unitConvert_(U2,U1.key), "Different units!")
   local res = units.copy(U1)
   res.value = res.value + tmp.value
   return res
end

--- U1 - U2
--  @param U1 First unit object.
--  @param U2 Second unit object.
--  @return Subtraction of objects with the same units.
units.__sub = function (U1,U2)
   U1,U2 = units._args_(U1,U2)
   local tmp = assert(units._unitConvert_(U2,U1.key), "Different units!")
   local res = units.copy(U1)
   res.value = res.value - tmp.value
   return res
end

--- U1 * U2
--  @param U1 First unit object.
--  @param U2 Second unit object.
--  @return Product of unit objects.
units.__mul = function (U1,U2)
   U1,U2 = units._args_(U1,U2)
   local ta, tb = fromKey(U1.key), fromKey(U2.key)
   op['*'](ta,tb)
   ta = reduce(ta)
   if not next(ta) then return U1.value*U2.value end
   local res = units:new(U1.value*U2.value)
   res.key = toKey(ta)
   return res.key == '' and res.value or res
end

--- U1 / U2
--  @param U1 First unit object.
--  @param U2 Second unit object.
--  @return Ratio of unit objects.
units.__div = function (U1,U2)
   U1,U2 = units._args_(U1,U2)
   local ta, tb = fromKey(U1.key), fromKey(U2.key)
   op['/'](ta,tb)
   ta = reduce(ta)
   if not next(ta) then return U1.value/U2.value end
   local res = units:new(U1.value/U2.value)
   res.key = toKey(ta)
   return res.key == '' and res.value or res
end

--- U1 ^ U2
--  @param U1 Unit object.
--  @param b Number.
--  @return Power.
units.__pow = function (U1,b)
   assert(not isunits(b), "Wrong power!")
   local res = isunits(U1) and units.copy(U1) or units:new(U1)
   local ta = fromKey(res.key)
   op['^'](ta,b)
   res.value = (res.value)^b
   res.key = toKey(ta)
   return res
end

units.arithmetic = 'arithmetic'
units.about[units.arithmetic] = {units.arithmetic, 'U1+U2, U1-U2, U1*u2, U1/U2, U1^n', help.META}

--- U1 == b
--  @param U1 First unit object.
--  @param U2 Second unit object.
--  @return True if the objects are equal.
units.__eq = function (U1,U2)
   if not (isunits(U1) and isunits(U2)) then return false end
   if U1.key == U2.key then return equal(U1.value, U2.value) end
   local tmp = units._unitConvert_(U2, U1.key)
   if tmp == nil then return false end
   return equal(U1.value, tmp.value)
end

--- U1 < U2
--  @param U1 First unit object.
--  @param U2 Second unit object.
--  @return Result of comparison.
units.__lt = function (U1,U2)
   assert(isunits(U1) and isunits(U2), 'Not compatible!')
   if U1.key == U2.key then return U1.value < U2.value end
   local tmp = assert(units._unitConvert_(U2, U1.key), 'Not compatible!')
   return U1.value < U2.value 
end

--- U1 <= U2
--  @param U1 First unit object.
--  @param U2 Second unit object.
--  @return Result of comparison.
units.__le = function (U1,U2)
   assert(isunits(U1) and isunits(U2), 'Not compatible!')
   if U1.key == U2.key then return U1.value <= U2.value end
   local tmp = assert(units._unitConvert_(U2, U1.key), 'Not compatible!')
   return U1.value <= U2.value 
end

units.comparison = 'comparison'
units.about[units.comparison] = {units.comparison, 'U1==U2, U1~=U2, U1<U2, U1<=U2, U1>U2, U1>=U2', help.META}

--- Prepare units for print operation.
--  @param str Units key.
--  @return Traditional representation.
units._collect_ = function (str)
   local t = {}
   for v,k in string.gmatch(str, PART) do 
      local w = tonumber(v)
      if w < 0 then w=-w end
      t[w] = t[w] or {}
      table.insert(t[w], k)
   end
   local g = {}
   for n, f in pairs(t) do
      local templ = n > 1 and (#f > 1 and '(%s)^%d' or '%s^%d') or '%s'
      g[#g+1] = string.format(templ, table.concat(f,'*'), n)
   end
   return table.concat(g,'*')
end

--- Units representation as string.
--  @param U Unit object.
--  @return Unit value in traditional form.
units.__tostring = function (U)
   local num, denom = string.match(U.key, '([^%-]*)(.*)')
   num = #num > 0 and units._collect_(num) or '1'
   denom = #denom > 0 and units._collect_(denom) or ''

   if string.find(denom, '*') then denom = '('..denom..')' end
   if #denom > 0 then num = num..'/' end

   return U.value .. ' ' .. num .. denom
end

--- Value of the unit object.
--  The same as #U.
--  @param U Unit object.
--  @return Value.
units.val = function (U) return U.value end
units.__len = units.val

--- Convert using v['new_units'] notation.
--  @param U Initial unit object.
--  @param k New Units.
--  @return Result of conversation of <code>nil</code>.
units.__index = function (U,k)
   if units[k] then
      return units[k]
   else
      local v = units.convert(U,k)
      return v and v.value or nil
   end
end

--- Reduce elements with common base part.
--  @param U Unit object.
--  @return Result of simplification.
units.simp = function (U)
   local val, t = units._simplify_(U.value, fromKey(U.key))
   t = reduce(t)
   if #t > 0 then
      local res = units:new(val)
      res.key = toKey(t)
      return res
   else return val end
end

--- Add new rule for unit transformation.
--  @param U String with units.
--  @param rule Result represented as unit object.
units.add = function (U, rule)
   assert(isunits(rule), 'Units object is expected!')
   units.rules[U] = rule
end
units.about[units.add] = {'add(U,rule)', 'Add new rule for conversation.'}


-- simplify constructor call
setmetatable(units, {__call = function (self,v,u) return units:new(v,u) end })
units.Unit = 'Unit'
units.about[units.Unit] = {'Unit(v[,u])', 'Create new elements with units.', help.NEW}


-- free memory if need
if not LC_DIALOG then units.about = nil end

return units

