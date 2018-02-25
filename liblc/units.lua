--[[      liblc/units.lua 

--- Operations and conversations with units.
--  @author <a href="mailto:vpsys@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

            module 'units'
--]]

-------------------- Tests -------------------
--[[!!
Unit = require 'liblc.units'

-- add some rules
Unit.add('h', Unit(60,'min'))
Unit.add('min', Unit(60,'s'))

-- define variable
a = Unit(1,'m/s')
-- convert to km/h, get only value
ans = a['km/h']                    --> 3.6

-- get numerical value
ans = #a                           --> 1

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
e = d:convert(function (x) return Unit(10*math.log((x/Unit(1,'mW')):simp(),10), 'dBm') end)
ans = #e                           --> 30

-- another definition syntax
ans = 2 * Unit('N')                --> Unit(2,'N')

-- show result
print(a)
]]

local part = '([^%a]+)(%a+)'     -- highlight value and unit from string
-- possible signs
local c_prod, c_rat, c_pow = string.byte('*'), string.byte('/'), string.byte('^')

-------------------------------------------- 
-- @class table
-- @name units
-- @field type Define object type string.
-- @field isunits <code>true</code> for unit object.
-- @field about Description of functions.
-- @field prefix List of prefixes in SI. Can be expanded.
-- @field mem_parts Intermediate results for unit parts.
-- @field mem_keys Intermediate results for unit keys.
-- @field rules List of rules for units transformation.

local units = {}
-- mark
units.type = 'units'
units.isunits = true
-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
units.about = help:new("Operations and conversations according the units.")

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

-- save some results
units.mem_parts = {}
units.mem_keys = {}

-- rules for unit conversation
units.rules = {}

--- Check object type.
--    <i>Private function.</i>
--    @param t Object for checking.
--    @return <code>true</code> if table is units.
local function isunits(t) return type(t) == 'table' and t.isunits end

--- Comparison rule for unit sorting.
--    <i>Private function.</i>
--    @param p1 First table of components.
--    @param p2 Second table of components.
--    @return <code>true</code> if p1 > p2.
local function comp (p1,p2) 
   return p1[1] > p2[1] or (p1[1] == p2[1] and string.reverse(p1[2]) < string.reverse(p2[2])) 
end

--- Represent table of units as string.
--    <i>Private function.</i>
--    @param t Table of units.
--    @return Key string.
local function tokey(t)
   local s = {}
   for k,v in pairs(t) do s[#s+1] = {v,k} end
   table.sort(s, comp)
   for i,v in ipairs(s) do s[i] = v[1] .. v[2] end
   return table.concat(s)
end

--- Convert internal string representation to the table.
--    <i>Private function.</i>
--    @param s Key string.
--    @return Table of units.
local function fromkey(s)
   local t = {}
   for v,k in string.gmatch(s, part) do t[k] = tonumber(v) end
   return t
end

--- Get common part and difference between 2 strings.
--    <i>Private function.</i>
--    @param s1 First string.
--    @param s2 Second string.
--    @return First prefix, second prefix, common part.
local function diff(s1, s2)
   -- check memory
   local p1, p2 = units.mem_parts[s1], units.mem_parts[s2]
   if p1 and p2 then
      if p1.base == p2.base then
         return p1.prefix, p2.prefix, p1.base
      else
         return s1, s2, nil
      end
   end
   -- compare, add to memory 
   local first, base, second = string.match(s1..'|'..s2, '^(.-)(.+)|(.-)%2$')
   if base then
      units.mem_parts[s1] = {base = base, prefix = first}
      units.mem_parts[s2] = {base = base, prefix = second}
      return first, second, base
   else
      return s1, s2, nil
   end
end

--- Prepare arguments.
--    <i>Private function.</i>
--    @param a First unit object or number.
--    @param b Second unit object or number.
--    @return Arguments as units.
local function args(a,b)
   a = isunits(a) and a or units:new(a)
   b = isunits(b) and b or units:new(b)
   return a,b
end

-- Operations with tables of units.
local op = {
['*'] = function(u,u1) for k,v in pairs(u1) do u[k] = (u[k] or 0) + v end end,
['/'] = function(u,u1) for k,v in pairs(u1) do u[k] = (u[k] or 0) - v end end,
['^'] = function(u,n) for k,v in pairs(u) do u[k] = v*n end end
}

--- Remove empty elements from table of units.
--    <i>Private function.</i>
--    @param t Table with units.
--    @return Reduced table.
local function reduce(t)
   local tmp = {}
   for k,v in pairs(t) do
      if not(v == 0 or tonumber(k)) then tmp[k] = v end
   end
   return tmp
end

--- Combine elements with common base.
--    <i>Private function.</i>
--    @param val Initial value.
--    @param t Table of units.
--    @return New value and reduced table of units.
local function simplify(val,t)
   local acc,new,res = {}, {}, val
   for k,v in pairs(t) do
      local previous = nil
      local l,r,base
      for _,a in ipairs(acc) do
         l,r,base = diff(k,a)
	 if base then previous = a; break end
      end
      if previous then
         new[previous] = new[previous] + v
	 res = res * math.pow(units.prefix[l]/units.prefix[r], v)
      else
         new[k] = v
	 acc[#acc+1] = k
      end
   end
   return res, new
end

-- ~~~~~~~~~~~~~~~~ Parsing ~~~~~~~~~~~~~~~~~~~~~~~~~~

--- Get value or evaluate expression in brackets.
--    @param str Expression.
--    @return Table of units and string rest.
units.get_expr = function (str)
   if #str == 0 then return {}, "" end
   -- check for brackets
   local expr, rest = string.match(str, '^(%b())(.*)')
   if expr then
      -- evaluate
      local res = units.get_term(string.match(expr,'%((.+)%)'))
      return res, rest
   else
      -- return table with units
      expr, rest = string.match(str, '([^%(%)%*/%^]+)(.*)')
      return {[expr]=1}, rest
   end
end

--- Evaluate expression a^b.
--    @param str Expression.
--    @return Table of units and string rest.
units.get_pow = function (str)
   local res, rest, num = units.get_expr(str)
   if string.byte(rest) == c_pow then
      -- get powert and evaluate
      num, rest = string.match(rest, '%^(-?[%d%.]+)(.*)')
      op['^'](res, tonumber(num))
   end
   return res, rest
end

--- Evaluate a * b.
--    @param str Expression.
--    @return Table of units and string rest.
units.get_term = function (str)
   local res, rest = units.get_pow(str)
   while string.byte(rest) == c_prod or string.byte(rest) == c_rat do
      -- while get * or / get terms and evaluate
      local sign, tmp
      sign, rest = string.match(rest, "([%*/])(.*)")
      tmp, rest = units.get_pow(rest)
      op[sign](res, tmp)
   end
   return res, rest
end

--- Parse units expression.
--    @param str Initial string with units.
--    @return Reduced list of units.
units.parse = function (str)
   local res = units.get_term(str)
   return reduce(res)
end

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

--- Create new unit object.
--    @param v Numerical value. Can be unit string.
--    @param u String of units. Can be omitted.
--    @return Unit object.
function units:new(v,u)
   if not u then 
      if type(v) == 'string' then v,u = 1,v else u = "" end
   end
   if not units.mem_keys[u] then
      units.mem_keys[u] = tokey(units.parse(u))
   end
   local o = {value=v, key=units.mem_keys[u]}
   setmetatable(o, self)
   return o
end

--- Create copy of the element.
--    @param u Source object.
--    @return Deep copy.
units.copy = function (u)
   local cp = units:new(u.value)
   cp.key = u.key
   return cp
end
units.about[units.copy] = {'copy(u)', 'Create copy of the element.', help.OTHER}

--- Check if 2 elements can be converted to each other.
--    <i>Private function.</i>
--    @param k1 First key.
--    @param k2 Second key.
--    @return <code>true</code> if conversation is possible.
local function iscompatible(k1,k2)
   if k1 == k2 then return true end
   if #k1 == 0 and #k2 ~= 0 then return false end
   local fk2 = string.gmatch(k2, part)
   for v1,u1 in string.gmatch(k1, part) do
      local v2,u2 = fk2()
      -- compare powers and units
      if v1 ~= v2 then return false end
      if u1 ~= u2 then 
         local l,r,base = diff(u1,u2)
	 if not (base and units.prefix[l] and units.prefix[r]) then return false end
      end
   end
   return true
end

--- Calculate value as the result of conversation.
--    <i>Private function.</i>
--    @param v Initial value.
--    @param from Initial key.
--    @param to Final key.
--    @return Result value of conversation.
local function vconvert(v, from, to)
   local f,res = string.gmatch(to, part), v
   for v1,u1 in string.gmatch(from, part) do
      local _,u2 = f()
      local l,r = diff(u1,u2)
      res = res*math.pow(units.prefix[l]/units.prefix[r], tonumber(v1))
   end
   return res
end

--- Simplify units representation.
--    @param uv Initial units object.
--    @return Replace all unit to its representation if it is possible.
units.toatom = function (uv)
   local t, res = fromkey(uv.key), uv.value
   local kold, knew = "", uv.key
   while kold ~= knew do                            -- while can be expanded
      kold = knew
      for v1,u1 in string.gmatch(knew, part) do     -- for every unit
         local left,right,base,row                  -- get common part and prefixes
         if units.rules[u1] then
	    row = u1
	    left,right,base = '','',u1
	 else
	    for k in pairs(units.rules) do
	       left,right,base = diff(k,u1)
	       if base and units.prefix[left] and units.prefix[right] then row=k; break end
	    end
	 end
	 if base then                               -- get common part
	    -- replace
	    local tmp = units.rules[row]
	    local add,num = fromkey(tmp.key), tonumber(v1)
	    t[u1] = nil
	    op['^'](add,num)
	    op['*'](t,add)
	    res = res*math.pow(tmp.value*units.prefix[right]/units.prefix[left], num)
	    res, t = simplify(res, t)
	    t = reduce(t)
	 end
      end
      knew = tokey(t)                              -- back to string
   end
   return res, knew
end

--- Convert one unit to another using internal representation.
--    @param u Initial unit object,
--    @param tokey Expected key.
--    @return Converted object of <code>nil</nil>.
local function uconvert(u, tokey)
   local res = units:new(1)
   res.key = tokey
   if iscompatible(u.key, res.key) then
      res.value = vconvert(u.value, u.key, res.key)
   else
      local v1,u1 = units.toatom(u)
      local v2,u2 = units.toatom(res)
      if iscompatible(u1,u2) then
         res.value = vconvert(v1/v2, u1, u2)
      else
         res = nil
      end
   end
   return res
end

--- Convert one units to another.
--    @param u Source unit object.
--    @param r Unit string of function f(u).
--    @return Result of conversation of <code>nil</code>.
units.convert = function (u, r)
   if type(r) == 'function' then return r(u)
   else
      local res = units:new(1, r)
      return uconvert(u, res.key)
   end
end
units.about[units.convert] = {'convert(v, units)','Convert one units to another, return new object or nil.', }

--- -a
--    @param u Unit object.
--    @return Negative value.
units.__unm = function (u)
   local res = units.copy(u)
   res.value = -res.value
   return res
end

--- a + b
--    @param a First unit object.
--    @param b Second unit object.
--    @return Sum of objects with the same units.
units.__add = function (a,b)
   a,b = args(a,b)
   local tmp = assert(uconvert(b,a.key), "Different units!")
   local res = units.copy(a)
   res.value = res.value + tmp.value
   return res
end

--- a - b
--    @param a First unit object.
--    @param b Second unit object.
--    @return Subtraction of objects with the same units.
units.__sub = function (a,b)
   a,b = args(a,b)
   local tmp = assert(uconvert(b,a.key), "Different units!")
   local res = units.copy(a)
   res.value = res.value - tmp.value
   return res
end

-- a * b
--    @param a First unit object.
--    @param b Second unit object.
--    @return Product of unit objects.
units.__mul = function (a,b)
   a,b = args(a,b)
   local ta, tb = fromkey(a.key), fromkey(b.key)
   op['*'](ta,tb)
   ta = reduce(ta)
   if not next(ta) then return a.value*b.value end
   local res = units:new(a.value*b.value)
   res.key = tokey(ta)
   return res.key == '' and res.value or res
end

--- a / b
--    @param a First unit object.
--    @param b Second unit object.
--    @return Ratio of unit objects.
units.__div = function (a,b)
   a,b = args(a,b)
   local ta, tb = fromkey(a.key), fromkey(b.key)
   op['/'](ta,tb)
   ta = reduce(ta)
   if not next(ta) then return a.value/b.value end
   local res = units:new(a.value/b.value)
   res.key = tokey(ta)
   return res.key == '' and res.value or res
end

-- a ^ b
--    @param a Unit object.
--    @param b Number.
--    @return Power.
units.__pow = function (a,b)
   assert(not isunits(b), "Wrong power!")
   local res = isunits(a) and units.copy(a) or units:new(a)
   local ta = fromkey(res.key)
   op['^'](ta,b)
   res.value = math.pow(res.value, b)
   res.key = tokey(ta)
   return res
end

--- Check equality of float point numbers.
--    <i>Private function.</i>
--    @param a First unit object.
--    @param b Second unit object.
--    @return <code>true</code> if the objects are equal.
local function equal(a,b)
   return math.abs(a-b) <= 1e-3*math.abs(a)
end

--- a == b
--    @param a First unit object.
--    @param b Second unit object.
--    @return <code>true</code> if the objects are equal.
units.__eq = function (a,b)
   if not (isunits(a) and isunits(b)) then return false end
   if a.key == b.key then return equal(a.value, b.value) end
   local tmp = uconvert(b, a.key)
   if tmp == nil then return false end
   return equal(a.value, tmp.value)
end

--- a < b
--    @param a First unit object.
--    @param b Second unit object.
--    @return Result of comparison.
units.__lt = function (a,b)
   assert(isunits(a) and isunits(b), 'Not compatible!')
   if a.key == b.key then return a.value < b.value end
   local tmp = assert(uconvert(b, a.key), 'Not compatible!')
   return a.value < b.value 
end

--- a <= b
--    @param a First unit object.
--    @param b Second unit object.
--    @return Result of comparison.
units.__le = function (a,b)
   assert(isunits(a) and isunits(b), 'Not compatible!')
   if a.key == b.key then return a.value <= b.value end
   local tmp = assert(uconvert(b, a.key), 'Not compatible!')
   return a.value <= b.value 
end

--- Prepare units for print operation.
--    <i>Private function.</i>
--    @param str Units key.
--    @return Traditional representation.
local function collect(str)
   local t = {}
   for v,k in string.gmatch(str, part) do 
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
--    @param u Unit object.
--    @return Unit value in traditional form.
units.__tostring = function (u)
   local num, denom = string.match(u.key, '([^%-]*)(.*)')
   num = #num > 0 and collect(num) or '1'
   denom = #denom > 0 and collect(denom) or ''

   if string.find(denom, '*') then denom = '('..denom..')' end
   if #denom > 0 then num = num..'/' end

   return u.value .. ' ' .. num .. denom
end

--- #u
--    @param u Unit object.
--    @return Current value.
units.__len = function (u) return u.value end

--- Convert using v['new_units'] notation.
--    @param t Initial unit object.
--    @param k New Units.
--    @return Result of conversation of <code>nil</code>.
units.__index = function (t,k)
   if units[k] then
      return units[k]
   else
      local v = units.convert(t,k)
      return v and v.value or nil
   end
end

--- Reduce elements with common base part.
--    @param u Unit object.
--    @return Result of simplification.
units.simp = function (u)
   local val, t = simplify(u.value, fromkey(u.key))
   t = reduce(t)
   if #t > 0 then
      local res = units:new(val)
      res.key = tokey(t)
      return res
   else return val end
end


--- Add new rule for unit transformation.
--    @param u String with units.
--    @param rule Result represented as unit object.
units.add = function (u, rule)
   assert(isunits(rule), 'Units object is expected!')
   units.rules[u] = rule
end
units.about[units.add] = {'add(unit,rule)', 'Add new rule for conversation.', }


-- simplify constructor call
setmetatable(units, {__call = function (self,v,u) return units:new(v,u) end })
units.Unit = 'Unit'
units.about[units.Unit] = {'Unit(v[,u])', 'Create new elements with units.', help.NEW}

--- Unit object serialization.
--    @param obj Unit object.
--    @return String, suitable for exchange.
units.serialize = function (obj)
   local s = {}
   s[#s+1] = 'value='..obj.value
   s[#s+1] = string.format("key='%s'", obj.key)
   s[#s+1] = "metatablename='Unit'"
   s[#s+1] = "modulename='units'"
   return string.format("{%s}", table.concat(s, ','))
end
units.about[units.serialize] = {"serialize(obj)", "Save internal representation of units object.", help.OTHER}

-- free memory if need
if not lc_version then units.about = nil end

return units

