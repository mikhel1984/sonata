------------  units.lua ----------------
--
-- Operations and conversations with units.
--
-- This file is a part of liblc collection. 
-- Stanislav Mikhel, 2017.
----------------------------------------

-------------------- Tests -------------------
--[[!!
Unit = require 'liblc.units'

a = Unit(1,'m/s')
ans = a['km/h']                    --> 3.6

ans = #a                           --> 1

cp = a:copy() 
ans = cp                           --> Unit(1,'m/s')

b = a:convert('km/h')
ans = b                            --> Unit(3.6, 'km/h')

b = 3 * b
ans = a + b                        --> Unit(4, 'm/s')

ans = b - a                        --> Unit(2, 'm/s')

ans = a * b                        --> Unit(3, 'm^2/s^2')

ans = b / a                        --> 3

ans = (a < b)                      --> true

ans = b ^ 3                        --> Unit(27, 'm^3/s^3')

Unit.add('snake', Unit(48, 'parrot'))
c = Unit(2,'snake')
ans = c['parrot']                  --> 96

ans = c['ksnake']                  --> 0.002

print(a)
]]
---------------------------------------------

local units = {}

units.type = 'units'

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
units.about = help:new("Operations and conversations according the units")

-- prefix list
units.prefix = {
p = 1e-12,
n = 1e-9,
u = 1e-6,
m = 1e-3,
c = 1e-2,
[""] = 1,
k = 1e+3,
M = 1e+6,
G = 1e+9,
}
units.about[units.prefix] = {'prefix', 'Table of possible prefixes for units.', help.OTHER}

local part = '([^%a]+)(%a+)'     -- highlight value and unit from string

-- save some results
units.mem_parts = {}
units.mem_keys = {}

-- check for type
local function isunits(t) return type(t) == 'table' and t.type == units.type end

-- comporation for unit sorting
local function comp (p1,p2) 
   return p1[1] > p2[1] or (p1[1] == p2[1] and string.reverse(p1[2]) < string.reverse(p2[2])) 
end

-- represent table of units as string
local function tokey(t)
   local s = {}
   for k,v in pairs(t) do s[#s+1] = {v,k} end
   table.sort(s, comp)
   for i,v in ipairs(s) do s[i] = v[1] .. v[2] end
   return table.concat(s)
end

-- convert internal string representation to the table
local function fromkey(s)
   local t = {}
   for v,k in string.gmatch(s, part) do t[k] = tonumber(v) end
   return t
end

-- get common part and difference between 2 strings
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

-- prepare arguments
local function args(a,b)
   a = isunits(a) and a or units:new(a)
   b = isunits(b) and b or units:new(b)
   return a,b
end

-- check if table is empty
local function isempty(t)
   for k in pairs(t) do return false end
   return true
end

-- operations with tables of units
local op = {
['*'] = function(u,u1) for k,v in pairs(u1) do u[k] = (u[k] or 0) + v end end,
['/'] = function(u,u1) for k,v in pairs(u1) do u[k] = (u[k] or 0) - v end end,
['^'] = function(u,n) for k,v in pairs(u) do u[k] = v*n end end
}

-- remove empty elements from table of units
local function reduce(t)
   local tmp = {}
   for k,v in pairs(t) do
      if not(v == 0 or tonumber(k)) then tmp[k] = v end
   end
   return tmp
end

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

-- possible signs
local c_prod, c_rat, c_pow = string.byte('*'), string.byte('/'), string.byte('^')

-- get value or evaluate expression in brackets
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

-- evaluate expression a^b
units.get_pow = function (str)
   local res, rest, num = units.get_expr(str)
   if string.byte(rest) == c_pow then
      -- get powert and evaluate
      num, rest = string.match(rest, '%^(-?[%d%.]+)(.*)')
      op['^'](res, tonumber(num))
   end
   return res, rest
end

-- evaluate a * b
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

-- parse units expression
units.parse = function (str)
   local res = units.get_term(str)
   return reduce(res)
end

-- create new element
function units:new(v,u)
   u = u or ""
   if not units.mem_keys[u] then
      units.mem_keys[u] = tokey(units.parse(u))
   end
   local o = {value=v, key=units.mem_keys[u]}
   setmetatable(o, self)
   return o
end

-- create copy of the element
units.copy = function (u)
   local cp = units:new(u.value)
   cp.key = u.key
   return cp
end
units.about[units.copy] = {'copy(u)', 'Create copy of the element.', help.OTHER}

-- check if 2 elements can be converted to each other
local function iscompatible(k1,k2)
   if k1 == k2 then return true end
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

-- calculate value as the result of conversation
local function vconvert(v, from, to)
   local f,res = string.gmatch(to, part), v
   for v1,u1 in string.gmatch(from, part) do
      local _,u2 = f()
      local l,r = diff(u1,u2)
      res = res*math.pow(units.prefix[l]/units.prefix[r], tonumber(v1))
   end
   return res
end

-- simplify units representation
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

-- convert one unit to another using internal representation, return nil if can't
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

-- convert one units to another, nil if can't
units.convert = function (u, r)
   if type(r) == 'function' then return r(u)
   else
      local res = units:new(1, r)
      return uconvert(u, res.key)
   end
end
units.about[units.convert] = {'convert(v, units)','Convert one units to another, return new object or nil.', help.BASE}

-- -a
units.__unm = function (u)
   local res = units.copy(u)
   res.value = -res.value
   return res
end

-- a + b
units.__add = function (a,b)
   a,b = args(a,b)
   local tmp = assert(uconvert(b,a.key), "Different units!")
   local res = units.copy(a)
   res.value = res.value + tmp.value
   return res
end

-- a - b
units.__sub = function (a,b)
   a,b = args(a,b)
   local tmp = assert(uconvert(b,a.key), "Different units!")
   local res = units.copy(a)
   res.value = res.value - tmp.value
   return res
end

-- a * b
units.__mul = function (a,b)
   a,b = args(a,b)
   local ta, tb = fromkey(a.key), fromkey(b.key)
   op['*'](ta,tb)
   ta = reduce(ta)
   if isempty(ta) then return a.value*b.value end
   local res = units:new(a.value*b.value)
   res.key = tokey(ta)
   return res
end

-- a / b
units.__div = function (a,b)
   a,b = args(a,b)
   local ta, tb = fromkey(a.key), fromkey(b.key)
   op['/'](ta,tb)
   ta = reduce(ta)
   if isempty(ta) then return a.value/b.value end
   local res = units:new(a.value/b.value)
   res.key = tokey(ta)
   return res
end

-- a ^ b
units.__pow = function (a,b)
   assert(not isunits(b), "Wrong power!")
   local res = isunits(a) and units.copy(a) or units:new(a)
   local ta = fromkey(res.key)
   op['^'](ta,b)
   res.value = math.pow(res.value, b)
   res.key = tokey(ta)
   return res
end

local function equal(a,b)
   return math.abs(a-b) <= 1e-3*math.abs(a)
end

-- a == b
units.__eq = function (a,b)
   if not (isunits(a) and isunits(b)) then return false end
   if a.key == b.key then return equal(a.value, b.value) end
   local tmp = uconvert(b, a.key)
   if tmp == nil then return false end
   return equal(a.value, tmp.value)
end

-- a < b
units.__lt = function (a,b)
   assert(isunits(a) and isunits(b), 'Not compatible!')
   if a.key == b.key then return a.value < b.value end
   local tmp = assert(uconvert(b, a.key), 'Not compatible!')
   return a.value < b.value 
end

-- a <= b
units.__le = function (a,b)
   assert(isunits(a) and isunits(b), 'Not compatible!')
   if a.key == b.key then return a.value <= b.value end
   local tmp = assert(uconvert(b, a.key), 'Not compatible!')
   return a.value <= b.value 
end

-- prepare units for print operation
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

-- string representation
units.__tostring = function (u)
   local num, denom = string.match(u.key, '([^%-]*)(.*)')
   num = #num > 0 and collect(num) or '1'
   denom = #denom > 0 and collect(denom) or ''

   if string.find(denom, '*') then denom = '('..denom..')' end
   if #denom > 0 then num = num..'/' end

   return u.value .. ' ' .. num .. denom
end

-- #u
units.__len = function (u) return u.value end

-- convert using v['new_units'] notation
units.__index = function (t,k)
   if units[k] then
      return units[k]
   else
      local v = units.convert(t,k)
      return v and v.value or nil
   end
end

-- rules for unit conversation
units.rules = {
}

-- add new rule
units.add = function (u, rule)
   assert(isunits(rule), 'Units object is expected!')
   units.rules[u] = rule
end
units.about[units.add] = {'add(unit,rule)', 'Add new rule for conversation.', help.BASE}

-- some rules
units.add('h', units:new(60,'min'))
units.add('min', units:new(60,'s'))

-- simplify constructor call
setmetatable(units, {__call = function (self,v,u) return units:new(v,u) end })
units.Unit = 'Unit'
units.about[units.Unit] = {'Unit(v[,u])', 'Create new elements with units.', help.NEW}

units.serialize = function (obj)
   local s = {}
   s[#s+1] = 'value='..obj.value
   s[#s+1] = string.format("key='%s'", obj.key)
   s[#s+1] = "metatablename='Unit'"
   s[#s+1] = "modulename='units'"
   return string.format("{%s}", table.concat(s, ','))
end
units.about[units.serialize] = {"serialize(obj)", "Save internal representation of units object.", help.OTHER}

return units

