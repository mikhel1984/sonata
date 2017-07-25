

local units = {}
units.__index = units

units.type = 'units'

local coeff = {
p = 1e-12,
n = 1e-9,
u = 1e-6,
m = 1e-3,
s = 1e-2,
[""] = 1,
k = 1e+3,
M = 1e+6,
G = 1e+9,
}

local part = '([^%a]+)(%a+)'  

local function isunits(t) return type(t) == 'table' and t.type == units.type end

local function tokey(t)
   local s = {}
   for k,v in pairs(t) do s[#s+1] = {v,k} end
   table.sort(s, function (p1, p2) 
                    return p1[1] > p2[1] or (p1[1] == p2[1] and string.reverse(p1[2]) < string.reverse(p2[2])) 
              end)
   for i,v in ipairs(s) do s[i] = v[1] .. v[2] end
   return table.concat(s)
end

local function fromkey(s)
   local t = {}
   for v,k in string.gmatch(s, part) do t[k] = tonumber(v) end
   return t
end

local function diff(s1, s2)
   local first, base, second = string.match(s1..'|'..s2, '^(.-)(.+)|(.-)%2$')
   if base then
      return first, second, base
   else
      return s1, s2, nil
   end
end

local op = {
['*'] = function(u,u1) for k,v in pairs(u1) do u[k] = (u[k] or 0) + v end end,
['/'] = function(u,u1) for k,v in pairs(u1) do u[k] = (u[k] or 0) - v end end,
['^'] = function(u,n) for k,v in pairs(u) do u[k] = v*n end end
}

local function reduce(t)
   local tmp = {}
   for k,v in pairs(t) do
      if v == 0 or tonumber(k) then tmp[#tmp+1] = k end
   end
   for _,k in ipairs(tmp) do t[k] = nil end
   return t
end

local c_prod, c_rat, c_pow = string.byte('*'), string.byte('/'), string.byte('^')

units.get_expr = function (str)
   local expr, rest = string.match(str, '^(%b())(.*)')
   if expr then
      local res = units.get_term(string.match(expr,'%((.+)%)'))
      return res, rest
   else
      expr, rest = string.match(str, '([^%(%)%*/%^]+)(.*)')
      return {[expr]=1}, rest
   end
end

units.get_pow = function (str)
   local res, rest = units.get_expr(str)
   if string.byte(rest) == c_pow then
      local num
      num, rest = string.match(rest, '%^(-?[%d%.]+)(.*)')
      op['^'](res, tonumber(num))
   end
   return res, rest
end

units.get_term = function (str)
   local res, rest = units.get_pow(str)
   while string.byte(rest) == c_prod or string.byte(rest) == c_rat do
      local sign, tmp
      sign, rest = string.match(rest, "([%*/])(.*)")
      tmp, rest = units.get_pow(rest)
      op[sign](res, tmp)
   end
   return res, rest
end

units.parse = function (str)
   local res = units.get_term(str)
   return reduce(res)
end

function units:new(v,u)
   local tmp = u and units.parse(u) or {}
   local o = {value=v, key=tokey(tmp)}
   setmetatable(o, self)
   return o
end

units.copy = function (u)
   local cp = units:new(u.value)
   cp.key = u.key
   --for k,v in pairs(u.units) do cp.units[k] = v end
   return cp
end

local function iscompatible(k1,k2)
   if k1 == k2 then return true end
   local fk2 = string.gmatch(k2, part)
   for v1,u1 in string.gmatch(k1, part) do
      local v2,u2 = fk2()
      if v1 ~= v2 then return false end
      if u1 ~= u2 then 
         local l,r,base = diff(u1,u2)
	 if not (base and coeff[l] and coeff[r]) then return false end
      end
   end
   return true
end

local function vconvert(v, from, to)
   local f,res = string.gmatch(to, part), v
   for v1,u1 in string.gmatch(from, part) do
      local _,u2 = f()
      local l,r = diff(u1,u2)
      res = res * coeff[l]/coeff[r]
   end
   return res
end

units.convert = function (u, str)
   local res = units:new(1, str)
end

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

units.__tostring = function (u)
   local num, denom = string.match(u.key, '([^%-]*)(.*)')
   num = #num > 0 and collect(num) or '1'
   denom = #denom > 0 and collect(denom) or ''

   if string.find(denom, '*') then denom = '('..denom..')' end
   if #denom > 0 then num = num..'/' end

   return u.value .. ' ' .. num .. denom
end

units.rules = {
}

------------------------------
--          tests
-----------------------------
--p = units.parse('kg*km/(mf*Mf)^2')
--for k,v in pairs(p) do print(k,v) end
--[[
a = units:new(2, 'm*k/(f*s)^2')
print(a)
p = tokey(a.units)
print(p)
for s in string.gmatch(p, '(%a+[^%a]+)') do print(s) end
b = fromkey(p)
for k,v in pairs(b) do print(k,v) end
]]
--print(tokey(p))
--q = tokey(p)
--r = fromkey(q)
--for k,v in pairs(r) do print(k,v) end
--a = units:new(2, 'm*k/(f*h^2)^2')
--print(a)
--print(diff('mm','km'))
print(vconvert(1, '1mm','1km'))
