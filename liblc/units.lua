

local units = {}
units.__index = units

units.type = 'units'

local coeff = {
p = 1e-12,
n = 1e-9,
u = 1e-6,
m = 1e-3,
s = 1e-2,
k = 1e+3,
M = 1e+6,
G = 1e+9,
}

local function isunits(t) return type(t) == 'table' and t.type == units.type end

local function key(t)
   local s = {}
   for k,v in pairs(t) do s[#s+1] = k .. tostring(v) end
   table.sort(s)
   return table.concat(s)
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
   local o = {value=v}
   o.units = u and units.parse(u) or {}
   setmetatable(o, self)
   return o
end

units.copy = function (u)
   local cp = units:new(u.value)
   for k,v in pairs(u.units) do cp.units[k] = v end
   return cp
end

units.__tostring = function (u)
   local tmp = {}
   for k,v in pairs(u.units) do
      tmp[v] = tmp[v] or {}
      table.insert(tmp[v],k)
   end
   local num, denom = {}, {}
   for n in pairs(tmp) do
      local str = table.concat(tmp[n], '*')
      if n > 0 then 
         table.insert(num, (n == 1 and str or string.format('(%s)^%d',str,n)))
      else
         table.insert(denom, (n == -1 and str or string.format('(%s)^%d',str,-n)))
      end
   end
   num = #num > 0 and table.concat(num, '*') or '1'
   local strdenom = #denom > 0 and table.concat(denom, '*') or ''
   denom = #denom > 1 and '('..strdenom..')' or strdenom
   if #denom > 0 then denom = '/' .. denom end
   return u.value .. ' ' .. num .. denom
end

units.rules = {
}

------------------------------
--          tests
-----------------------------
--p = units.parse('m*k/(f*s)^2')
--for k,v in pairs(p) do print(k,v) end
a = units:new(2, 'm*k/(f*s)^2')
b = a:copy()
--for k,v in pairs(a.units) do print(k,v) end
print(key(b.units))
