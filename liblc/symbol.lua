--[[       liblc/symbol.lua

--- Symbolical calculations.
--  @author My Name

           module 'symbol'
--]]

--------------- Tests ------------
-- Define here your tests, save results to 'ans', use --> for equality and --~ for estimation.
--[[!!
Sym = require 'liblc.symbol'

-- example
a = Sym()
ans = a.type                   --> 'symbol'
]]

local symbol = {}
--------
local parser = {}
-- eval
-- copy

parser.isnumber = function(s,n) return string.find(s,'^%d',n) ~= nil end

parser.isname = function(s,n) return string.find(s,'^%a',n) ~= nil end

parser.get_number = function(s,n)
   local p,q = string.find(s,'%d+%.?%d*',n)
   local p2,q2 = string.find(s,'[eE][-+]?%d+',q+1)
   if p2 then q = q2 end
   return string.sub(s,p,q), q+1
end

parser.get_word = function(s,n)
   local p,q = string.find(s,'%a[%w_%.]*',n)
   return string.sub(s,p,q), q+1
end

parser.get_sign = function(s,n)
   return string.sub(s,n,n), n+1
end

parser.noblank = function(s,n)
   n = string.find(s,'[^%s%c]',n)
   return n and n or #s+1
end

parser.Sum = function(s,n)
   --print('sum',n)
   n = parser.noblank(s,n)
   local res,res2
   res,n = parser.Prod(s,n)
   while n <= #s do
      n = parser.noblank(s,n)
      local op = string.sub(s,n,n)
      if op == '+' or op == '-' then
         res2,n = parser.Prod(s,n+1)
	 res = symbol._op(op,res,res2)
      else break end
   end
   return res, n
end

parser.Prod = function (s,n)
   --print('prod',n)
   n = parser.noblank(s,n)
   local res,res2
   res,n = parser.Pow(s,n)
   while n <= #s do
      n = parser.noblank(s,n)
      local op = string.sub(s,n,n)
      if op == '*' or op == '/' then
         res2,n = parser.Pow(s,n+1)
	 res = symbol._op(op,res,res2)
      else break end
   end
   return res, n
end

parser.Pow = function (s,n)
   --print('pow',n)
   n = parser.noblank(s,n)
   local res,res2
   res,n = parser.Prim(s,n)
   local op = string.sub(s,n,n)
   if op == '^' then
      res2,n = parser.Prim(s,n+1)
      res = symbol._op(op,res,res2)
   end
   return res, n
end

parser.Args = function (s,n)
   --print('args',n)
   n = parser.noblank(s,n)
   if string.sub(s,n,n) ~= '(' then return nil end
   local t = {}
   while true do
      t[#t+1],n = parser.Sum(s,n+1)
      n = parser.noblank(s,n)
      local p = string.sub(s,n,n)
      if p ~= ',' then break end
   end
   return t, n+1
end

parser.Prim = function (s,n)
   --print('prim',n)
   n = parser.noblank(s,n)
   local p,res = string.sub(s,n,n)
   if parser.isnumber(s,n) then
      res,n = parser.get_number(s,n)
      res = symbol._num(res)
   elseif parser.isname(s,n) then
      res,n = parser.get_word(s,n)
      local list,n2 = parser.Args(s,n)
      if list then
	 res = symbol._fn(res,list)
	 n = n2
      else
	 res = symbol._var(res)
      end
   elseif p == '(' then
      res,n = parser.Sum(s,n+1)
      n = parser.noblank(s,n)
      if string.sub(s,n,n) ~= ')' then
         error(') is expected, got '..string.sub(s,n,n))
      end
   elseif p == '-' then
      -- negative number, do nothing
   else
      error('unexpected symbol '..p)
   end
   return res, n
end

-------------------------------------
symbol.__index = symbol

-- mark
symbol.type = 'symbol'
symbol.issymbol = true
local function issymbol(t) return type(t)=='table' and t.issymbol end

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
symbol.about = help:new("Symbolical calculations.")

--- Constructor example
--    @param t Some value.
--    @return New object of symbol.
function symbol:new(t)
   local o = t or {}
   setmetatable(o,self)
   return o
end

symbol._op = function (op,s1,s2) 
   return symbol:new {op=op, left=s1, right=s2, str=symbol._strOp} 
end

symbol._num = function (v) return symbol:new {value=v, str=symbol._strNum} end

symbol._var = function (n) return symbol:new {name=n, str=symbol._strVar} end

symbol._fn = function (f,a) return symbol:new {fn=f, args=a, str=symbol._strFn} end

symbol.parse = function (s)
   local val = parser.Sum(s,1)
   return val
end

function symbol._strOp (t)
   local left = t.left and t.left:str() or ""
   local right = t.right:str()
   if t.op == '*' or t.op == '/' then
      local op1,op2 = (t.left and t.left.op), t.right.op
      if op1=='+' or op1=='-' then left = string.format('(%s)',left) end
      if op2=='+' or op2=='-' then right = string.format('(%s)',right) end
   elseif t.op == '^' then
      if t.left.op then left = string.format('(%s)',left) end
      if t.right.op then right = string.format('(%s)',right) end
   end
   return string.format('%s%s%s', left, t.op, right)
end

function symbol._strFn (t)
   local a = {}
   for _,v in ipairs(t.args) do a[#a+1] = v:str() end
   return string.format('%s(%s)', t.fn, table.concat(a,','))
end

function symbol._strVar (t) return t.name end

function symbol._strNum (t) return tostring(t.value) end

symbol.__tostring = function (s) return s:str() end

--[[
-- simplify constructor call
setmetatable(symbol, {__call = function (self,v) return symbol:new(v) end})
symbol.Sym = 'Sym'
symbol.about[symbol.Sym] = {"Sym(t)", "Create new symbol.", help.NEW}

--- Method example
--   It is good idea to define method for the copy creation.
--   @param t Initial object.
--   @return Copy of the object.
symbol.copy = function (t)
   -- some logic
   return symbol:new(argument)
end
symbol.about[symbol.copy] = {"copy(t)", "Create a copy of the object.", help.BASE}
]]

-- free memory in case of standalone usage
--if not lc_version then symbol.about = nil end

--return symbol

a = symbol.parse('name.fn(b+c, q )^( a+ d * f)')
print(a )
