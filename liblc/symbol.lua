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

local function isnumber(s,n) return string.find(s,'^%d',n) ~= nil end

local function isname(s,n) return string.find(s,'^%a',n) ~= nil end

local function get_number(s,n)
   local p,q = string.find(s,'%d+%.?%d*',n)
   local p2,q2 = string.find(s,'[eE][-+]?%d+',q+1)
   if p2 then q = q2 end
   return string.sub(s,p,q), q+1
end

local function get_word(s,n)
   local p,q = string.find(s,'%a[%w_]*',n)
   return string.sub(s,p,q), q+1
end

local function get_sign(s,n)
   return string.sub(s,n,n+1), n+1
end

local function noblank(s,n)
   return string.find(s,'[^%s]',n)
end

local function parseSum (s,n)
   n = noblank(s,n)
   local res,res2
   res,n = parseProd(s,n)
   while n <= #s do
      n = noblank(s,n)
      local op = string.sub(s,n,1)
      if op == '+' or op == '-' then
         res2,n = parseProd(s,n+1)
         res = symbol:new {op=op, left=res, right=res2}
      else break end
   end
   return res, n
end

local function parseFactor(s,n)
   n = noblank(s,n)
   local res,res2
   res,n = parsePow(s,n)
   while n <= #s do
      n = noblank(s,n)
      local op = string.sub(s,n,1)
      if op == '*' or '/' then
         res2,n = parsePow(s,n)
         res = symbol:new {op=op, left=res, right=res2}
      else break end
   end
   return res, n
end

local function parsePow(s,n)
   n = noblank(s,n)
   local res,res2
   res,n = parsePrim(s,n)
   local op = string.sub(s,n,1)
   if op == '^' then
      res2 = parsePrim(s,n+1)
      res2,n = parsePrim(s,n)
      res = symbol:new {op=op, left=res, right=res2}
   end
   return res, n
end

local function parseArgs(s,n)
   n = noblank(s,n)
   if string.sub(s,n,1) ~= '(' then return nil end
   local t = {}
   n = n+1
   while true do
      t[#t+1] = parseSum(s,n)
      n = noblank(s,n)
      local p = string.sub(s,n,1)
      if p == ')' then break
      elseif p == ',' then 
         n = n+1
      end
   end
   return t, n+1
end

local function parsePrim(s,n)
   n = noblank(s,n)
   local res
   if isnumber(s,n) then
      res,n = get_number(s,n)
      res = symbol:new {value=res}
   elseif isname(s,n) then
      res,n = get_word(s,n)
      local list,n2 = parseArgs(s,n)
      if list then
         res = symbol:new {fn=res, args=list}
	 n = n2
      else
         res = symbol:new {name=res}
      end
   elseif string.sub(s,n,1) == '(' then
      res,n = parseSum(s,n)
      n = noblank(s,n)
      if string.sub(s,n,1) ~= ')' then
         error('unexpected symbol '..string.sub(s,n,1))
      end
   else
      error('unexpected symbol '..string.sub(s,n,1))
   end
   return res, n
end

---------------------------------
-- @class table
-- @name symbol
-- @field about Description of functions.
local symbol = {}
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

print(get_word('a2b_c',1))
