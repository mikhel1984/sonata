--[[       liblc/symbol.lua

--- Symbolical calculations.
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

           module 'symbol'
--]]


--[[!!
Sym = require 'liblc.symbol'

-- parse equation
s1 = Sym('fn(a+b, c)^(3+c/2*d)')
-- show as string
print(s1)

-- make copy
ans = s1:copy()                       --> s1

-- evaluation
s2 = Sym('a-b*sin(c)')
ans = s2:eval({a=1,b=2,c=math.pi/2,sin=math.sin})  --> -1

-- partial definition
ans = s2:eval({a=1,c=0})              --> Sym('1-b*sin(0)')

-- use global variables
a,b,c = 1,2,0
sin = math.sin
ans = s2:eval()                       --> 1

-- s1 + s2
s1 = Sym('a+b')
s2 = Sym('a-b')
s3 = s1 + s2
ans = s3:eval()                       --> 2

-- s1 - s2
s3 = s1 - s2
ans = s3:eval()                       --> 4

-- s1 * s2
s3 = s1 * s2                          
ans = s3:eval()                       --> -3

-- s1 / s2
s3 = s1 / s2
ans = s3:eval()                       --> -3

-- s2 ^ s1
s3 = s2 ^ s1
ans = s3:eval()                       --> 1

-- -s2
s3 = -s2
ans = s3:eval()                       --> 1

]]

local OPERATION, VARIABLE, NUMBER, FUNCTION = 1, 2, 3, 4

-- 
local symbol = {}
-- mark
symbol.type = 'symbol'
symbol.issymbol = true
local function issymbol(t) return type(t)=='table' and t.issymbol end

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
symbol.about = help:new("Symbolical calculations.")

local function args(a,b)
   a = issymbol(a) and a or symbol._num(a)
   b = issymbol(b) and b or symbol._num(b)
   return a,b
end

-- collect addidional libs
symbol._libs = {}
-- add functions for symbol processing
symbol._add_lib = function (self, s)
   local lib = require(s)
   lib.root = self
   table.insert(symbol._libs, lib)
end
-- basic simplifications
--symbol:_add_lib('liblc.sym_base')

-- String parser (private class)
local parser = {}
-- check for numbers
parser.isnumber = function(s,n) return string.find(s,'^%d',n) ~= nil end
-- check for literals
parser.isname = function(s,n) return string.find(s,'^%a',n) ~= nil end
-- extract number in different forms of representations
parser.get_number = function(s,n)
   local p,q = string.find(s,'%d+%.?%d*',n)
   local p2,q2 = string.find(s,'[eE][-+]?%d+',q+1)
   if p2 then q = q2 end
   return string.sub(s,p,q), q+1
end
-- get literal
parser.get_word = function(s,n)
   local p,q = string.find(s,'%a[%w_%.]*',n)
   return string.sub(s,p,q), q+1
end
-- remove white space
parser.noblank = function(s,n)
   n = string.find(s,'[^%s%c]',n)
   return n and n or #s+1
end
-- parse summation
parser.Sum = function(s,n)
   --print('sum',n)
   n = parser.noblank(s,n)
   local res,res2
   res,n = parser.Prod(s,n)
   res = res or symbol.NULL
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
-- parse product
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
-- parse power
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
-- parse function arguments
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
-- parse atoms
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
      n = n+1
   elseif p == '-' then
      -- negative number, do nothing
   else
      error('unexpected symbol '..p)
   end
   return res, n
end

-----------------------------------
-- Simple hash calculator
local hash = {
-- function representation
testfn = function (nm,t) 
   for i = 1,#t do nm = nm + i*t[i] end 
   return nm
end,
-- get hash of find new
find = function (nm,h) 
   if not h[nm] then h[nm] = math.random(65535)*1.0 end
   return h[nm]
end,
}
-- apply for variable
hash.var = function (s,h) 
   s.hash = hash.find(s.name,h)
   return s.hash
end
-- apply for number
hash.num = function (s,h)
   s.hash = s.value*1.0
   return s.hash
end
-- apply for operation
hash.op = function (s,h)
   s.hash = symbol[s.op].eval(s.left:doHash(h), s.right:doHash(h))   
   return s.hash
end
-- apply for function
hash.fn = function (s,h)
   local t = {}
   for i = 1,#s.args do t[#t+1] = s.args[i]:doHash(h) end
   s.hash = hash.testfn(hash.find(s.fn,h), t)
   return s.hash
end 
-- fund function in libraries
symbol._get_lib_for = function (key)
   for _,v in ipairs(symbol._libs) do
      if v[key] then return v[key] end
   end
   return nil
end

-- operations
symbol['+'] = {eval=function (a,b) return a+b end, level=0, commutative=true}
symbol['-'] = {eval=function (a,b) return a-b end, level=0, commutative=false}
symbol['*'] = {eval=function (a,b) return a*b end, level=1, commutative=true}
symbol['/'] = {eval=function (a,b) return a/b end, level=1, commutative=false}
symbol['^'] = {eval=function (a,b) return a^b end, level=2, commutative=false}


--- Constructor example
--    @param t Some value.
--    @return New object of symbol.
function symbol:new(t)
   return setmetatable(t,self)
end

-- local constructors
symbol._op = function (op,s1,s2) return symbol:new {cls=OPERATION, op=op, left=s1, right=s2, hash=0} end

symbol._num = function (v) return symbol:new {cls=NUMBER, value=v, hash=0} end

symbol._var = function (n) return symbol:new {cls=VARIABLE, name=n, hash=0} end

symbol._fn = function (f,a) return symbol:new {cls=FUNCTION, fn=f, args=a, hash=0} end
-- null value for unar minus
symbol.NULL = symbol._num(0)
symbol.NULL.cpy = function () return symbol.NULL end
symbol.NULL.str = function () return "" end

symbol.ZERO = symbol._num(0)
symbol.ONE = symbol._num(1)

-- do parsing
symbol.parse = function (s)
   local val = parser.Sum(s,1)
   return val
end
-- find value
symbol.eval = function (s,env) 
   env = env or _ENV
   return s:ev(env) 
end
-- a+b
symbol.__add = function (a,b)
   a,b = args(a,b)
   return symbol._op('+',a,b)
end
-- a-b
symbol.__sub = function (a,b)
   a,b = args(a,b)
   return symbol._op('-',a,b)
end
-- a*b
symbol.__mul = function (a,b)
   a,b = args(a,b)
   return symbol._op('*',a,b)
end
-- a/b
symbol.__div = function (a,b)
   a,b = args(a,b)
   return symbol._op('/',a,b)
end
-- a^b
symbol.__pow = function (a,b)
   a,b = args(a,b)
   return symbol._op('^',a,b)
end
-- -a
symbol.__unm = function (a)
   a = issymbol(a) and a or symbol._num(a)
   return symbol._op('-',symbol.NULL,a)
end
-- string representation
symbol.__tostring = function (s) return s:str() end
-- check equality
symbol.__eq = function (s1,s2)
   return issymbol(s1) and issymbol(s2) and s1:equal(s2)
end

-- simplify constructor call
setmetatable(symbol, {__call = function (self,v) return symbol.parse(v) end})
symbol.Sym = 'Sym'
symbol.about[symbol.Sym] = {"Sym(t)", "Create new symbol.", help.NEW}

--   @param t Initial object.
--   @return Copy of the object.
symbol.copy = function (t)
   return t:cpy()
end
symbol.about[symbol.copy] = {"copy(t)", "Create a copy of the object.", help.BASE}

-- operation to string
symbol._strOp = function (t)
   local left = t.left:str() 
   local right = t.right:str()
   local op1,op2 = t.left.op, t.right.op
   if t.op == '*' then
      if op1=='+' or op1=='-' then left = string.format('(%s)',left) end
      if op2=='+' or op2=='-' then right = string.format('(%s)',right) end
   elseif t.op == '^' or t.op == '/' then
      if op1 then left = string.format('(%s)',left) end
      if op2 then right = string.format('(%s)',right) end
   elseif t.op == '-' then
      if op2 then right = string.format('(%s)',right) end
   end
   return string.format('%s%s%s', left, t.op, right)
end
-- function to string
symbol._strFn = function  (t)
   local a = {}
   for _,v in ipairs(t.args) do a[#a+1] = v:str() end
   return string.format('%s(%s)', t.fn, table.concat(a,','))
end

-- operation copy
symbol._cpyOp = function (t) 
   return symbol._op(t.op, t.left:cpy(), t.right:cpy())
end
-- function copy
symbol._cpyFn = function (t)
   local a = {}
   for _,v in ipairs(t.args) do a[#a+1] = v:cpy() end
   return symbol._fn(t.fn, a)
end
-- evaluate operation
symbol._evOp = function (t,env)
   local left = t.left:ev(env)
   local right = t.right:ev(env)
   local sl,sr = issymbol(left), issymbol(right)
   if not (sl or sr) then
      return symbol[t.op].eval(left,right)
   else
      if left and not sl then left = symbol._num(left) end
      right = sr and right or symbol._num(right)
      return symbol._op(t.op, left, right)
   end
end
-- evaluate function
symbol._evFn = function (t,env)
   local a,issym = {}, false
   for _,v in ipairs(t.args) do
      a[#a+1] = v:ev(env)
      issym = issym or issymbol(a[#a])
   end
   -- 
   local fn = env[t.fn] 
   if type(fn) ~= 'function' or issym then
      for i,v in ipairs(a) do
         if not issymbol(v) then a[i] = symbol._num(v) end
      end
      return symbol._fn(t.fn, a)
   else
      return fn(table.unpack(a))
   end
end
-- operation equality
symbol._eqOp = function (t1,t2)
   if rawequal(t1,t2) then return true end
   if not (t2.cls == OPERATION and t1.op == t2.op)  then return false end

   local res = t1.left:equal(t2.left) and t1.right:equal(t2.right)
   if not res and (t1.op == '+' or t1.op == '*') then 
      res = t1.left:equal(t2.right) and t1.right:equal(t2.left)
   end
   return res
end
-- function equality
symbol._eqFn = function (t1,t2)
   if rawequal(t1,r2) then return true end
   if t2.cls == FUNCTION and t1.fn == t2.fn and #t1.args == #t2.args then
      for i = 1,#t1.args do
         if not t1.args[i]:equal(t2.args[i]) then return false end
      end
   else
      return false
   end
   return true
end

------ base classes
local _class = {0,0,0,0}
_class[OPERATION] = {str=symbol._strOp, cpy=symbol._cpyOp, ev=symbol._evOp, 
                  equal=symbol._eqOp, doHash=hash.op}

_class[FUNCTION] = {str=symbol._strFn, cpy=symbol._cpyFn, ev=symbol._evFn, 
                  equal=symbol._eqFn, doHash=hash.fn}

_class[NUMBER] = {
str = function (t) return tostring(t.value) end,
cpy = function (t) return symbol._num (t.value) end,
ev = function (t,env) return t.value end,
equal = function (t1,t2) return rawequal(t1,t2) or t2.cls == NUMBER and t2.value == t1.value end,
doHash = hash.num,
}

_class[VARIABLE] = {
str = function (t) return t.name end,
cpy = function (t) return symbol._var(t.name) end,
ev = function (t,env) return env[t.name] or t end,
equal = function (t1,t2) return rawequal(t1,t2) or t2.cls == VARIABLE and t2.name == t1.name end,
doHash = hash.var,
}

----- inheritance
symbol.__index = function (t,k)
   return rawget(symbol,k) and symbol[k] or _class[t.cls][k] or symbol._get_lib_for(k)
end

-- free memory in case of standalone usage
if not lc_version then symbol.about = nil end

return symbol
