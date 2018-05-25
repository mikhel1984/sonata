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

local CLS_OP, CLS_VAR, CLS_NUM, CLS_FN = 1, 2, 3, 4
local OP_SUM, OP_PROD, OP_POW = 1, 2, 3

local oplist = {
['+'] = function (a,b) return a+b end,
['-'] = function (a,b) return a-b end,
['*'] = function (a,b) return a*b end,
['/'] = function (a,b) return a/b end,
['^'] = function (a,b) return a^b end,
type = {
['+'] = OP_SUM, ['-'] = OP_SUM, ['*'] = OP_PROD, ['/'] = OP_PROD, ['^'] = OP_POW
},
}

local symbol = {}
--------
local parser = {}

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

parser.noblank = function(s,n)
   n = string.find(s,'[^%s%c]',n)
   return n and n or #s+1
end

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

-----------------------------------

local simplify = {}

simplify.testfn = function (nm,t) 
   for i = 1,#t do nm = nm + i*t[i] end 
   return nm
end

simplify.hash = function (nm,h) 
   if not h[nm] then h[nm] = math.random(65535)*1.0 end
   return h[nm]
end

simplify._varHash = function (s,h) 
   s.hash = simplify.hash(s.name,h)
   return s.hash
end

simplify._numHash = function (s,h)
   s.hash = s.value*1.0
   return s.hash
end

simplify._opHash = function (s,h)
   s.hash = oplist[s.op](s.left:doHash(h), s.right:doHash(h))   
   return s.hash
end

simplify._fnHash = function (s,h)
   local t = {}
   for i = 1,#s.args do t[#t+1] = s.args[i]:doHash(h) end
   s.hash = simplify.testfn(simplify.hash(s.fn,h), t)
   return s.hash
end

simplify._colNumFn = function (s)
   for i = 1,#s.args do s.args[i]:collectNum() end
end

simplify._colNumOp = function (s)
   s.left:collectNum()
   s.right:collectNum()
   local cl,cr = s.left.cls, s.right.cls
   if cl == CLS_NUM and cr == CLS_NUM then
      s = symbol._num(oplist[s.op](s.left.value, s.right.value))
   else
   end
end

simplify._colNumOp1 = function (op,sl,sr,invert)
end

simplify._sumExpr = function (op,s1,s2)
   local res
   if s1.value then
   end
end



----------------------------------
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

--- Constructor example
--    @param t Some value.
--    @return New object of symbol.
function symbol:new(t)
   return setmetatable(t,self)
end


symbol._op = function (op,s1,s2) return symbol:new {cls=CLS_OP, op=op, left=s1, right=s2, hash=0} end

symbol._num = function (v) return symbol:new {cls=CLS_NUM, value=v, hash=0} end

symbol._var = function (n) return symbol:new {cls=CLS_VAR, name=n, hash=0} end

symbol._fn = function (f,a) return symbol:new {cls=CLS_FN, fn=f, args=a, hash=0} end
-- null value for unar minus
symbol.NULL = symbol._num(0)
symbol.NULL.cpy = function () return symbol.NULL end
symbol.NULL.str = function () return "" end

symbol.parse = function (s)
   local val = parser.Sum(s,1)
   return val
end


symbol.eval = function (s,env) 
   env = env or _ENV
   return s:ev(env) 
end

symbol.__add = function (a,b)
   a,b = args(a,b)
   return symbol._op('+',a,b)
end

symbol.__sub = function (a,b)
   a,b = args(a,b)
   return symbol._op('-',a,b)
end

symbol.__mul = function (a,b)
   a,b = args(a,b)
   return symbol._op('*',a,b)
end

symbol.__div = function (a,b)
   a,b = args(a,b)
   return symbol._op('/',a,b)
end

symbol.__pow = function (a,b)
   a,b = args(a,b)
   return symbol._op('^',a,b)
end

symbol.__unm = function (a)
   a = issymbol(a) and a or symbol._num(a)
   return symbol._op('-',symbol.NULL,a)
end

symbol.__tostring = function (s) return s:str() end

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

symbol._strFn = function  (t)
   local a = {}
   for _,v in ipairs(t.args) do a[#a+1] = v:str() end
   return string.format('%s(%s)', t.fn, table.concat(a,','))
end


symbol._cpyOp = function (t) 
   return symbol._op(t.op, t.left:cpy(), t.right:cpy())
end

symbol._cpyFn = function (t)
   local a = {}
   for _,v in ipairs(t.args) do a[#a+1] = v:cpy() end
   return symbol._fn(t.fn, a)
end

symbol._evOp = function (t,env)
   local left = t.left:ev(env)
   local right = t.right:ev(env)
   local sl,sr = issymbol(left), issymbol(right)
   if not (sl or sr) then
      return oplist[t.op](left,right)
   else
      if left and not sl then left = symbol._num(left) end
      right = sr and right or symbol._num(right)
      return symbol._op(t.op, left, right)
   end
end

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

symbol._eqOp = function (t1,t2)
   if rawequal(t1,t2) then return true end
   if not (t2.cls == CLS_OP and t1.op == t2.op)  then return false end

   local res = t1.left:equal(t2.left) and t1.right:equal(t2.right)
   if not res and (t1.op == '+' or t1.op == '*') then 
      res = t1.left:equal(t2.right) and t1.right:equal(t2.left)
   end
   return res
end

symbol._eqFn = function (t1,t2)
   if rawequal(t1,r2) then return true end
   if t2.cls == CLS_FN and t1.fn == t2.fn and #t1.args == #t2.args then
      for i = 1,#t1.args do
         if not t1.args[i]:equal(t2.args[i]) then return false end
      end
   else
      return false
   end
   return true
end

------ base classes
local _class = {}
_class[CLS_OP] = {str=symbol._strOp, cpy=symbol._cpyOp, ev=symbol._evOp, 
                  equal=symbol._eqOp, doHash=simplify._opHash}

_class[CLS_FN] = {str=symbol._strFn, cpy=symbol._cpyFn, ev=symbol._evFn, 
                  equal=symbol._eqFn, doHash=simplify._fnHash}

_class[CLS_NUM] = {
str = function (t) return tostring(t.value) end,
cpy = function (t) return symbol._num (t.value) end,
ev = function (t,env) return t.value end,
equal = function (t1,t2) return rawequal(t1,t2) or t2.cls == CLS_NUM and t2.value == t1.value end,
doHash = simplify._numHash,
collectNum = function() end,
}

_class[CLS_VAR] = {
str = function (t) return t.name end,
cpy = function (t) return symbol._var(t.name) end,
ev = function (t,env) return env[t.name] or t end,
equal = function (t1,t2) return rawequal(t1,t2) or t2.cls == CLS_VAR and t2.name == t1.name end,
doHash = simplify._varHash,
collectNum = function() end,
}

----- inheritance
symbol.__index = function (t,k)
   if rawget(symbol,k) then return symbol[k]
   else return _class[t.cls][k]
   end
end
-- free memory in case of standalone usage
--if not lc_version then symbol.about = nil end

--[[
--return symbol
a = symbol.parse('name.fn(b+c, q )^( a+ d * f)')
b = a:copy()

fun = function (a,b) return a/b end
p1 = 4
p2 = 2

c = symbol('p1+p2*fun(p1,p2)^2')
print(c:eval())
]]


--print(symbol('-b'))
--a = symbol('fn1(a,b+c)')
--b = symbol('fn2(a,c+b)')
--print(a == b)
--print(a:doHash({}))

--[[
x1 = symbol.parse('a+b')
x2 = symbol.parse('c*d')

print(x1+x2)
print(x1-x2)
print(x1*x2)
print(x1/x2)
print(x1^x2)
print(-x1)
]]
