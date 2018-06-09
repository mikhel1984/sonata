
local rules = {}

local sym_base = {}

sym_base._colNumFn = function (s)
   for i = 1,#s.args do s.args[i] = s.args[i]:collectNum() end
end

sym_base._colNumOp = function (s)
   s.left = s.left:collectNum()
   s.right = s.right:collectNum()   
   local op = s.op
   -- order elements
   if s.right.value then 
      if s.left.value then
      -- evaluate
         return symbol._num(oplist[op](s.left.value, s.right.value))  
      elseif op == '*' or op == '+' then
      -- swap, number should be in the left
         s.left, s.right = s.right, s.left   
      elseif op == '-' then
         s.right.value = -s.right.value
	 s = symbol._op('+', s.right, s.left)
      elseif op == '/' then
         s.right.value = 1/s.right.value
	 s = symbol._op('*', s.right, s.left)
      end 
      op = s.op
   end
   -- sym_base
   local L, R = s.left, s.right
   local optype = oplist.type[op]
   if optype == oplist.type[R.op] and R.left.value then
      if L.value then
         s = rules.num[(optype==OP_SUM) and '+' or '*'][op][R.op](L, symbol.NULL, R.left, R.right)
	 sym_base._removeNull(s)
      elseif optype == oplist.type[L.op] and L.left.value then
         s = rules.num[L.op][op][R.op](L.left, L.right, R.left, R.right)
      end
   elseif optype == oplist.type[L.op] and L.left.value then
      s = rules.num[L.op][op][(optype==OP_SUM) and '+' or '*'](L.left, L.right, (optype==OP_SUM) and symbol.ZERO or symbol.ONE, R)
   end
   return s
end

sym_base._removeNull = function (s)
   local t = s.right
   if rawequal(t.right,symbol.NULL) then
      s.right = t.left
   elseif rawequal(t.left,symbol.NULL) then
      s.right = t.right
      if s.op == '+' or s.op == '*' then
	 s.op = t.op
      else
         if     t.op == '-' then s.op = '+'
	 elseif t.op == '/' then s.op = '*'
	 end
      end
   end
   --return s
end

rules.num = {
['+'] = {
  ['+'] = {
    ['+'] = function (n1,s1,n2,s2) return symbol._op('+', symbol._num(n1.value+n2.value), symbol._op('+',s1,s2)) end,
    ['-'] = function (n1,s1,n2,s2) return symbol._op('+', symbol._num(n1.value+n2.value), symbol._op('-',s1,s2)) end,
  },
  ['-'] = {
    ['+'] = function (n1,s1,n2,s2) return symbol._op('+', symbol._num(n1.value-n2.value), symbol._op('-',s1,s2)) end,
    ['-'] = function (n1,s1,n2,s2) return symbol._op('+', symbol._num(n1.value-n2.value), symbol._op('+',s1,s2)) end,
  },
},
['-'] = {
  ['+'] = {
    ['+'] = function (n1,s1,n2,s2) return symbol._op('+', symbol._num(n1.value+n2.value), symbol._op('-',s2,s1)) end,
    ['-'] = function (n1,s1,n2,s2) return symbol._op('-', symbol._num(n1.value+n2.value), symbol._op('+',s1,s2)) end,
  },
  ['-'] = {
    ['+'] = function (n1,s1,n2,s2) return symbol._op('-', symbol._num(n1.value-n2.value), symbol._op('+',s1,s2)) end,
    ['-'] = function (n1,s1,n2,s2) return symbol._op('+', symbol._num(n1.value-n2.value), symbol._op('-',s2,s1)) end,
  },
},
['*'] = {
  ['*'] = {
    ['*'] = function (n1,s1,n2,s2) return symbol._op('*', symbol._num(n1.value*n2.value), symbol._op('*',s1,s2)) end,
    ['/'] = function (n1,s1,n2,s2) return symbol._op('*', symbol._num(n1.value*n2.value), symbol._op('/',s1,s2)) end,
  },
  ['/'] = {
    ['*'] = function (n1,s1,n2,s2) return symbol._op('*', symbol._num(n1.value/n2.value), symbol._op('/',s1,s2)) end,
    ['/'] = function (n1,s1,n2,s2) return symbol._op('*', symbol._num(n1.value/n2.value), symbol._op('*',s1,s2)) end,
  },
},
['/'] = {
  ['*'] = {
    ['*'] = function (n1,s1,n2,s2) return symbol._op('*', symbol._num(n1.value*n2.value), symbol._op('/',s2,s1)) end,
    ['/'] = function (n1,s1,n2,s2) return symbol._op('/', symbol._num(n1.value*n2.value), symbol._op('*',s1,s2)) end,
  },
  ['/'] = {
    ['*'] = function (n1,s1,n2,s2) return symbol._op('/', symbol._num(n1.value/n2.value), symbol._op('*',s1,s2)) end,
    ['/'] = function (n1,s1,n2,s2) return symbol._op('*', symbol._num(n1.value/n2.value), symbol._op('/',s2,s1)) end,
  },
},
}
