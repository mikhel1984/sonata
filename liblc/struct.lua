--[[       liblc/struct.lua

--- Main data structures.
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

           module 'struct'
--]]

--------------- Tests ------------
--[[!!
DS = require 'liblc.struct'

-- create stack
a = DS.Stack()
-- add values
a:push(1)
a:push(2)
-- stack size
ans = #a                        --> 2

-- top value
ans = a:peek()                  --> 2

-- make copy
aa = a:copy()

-- get value
a:pop()
ans = a:pop()                   --> 1

-- check capacity
ans = a:isEmpty()               --> true

-- create queue
b = DS.Queue()
-- add elements to the tale
b:add(1)
b:add(2)
-- get from the head
ans = b:rem()                   --> 1

-- add to the head
b:addFirst(4)
-- remove from the tale
ans = b:remLast()               --> 2

-- check queue size
ans = #b                        --> 1

-- top value
ans = b:peek()                  --> 4

-- check capacity
ans = b:isEmpty()               --> false

-- make copy
bb = b:copy()

-- create heap
c = DS.Heap()
c:insert(1)
c:insert(3)
c:insert(2)
ans = c:top()                --> 3

-- check capacity
ans = c:isEmpty()               --> false

-- new heap
-- with user comparison function
-- (return minimum element)
d = DS.Heap(function (a,b) return a > b end)
d:insert(1)
d:insert(3)
d:insert(2)
ans = d:top()                --> 1

-- make copy
dd = d:copy()

-- heap size
ans = #dd                       --> 2

-- define elements of the set
a = DS.Set {1,2,3,4,1}           
b = DS.Set {3,4,5}               
ans = a                        --> DS.Set {1,2,3,4}

-- check if 6 in set b
ans = b[6]                     --> nil

-- add value
b:insert(6)
ans = b[6]  
-- remove value             
b:remove(6)                    --> true

-- union
ans = a + b                    --> DS.Set {1,2,3,4,5}

-- intersection
ans = a * b                    --> DS.Set {3,4}

-- difference
ans = a / b                    --> DS.Set {1,2}

-- comparison
ans = (a == b)                 --> false

ans = (a < b)                  --> false

-- represent as list
t = a:table()
ans = a[ t[1] ]                --> true

-- size of the set
ans = #a                       --> 4

-- make copy
d = a:copy()
ans = (d == a)                 --> true

-- generate new set from given
-- use function
e = a:map(function (x) return x^2 end)
ans = e[16]                    --> true

-- show
print(a)
]]

--	LOCAL

local Ver = require "liblc.versions"

local STACK = 'stack'
local QUEUE = 'queue'
local HEAP = 'heap'
local SET = 'set'

--	INFO

local help = lc_version and (require "liblc.help") or {new=function () return {} end}

--	MODULE

local struct = {
-- description
about = help:new("Main data structures.")
}

-- common methods
--struct.about['COPY'] = {"copy()", "Return copy of the object."}
--struct.about['ISEMPTY'] = {"isEmpty()", "Return true if the object is empty."}

--	STACK

struct.Stack = {type='stack'}
struct.Stack.__index = struct.Stack

--- Constructor for stack data structure.
--    @return New stack.
struct.Stack.new = function (self)
   return setmetatable({}, self)
end

-- Alias for stack constructor
setmetatable(struct.Stack, {__call = function (self) return struct.Stack:new() end})
struct.about[struct.Stack] = {"Stack()", "Create new stack.", STACK}

--- Add element to the stack.
--    @param self Stack object.
--    @param val Value to push (except nil).
struct.Stack.push = function (self, val)
   assert(val ~= nil)
   self[#self+1] = val
end
struct.about[struct.Stack.push] = {"push(val)", "Push value to the stack, except nil.", STACK}

--- Get value from the top of stack.
--    @param self Stack object.
--    @return Top value or nil.
struct.Stack.pop = function (self)
   local res = self[#self]
   if res then self[#self] = nil end
   return res
end
struct.about[struct.Stack.pop] = {"pop()", "Pop value from the stack, return element or nil.", STACK}

--- Top value of the stack.
--    @param s Stack object.
--    @return Top value without removing it.
struct.Stack.peek = function (s) return s[#s] end
struct.about[struct.Stack.peek] = {"peek()", "Return top value without removing it.", STACK}

--- Copy stack.
--    @param s Stack object.
--    @return Copy.
struct.Stack.copy = function (s)
   local res = struct.Stack:new()
   if s[1] then
      Ver.move(s,1,#s,1,res)
   end
   return res
end
struct.about[struct.Stack.copy] = {"Stack.copy(s)", "Create copy of the stack.", STACK}

struct.Stack.size = function (s) return #s end
struct.about[struct.Stack.size] = {"Stack.size(s)", "Return number of elements in stack.", STACK}

--- Check stack size.
--    @param s Stack object.
--    @return True if stack is empty.
struct.Stack.isEmpty = function (s) return #s == 0 end
struct.about[struct.Stack.isEmpty] = {"Stack.isEmpty(s)", "Return true if the stack is empty.", STACK}

--	QUEUE
struct.Queue = {type='queue'}
struct.Queue.__index = struct.Queue

--- Queue constructor.
--    @return New queue.
struct.Queue.new = function (self)
   return setmetatable({first=0, last=-1}, self)
end

-- Alias for queue constructor
setmetatable(struct.Queue, {__call = function (self) return struct.Queue:new() end})
struct.about[struct.Queue] = {"Queue()", "Create new queue.", QUEUE}

--- Put value to the end of queue.
--    @param self Queue object.
--    @param val Value to put.
struct.Queue.add = function (self,val)
   local last = self.last+1
   self.last = last
   self[last] = val
end
struct.about[struct.Queue.add] = {"add(val)", "Add value to the back of queue.", QUEUE}

--- Put value to the top of queue (as in stack).
--    @param self Queue object.
--    @param val Value to put.
struct.Queue.addFirst = function (self,val)
   local first = self.first-1
   self.first = first
   self[first] = val
end
struct.about[struct.Queue.addFirst] = {"addFirst(val)", "Add value to the top of queue.", QUEUE}

--- Get value from the top of queue.
--    @param self Queue object.
--    @return Top value of nil.
struct.Queue.rem = function (self)
   local first,val = self.first
   if first <= self.last then
      val = self[first]
      self[first] = nil
      self.first = first+1
   end
   return val
end
struct.about[struct.Queue.rem] = {"rem()", "Get value from the top of queue, remove it.", QUEUE}

--- Get value from the end of queue.
--    @param self Queue object.
--    @return Last value of nil.
struct.Queue.remLast = function (self)
   local last, val = self.last
   if self.first <= last then
      val = self[last]
      self[last] = nil
      self.last = last-1
   end
   return val
end
struct.about[struct.Queue.remLast] = {"remLast()", "Get value from the end of queue, remove it.", QUEUE}

--- Get top value of the queue.
--    @param q Queue object.
--    @return Top value without removing it.
struct.Queue.peek = function (q) return q[q.first] end
struct.about[struct.Queue.peek] = {"Queue.peek(q)", "Get next element, don't remove it.", QUEUE}

-- Queue size
struct.Queue.__len = function (t) return t.last-t.first+1 end

struct.Queue.size = function (t) return t.last-t.first+1 end
struct.about[struct.Queue.size] = {"Queue.size(q)", "Return number of elements in queue.", QUEUE}

--- Check if the queue is empty.
--    @return True if there is no elements in the queue.
struct.Queue.isEmpty = function (q) return q.first+1 == q.last end
struct.about[struct.Queue.isEmpty] = {"Queue.isEmpty(q)", "Return true if the queue is empty.", QUEUE}

--- Queue copy.
--    @param q Original queue.
--    @return Copy.
struct.Queue.copy = function (q)
   local res = struct.Queue:new()
   local first,last = q.first, q.last
   res.first = first; res.last = last
   Ver.move(q,first,last,first,res)
   return res
end
struct.about[struct.Queue.copy] = {"Queue.copy(q)", "Make copy of the queue.", QUEUE}

--	HEAP
struct.Heap = {type='heap'}
struct.Heap.__index = struct.Heap

--- Heap constructor.
--    @param less Comparision function.
--    @return New heap object.
struct.Heap.new = function (self, less)
   local o = {N=0}
   -- default function for comparision, can be changed
   o.less = less or function (a,b) return a < b end
   return setmetatable(o, self)
end

-- Simplify constructor call.
setmetatable(struct.Heap, {__call = function (self,l) return struct.Heap:new(l) end})
struct.about[struct.Heap] = {"Heap([less])", "Create new heap object. Comparison method 'less' can be predefined.", HEAP}

--- Fix order of the heap in up direction.
--    @param h Heap object.
--    @param k Start index.
struct.Heap._fixUp = function (h, k)
   while k > 1 and h.less(h[math.modf(k*0.5)], h[k]) do
      local k2 = math.modf(k*0.5)
      h[k],h[k2] = h[k2],h[k]
      k = k2
   end
end

--- Fix order of the heap in down direction.
--    @param h Heap object.
--    @param k Start index.
--    @param N End index.
struct.Heap._fixDown = function (h, k, N)
   while 2*k <= N do
      local j = 2*k
      if j < N and h.less(h[j],h[j+1]) then j=j+1 end
      if not h.less(h[k],h[j]) then break end
      h[k],h[j] = h[j],h[k]
      k = j
   end
end

--- Insert element to the heap.
--    @param h Heap object.
--    @param v Element to add.
struct.Heap.insert = function (h, v)
   local n = h.N+1
   h.N = n
   h[n] = v
   struct.Heap._fixUp(h, n)
end
struct.about[struct.Heap.insert] = {"insert(v)", "Add element to the heap.", HEAP}

--- Get top element from the heap.
--    If 'less' method is default, top is the maximum element.
--    @param h Heap object.
--    @return Top element or nil.
struct.Heap.top = function (h)
   local n = h.N
   if n == 0 then return nil end
   h[1],h[n] = h[n],h[1]
   struct.Heap._fixDown(h,1,n-1)
   h.N = n-1
   return h[n]
end
struct.about[struct.Heap.top] = {"top()", "Return top element. For the default less() function top is maximum.", HEAP}

--- Check for elements in the heap.
--    @param h Heap object.
--    @return True if heap is empty.
struct.Heap.isEmpty = function (h) return h.N == 0 end
struct.about[struct.Heap.isEmpty] = {"Heap.isEmpty(h)", "Return true if the heap is empty.", HEAP}

-- Number of elements.
struct.Heap.size = function (h) return h.N end
struct.Heap.__len = struct.Heap.size
struct.about[struct.Heap.size] = {"Heap.size(h)", "Get number of elements in the heap.", HEAP}

--- Make heap copy.
--    @param h Original heap.
--    @return New heap.
struct.Heap.copy = function (h) 
   local res = struct.Heap:new(h.less)
   res.N = h.N
   Ver.move(h,1,#h,1,res)
   return res
end
struct.about[struct.Heap.copy] = {"Heap.copy(h)", "Make copy of the heap.", HEAP}

--	SET

struct.Set = {type='set'}
struct.Set.__index = struct.Set


--- Create new object, set metatable.
--    @param l Table of elements.
--    @return New set object.
struct.Set.new = function (self, l)
   local o = {}
   for i = 1, #l do o[l[i]] = true end
   return setmetatable(o, self)
end

--- Add new element.
--    @param s Set object.
--    @param v New element.
struct.Set.insert = function (s, v)
   s[v] = true
end
struct.about[struct.Set.insert] = {"insert(set,val)", "Insert element into set.", SET}

--- Delete element.
--    @param s Set object.
--    @param v Element.
struct.Set.remove = function (s,v)
   s[v] = nil
end
struct.about[struct.Set.remove] = {"remove(set,val)", "Remove element from set.", SET}

--- Convert into Lua table.
--    @param s Set object.
--    @return List of elements.
struct.Set.table = function (s)
   local res = {}
   for k in pairs(s) do table.insert(res, k) end
   return res
end
struct.about[struct.Set.table] = {"table(set)", "Represent set as a table.", SET}

--- Copy of the set.
--    @param s Initial set.
--    @return Copy object.
struct.Set.copy = function (s)
   local res = struct.Set:new({})
   for k in pairs(s) do res[k] = true end
   return res
end
struct.about[struct.Set.copy] = {"Set.copy(s)", "Get copy of the set.", SET}

--- Apply function to the elements of set.
--    @param s Initial set.
--    @param fn Function.
--    @return New set, obtained from function.
struct.Set.map = function (s,fn)
   local res = struct.Set:new({})
   for k in pairs(s) do res[fn(k)] = true end
   return res
end
struct.about[struct.Set.map] = {"map(s,fn)", "Apply function fn() to obtain new set.", SET}


--- a + b
--    @return Union.
struct.Set.__add = function (a,b)
   assert(a.type == 'set' and b.type == 'set')
   local res = struct.Set:new({})
   for k in pairs(a) do res[k] = true end
   for k in pairs(b) do res[k] = true end
   return res
end

--- a * b
--    @return Intersection.
struct.Set.__mul = function (a,b)
   assert(a.type == 'set' and b.type == 'set')
   local res = struct.Set:new({})
   for k in pairs(a) do res[k] = b[k] end
   return res
end

--- a / b
--    @return Difference.
struct.Set.__div = function (a,b)
   assert(a.type == 'set' and b.type == 'set')
   local res = struct.Set:new({})
   for k in pairs(a) do
      if not b[k] then res[k] = true end
   end
   return res
end

struct.Set.arithmetic = 'arithmetic'
struct.about[struct.Set.arithmetic] = {"union, intersection, difference", "a+b, a*b, a/b", SET}

--- a <= b
--    @return <code>true</code> if <code>a</code> is a subset of <code>b</code>.
struct.Set.__le = function (a,b)
   assert(a.type == 'set' and b.type == 'set')
   for k in pairs(a) do
      if not b[k] then return false end
   end
   return true
end

--- a < b
--    @return <code>true</code> if <code>a</code> is a subset of <code>b</code> but not equal.
struct.Set.__lt = function (a,b)
   return a <= b and not (b <= a)
end

--- a == b
--    @return <code>true</code> if <code>a</code> and <code>b</code> are equal.
struct.Set.__eq = function (a,b)
   return a <= b and b <= a
end

struct.Set.comparison = 'comparison'
struct.about[struct.Set.comparison] = {struct.Set.comparison, "a==b, a~=b, a<b, a<=b, a>b, a>=b", SET}

-- #a 
--    @return Number of elements.
struct.Set.__len = function (s)
   local n = 0
   for k in pairs(s) do n = n+1 end
   return n
end
struct.Set.size = struct.Set.__len
struct.about[struct.Set.size] = {"Set.size(s)", "Number of elements in the set.", SET}

struct.Set.isEmpty = function (s) return next(s) == nil end
struct.about[struct.Set.isEmpty] = {"Set.isEmpty(s)", "Return true if the set is empty.", SET}

--- String representation.
struct.Set.__tostring = function (s)
   local lst = {}
   for e in pairs(s) do table.insert(lst, e) end
   return string.format('{%s}', table.concat(lst,','))
end

-- redefine constructor
setmetatable(struct.Set, {__call = function (self, v) return struct.Set:new(v) end})
struct.about[struct.Set] = {"Set(t)", "Create new set from table of elements.", SET}

-- free memory in case of standalone usage
if not lc_version then struct.about = nil end

return struct

