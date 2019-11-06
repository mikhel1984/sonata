--[[       sonatalib/struct.lua

--- Main data structures.
--  
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonatalib</a> collection, 2017-2019.

           module 'struct'
--]]

--------------- Tests ------------
--[[TEST

-- import 'struct'
DS = require 'sonatalib.struct'

-- create stack
a = DS.Stack()
-- add values
a:push(1)
a:push(2)
-- stack size
-- (the same as #a)
ans = a:size()                  --> 2

-- top value
ans = a:top()                   --> 2

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
b:push(1)
b:push(2)
-- get from the head
ans = b:pop()                   --> 1

-- add to the head
b:pushFront(4)
-- remove from the tale
ans = b:popBack()               --> 2

-- check queue size
-- (the same as #b)
ans = b:size()                  --> 1

-- top value
ans = b:front()                 --> 4

-- opposite
ans = b:back()                  --> 4

-- check capacity
ans = b:isEmpty()               --> false

-- make copy
bb = b:copy()

-- create heap
c = DS.Heap()
c:push(1)
c:push(3)
c:push(2)
ans = c:top()                  --> 3

-- get element
ans = c:pop()                  --> 3

-- check capacity
ans = c:isEmpty()              --> false

-- new heap
-- with user comparison function
-- (return minimum element)
d = DS.Heap(function (a,b) return a > b end)
d:push(1)
d:push(3)
d:push(2)
ans = d:top()                   --> 1

-- make copy
dd = d:copy()

-- heap size
-- (equal to #dd)
ans = dd:size()                 --> 3

-- get heap element
ans = dd:pop()                  --> 1

-- define elements of the set
a = DS.Set {1,2,3,4,1}           
b = DS.Set {3,4,5}               
ans = a                        --> DS.Set {1,2,3,4}

-- check if 6 in set b
ans = b[6]                     --> nil

-- add value
b:add(6)
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
t = a:list()
ans = a[ t[1] ]                --> true

-- size of the set
-- (the same as #a)
ans = a:size()                 --> 4

ans = a:isEmpty()              --> false

-- make copy
d = a:copy()
ans = (d == a)                 --> true

-- generate new set 
-- using function
e = a:map(function (x) return x^2 end)
ans = e[16]                    --> true

-- show
print(a)
--]]

--	LOCAL

local Ver = require "sonatalib.versions"

local STACK = 'stack'
local QUEUE = 'queue'
local HEAP  = 'heap'
local SET   = 'set'

--	INFO

local help = LC_DIALOG and (require "sonatalib.help") or {new=function () return {} end}

--	MODULE

local struct = {
-- description
about = help:new("Main data structures.")
}

--	STACK

struct.Stack = {type='stack'}
struct.Stack.__index = struct.Stack

--- Constructor for stack data structure.
--  @param self Parent object.
--  @return New stack.
struct.Stack.new = function (self) return setmetatable({}, self) end

-- Alias for stack constructor
setmetatable(struct.Stack, {__call = function (self) return struct.Stack:new() end})
struct.about[struct.Stack] = {"Stack()", "Create new stack.", STACK}

--- Add element to the stack.
--  @param self Stack object.
--  @param val Value to push. Nil is ignored.
struct.Stack.push = function (self, val)
   table.insert(self,val)
end
struct.about[struct.Stack.push] = {"Stack.push(S,val)", "Push value to the stack (except nil).", STACK}

--- Get value from the top of stack.
--  @param self Stack object.
--  @return Top value or nil.
struct.Stack.pop = function (self)
   return table.remove(self)
end
struct.about[struct.Stack.pop] = {"Stack.pop(S)", "Pop value from the stack, return element or nil.", STACK}

--- Top value of the stack.
--  @param S Stack object.
--  @return Top value without removing it.
struct.Stack.top = function (S) return S[#S] end
struct.about[struct.Stack.top] = {"Stack.top(S)", "Return top value without removing it.", STACK}

--- Copy stack.
--  @param S Stack object.
--  @return Copy.
struct.Stack.copy = function (S)
   local res = struct.Stack:new()
   if S[1] then
      Ver.move(S,1,#S,1,res)
   end
   return res
end
struct.about[struct.Stack.copy] = {"Stack.copy(S)", "Create copy of the stack.", STACK}

--- Number of elements.
--  @param S Stack object.
--  @return Stack size.
struct.Stack.size = function (S) return #S end
struct.about[struct.Stack.size] = {"Stack.size(S)", "Return number of elements in stack.", STACK}

--- Check stack size.
--  @param S Stack object.
--  @return True if stack is empty.
struct.Stack.isEmpty = function (S) return #S == 0 end
struct.about[struct.Stack.isEmpty] = {"Stack.isEmpty(S)", "Return true if the stack is empty.", STACK}

--	QUEUE
struct.Queue = {type='queue'}
struct.Queue.__index = struct.Queue

--- Queue constructor.
--  @param self Parent object.
--  @return New queue.
struct.Queue.new = function (self)
   return setmetatable({first=0, last=-1}, self)
end

-- Alias for queue constructor
setmetatable(struct.Queue, {__call = function (self) return struct.Queue:new() end})
struct.about[struct.Queue] = {"Queue()", "Create new queue.", QUEUE}

--- Put value to the end of queue.
--  @param Q Queue object.
--  @param val Value to put.
struct.Queue.push = function (Q,val)
   local last = Q.last+1
   Q.last = last
   Q[last] = val
end
struct.about[struct.Queue.push] = {"Queue.push(Q,val)", "Add value to the back of queue.", QUEUE}

--- Put value to the top of queue (as in stack).
--  @param Q Queue object.
--  @param val Value to put.
struct.Queue.pushFront = function (Q,val)
   local first = Q.first-1
   Q.first = first
   Q[first] = val
end
struct.about[struct.Queue.pushFront] = {"Queue.pushFront(Q,val)", "Add value to the top of queue.", QUEUE}

--- Get value from the top of queue.
--  @param Q Queue object.
--  @return Top value of nil.
struct.Queue.pop = function (Q)
   local first,val = Q.first
   if first <= Q.last then
      val = Q[first]
      Q[first] = nil
      Q.first = first+1
   end
   return val
end
struct.about[struct.Queue.pop] = {"Queue.pop(Q)", "Get value from the top of queue, remove it.", QUEUE}

--- Get value from the end of queue.
--  @param Q Queue object.
--  @return Last value of nil.
struct.Queue.popBack = function (Q)
   local last, val = Q.last
   if Q.first <= last then
      val = Q[last]
      Q[last] = nil
      Q.last = last-1
   end
   return val
end
struct.about[struct.Queue.popBack] = {"Queue.popBack(Q)", "Get value from the end of queue, remove it.", QUEUE}

--- Get top value of the queue.
--  @param Q Queue object.
--  @return Front value without removing it.
struct.Queue.front = function (Q) return Q[Q.first] end
struct.about[struct.Queue.front] = {"Queue.front(Q)", "Get next element, don't remove it.", QUEUE}

--- Get back element from the queue.
--  @param Q Queue object.
--  @return Back element without removing it.
struct.Queue.back = function (Q) return Q[Q.last] end
struct.about[struct.Queue.back] = {"Queue.back(Q)", "Get next element, don't remove it.", QUEUE}

--- Queue size
--  @param Q Queue object.
--  @return Number of elements in queue.
struct.Queue.size = function (Q) return Q.last-Q.first+1 end
struct.about[struct.Queue.size] = {"Queue.size(Q)", "Return number of elements in queue.", QUEUE}
struct.Queue.__len = struct.Queue.size

--- Check if the queue is empty.
--  @param Q Queue object.
--  @return True if there is no elements in the queue.
struct.Queue.isEmpty = function (Q) return Q.first-1 == Q.last end
struct.about[struct.Queue.isEmpty] = {"Queue.isEmpty(Q)", "Return true if the queue is empty.", QUEUE}

--- Queue copy.
--  @param Q Original queue.
--  @return Copy.
struct.Queue.copy = function (Q)
   local res = struct.Queue:new()
   local first,last = Q.first, Q.last
   res.first = first; res.last = last
   Ver.move(Q,first,last,first,res)
   return res
end
struct.about[struct.Queue.copy] = {"Queue.copy(Q)", "Make copy of the queue.", QUEUE}

--	HEAP
struct.Heap = {type='heap'}
struct.Heap.__index = struct.Heap

--- Heap constructor.
--  @param self Heap object.
--  @param less Comparison function.
--  @return New heap object.
struct.Heap.new = function (self, less)
   local o = {N=0}
   -- default function for comparison, can be changed
   o.less = less or function (a,b) return a < b end
   return setmetatable(o, self)
end

-- Simplify constructor call.
setmetatable(struct.Heap, {__call = function (self,l) return struct.Heap:new(l) end})
struct.about[struct.Heap] = {"Heap([less])", "Create new heap object. Comparison method 'less' can be predefined.", HEAP}

--- Fix order of the heap in up direction.
--  @param H Heap object.
--  @param k Start index.
struct.Heap._fixUp_ = function (H, k)
   while k > 1 and H.less(H[math.modf(k*0.5)], H[k]) do
      local k2 = math.modf(k*0.5)
      H[k],H[k2] = H[k2],H[k]
      k = k2
   end
end

--- Fix order of the heap in down direction.
--  @param H Heap object.
--  @param k Start index.
--  @param N End index.
struct.Heap._fixDown_ = function (H, k, N)
   while 2*k <= N do
      local j = 2*k
      if j < N and H.less(H[j],H[j+1]) then j=j+1 end
      if not H.less(H[k],H[j]) then break end
      H[k],H[j] = H[j],H[k]
      k = j
   end
end

--- Insert element to the heap.
--  @param H Heap object.
--  @param v Element to add.
struct.Heap.push = function (H, v)
   local n = H.N+1
   H.N = n
   H[n] = v
   struct.Heap._fixUp_(H, n)
end
struct.about[struct.Heap.push] = {"Heap.push(H,v)", "Add element to the heap.", HEAP}

--- Get top element from the heap.
--  If 'less' method is default, top is the maximum element.
--  @param H Heap object.
--  @return Top element or nil.
struct.Heap.pop = function (H)
   local n = H.N
   if n == 0 then return nil end
   H[1],H[n] = H[n],H[1]
   struct.Heap._fixDown_(H,1,n-1)
   H.N = n-1
   return H[n]
end
struct.about[struct.Heap.pop] = {"Heap.pop(H)", "Return top element. For the default less() function top is maximum.", HEAP}

--- Get top of the heap.
--  @param H Heap object.
--  @return Value of the top element.
struct.Heap.top = function (H) return H[1] end
struct.about[struct.Heap.top] = {'Heap.top(H)', "Return value of the top element.", HEAP}

--- Check for elements in the heap.
--  @param H Heap object.
--  @return True if heap is empty.
struct.Heap.isEmpty = function (H) return H.N == 0 end
struct.about[struct.Heap.isEmpty] = {"Heap.isEmpty(H)", "Return true if the heap is empty.", HEAP}

--- Number of elements.
--  @param H Heap object.
--  @return Size of heap.
struct.Heap.size = function (H) return H.N end
struct.about[struct.Heap.size] = {"Heap.size(H)", "Get number of elements in the heap.", HEAP}
struct.Heap.__len = struct.Heap.size

--- Make heap copy.
--  @param H Original heap.
--  @return New heap.
struct.Heap.copy = function (H) 
   local res = struct.Heap:new(H.less)
   res.N = H.N
   Ver.move(H,1,#H,1,res)
   return res
end
struct.about[struct.Heap.copy] = {"Heap.copy(H)", "Make copy of the heap.", HEAP}

--	SET

struct.Set = {type='set'}
struct.Set.__index = struct.Set

--- Create new object, set metatable.
--  @param self Set object.
--  @param lst Table of elements.
--  @return New set object.
struct.Set.new = function (self, lst)
   local o = {}
   for i = 1, #lst do o[lst[i]] = true end
   return setmetatable(o, self)
end

--- Add new element.
--  @param S Set object.
--  @param v New element.
struct.Set.add = function (S, v) S[v] = true end
struct.about[struct.Set.add] = {"Set.add(S,val)", "Insert element into set.", SET}

--- Delete element.
--  @param S Set object.
--  @param v Element.
struct.Set.remove = function (S,v) S[v] = nil end
struct.about[struct.Set.remove] = {"Set.remove(S,val)", "Remove element from set.", SET}

--- Convert into array.
--  @param S Set object.
--  @return List of elements.
struct.Set.list = function (S)
   local res = {}
   for k in pairs(S) do table.insert(res, k) end
   return res
end
struct.about[struct.Set.list] = {"Set.list(S)", "Represent set as a list of elements.", SET}

--- Copy of the set.
--  @param S Initial set.
--  @return Copy object.
struct.Set.copy = function (S)
   local res = struct.Set:new({})
   for k in pairs(S) do res[k] = true end
   return res
end
struct.about[struct.Set.copy] = {"Set.copy(S)", "Get copy of the set.", SET}

--- Apply function to the elements of set.
--  @param S Initial set.
--  @param fn Function.
--  @return New set, obtained from function.
struct.Set.map = function (S,fn)
   local res = struct.Set:new({})
   for k in pairs(S) do res[fn(k)] = true end
   return res
end
struct.about[struct.Set.map] = {"Set.map(S,fn)", "Apply function fn() to obtain new set.", SET}


--- S1 + S2
--  @param S1 First set.
--  @param S2 Second set.
--  @return Union.
struct.Set.__add = function (S1,S2)
   assert(S1.type == 'set' and S2.type == 'set')
   local res = struct.Set:new({})
   for k in pairs(S1) do res[k] = true end
   for k in pairs(S2) do res[k] = true end
   return res
end

--- S1 * S2
--  @param S1 First set.
--  @param S2 Second set.
--  @return Intersection.
struct.Set.__mul = function (S1,S2)
   assert(S1.type == 'set' and S2.type == 'set')
   local res = struct.Set:new({})
   for k in pairs(S1) do res[k] = S2[k] end
   return res
end

--- S1 / S2
--  @param S1 First set.
--  @param S2 Second set.
--  @return Difference.
struct.Set.__div = function (S1,S2)
   assert(S1.type == 'set' and S2.type == 'set')
   local res = struct.Set:new({})
   for k in pairs(S1) do
      if not S2[k] then res[k] = true end
   end
   return res
end

struct.Set.arithmetic = 'arithmetic'
struct.about[struct.Set.arithmetic] = {"Set: union, intersection, difference", "S1+S2, S1*S2, S1/S2", SET}

--- S1 <= S2
--  @param S1 First set.
--  @param S2 Second set.
--  @return True if S1 is a subset of S2.
struct.Set.__le = function (S1,S2)
   assert(S1.type == 'set' and S2.type == 'set')
   for k in pairs(S1) do
      if not S2[k] then return false end
   end
   return true
end

--- S1 < S1
--  @param S1 First set.
--  @param S2 Second set.
--  @return True if S1 is a subset of S2 but not equal.
struct.Set.__lt = function (S1,S2)
   return S1 <= S2 and not (S2 <= S1)
end

--- S1 == S2
--  @param S1 First set.
--  @param S2 Second set.
--  @return True if S1 and S2 are equal.
struct.Set.__eq = function (S1,S2)
   return S1 <= S2 and S2 <= S1
end

struct.Set.comparison = 'Set: comparison'
struct.about[struct.Set.comparison] = {struct.Set.comparison, "S1==S2, S1~=S2, S1<S2, S1<=S2, S1>S2, S1>=S2", SET}

--- #S 
--  @param S Set object.
--  @return Number of elements in set.
struct.Set.__len = function (S)
   local n = 0
   for k in pairs(S) do n = n+1 end
   return n
end
struct.Set.size = struct.Set.__len
struct.about[struct.Set.size] = {"Set.size(S)", "Number of elements in the set.", SET}

--- Check if the set is empty.
--  @param S Set object.
--  @return True if the set is empty.
struct.Set.isEmpty = function (S) return next(S) == nil end
struct.about[struct.Set.isEmpty] = {"Set.isEmpty(S)", "Return true if the set is empty.", SET}

--- String representation.
--  @param S Set object.
--  @return String with the set elements.
struct.Set.__tostring = function (S)
   local lst = {}
   for e in pairs(S) do table.insert(lst, e) end
   return string.format('{%s}', table.concat(lst,','))
end

-- redefine constructor
setmetatable(struct.Set, {__call = function (self, v) return struct.Set:new(v) end})
struct.about[struct.Set] = {"Set(t)", "Create new set from table of elements.", SET}

-- free memory in case of standalone usage
if not LC_DIALOG then struct.about = nil end

return struct

--========================================
