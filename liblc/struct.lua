--[[       liblc/struct.lua

--- Main data structures.
--  @author My Name

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
ans = a:isempty()               --> true

-- create queue
b = DS.Queue()
-- add elements to the tale
b:add(1)
b:add(2)
-- get from the head
ans = b:rem()                   --> 1

-- add to the head
b:addfirst(4)
-- remove from the tale
ans = b:remlast()               --> 2

-- check queue size
ans = #b                        --> 1

-- top value
ans = b:peek()                  --> 4

-- check capacity
ans = b:isempty()               --> false

-- make copy
bb = b:copy()

-- create heap
c = DS.Heap()
c:insert(1)
c:insert(3)
c:insert(2)
ans = c:deltop()                --> 3

-- check capacity
ans = c:isempty()               --> false

-- new heap
-- with user comparison function
-- (return minimum element)
d = DS.Heap(function (a,b) return a > b end)
d:insert(1)
d:insert(3)
d:insert(2)
ans = d:deltop()                --> 1

-- make copy
dd = d:copy()

-- heap size
ans = #dd                       --> 2
]]

local STACK = 'stack'
local QUEUE = 'queue'
local HEAP = 'heap'

---------------------------------
-- @class table
-- @name struct
-- @field about Description of functions.
local struct = {}

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
struct.about = help:new("Main data structures.")

-- common methods
struct.about['COPY'] = {"copy()", "Return copy of the object."}
struct.about['ISEMPTY'] = {"isempty()", "Return true if the object is empty."}

--	STACK
struct.Stack = {type='stack'}
struct.Stack.__index = struct.Stack

--- Constructor for stack data structure.
--    @return New stack.
struct.Stack.new = function (self)
   local o = {}
   setmetatable(o, self)
   return o
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
      table.move(s,1,#s,1,res)
   end
   return res
end

--- Check stack size.
--    @param s Stack object.
--    @return True if stack is empty.
struct.Stack.isempty = function (s) return #s == 0 end

--	QUEUE
struct.Queue = {type='queue'}
struct.Queue.__index = struct.Queue

--- Queue constructor.
--    @return New queue.
struct.Queue.new = function (self)
   local o = {first=0, last=-1}
   setmetatable(o, self)
   return o
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
struct.Queue.addfirst = function (self,val)
   local first = self.first-1
   self.first = first
   self[first] = val
end
struct.about[struct.Queue.addfirst] = {"addfirst(val)", "Add value to the top of queue.", QUEUE}

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
struct.Queue.remlast = function (self)
   local last, val = self.last
   if self.first <= last then
      val = self[last]
      self[last] = nil
      self.last = last-1
   end
   return val
end
struct.about[struct.Queue.remlast] = {"remlast()", "Get value from the end of queue, remove it.", QUEUE}

--- Get top value of the queue.
--    @param q Queue object.
--    @return Top value without removing it.
struct.Queue.peek = function (q) return q[q.first] end

-- Queue size
struct.Queue.__len = function (t) return t.last-t.first+1 end

--- Check if the queue is empty.
--    @return True if there is no elements in the queue.
struct.Queue.isempty = function (q) return q.first+1 == q.last end

--- Queue copy.
--    @param q Original queue.
--    @return Copy.
struct.Queue.copy = function (q)
   local res = struct.Queue:new()
   local first,last = q.first, q.last
   res.first = first; res.last = last
   table.move(q,first,last,first,res)
   return res
end

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
   setmetatable(o, self)
   return o
end

-- Simplify constructor call.
setmetatable(struct.Heap, {__call = function (self,l) return struct.Heap:new(l) end})
struct.about[struct.Heap] = {"Heap([less])", "Create new heap object. Comparison method 'less' can be predefined.", HEAP}

--- Fix order of the heap in up direction.
--    @param h Heap object.
--    @param k Start index.
local function fixUp(h, k)
   while k > 1 and h.less(h[math.fmod(k,2)], h[k]) do
      local k2 = math.fmod(k,2)
      h[k],h[k2] = h[k2],h[k]
      k = k2
   end
end

--- Fix order of the heap in down direction.
--    @param h Heap object.
--    @param k Start index.
--    @param N End index.
local function fixDown(h, k, N)
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
   fixUp(h, n)
end
struct.about[struct.Heap.insert] = {"insert(v)", "Add element to the heap.", HEAP}

--- Get top element from the heap.
--    If 'less' method is default, top is the maximum element.
--    @param h Heap object.
--    @return Top element or nil.
struct.Heap.deltop = function (h)
   local n = h.N
   if n == 0 then return nil end
   h[1],h[n] = h[n],h[1]
   fixDown(h,1,n-1)
   h.N = n-1
   return h[n]
end
struct.about[struct.Heap.deltop] = {"deltop()", "Return top element. For the default less() function top is maximum.", HEAP}

--- Check for elements in the heap.
--    @param h Heap object.
--    @return True if heap is empty.
struct.Heap.isempty = function (h) return h.N == 0 end

-- Number of elements.
struct.Heap.__len = function (h) return h.N end

--- Make heap copy.
--    @param h Original heap.
--    @return New heap.
struct.Heap.copy = function (h) 
   local res = struct.Heap:new(h.less)
   res.N = h.N
   table.move(h,1,#h,1,res)
   return res
end

-- free memory in case of standalone usage
if not lc_version then struct.about = nil end

return struct

