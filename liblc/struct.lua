--[[       liblc/struct.lua

--- Main data structures.
--  @author My Name

           module 'struct'
--]]

--------------- Tests ------------
--[[!!
Struct = require 'liblc.struct'

a = Struct.Stack()
a:push(1)
a:push(2)
ans = #a                        --> 2

a:push(3)
ans = a:pop()                   --> 3

a:pop()
ans = a:pop()                   --> 1

b = Struct.Queue()
b:enqueue(1)
b:enqueue(2)
ans = b:dequeue()               --> 1

b:pushfirst(4)
ans = b:poplast()               --> 2

ans = #b                        --> 1
]]

local STACK = 'stack'
local QUEUE = 'queue'

---------------------------------
-- @class table
-- @name struct
-- @field about Description of functions.
local struct = {}

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
struct.about = help:new("Main data structures.")

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

--- Alias for stack constructor.
--    @return New stack.
--struct.Stack = function () return struct.stack:new() end
setmetatable(struct.Stack, {__call = function (self) return struct.Stack:new() end})
struct.about[struct.Stack] = {"Stack()", "Create new stack.", STACK}

--- Add element to the stack.
--    @param val Value to push (except nil).
struct.Stack.push = function (self, val)
   assert(val ~= nil)
   self[#self+1] = val
end
struct.about[struct.Stack.push] = {"push(val)", "Push value to the stack, except nil.", STACK}

--- Get value from the top of stack.
--    @return Top value or nil.
struct.Stack.pop = function (self)
   local res = self[#self]
   if res then self[#self] = nil end
   return res
end
struct.about[struct.Stack.pop] = {"pop()", "Pop value from the stack, return element or nil.", STACK}

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

--struct.Queue = function () return struct.queue:new() end
setmetatable(struct.Queue, {__call = function (self) return struct.Queue:new() end})
struct.about[struct.Queue] = {"Queue()", "Create new queue.", QUEUE}

--- Put value to the end of queue.
--    @param val Value to put.
struct.Queue.enqueue = function (self,val)
   local last = self.last+1
   self.last = last
   self[last] = val
end
struct.about[struct.Queue.enqueue] = {"enqueue(val)", "Add value to the back of queue.", QUEUE}

--- Put value to the top of queue (as in stack).
--    @param val Value to put.
struct.Queue.pushfirst = function (self,val)
   local first = self.first-1
   self.first = first
   self[first] = val
end
struct.about[struct.Queue.pushfirst] = {"pushfirst(val)", "Add value to the top of queue.", QUEUE}

--- Get value from the top of queue.
--    @return Top value of nil.
struct.Queue.dequeue = function (self)
   local first,val = self.first
   if first <= self.last then
      val = self[first]
      self[first] = nil
      self.first = first+1
   end
   return val
end
struct.about[struct.Queue.dequeue] = {"dequeue()", "Get value from the top of queue.", QUEUE}

--- Get value from the end of queue.
--    @return Last value of nil.
struct.Queue.poplast = function (self)
   local last, val = self.last
   if self.first <= last then
      val = self[last]
      self[last] = nil
      self.last = last-1
   end
   return val
end
struct.about[struct.Queue.poplast] = {"poplast()", "Get value from the end of queue.", QUEUE}

--- Queue size.
--    @return Number of elements in queue.
struct.Queue.__len = function (t) return t.last-t.first+1 end

-- free memory in case of standalone usage
if not lc_version then struct.about = nil end

return struct

