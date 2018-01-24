--[[       liblc/struct.lua

--- Main data structures.
--  @author My Name

           module 'struct'
--]]

--------------- Tests ------------
--[[ !!
Struct = require 'liblc.struct'

]]

---------------------------------
-- @class table
-- @name struct
-- @field about Description of functions.
local struct = {}

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
struct.about = help:new("Main data structures.")

struct.stack = {}
struct.stack.__index = struct.stack
struct.stack.type = 'stack'

struct.stack.new = function (self)
   local o = {}
   setmetatable(o, self)
   return o
end

struct.Stack = function () return struct.stack:new() end

struct.stack.push = function (self, val)
   assert(val ~= nil)
   self[#self+1] = val
end

struct.stack.pop = function (self)
   local res = self[#self]
   if res then self[#self] = nil end
   return res
end

struct.queue = {}
struct.queue.__index = struct.queue
struct.queue.type = 'queue'

struct.queue.new = function (self)
   local o = {first=0, last=-1}
   setmetatable(o, self)
   return o
end

struct.Queue = function () return struct.queue:new() end

struct.queue.enqueue = function (self,val)
   local last = self.last+1
   self.last = last
   self[last] = val
end

struct.queue.pushfirst = function (self,val)
   local first = self.first-1
   self.first = first
   self[first] = val
end

struct.queue.dequeue = function (self)
   local first,val = self.first
   if first <= self.last then
      val = self[first]
      self[first] = nil
      self.first = first+1
   end
   return val
end

struct.queue.poplast = function (self)
   local last, val = self.last
   if self.first <= last then
      val = self[last]
      self[last] = nil
      self.last = last-1
   end
   return val
end

struct.queue.__len = function (t) return t.last-t.first+1 end

-- free memory in case of standalone usage
if not lc_version then struct.about = nil end

return struct

