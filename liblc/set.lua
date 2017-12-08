--[[      liblc/set.lua 

--- Manipulation with sets.
--  Based on implementation of Roberto Ierusalimschy.
--  @author Stanislav Mikhel, 2017
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection.

            module 'set'
--]]

-------------------- Tests -------------------
--[[!!
Set = require 'liblc.set'

a = Set {1,2,3,4,1}           
b = Set {3,4,5}               
ans = a                        --> Set {1,2,3,4}

ans = b:check(6)               --> false

b:insert(6)
ans = b:check(6)               
b:remove(6)                    --> true

ans = a + b                    --> Set {1,2,3,4,5}

ans = a * b                    --> Set {3,4}

ans = a / b                    --> Set {1,2}

ans = (a == b)                 --> false

ans = (a < b)                  --> false

t = a:table()
ans = a:check(t[1])            --> true

ans = #a                       --> 4

print(a)
]]

local NOT_A_SET = "Set is expected!"
-------------------------------------------- 
-- @class table
-- @name set
-- @field type Define object type string.
-- @field about Function description collection.
local set = {}
set.__index = set
-- mark
set.type = 'set'
set.isset = true
-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
set.about = help:new("Manipulation with sets.")

--- Check object type.
--    <i>Private function.</i>
--    @param c Object for checking.
--    @return <code>true</code> if table is a set.
local function isset(s) return type(s) == 'table' and s.isset end

--- Create new object, set metatable.
--    @param l Table of elements.
--    @return New set object.
function set:new(l)
   local o = {}
   for i = 1, #l do o[l[i]] = true end
   setmetatable(o, self)
   return o
end

--- Check if value is in set.
--    @param s Source set.
--    @param v Element to check.
--    @return <code>true</code> if the element is in the set.
set.check = function (s, v)
   assert(isset(s), NOT_A_SET)
   return s[v] == true
end
set.about[set.check] = {"check(set,val)", "Check if value is in set.", help.OTHER}

--- Add new element.
--    @param s Set object.
--    @param v New element.
set.insert = function (s, v)
   assert(isset(s), NOT_A_SET)
   s[v] = true
end
set.about[set.insert] = {"insert(set,val)", "Insert element into set.", help.OTHER}

--- Delete element.
--    @param s Set object.
--    @param v Element.
set.remove = function (s,v)
   assert(isset(s), NOT_A_SET)
   s[v] = nil
end
set.about[set.remove] = {"remove(set,val)", "Remove element from set.", help.OTHER}

--- Convert into lua table.
--    @param s Set object.
--    @return List of elements.
set.table = function (s)
   local res = {}
   for k in pairs(s) do table.insert(res, k) end
   return res
end
set.about[set.table] = {"table(set)", "Represent set as a table.", help.OTHER}

--- a + b
--    @param a First set.
--    @param b Second set.
--    @return Union.
set.__add = function (a,b)
   assert(isset(a) and isset(b), NOT_A_SET)
   local res = set:new({})
   for k in pairs(a) do res[k] = true end
   for k in pairs(b) do res[k] = true end
   return res
end

--- a * b
--    @param a First set.
--    @param b Second set.
--    @return Intersection.
set.__mul = function (a,b)
   assert(isset(a) and isset(b), NOT_A_SET)
   local res = set:new({})
   for k in pairs(a) do res[k] = b[k] end
   return res
end

--- a / b
--    @param a First set.
--    @param b Second set.
--    @return Difference.
set.__div = function (a,b)
   assert(isset(a) and isset(b), NOT_A_SET)
   local res = set:new({})
   for k in pairs(a) do
      if not b[k] then res[k] = true end
   end
   return res
end

set.arithmetic = 'arithmetic'
set.about[set.arithmetic] = {"union, intersection, defference", "a+b, a*b, a/b", help.BASE}

--- a <= b
--    @param a First set.
--    @param b Second set.
--    @return <code>true</code> if <code>a</code> is a subset of <code>b</code>.
set.__le = function (a,b)
   assert(isset(a) and isset(b), NOT_A_SET)
   for k in pairs(a) do
      if not b[k] then return false end
   end
   return true
end

--- a < b
--    @param a First set.
--    @param b Second set.
--    @return <code>true</code> if <code>a</code> is a subset of <code>b</code> but not equial.
set.__lt = function (a,b)
   return a <= b and not (b <= a)
end

--- a == b
--    @param a First set.
--    @param b Second set.
--    @return <code>true</code> if <code>a</code> and <code>b</code> are equial.
set.__eq = function (a,b)
   return a <= b and b <= a
end

set.comparation = 'comparation'
set.about[set.comparation] = {set.comparation, "a==b, a~=b, a<b, a<=b, a>b, a>=b", help.BASE}

--- #a 
--    @param s Set object.
--    @return Number of elements.
set.__len = function (s)
   local n = 0
   for k in pairs(s) do n = n+1 end
   return n
end

--- String representation.
--    @param s Set object.
--    @return Set as string.
set.__tostring = function (s)
   local lst = {}
   for e in pairs(s) do table.insert(lst, e) end
   return string.format('{%s}', table.concat(lst,','))
end

-- redefine constructor
setmetatable(set, {__call = function (self, v) return set:new(v) end})
set.Set = 'Set'
set.about[set.Set] = {"Set(t)", "Create new set from table of elements.", help.NEW}

--- Set serialization.
--    @param obj Set object.
--    @return String, suitable for exchange.
set.serialize = function (obj)
   local s = {}
   for k in pairs(obj) do 
      if type(k) == 'string' then
         s[#s+1] = string.format("['%s']=true", k)
      else
         s[#s+1] = string.format("[%s]=true", tostring(k)) 
      end
   end
   s[#s+1] = "metatablename='Set'"
   s[#s+1] = "modulename='set'"
   return string.format("{%s}", table.concat(s, ','))
end
set.about[set.serialize] = {"serialize(obj)", "Save internal representation of the set.", help.OTHER}

-- free memory if need
if not lc_version then set.about = nil end

return set
