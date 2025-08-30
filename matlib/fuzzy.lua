--[[		sonata/matlib/fuzzy.lua

--- Fuzzy logic..
--
--  </br></br><b>Authors</b>: Your Name

	module 'fuzzy'
--]]

-- Define here your tests, save results to 'ans',
-- use --> for the strict equality
-- and --n> for the n-digit precision in the case of floating numbers.
--[[TEST_IT

-- use 'fuzzy'
Fz = require 'matlib.fuzzy'

-- example
a = Fz()
-- check equality
ans = a.type                  -->  'fuzzy'

-- check relative equality ( ~10^(-2) )
ans = math.pi               --.2> 355/113

--]]


--	LOCAL

local _foo = 42

--	INFO

local _help = SonataHelp or {}  -- optional
-- description
local _about = {
__module__ = "Fuzzy logic."
}


--	MODULE

local fuzzy = {
-- mark
type = 'fuzzy',
}
-- methametods
fuzzy.__index = fuzzy


--- Check object type.
--  @param v Object.
--  @return True if the object is fuzzy.
local function _isfuzzy(v) return getmetatable(v) == fuzzy end


--- Constructor example.
--  @param t Some value.
--  @return New object of fuzzy.
fuzzy.new = function(self, t)
  local o = {}
  -- your logic
  -- return object
  return setmetatable(o, self)
end
_about[fuzzy.new] = {":new(t) --> F", "Explicit constructor.", _help.NEW}
-- begin from ':' to get 'Fz:new(t)'


-- simplify constructor call
setmetatable(fuzzy, {__call = function (self, v) return fuzzy:new(v) end})
_about[fuzzy] = {" (t) --> F", "Create new fuzzy.", _help.NEW}
-- begin from ' ' to get 'Fz ()'


--- Method example.
--  It is good idea to define method for the copy creation.
--  @return Copy of the object.
fuzzy.copy = function (self)
  -- some logic
  return fuzzy:new(argument)
end
_about[fuzzy.copy] = {"F:copy() --> cpy_F",
  "Create a copy of the object."} -- third element is optional, default is 'base'


-- Comment to remove descriptions
fuzzy.about = _about

return fuzzy

--======================================
--TODO: write new functions
