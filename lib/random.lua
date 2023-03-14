--[[		sonata/lib/random.lua

--- Random number generators
--  @author Your Name

	module 'random'
--]]

--[[TEST

-- use 'random'
Rand = require 'lib.random'

-- example
a = Rand()
ans = a.type   -->  'random'

ans = math.pi  --2> 355/113

--]]

--	LOCAL

--- Check object type.
--  @param v Object.
--  @return True if the object is random.
local function israndom(v) return type(v)=='table' and v.israndom end

--	INFO

local help = SonataHelp or {}  -- optional
-- description
local about = {
__module__ = "Random number generators."
}

--	MODULE

local random = {
-- mark
type = 'random', israndom = true,
-- components
_fn = math.random,
_fnRng = math.random,
_seed = 0,
}

random.__call = function (R) return R._fn() end

-- methametods
random.__index = random

--- Get copy of object.
--  @param R Generator object.
--  @return Copy of the object.
random.copy = function (R)
  local cpy = {
    _fn = R._fn,
    _fnRng = R._fnRng,
    _seed = R._seed,
  }
  return setmetatable(cpy, random)
end
about[random.copy] = {"R:copy() --> cpy_R", "Create a copy of the object."}

random.flip = function (R) return R._fn() >= 0.5 end
about[random.flip] = {":flip() --> bool", "Uniform distributed binary value."}

random.int = function (R, N) return R._fnRng(1, N) end
about[random.int] = {":int(N) -> int", "Uniform distributed random integer in range from 1 to N."}

--- Constructor example.
--  @param t Some value.
--  @return New object of random.
random.new = function(self)
  local o = {
    _fn = math.random,
    _fnRng = math.random,
    _seed = 0,
  }
  return setmetatable(o, self)
end
about[random.new] = {":new() --> R", "Create generator object."}

random.norm = function (R, dMean, dev)
  dMean = dMean or 0
  dev = dev or 1
  -- use Box-Muller transform
  local u, v, s = 0, 0, 0
  local fn = R._fn
  repeat
    u = 2*fn()-1
    v = 2*fn()-1
    s = u*u + v*v
  until s <= 1 and s > 0
  local norm = u * math.sqrt(-2*math.log(s)/s)
  return norm * dev + dMean
end
about[random.norm] = {":norm(mean_d=0, dev_d=1) --> float",
  "Normal distributed random value with the given mean and deviation."}

random.seed = function (R, N)
  N = N or os.time()  -- set 'arbitrary' value by default
  if R._fn == math.random then
    math.randomseed(N)
  end
  R._seed = N
end
about[random.seed] = {":seed() --> nil", "Set random generator seed."}

-- simplify constructor call
setmetatable(random, {__call = function (self) return random._fn() end})
about[random] = {" () --> float", "Uniform distributed random number between 0 and 1."}

-- Comment to remove descriptions
random.about = about

return random

--======================================
--TODO: write new functions
