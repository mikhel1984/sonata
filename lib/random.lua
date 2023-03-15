--[[		sonata/lib/random.lua

--- Random number generators.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2023.

	module 'random'
--]]

--[[TEST

-- use 'random'
Rand = require 'lib.random'

-- random from 0 to 1
v = Rand()
ans = (0 <= v and v <= 1)     --> true 

-- get random true/false
v = Rand:flip()
ans = type(v)                 --> 'boolean'

-- get integer from 1 to 10
v = Rand:int(10)
ans = (1 <= v and v <= 10)    --> true

-- 'normal' distribution
-- with mean 1 and dispertion 2
v = 0
for i = 1, 10 do
  v = v + Rand:norm(1, 2)
end
ans = v / 10                  --0> 1.0

-- new generator
rnd = Rand:new()
-- from 0 to 1
v = rnd()
ans = (0 <= v and v <= 1)     --> true 

-- get integer from 1 to 10
v = rnd:int(10)
ans = (1 <= v and v <= 10)    --> true

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
_fn = function (self) return math.random() end,
_fnRng = function (self, a, b) return math.random(a, b) end,
_seed = 0,
}

random.__call = function (R) return R:_fn() end

-- methametods
random.__index = random

random.flip = function (R) return R:_fn() >= 0.5 end
about[random.flip] = {":flip() --> bool", "Uniform distributed binary value."}

random.int = function (R, N) return R:_fnRng(1, N) end
about[random.int] = {":int(N) -> int", "Uniform distributed random integer in range from 1 to N."}

--- Constructor example.
--  @param t Some value.
--  @return New object of random.
random.new = function(self)
  local o = {
    _fn = random._fn,
    _fnRng = random._fnRng,
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
  repeat
    u = 2*R:_fn()-1
    v = 2*R:_fn()-1
    s = u*u + v*v
  until s <= 1 and s > 0
  local norm = u * math.sqrt(-2*math.log(s)/s)
  return norm * dev + dMean
end
about[random.norm] = {":norm(mean_d=0, dev_d=1) --> float",
  "Normal distributed random value with the given mean and deviation."}

random.seed = function (R, N)
  N = N or os.time()  -- set 'arbitrary' value by default
  if rawget(R, 'israndom') then
    math.randomseed(N)
  end
  R._seed = N
end
about[random.seed] = {":seed() --> nil", "Set random generator seed."}


-- simplify constructor call
setmetatable(random, {__call = function (self) return random._fn(self) end})
about[random] = {" () --> float", "Uniform distributed random number between 0 and 1."}

-- Comment to remove descriptions
random.about = about

return random

--======================================
--TODO: write new functions
