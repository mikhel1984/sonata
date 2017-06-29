

local numeric = {}

--local help = require "liblc.help"
local help = require 'help'
numeric.about = help:new("Group of functions for numerical calculations")

numeric.EPS = 1e-4
numeric.about[numeric.EPS] = {"EPS", "The solution tolerance", help.CONST}

numeric.solve = function (fn, a, b)
   local f0, f1 = fn(a), fn(b)
   assert(f0*f1 < 0, "Boundary values must have different sign!")
   repeat
      b = b - (b-a)*f1/(f1-f0)
      f1 = fn(b)
   until math.abs(f1) < numeric.EPS
   return b
end
numeric.about[numeric.solve] = {"solve(fn,a,b)", "Find root of equation fn(x)=0 in interval [a,b]", help.BASE}


--- test

print(numeric.solve(math.sin, 1.57, 1.57*3))
