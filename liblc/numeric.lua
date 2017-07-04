

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

-- simple derivative
numeric.diff = function (fn, x)
   local dx = 1e-2
   return (fn(x+dx)-fn(x-dx))/(2*dx)
end

-- Simpson's method
numeric.int = function (fn, a, b)
   local steps = 101
   local dx = (b-a) / steps
   local even, odd, x = 0, 0, a
   for i = 1, (steps-1)/2 do
      odd = odd + fn(x);   x = x + dx
      even = even + fn(x); x = x + dx
   end
   local fa = fn(a)
   odd = odd - fa
   return (fa + fn(b) + 4*odd + 2*even) * dx / 3
end

--- test

--print(numeric.solve(math.sin, 1.57, 1.57*3))
--print(numeric.diff(math.sin, 0))
print(numeric.int(math.sin, 0, 3.1415))
