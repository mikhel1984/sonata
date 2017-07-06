

local numeric = {}

--local help = require "liblc.help"
local help = require 'help'
numeric.about = help:new("Group of functions for numerical calculations")

numeric.TOL = 1e-3
numeric.about[numeric.TOL] = {"TOL", "The solution tolerance", help.CONST}

numeric.solve = function (fn, a, b)
   local f0, f1 = fn(a), fn(b)
   assert(f0*f1 < 0, "Boundary values must have different sign!")
   repeat
      b = b - (b-a)*f1/(f1-f0)
      f1 = fn(b)
   until math.abs(f1) < numeric.TOL
   return b
end
numeric.about[numeric.solve] = {"solve(fn,a,b)", "Find root of equation fn(x)=0 in interval [a,b]", help.BASE}

-- simple derivative
numeric.diff = function (fn, x)
   local dx = 2e-2
   local der, last = (fn(x+dx)-fn(x-dx))/(2*dx)
   repeat
      dx = dx * 0.5
      der, last = (fn(x+dx)-fn(x-dx))/(2*dx), der
   until math.abs(der-last) < numeric.TOL
   return der
end

-- Simpson's method
numeric.int = function (fn, a, b)
   local steps = 100
   local dx = (b-a) / steps
   local even, odd, x = 0, 0, a
   for i = 1, (steps-1)/2 do
      odd = odd + fn(x);   x = x + dx
      even = even + fn(x); x = x + dx
   end
   local fa, fb = fn(a), fn(b)
   odd = odd - fa
   even = even - fb
   return (fa + fb + 4*odd + 2*even) * dx / 3
end

-- Runge Kutt
numeric.ode = function (fn, x0,y0,xn)
   local N = 100
   local h = (xn-x0)/N
   local h2 = 0.5*h
   local k1, k2, k3, k4
   local res = {{x0,y0}}
   for i = 2, N do
      local v = res[i-1]
      k1 = fn(v[1],    v[2])
      k2 = fn(v[1]+h2, v[2]+h2*k1)
      k3 = fn(v[1]+h2, v[2]+h2*k2)
      k4 = fn(v[1]+h,  v[2]+h*k3)
      res[i] = {v[1]+h, v[2]+h*(k1+2*k2+2*k3+k4)/6}
   end
   return res, res[#res][2]
end

--- test

--print(numeric.solve(math.sin, 1.57, 1.57*3))
print(numeric.diff(math.sin, 0))
--print(numeric.int(math.sin, 0, 3.1415))
--dy = function (x,y) return math.sqrt(1-y*y) end
--print(numeric.ode(dy, 0,0,1.57))
