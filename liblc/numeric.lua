--[[        numeric.lua
Numerical solutions for some mathematical problems.

]]

local numeric = {}

--local help = require "liblc.help"
local help = require 'help'
numeric.about = help:new("Group of functions for numerical calculations")

-- current tolerance
numeric.TOL = 1e-3
numeric.about[numeric.TOL] = {"TOL", "The solution tolerance", help.CONST}

-- find root of equation
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

-- intergration using trapez method
numeric.trapez = function (fn, a, b)
   local N, sum = 10, 0
   local fab = (fn(a)+fn(b)) * 0.5
   local x, dx, N = a, (b-a)/N, N-1
   -- initial approximation
   for i = 1, N do
      x = x + dx
      sum = sum + fn(x)
   end
   local I, last = (fab + sum) * dx
   local Nsum, Nlast = 0, N
   -- correct
   repeat
      last, x, N = I, a + 0.5*dx, N+Nsum+1
      Nsum = Nsum + Nlast
      for i = 1, N do
         sum = sum + fn(x)
	 x = x + dx
      end
      dx, Nlast = dx * 0.5, N
      I = (fab + sum) * dx
   until math.abs(I-last) < numeric.TOL
   return I
end

local function rk(fn, x, y, h)
   local h2 = 0.5*h
   local k1 = fn(x,    y)
   local k2 = fn(x+h2, y+h2*k1)
   local k3 = fn(x+h2, y+h2*k2)
   local k4 = fn(x+h,  y+h*k3)
   return y+h*(k1+2*(k2+k3)+k4)/6
end

-- differential equation solution (Runge-Kutt)
numeric.ode = function (fn, x0,y0,xn)
   local h = (xn-x0)/10    -- initial step
   local res = {{x0,y0}}
   repeat
      local x,y = table.unpack(res[#res])
      h = math.min(h, xn-x)
      -- correct step
      local h2 = 0.5*h
      local y1 =  rk(fn, x, y, h)
      local y2 =  rk(fn, x+h2, rk(fn,x,y,h2), h2)
      local dy = math.abs(y1-y2)
      if dy > 15*numeric.TOL then 
         h = h2
      elseif dy < 0.1*numeric.TOL then
         h = 2*h
      else
         -- calculate for current step
         res[#res+1] = {x+h, y2}
      end
   until x + h >= xn 
   return res, res[#res][2]
end

--return numeric

--- test

--print(numeric.solve(math.sin, 1.57, 1.57*3))
--print(numeric.diff(math.sin, 0))
--print(numeric.trapez(math.sin, 0, 3.1415))
dy = function (x,y) return math.sqrt(1-y*y) end
print(numeric.ode(dy, 0,0,1.4))
