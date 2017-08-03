------------  numeric.lua ----------------
--
-- Numerical solutions for some mathematical problems.
--
-- This file is a part of liblc collection. 
-- Stanislav Mikhel, 2017.
----------------------------------------

-------------------- Tests -------------------
--[[!!
Num = require 'liblc.numeric'

Num.TOL = 1e-4
a = Num.solve(math.sin, math.pi*0.5, math.pi*1.5)
ans = a                                   --~ math.pi

d = Num.newton(math.sin, math.pi*0.7)
ans = d                                   --~ math.pi

b = Num.diff(math.sin, 0)
ans = b                                   --~ 1

c = Num.trapez(math.sin, 0, math.pi)      
ans = c                                   --~ 2

tbl, yn = Num.ode(function (x,y) return x*y end, 0, 1, 3)
ans = yn                                  --~ 90.011
]]
---------------------------------------------

local numeric = {}

local help = lc_version and (require "liblc.help") or {new=function () return {} end}
numeric.about = help:new("Group of functions for numerical calculations.")

-- current tolerance
numeric.TOL = 1e-3
numeric.about[numeric.TOL] = {"TOL", "The solution tolerance (0.001 by default).", help.CONST}

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
numeric.about[numeric.solve] = {"solve(fn,a,b)", "Find root of equation fn(x)=0 at interval [a,b].", help.BASE}

-- another solution based on Newton's rule
numeric.newton = function (fn, x1)
   local h, k, x2 = 0.1, 0, x1
   repeat
      x1 = x2
      x2 = x1 - fn(x1)*h/(fn(x1+h)-fn(x1))
      k, h = k+1, h*0.618
      if k > 50 then error("Too many circles!") end
   until math.abs(fn(x2)-fn(x1)) < numeric.TOL 
   return x2
end
numeric.about[numeric.newton] = {"newton(fn,x0)", "Find root of equation using Newton's rule, use only one initial condition", help.BASE}

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
numeric.about[numeric.diff] = {"diff(fn,x)", "Calculate the derivative value for given function.", help.BASE}

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
numeric.about[numeric.trapez] = {"trapez(fn,a,b)", "Get integral using trapezoidal rule", help.BASE}

-- Runge-Kutta method
local function rk(fn, x, y, h)
   local h2 = 0.5*h
   local k1 = fn(x,    y)
   local k2 = fn(x+h2, y+h2*k1)
   local k3 = fn(x+h2, y+h2*k2)
   local k4 = fn(x+h,  y+h*k3)
   return y+h*(k1+2*(k2+k3)+k4)/6
end

-- differential equation solution (Runge-Kutta method)
numeric.ode = function (fn, x0,y0,xn, dx)
   local h = dx or (xn-x0)/10      -- initial step
   local res = {{x0,y0}}           -- save intermediate points
   repeat
      local x,y = table.unpack(res[#res])
      h = math.min(h, xn-x)
      -- correct step
      if dx then
         res[#res+1] = {x+h, rk(fn,x,y,h)}
      else
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
      end
   until x + h >= xn 
   return res, res[#res][2]
end
numeric.about[numeric.ode] = {"ode(fn,x0,y0,xn[,dx])", "Numerical approximation of the ODE solution.\nIf step dx is not defined it is calculated automaticaly according the given tolerance.\nReturn table of intermediate points and result yn.", help.BASE}

return numeric
