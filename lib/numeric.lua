--[[		sonata/lib/numeric.lua 

--- Numerical solutions for some mathematical problems.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2021.

	module 'numeric'
--]]

-------------------- Tests -------------------
--[[TEST

-- use 'numeric'
Num = require 'lib.numeric'

-- define tolerance
Num.TOL = 1e-4
-- solve 'sin(x) = 0' for x in (pi/2...3*pi/2)
a = Num.solve(math.sin, math.pi*0.5, math.pi*1.5)
ans = a                      --3> math.pi

-- Newton method
-- only one initial value
d = Num.Newton(math.sin, math.pi*0.7)
ans = d                      --3> math.pi

-- numeric derivative
b = Num.der(math.sin, 0)
ans = b                      --0> 1

-- numeric integral
c = Num.trapez(math.sin, 0, math.pi)
ans = c                      --0> 2

-- solve ODE x*y = x'
-- for x = 0..3, y(0) = 1
-- return table of solutions and y(3)
tbl, yn = Num.ode45(function (x,y) return x*y end, 
                    {0,3}, 1)
ans = yn                     --2> 90.011

-- use matrices for high order equations
Mat = require 'lib.matrix'

-- y''-2*y'+2*y = 1
-- represent as: x1 = y, x2 = y'
-- so: x1' = x2, x2' = 1+2*x2-2*x1
myfun = function (t,x) 
  return Mat.V {x(2), 1+2*x(2)-2*x(1)}
end
_, xn = Num.ode45(myfun, {0,2}, Mat.V{3,2}, {dt=0.2}) 
ans = xn(1)                  --2>  -10.54

-- define exit condition
-- from time, current and previous results
cond = function (time,current,previous) 
  return current < 0.1 
end
myfun = function (t,x) return -x end
y = Num.ode45(myfun, {0,1E2}, 1, {exit=cond})
ans = y[#y][1]               --2> 2.56

--]]

--	LOCAL

local Ver = require("lib.utils").versions

-- Runge-Kutta method.
-- @param fn Function f(x,y).
-- @param x First variable.
-- @param y Second variable.
-- @param h Step.
-- @return Approximation for y.
local function rk(fn, dX, dY, dH)
  local h2 = 0.5*dH
  local k1 = fn(dX,    dY)
  local k2 = fn(dX+h2, dY+h2*k1)
  local k3 = fn(dX+h2, dY+h2*k2)
  local k4 = fn(dX+dH, dY+dH*k3)
  return dY+dH*(k1+2*(k2+k3)+k4)/6
end

--	INFO

local help = SonataHelp or {new=function () return {} end}
-- description
local about = help:new("Group of functions for numerical calculations. Tolerance for all functions is defined with parameter TOL.")

--	MODULE

local numeric = {
-- current tolerance
TOL = 1E-3,
-- max Newton algorithm attempts
newton_max = 50,
}

about[numeric.TOL] = {"TOL[=0.001]", "The solution tolerance.", "parameters"}

--- Find root of equation at the given interval.
--  @param fn Function to analyze.
--  @param a Lower bound.
--  @param b Upper bound.
--  @return Function root.
numeric.solve = function (fn, dA, dB)
  local f0, f1 = fn(dA), fn(dB)
  if f0*f1 >= 0 then error("Boundary values must have different sign!") end
  repeat
    dB = dB - (dB-dA)*f1/(f1-f0)
    f1 = fn(dB)
  until math.abs(f1) < numeric.TOL
  return dB 
end
about[numeric.solve] = {"solve(fn,dA,dB)", "Find root of equation fn(x)=0 at interval [a,b]."}

--- Another solution based on Newton's rule.
--  @param fn Function to analyze.
--  @param d1 Initial value of the root.
--  @return Function root of <code>nil</code>.
numeric.Newton = function (fn, d1)
  local h, k, x2 = 0.1, 0, d1
  repeat
    d1 = x2
    local fd1 = fn(d1)
    x2 = d1 - fd1*h/(fn(d1+h)-fd1)
    k, h = k+1, h*0.618
    if k > numeric.newton_max then error("Too many iterations!") end
  until math.abs(fn(x2)-fd1) < numeric.TOL 
  return x2
end
about[numeric.Newton] = {"Newton(fn,d0)", "Find root of equation using Newton's rule with only one initial condition."}

--- Simple derivative.
--  @param fn Function f(x).
--  @param d Parameter.
--  @return Numerical approximation of the derivative value.
numeric.der = function (fn, d)
  local dx = 2e-2
  local der, last = (fn(d+dx)-fn(d-dx))/(2*dx)
  repeat
    dx = dx * 0.5
    der, last = (fn(d+dx)-fn(d-dx))/(2*dx), der
  until math.abs(der-last) < numeric.TOL
  return der
end
about[numeric.der] = {"der(fn,x)", "Calculate the derivative value for given function."}

--- Integration using trapeze method.
--  @param fn Function f(x).
--  @param a Lower bound.
--  @param b Upper bound.
--  @return Numerical approximation of the integral.
numeric.trapez = function (fn, dA, dB)
  local N, sum = 10, 0
  local fab = (fn(dA)+fn(dB)) * 0.5
  local x, dx, N = dA, (dB-dA)/N, N-1
  -- initial approximation
  for i = 1, N do
    x = x + dx
    sum = sum + fn(x)
  end
  local I, last = (fab + sum) * dx
  local Nsum, Nlast = 0, N
  -- correct
  repeat
    last, x, N = I, dA + 0.5*dx, N+Nsum+1
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
about[numeric.trapez] = {"trapez(fn,a,b)", "Get integral using trapezoidal rule."}

--- Differential equation solution (Runge-Kutta method).
--  @param fn function f(t,y).
--  @param tDelta Time interval {t0,tn}
--  @param y0 Function value at time t0.
--  @param param Table of additional parameters: dt - time step, exit - exit condition
--  @return Table of intermediate results and value in final point.
numeric.ode45 = function (fn,tDelta,dY0,tParam)
  local MAX, MIN = 15*numeric.TOL, 0.1*numeric.TOL
  local xn = tDelta[2]
  local h = tParam and tParam.dt or (10*numeric.TOL)
  local exit = tParam and tParam.exit or function () return false end
  -- evaluate
  local res = {{tDelta[1],dY0}}        -- save intermediate points
  local upack = Ver.unpack
  while res[#res][1] < xn do
  --repeat
    local x,y = upack(res[#res])
    h = math.min(h, xn-x)
    -- correct step
    if tParam and tParam.dt then
      res[#res+1] = {x+h, rk(fn,x,y,h)}
    else
      -- step correction
      local h2 = 0.5*h
      local y1 =  rk(fn, x, y, h)
      local y2 =  rk(fn, x+h2, rk(fn,x,y,h2), h2)
      local dy = (type(y1) == 'table') and (y2-y1):norm() or math.abs(y2-y1)
      if dy > MAX then 
        h = h2
      elseif dy < MIN then
        h = 2*h
      else
        -- save for current step
        res[#res+1] = {x+h, y2}    -- use y2 instead y1 because it is probably more precise (?)
      end
    end
    if exit(res[#res][1],res[#res][2], (#res>1) and res[#res-1][2]) then break end 
  end
  return res, res[#res][2]
end
about[numeric.ode45] = {"ode45(fn,tDelta,y0[,param])", "Numerical approximation of the ODE solution.\nFirst parameter is differential equation, second - time interval, third - initial function value. List of parameters is optional and can includes time step or exit condition.\nReturn table of intermediate points and result yn."}

-- Comment to remove descriptions
numeric.about = about

return numeric

--===============================
-- TODO: check ode solver