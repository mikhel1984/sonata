--[[		sonata/lib/numeric.lua

--- Numerical solutions for some mathematical problems.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2023.

	module 'numeric'
--]]


-------------------- Tests -------------------
--[[TEST_IT

-- use 'numeric'
Num = require 'matlib.numeric'
-- use matrices for high order equations
Mat = require 'matlib.matrix'

-- define tolerance
Num.TOL = 1e-4
-- solve 'sin(x) = 0' for x in (pi/2...3*pi/2)
a = Num:solve(math.sin, math.pi*0.5, math.pi*1.5)
ans = a                      --3>  math.pi

-- Newton method
-- only one initial value
d = Num:newton(math.sin, math.pi*0.7)
ans = d                      --3>  math.pi

-- numeric derivative
b = Num:der(math.sin, 0)
ans = b                      --0>  1

-- numeric limit
fn = function (x) return math.sin(x) / x end
ans = Num:lim(fn, 0)         --3>  1.0

-- inf limit
ans = Num:lim(math.exp, -math.huge)  --3> 0.0

-- numeric integral
c = Num:trapez(math.sin, 0, math.pi)
ans = c                      --0>  2

-- solve ODE x*y = x'
-- for x = 0..3, y(0) = 1
-- return table of solutions and y(3)
tbl = Num:ode45(function (x,y) return x*y end,
                {0,3}, 1)
ans = tbl[#tbl][2]           --2>  90.011

-- y''-2*y'+2*y = 1
-- represent as: x1 = y, x2 = y'
-- so: x1' = x2, x2' = 1+2*x2-2*x1
myfun = function (t,x)
  return Mat:V {x(2), 1+2*x(2)-2*x(1)}
end
res = Num:ode45(myfun, {0,2}, Mat:V{3,2}, {dt=0.2})
xn = res[#res][2]  -- last element
ans = xn(1)                  --2>   -10.54

-- define exit condition
cond = function (states)
  -- check current value
  return states[#states][2] < 0.1
end
myfun = function (t,x) return -x end
y = Num:ode45(myfun, {0, 1E2}, 1, {exit=cond})
-- time of execution before break
ans = y[#y][1]               --1>  2.3

--]]


--	LOCAL

local Vunpack, Cnorm do
  local lib = require("matlib.utils")
  Vunpack = lib.versions.unpack
  Cnorm = lib.cross.norm
end
local inform = Sonata and Sonata.warning or print


-- Runge-Kutta method.
-- @param fn Function f(x,y).
-- @param x First variable.
-- @param y Second variable.
-- @param h Step.
-- @return Approximation for y.
local function rk(fn, x, y, h)
  local h2 = 0.5*h
  local k1 = fn(x,    y)
  local k2 = fn(x+h2, y+h2*k1)
  local k3 = fn(x+h2, y+h2*k2)
  local k4 = fn(x+h,  y+h*k3)
  return y + (k1 + 2*(k2 + k3) + k4)*(h/6)
end


--	INFO

-- description
local about = { __module__ =
  "Group of functions for numerical calculations."
}


--	MODULE

local numeric = {
-- current tolerance
TOL = 1E-3,
-- max Newton algorithm attempts
NEWTON_MAX = 50,
SMALL = 1E-20,
}


--- Simple derivative.
--  @param fn Function f(x).
--  @param d Parameter.
--  @return Numerical approximation of the derivative value.
numeric.der = function (self, fn, d)
  local dx = 2e-2
  local der, last = (fn(d+dx) - fn(d-dx)) / (2*dx), nil
  repeat
    local d2 = dx
    dx = dx * 0.5
    der, last = (fn(d+dx) - fn(d-dx)) / d2, der
  until dx < numeric.SMALL or Cnorm(der-last) < numeric.TOL
  return der
end
about[numeric.der] = {":der(fn, x_d) --> num",
  "Calculate the derivative value for given function."}


--- Estimate lim(fn(x)) for x -> xn.
--  @param fn Function.
--  @param xn Value to approach.
--  @param isPositive Flag for +/-xn.
--  @return The result and flag of success.
numeric.lim = function (self, fn, xn, isPositive)
  local prev = nil
  if -math.huge < xn and xn < math.huge then
    -- limited number
    local del = 1
    while del > numeric.SMALL do
      local curr = isPositive and fn(xn + del) or fn(xn - del)
      if prev and Cnorm(curr - prev) < numeric.TOL then
        return curr, true
      end
      del, prev = del*1E-3, curr
    end
  else 
    -- +/- inf
    xn = (xn < math.huge) and -1 or 1
    while math.abs(xn) < math.huge do
      local curr = fn(xn)
      if prev and Cnorm(curr - prev) < numeric.TOL then
        return curr, true
      end
      xn, prev = xn * 1E3, curr
    end
  end
  return prev, false
end
about[numeric.lim] = {":lim(fn, xn_d, isPositive) --> y, isFound",
  "Estimate limit of a function."}


--- Solve equation based on Newton's rule.
--  @param fn Function to analyze.
--  @param d1 Initial estimation for the root.
--  @return root of nil.
numeric.newton = function (self, fn, d1)
  local h, k, x2 = 0.1, 0, d1
  repeat
    d1 = x2
    local fd1 = fn(d1)
    x2 = d1 - fd1*h / (fn(d1+h) - fd1)
    k, h = k+1, h*0.618
    if k > numeric.NEWTON_MAX then 
      inform("newton: too many iterations") 
      break
    end
  until Cnorm(fn(x2)-fd1) < numeric.TOL
  return x2
end
about[numeric.newton] = {":newton(fn, x0_d) --> num",
  "Find root of equation using Newton's rule."}


--- Differential equation solution (Runge-Kutta method).
--  @param fn function f(t,y).
--  @param tDelta Time interval {t0,tn}
--  @param y0 Function value at time t0.
--  @param param Table of additional parameters: dt - time step, exit - exit condition
--  @return Table of intermediate results.
numeric.ode45 = function (self, fn, tDelta, dY0, tParam)
  local MAX, MIN = 10*numeric.TOL, 0.1*numeric.TOL
  local xn = tDelta[2]
  tParam = tParam or {}
  local h = tParam.dt or math.min((xn - tDelta[1]), 1.0) / 20
  local exit = tParam.exit or function () return false end
  -- evaluate
  local res, last = {{tDelta[1], dY0}}, false
  while not exit(res) do
    local x, y = Vunpack(res[#res])
    if x >= xn then 
      break
    elseif x + h > xn then
      h, last = xn-x, true
    end
    -- find next
    if tParam.dt or last then
      res[#res+1] = {x+h, rk(fn, x, y, h)}
    else
      -- step correction
      local h2 = 0.5 * h
      local y1 = rk(fn, x, y, h)
      local y2 = rk(fn, x+h2, rk(fn, x, y, h2), h2)
      local dy = Cnorm(y2 - y1)
      if dy > MAX then
        h = h2
      else
        -- use y2 because it should be more precise (?)
        res[#res+1] = {x+h, y2}
        if dy < MIN then h = 2*h end
      end
    end
  end
  return res
end
about[numeric.ode45] = {":ode45(fn, interval_t, y0, {dt=del/20,exit=nil}) --> ys_t",
  "Numerical approximation of the ODE solution.\nList of parameters is optional and can includes time step and exit condition.\nReturn table of intermediate points in form {t, x(t)}."}


--- Find root of equation at the given interval.
--  @param fn Function to analyze.
--  @param a Lower bound.
--  @param b Upper bound.
--  @return Function root.
numeric.solve = function (self, fn, a, b)
  local f0, f1 = fn(a), fn(b)
  if f0*f1 >= 0 then 
    error "Boundary values must have different sign!"
  end
  repeat
    b = b - (b-a)*f1 / (f1-f0)
    f1 = fn(b)
  until math.abs(f1) < numeric.TOL
  return b 
end
about[numeric.solve] = {":solve(fn, low_d, up_d) --> num",
  "Find root of equation fn(x)=0 at interval [a,b]."}


--- Integration using trapeze method.
--  @param fn Function f(x).
--  @param a Lower bound.
--  @param b Upper bound.
--  @return Numerical approximation of the integral.
numeric.trapez = function (self, fn, dA, dB)
  local N, sum = 10, 0
  local fab = (fn(dA)+fn(dB)) * 0.5
  local x, dx, N = dA, (dB-dA)/N, N-1  -- TODO why redefine N?
  -- initial approximation
  for i = 1, N do
    x = x + dx
    sum = sum + fn(x)
  end
  local I, last = (fab + sum) * dx, 0
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
about[numeric.trapez] = {":trapez(fn, x1_d, x2_d) --> num",
  "Get integral using trapezoidal rule."}


-- Comment to remove descriptions
numeric.about = about

return numeric

--===============================
