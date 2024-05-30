--[[		sonata/lib/numeric.lua

--- Numerical solutions for some mathematical problems.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2024.

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
ans = Num:lim(math.exp, -INF)  --3> 0.0

-- numeric integral
c = Num:int(math.sin, 0, math.pi)
ans = c                      --2>  2.0

-- infinite limits
ans = Num:int(
  function (x) return math.exp(-x*x) end, -INF, INF)  --2> math.sqrt(math.pi)

-- solve ODE x*y = y'
-- for x = 0..3, y(0) = 1
-- return table of solutions and y(3)
tbl = Num:ode(function (x,y) return x*y end,
                {0,3}, 1)
ans = tbl[#tbl][2]           --2>  90.011

-- y''-2*y'+2*y = 1
-- represent as: x1 = y, x2 = y'
-- so: x1' = x2, x2' = 1+2*x2-2*x1
myfun = function (t,x)
  return Mat:V {x(2), 1+2*x(2)-2*x(1)}
end
res = Num:ode(myfun, {0,2}, Mat:V{3,2}, {dt=0.2})
xn = res[#res][2]  -- last element
ans = xn(1)                  --2>   -10.54

-- define exit condition
cond = function (states)
  -- check current value
  return states[#states][2] < 0.1
end
myfun = function (t,x) return -x end
y = Num:ode(myfun, {0, 1E2}, 1, {exit=cond})
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


local ODE = 'ode'


--- Runge-Kutta method.
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


--- Trapez method step.
--  @param fn Function to integrate.
--  @param a Lower limit.
--  @param b Upper limit.
--  @param n Step number.
--  @param s Previous sum.
--  @return sum of points.
local function trapzd (fn, a, b, n, s)
  if n == 1 then
    return 0.5*(b - a)*(fn(a) + fn(b))
  end
  local tnm = 2^(n-2)
  local del = (b - a)/tnm
  local x, sum = a + 0.5*del, 0.0
  for i = 1, tnm do
    sum, x = sum + fn(x), x + del
  end
  return 0.5*(s + sum*del)
end

--- Step in extended midpoint rule.
--  @param fn Function to integrate.
--  @param a Lower limit.
--  @param b Upper limit.
--  @param n Step number.
--  @param s Previous sum.
--  @return sum of points.
local function midpnt (fn, a, b, n, s)
  if n == 1 then
    return (b - a)*fn(0.5*(a + b))
  end
  local tnm = 3^(n - 2)
  local del = (b - a)/(3.0 * tnm)
  local del2 = del + del
  local x, sum = a + 0.5*del, 0.0
  for j = 1, tnm do
    sum, x = sum + fn(x), x + del2
    sum, x = sum + fn(x), x + del
  end
  return s/3.0 + sum*del
end


--- Integration rule, similar to Simplon's.
--  @param fn Function to integrate.
--  @param a Lower limit.
--  @param b Upper limit.
--  @param eps Accuracy.
--  @param eval Method to evaluate each step.
--  @return integra value and flag if it converges.
local function qsimp (fn, a, b, eps, eval, N)
  local s, si = 0, -1e30
  local ost, st = si, 0
  for j = 1, N do
    st = eval(fn, a, b, j, st)
    s = (4.0*st - ost) / 3.0
    if j > 3 and math.abs(s-si) < eps then
      return s
    end
    si, ost = s, st
  end
  inform("integral: too many iterations")
  return si, true  -- error flag
end


--- Check if the number is limited.
--  @param x Number to check.
--  @return true when not infinite.
local function limited (x) return -math.huge < x and x < math.huge end


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
-- max integration attempts
INT_MAX = 20,
-- 'minima' step
SMALL = 1E-20,
}


-- check limits
if 1E300 < math.huge then
  -- 64 bits
  numeric.EPS = 2^(-52)
  numeric.FMAX = 2^1023
elseif 1E60 < math.huge then
  -- 32 bits
  numeric.EPS = 2^(-23)
  numeric.FMAX = 2^127
else
  -- 16 bits
  numeric.EPS = 2^(-10)
  numeric.FMAX = 2^15
end


--- Simple derivative.
--  @param fn Function f(x).
--  @param d Parameter.
--  @return Numerical approximation of the derivative value.
numeric.der = function (_, fn, d)
  local dx = 2e-2
  local der, last = (fn(d+dx) - fn(d-dx)) / (2*dx), nil
  repeat
    local d2 = dx
    dx = dx * 0.5
    der, last = (fn(d+dx) - fn(d-dx)) / d2, der
    if dx < numeric.SMALL then
      inform("derivative: not found")
      return der, true
    end
  until Cnorm(der-last) < numeric.TOL
  return der
end
about[numeric.der] = {":der(fn, x_d) --> num",
  "Calculate the derivative value for the given function."}


--- Replace vector with sequence of elements in ODE solver result.
--  @param t Table with ODE solution.
numeric.flat = function (t)
  for i = 1, #t do
    local ti = t[i]
    local v = ti[2]
    if type(v) == 'table' then
      -- replace with vector elements
      v = v:vec()
      for j = 1, #v do ti[j+1] = v[j] end
    end
  end
end
about[numeric.flat] = {"ys:flat() --> ys",
  "Transform vector to list for each rov in ODE output.", ODE}


--- Estimate lim(fn(x)) for x -> xn.
--  @param fn Function.
--  @param xn Value to approach.
--  @param isPositive Flag for +/-xn.
--  @return obtained value.
numeric.lim = function (_, fn, xn, isPositive)
  local prev = nil
  if limited(xn) then
    -- limited number
    local del = 1
    while del > numeric.SMALL do
      local curr = isPositive and fn(xn + del) or fn(xn - del)
      if prev and Cnorm(curr - prev) < numeric.TOL then
        return curr
      end
      del, prev = del*1E-3, curr
    end
  else
    -- +/- inf
    xn = (xn < math.huge) and -1 or 1
    while math.abs(xn) < math.huge do
      local curr = fn(xn)
      if prev and Cnorm(curr - prev) < numeric.TOL then
        return curr
      end
      xn, prev = xn * 1E3, curr
    end
  end
  inform('limit: not found')
  return prev, true  -- error flag
end
about[numeric.lim] = {":lim(fn, xn_d, isPositive=false) --> y",
  "Estimate limit of a function."}


--- Solve equation based on Newton's rule.
--  @param fn Function to analyze.
--  @param d1 Initial estimation for the root.
--  @return root of nil.
numeric.newton = function (_, fn, d1)
  local h, k, x2 = 0.1, 0, d1
  repeat
    d1 = x2
    local fd1 = fn(d1)
    x2 = d1 - fd1*h / (fn(d1+h) - fd1)
    k, h = k+1, h*0.618
    if k > numeric.NEWTON_MAX then
      inform("newton: too many iterations")
      return x2, true  -- error flag
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
numeric.ode = function (_, fn, tDelta, dY0, tParam)
  local MAX, MIN = 10*numeric.TOL, 0.1*numeric.TOL
  local xn = tDelta[2]
  tParam = tParam or {}
  local h = tParam.dt or math.min((xn - tDelta[1]), 1.0) / 20
  local exit = tParam.exit or function (_) return false end
  -- evaluate
  local res, last = {{tDelta[1], dY0}}, false
  res.flat = numeric.flat
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
about[numeric.ode] = {":ode(fn, interval_t, y0, {dt=del/20,exit=nil}) --> ys_t",
[[Numerical approximation of the ODE solution.
List of parameters is optional and can includes time step and exit condition.
Return table of intermediate points in form {t, x(t)}.]], ODE}


--- Find root of equation at the given interval.
--  @param fn Function to analyze.
--  @param a Lower bound.
--  @param b Upper bound.
--  @return Function root.
numeric.solve = function (_, fn, a, b)
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
  "Find root of equation fn(x)=0 on interval [a,b]."}


--- Integration using trapeze method.
--  @param fn Function f(x).
--  @param a Lower bound.
--  @param b Upper bound.
--  @return Numerical approximation of the integral.
numeric.int = function (_, fn, a, b)
  -- check arguments
  if a > b then
    return numeric.int(nil, fn, b, a)
  elseif a == b then
    return 0, true
  end
  local N, TOL = numeric.INT_MAX, numeric.TOL
  -- check inf
  local afin, bfin = limited(a), limited(b)
  if afin and bfin then
    return (limited(fn(a)) and limited(fn(b)))
      and qsimp(fn, a, b, TOL, trapzd, N)
      or  qsimp(fn, a, b, TOL, midpnt, N)  -- improper limits
  end

  -- infinite limits
  local fni = function (x) return fn(1/x)/(x*x) end
  -- -inf
  if not afin and a < math.huge then
    if b < 0 then
      return qsimp(fni, 1/b, 0, TOL, midpnt, N)
    end
    -- -inf to -1
    local s1, e1 = qsimp(fni, -1, 0, TOL, midpnt, N)
    if b < math.huge then
      local s2, e2 = qsimp(fn, -1, b, TOL, midpnt, N)
      return s1 + s2, e1 or e2
    else
      local s2, e2 = qsimp(fn, -1, 1, TOL, midpnt, N)
      local s3, e3 = qsimp(fni, 0, 1, TOL, midpnt, N)
      return s1 + s2 + s3, e1 or e2 or e3
    end
  end
  -- +inf
  if a > 0 then
    return qsimp(fni, 0, 1/a, TOL, midpnt, N)
  else
    local s1, e1 = qsimp(fni, 0, 1, TOL, midpnt, N)
    local s2, e2 = qsimp(fn, a, 1, TOL, midpnt, N)
    return s1 + s2, e1 or e2
  end
end
about[numeric.int] = {":int(fn, x1_d, x2_d) --> num",
  "Get integral of the function. Improper integrals with infinite limits are possible."}


if Sonata
then  --=====================

  -- short alias
  INF = math.huge

end   --=====================


-- Comment to remove descriptions
numeric.about = about

return numeric

--===============================
