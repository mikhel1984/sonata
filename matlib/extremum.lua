--[[		sonata/matlib/extremum.lua

--- Finding extremum point.
--
--  </br></br><b>Authors</b>: Your Name

	module 'extremum'
--]]

-- Define here your tests, save results to 'ans',
-- use --> for the strict equality
-- and --n> for the n-digit precision in the case of floating numbers.
--[[TEST_IT

-- use 'extremum'
Ex = require 'matlib.extremum'

-- example
a = Ex()
-- check equality
ans = a.type                  -->  'extremum'

-- check relative equality ( ~10^(-2) )
ans = math.pi                --2> 355/113

--]]


--	LOCAL

local GOLD = 1.6180339
local TOL = 1E-4

--	INFO

local help = SonataHelp or {}  -- optional
-- description
local about = {
__module__ = "Finding extremum point"
}


--	MODULE

local extremum = {
-- mark
type = 'extremum',
}
-- methametods
extremum.__index = extremum


--- Check object type.
--  @param v Object.
--  @return True if the object is extremum.
local function isextremum(v) return getmetatable(v) == extremum end


--- Search in downhill direction to find bracket a minimum of a function.
--  @param fun Function to minimize.
--  @param a Initial left point.
--  @param b Initial right point.
--  @return tuple of points (a, b, c, fa, fb, fc), such that fb <= fa, fc
extremum._bracket = function (fun, a, b)
  local tiny, limit = 1E-20, 100
  local fa, fb = fun(a), fun(b)
  if fb > fa then
    a, b = b, a
    fa, fb = fb, fa
  end
  local c = b + GOLD*(b-a)
  local fc, fu = fun(c), nil
  while fb > fc do
    local r = (b-a)*(fb-fc)
    local q = (b-c)*(fb-fa)
    local denom = 2*math.max(math.abs(q-r), tiny)
    local u = b - ((b-c)*q - (b-a)*r)/((q >= r) and denom or -denom)
    local ulim = b + limit*(c-b)
    if (b-u)*(u-c) > 0 then   -- parabolic u between b and c
      fu = fun(u)
      if fu < fc then
        a, b, fa, fb = b, u, fb, fu
        break
      elseif fu > fb then
        c, fc = u, fu
        break
      end
      u = c + GOLD*(c-b)
      fu = fun(u)
    elseif (c-u)*(u-ulim) > 0 then  -- parabolic fit between c and limit
      fu = fun(u)
      if fu < fc then
        b, c, u = c, u, u + GOLD*(u-c)
        fb, fc, fu = fc, fu, fun(u)
      end
    elseif (u-ulim)*(ulim-c) >= 0 then  -- limit parabolic u to maximum value
      u = ulim
      fu = fun(u)
    else                           -- use default magnification
      u = c + GOLD*(c-b)
      fu = fun(u)
    end
    a, b, c = b, c, u
    fa, fb, fc = fb, fc, fu
  end
  return a, b, c, fa, fb, fc
end


extremum._minGolden = function (fun, a, b, c)
  local gr, gc = GOLD-1, 2-GOLD
  local x0, x1, x2, x3 = a, nil, nil, c
  if math.abs(c-b) > math.abs(b-a) then
    x1, x2 = b, b + gc*(c-b)
  else
    x2, x1 = b, b - gc*(b-a)
  end
  local f1, f2 = fun(x1), fun(x2)
  while math.abs(x3-x0) > TOL do
    if f2 < f1 then
      x0, x1, x2 = x1, x2, gr*x2 + gc*x3
      f1, f2 = f2, fun(x2)
    else
      x3, x2, x1 = x2, x1, gr*x1 + gc*x0
      f2, f1 = f1, fun(x1)
    end
  end
  if f1 < f2 then
    return x1, f1
  else
    return x2, f2
  end
end

extremum._minBrent = function (fun, a, b, c)
  local imax, small, e, d = 100, 1E-30, 0, 0
  local gc = 2-GOLD
  a, c = math.min(a, c), math.max(a, c)
  local x, w, v = b, b, b
  local fx = fun(x)
  local fv, fw = fx, fx
  for i = 1, imax do
    local xm = 0.5*(a+c)
    local tol1 = TOL*math.abs(x) + small
    local tol2 = 2*tol1
    if math.abs(x-xm) <= (tol2-0.5*(c-a)) then
      return x, fx
    end
    -- parabolic fit
    local upd = false
    if math.abs(e) > tol1 then
      local r = (x-w)*(fx-fv)
      local q = (x-v)*(fx-fw)
      local p = (x-v)*q - (x-w)*r
      q = 2*(q-r)
      if q > 0 then p = -p end
      q = math.abs(q)
      local etemp = e
      e = d
      if math.abs(p) >= math.abs(0.5*q*etemp) or p <= q*(a-x) or p >= q*(c-x) then
        upd = true
      else
        d = p / q
        u = x + d
        if (u-a < tol2) or (c-u < tol2) then
          d = (xm >= x) and tol1 or (-tol1)
        end
      end
    else
      upd = true
    end
    if upd then
      e = (x >= xm) and (a-x) or (c-x)
      d = gc*e
    end
    local u = (math.abs(d) >= tol1) and (x+d) or (x + (d >= 0 and tol1 or -tol1))
    local fu = fun(u)
    if fu <= fx then
      if u >= x then a = x else c = x end
      v, w, x = w, x, u
      fv, fw, fx = fw, fx, fu
    else
      if u < x then a = u else c = u end
      if fu <= fw or w == x then
        v, w = w, u
        fv, fw = fw, fu
      elseif fu < fv or v == x or v == w then
        v, fv = u, fu
      end
    end
  end
  error('Too many iterations')  --TODO warning
end



extremum._minBrentD = function (fun, dfun, a, b, c)
  local imax, small, e, d = 100, 1E-30, 0, 0
  a, c = math.min(a, c), math.max(a, c)
  local x, w, v = b, b, b
  local fx = fun(x)
  local fv, fw = fx, fx
  local dx = dfun(x)
  local dv, dw = dx, dx
  for i = 1, imax do
    local xm = 0.5*(a+c)
    local tol1 = TOL*math.abs(x) + small
    local tol2 = 2*tol1
    if math.abs(x-xm) <= (tol2-0.5*(c-a)) then
      return x, fx
    end
    -- parabolic fit
    local upd = false
    if math.abs(e) > tol1 then
      local d1 = 2*(c-a)
      local d2 = d1
      if dw ~= dx then d1 = (w-x)*dx/(dx-dw) end
      if dv ~= dx then d2 = (v-x)*dx/(dx-dv) end
      local u1, u2 = x + d1, x + d2
      local ok1 = (a-u1)*(u1-c) > 0 and dx*d1 <= 0
      local ok2 = (a-u2)*(u2-c) > 0 and dx*d2 <= 0
      local olde = e
      e = d
      if ok1 or ok2 then
        if ok1 and ok2 then
          d = (math.abs(d1) < math.abs(d2)) and d1 or d2
        else
          d = ok1 and d1 or d2
        end
        if math.abs(d) <= math.abs(0.5*olde) then
          local u = x + d
          if (u-a < tol2) or (c-u < tol2) then
            d = (xm >= x) and tol1 or (-tol1)
          end
        else upd = true end
      else upd = true end
    else upd = true end
    if upd then
      e = dx >= 0 and (a-x) or (c-x)
      d = 0.5*e
    end
    local u, fu = nil, nil
    if math.abs(d) >= tol1 then
      u = x + d
      fu = fun(u)
    else
      u = x + (d >= 0 and tol1 or -tol1)
      fu = fun(u)
      if fu > fx then
        return x, fx
      end
    end
    local du = dfun(u)
    if fu <= fx then
      if u >= x then a = x else c = x end
      v, fv, dv = w, fw, dw
      w, fw, dw = x, fx, dx
      x, fx, dx = u, fu, du
    else
      if u < x then a = u else c = u end
      if fu <= fw or w == x then
        v, fv, dv = w, fw, dw
        w, fw, dw = u, fu, du
      elseif fu < fv or v == x or v == w then
        v, fv, dv = u, fu, du
      end
    end
  end
  error('Too many iterations')  --TODO warning
end

-- Comment to remove descriptions
extremum.about = about

--return extremum
local fun = function (x) return x*x end
local dfun = function (x) return 2*x end
print(extremum._minBrentD(fun, dfun, -4, -1, 3))
--print(extremum._minBrent(fun, -4, -1, 3))
--print(extremum._minGolden(fun, -4, -1, 3))

