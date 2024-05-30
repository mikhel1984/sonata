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
  local i = 1
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

-- Comment to remove descriptions
extremum.about = about

--return extremum
local x, y = -2, 1
local fun = function (x) return x*x end
print(extremum._minGolden(fun, -2, -1, 3))

