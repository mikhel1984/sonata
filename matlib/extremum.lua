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

local mabs, mmax, mmin = math.abs, math.max, math.min
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
    local denom = 2*mmax(mabs(q-r), tiny)
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
  if mabs(c-b) > mabs(b-a) then
    x1, x2 = b, b + gc*(c-b)
  else
    x2, x1 = b, b - gc*(b-a)
  end
  local f1, f2 = fun(x1), fun(x2)
  while mabs(x3-x0) > TOL do
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
  a, c = mmin(a, c), mmax(a, c)
  local x, w, v = b, b, b
  local fx = fun(x)
  local fv, fw = fx, fx
  for i = 1, imax do
    local xm = 0.5*(a+c)
    local tol1 = TOL*mabs(x) + small
    local tol2 = 2*tol1
    if mabs(x-xm) <= (tol2-0.5*(c-a)) then
      return x, fx
    end
    -- parabolic fit
    local upd = false
    if mabs(e) > tol1 then
      local r = (x-w)*(fx-fv)
      local q = (x-v)*(fx-fw)
      local p = (x-v)*q - (x-w)*r
      q = 2*(q-r)
      if q > 0 then p = -p end
      q = mabs(q)
      if mabs(p) >= mabs(0.5*q*e) or p <= q*(a-x) or p >= q*(c-x) then
        upd = true
      else
        e, d = d, p/q
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
      d = (2-GOLD)*e
    end
    local u = (mabs(d) >= tol1) and (x+d) or (x + (d >= 0 and tol1 or -tol1))
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
  return x, fx
end



extremum._minBrentD = function (fun, dfun, a, b, c)
  local imax, small, e, d = 100, 1E-30, 0, 0
  a, c = mmin(a, c), mmax(a, c)
  local x, w, v = b, b, b
  local fx = fun(x)
  local fv, fw = fx, fx
  local dx = dfun(x)
  local dv, dw = dx, dx
  for i = 1, imax do
    local xm = 0.5*(a+c)
    local tol1 = TOL*mabs(x) + small
    local tol2 = 2*tol1
    if mabs(x-xm) <= (tol2-0.5*(c-a)) then
      return x, fx
    end
    -- parabolic fit
    local upd = true
    if mabs(e) > tol1 then
      local d1 = 2*(c-a)
      local d2 = d1
      if dw ~= dx then d1 = (w-x)*dx/(dx-dw) end
      if dv ~= dx then d2 = (v-x)*dx/(dx-dv) end
      b = x + d1  -- reuse
      local ok1 = (a-b)*(b-c) > 0 and dx*d1 <= 0
      b = x + d2
      local ok2 = (a-b)*(b-c) > 0 and dx*d2 <= 0
      b, e = e, d
      if ok1 or ok2 then
        if ok1 and ok2 then
          d = (mabs(d1) < mabs(d2)) and d1 or d2
        else
          d = ok1 and d1 or d2
        end
        if mabs(d) <= mabs(0.5*b) then
          b = x + d
          if (b-a < tol2) or (c-b < tol2) then
            d = (xm >= x) and tol1 or (-tol1)
          end
          upd = false
        end
      end
    end
    if upd then
      e = dx >= 0 and (a-x) or (c-x)
      d = 0.5*e
    end
    local u, fu = nil, nil
    if mabs(d) >= tol1 then
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
  return x, fx
end

extremum._simplexPsum = function (pp)
  local psum = {}
  for i = 1, pp:rows() do
    local s, pi = 0, pp[i]
    for j = 1, pp:cols() do s = s + pi[j] end
    psum[i] = s
  end
  return psum
end

extremum._simplex = function (pp, fun)
  extremum.ext_matrix = extremum.ext_matrix or require("matlib.matrix")
  local mat = extremum.ext_matrix
  local nmax, small = 200, 1E-10
  local y, pmin = {}, {}
  for i = 1, pp:cols() do y[i] = fun(pp({}, i)) end
  local psum = extremum._simplexPsum(pp)
  local p = pp:copy()
  local nfunc = 0  -- TODO for loop ?
  while true do
    -- find highest, next highest and lowest
    local ilo = 1
    local ihi, inhi = 1, 2
    if y[1] > y[2] then 
      ihi, inhi = 2, 1
    end
    for i = 1, pp:cols() do
      if y[i] <= y[ilo] then ilo = i end
      if y[i] > y[ihi] then
        inhi, ihi = ihi, i
      elseif y[i] > y[inhi] and i ~= ihi then
        inhi = i
      end
    end
    local rtol = 2*mabs(y[ihi]-y[ilo])/(mabs(y[ihi]) + mabs(y[ilo]) + small)
    if rtol < TOL then
      -- put the best point into first element
      y[1], y[ilo] = y[ilo], y[1]
      for i = 1, pp:rows() do
        local pi = p[i]
        pi[1], pi[ilo] = pi[ilo], pi[1]
        pmin[i] = pi[1]
      end
      return mat:V(pmin), y[1]
    end
    if nfunc >= nmax then error('Too mutch iterations') end
    -- new iteration
    local ytry = extremum._simplexExtra(p, y, psum, ihi, -1.0, fun)
    if ytry <= y[ilo] then
      -- try additional extrapolation
      ytry = extremum._simplexExtra(p, y, psum, ihi, 2.0, fun)
    elseif ytry >= y[inhi] then
      -- one dimentional construction
      local ysave = y[ihi]
      ytry = extremum._simplexExtra(p, y, psum, ihi, 0.5, fun)
      if ytry >= ysave then
        for i = 1, p:cols() do
          if i ~= ilo then
            local pj = p[j]
            psum[j] = 0.5*(pj[i] + pj[ilo])
            pj[i] = psum[j]
          end
          y[i] = fun(mat:V(psum))
        end
        psum = extremum._simplexPsum(p)
      end
    else
      nfunc = nfunc - 1
    end
    nfunc = nfunc + 1
  end
end

extremum._simplexExtra = function (p, y, psum, ihi, fac, fun)
  local ndim = p:rows()
  local fac1 = (1.0-fac)/ndim
  local fac2 = fac1 - fac
  local ptry = {}
  for j = 1, ndim do ptry[j] = psum[j]*fac1 - p[j][ihi]*fac2 end
  local ytry = fun(p:V(ptry))  -- use matrix as reference to method V
  if ytry < y[ihi] then
    y[ihi] = ytry
    for j = 1, ndim do
      psum[j] = psum[j] + ptry[j] - p[j][ihi]
      p[j][ihi] = ptry[j]
    end
  end
  return ytry
end


extremum._linmin = function (p, xi, fun)
  local ff = function (x) return fun(p + x*xi) end 
  local a, b, c = extremum._bracket(ff, 0.0, 1.0)
  local xm, fm = extremum._minBrent(ff, a, b, c)
  return ff(xm), fm
end


extremum._minPowel = function (fun, pp)
  extremum.ext_matrix = extremum.ext_matrix or require("matlib.matrix")
  local mat = extremum.ext_matrix
  local ximat = mat:eye(pp:rows())
  local imax, small, n = 200, 1E-25, pp:rows()
  local p, pt = pp, pp
  local fret = fun(p)
  for iter = 1, math.huge do
    local ibig, del = 1, 0.0
    local fp, fptt = fret, nil
    -- find the biggest decrease
    for i = 0, n do
      fptt = fret
      p, fret = extremum._linmin(p, ximat({}, i), fun)
      if fptt - fret > del then
        del, ibig = fptt - fret, i
      end
    end
    if 2*(fp-fret) <= TOL*(mabs(fp) + mabs(fret) + small) then
      return p, fret
    end
    if iter >= imax then error("Too much iterations") end
    -- extrapolated point
    local xi = p - pt
    pt = p
    fptt = fun(2*p - pt)
    if fptt < fp then
      local t = 2*(fp-2*fret+fptt)*(fp-fret-del)^2 - del*(fp-fptt)^2
      if t < 0 then
        p, fret = extremum._linmin(p, xi, fun)
        for j = 1, n do
          local xmatj = ximat[j]
          xmatj[ibig] = xmatj[n]
          xmatj[n] = xi[j][1]
        end
      end
    end


  end
  
end



-- Comment to remove descriptions
extremum.about = about

--return extremum
local fun = function (x) return x*x end
local dfun = function (x) return 2*x end
print(extremum._minBrentD(fun, dfun, -10, -9, 10))
--print(extremum._minBrent(fun, -20, 19, 20))
--print(extremum._minGolden(fun, -4, -1, 3))

--local foo = function (y) return (y[1][1]-1)^2 + (y[2][1]-2)^2 end
--local Mat = require 'matlib.matrix'
--
--pts = Mat{{3,1},{1,-1},{-1,-2}}:T()
--x, fx = extremum._simplex(pts, foo)
--print(x)
--print(fx)
