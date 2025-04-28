--[[		sonata/matlib/extremum.lua

--- Finding extremum point for the given funciton.
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

local help = SonataHelp or {} 
-- description
local about = {
__module__ = "Finding extremum point."
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
--  @param a Initial low bound.
--  @param b Initial high bound.
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


--- Find minimum using golden section method.
--  @param fun Source function.
--  @param a Small bound.
--  @param b Intermediate point.
--  @param c High bound.
--  @return point where function is minimal and its value.
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


--- Find minimum using Brent method.
--  @param fun Source function.
--  @param a Small bound.
--  @param b Intermediate point.
--  @param c High bound.
--  @return point where function is minimal and its value.
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


--- Find minimum using Brent method and a function derivative.
--  @param fun Source function.
--  @param dfun Derivative of the function.
--  @param a Small bound.
--  @param b Intermediate point.
--  @param c High bound.
--  @return point where function is minimal and its value.
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

extremum.minimum1D = function (fun, a, b, param)
  param = param or {}
  local c = param.c or (a + b)*0.5
  if param.method == 'Brent' then
    if param.dfun then
      return extremum._minBrentD(fun, param.dfun, a, b, c)
    else
      return extremum._minBrent(fun, a, b, c)
    end
  else
    return extremum._minGolden(fun, a, b, c)
  end
end


--- Find sum of elements for the simplex method.
--  @param pp Matrix with points.
--  @param psum Table to store the result.
extremum._simplexPsum = function (pp, psum)
  for i = 1, pp:rows() do
    local s, pi = 0, pp[i]
    for j = 1, pp:cols() do s = s + pi[j] end
    psum[i] = s
  end
end


--- Multidimentional minimum search with simplex method.
--  @param fun Funciton for minimization.
--  @param pp List of bound points as a matrix.
--  @return point where function is minimal and its value.
extremum._simplex = function (fun, pp)
  extremum.ext_matrix = extremum.ext_matrix or require("matlib.matrix")
  local mat = extremum.ext_matrix
  local nmax, small = 500, 1E-10
  local y, psum = {}, {}
  for i = 1, pp:cols() do y[i] = fun(pp({}, i)) end
  extremum._simplexPsum(pp, psum)
  local p = pp:copy()
  for _ = 1, nmax do
    -- find highest, next highest and lowest
    local ilo, ihi, inhi = 1, 1, 2
    if y[1] > y[2] then 
      ihi, inhi = 2, 1
    end
    for i = 1, #y do
      local yi = y[i]
      if yi <= y[ilo] then ilo = i end
      if yi > y[ihi] then
        inhi, ihi = ihi, i
      elseif i ~= ihi and yi > y[inhi] then
        inhi = i
      end
    end
    local rtol = 2*mabs(y[ihi]-y[ilo])/(mabs(y[ihi]) + mabs(y[ilo]) + small)
    if rtol < TOL then
      -- put the best point into the first element
      y[1], y[ilo] = y[ilo], y[1]
      for i = 1, pp:rows() do
        local pi = p[i]
        pi[1], pi[ilo] = pi[ilo], pi[1]
      end
      return p({}, 1), y[1]
    end
    -- new iteration
    local ytry = extremum._simplexExtra(p, y, psum, ihi, -1.0, fun)
    if ytry <= y[ilo] then
      -- try additional extrapolation
      extremum._simplexExtra(p, y, psum, ihi, 2.0, fun)
    elseif ytry >= y[inhi] then
      -- one dimentional construction
      local ysave = y[ihi]
      ytry = extremum._simplexExtra(p, y, psum, ihi, 0.5, fun)
      if ytry >= ysave then
        -- contract around the lowest point
        for i = 1, p:cols() do
          if i ~= ilo then
            for j = 1, p:rows() do
              local pj = p[j]
              psum[j] = 0.5*(pj[i] + pj[ilo])
              pj[i] = psum[j]
            end
            y[i] = fun(mat:V(psum))
          end
        end
        extremum._simplexPsum(p, psum)
      end
    end
  end
  error('Too mutch iterations')
  return p({}, 1), y[1]
end

extremum._simplexExtra = function (p, y, psum, ihi, fac, fun)
  local ndim = p:rows()
  local fac1 = (1.0-fac)/ndim
  local fac2 = fac1 - fac
  local ptry = {}
  for j = 1, ndim do 
    ptry[j] = psum[j]*fac1 - p[j][ihi]*fac2 
  end
  local ytry = fun(extremum.ext_matrix:V(ptry)) 
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
  xi = xm * xi
  return p + xi, xi, fm
end

extremum._linmind = function (p, xi, fun, dfun)
  local ff = function (x) return fun(p + x*xi) end
  local df = function (x) return dfun(p + x*xi):T() * xi end
  local a, b, c = extremum._bracket(ff, 0.0, 1.0)
  local xm, fm = extremum._minBrentD(ff, df, a, b, c)
  xi = xm * xi
  return p + xi, xi, fm
end


extremum._minPowel = function (fun, p)
  extremum.ext_matrix = extremum.ext_matrix or require("matlib.matrix")
  local mat = extremum.ext_matrix
  local imax, small, n = 200, 1E-25, p:rows()
  local ximat = mat:eye(n)
  local fret = fun(p)
  for iter = 1, imax do
    local ibig, del = 1, 0.0
    local pt, fp = p, fret
    -- find the biggest decrease
    for i = 1, n do
      local fprev = fret
      p, _, fret = extremum._linmin(p, ximat({}, i), fun)
      fprev = fprev - fret  -- reuse
      if fprev > del then
        del, ibig = fprev, i
      end
    end
    if 2*mabs(fp-fret) <= TOL*(mabs(fp) + mabs(fret) + small) then
      return p, fret
    end
    -- extrapolated point
    local grad = p - pt
    local fptt = fun(p + grad)
    if fptt < fp then
      local t = 2*(fp-2*fret+fptt)*(fp-fret-del)^2 - del*(fp-fptt)^2
      if t < 0 then
        p, grad, fret = extremum._linmin(p, grad, fun)
        for j = 1, n do
          local xmatj = ximat[j]
          xmatj[ibig] = xmatj[n]
          xmatj[n] = grad[j][1]
        end
      end
    end
  end
  error("Too much iterations")
  return p, fret
end

extremum._minGrad = function (fun, dfun, p)
  local imax, small, n = 200, 1E-20, p:rows()
  local fret, xi = fun(p), dfun(p)
  local g = -xi
  local h = g
  for i = 1, imax do
    local fp = fret
    --p, xi, fret = extremum._linmin(p, xi, fun)
    p, xi, fret = extremum._linmind(p, xi, fun, dfun)
    if 2*mabs(fp - fret) <= TOL*(mabs(fret) + mabs(fp) + small) then
      return p, fret
    end
    xi = dfun(p)
    local gg, dgg = 0.0, 0.0
    for j = 1, n do
      local gj, xj = g[j][1], xi[j][1]
      gg = gg + gj*gj
      dgg = dgg + xj*xj + (xj+gj)*xj
    end
    if gg == 0.0 then
      return p, fret
    end
    local gam = dgg / gg
    h, g = gam*h - xi, -xi
    xi = h
  end
  error("Too much iterations")
  return p, fret
end

extremum.minimum = function (fun, p, param)
  param = param or {}
  if param.method == 'Powel' then
    if param.dfun then
      return extremum._minGrad(fun, param.dfun, p)
    else
      return extremum._minPowel(fun, p)
    end
  else
    return extremum._simplex(fun, param.p)
  end
end

extremum._lpSimpxEliminate = function (H,  ids)
  local hm = H:cols()
  for s = 1, hm do
    -- get pivot column
    local col = 0
    for i = s, hm-1 do
      if H[1][i] > 0 then
        col = i
        break
      end
    end
    -- apply pivot
    if col > 0 then
      -- find row
      local jmin, vmin = 0, math.huge
      for j = 2, H:rows() do
        local vc, vm = H[j][col], H[j][hm]
        if vc > 0 and vm/vc < vmin then
          jmin, vmin = j, vm/vc
        end
      end
      if jmin == 0 then  break end  -- no solution ?

      ids[jmin-1] = col 
      local Hj = H[jmin]
      vmin = Hj[col]  -- reuse
      -- normalize line
      for k = 1, hm do Hj[k] = Hj[k] / vmin end
      -- extract
      for j = 1, H:rows() do
        if j ~= jmin then
          local Hi = H[j]
          local t = Hi[col]
          for k = 1, hm do Hi[k] = Hi[k] - Hj[k]*t end
        end
      end
    else
      break
    end
  end
  return H, ids
end

extremum._lpSimplex = function (c, param)
  extremum.ext_matrix = extremum.ext_matrix or require("matlib.matrix")
  local mat = extremum.ext_matrix
  local nu, ne, nl, nx = 0, 0, 0, c:cols()
  local As, bs, inter = param.Au, param.bu, nil

  if As then
    nu = As:rows()
    inter = mat:eye(nu)
  end

  if param.Al then
    nl = param.Al:rows()
    local eye = mat:eye(nl)
    if As then
      As, bs = mat:ver {As, param.Al}, mat:ver {bs, param.bl}
      local r, c = inter:rows(), inter:cols()
      inter = mat:ver {
        mat:hor {inter, mat:zeros(r, nl+nl)},
        mat:hor {mat:zeros(nl, c), -eye, eye}
      }
    else
      As, bs = param.Al, param.bl
      inter = mat:hor {-eye, eye}
    end
  end

  if param.Ae then
    ne = param.Ae:rows()
    if As then
      As, bs = mat:ver {As, param.Ae}, mat:ver {bs, param.be}
      local r, c = inter:rows(), inter:cols()
      inter = mat:ver {
        mat:hor {inter, mat:zeros(r, ne)},
        mat:hor {mat:zeros(ne, c), mat:eye(ne)}
      }
    else
      As, bs, inter = param.Ae, param.be, mat:eye(ne)
    end
  end

  local H, ids = nil, {}
  for i = 1, nu+ne+nl do ids[i] = i + nx end

  if ne + nl > 0 then
    -- prepare first row
    local asum, bsum = {}, 0
    for i = 1, As:cols() do
      local s = 0
      for j = nu+1, As:rows() do s = s + As[j][i] end
      asum[i] = s
    end
    for j = nu+1, bs:rows() do bsum = bsum + bs[j][1] end
    -- phase 1 matrix
    H = mat:ver {
      mat:hor {mat{asum}, mat:zeros(1, inter:cols()), mat(bsum)},
      mat:hor {As, inter, bs},
    }:copy()

    H, ids = extremum._lpSimpxEliminate(H, ids)
    -- TODO check H(1, -1) is 0
    -- update table
    local hm, last = H:cols(), H:cols()-ne-nl
    for i = 1, H:rows() do H[i][last] = H[i][hm] end
    H._cols = last    -- update size
    for i = 1, H:cols() do
      H[1][i] = (i <= nx) and -c[1][i] or 0
    end
  elseif nu > 0 then
    H = mat:ver {
      mat:hor {-c, mat:zeros(1, inter:cols()+1)},
      mat:hor {As, inter, bs}
    }:copy() 
  end

  if H then
    H, ids = extremum._lpSimpxEliminate(H, ids)
    local hm = H:cols()
    local res = mat:zeros(nx, 1)
    for i, v in ipairs(ids) do
      if v <= nx then res[v][1] = H[i+1][hm] end
    end
    return res, H[1][hm]
  end
end


extremum.linprog = function (c, param) return extremum._lpSimplex(c, param) end


-- Comment to remove descriptions
extremum.about = about

--return extremum
--local fun = function (x) return x*x end
--local dfun = function (x) return 2*x end
--print(extremum._minBrentD(fun, dfun, -10, -9, 10))
--print(extremum._minBrent(fun, -20, 19, 20))
--print(extremum._minGolden(fun, -4, -1, 3))

local Mat = require 'matlib.matrix'
--local foo = function (y) return (y[1][1]-1)^4 + (y[2][1]-2)^4 end
--local dfoo = function (y) return Mat:V{4*(y[1][1]-1)^3, 4*(y[2][1]-2)^3} end

--pts = Mat{{5,3},{7,-9},{-7,-4}}:T()
--x, fx = extremum._simplex(pts, foo)
--print(x)
--print(fx)

--p0 = Mat:V{5, 4}
--x, fx = extremum._minGrad(foo, dfoo, p0)
--x, fx = extremum._minPowel(foo, p0)

--print(x)
--print(fx)

--Cc = Mat{{-3, -5}}
--Au = Mat{{1, 0}, {0, 2}, {3, 2}}
--bu = Mat:V {4, 12, 18}

Cc = Mat{{-2, -3, -4}}
--Au = Mat{{3, 2, 1}, {2, 5, 3}}; bu = Mat:V{10, 15}
Ae = Mat{{3, 2, 1}, {2, 5, 3}}; be = Mat:V{10, 15}

--Cc = Mat{{-0.4, -0.5}}
--Au = Mat{{0.3, 0.1}}; bu = Mat(2.7) 
--Ae = Mat{{0.5, 0.5}}; be = Mat(6)
--Al = Mat{{0.6, 0.4}}; bl = Mat(6)

x, fx = extremum._lpSimplex(Cc, {Au=Au, bu=bu, Ae=Ae, be=be, Al=Al, bl=bl})
print(x)
print('f(x)', fx)

