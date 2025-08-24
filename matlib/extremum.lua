--[[		sonata/matlib/extremum.lua

--- Extremum search and optimization methods.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2025.

	module 'extremum'
--]]


---------------- Tests -----------------
--[[TEST_IT

-- use 'extremum'
Ex = require 'matlib.extremum'
-- use matrices for multidimentional optimization
Mat = require 'matlib.matrix'

-- 1D minimum point, golden section
fun = function (x) return x*x end
xm, fm = Ex:minimum1D(fun, -10, 5)
ans = xm                    --.2> 0.0

ans = fm                    --.2> 0.0

-- 1D Brent method, with initial point
xm, fm = Ex:minimum1D(fun, -10, 5, {method='Brent', b=-5})
ans = xm                    --.2> 0.0

-- 1D Brent method, use derivative
df = function (x) return 2*x end
xm, fm = Ex:minimum1D(fun, -10, 5, {method='Brent', dfun=df})
ans = xm                    --.2> 0.0

-- Find maximum
xm, fm = Ex:maximum1D(math.cos, -1, 1, {b=0.5})
ans = fm                    --.2> 1.0

-- multidimentional, Powel method
foo = function (y) return (y[1][1]-1)^4 + (y[2][1]-2)^4 end
p0 = Mat:V {5, 4}
xm, fm = Ex:minimum(foo, p0)
ans = xm(1)                 --.2> 1.0

ans = fm                    --.2> 0.0

-- multidimentional, with derivative
dfoo = function (y) return Mat:V{4*(y[1][1]-1)^3, 4*(y[2][1]-2)^3} end
xm, fm = Ex:minimum(foo, p0, {dfun=dfoo})
ans = xm(2)                 --.2> 2.0

-- multidimentional, simplex
-- expected column-vectors
pp = Mat{{5,3},{7,-9},{-7,-4}}:T()
xm, fm = Ex:minimum(foo, pp, {method='simplex'})
ans = fm                    --.2> 0.0

-- multidimentional, maximum
foo = function (x) return 3 - x:norm() end
xm, fm = Ex:maximum(foo, Mat:V{-3, 4})
ans = fm                    --.3> 3.0

-- linear programming, simplex method
-- minimize C*x when Au*x <= bu
C = Mat{{-3, -5}}
A = Mat{{1, 0}, {0, 2}, {3, 2}}; b = Mat:V {4, 12, 18}
xm, fm = Ex:linprog(C, {Au=A, bu=b})
ans = fm                    --.2> -36.0

-- LP, additional constraints
-- C*x -> min, Au*x <= bu, Ae*x == be, Al*x >= bl
C = Mat{{-0.4, -0.5}}
Au = Mat{{0.3, 0.1}}; bu = Mat(2.7)
Ae = Mat{{0.5, 0.5}}; be = Mat(6)
Al = Mat{{0.6, 0.4}}; bl = Mat(6)
xm, fm = Ex:linprog(C, {Au=Au, bu=bu, Ae=Ae, be=be, Al=Al, bl=bl})
ans = fm                    --.2> -5.4

-- simulated annealing
-- solve the problem of 8 chess queens
pos = {1, 2, 3, 4, 5, 6, 7, 8}  -- initial positions
-- update method, replace 2 arbitrary elements
tweak = function (x)
  local y, m, n = {}, nil, nil
  for i = 1, #x do y[i] = x[i] end  -- return copy
  repeat
    m, n = math.random(#y), math.random(#y)
  until m ~= n
  y[m], y[n] = y[n], y[m]
  return y
end
-- state energy, number of collisions
energy = function (x)
  local s = 0
  for i = 1, #x-1 do
    for j = i+1, #x do
      -- check the same diagonal
      if math.abs(j-i) == math.abs(x[j]-x[i]) then s = s + 1 end
    end
  end
  return s
end
-- find solution
xm, fm = Ex:annealing {
  energy=energy,
  update=tweak,
  init=pos,
  T=20,
  alpha=0.95,
  loop=2
}
print(table.concat(xm, ' '))
ans = fm                     --> 0

-- nonlinear data fit
-- prepare data
xs, ys = {}, {}
foo = function (x, t) return t.A*math.sin(t.B*x) + t.C end
t0 = {A=2, B=0.5, C=-1}
for i = 1, 40 do xs[i] = i*0.1; ys[i] = foo(xs[i], t0) end
-- initial estimation
tinit = {A=3, B=2, C=-2}
tm, fm = Ex:fit(foo, tinit, xs, ys)
ans = tm.A                  --.2> t0.A

--]]


--	LOCAL

local GOLD = 1.6180339
local TOL = 1E-4

local _ext = {
  -- matrix = require("matlib.matrix"),
}

local _abs, _max, _min = math.abs, math.max, math.min
local _inform = Sonata and Sonata.warning or print


--	INFO

-- description
local _about = {
__module__ = "Extremum search and optimization methods."
}


--	MODULE

local extremum = {}
-- methametods
extremum.__index = extremum


--- Search in downhill direction to find bracket a minimum of a function.
--  @param fun Function to minimize.
--  @param a Initial low bound.
--  @param b Initial high bound.
--  @return tuple of points (a, b, c, fa, fb, fc), such that fb <= fa, fc
local function _bracket (fun, a, b)
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
    local denom = 2*_max(_abs(q-r), tiny)
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


--- Moves along the fiven direction to find minimum value.
--  @param p Initial point.
--  @param xi Multidimentional direction.
--  @param fun Source function.
--  @return found point, deflection and function value.
local function _linmin (p, xi, fun)
  local ff = function (x) return fun(p + x*xi) end
  local a, b, c = _bracket(ff, 0.0, 1.0)
  local xm, fm = extremum._minBrent(ff, a, b, c)
  xi = xm * xi
  return p + xi, xi, fm
end


--- Moves along the fiven direction to find minimum value.
--  Use gradient function.
--  @param p Initial point.
--  @param xi Multidimentional direction.
--  @param fun Source function.
--  @param dfun Gradient function.
--  @return found point, deflection and function value.
local function _linmind (p, xi, fun, dfun)
  local ff = function (x) return fun(p + x*xi) end
  local df = function (x) return dfun(p + x*xi):T() * xi end
  local a, b, c = _bracket(ff, 0.0, 1.0)
  local xm, fm = extremum._minBrentD(ff, df, a, b, c)
  xi = xm * xi
  return p + xi, xi, fm
end


--- Find and apply pivot operation.
--  @param H Matrix for optimization.
--  @param ids List of variable indices.
--  @return updated matrix and list of indices.
local function _lpSimpxEliminate (H,  ids)
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


--- Extrapolates by factor through the face of the simplex.
--  Replace the high point it the new is better.
--  @param p List of bound points as a matrix.
--  @param y List of funciton values.
--  @param psum List of vector sums.
--  @param fac Factor value.
--  @param fun Source function.
--  @return the found extremum.
local function _simplexExtra (p, y, psum, ihi, fac, fun)
  local ndim = p:rows()
  local fac1 = (1.0-fac)/ndim
  local fac2 = fac1 - fac
  local ptry = {}
  for j = 1, ndim do
    ptry[j] = psum[j]*fac1 - p[j][ihi]*fac2
  end
  local ytry = fun(_ext.matrix:V(ptry))
  if ytry < y[ihi] then
    y[ihi] = ytry
    for j = 1, ndim do
      psum[j] = psum[j] + ptry[j] - p[j][ihi]
      p[j][ihi] = ptry[j]
    end
  end
  return ytry
end


--- Find sum of elements for the simplex method.
--  @param pp Matrix with points.
--  @param psum Table to store the result.
local function _simplexPsum (pp, psum)
  for i = 1, pp:rows() do
    local s, pi = 0, pp[i]
    for j = 1, pp:cols() do s = s + pi[j] end
    psum[i] = s
  end
end


--- Solve LP problem using simplex method.
--  @param c Row-vector of coefficients for equation c*x -> min.
--  @param param Optimization parameters (matrices Au*x <= bu, Ae*x == be, Al*x >= bl).
--  @return minimum point and function value.
extremum._lpSimplex = function (c, param)
  _ext.matrix = _ext.matrix or require("matlib.matrix")
  local mat = _ext.matrix
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

    H, ids = _lpSimpxEliminate(H, ids)
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
    H, ids = _lpSimpxEliminate(H, ids)
    local hm = H:cols()
    local res = mat:zeros(nx, 1)
    for i, v in ipairs(ids) do
      if v <= nx then res[v][1] = H[i+1][hm] end
    end
    return res, H[1][hm]
  end
end


--- Find minimum using Brent method.
--  @param fun Source function.
--  @param a Low bound.
--  @param b Intermediate point.
--  @param c High bound.
--  @return point where function is minimal and its value.
extremum._minBrent = function (fun, a, b, c)
  local imax, small, e, d = 100, 1E-30, 0, 0
  a, c = _min(a, c), _max(a, c)
  local x, w, v = b, b, b
  local fx = fun(x)
  local fv, fw = fx, fx
  for i = 1, imax do
    local xm = 0.5*(a+c)
    local tol1 = TOL*_abs(x) + small
    local tol2 = 2*tol1
    if _abs(x-xm) <= (tol2-0.5*(c-a)) then
      return x, fx
    end
    -- parabolic fit
    local upd = false
    if _abs(e) > tol1 then
      local r = (x-w)*(fx-fv)
      local q = (x-v)*(fx-fw)
      local p = (x-v)*q - (x-w)*r
      q = 2*(q-r)
      if q > 0 then p = -p end
      q = _abs(q)
      if _abs(p) >= _abs(0.5*q*e) or p <= q*(a-x) or p >= q*(c-x) then
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
    local u = (_abs(d) >= tol1) and (x+d) or (x + (d >= 0 and tol1 or -tol1))
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
  _inform('Too many iterations')
  return x, fx
end


--- Find minimum using Brent method and a function derivative.
--  @param fun Source function.
--  @param dfun Derivative of the function.
--  @param a Low bound.
--  @param b Intermediate point.
--  @param c High bound.
--  @return point where function is minimal and its value.
extremum._minBrentD = function (fun, dfun, a, b, c)
  local imax, small, e, d = 100, 1E-30, 0, 0
  a, c = _min(a, c), _max(a, c)
  local x, w, v = b, b, b
  local fx = fun(x)
  local fv, fw = fx, fx
  local dx = dfun(x)
  local dv, dw = dx, dx
  for i = 1, imax do
    local xm = 0.5*(a+c)
    local tol1 = TOL*_abs(x) + small
    local tol2 = 2*tol1
    if _abs(x-xm) <= (tol2-0.5*(c-a)) then
      return x, fx
    end
    -- parabolic fit
    local upd = true
    if _abs(e) > tol1 then
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
          d = (_abs(d1) < _abs(d2)) and d1 or d2
        else
          d = ok1 and d1 or d2
        end
        if _abs(d) <= _abs(0.5*b) then
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
    if _abs(d) >= tol1 then
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
  _inform('Too many iterations')
  return x, fx
end


--- Find minimum using golden section method.
--  @param fun Source function.
--  @param a Low bound.
--  @param b Intermediate point.
--  @param c High bound.
--  @return point where function is minimal and its value.
extremum._minGolden = function (fun, a, b, c)
  local gr, gc = GOLD-1, 2-GOLD
  local x0, x1, x2, x3 = a, nil, nil, c
  if _abs(c-b) > _abs(b-a) then
    x1, x2 = b, b + gc*(c-b)
  else
    x2, x1 = b, b - gc*(b-a)
  end
  local f1, f2 = fun(x1), fun(x2)
  while _abs(x3-x0) > TOL do
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


--- Multidimentional minimum search using Powel method
--  and gradient function.
--  @param fun Source function.
--  @param dfun Gradient function.
--  @param p Initial point.
--  @return minimum point and function value.
extremum._minGrad = function (fun, dfun, p)
  local imax, small, n = 200, 1E-20, p:rows()
  local fret, xi = fun(p), dfun(p)
  local g = -xi
  local h = g
  for i = 1, imax do
    local fp = fret
    p, xi, fret = _linmind(p, xi, fun, dfun)
    if 2*_abs(fp - fret) <= TOL*(_abs(fret) + _abs(fp) + small) then
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


--- Multidimentional minimum search using Powel method.
--  @param fun Source function.
--  @param p Initial point.
--  @return minimum point and function value.
extremum._minPowel = function (fun, p)
  _ext.matrix = _ext.matrix or require("matlib.matrix")
  local mat = _ext.matrix
  local imax, small, n = 200, 1E-25, p:rows()
  local ximat = mat:eye(n)
  local fret = fun(p)
  for iter = 1, imax do
    local ibig, del = 1, 0.0
    local pt, fp = p, fret
    -- find the biggest decrease
    for i = 1, n do
      local fprev = fret
      p, _, fret = _linmin(p, ximat({}, i), fun)
      fprev = fprev - fret  -- reuse
      if fprev > del then
        del, ibig = fprev, i
      end
    end
    if 2*_abs(fp-fret) <= TOL*(_abs(fp) + _abs(fret) + small) then
      return p, fret
    end
    -- extrapolated point
    local grad = p - pt
    local fptt = fun(p + grad)
    if fptt < fp then
      local t = 2*(fp-2*fret+fptt)*(fp-fret-del)^2 - del*(fp-fptt)^2
      if t < 0 then
        p, grad, fret = _linmin(p, grad, fun)
        for j = 1, n do
          local xmatj = ximat[j]
          xmatj[ibig] = xmatj[n]
          xmatj[n] = grad[j][1]
        end
      end
    end
  end
  -- ignore when defined manually
  _inform("Too much iterations")
  return p, fret
end



--- Multidimentional minimum search with simplex method.
--  @param fun Funciton for minimization.
--  @param pp List of bound points as a matrix.
--  @return point where function is minimal and its value.
extremum._simplex = function (fun, pp)
  _ext.matrix = _ext.matrix or require("matlib.matrix")
  local mat = _ext.matrix
  local nmax, small = 500, 1E-10
  local y, psum = {}, {}
  for i = 1, pp:cols() do y[i] = fun(pp({}, i)) end
  _simplexPsum(pp, psum)
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
    local rtol = 2*_abs(y[ihi]-y[ilo])/(_abs(y[ihi]) + _abs(y[ilo]) + small)
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
    local ytry = _simplexExtra(p, y, psum, ihi, -1.0, fun)
    if ytry <= y[ilo] then
      -- try additional extrapolation
      _simplexExtra(p, y, psum, ihi, 2.0, fun)
    elseif ytry >= y[inhi] then
      -- one dimentional construction
      local ysave = y[ihi]
      ytry = _simplexExtra(p, y, psum, ihi, 0.5, fun)
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
        _simplexPsum(p, psum)
      end
    end
  end
  _inform('Too mutch iterations')
  return p({}, 1), y[1]
end


--- Simulated annealing optimization method.
--  @param task Talbe with elements: energy, update, init, T, alpha, loop.
--  @return found solution and its energy.
extremum.annealing = function (_, task)
  local energy, update = task.energy, task.update
  local curr = task.init  -- start point
  local e0 = energy(curr) -- initial energy
  local temp = task.T or math.max(e0, 1.0)  -- initial annealing temperature
  local alpha = task.alpha or 0.9  -- temperature update coefficient
  local nmax = task.loop or 1      -- number of attempts with one temperature
  repeat
    for i = 1, nmax do
      local new = update(curr)
      local ei = energy(new)
      if ei <= e0 then
        e0, curr = ei, new
      else
        local v = math.exp((e0-ei)/temp)
        if math.random() < v then
          e0, curr = ei, new
        end
      end
      if e0 == 0 then break end
    end
    temp = temp * alpha
  until temp <= TOL or e0 == 0
  return curr, e0
end
_about[extremum.annealing] = {
  ":annealing(task={energy=fn,update=fn,init=x0,T=energy(x0),alpha=0.9,loop=1}) -> x_M, energy_d",
  "Simulated annealing method."}


--- Nonlinear data approximation.
--  @param fn Function with nonlinear model fn(x, t) -> y.
--  @param t0 Initial estimation of parameters.
--  @param xs List of x values.
--  @param ys List of y values.
--  @return table with parameters and sum of squares
extremum.fit = function (_, fn, t0, xs, ys)
  _ext.matrix = _ext.matrix or require("matlib.matrix")
  local mat = _ext.matrix
  local keys, v0 = {}, {}
  for k, w in pairs(t0) do
    keys[#keys+1] = k
    v0[#v0+1] = w
  end
  -- make function for optimization
  local t = {}
  local test = function (vec)
    for i = 1, #keys do t[keys[i]] = vec[i][1] end
    local s = 0.0
    for i = 1, #xs do
      local dy = ys[i] - fn(xs[i], t)
      s = s + dy*dy
    end
    return s
  end
  -- find minimum
  v0 = _ext.matrix:V(v0)  -- reuse
  local vm, fmin = extremum._minPowel(test, v0)
  -- to table
  local tm = {}
  for i, k in ipairs(keys) do tm[k] = vm[i][1] end
  return tm, fmin
end
_about[extremum.fit] = {
  ":fit(model_fn, param_t, xs_t, ys_t) --> minParam_t, sqSum_d",
  "Fit data with nonlinear model y = fn(x,t), where t is a parameter dictionary."}



--- Find maximum of a function with scalar argument.
--  Set param.method=Brent to use Brent algorithm, param.b - initial point,
--  param.dfun - function derivative (for Brent). Default method is golden section.
--  @param fun Source function.
--  @param a Low bound.
--  @param c Hight bound.
--  @param param Table with additional parameters {mehtod=nil, b=nil, dfun=nil}.
--  @return maximum point and function value.
extremum.maximum1D = function (_, fun, a, c, param)
  local xm, fm = extremum.minimum1D (_, function (x) return -fun(x) end, a, c, param)
  return xm, -fm
end
_about[extremum.maximum1D] = {
  ":maximum1D(fn, a_d, c_d, param={method='golden',b=millde,dfun=nil}) -> x_d, min_d",
  "Find maximum of a function with scalar argument. Parameters: method=Brent|nil, b - initial point, dfun - derivative (for Brent method)."}


--- Multidimentional maximum search.
--  param.method: 'Powel' or 'simplex', param.dfun - gradient function.
--  @param fun Source function.
--  @param p Initial point for 'Powel', matrix with points for 'simplex'.
--  @param param Table with additional parameters {method=nil, dfun=nil}.
--  @return maximum point and function value.
extremum.maximum = function (_, fun, p, param)
  local xm, fm = extremum.minimum (_, function (x) return -fun(x) end, p, param)
  return xm, -fm
end
_about[extremum.maximum] = {
  ":maximum(fn, p_M, param={method='Powel',dfun=nil}) -> x_M, min_d",
  "Find maximum of a multidimentional function. Parameters: method=Powel|simplex, dfun - gradient. p is matrix in case of simplex approach."}


--- Find minimum of a function with scalar argument.
--  Set param.method=Brent to use Brent algorithm, param.b - initial point,
--  param.dfun - function derivative (for Brent). Default method is golden section.
--  @param fun Source function.
--  @param a Low bound.
--  @param c Hight bound.
--  @param param Table with additional parameters {mehtod=nil, b=nil, dfun=nil}.
--  @return minimum point and function value.
extremum.minimum1D = function (_, fun, a, c, param)
  param = param or {}
  local b = param.b or (a + c)*0.5
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
_about[extremum.minimum1D] = {
  ":minimum1D(fn, a_d, c_d, param={method='golden',b=middle,dfun=nil}) -> x_d, min_d",
  "Find minimum of a function with scalar argument. Parameters: method=Brent|nil, b - initial point, dfun - derivative (for Brent method)."}


--- Multidimentional minimum search.
--  param.method: 'Powel' or 'simplex', param.dfun - gradient function.
--  @param fun Source function.
--  @param p Initial point for 'Powel', matrix with points for 'simplex'.
--  @param param Table with additional parameters {method=nil, dfun=nil}.
--  @return minimum point and function value.
extremum.minimum = function (_, fun, p, param)
  param = param or {}
  if not param.method or param.method == 'Powel' then
    if param.dfun then
      return extremum._minGrad(fun, param.dfun, p)
    else
      return extremum._minPowel(fun, p)
    end
  elseif param.method == 'simplex' then
    return extremum._simplex(fun, p)
  end
end
_about[extremum.minimum] = {
  ":minimum(fn, p_M, param={method='Powel',dfun=nil}) -> x_M, min_d",
  "Find minimum of a multidimentional function. Parameters: method=Powel|simplex, dfun - gradient. p is matrix in case of simplex approach."}


--- Solve LP problem c*x -> min.
--  Use simplex method.
--  @param c Row-vector of coefficients.
--  @param param List of parameters (matrices Au*x <= bu, Ae*x == be, Al*x >= bl).
--  @return minimum point and function value.
extremum.linprog = function (_, c, param) return extremum._lpSimplex(c, param) end
_about[extremum.linprog] = {
  ":linprog(c_M, param={Au=nil,bu=nil,Ae=nil,be=nil,Al=nil,bl=nil}) -> x_M, min_d",
  "Solve LP problem c*x -> min with Au*x <= bu, Ae*x == be, Al*x >= bl."}


-- Comment to remove descriptions
extremum.about = _about

return extremum

--===============================
-- TODO improve fit accuracy
-- TODO hide warnings in fit
