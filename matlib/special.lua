--[[		sonata/lib/special.lua

--- Special mathematical functions.
--
--  Most functions are based on "Numerical recipes in C"
--  by W.H.Press, S.A.Teukolsky, W.T.Vetterling and B.P.Flannery
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2025.

	module 'special'
--]]


--------------- Tests --------------
--[[TEST_IT

-- use 'special'
Spec = require 'matlib.special'

-- beta functions
ans = Spec:beta(3,4) * 1E2   --3>  1.667

ans = Spec:betaln(10,20)     --3>  -19.115

ans = Spec:betainc(0.5, 2, 3.3)  --3>  0.7309

-- error functions
ans = Spec:erf(1)            --3>  0.8427

ans = Spec:erfc(0.5)         --3>  0.4795

-- Ei(x)
ans = Spec:expint(3.3) * 1E3   --3>  8.939

-- E3(x)
ans = Spec:expint(2, 5) * 1E4  --3>  9.965

-- gamma functions
ans = Spec:gamma(-1.5)       --3>  2.3633

ans = Spec:gammaln(100)      --3>  359.1342

ans = Spec:gammp(7.7, 2.3) * 1E3  --2>  3.85

ans = Spec:gammq(1.5, 4.8) * 1E2  --2>  2.23

-- Bessel functions
ans = Spec:besselj(3, 1.5) * 1E2  --3>  6.096

ans = Spec:bessely(4, 0.8)   --3>  -78.751

ans = Spec:besseli(2, -3.6)  --3>  4.254

ans = Spec:besselk(5, 5) * 1E2   --3>  3.2706

-- Dawson function
ans = Spec:dawson(3.3)       --3>  0.1598

--]]


--	LOCAL

local _utils = require("matlib.utils")
local _tointeger = _utils.versions.toInteger

-- constants for gamma approximation
local _k_gamma = {676.5203681218851, -1259.1392167224028, 771.32342877765313,
  -176.61502916214059, 12.507343278686905, -0.13857109526572012,
  9.9843695780195716E-6, 1.5056327351493116E-7}


-- constants for ln(gamma) approximation
local _k_gammaln = {76.18009172947146, -86.50532032941677, 24.01409824083091,
  -1.231739572450155, 0.1208650973866179E-2, -0.5395239384953E-5}

-- doc categories
local _tag = { GAMMA="gamma", BETA="beta", BESSEL="bessel" }


-- error
local ERR_POSINT = "Non-negative integer order is expected!"
local ERR_INVARG = "Invalid arguments!"


--- Fixed lower bound based on absolute value.
--  @param a Value to return.
--  @param b Lower bound.
--  @return Second argument if first is too small.
local function _lowBound(a, b) return math.abs(a) > b and a or b end


--	INFO

-- description
local _about = {
__module__ = "Special mathematical functions."
}


--	MODULE

local special = {}
special.__index = special


--- Continued fraction representation for incomplete gamma function Q.
--  @param N Order.
--  @param x Real value.
--  @return Representation of Q.
local function _gcf (N, x)
  local ITMAX, EPS, FPMIN = 100, 3E-7, 1E-30
  local b, c = x+1.0-N, 1.0/FPMIN
  local d = 1.0/b
  local h, an, del = d, nil, nil
  for i = 1, ITMAX do
    an, b = -i*(i-N), b+2.0
    d, c = 1.0/_lowBound(an*d+b, FPMIN), _lowBound(b+an/c, FPMIN)
    del = d*c
    h = h*del
    if math.abs(del-1.0) < EPS then break end
  end
  return math.exp(-x+N*math.log(x)-special:gammaln(N))*h
end


--- Modified Bessel function I0.
--  @param x Real number.
--  @return I0(x).
local function _bessi0 (x)
  local ax = math.abs(x)
  if ax < 3.75 then
    local y = x/3.75
    y = y*y
    return 1.0 + y*(3.5156229 + y*(3.0899424 + y*(1.2067492 + y*(0.2659732
      + y*(0.360768E-1 + y*0.45813E-2)))))
  else
    local y = 3.75/ax
    return (math.exp(ax)/math.sqrt(ax))*(0.39894228 + y*(0.1328592E-1
      + y*(0.225319E-2 + y*(-0.157565E-2 + y*(0.916281e-2
      + y*(-0.2057706E-1 + y*(0.2635537E-1 + y*(-0.1647633E-1
      + y*0.392377E-2))))))))
  end
end


--- Modified Bessel function I1.
--  @param x Real number.
--  @return I1(x).
local function _bessi1 (x)
  local ax, ans = math.abs(x), 0
  if ax < 3.75 then
    local y = x/3.75
    y = y*y
    ans = ax*(0.5 + y*(0.87890594 + y*(0.51498869 + y*(0.15084934
      + y*(0.2658733e-1 + y*(0.301532E-2 + y*0.32411E-3))))))
  else
    local y = 3.75/ax
    ans = 0.2282967E-1 + y*(-0.2895312E-1 + y*(0.1787654E-1 - y*0.420059E-2))
    ans = 0.39894228 + y*(-0.3988024E-1 + y*(-0.362018E-2 + y*(0.163801E-2
      + y*(-0.1031555E-1 + y*ans))))
    ans = ans*(math.exp(ax)/math.sqrt(ax))
  end
  return (x < 0) and -ans or ans
end


--- Bessel function J0.
--  @param x Real number.
--  @return J0(x).
local function _bessj0 (x)
  local ax  = math.abs(x)
  local ans1, ans2, y = 0, 0, 0
  if ax < 8.0 then
    y = x*x
    ans1 = 57568490574.0 + y*(-13362590354.0 + y*(651619640.7
      + y*(-11214424.18 + y*(77392.33017 - y*184.9052456))))
    ans2 = 57568490411.0 + y*(1029532985.0 + y*(9494680.718 + y*(59272.64853
      + y*(267.8532712 + y))))
    return ans1/ans2
  else
    local z = 8.0/ax
    y = z*z
    ans1 = 1.0 + y*(-0.1098628627E-2 + y*(0.2734510407E-4
      + y*(-0.2073370639E-5 + y*0.2093887211E-6)))
    ans2 = -0.1562499995E-1 + y*(0.1430488765E-3 + y*(-0.6911147651E-5
      + y*(0.7621095161E-6 - y*0.934935152E-7)))
    local xx = ax-0.785398164
    return math.sqrt(0.636619772/ax)*(math.cos(xx)*ans1 - z*math.sin(xx)*ans2)
  end
end


--- Bessel function J1.
--  @param x Real number.
--  @return J1(x).
local function _bessj1 (x)
  local ax = math.abs(x)
  local ans1, ans2, y = 0, 0, 0
  if ax < 8.0 then
    y = x*x
    ans1 = x*(72362614232.0 + y*(-7895059235.0 + y*(242396853.1 +
      y*(-2972611.439 + y*(15704.4826 - y*30.16036606)))))
    ans2 = 144725228442.0 + y*(2300535178.0 + y*(18583304.74
      + y*(99447.43394 + y*(376.9991397 + y))))
    return ans1/ans2
  else
    local z = 8.0/ax
    y = z*z
    ans1 = 1.0 + y*(0.183105E-2 + y*(-0.3516396496E-4 + y*(0.2457520174E-5
      - y*0.240337019E-6)))
    ans2 = 0.04687499995 + y*(-0.2002690873E-3 + y*(0.8449199096E-5
      + y*(-0.88228987E-6 + y*0.105787412E-6)))
    local xx = ax-2.356194491
    ans1 = math.sqrt(0.636619772/ax)*(math.cos(xx)*ans1 - z*math.sin(xx)*ans2)
    return (x >= 0) and ans1 or -ans1
  end
end


--- Modified Bessel function K0.
--  @param x Non-negative number.
--  @return K0(x).
local function _bessk0 (x)
  if x <= 2.0 then
    local y = x*x/4.0
    return (-math.log(x/2.0)*_bessi0(x)) + (-0.57721566
      + y*(0.42278420 + y*(0.23069756 + y*(0.3488590E-1 + y*(0.262698E-2
      + y*(0.10750E-3 + y*0.74E-5))))))
  else
    local y = 2.0/x
    return (math.exp(-x)/math.sqrt(x))*(1.25331414 + y*(-0.7832358E-1
      + y*(0.2189568E-1 + y*(-0.1062446E-1 + y*(0.587872E-2
      + y*(-0.251540E-2 + y*0.53208E-3))))))
  end
end


--- Modified Bessel function K1.
--  @param x Non-negative number.
--  @return K1(x).
local function _bessk1 (x)
  if x <= 2.0 then
    local y = x*x/4.0
    return (math.log(x/2.0)*_bessi1(x)) + (1.0/x)*(1.0
      + y*(0.15443144 + y*(-0.67278579 + y*(-0.18156897 + y*(-0.1919402E-1
      + y*(-0.110404E-2 - y*0.4686E-4))))))
  else
    local y = 2.0/x
    return (math.exp(-x)/math.sqrt(x))*(1.25331414 + y*(0.23498619
      + y*(-0.3655620E-1 + y*(0.1504268E-1 + y*(-0.780353E-2 + y*(0.325614E-2
      - y*0.68245E-3))))))
  end
end


--- Bessel function Y0.
--  @param x Non-negative number.
--  @return Y0(x).
local function _bessy0 (x)
  local ans1, ans2, y = 0, 0, 0
  if x < 8.0 then
    y = x*x
    ans1 = -2957821389.0 + y*(7062834065.0 + y*(-512359803.6 + y*(10879881.29
      + y*(-86327.92757 + y*228.4622733))))
    ans2 = 40076544269.0 + y*(745249964.8 + y*(7189466.438 + y*(47447.26470
      + y*(226.1030244 + y))))
    return ans1/ans2 + 0.636619772*_bessj0(x)*math.log(x)
  else
    local z = 8.0/x
    y = z*z
    ans1 = 1.0 + y*(-0.1098628627E-2 + y*(0.2734510407E-4
      + y*(-0.2073370639E-5 + y*0.2093887211E-6)))
    ans2 = -0.1562499995E-1 + y*(0.1430488765E-3 + y*(-0.6911147651E-5
      + y*(0.7621095161E-6 - y*0.934945152E-7)))
    local xx = x - 0.785398164
    return math.sqrt(0.636619772/x)*(math.sin(xx)*ans1 + z*math.cos(xx)*ans2)
  end
end


--- Bessel function Y1.
--  @param x Non-negative number.
--  @return Y1(x).
local function _bessy1 (x)
  local ans1, ans2, y = 0, 0, 0
  if x < 8.0 then
    y = x*x
    ans1 = x*(-0.4900604943E13 + y*(0.1275274390E13 + y*(-0.5153438139E11
      + y*(0.7349264551E9 + y*(-0.4237922726E7 + y*0.8511937935E4)))))
    ans2 = 0.2499580570E14 + y*(0.4244419664E12 + y*(0.3733650367E10
      + y*(0.2245904002E8 + y*(0.1020426050E6 + y*(0.3549632885E3 + y)))))
    return ans1/ans2 + 0.636619772*(_bessj1(x)*math.log(x) - 1.0/x)
  else
    local z = 8.0/x
    y = z*z
    ans1 = 1.0 + y*(0.183105E-2 + y*(-0.3516396496E-4 + y*(0.2457520174E-5
      - y*0.240337019E-6)))
    ans2 = 0.04687499995 + y*(-0.2002690873E-3 + y*(0.8449199096E-5
      + y*(-0.88228987E-6 + y*0.105787412E-6)))
    local xx = x - 2.356194491
    return math.sqrt(0.636619772/x)*(math.sin(xx)*ans1 + z*math.cos(xx)*ans2)
  end
end


--- Evaluates continued fraction for incomplete beta function
--  by modified Lentz's method.
--  @param a First bound.
--  @param b Second bound.
--  @param x Value between 0 and 1.
--  @return Fraction value.
local function _betacf (a, b, x)
  local MAXIT, EPS, FPMIN = 100, 3E-7, 1E-30
  local qab, qap, qam = a+b, a+1.0, a-1.0
  local c, d = 1.0, 1.0/_lowBound(1.0-qab*x/qap, FPMIN)
  local h, m2, aa, del = d, nil, nil, nil
  for m = 1, MAXIT do
    m2 = 2*m
    aa = m*(b-m)*x/((qam+m2)*(a+m2))
    d, c = 1.0/_lowBound(1.0+aa*d, FPMIN), _lowBound(1.0+aa/c, FPMIN)
    h = h*d*c
    aa = -(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
    d, c = 1.0/_lowBound(1.0+aa*d, FPMIN), _lowBound(1.0+aa/c, FPMIN)
    del = d*c
    h = h*del
    if math.abs(del-1.0) < EPS then break end
  end
  return h
end


--- Series representation for incomplete gamma function P.
--  @param N Order.
--  @param x Real value.
--  @return Representation of P.
local function _gammaSer (N, x)
  local ITMAX, EPS = 100, 3E-7
  local gamser = 0.0
  if x <= 0 then assert(x == 0)
  else
    local ap, del = N, 1.0/N
    local sum = del
    local gammaln = special.gammaln
    for i = 1, ITMAX do
      ap = ap+1
      del = del*x/ap
      sum = sum+del
      if math.abs(del) < math.abs(sum)*EPS then
        gamser = sum*math.exp(-x+N*math.log(x)-gammaln(nil, N))
        break
      end
    end
  end
  return gamser
end


--- Modified Bessel function In.
--  @param N Order.
--  @param x Real number.
--  @return In(x).
special.besseli = function (_, N, x)
  if N < 0 or _tointeger(N) == nil then error(ERR_POSINT) end
  if N == 0 then return _bessi0(x) end
  if N == 1 then return _bessi1(x) end
  if x == 0 then return 0.0 end
  local ACC, BIGNO, BIGNI = 40.0, 1E10, 1E-10
  local tox = 2.0/math.abs(x)
  local bip, ans, bi = 0.0, 0.0, 1.0
  for j = 2*(N+math.floor(math.sqrt(ACC*N))), 1, -1 do
    bi, bip = bip+j*tox*bi, bi
    if math.abs(bi) > BIGNO then
      ans = ans*BIGNI
      bi = bi*BIGNI
      bip = bip*BIGNI
    end
    if j == N then ans = bip end
  end
  ans = ans*_bessi0(x)/bi
  return (x < 0.0 and (N % 2)==1) and -ans or ans
end
_about[special.besseli] = {":besseli(order_N, x_d) --> num",
  "Modified Bessel function In(x).", _tag.BESSEL}


--- Bessel function of the first kind
--  @param N Polynomial order.
--  @param x Real number.
--  @return Polynomial value
special.besselj = function (_, N, x)
  if N < 0 or _tointeger(N) == nil then error(ERR_POSINT) end
  if N == 0 then return _bessj0(x) end
  if N == 1 then return _bessj1(x) end
  if x == 0 then return 0 end
  local ACC, BIGNO, BIGNI = 40, 1E10, 1E-10
  local ax = math.abs(x)
  local tox = 2.0 / ax
  local bj, bjm, ans = nil, nil, nil
  if ax > N then
    bjm = _bessj0(ax)
    bj = _bessj1(ax)
    for i = 1, (N-1) do
      bj, bjm = i*tox*bj-bjm, bj
    end
    ans = bj
  else
    local m = math.floor((N+math.floor(math.sqrt(ACC*N)))/2)*2
    local jsum, sum = false, 0
    local bjp = 0
    bj, ans = 1, 0
    for i = m, 1, -1 do
      bj, bjp = i*tox*bj-bjp, bj
      if math.abs(bj) > BIGNO then
        bj = bj*BIGNI
        bjp = bjp*BIGNI
        ans = ans*BIGNI
        sum = sum*BIGNI
      end
      if jsum then sum = sum + bj end
      jsum = not jsum
      if i == N then ans = bjp end
    end
    sum = 2.0*sum - bj
    ans = ans / sum
  end
  return (x < 0.0 and (N % 2)==1) and -ans or ans
end
_about[special.besselj] = {":besselj(order_N, x_d) --> num",
  "Bessel function of the first kind.", _tag.BESSEL}


--- Modified Bessel function Kn.
--  @param N Order.
--  @param x Positive value.
--  @return Kn(x).
special.besselk = function (_, N, x)
  if x <= 0 then error("Positive value is expected!") end
  if N < 0 or _tointeger(N) == nil then error(ERR_POSINT) end
  if N == 0 then return _bessk0(x) end
  if N == 1 then return _bessk1(x) end
  local tox, bkm, bk = 2.0/x, _bessk0(x), _bessk1(x)
  for j = 1, N-1 do
    bk, bkm = bkm+j*tox*bk, bk
  end
  return bk
end
_about[special.besselk] = {":besselk(order_N, x_d) --> num",
  "Modified Bessel function Kn(x).", _tag.BESSEL}


--- Bessel function of the second kind
--  @param n Polynomial order.
--  @param x Non-negative real number.
--  @return Polynomial value
special.bessely = function (_, n, x)
  if x <= 0 then error('Positive value is expected!') end
  if n < 0 or _tointeger(n) == nil then error(ERR_POSINT) end
  if n == 0 then return _bessy0(x) end
  if n == 1 then return _bessy1(x) end
  local tox = 2.0/x
  local by = _bessy1(x)
  local bym = _bessy0(x)
  for i = 1, (n-1) do
    by, bym = i*tox*by-bym, by
  end
  return by
end
_about[special.bessely] = {":bessely(order_N, x_d) --> num",
  "Bessel function of the second kind.", _tag.BESSEL}


--- Beta function.
--  @param z First value.
--  @param w Second value.
--  @return B(z,w).
special.beta = function (_, z, w) return math.exp(special.betaln(_, z, w)) end
_about[special.beta] = {":beta(z_d, w_d) --> num", "Beta function.", _tag.BETA}


--- Incomplete beta function
--  @param x Value between 0 and 1.
--  @param a First bound.
--  @param b Second bound.
--  @return Value of Ix(a,b).
special.betainc = function (_, x, a, b)
  if x < 0.0 or x > 1.0 then error("Expected x between 0 and 1!") end
  local bt = nil
  if x == 0 or x == 1 then
    bt = 0.0
  else
    bt = math.exp(special:gammaln(a+b) - special:gammaln(a)
      - special:gammaln(b) + a*math.log(x) + b*math.log(1.0-x))
  end
  return (x < (a+1.0)/(a+b+2.0))
     and (bt*_betacf(a, b, x)/a)
      or (1.0-bt*_betacf(b, a, 1.0-x)/b)
end
_about[special.betainc] = {":betainc(x_d, a_d, b_d) --> num",
  "Incomplete beta function Ix(a,b).", _tag.BETA}


--- Logarithm of beta function.
--  @param z First argument.
--  @param w Second argument.
--  @return log(B(x)).
special.betaln = function (_, z, w)
  return special:gammaln(z) + special:gammaln(w) - special:gammaln(z+w)
end
_about[special.betaln] = {":betaln(z_d, w_d) --> num",
  "Natural logarithm of beta function.", _tag.BETA}


--- Dawson integral.
--  @param x Real number.
--  @return Integral value.
special.dawson = function (_, x)
  local NMAX, H, A1, A2, A3 = 6, 0.4, 2.0/3.0, 0.4, 2.0/7.0
  if not special._c_dawson then
    -- List of Dawson function coefficients.
    special._c_dawson = {0, 0, 0, 0, 0, 0}
    for i = 1, NMAX do special._c_dawson[i] = math.exp(-((2.0*i-1.0)*H)^2) end
  end
  local xx = math.abs(x)
  if xx < 0.2 then
    local x2 = x*x
    return x*(1.0-A1*x2*(1.0-A2*x2*(1.0-A3*x2)))
  else
    local n0 = 2*math.floor(0.5*xx/H+0.5)
    local xp = xx - n0*H
    local e1 = math.exp(2.0*xp*H)
    local e2, d1 = e1*e1, n0+1
    local d2, sum = d1-2.0, 0.0
    for i = 1, NMAX do
      sum = sum + special._c_dawson[i]*(e1/d1+1.0/(d2*e1))
      d1, d2, e1 = d1+2.0, d2-2.0, e1*e2
    end
    return 0.5641895835*sum*(x>=0 and math.exp(-xp*xp) or -math.exp(-xp*xp))
  end
end
_about[special.dawson] = {":dawson(x_d) --> num", "Dawson integral."}


--- Error function.
--  @param x Real value.
--  @return Error value.
special.erf = function (_, x) return 1 - special:erfc(x) end
_about[special.erf] = {":erf(x_d) --> num", "Error function."}


--- Complementary error function.
--  @param x Real value.
--  @return Error value.
special.erfc = function (_, x)
  local z = math.abs(x)
  local t = 1.0 / (1+0.5*z)
  local ans = t*math.exp(-z*z - 1.26551223 + t*(1.00002368 + t*(0.37409196
    + t*(0.09678418 + t*(-0.18628806 + t*(0.27886807 + t*(-1.13520398
    + t*(1.48851587 + t*(-0.82215223 + t*0.17087277)))))))))
  return (x >= 0.0) and ans or (2.0 - ans)
end
_about[special.erfc] = {":erfc(x_d) --> num", "Complementary error function."}


--- Exponential integral.
--  @param n Power.
--  @param x Non-negative value.
--  @return Value of En(x).
special.expint = function (_, n, x)
  if x == nil then n, x = 1, n end
  if not (n >= 0 and x >= 0 and not (x == 0 and (n == 0 or n == 1))) then
    error(ERR_INVARG)
  end
  if n == 0 then return math.exp(-x)/x end
  local nm1 = n-1
  if x == 0.0 then return 1.0/nm1 end
  local MAXIT, EULER, FPMIN, EPS = 100, 0.5772156649, 1E-30, 1E-7
  if x > 1.0 then
    local b, c = x+n, 1.0/FPMIN
    local d = 1.0 / b
    local h = d
    for i = 1, MAXIT do
      local a = -i*(nm1+i)
      b = b+2.0
      d, c = 1.0/(a*d+b), b+a/c
      local del = c * d
      h = h * del
      if math.abs(del-1.0) < EPS then return h*math.exp(-x) end
    end
  else
    local ans = (nm1 ~= 0) and 1.0/nm1 or -math.log(x)-EULER
    local fact, psy, del = 1.0, nil, nil
    for i = 1, MAXIT do
      fact = fact * (-x/i)
      if i ~= nm1 then
        del = -fact / (i-nm1)
      else
        psy = -EULER
        for ii = 1, nm1 do psy = psy+1.0/ii end
        del = fact * (-math.log(x)+psy)
      end
      ans = ans + del
      if math.abs(del) < math.abs(ans)*EPS then return ans end
    end -- for i
  end -- if x
  error('Evaluation is failed!')
end
_about[special.expint] = {":expint(pow_N, x_d) --> num",
  "Exponential integral En(x)."}


--- Gamma function.
--  @param z Real number.
--  @return G(z).
special.gamma = function (_, z)
  if z < 0.5 then
    return math.pi / (math.sin(math.pi*z) * special:gamma(1-z))
  else
    z = z-1
    local x = 0.99999999999980993
    for i = 1, #_k_gamma do x = x + _k_gamma[i]/(z+i) end
    local t = z + #_k_gamma - 0.5
    -- sqrt(2*pi) = 2.506...
    return 2.5066282746310002 * t^(z+0.5) * math.exp(-t) * x
  end
end
_about[special.gamma] = {":gamma(x_d) --> num", "Gamma function.", _tag.GAMMA}


--- Logarithm of gamma function.
--  @param z Positive number.
--  @return log(gamma(z))
special.gammaln = function (_, z)
  local x, y = z, z
  local tmp = x + 5.5
  tmp = tmp - (x+0.5)*math.log(tmp)
  local ser = 1.000000000190015
  for i = 1, #_k_gammaln do
    y = y +1
    ser = ser + _k_gammaln[i]/y
  end
  return -tmp + math.log(2.5066282746310005*ser/x)
end
_about[special.gammaln] = {":gammaln(x_d) --> num",
  "Natural logarithm of gamma function.", _tag.GAMMA}


--- Incomplete gamma function P(N,x).
--  @param N Order.
--  @param x Non-negative value.
--  @return Value of P(N,x).
special.gammp = function (_, N, x)
  if x < 0.0 or N <= 0 then error(ERR_INVARG) end
  return (x < N+1.0) and _gammaSer(N, x) or 1.0-_gcf(N, x)
end
_about[special.gammp] = {":gammp(order_N, x_d) --> num",
  "Incomplete gamma function P(N,x).", _tag.GAMMA}


--- Incomplete gamma function Q(N,x).
--  @param N Order.
--  @param x Non-negative value.
--  @return Value of Q(N,x).
special.gammq = function (_, N, x)
  if x < 0.0 or N <= 0 then error(ERR_INVARG) end
  return (x < N+1.0) and 1-_gammaSer(N, x) or _gcf(N, x)
end
_about[special.gammq] = {":gammq(order_N, x_d) --> num",
  "Incomplete gamma function Q(N,x) = 1-P(N,x).", _tag.GAMMA}


-- Comment to remove descriptions
special.about = _about
-- clear load data
_tag = nil

return special

--===================================
-- TODO add other functions
