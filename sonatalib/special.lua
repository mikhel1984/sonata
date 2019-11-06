--[[       sonatalib/special.lua

--- Special mathematical functions.
--
--  Most functions are based on "Numerical recipes in C" by W.H.Press, S.A.Teukolsky, W.T.Vetterling and B.P.Flannery
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonatalib</a> collection, 2017-2019.

           module 'special'
--]]

--------------- Tests --------------
--[[TEST

-- import 'special'
Spec = require 'sonatalib.special'

-- beta functions
ans = Spec.beta(3,4) * 1E2         --3> 1.667

ans = Spec.betaln(10,20)           --3> -19.115

ans = Spec.betainc(0.5, 2, 3.3)    --3> 0.7309

-- error functions
ans = Spec.erf(1)                  --3> 0.8427

ans = Spec.erfc(0.5)               --3> 0.4795

-- Ei(x)
ans = Spec.expint(3.3) * 1E3       --3> 8.939

-- E3(x)
ans = Spec.expint(2, 5) * 1E4      --3> 9.965

-- gamma functions
ans = Spec.gamma(-1.5)             --3> 2.3633

ans = Spec.gammaln(100)            --3> 359.1342

ans = Spec.gammp(7.7, 2.3) * 1E3   --2> 3.85

ans = Spec.gammq(1.5, 4.8) * 1E2   --2> 2.23

-- another syntax
ans = Spec.gammainc(2.1, 0.3, 'upper') * 1E2 --3> 1.942

-- Bessel functions
ans = Spec.besselj(3, 1.5) * 1E2   --3> 6.096

ans = Spec.bessely(4, 0.8)         --3> -78.751

ans = Spec.besseli(2, -3.6)        --3> 4.254

ans = Spec.besselk(5, 5) * 1E2     --3> 3.2706

-- Legendre function
lst = Spec.legendre(3, 0.5)
ans = lst[1]                       --3> -0.4375

-- Dawson function
ans = Spec.dawson(3.3)             --3> 0.1598

--]]

--	LOCAL

local Ver = require "sonatalib.versions"

-- constants for gamma approximation
local k_gamma = {676.5203681218851,-1259.1392167224028,771.32342877765313,-176.61502916214059,
               12.507343278686905,-0.13857109526572012,9.9843695780195716E-6,1.5056327351493116E-7}

-- constants for ln(gamma) approximation
local k_gammaln = {76.18009172947146,-86.50532032941677,24.01409824083091,-1.231739572450155,
                   0.1208650973866179E-2,-0.5395239384953E-5}

--- Fixed lower bound based on absolute value.
--  @param a Value to return.
--  @param b Lower bound.
--  @return Second argument if first is too small.
local function lowBound(a,b) return math.abs(a) > b and a or b end

-- doc categories
local GAMMA, BETA, BESSEL = 'gamma', 'beta', 'bessel'

--	INFO

local help = LC_DIALOG and (require "sonatalib.help") or {new=function () return {} end}

--	MODULE

local special = {
-- description
about = help:new("Special mathematical functions.")
}
special.__index = special

--- Gamma function.
--  Lanczos approximation (based on Wikipedia) for real numbers.
--  @param z Real number.
--  @return G(z).
special.gamma = function (z) 
   if z < 0.5 then
      return math.pi / (math.sin(math.pi*z) * special.gamma(1-z))
   else
      z = z-1
      local x = 0.99999999999980993
      for i = 1, #k_gamma do x = x + k_gamma[i]/(z+i) end
      local t = z + #k_gamma - 0.5
      return math.sqrt(2*math.pi)*t^(z+0.5)*math.exp(-t)*x
   end
end
special.about[special.gamma] = {"gamma(z)", "Gamma function.", GAMMA}

--- Logarithm of gamma function.
--  @param z Positive number.
--  @return log(gamma(z))
special.gammaln = function (z)
   local x,y = z,z
   local tmp = x+5.5
   tmp = tmp-(x+0.5)*math.log(tmp)
   local ser = 1.000000000190015
   for i = 1, #k_gammaln do y=y+1; ser = ser+k_gammaln[i]/y end
   return -tmp+math.log(2.5066282746310005*ser/x)
end
special.about[special.gammaln] = {"gammaln(z)", "Natural logarithm of gamma function.", GAMMA}

--- Series representation for incomplete gamma function P.
--  @param a Order.
--  @param x Real value.
--  @return Representation of P.
special._gammaSer_ = function (a,x)
   local ITMAX,EPS = 100, 3E-7
   local gamser = 0.0
   if x <= 0 then assert(x == 0)
   else
      local ap,del = a, 1.0/a
      local sum = del
      local gammaln = special.gammaln
      for i = 1,ITMAX do
         ap = ap+1
	 del = del*x/ap
	 sum = sum+del
	 if math.abs(del) < math.abs(sum)*EPS then
	    gamser = sum*math.exp(-x+a*math.log(x)-gammaln(a))
	    break
	 end
      end
   end
   return gamser
end

--- Continued fraction representation for incomplete gamma function Q.
--  @param a Order.
--  @param x Real value.
--  @return Representation of Q.
special._gcf_ = function (a,x)
   local ITMAX,EPS,FPMIN = 100, 3E-7, 1E-30
   local b,c = x+1.0-a, 1.0/FPMIN
   local d = 1.0/b
   local h,an,del = d
   for i = 1,ITMAX do
      an,b = -i*(i-a), b+2.0
      d,c = 1.0/lowBound(an*d+b,FPMIN), lowBound(b+an/c,FPMIN)
      del = d*c
      h = h*del
      if math.abs(del-1.0) < EPS then break end
   end
   return math.exp(-x+a*math.log(x)-special.gammaln(a))*h
end

--- Incomplete gamma function P(a,x).
--  @param a Order.
--  @param x Non-negative value.
--  @return Value of P(a,x).
special.gammp = function (a,x)
   if not (x >= 0.0 and a > 0) then error('Invalid arguments!') end
   return (x < a+1.0) and special._gammaSer_(a,x) or 1.0-special._gcf_(a,x)
end
special.about[special.gammp] = {"gammp(a,x)", "Incomplete gamma function P(a,x).", GAMMA}

--- Incomplete gamma function Q(a,x).
--  @param a Order.
--  @param x Non-negative value.
--  @return Value of Q(a.x).
special.gammq = function (a,x)
   if not (x >= 0.0 and a > 0) then error('Invalid arguments!') end
   return (x < a+1.0) and 1-special._gammaSer_(a,x) or special._gcf_(a,x)
end
special.about[special.gammq] = {"gammq(a,x)", "Incomplete gamma function Q(a,x) = 1-P(a,x).", GAMMA}

--- Other syntax for incomplete gamma function.
--  @param x Real value.
--  @param a Order.
--  @param tp Type of function (lower of upper).
--  @return Value of correspondent incomplete function.
special.gammainc = function (x,a,tp)
   tp = tp or 'lower'
   if     tp == 'lower' then return special.gammp(a,x)
   elseif tp == 'upper' then return special.gammq(a,x)
   else error('Unexpected type '..tostring(tp))
   end
end
special.about[special.gammainc] = {"gammainc(x,a[,type='lower')", "Incomplete gamma function, P (type=lower) or Q (type=upper).", GAMMA}

--- Beta function.
--  @param z First value.
--  @param w Second value.
--  @return B(z,w).
special.beta = function (z,w)
   return math.exp(special.gammaln(z)+special.gammaln(w)-special.gammaln(z+w))
end
special.about[special.beta] = {"beta(z,w)", "Beta function.", BETA}

--- Logarithm of beta function.
--  @param z First argument.
--  @param w Second argument.
--  @return log(B(x)).
special.betaln = function (z,w)
   return special.gammaln(z)+special.gammaln(w)-special.gammaln(z+w)
end
special.about[special.betaln] = {"betaln(z,w)", "Natural logarithm of beta function.", BETA}


--- Evaluates continued fraction for incomplete beta function by modified Lentz's method.
--  @param a First bound.
--  @param b Second bound.
--  @param x Value between 0 and 1.
--  @return Fraction value.
special._betacf = function (a,b,x)
   local MAXIT,EPS,FPMIN = 100, 3E-7, 1E-30
   local qab,qap,qam = a+b, a+1.0, a-1.0
   local c,d = 1.0, 1.0/lowBound(1.0-qab*x/qap,FPMIN)
   local h,m2,aa,del = d
   for m = 1,MAXIT do
      m2 = 2*m
      aa = m*(b-m)*x/((qam+m2)*(a+m2))
      d,c = 1.0/lowBound(1.0+aa*d,FPMIN), lowBound(1.0+aa/c,FPMIN)
      h = h*d*c
      aa = -(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
      d,c = 1.0/lowBound(1.0+aa*d,FPMIN), lowBound(1.0+aa/c,FPMIN)
      del = d*c
      h = h*del
      if math.abs(del-1.0) < EPS then break end
   end
   return h
end

--- Incomplete beta function
--  @param x Value between 0 and 1.
--  @param a First bound.
--  @param b Second bound.
--  @return Value of Ix(a,b).
special.betainc = function (x,a,b)
   if not (x >= 0.0 and x <= 1.0) then error("Expected x between 0 and 1!") end
   local bt
   if x == 0 or x == 1 then 
      bt = 0.0
   else
      bt = math.exp(special.gammaln(a+b)-special.gammaln(a)-special.gammaln(b)+a*math.log(x)+b*math.log(1.0-x))
   end
   return (x < (a+1.0)/(a+b+2.0)) and (bt*special._betacf(a,b,x)/a) or (1.0-bt*special._betacf(b,a,1.0-x)/b)
end
special.about[special.betainc] = {"betainc(x,a,b)", "Incomplete beta function Ix(a,b).", BETA}

--- Exponential integral.
--  @param n Power.
--  @param x Non-negative value.
--  @return Value of En(x).
special.expint = function (n,x)
   if x == nil then n,x = 1,n end
   if not (n >= 0 and x >= 0 and not (x == 0 and (n == 0 or n == 1))) then error('Bad arguments!') end
   if n == 0 then return math.exp(-x)/x end
   local nm1 = n-1
   if x == 0.0 then return 1.0/nm1 end
   local MAXIT, EULER, FPMIN, EPS = 100, 0.5772156649, 1E-30, 1E-7
   if x > 1.0 then
      local b, c = x+n, 1.0/FPMIN
      local d = 1.0/b
      local h = d
      for i = 1,MAXIT do
         local a = -i*(nm1+i)
	 b = b+2.0
	 d, c = 1.0/(a*d+b), b+a/c
	 local del = c*d
	 h = h*del
	 if math.abs(del-1.0) < EPS then return h*math.exp(-x) end
      end
   else
      local ans = (nm1 ~= 0) and 1.0/nm1 or -math.log(x)-EULER
      local fact, psy, del = 1.0
      for i = 1,MAXIT do
         fact = fact*(-x/i)
	 if i ~= nm1 then 
	    del = -fact/(i-nm1) 
	 else
	    psy = -EULER
	    for ii = 1,nm1 do psy = psy+1.0/ii end
	    del = fact*(-math.log(x)+psy)
	 end
	 ans = ans+del
	 if math.abs(del) < math.abs(ans)*EPS then return ans end
      end -- for i
   end -- if x
   error('Evaluation is failed!')
end
special.about[special.expint] = {"expint(n,x)", "Exponential integral En(x)."}

--- Complementary error function.
--  @param x Real value.
--  @return Error value.
special.erfc = function (x)
   local z = math.abs(x)
   local t = 1.0/(1+0.5*z)
   local ans = t*math.exp(-z*z-1.26551223+t*(1.00002368+t*(0.37409196+t*(0.09678418+t*(-0.18628806+t*
               (0.27886807+t*(-1.13520398+t*(1.48851587+t*(-0.82215223+t*0.17087277)))))))))
   return (x >= 0.0) and ans or (2.0-ans)
end
special.about[special.erfc] = {"erfc(x)", "Complementary error function."}

--- Error function.
--  @param x Real value.
--  @return Error value.
special.erf = function (x) return 1-special.erfc(x) end
special.about[special.erf] = {"erf(x)", "Error function."}

--- Legendre coefficient.
--  @param n Total order.
--  @param m Current order.
--  @param x Real number.
--  @return Pn_m(x)
special._plgndr_ = function (n,m,x)
   local pmm = 1.0
   if m > 0 then
      local somx2 = math.sqrt((1-x)*(1+x))
      local fact = 1.0
      for i = 1,m do 
         pmm = pmm*(-fact)*somx2
	 fact = fact+2.0
      end
   end
   if n == m then return pmm
   else
      local pmmp1 = x*(2*m+1)*pmm
      if n ~= m+1 then
         for ll = m+2,n do
	    pmmp1, pmm = (x*(2*ll-1)*pmmp1-(ll+m-1)*pmm)/(ll-m), pmmp1
	 end
      end
      return pmmp1
   end
end

--- List of Legendre coefficients.
--  @param n Polynomial order.
--  @param x Real number.
--  @return Table with coefficients.
special.legendre = function (n,x)
   assert(n >= 0 and math.abs(x) <= 1, 'Bad arguments')
   local res = {}
   local plgndr = special._plgndr_
   for i = 1,n+1 do res[i] = plgndr(n,i-1,x) end
   return res
end
special.about[special.legendre] = {"legendre(n,x)","Return list of Legendre polynomial coefficients."}

--- Dawson integral.
--  @param x Real number.
--  @return Integral value.
special.dawson = function (x)
   local NMAX,H,A1,A2,A3 = 6, 0.4, 2.0/3.0, 0.4, 2.0/7.0
    
   if not special._c_dawson then 
      -- List of Dawson function coefficients.
      special._c_dawson = {0,0,0,0,0,0}
      for i = 1,NMAX do special._c_dawson[i] = math.exp(-((2.0*i-1.0)*H)^2) end
   end
   local xx = math.abs(x)
   if xx < 0.2 then 
      local x2 = x*x
      return x*(1.0-A1*x2*(1.0-A2*x2*(1.0-A3*x2)))
   else
      local n0 = 2*math.floor(0.5*xx/H+0.5)
      local xp = xx-n0*H
      local e1 = math.exp(2.0*xp*H)
      local e2,d1 = e1*e1, n0+1
      local d2,sum = d1-2.0, 0.0
      for i = 1,NMAX do
         sum = sum + special._c_dawson[i]*(e1/d1+1.0/(d2*e1))
	 d1,d2,e1 = d1+2.0, d2-2.0, e1*e2
      end
      return 0.5641895835*sum*(x>=0 and math.exp(-xp*xp) or -math.exp(-xp*xp))
   end
end
special.about[special.dawson] = {"dawson(x)", "Dawson integral."}

--***** Bessel functions ******

--- Bessel function J0.
--  @param x Real number.
--  @return J0(x).
special._bessj0_ = function (x)
   local ax  = math.abs(x)
   local ans1, ans2, y
   if ax < 8.0 then
      y = x*x
      ans1 = 57568490574.0+y*(-13362590354.0+y*(651619640.7+y*(-11214424.18+y*(77392.33017-y*184.9052456))))
      ans2 = 57568490411.0+y*(1029532985.0+y*(9494680.718+y*(59272.64853+y*(267.8532712+y))))
      return ans1/ans2
   else
      local z = 8.0/ax
      y = z*z
      ans1 = 1.0+y*(-0.1098628627E-2+y*(0.2734510407E-4+y*(-0.2073370639E-5+y*0.2093887211E-6)))
      ans2 = -0.1562499995E-1+y*(0.1430488765E-3+y*(-0.6911147651E-5+y*(0.7621095161E-6-y*0.934935152E-7)))
      local xx = ax-0.785398164
      return math.sqrt(0.636619772/ax)*(math.cos(xx)*ans1-z*math.sin(xx)*ans2)
   end
end

--- Bessel function Y0.
--  @param x Non-negative number.
--  @return Y0(x).
special._bessy0_ = function (x)
   local ans1, ans2, y
   if x < 8.0 then
      y = x*x
      ans1 = -2957821389.0+y*(7062834065.0+y*(-512359803.6+y*(10879881.29+y*(-86327.92757+y*228.4622733))))
      ans2 = 40076544269.0+y*(745249964.8+y*(7189466.438+y*(47447.26470+y*(226.1030244+y))))
      return ans1/ans2+0.636619772*special._bessj0_(x)*math.log(x)
   else
      local z = 8.0/x
      y = z*z
      ans1 = 1.0+y*(-0.1098628627E-2+y*(0.2734510407E-4+y*(-0.2073370639E-5+y*0.2093887211E-6)))
      ans2 = -0.1562499995E-1+y*(0.1430488765E-3+y*(-0.6911147651E-5+y*(0.7621095161E-6-y*0.934945152E-7)))
      local xx = x-0.785398164
      return math.sqrt(0.636619772/x)*(math.sin(xx)*ans1+z*math.cos(xx)*ans2)
   end
end

--- Bessel function J1.
--  @param x Real number.
--  @return J1(x).
special._bessj1_ = function (x)
   local ax = math.abs(x)
   local ans1, ans2, y
   if ax < 8.0 then
      y = x*x
      ans1 = x*(72362614232.0+y*(-7895059235.0+y*(242396853.1+y*(-2972611.439+y*(15704.4826-y*30.16036606)))))
      ans2 = 144725228442.0+y*(2300535178.0+y*(18583304.74+y*(99447.43394+y*(376.9991397+y))))
      return ans1/ans2
   else
      local z = 8.0/ax
      y = z*z
      ans1 = 1.0+y*(0.183105E-2+y*(-0.3516396496E-4+y*(0.2457520174E-5-y*0.240337019E-6)))
      ans2 = 0.04687499995+y*(-0.2002690873E-3+y*(0.8449199096E-5+y*(-0.88228987E-6+y*0.105787412E-6)))
      local xx = ax-2.356194491
      ans1 = math.sqrt(0.636619772/ax)*(math.cos(xx)*ans1-z*math.sin(xx)*ans2)
      return (x >= 0) and ans1 or -ans1
   end
end

--- Bessel function Y1.
--  @param x Non-negative number.
--  @return Y1(x).
special._bessy1 = function (x)
   local ans1, ans2, y
   if x < 8.0 then
      y = x*x
      ans1 = x*(-0.4900604943E13+y*(0.1275274390E13+y*(-0.5153438139E11+y*(0.7349264551E9+y*(-0.4237922726E7+y*0.8511937935E4)))))
      ans2 = 0.2499580570E14+y*(0.4244419664E12+y*(0.3733650367E10+y*(0.2245904002E8+y*(0.1020426050E6+y*(0.3549632885E3+y)))))
      return ans1/ans2+0.636619772*(special._bessj1_(x)*math.log(x)-1.0/x)
   else
      local z = 8.0/x
      y = z*z
      ans1 = 1.0+y*(0.183105E-2+y*(-0.3516396496E-4+y*(0.2457520174E-5-y*0.240337019E-6)))
      ans2 = 0.04687499995+y*(-0.2002690873E-3+y*(0.8449199096E-5+y*(-0.88228987E-6+y*0.105787412E-6)))
      local xx = x-2.356194491
      return math.sqrt(0.636619772/x)*(math.sin(xx)*ans1+z*math.cos(xx)*ans2)
   end
end

--- Bessel function of the second kind
--  @param n Polynomial order.
--  @param x Non-negative real number.
--  @return Polynomial value
special.bessely = function (n,x)
   if x <= 0 then error('Positive value is expected!') end
   if not (n >= 0 and Ver.isInteger(n)) then error('Non-negative integer order is expected!') end
   if n == 0 then return special._bessy0_(x) end
   if n == 1 then return special._bessy1(x) end
   local tox = 2.0/x
   local by = special._bessy1(x)
   local bym = special._bessy0_(x)
   for i = 1,(n-1) do
      by, bym = i*tox*by-bym, by
   end
   return by
end
special.about[special.bessely] = {"bessely(n,x)","Bessel function of the second kind.", BESSEL}

--- Bessel function of the first kind
--  @param n Polynomial order.
--  @param x Real number.
--  @return Polynomial value
special.besselj = function (n,x)
   if not (n >= 0 and Ver.isInteger(n)) then error('Non-negative integer order is expected!') end
   if n == 0 then return special._bessj0_(x) end
   if n == 1 then return special._bessj1_(x) end
   if x == 0 then return 0 end
   local ACC, BIGNO, BIGNI = 40, 1E10, 1E-10
   
   local ax = math.abs(x)
   local tox = 2.0/ax
   local bj, bjm, ans
   if ax > n then
      bjm = special._bessj0_(ax)
      bj = special._bessj1_(ax)
      for i = 1,(n-1) do 
         bj, bjm = i*tox*bj-bjm, bm
      end
      ans = bj
   else
      local m = math.floor((n+math.floor(math.sqrt(ACC*n)))/2)*2 
      local jsum, sum = false, 0
      local bjp = 0
      bj, ans = 1, 0
      for i = m,1,-1 do
         bj, bjp = i*tox*bj-bjp, bj
	 if math.abs(bj) > BIGNO then
	    bj = bj*BIGNI
	    bjp = bjp*BIGNI
	    ans = ans*BIGNI
	    sum = sum*BIGNI
	 end
	 if jsum then sum = sum+bj end
	 jsum = not jsum
	 if i == n then ans = bjp end
      end
      sum = 2.0*sum-bj
      ans = ans/sum
   end
   return (x < 0.0 and (n % 2)==1) and -ans or ans
end
special.about[special.besselj] = {"besselj(n,x)", "Bessel function of the first kind.", BESSEL}

--- Modified Bessel function I0.
--  @param x Real number.
--  @return I0(x).
special._bessi0_ = function (x)
   local ax = math.abs(x)
   if ax < 3.75 then
      local y = x/3.75
      y = y*y
      return 1.0+y*(3.5156229+y*(3.0899424+y*(1.2067492+y*(0.2659732+y*(0.360768E-1+y*0.45813E-2)))))
   else
      local y = 3.75/ax
      return (math.exp(ax)/math.sqrt(ax))*(0.39894228+y*(0.1328592E-1+y*(0.225319E-2+y*(-0.157565E-2+y*(0.916281e-2+
                                           y*(-0.2057706E-1+y*(0.2635537E-1+y*(-0.1647633E-1+y*0.392377E-2))))))))
   end
end

--- Modified Bessel function K0.
--  @param x Non-negative number.
--  @return K0(x).
special._bessk0_ = function (x)
   if x <= 2.0 then
      local y = x*x/4.0
      return (-math.log(x/2.0)*special._bessi0_(x))+(-0.57721566+y*(0.42278420+y*(0.23069756+y*(0.3488590E-1+y*(0.262698E-2+y*(0.10750E-3+y*0.74E-5))))))
   else
      local y = 2.0/x
      return (math.exp(-x)/math.sqrt(x))*(1.25331414+y*(-0.7832358E-1+y*(0.2189568E-1+y*(-0.1062446E-1+y*(0.587872E-2+y*(-0.251540E-2+y*0.53208E-3))))))
   end
end

--- Modified Bessel function I1.
--  @param x Real number.
--  @return I1(x).
special._bessi1_ = function (x)
   local ax,ans = math.abs(x)
   if ax < 3.75 then
      local y = x/3.75
      y = y*y
      ans = ax*(0.5+y*(0.87890594+y*(0.51498869+y*(0.15084934+y*(0.2658733e-1+y*(0.301532E-2+y*0.32411E-3))))))
   else
      local y = 3.75/ax
      ans = 0.2282967E-1+y*(-0.2895312E-1+y*(0.1787654E-1-y*0.420059E-2))
      ans = 0.39894228+y*(-0.3988024E-1+y*(-0.362018E-2+y*(0.163801E-2+y*(-0.1031555E-1+y*ans))))
      ans = ans*(math.exp(ax)/math.sqrt(ax))
   end
   return (x < 0) and -ans or ans
end

--- Modified Bessel function K1.
--  @param x Non-negative number.
--  @return K1(x).
special._bessk1_ = function (x)
   if x <= 2.0 then
      local y = x*x/4.0
      return (math.log(x/2.0)*special._bessi1_(x))+(1.0/x)*(1.0+y*(0.15443144+y*(-0.67278579+y*(-0.18156897+y*(-0.1919402E-1+y*(-0.110404E-2-y*0.4686E-4))))))
   else
      local y = 2.0/x
      return (math.exp(-x)/math.sqrt(x))*(1.25331414+y*(0.23498619+y*(-0.3655620E-1+y*(0.1504268E-1+y*(-0.780353E-2+y*(0.325614E-2-y*0.68245E-3))))))
   end
end

--- Modified Bessel function Kn.
--  @param n Order.
--  @param x Positive value.
--  @return Kn(x).
special.besselk = function (n,x)
   if x <= 0 then error("Positive value is expected!") end
   if not (n >= 0 and Ver.isInteger(n)) then error("Non-negative integer order is expected!") end
   if n == 0 then return special._bessk0_(x) end
   if n == 1 then return special._bessk1_(x) end
   local tox,bkm,bk = 2.0/x, special._bessk0_(x), special._bessk1_(x)
   for j = 1,n-1 do
      bk, bkm = bkm+j*tox*bk, bk
   end
   return bk
end
special.about[special.besselk] = {"besselk(n,x)", "Modified Bessel function Kn(x).", BESSEL}

--- Modified Bessel function In.
--  @param n Order.
--  @param x Real number.
--  @return In(x).
special.besseli = function (n,x)
   assert(n >= 0 and Ver.isInteger(n), "Non-negative integer order is expected!")
   if n == 0 then return special._bessi0_(x) end
   if n == 1 then return special._bessi1_(x) end
   if x == 0 then return 0.0 end
   local ACC,BIGNO,BIGNI = 40.0, 1E10, 1E-10
   local tox = 2.0/math.abs(x)
   local bip,ans,bi = 0.0, 0.0, 1.0
   for j = 2*(n+math.floor(math.sqrt(ACC*n))),1,-1 do
      bi, bip = bip+j*tox*bi, bi
      if math.abs(bi) > BIGNO then
         ans = ans*BIGNI
	 bi = bi*BIGNI
	 bip = bip*BIGNI
      end
      if j == n then ans = bip end
   end
   ans = ans*special._bessi0_(x)/bi
   return (x < 0.0 and (n % 2)==1) and -ans or ans
end
special.about[special.besseli] = {"besseli(n,x)", "Modified Bessel function In(x).", BESSEL}

-- free memory in case of standalone usage
if not LC_DIALOG then special.about = nil end

return special
