--[[       liblc/special.lua

--- Special mathematical functions.
--  @author <a href="mailto:vpsys@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

           module 'special'
--]]

--------------- Tests --------------
--[[!!
Spec = require 'liblc.special'

ans = Spec.gamma(-1.5)             --~ 2.3633

ans = Spec.gammaln(100)            --~ 359.1342
]]

-- magic numbers for gamma approximation
local k_gamma = {676.5203681218851,-1259.1392167224028,771.32342877765313,-176.61502916214059,
               12.507343278686905,-0.13857109526572012,9.9843695780195716e-6,1.5056327351493116e-7}

local k_gammaln = {76.18009172947146,-86.50532032941677,24.01409824083091,-1.231739572450155,
                   0.1208650973866179E-2,-0.5395239384953E-5}

---------------------------------
-- @class table
-- @name special
local special = {}
special.__index = special

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
special.about = help:new("Special mathematical functions.")

--- Gamma function.
--    Lanczos approximation (based on Wikipedia) for real numbers.
--    <i>Private function.</i>
--    @param z Real number.
--    @return G(z).
special.gamma = function (z) 
   if z < 0.5 then
      return math.pi / (math.sin(math.pi*z) * special.gamma(1-z))
   else
      z = z-1
      local x = 0.99999999999980993
      for i = 1, #k_gamma do x = x + k_gamma[i]/(z+i) end
      local t = z + #k_gamma - 0.5
      return math.sqrt(2*math.pi)*math.pow(t, z+0.5)*math.exp(-t)*x
   end
end
special.about[special.gamma] = {"gamma(z)", "Gamma function.", help.BASE}

special.gammaln = function (z)
   local x,y = z,z
   local tmp = x+5.5
   tmp = tmp-(x+0.5)*math.log(tmp)
   local ser = 1.000000000190015
   for i = 1, #k_gammaln do y=y+1; ser = ser+k_gammaln[i]/y end
   return -tmp+math.log(2.5066282746310005*ser/x)
end
special.about[special.gammaln] = {"gammaln(z)", "Natural logarithm of gamma function.", help.OTHER}

special.beta = function (z,w)
   return math.exp(special.gammaln(z)+special.gammaln(w)-special.gammaln(z+w))
end
special.about[special.beta] = {"beta(z,w)", "Beta function.", help.BASE}

special.betaln = function (z,w)
   return special.gammaln(z)+special.gammaln(w)-special.gammaln(z+w)
end
special.about[special.betaln] = {"betaln(z,w)", "Natural logarithm of beta function.", help.OTHER}

special.expint = function (n,x)
   if x == nil then n,x = 1,n end
   assert(n >= 0 and x >= 0 and not (x == 0 and (n == 0 or n == 1)), 'Bad arguments!')
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
special.about[special.expint] = {"expint(n,x)", "Exponential integral En(x).", help.BASE}

--[[
special.ei = function (x)
   assert(x >= 0, "Positive argument is expected!")
   local EULER, MAXIT, FPMIN, EPS = 0.57721566, 100, 1E-30, 6E-8
   if x < FPMIN then return math.log(x)+EULER end
   local sum, term = 0.0, 1.0
   if x < -math.log(EPS) then
      local fact = 1.0
      for k = 1,MAXIT do
         fact = fact*x/k
	 term = fact/k
	 sum = sum+term
	 if term < sum*EPS then break end
      end
      return sum+math.log(x)+EULER
   else
      for k = 1,MAXIT do
         local prev = term
	 term = term*k/x
	 if term < EPS then break end
	 if term < prev then 
	    sum = sum+term
	 else
	    sum = sum-prev
	    break
	 end
      end
      return math.exp(x)*(1.0+sum)/x
   end
end
special.about[special.ei] = {"ei(x)", "Exponential integral Ei(x).", help.BASE}
]]

--[[
local function gRatio(x,y)
   local m = math.abs(math.max(x,y))
   if m <= 100 then
      return gamma(x)/gamma(y)
   else
      return math.pow(2,x-y)*gRatio(0.5*x,0.5*y)*gRatio(0.5*x+0.5,0.5*y+0.5)
   end
end

local function hyperGeom(a,b,c,z,N)
   N = N or 20
   local S,M = 1
   for i = 1,N do
      M = math.pow(z,i)
      for j = 0,i-1 do M = M * (a+j)*(b+j)/((1+j)*(c+j)) end
      S = S + M
   end
   return S
end

-- source: https://habrahabr.ru/post/307712/
stat.tdist = function (t,n)
   return 0.5+t*gRatio(0.5*(n+1),0.5*n)*hyperGeom(0.5,0.5*(n+1),1.5,-t*t/n)/math.sqrt(math.pi*n)
end
]]

special.erfc = function (x)
   local z = math.abs(x)
   local t = 1.0/(1+0.5*z)
   local ans = t*math.exp(-z*z-1.26551223+t*(1.00002368+t*(0.37409196+t*(0.09678418+t*(-0.18628806+t*
               (0.27886807+t*(-1.13520398+t*(1.48851587+t*(-0.82215223+t*0.17087277)))))))))
   return (x >= 0.0) and ans or (2.0-ans)
end
special.about[special.erfc] = {"erfc(x)", "Complementary error function.", help.BASE}

special.erf = function (x) return 1-special.erfc(x) end
special.about[special.erf] = {"erf(x)", "Error function.", help.BASE}

local function bessj0 (x)
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

local function bessy0 (x)
   local ans1, ans2, y
   if x < 8.0 then
      y = x*x
      ans1 = -2957821389.0+y*(7062834065.0+y*(-512359803.6+y*(10879881.29+y*(-86327.92757+y*228.4622733))))
      ans2 = 40076544269.0+y*(745249964.8+y*(7189466.438+y*(47447.26470+y*(226.1030244+y))))
      return ans1/ans2+0.636619772*bessj0(x)*math.log(x)
   else
      local z = 8.0/x
      y = z*z
      ans1 = 1.0+y*(-0.1098628627E-2+y*(0.2734510407E-4+y*(-0.2073370639E-5+y*0.2093887211E-6)))
      ans2 = -0.1562499995E-1+y*(0.1430488765E-3+y*(-0.6911147651E-5+y*(0.7621095161E-6-y*0.934945152E-7)))
      local xx = x-0.785398164
      return math.sqrt(0.636619772/x)*(math.sin(xx)*ans1+z*math.cos(xx)*ans2)

   end
end

local function bessj1 (x)
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

local function bessy1 (x)
   local ans1, ans2, y
   if x < 8.0 then
      y = x*x
      ans1 = x*(-0.4900604943E13+y*(0.1275274390E13+y*(-0.5153438139E11+y*(0.7349264551E9+y*(-0.4237922726E7+y*0.8511937935E4)))))
      ans2 = 0.2499580570E14+y*(0.4244419664E12+y*(0.3733650367E10+y*(0.2245904002E8+y*(0.1020426050E6+y*(0.3549632885E3+y)))))
      return ans1/ans2+0.636619772*(bessj1(x)*math.log(x)-1.0/x)
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
--    @param n Polynomial order.
--    @param x Nonnegative real number.
--    @return Polynomial value
special.bessy = function (n,x)
   assert(x > 0, 'Positive value is expected!')
   assert(n >= 0, 'Non-negative order is expected!')
   if n == 0 then return bessy0(x) end
   if n == 1 then return bessy1(x) end
   local tox = 2.0/x
   local by = bessy1(x)
   local bym = bessy0(x)
   for i = 1,(n-1) do
      by, bym = i*tox*by-bym, by
   end
   return by
end
special.about[special.bessy] = {"bessy(n,x)","Bessel function of the second kind.", help.BASE}

--- Bessel function of the first kind
--    @param n Polynomial order.
--    @param x Real number.
--    @return Polynomial value
special.bessj = function (n,x)
   assert(n >= 0, 'Non-negative order is expected!')
   if n == 0 then return bessj0(x) end
   if n == 1 then return bessj1(x) end
   if x == 0 then return 0 end
   local ACC, BIGNO, BIGNI = 40, 1E10, 1E-10
   
   local ax = math.abs(x)
   local tox = 2.0/ax
   local bj, bjm, ans
   if ax > n then
      bjm = bessj0(ax)
      bj = bessj1(ax)
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
   return (x < 0.0 and (n & 1)) and -ans or ans
end
special.about[special.bessj] = {"bessj(n,x)", "Bessel function of the first kind.", help.BASE}

-- free memory in case of standalone usage
if not lc_version then special.about = nil end

return special
