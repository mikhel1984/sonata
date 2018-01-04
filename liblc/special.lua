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
special.about[special.gammaln] = {"gammaln(z)", "Natural logarithm of gamma function.", help.BASE}

special.beta = function (z,w)
   return math.exp(special.gammaln(z)+special.gammaln(w)-special.gammaln(z+w))
end
special.about[special.beta] = {"beta(z,w)", "Beta function.", help.BASE}

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
   local ans1, ans2
   if ax < 8.0 then
      local y = x*x
      ans1 = 57568490574.0+y*(-13362590354.0+y*(651619640.7+y*(-11214424.18+y*(77392.33017-y*184.9052456))))
      ans2 = 57568490411.0+y*(1029532985.0+y*(9494680.718+y*(59272.64853+y*(267.8532712+y))))
      return ans1/ans2
   else
      local z = 8.0/ax
      local y,xx = z*z, ax-0.785398164
      ans1 = 1.0+y*(-0.1098628627E-2+y*(0.2734510407E-4+y*(-0.2073370639E-5+y*0.2093887211E-6)))
      ans2 = -0.1562499995E-1+y*(0.1430488765E-3+y*(-0.6911147651E-5+y*(0.7621095161E-6-y*0.934935152E-7)))
      return math.sqrt(0.636619772/ax)*(math.cos(xx)*ans1-z*math.sin(xx)*ans2)
   end
end

local function bessy0 (x)
   assert(x > 0, 'Positive value is expected!')
   local ans1, ans2
   if x < 8.0 then
      local y = x*x
      ans1 = -2957821389.0+y*(7062834065.0+y*(-512359803.6+y*(10879881.29+y*(-86327.92757+y*228.4622733))))
      ans2 = 40076544269.0+y*(745249964.8+y*(7189466.438+y*(47447.26470+y*(226.1030244+y))))
      return ans1/ans2+0.636619772*bessj0(x)*math.log(x)
   else
      local z = 8.0/x
      local y,xx = z*z, x-0.785398164
      ans1 = 1.0+y*(-0.1098628627E-2+y*(0.2734510407E-4+y*(-0.2073370639E-5+y*0.2093887211E-6)))
      ans2 = -0.1562499995E-1+y*(0.1430488765E-3+y*(-0.6911147651E-5+y*(0.7621095161E-6-y*0.934935152E-7)))
      return math.sqrt(0.636619772/x)*(math.sin(xx)*ans1+z*math.cos(xx)*ans2)

   end
end

-- free memory in case of standalone usage
if not lc_version then special.about = nil end

return special
