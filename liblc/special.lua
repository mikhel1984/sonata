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



-- free memory in case of standalone usage
if not lc_version then special.about = nil end

return special
