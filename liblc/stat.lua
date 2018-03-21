--[[      liblc/stat.lua 

--- Some statistical functions.
--  @author <a href="mailto:vpsys@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

            module 'stat'
--]]

-------------------- Tests -------------------
--[[!!
Stat = require 'liblc.stat'

-- initial data (tables)
X = {3,2,5,6,3,4,3,1}
w = {1,1,0,1,2,2,1,1}
-- average
ans = Stat.mean(X)                --~ 3.375

-- standard deviation
ans, tmp = Stat.std(X,W)          --~ 1.495

-- variance
ans = tmp                         --~ 2.234

-- maximum element and index
_,ans = Stat.max(X)               --> 4 

-- median
ans = Stat.median(X)              --> 3

-- table of frequencies
tmp = Stat.freq(X)
ans = tmp[3]                      --> 3

-- central moment
ans = Stat.cmoment(2,X)           --~ 2.234

-- non-central moment
ans = Stat.moment(3,X,W)          --~ 61.875

-- summ of elements
ans = Stat.sum(X)                 --> 27

-- corrected standard deviation
ans = Stat.stdcorr(X)             --~ 1.598

-- minimum value
ans = Stat.min(X)                 --> 1

-- geometrical mean
ans = Stat.geomean(X)             --~ 2.995

-- harmonic mean
ans = Stat.harmean(X,W)           --~ 2.567

-- find histogram
h = Stat.hyst(X,4)
ans = h.Y[1]                      --> 2

-- Poisson cdf and pdf
ans = Stat.poisscdf(3, 0.5)       --~ 0.998

ans = Stat.poisspdf(5, 1.1)       --~ 4.467E-3

-- Chi2 cdf and pdf
ans = Stat.chi2cdf(0.5, 2)        --~ 0.2212

ans = Stat.chi2pdf(1.2, 2.1)      --~ 0.2748

-- Student cdf and pdf
ans = Stat.tcdf(4, 2.5)           --~ 0.9805

ans = Stat.tpdf(2, 3.3)           --~ 0.0672

-- F cdf and pdf
ans = Stat.fcdf(0.8, 1.1, 2.2)    --~ 0.5285

ans = Stat.fpdf(1.3, 2.7, 2.4)    --~ 0.2174

-- Gauss cdf and pdf
ans = Stat.normcdf(1, 1.5, 2.1)   --~ 0.4059

ans = Stat.normpdf(0.7, 0.5, 0.8) --~ 0.4833
]]

local Ver = require "liblc.versions"

-------------------------------------------- 
-- @class table
-- @name stat
-- @field about Description of functions.

local stat = {}
-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
stat.about = help:new("Statistical calculations. Data set must be a Lua table.")

-- some functions depend of module 'special'
local done
done, stat.lc_special = pcall(require,'liblc.special')
if not done then
   print('WARNING >> Not available: poisscdf(), chi2cdf(), chi2pdf(), tcdf(), tpdf(), normcdf()')
   stat.lc_special = nil
end

--- Sum of all elements.
--    @param t Table with numbers.
--    @return Sum.
stat.sum = function (t)
   local s = 0
   for i = 1, #t do s = s+t[i] end
   return s
end
stat.about[stat.sum] = {"sum(t)", "Get sum of all elements.", help.OTHER}

--- Average value.
--    @param t Table with numbers.
--    @param w Table with weight. Can be omitted.
--    @return Average.
stat.mean = function (t, w)
   if w then
      local st, sw = 0, 0
      for i = 1, #t do
         st = st + t[i]*w[i]
	 sw = sw + w[i]
      end
      return st / sw
   else
      return stat.sum(t) / #t
   end
end
stat.about[stat.mean] = {"mean(t[,w])", "Calculate average value. Weights are can be used.", }

--- Corrected value of standard deviation and variance.
--    @param t Table of numbers.
--    @return Standard deviation, variance.
stat.stdcorr = function (t)
   local mean = stat.mean(t)
   local sq, n = 0, #t
   for i = 1,#t do sq = sq + (t[i]-mean)^2 end
   local sigma = math.sqrt(sq/(n-1))
   return sigma, sigma/math.sqrt(n)
end
stat.about[stat.stdcorr] = {"stdcorr(t)", "Corrected value of standard deviation and variance.", }

--- Standard deviation and variance.
--    @param t Table of numbers.
--    @param w Table of weights.
--    @return Standard deviation, variance.
stat.std = function (t, w)
   local mean = stat.mean(t,w)
   local disp = 0
   if w then
      local sw = 0
      for i = 1,#t do
         disp = disp + w[i]*(t[i]-mean)^2 
	 sw = sw + w[i]
      end
      disp = disp / sw
   else
      for i = 1,#t do disp = disp + (t[i]-mean)^2 end
      disp = disp / #t
   end
   return math.sqrt(disp), disp 
end
stat.about[stat.std] = {"std(t[,w])", "Standard deviation and variance. Weights are can be used.", }

--- Maximum value.
--    @param t Table of numbers.
--    @return Maximum value and its index.
stat.max = function (t)
   local m, k = t[1], 1
   for i = 2, #t do
      if t[i] > m then m, k = t[i], i end
   end
   return m,k
end
stat.about[stat.max] = {"max(t)", "Maximal element and its index.", help.OTHER}

--- Minimum value.
--    @param t Table of numbers.
--    @return Minimum value and its index.
stat.min = function (t)
   local m, k = t[1], 1
   for i = 2, #t do
      if t[i] < m then m, k = t[i], i end
   end
   return m,k
end
stat.about[stat.min] = {"min(t)", "Minimal element and its index.", help.OTHER}

--- Geometrical mean.
--    @param t Table of numbers.
--    @param w Table of weights. Can be omitted.
--    @return Geometrical mean.
stat.geomean = function (t, w)
   if w then
      local st, sw = 0, 0
      for i = 1,#t do 
         st = st + w[i]*math.log(t[i])
	 sw = sw + w[i]
      end
      return math.exp(st / sw)
   else
      local p = 1
      for i = 1, #t do p = p * t[i] end
      return p^(1/#t)
   end
end
stat.about[stat.geomean] = {"geomean(t[,w])", "Geometrical mean.", help.OTHER}

--- Harmonic mean.
--    @param t Table of numbers.
--    @param w Table of weights. Can be omitted.
--    @return Harmonic mean.
stat.harmean = function (t, w)
   if w then
      local st, sw = 0, 0
      for i = 1,#t do
         st = st + w[i]/t[i]
	 sw = sw + w[i]
      end
      return sw / st
   else
      local h = 0
      for i = 1, #t do h = h + 1/t[i] end
      return #t / h
   end
end
stat.about[stat.harmean] = {"harmean(t[,w])", "Harmonic mean.", help.OTHER}

--- Find median.
--    @param p Table of numbers.
--    @return Value of median.
stat.median = function (p)
   local len = #p
   local t = Ver.move(p,1,len,1,{})
   table.sort(t)
   if len % 2 == 1 then 
      return t[(len+1)/2]
   else
      len = len / 2
      return (t[len] + t[len+1]) * 0.5
   end
end
stat.about[stat.median] = {"median(t)", "List median.", }

--- Frequency of elements.
--    @param t Table of numbers.
--    @return Table where keys are elements and values are their frequencies.
stat.freq = function (t)
   local tmp = {}
   for _, v in ipairs(t) do
      tmp[v] = (tmp[v] or 0) + 1
   end
   return tmp
end
stat.about[stat.freq] = {"freq(t)", "Return table with frequencies of elements.", }

--- Central moment.
--    @param n Order of the moment.
--    @param x Table of numbers.
--    @param p Table of weights. Can be omitted.
--    @return Central moment value.
stat.cmoment = function (n, x, p)
   local pk, m = 1/#x, 0
   for i = 1,#x do m = m + x[i]*(p and p[i] or pk) end
   local mu = 0
   for i = 1,#x do mu = mu + (x[i]-m)^n*(p and p[i] or pk) end
   return mu
end
stat.about[stat.cmoment] = {"cmoment(n,x[,p])", "Central moment of x order n, p is a list of weights.", }

--- Non-central moment.
--    @param n Order of the moment.
--    @param x Table of numbers.
--    @param p Table of weights. Can be omitted.
--    @return Non-central moment value.
stat.moment = function (n, x, p)
   local pk, m = 1/#x, 0
   for i = 1,#x do m = m + (x[i])^n*(p and p[i] or pk) end
   return m
end
stat.about[stat.moment] = {"moment(n,x[,p])", "Moment of x order n, p is a list of weights.", }

--- Histogram of the data distribution.
--    @param t Data table.
--    @param N Number of intervals (default is 10).
--    @param a Low boundary (default is the minimum value).
--    @param b High boundary (default is the maximum value).
--    @return Table, where X is scale and Y is frequency.
stat.hyst = function (t,N,a,b)
   -- find limits
   if not (a and b) then
      local max, min = -math.huge, math.huge
      for _,v in ipairs(t) do
         if v > max then
	    max = v
	 elseif v < min then
	    min = v
	 end
      end
      a = a or min
      b = b or max
   end
   assert(a <= b, 'Wrong limits!')
   -- number of intervals
   N = N or 10
   local res = {X={},Y={}}
   -- X values
   local step = (b-a)/N
   res.X[0] = a
   for i = 1,N do res.X[i] = res.X[i-1]+step; res.Y[i] = 0 end
   -- Y values
   for _,v in ipairs(t) do
      if v >= a and v <= b then
         local pos = math.floor((v-a)/step)+1
         if pos > N then pos = N end -- for v == b
         local prev = res.Y[pos]
         res.Y[pos] = prev+1 
      end
   end
   return res
end
stat.about[stat.hyst] = {"hyst(data[,N[,a,b]])", "Find hystogram for given data on the interval [a,b] with N ranges. Return table with scale (X) and frequencies (Y).", help.OTHER}

--- Poisson cumulative distribution.
--    @param x Value.
--    @param lam Mean parameter.
--    @return Cumulative value.
stat.poisscdf = function (x,lam)
   return stat.lc_special.gammq(x+1,lam)
end
stat.about[stat.poisscdf] = {"poisscdf(x,lam)", "Poisson cumulative distribution."}

--- Poisson density function.
--    @param x Value.
--    @param lam Mean patameter.
--    @return Density value.
stat.poisspdf = function (x,lam)
   assert(lam >= 0, 'Wrong argument!')
   if Ver.tointeger(x) == nil then return 0.0 end
   local f = 1
   for i = 1,x do f = f*i end
   return lam^x*math.exp(-lam)/f
end
stat.about[stat.poisspdf] = {"poisspdf(x,lam)", "Poisson distribution density."}

--- Chi-square cumulative distribution
--    @param x Value.
--    @param v Degree of freedom.
--    @return Cumulative value.
stat.chi2cdf = function (x,v)
   return stat.lc_special.gammp(v/2,x/2)
end
stat.about[stat.chi2cdf] = {"chi2cdf(x,v)", "Chi-square cumulative distribution."}

--- Chi-square density function.
--    @param x Value.
--    @param v Degree of freedom.
--    @return Density value.
stat.chi2pdf = function (x,v)
   if x <= 0 then return 0 end
   local v2 = 0.5*v
   return x^(v2-1)*math.exp(-x*0.5)/(2.0^v2*stat.lc_special.gamma(v2))
end
stat.about[stat.chi2pdf] = {"chi2pdf(x,v)", "Chi-square distribution density."}

--- Student's cumulative distribution
--    @param x Value.
--    @param nu Degree of freedom.
--    @return Cumulative value.
stat.tcdf = function (x,nu)
   local tmp = nu/(nu+x*x)
   return 1-0.5*stat.lc_special.betainc(tmp,0.5*nu,0.5)
end
stat.about[stat.tcdf] = {"tcdf(x,nu)", "Student's cumulative distribution."}

--- Student's density function.
--    @param x Value.
--    @param nu Degree of freedom.
--    @return Density value.
stat.tpdf = function (x,nu)
   local tmp = math.sqrt(nu)*stat.lc_special.beta(0.5,0.5*nu)
   return (1+x*x/nu)^(-0.5*(nu+1))/tmp
end
stat.about[stat.tpdf] = {"tpdf(x,nu)", "Student's distribution density."}

--- F cumulative distribution
--    @param x Value.
--    @param v1 Numerator degree of freedom.
--    @param v2 Denomenator degree of freedom.
--    @return Cumulative value.
stat.fcdf = function (x,v1,v2)
   local tmp = v1*x/(v2+v1*x)
   return stat.lc_special.betainc(tmp,v1*0.5,v2*0.5)
end
stat.about[stat.fcdf] = {"fcdf(x,v1,v2)", "F cumulative distribution."}

--- F density function.
--    @param x Value.
--    @param v1 Numerator degree of freedom.
--    @param v2 Denomenator degree of freedom.
--    @return Density value.
stat.fpdf = function (x,v1,v2)
   local tmp = (v1*x)^v1*v2^v2/(v1*x+v2)^(v1+v2)
   return math.sqrt(tmp)/(x*stat.lc_special.beta(0.5*v1,0.5*v2))
end
stat.about[stat.fpdf] = {"fpdf(x,v1,v2)", "F distribution density."}

--[[
-- Binomial cumulative distribution
-- @param x Value.
-- @param N Number of trails.
-- @param p Probability of success.
stat.binocdf = function (x,N,p)
   return stat.lc_special.betainc(p,x,N-x+1)
end
]]

--- Normal cumulative distribution
--    @param x Value.
--    @param mu Shift.
--    @param sig Width.
--    @return Cumulative value.
stat.normcdf = function (x,mu,sig)
   mu,sig = mu or 0, sig or 1
   return 0.5*(1+stat.lc_special.erf((x-mu)/(sig*1.4142135623731)))
end
stat.about[stat.normcdf] = {"normcdf(x,mu,sig)", "Normal cumulative distribution."}

--- Normal density function.
--    @param x Value.
--    @param mu Shift.
--    @param sig Width.
--    @return Density value.
stat.normpdf = function (x,mu,sig)
   mu,sig = mu or 0, sig or 1
   return math.exp(-0.5*((x-mu)/sig)^2)/math.sqrt(2*math.pi*sig*sig)
end
stat.about[stat.normpdf] = {"normpdf(x,nu,sig)", "Normal distribution density."}

-- free memory if need
if not lc_version then stat.about = nil end

return stat

--====================================
-- TODO: binomial distribution
