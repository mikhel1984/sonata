--[[      liblc/stat.lua 

--- Some statistical functions.
--  @author Stanislav Mikhel, 2017
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection.

            module 'stat'
--]]

-------------------- Tests -------------------
--[[!!
Stat = require 'liblc.stat'

X = {3,2,5,6,3,4,3,1}
w = {1,1,0,1,2,2,1,1}
ans = Stat.mean(X)            --~ 3.375

ans, tmp = Stat.std(X,W)      --~ 1.495

ans = tmp                     --~ 2.234

_,ans = Stat.max(X)           --> 4 

ans = Stat.median(X)          --> 3

tmp = Stat.freq(X)
ans = tmp[3]                  --> 3

ans = Stat.cmoment(2,X)       --~ 2.234

ans = Stat.moment(3,X,W)      --~ 61.875

ans = Stat.sum(X)             --> 27

ans = Stat.stdcorr(X)         --~ 1.598

ans = Stat.min(X)             --> 1

ans = Stat.geomean(X)         --~ 2.995

ans = Stat.harmean(X,W)       --~ 2.567
]]

-- magic numbers for gamma approximation
local magic = {676.5203681218851,-1259.1392167224028,771.32342877765313,-176.61502916214059,
               12.507343278686905,-0.13857109526572012,9.9843695780195716e-6,1.5056327351493116e-7}
-------------------------------------------- 
-- @class table
-- @name stat
-- @field about Function description collection.

local stat = {}
-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
stat.about = help:new("Statistical calculations. Data set must be a Lua table.")

--- Gamma function.
--    Lanczos approximation (based on Wikipedia) for real numbers.
--    <i>Private function.</i>
--    @param z Real number.
--    @return G(z).
local function gamma(z) 
   if z < 0.5 then
      return math.pi / (math.sin(math.pi*z) * gamma(1-z))
   else
      z = z-1
      local x = 0.99999999999980993
      for i = 1, #magic do x = x + magic[i]/(z+i) end
      local t = z + #magic - 0.5
      return math.sqrt(2*math.pi)*math.pow(t, z+0.5)*math.exp(-t)*x
   end
end

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

--- Summ of all elements.
--    @param t Table with numbers.
--    @return Summ.
stat.sum = function (t)
   local s = 0
   for i = 1, #t do s = s+t[i] end
   return s
end
stat.about[stat.sum] = {"sum(t)", "Get sum of all elements.", help.OTHER}

--- Average value.
--    @param t Table with numbers.
--    @param w Table with wiegth. Can be omitted.
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
stat.about[stat.mean] = {"mean(t[,w])", "Calculate average value. Weights are can be used.", help.BASE}

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
stat.about[stat.stdcorr] = {"stdcorr(t)", "Corrected value of standard deviation and variance.", help.BASE}

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
stat.about[stat.std] = {"std(t[,w])", "Standard deviation and variance. Weigths are can be used.", help.BASE}

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
      return math.pow(p, 1/#t)
   end
end
stat.about[stat.geomean] = {"geomean(t[,w])", "Geometrical mean.", help.OTHER}

--- Harmonical mean.
--    @param t Table of numbers.
--    @param w Table of weights. Can be omitted.
--    @return Harmonical mean.
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
stat.about[stat.harmean] = {"harmean(t[,w])", "Harmonical mean.", help.OTHER}

--- Find mediana.
--    @param p Table of numbers.
--    @return Value of mediana.
stat.median = function (p)
   local len = #p
   local t = table.move(p,1,len,1,{})
   table.sort(t)
   if len % 2 == 1 then 
      return t[(len+1)/2]
   else
      len = len / 2
      return (t[len] + t[len+1]) * 0.5
   end
end
stat.about[stat.median] = {"median(t)", "List median.", help.BASE}

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
stat.about[stat.freq] = {"freq(t)", "Return table with frequencies of elements.", help.BASE}

--- Central moment.
--    @param n Order of the moment.
--    @param x Table of numbers.
--    @param p Table of weights. Can be omitted.
--    @return Central moment value.
stat.cmoment = function (n, x, p)
   local pk, m = 1/#x, 0
   for i = 1,#x do m = m + x[i]*(p and p[i] or pk) end
   local mu = 0
   for i = 1,#x do mu = mu + math.pow(x[i]-m, n)*(p and p[i] or pk) end
   return mu
end
stat.about[stat.cmoment] = {"cmoment(n,x[,p])", "Central moment of x order n, p is a list of waights.", help.BASE}

--- Noncentral moment.
--    @param n Order of the moment.
--    @param x Table of numbers.
--    @param p Table of weights. Can be omitted.
--    @return Noncentral moment value.
stat.moment = function (n, x, p)
   local pk, m = 1/#x, 0
   for i = 1,#x do m = m + math.pow(x[i],n)*(p and p[i] or pk) end
   return m
end
stat.about[stat.moment] = {"moment(n,x[,p])", "Moment of x order n, p is a list of waights.", help.BASE}

-- free memory if need
if not lc_version then stat.about = nil end

return stat
