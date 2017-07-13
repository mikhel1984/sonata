--[[       stat.lua

------------ Examples -------------

Stat = require 'liblc.stat'

X = {3,2,5,6,3,4,3,1}
w = {1,1,0,1,2,2,1,1}

Stat.mean(X)                  --> 3.375

Stat.std(X,w)                 --> 1.314 1.728

Stat.stdcorr(X)               --> 1.598

Stat.median(X)                --> 3.0

tmp = Stat.freq(X)
tmp[3]                        --> 1.0

Stat.cmoment(2,X)             --> 2.234

This file is a part of liblc collection. 
Stanislav Mikhel, 2017.
]]

local stat = {}

local help = require "liblc.help"
--local help = require 'help'
stat.about = help:new("Statistical calculations. Data set must be a Lua table.")

-- magic numbers for gamma approximation
local magic = {676.5203681218851,-1259.1392167224028,771.32342877765313,-176.61502916214059,
               12.507343278686905,-0.13857109526572012,9.9843695780195716e-6,1.5056327351493116e-7}

-- gamma function Lanczos approximation (based on Wikipedia) for real numbers
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

-- summ of all elements
stat.sum = function (t)
   local s = 0
   for i = 1, #t do s = s+t[i] end
   return s
end
stat.about[stat.sum] = {"sum(t)", "Get sum of all elements.", help.OTHER}

-- average
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

-- corrected value of standard deviation and variance
stat.stdcorr = function (t)
   local mean = stat.mean(t)
   local sq, n = 0, #t
   for i = 1,#t do sq = sq + (t[i]-mean)^2 end
   local sigma = math.sqrt(sq/(n-1))
   return sigma, sigma/math.sqrt(n)
end
stat.about[stat.stdcorr] = {"stdcorr(t)", "Corrected value of standard deviation and variance.", help.BASE}

-- standard deviation and variance
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

-- maximum value and position
stat.max = function (t)
   local m, k = t[1], 1
   for i = 2, #t do
      if t[i] > m then m, k = t[i], i end
   end
   return m,k
end
stat.about[stat.max] = {"max(t)", "Maximal element and its index.", help.OTHER}

-- minimum value and position
stat.min = function (t)
   local m, k = t[1], 1
   for i = 2, #t do
      if t[i] < m then m, k = t[i], i end
   end
   return m,k
end
stat.about[stat.min] = {"min(t)", "Minimal element and its index.", help.OTHER}

-- geometrical mean
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

-- harmonical mean
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

-- get mediana
stat.median = function (t)
   table.sort(t)
   local len = #t
   if len % 2 == 1 then 
      return t[(len+1)/2]
   else
      len = len / 2
      return (t[len] + t[len+1]) * 0.5
   end
end
stat.about[stat.median] = {"median(t)", "List median.", help.BASE}

-- frequency of elements
stat.freq = function (t)
   local tmp, r = {}
   for _, v in ipairs(t) do
      r = tmp[v] or 0
      tmp[v] = r+1
   end
   return tmp
end
stat.about[stat.freq] = {"freq(t)", "Return table with frequencies of elements.", help.BASE}

-- central moment
stat.cmoment = function (n, x, p)
   local pk, m = 1/#x, 0
   for i = 1,#x do m = m + x[i]*(p and p[i] or pk) end
   local mu = 0
   for i = 1,#x do mu = mu + math.pow(x[i]-m, n)*(p and p[i] or pk) end
   return mu
end
stat.about[stat.cmoment] = {"cmoment(n,x[,p])", "Central moment of x order n, p is a list of waights.", help.BASE}

-- n moment
stat.moment = function (n, x, p)
   local pk, m = 1/#x, 0
   for i = 1,#x do m = m + math.pow(x[i],n)*(p and p[i] or pk) end
   return m
end
stat.about[stat.moment] = {"moment(n,x[,p])", "Moment of x order n, p is a list of waights.", help.BASE}

return stat

