

local stat = {}

-- summ of all elements
stat.sum = function (t)
   local s = 0
   for i = 1, #t do s = s+t[i] end
   return s
end

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

stat.stdcorr = function (t)
   local mean = stat.mean(t)
   local sq, n = 0, #t
   for i = 1,#t do sq = sq + (t[i]-mean)^2 end
   local sigma = math.sqrt(sq/(n-1))
   return sigma, sigma/math.sqrt(n)
end

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

-- maximum value and position
stat.max = function (t)
   local m, k = t[1], 1
   for i = 2, #t do
      if t[i] > m then m, k = t[i], i end
   end
   return m,k
end

-- minimum value and position
stat.min = function (t)
   local m, k = t[1], 1
   for i = 2, #t do
      if t[i] < m then m, k = t[i], i end
   end
   return m,k
end

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

-- frequency of elements
stat.freq = function (t)
   local tmp, r = {}
   for _, v in ipairs(t) do
      r = tmp[v] or 0
      tmp[v] = r+1
   end
   local res = {}
   for k,v in pairs(tmp) do res[#res+1] = {k, v} end
   return res
end

---------------------
--      Test
---------------------

a = {1.1, 0.9, 1.2, 0.8, 1.2}
w = {1, 2, 1, 2, 1}
t = stat.freq(a)

for _, k in ipairs(t) do print(k[1], k[2]) end
