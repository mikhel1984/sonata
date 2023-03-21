--[[		sonata/lib/random.lua

--- Random number generators.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2023.

	module 'random'
--]]

--[[TEST

-- use 'random'
Rand = require 'lib.random'
Ap = require 'lib.asciiplot'
_D = require 'lib.data'
-- prepare for data generation
N = 100
normalize = function (lst, step)
  local d = step * N
  for i, v in ipairs(lst) do lst[i] = v / d end
end

-- default generator
lst = {}
for i = 1, N do
  lst[i] = Rand()  -- 0 to 1
end

-- visualize
ts, te = _D:histcounts(lst, 10)
normalize(ts)
fig = Ap()
fig:setY {view='min'}
v = _D:T({te, ts})  -- to list of pairs
fig:bar(v)
print(fig)

-- custom generator
rnd = Rand:new()
for i = 1, N do
  lst[i] = rnd()  -- 0 to 1
end

-- visualize renerated data
ts, te = _D:histcounts(lst, 10)
normalize(ts)
v = _D:T({te, ts})  -- to list of pairs
fig:bar(v)
print(fig)

-- normal distribution
mu, sig = 1, 0.5
for i = 1, N do
  lst[i] = rnd:norm(mu, sig)
end

-- visualize normal distribution
step = 4*sig / 10.0
lim = {}
for x = (mu-2*sig), (mu+2*sig), step do lim[#lim+1] = x end
ts, te = _D:histcounts(lst, lim)
normalize(ts)
fig = Ap(53, 19)
fig:setX {view='min'}
pdf = function (x) 
  return math.exp(-0.5*(x-mu)^2/sig^2) / (sig*math.sqrt(2*math.pi)) 
end
fig:plot(te, ts, 'random', pdf, 'pdf')
print(fig)

-- exponential distribution
lam = 0.5
for i = 1, N do
  lst[i] = Rand:exp(lam)
end

-- visualize
lim = {}
step = 0.5
for x = 0, 10*step, step do lim[#lim+1] = x end
ts, te = _D:histcounts(lst, lim)
normalize(ts)
-- pdf for normal distribution
pdf = function (x) return  lam*math.exp(-lam*x) end
fig:plot(te, ts, 'random', pdf, 'pdf')
print(fig)

-- logistic distribution
mu = 2; sig = 2
for i = 1, N do
  lst[i] = Rand:logistic(mu, sig)
end

-- visualize
lim = {}
step = 8*sig / 10.0
for x = mu-4*sig, mu+4*sig, step do lim[#lim+1] = x end
ts, te = _D:histcounts(lst, lim)
normalize(ts, step)
-- pdf for normal distribution
pdf = function (x) 
  local v = math.exp(-(x-mu)/sig)
  return v / sig / (1 + v)^2
end
fig:plot(te, ts, 'random', pdf, 'pdf')
print(fig)

-- Raileigh distribution
sig = 0.5
for i = 1, N do
  lst[i] = rnd:rayleigh(sig)
end

-- visualize
lim = {}
step = 4*sig / 10.0
for x = 0, 4*sig, step do lim[#lim+1] = x end
ts, te = _D:histcounts(lst, lim)
normalize(ts, step)
pdf = function (x) 
  return x/sig^2*math.exp(-0.5*x^2/sig^2)
end
fig:plot(te, ts, 'random', pdf, 'pdf')
print(fig)

-- Cauchy distribution
mu = 0; sig = 1
for i = 1, N do
  lst[i] = Rand:cauchy(mu, sig)
end

-- visualize
lim = {}
step = 6*sig / 10.0
for x = mu-3*sig, mu+3*sig, step do lim[#lim+1] = x end
ts, te = _D:histcounts(lst, lim)
normalize(ts, step)
pdf = function (x) 
  return 1/(math.pi*sig)/(1+(x-mu)^2/sig^2)
end
fig:plot(te, ts, 'random', pdf, 'pdf')
print(fig)


-- get random true/false
v = Rand:flip()
ans = type(v)                 --> 'boolean'

-- get integer from 1 to 10
v = Rand:int(10)
ans = (1 <= v and v <= 10)    --> true



-- random order iterator
a = {1, 2, 3, 4, 5}
for i, v in Rand:ipairs(a) do print(i, v) end

-- change order in place
Rand:shuffle(a)
for i, v in ipairs(a) do print(i, v) end

--]]

--	LOCAL

--- Check object type.
--  @param v Object.
--  @return True if the object is random.
local function israndom(v) return type(v)=='table' and v.israndom end

--	INFO

local help = SonataHelp or {}  -- optional
-- description
local about = {
__module__ = "Random number generators."
}

--	MODULE

local random = {
-- mark
type = 'random', israndom = true,
-- from 0 to 1
_fn = function (self) return math.random() end,
-- from a to b
_fnRng = function (self, a, b) return math.random(a, b) end,
}

random.__call = function (R) return R:_fn() end

-- methametods
random.__index = random

random.flip = function (R) return R:_fn() >= 0.5 end
about[random.flip] = {":flip() --> bool", "Uniform distributed binary value."}

random.int = function (R, N) return R:_fnRng(1, N) end
about[random.int] = {":int(N) -> int", "Uniform distributed random integer in range from 1 to N."}

--- Constructor example.
--  @param t Some value.
--  @return New object of random.
random.new = function(self, seed)
  local o = {
    _fn = random._rand,
    _fnRng = random._randRng,
  }
  random._init(o, seed or 0)
  return setmetatable(o, random)
end
about[random.new] = {":new() --> R", "Create generator object."}

random._genPM = function (t, rmax)
  rmax = rmax or 0.9999998
  local state = t._state
  local k = math.floor(state / 127773)
  state = 16807*(state - k*127773) - 2836*k   -- (16807*state + 2836) % 127773
  state = (state < 0) and (state + 2147483647) or state
  local j = math.floor(t._iy / 69273666) + 1  -- 1 to 32
  t._iy, t._iv[j] = t._iv[j], state
  t._state = state
  local tmp = 4.65661287E-10 * t._iy   -- y / 2147483647
  return tmp > rmax and rmax or tmp
end

random._genPMInit = function (t, seed)
  if seed < 1 then seed = 1 end
  local iv = {}
  for j = -4, 32 do
    local k = math.floor(seed / 127773)
    seed = 16807*(seed - k*127773) - 2836*k
    if seed < 0 then seed = seed + 2147483647 end
    if j > 0 then iv[j] = seed end   
  end
  t._state = seed
  t._iv, t._iy = iv, iv[#iv]
end

random._init = random._genPMInit

random._rand = random._genPM

random._randRng = function (R, a, b)
  local p, q = math.modf((b - a)*R:_rand())
  return a + (q < 0.5 and p or p + 1) 
end

random.norm = function (R, dMean, dev)
  dMean, dev = dMean or 0.0, dev or 1.0
  -- use Box-Muller transform
  local u, v, s = 0, 0, 0
  repeat
    u = 2*R:_fn()-1
    v = 2*R:_fn()-1
    s = u*u + v*v
  until 0 < s and s <= 1
  return dMean + dev * u * math.sqrt(-2*math.log(s)/s)
end
about[random.norm] = {":norm(mean_d=0, dev_d=1) --> float",
  "Normal distributed random value with the given mean and deviation."}

random.exp = function (R, dLam)
  local s = 0
  repeat s = R:_fn() until s > 0
  return -math.log(s) / (dLam or 1.0)
end

random.logistic = function (R, dMu, dSigma)
  dMu, dSigma = dMu or 0.0, dSigma or 1.0
  local s = 0
  repeat s = R:_fn() until 0 < s and s < 1
  return dMu + 0.551328895421792050*dSigma*math.log(s /(1.0 - s))
end

random.rayleigh = function (R, dSigma)
  local s = 0
  repeat s = R:_fn() until 0 < s and s < 1
  return dSigma*math.sqrt(-2*math.log(s))
end

random.cauchy = function (R, dMu, dSigma)
  dMu, dSigma = dMu or 0.0, dSigma or 1.0
  local u = 0
  repeat u = R:_fn() until 0 < u and u < 1
  return math.tan(math.pi*(u-0.5))*dSigma + dMu
end

random.gamma = function (R, iAlpha)
  if iAlpha < 1 then error("Alpha < 1") end
  if iAlpha < 6 then
    local x = 1.0
    for i = 1, iAlpha do x = x * R:_fn() end
    return -math.log(x)  -- TODO check for 0
  else
    local x, y, s= 0, 0, 0
    iAlpha = iAlpha - 1  -- reuse
    repeat
      repeat
        local v1, v2 = 0, 0
        repeat
          v1 = R:_fn()
          v2 = 2*R:_fn()-1.0
        until v1*v1 + v2*v2 <= 1
        y = v2 / v1
        s = math.sqrt(2.0*iAlpha + 1.0)
        x = s*y + iAlpha
      until x > 0
      local e = (1.0 + y*y)*math.exp(iAlpha*math.log(x/iAlpha) - s*y)
    until R:_fn() < e
    return x
  end
end

random.poisson = function (R, dLam)
  random.ext_special = random.ext_special or require('lib.special')
  local gln = random.ext_special.gammaln
  local em = -1
  if dLam < 12 then
    local g = math.exp(-dLam)
    local t = 1.0
    repeat
      t = t * R:_fn()
      em = em + 1
    until t <= g
  else
    local sq, al, y = math.sqrt(2*dLam), math.log(dLam), 0
    local g = dLam*al - gln(nil, dLam+1)
    repeat
      repeat
        y = math.tan(math.pi * R:_fn())
        em = sq*y + dLam
      until em >= 0
      em = math.floor(em)
      local t = 0.9*(1 + y*y)*math.exp(em*al - gln(nil, em + 1)-g)
    until R:_fn() <= t
  end
  return em
end

random.binomial = function (R, dp, N)
  random.ext_special = random.ext_special or require('lib.special')
  local gln = random.ext_special.gammaln
  local p = (dp < 0.5) and dp or (1 - dp)
  local bnl, am = 0, p*N
  if N < 25 then
    for j = 1, N do
      if R:_fn() < p then bnl = bnl + 1 end
    end
  elseif am < 1.0 then
    local g, t, j = math.exp(-am), 1.0, 0
    while j <= N do
      t = t*R:_fn()
      if t < g then break end
      j = j + 1
    end
    bnl = (j <= N) and j or N
  else
    local pc, og = 1 - p, gln(nil, N + 1)
    local sq, em = math.sqrt(2*am*pc), 0
    local plog, pclog = math.log(p), math.log(1 - p)
    repeat
      local y = 0
      repeat
        y = math.tan(math.pi*R:_fn())
        em = sq*y + am
      until 0.0 <= em and em < (N + 1)
      em = math.floor(em)
      local t = 1.2*sq*(1 + y*y)*math.exp(
        og - gln(nil, em + 1) - gln(nil, N - em + 1) + em*plog + (N - em)*pclog)
    until R:_fn() <= t
    bnl = em
  end
  return (p ~= dp) and (N - bnl) or bnl
end

random.seed = function (R, N)
  N = N or os.time()  -- set 'arbitrary' value by default
  if rawget(R, 'israndom') then
    math.randomseed(N)   -- common rand
  else
    random._init(R, N)   -- object specific rand
  end
end
about[random.seed] = {":seed([N]) --> nil", "Set random generator seed."}

random.shuffle = function (R, t)
  local N = #t
  for i = 1, N do
    local j = math.random(1, N)
    t[i], t[j] = t[j], t[i]
  end
end

random.ipairs = function (R, t)
  local ind, n = {}, 1
  for i = 1, #t do ind[i] = i end
  random.shuffle(R, ind)
  -- iterator
  return function ()
    local k = ind[n]
    if k then
      n = n + 1
      return k, t[k]
    end
  end
end

-- simplify constructor call
setmetatable(random, {__call = function (self) return random._fn(self) end})
about[random] = {" () --> float", "Uniform distributed random number between 0 and 1."}

-- Comment to remove descriptions
random.about = about

return random

--======================================
