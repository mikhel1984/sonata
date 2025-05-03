--[[		sonata/lib/random.lua

--- Random number generators.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2023-2025.

	module 'random'
--]]


--[[TEST_IT

-- use 'random'
Rand = require 'matlib.random'
Ap = require 'matlib.asciiplot'     -- for visualization
_D = require 'matlib.data'          -- hystogram
Spec = require 'matlib.special'     -- gamma function
-- prepare for data generation
N = 100  -- number of points
-- normalize hystograms
function normalize (lst, step)
  local d = step * N
  for i, v in ipairs(lst) do lst[i] = v / d end
end

-- default generator (0 to 1)
lst = {}
for i = 1, N do
  lst[i] = Rand()
end

-- visualize
ts, te = _D:histcounts(lst, 10)
normalize(ts, 1)
fig = Ap()
fig:setY {view='min'}; fig:setX {range={0, 0.5}, fix=true}
v = _D:T({te, ts})  -- get list of pairs
fig:bar(v)
print(fig)

-- custom generator (0 to 1)
rnd = Rand:new()
for i = 1, N do
  lst[i] = rnd()
end

-- visualize renerated data
ts, te = _D:histcounts(lst, 10)
normalize(ts, 1)
v = _D:T({te, ts})   -- list of pairs
fig:bar(v)
print(fig)

-- normal distribution
mu, sig = 1, 0.5
for i = 1, N do
  lst[i] = rnd:norm(mu, sig)
end

-- show normal distribution
fig = Ap(53, 19)
fig:setX {view='min'}
-- simplify visualization
function visualize (data, a, b, step, fn)
  local lim = {}
  for x = a, b, step do lim[#lim+1] = x end
  local ts, te = _D:histcounts(data, lim)
  normalize(ts, step)
  fig:plot(te, ts, 'random', fn, 'pdf')
  print(fig)
end
-- probability density function
pdf = function (x)
  return math.exp(-0.5*(x-mu)^2/sig^2) / (sig*math.sqrt(2*math.pi))
end
visualize(lst, mu-2*sig, mu+2*sig, 4*sig/10, pdf)

-- exponential distribution
lam = 0.5
for i = 1, N do
  lst[i] = Rand:exp(lam)
end

-- show exponential distribution
pdf = function (x) return  lam*math.exp(-lam*x) end
step = 0.5
visualize(lst, 0, 10*step, step, pdf)

-- logistic distribution
mu = 2; sig = 2
for i = 1, N do
  lst[i] = Rand:logistic(mu, sig)
end

-- show logistic distribution
pdf = function (x)
  local v = math.exp(-(x-mu)/sig)
  return v / sig / (1 + v)^2
end
visualize(lst, mu-4*sig, mu+4*sig, 8*sig/10, pdf)

-- Raileigh distribution
sig = 0.5
for i = 1, N do
  lst[i] = rnd:rayleigh(sig)
end

-- show Raileigh distribution
pdf = function (x)
  return x/sig^2*math.exp(-0.5*x^2/sig^2)
end
visualize(lst, 0, 4*sig, 4*sig/10, pdf)

-- Cauchy distribution
mu = 0; sig = 1
for i = 1, N do
  lst[i] = Rand:cauchy(mu, sig)
end

-- show Cauchy distribution
pdf = function (x)
  return 1/(math.pi*sig)/(1+(x-mu)^2/sig^2)
end
visualize(lst, mu-3*sig, mu+3*sig, 6*sig/10, pdf)

-- gamma distribution
alpha = 7; beta = 1
for i = 1, N do
  lst[i] = rnd:gamma(alpha, beta)
end

-- show gamma distribution
gamma = Spec:gamma(alpha)
pdf = function (x)
  return beta^alpha * x^(alpha-1) * math.exp(-beta*x)/gamma
end
visualize(lst, 2, 14, 1.2, pdf)

-- Poisson distribution
lambda = 4
for i = 1, N do
  lst[i] = Rand:poisson(lambda)
end

-- show Poisson distribution
-- find factorial
function fact(n)
  local p = 1
  for k = 2, n do p = p * k end
  return p
end
pmf = function (x)
  x = math.floor(x)  -- to 'integer'
  return lambda^x * math.exp(-lambda) / fact(x)
end
visualize(lst, 0, 10, 1, pmf)

-- binomial distribution
n = 20; p = 0.4
for i = 1, N do
  lst[i] = Rand:binomial(p, n)
end

-- show binomial distribution
pmf = function (x)
  x = math.floor(x)  -- to 'integer'
  return fact(n) * p^x * (1-p)^(n-x) / fact(x) / fact(n-x)
end
visualize(lst, 0, n, n/10, pmf)

-- get random true/false
v = Rand:flip()
ans = type(v)                 -->  'boolean'

-- get integer from 1 to 10
v = Rand:int(10)
ans = (1 <= v and v <= 10)    -->  true

-- random order iterator
a = {1, 2, 3, 4, 5}
for i, v in Rand:ipairs(a) do print(i, v) end

-- random choice
v, i = Rand:choice(a)
ans = v                       -->  a[i]

-- change order in place
Rand:shuffle(a)
for i, v in ipairs(a) do print(i, v) end

-- random chars
print(rnd:bytes(8))

-- same seed
r1 = Rand:new():seed(1)
r2 = Rand:new():seed(1)
ans = (r1() == r2())          -->  true

-- change seed
r2:seed(2)
ans = (r1() ~= r2())          -->  true

-- fill array 2x2x3
arr = Rand:array(2, 2, 3)
ans = #arr[1][1]              --> 3

ans = arr[2][1][2] <= 1.0     --> true

--]]


--	LOCAL

-- categories
DIST = 'distribution'


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


--- Recursive making of random array.
--  @param n Current size.
--  @param ... Rest of dimensions.
--  @return table with random tables or scalar value.
local function _arrayRest (self, n, ...)
  if n then
    if n <= 0 then error('expected positive size') end
    local res = {}
    for i = 1, n do res[i] = _arrayRest(self, ...) end
    return res
  else
    return self:_fn()
  end
end


--- Generate random from 0 to 1. Use generator of Park and Miller with
--  Bays-Durham shuffle ("Numerical recipes in C")
--  @param t Random object.
--  @param rmax Maximal value less or equal to 1.0.
--  @return random number.
local function _genPM (t, rmax)
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


--- Initialize Park-Miller generator.
--  @param t Generator object.
--  @param seed Integer value.
local function _genPMInit (t, seed)
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


--- Get random integer from range.
--  @param R Random generator.
--  @param a Lower bound.
--  @param b Upper bound.
--  @return Integer value.
local function _randRng (R, a, b)
  local p, q = math.modf((b - a)*R:_rand())
  return a + (q < 0.5 and p or p + 1)
end


-- make object callable
random.__call = function (R) return R:_fn() end


-- methametods
random.__index = random


-- set generator
random._init = _genPMInit
random._rand = _genPM


--- Get array of random numbers [0;1) with given size.
--  @param ... sequence of dimensions.
--  @return multidimentional random array.
random.array = function (self, ...)
  return _arrayRest(self, ...)
end
about[random.array] = {"R:array(n1,[n2,..]) --> tbl", 
  "Get multidimentional random array."}


--- Binomial distribution.
--  @param dp Probability (p).
--  @param N Total number of attempts.
--  @return Random value.
random.binomial = function (self, dp, N)
  random.ext_special = random.ext_special or require('matlib.special')
  local gln = random.ext_special.gammaln
  local p = (dp < 0.5) and dp or (1 - dp)
  local bnl, am = 0, p*N
  if N < 25 then
    for j = 1, N do
      if self:_fn() < p then bnl = bnl + 1 end
    end
  elseif am < 1.0 then
    local g, t, j = math.exp(-am), 1.0, 0
    while j <= N do
      t = t*self:_fn()
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
        y = math.tan(math.pi*self:_fn())
        em = sq*y + am
      until 0.0 <= em and em < (N + 1)
      em = math.floor(em)
      local t = 1.2*sq*(1 + y*y)*math.exp(
        og - gln(nil, em+1) - gln(nil, N-em+1) + em*plog + (N-em)*pclog)
    until self:_fn() <= t
    bnl = em
  end
  return (p ~= dp) and (N - bnl) or bnl
end
about[random.binomial] = {"R:binomial(p_d, N) --> int",
  "Binomial distributed random values.", DIST}


--- Generate sequence of bytes.
--  @param N Sequence length.
--  @return String with random bytes.
random.bytes = function (self, N)
  local res = {}
  for i = 1, N do res[i] = string.char(self:_fnRng(33, 126)) end
  return table.concat(res)
end
about[random.bytes] = {"R:bytes(N) --> str", "Get sequence of random bytes."}


--- Cauchy distribution.
--  @param dMu Mean value.
--  @param dSigma Scale.
--  @return Random number.
random.cauchy = function (self, dMu, dSigma)
  dMu, dSigma = dMu or 0.0, dSigma or 1.0
  local u = 0
  repeat u = self:_fn() until 0 < u and u < 1
  return math.tan(math.pi*(u-0.5))*dSigma + dMu
end
about[random.cauchy] = {"R:cauchy(mu_d=0, sigma_d=1) --> float",
  "Cauchy distributed random numbers.", DIST}


--- Get random element from the list.
--  @param t Source table.
--  @return Table element and index.
random.choice = function (self, t)
  local i = self:_fnRng(1, #t)
  return t[i], i
end
about[random.choice] = {"R:choice(tbl) --> element, index_N",
  "Get random table element."}


--- Exponential distribution.
--  @param dLam Lambda.
--  @return random number.
random.exp = function (self, dLam)
  local s = 0
  repeat s = self:_fn() until s > 0
  return -math.log(s) / (dLam or 1.0)
end
about[random.exp] = {"R:exp(lambda_d=1) --> float",
  "Exponential distributed random values.", DIST}


--- Get random binary value.
--  @param p Probability of 'true'.
--  @return True of false.
random.flip = function (self, p) return self:_fn() <= (p or 0.5) end
about[random.flip] = {"R:flip(p=0.5) --> bool",
  "Uniform distributed binary value."}


--- Gamma distribution.
--  @param iAlpha Alpha.
--  @param iBeta Beta.
--  @return random number.
random.gamma = function (self, iAlpha, dBeta)
  if iAlpha < 1 then
    error "Alpha < 1"
  end
  local x = 1.0
  if iAlpha < 6 then
    for i = 1, iAlpha do x = x * self:_fn() end
    x = -math.log(x)  -- TODO can be 0?
  else
    local y, s= 0, 0
    iAlpha = iAlpha - 1  -- reuse
    repeat
      repeat
        local v1, v2 = 0, 0
        repeat
          v1 = self:_fn()
          v2 = 2*self:_fn()-1.0
        until v1 > 0 and v1*v1 + v2*v2 <= 1
        y = v2 / v1
        s = math.sqrt(2.0*iAlpha + 1.0)
        x = s*y + iAlpha
      until x > 0
      local e = (1.0 + y*y)*math.exp(iAlpha*math.log(x/iAlpha) - s*y)
    until self:_fn() < e
  end
  return x / (dBeta or 1)
end
about[random.gamma] = {"R:gamma(alpha_N, beta_d=1) --> float",
  "Gamma distributed random values.", DIST}


--- Get random integer.
--  @param N1 Lower bound or total number.
--  @param N2 Upper bound or nil.
--  @return Integer value.
random.int = function (self, N1, N2)
  N1, N2 = (N2 and N1 or 1), (N2 or N1)
  return self:_fnRng(N1, N2)
end
about[random.int] = {"R:int(lower_i=1, upper_i) -> int",
  "Uniform distributed random integer in the given range.", DIST}


--- Iterate randomly without repeat.
--  @param t Source table.
--  @return iterator function over the source table.
random.ipairs = function (self, t)
  local ind, n = {}, 1
  for i = 1, #t do ind[i] = i end
  random.shuffle(self, ind)
  -- iterator
  return function ()
    local k = ind[n]
    if k then
      n = n + 1
      return k, t[k]
    end
  end
end
about[random.ipairs] = {"R:ipairs(tbl) --> iterator_fn",
  "Random iterator over the table elements."}


--- Logistic distribution.
--  @param dMu Mean value.
--  @param dSigma Scale.
random.logistic = function (self, dMu, dSigma)
  dMu, dSigma = dMu or 0.0, dSigma or 1.0
  local s = 0
  repeat s = self:_fn() until 0 < s and s < 1
  return dMu + 0.551328895421792050*dSigma*math.log(s /(1.0 - s))
end
about[random.logistic] = {"R:logistic(mu_d=0, sigma_d=1) --> float",
  "Logistic distributed random value.", DIST}


--- Constructor example.
--  @return New random generator.
random.new = function(_)
  local o = {
    _fn = random._rand,
    _fnRng = _randRng,
  }
  random._init(o, 0)
  return setmetatable(o, random)
end
about[random.new] = {":new() --> R",
  "Create random generator object.", help.NEW}


--- Get Gaussian distribution.
--  @param dMean Mean value.
--  @dev Deviation.
--  @return random number.
random.norm = function (self, dMean, dev)
  dMean, dev = dMean or 0.0, dev or 1.0
  -- use Box-Muller transform
  local u, v, s = 0, 0, 0
  repeat
    u = 2*self:_fn()-1
    v = 2*self:_fn()-1
    s = u*u + v*v
  until 0 < s and s <= 1
  return dMean + dev * u * math.sqrt(-2*math.log(s)/s)
end
about[random.norm] = {"R:norm(mean_d=0, dev_d=1) --> float",
  "Normal distributed random value with the given mean and deviation.", DIST}


--- Poisson distribution.
--  @param dLam Lambda.
--  @return random number.
random.poisson = function (self, dLam)
  random.ext_special = random.ext_special or require('matlib.special')
  local gln = random.ext_special.gammaln
  local em = -1
  if dLam < 12 then
    local g = math.exp(-dLam)
    local t = 1.0
    repeat
      t = t * self:_fn()
      em = em + 1
    until t <= g
  else
    local sq, al, y = math.sqrt(2*dLam), math.log(dLam), 0
    local g = dLam*al - gln(nil, dLam+1)
    repeat
      repeat
        y = math.tan(math.pi * self:_fn())
        em = sq*y + dLam
      until em >= 0
      em = math.floor(em)
      local t = 0.9*(1 + y*y)*math.exp(em*al - gln(nil, em + 1)-g)
    until self:_fn() <= t
  end
  return em
end
about[random.poisson] = {"R:poisson(lambda_d) --> int",
  "Poisson distributed random values.", DIST}


--- Rayleigh distribution.
--  @param dSigma Scale.
--  @return random number.
random.rayleigh = function (self, dSigma)
  local s = 0
  repeat s = self:_fn() until 0 < s and s < 1
  return dSigma*math.sqrt(-2*math.log(s))
end
about[random.rayleigh] = {"R:rayleigh(sigma_d) --> float",
  "Rayleigh distributed random values.", DIST}


--- Update random generator seed.
--  @param N Seed value.
random.seed = function (self, N)
  N = N or os.time()  -- set 'arbitrary' value by default
  if rawget(self, 'israndom') then
    math.randomseed(N)   -- common rand
  else
    random._init(self, N)   -- custom rand
  end
  return self
end
about[random.seed] = {"R:seed(N=os.time) --> R", "Set random generator seed."}


--- Change elements order in the table.
--  @param t Table.
random.shuffle = function (_, t)
  local N = #t
  for i = 1, N do
    local j = math.random(1, N)
    t[i], t[j] = t[j], t[i]
  end
end
about[random.shuffle] = {"R:shuffle(tbl)", "Change order of elements in place."}


-- simplify constructor call
setmetatable(random, {__call = function (self) return random._fn(self) end})
about[random] = {" () --> float",
  "Uniform distributed random number between 0 and 1."}


-- Comment to remove descriptions
random.about = about

return random

--======================================
-- TODO add another custom generator
