--[[		sonata/matlib/fuzzy.lua

--- Fuzzy logic..
--
--  </br></br><b>Authors</b>: Your Name

	module 'fuzzy'
--]]

-- Define here your tests, save results to 'ans',
-- use --> for the strict equality
-- and --n> for the n-digit precision in the case of floating numbers.
--[[TEST_IT

-- use 'fuzzy'
Fz = require 'matlib.fuzzy'

-- example
a = Fz()
-- check equality
ans = a.type                  -->  'fuzzy'

-- check relative equality ( ~10^(-2) )
ans = math.pi               --.2> 355/113

--]]


--	LOCAL

local _ext = {
  utils = require("matlib.utils"),
  -- ap = require("matlib.asciiplot"),
}

-- common errors
local ERR_ORDER = "Wrong order"
local ERR_OPERATION = "Unexpected operation"

-- fuzzy set operations
local _op = {
  AND=1, OR=2, NOT=3,
}


--	INFO

local _help = SonataHelp or {}  -- optional
-- description
local _about = {
__module__ = "Fuzzy logic."
}


--	MODULE

-- Fuzzy set object.
local mt_set = {
  type = 'fuzzy_set',
  -- function default name
  _default_name = 'user_mf',
  -- environment settings
  _default_env = {
    AND = math.min,
    OR = math.max,
    NOT = function (x) return 1 - x end,
    -- defuzzification method
    DEFUZ = 'centroid',
    -- implication method
    IMPL = math.min,
    -- aggregation method
    AGG = math.max,
  }
}
mt_set.__index = mt_set


--- Make fuzzy set object.
--  @param s Function or other set.
--  @param op Operation to apply (optional).
--  @param nm Set name (optional).
local function _newSet (s, op, nm)
  local o = {
    set=s,
    op=op,
    name=nm
  }
  return setmetatable(o, mt_set)
end


--- Check and convert arguments.
--  @param S1 First set or function.
--  @param S2 Second set or function.
--  @return Same arguments or new fuzzy sets.
function _setArgs (S1, S2)
  if getmetatable(S1) ~= mt_set then S1 = _newSet(S1) end
  if getmetatable(S2) ~= mt_set then S2 = _newSet(S2) end
  return S1, S2
end


--- Find member function value.
--  @param x Input value.
--  @param env Environment settings.
--  @return membership.
mt_set.__call = function (self, x, env)
  env = env or mt_set._default_env
  return mt_set._eval(self, x, env)
end


--- Make string representation.
--  @return string.
mt_set.__tostring = function (self)
  return string.format("fuzzy %s",  mt_set._str(self))
end


--- Find membership function value of a fuzzy set.
--  @param x Input value.
--  @param env Environment settings.
--  @return membership.
mt_set._eval = function (self, x, env)
  if not self.op then
    return self.set(x)
  elseif self.op == _op.AND then
    return env.AND(self.set[1]:_eval(x, env), self.set[2]:_eval(x, env))
  elseif self.op == _op.OR then
    return env.OR(self.set[1]:_eval(x, env), self.set[2]:_eval(x, env))
  elseif self.op == _op.NOT then
    return env.NOT(self.set:_eval(x, env))
  end
  error(ERR_OPERATION)
end


--- Find membership values for the given domains, combine results.
--  @param p Table with domain values.
--  @param env Environment settings.
--  @return combined membership value.
mt_set._evalDomains = function (self, p, env)
  local d = self.domain
  if d then
    local v = p[d]
    if not v then
      error("No value for domain " .. d)
    end
    return mt_set._eval(self, v, env)
  end
  if not self.op then
    error "Unable to evaluate"
  elseif self.op == _op.AND then
    return env.AND(self.set[1]:_evalDomains(p, env), self.set[2]:_evalDomains(p, env))
  elseif self.op == _op.OR then
    return env.OR(self.set[1]:_evalDomains(p, env), self.set[2]:_evalDomains(p, env))
  elseif self.op == _op.NOT then
    return env.NOT(self.set:_evalDomains(p, env))
  end
  error(ERR_OPERATION)
end


--- Make string representation for set or operation.
--  @return string.
mt_set._str = function (self)
  if not self.op then
    return self.name or mt_set._default_name
  elseif self.op == _op.AND then
    return string.format("(%s & %s)", self.set[1]:_str(), self.set[2]:_str())
  elseif self.op == _op.OR then
    return string.format("(%s | %s)", self.set[1]:_str(), self.set[2]:_str())
  elseif self.op == _op.NOT then
    return "~" .. self.set:_str()
  end
  error(ERR_OPERATION)
end


--- Apply set intersection (AND, &).
--  @param S1 First set or function.
--  @param S2 Second set or function.
--  @return intersection of two sets.
mt_set.andf = function (S1, S2)
  return _newSet({_setArgs(S1, S2)}, _op.AND)
end
mt_set.__band = mt_set.andf


--- Make set copy.
--  @return new set with the same function and name.
mt_set.copy = function (self)
  return _newSet(self.set, self.op, self.name)
end


--- Apply defuzzification.
--  Available methods are: centroid, bisector, lom, som, mom.
--  @param rng Range table {begin, end}.
--  @param env Environment settings.
--  @return value from the input set.
mt_set.defuzzify = function (self, rng, env)
  env = env or mt_set._default_env
  local res, a, b = 0, rng[1], rng[2]
  local n = 100  -- TODO adaptive search
  local dx = (b - a)/n
  if env.DEFUZ == 'centroid' then
    -- center of gravity
    local num, denom = 0, 0
    for x = a, b, dx do
      local v = self(x)
      num = num + x*v
      denom = denom + v
    end
    res = (denom > 0) and (num/denom) or 0
  elseif env.DEFUZ == 'bisector' then
    -- divide into equal area
    local v = {[0]=0}
    for x = a, b, dx do
      v[#v+1] = v[#v] + dx*self(x)
    end
    local i = _ext.utils.utils.binsearch(v, v[#v]*0.5)  -- TODO improve accuracy
    res = a + (i-1)*dx
  else
    -- find maximum points
    local v = {}
    local i, pp, p = 0, 0, 0
    for x = a, b, dx do
      local vi = self(x)
      if pp <= p and p > vi then
        v[#v+1] = {i-1, p}
      end
      pp, p = p, vi
      i = i + 1
    end
    -- evaluate
    if #v == 0 then
      res = a
    elseif env.DEFUZ == 'lom' then
      -- largest of maximum
      local vmax = v[1]
      for i = 2, #v do
        if v[i][2] > vmax[2] then
          vmax = v[i]
        end
      end
      res = a + vmax[1]*dx
    elseif env.DEFUZ == 'som' then
      -- smallest of maximum
      local vmin = v[1]
      for i = 2, #v do
        if v[i][2] < vmin[2] then
          vmin = v[i]
        end
      end
      res = a + vmin[1]*dx
    elseif env.DEFUZ == 'mom' then
      -- middle of maximum (average of points)
      local sum = 0
      for _, vi in ipairs(v) do
        sum = sum + vi[1]
      end
      res = a + dx*(sum/#v)
    else
      error "Unknown method"
    end
  end
  return res
end


--- Apply set union (OR, |).
--  @param S1 First set or function.
--  @param S2 Second set or function.
--  @return union of two sets.
mt_set.orf = function (S1, S2)
  return _newSet({_setArgs(S1, S2)}, _op.OR)
end
mt_set.__bor = mt_set.orf


--- Apply set complement (NOT, ~).
--  @retrun complemented set.
mt_set.notf = function (self)
  return _newSet(self, _op.NOT)
end
mt_set.__bnot = mt_set.notf


--- Domain object.
local mt_domain = {
  type="fuzzy_domain",
}


--- Get fuzzy set or object method.
--  @param k Element name.
--  @return found element.
mt_domain.__index = function (self, k)
  return self._set[k] or mt_domain[k]
end


--- Add new fuzzy set.
--  @param k Fuzzy set name.
--  @param v Fuzzy set object.
mt_domain.__newindex = function (self, k, v)
  if not (v == nil or getmetatable(v) == mt_set) then
    error "Fuzzy set is expected"
  end
  if type(k) ~= "string" then
    error "Name must be string"
  end
  self._set[k] = v
  v.domain = self._name
end


--- Make new domain object.
--  @param rng Rangle table.
--  @param nm Domain name.
--  @return new domain.
mt_domain._new  = function (rng, nm)
  local o = {
    _range = rng,
    _name = nm,
    _set = {}
  }
  return setmetatable(o, mt_domain)
end


--- Get domain range.
--  @return range table.
mt_domain.range = function (self)
  return {self._range[1], self._range[2]}
end


--- Get list of sets inside the domain.
--  @return list of names.
mt_domain.sets = function (self)
  local t = {}
  for k in pairs(self._set) do t[#t+1] = k end
  return t
end


-- Fuzzy infirence system.
local fuzzy = {
-- mark
type = 'fuzzy',
}


--- Get domain or fuzzy object method.
--  @param k Parameter name.
--  @return parameter value.
fuzzy.__index = function (self, k)
  return self._domain[k] or fuzzy[k]
end


--- Evaluate membership value for current system state.
--  @param x Input value.
--  @return membership.
fuzzy._aggregate = function (self, x)
  local env = self._env
  local implicate, aggregate = env.IMPL, env.AGG
  local res = 0
  for _, r in ipairs(self._rules) do
    local v = r[2]:_eval(x, env)
    v = implicate(v, r[4])
    res = aggregate(res, v)
  end
  return res
end


--- Make new fuzzy infirence system.
--  @param env Environment settings.
fuzzy._new = function (env)
  env = env or {}
  local acc = {}
  for k, v in pairs(mt_set._default_env) do
    acc[k] = env[k] or v
  end
  local o = {
    _env = acc,
    _domain = {},
    _rules = {},
  }
  return setmetatable(o, fuzzy)
end


--- Create new domain.
--  @param range Domain range.
--  @param name Domain name.
--  @return new domain.
fuzzy.addDomain = function (self, range, name)
  assert(type(range) == 'table' and range[1] < range[2], "Wrong range")
  assert(type(name) == "string", "Name must be string")
  self._domain[name] = mt_domain._new(range, name)
  return self._domain[name]
end


--- Add new rule for mappint from input to output.
--  @param iset Input fuzzy set.
--  @param oset Output fuzzy set.
--  @param w Rule weight.
fuzzy.addRule = function (self, iset, oset, w)
  w = w or 1
  if getmetatable(iset) ~= mt_set or getmetatable(oset) ~= mt_set then
    error "Expected fuzzy set on input and output"
  end
  if w < 0 or w > 1 then
    error "Incorrect weight"
  end
  table.insert(self._rules, {iset, oset, w, 0})
end


--- Plot fuzzy set from the given domain.
--  @param domain Domain name.
--  @param set Set name (optional).
--  @return string with plots.
fuzzy.apPlot = function (self, domain, set)
  local dom = assert(self[domain], "Domain not found")
  _ext.ap = _ext.ap or require("matlib.asciiplot")
  local fig = _ext.ap()
  fig:setX {range = dom._range}
  if set then
    local s = assert(dom[set], "Set not found")
    fig:plot(s, set)
  else
    local t = {}
    for name, s in pairs(dom._set) do
      t[#t+1] = s
      t[#t+1] = name
    end
    fig:plot(table.unpack(t))
  end
  return fig
end


fuzzy.dsigmf = function (_, k1, m1, k2, m2)
  assert(m1 <= m2, ERR_ORDER)
  local fn = function (x)
    local v1 = k1*(x-m1)
    local v2 = k2*(x-m2)
    return 1/(1 + math.exp(-v1)) - 1/(1 + math.exp(-v2))
  end
  return _newSet(fn, nil, 'dsigmf')
end



--- Find oubput set and value for the given domain parameters.
--  @param p Domain values.
--  @return output value and fuzzy set.
fuzzy.evalFor = function (self, p)
  if #self._rules == 0 then return 0 end
  for _, r in ipairs(self._rules) do
    r[4] = r[3] * r[1]:_evalDomains(p, self._env)
  end
  local fset = _newSet(
    function (x) return self:_aggregate(x) end,
    nil, "aggregated")
  local nm = self._rules[1][2].domain
  fset.domain = nm
  self[nm]["ANS"] = fset
  local rng = self._domain[nm]._range
  local d = fset:defuzzify(rng, self._env)
  return d, fset
end


fuzzy.gaussmf = function (_, sig, mu)
  local fn = function (x)
    local v = (x-mu)/sig
    return math.exp(-v*v*0.5)
  end
  return _newSet(fn, nil, 'gaussmf')
end

fuzzy.gauss2mf = function (_, s1, m1, s2, m2)
  assert(m1 <= m2, ERR_ORDER)
  local fn = function (x)
    if x < m1 then
      local v = (x-m1)/s1
      return math.exp(-v*v*0.5)
    elseif x > m2 then
      local v = (x-m2)/s2
      return math.exp(-v*v*0.5)
    end
    return 1
  end
  return _newSet(fn, nil, 'gauss2mf')
end

fuzzy.gbellmf = function (_, w, p, m)
  assert(w ~= 0, "Wrong width")
  local p = 2*p
  local fn = function (x)
    local v = math.abs((x-m)/w)
    return 1/(1 + v^p)
  end
  return _newSet(fn, nil, 'gbellmf')
end



fuzzy.linsmf = function (_, a, b)
  assert(a <= b, ERR_ORDER)
  local k = (b > a) and (1/(b-a)) or 0
  local fn = function (x)
    if x < a then
      return 0
    elseif x > b then
      return 1
    end
    return k*(x-a)
  end
  return _newSet(fn, nil, 'linsmf')
end



fuzzy.linzmf = function (_, a, b)
  assert(a <= b, ERR_ORDER)
  local k = (b > a) and (-1/(b-a)) or 0
  local fn = function (x)
    if x < a then
      return 1
    elseif x > b then
      return 0
    end
    return 1 + k*(x-a)
  end
  return _newSet(fn, nil, 'linzmf')
end

fuzzy.newmf = function (_, fn, name)
  return _newSet(fn, nil, name)
end


fuzzy.pimf = function (_, a, b, c, d)
  assert(a < b and b <= c and c < d, ERR_ORDER)
  local m1, m2 = (a+b)*0.5, (c+d)*0.5
  local q1, q2 = (b-a), (d-c)
  local fn = function (x)
    if x < a then
      return 0
    elseif x < m1 then
      local v = (x-a)/q1
      return 2*v*v
    elseif x < b then
      local v = (x-b)/q1
      return 1 - 2*v*v
    elseif x < c then
      return 1
    elseif x < m2 then
      local v = (x-c)/q2
      return 1 - 2*v*v
    elseif x < d then
      local v = (x-d)/q2
      return 2*v*v
    end
    return 0
  end
  return _newSet(fn, nil, 'pimf')
end


fuzzy.psigmf = function (_, k1, m1, k2, m2)
  assert(m1 <= m2, ERR_ORDER)
  local fn = function (x)
    local v1 = k1*(x-m1)
    local v2 = k2*(x-m2)
    return 1/(1 + math.exp(-v1))/(1 + math.exp(-v2))
  end
  return _newSet(fn, nil, 'psigmf')
end



--- Update environment settings.
--  @param env New environment settings.
fuzzy.setEnv = function (self, env)
  for k, v in ipairs(env) do
    self._env[k] = v
  end
end

fuzzy.sigmf = function (_, k, m)
  local fn = function (x)
    local v = k*(x-m)
    return 1/(1 + math.exp(-v))
  end
  return _newSet(fn, nil, 'sigmf')
end


fuzzy.smf = function (_, a, b)
  assert(a < b, ERR_ORDER)
  local m, d = (a+b)*0.5, b-a
  local fn = function (x)
    if x < a then
      return 0
    elseif x < m then
      local v = (x-a)/d
      return 2*v*v
    elseif x < b then
      local v = (x-b)/d
      return 1 - 2*v*v
    end
    return 1
  end
  return _newSet(fn, nil, 'smf')
end


fuzzy.trapmf = function (_, a, b, c, d)
  assert(a <= b and b <= c and c <= d, ERR_ORDER)
  local k1 = (b > a) and (1/(b-a)) or 0
  local k2 = (d > c) and (-1/(d-c)) or 0
  local fn = function (x)
    if x < a or x > d then
      return 0
    elseif x < b then
      return k1*(x-a)
    elseif x < c then
      return 1
    end
    return 1 + k2*(x-c)
  end
  return _newSet(fn, nil, 'trapmf')
end


fuzzy.trimf = function (_, a, b, c)
  assert(a <= b and b <= c, ERR_ORDER)
  local k1 = (b > a) and (1/(b-a)) or 0
  local k2 = (c > b) and (-1/(c-b)) or 0
  local fn = function (x)
    if x < a or x > c then
      return 0
    elseif x < b then
      return k1*(x-a)
    end
    return 1 + k2*(x-b)
  end
  return _newSet(fn, nil, 'trimf')
end


fuzzy.zmf = function (_, a, b)
  assert(a < b, ERR_ORDER)
  local m, d = (a+b)*0.5, b-a
  local fn = function (x)
    if x < a then
      return 1
    elseif x < m then
      local v = (x-a)/d
      return 1 - 2*v*v
    elseif x < b then
      local v = (x-b)/d
      return 2*v*v
    end
    return 0
  end
  return _newSet(fn, nil, 'zmf')
end


-- simplify constructor call
setmetatable(fuzzy, {
__call = function (self, v)
  return fuzzy:_new(v)
end})
_about[fuzzy] = {" (t) --> F", "Create new fuzzy inference system.", _help.NEW}


-- Comment to remove descriptions
fuzzy.about = _about

--return fuzzy

--======================================
--TODO: print rules

--a = fuzzy:trapmf(0, 1, 2, 3)
--b = fuzzy:trimf(-2, 0, 1)
--local c = a & b | (~a) & b
--local env = mt_set._default_env
----env.DEFUZ = 'bisector'
--print(b:defuzzify({-2, 1}, env))

fz = fuzzy()
s1 = fz:addDomain({0, 10}, "service")
s1.poor = fuzzy:gaussmf(1.5, 0)
s1.good = fuzzy:gaussmf(1.5, 5)
s1.excellent = fuzzy:gaussmf(1.5, 10)

s2 = fz:addDomain({0, 10}, "food")
s2.rancid = fuzzy:trapmf(-2, 0, 1, 3)
s2.delicious = fuzzy:trapmf(7, 9, 10, 12)

s3 = fz:addDomain({0, 30}, "tip")
fz.tip.cheap = fuzzy:trimf(0, 5, 10)
fz.tip.average = fuzzy:trimf(10, 15, 20)
s3.generous = fuzzy:trimf(20, 25, 30)

fz:addRule(fz.service.poor | fz.food.rancid, fz.tip.cheap)
fz:addRule(fz.service.good, fz.tip.average)
fz:addRule(fz.service.excellent | fz.food.delicious, fz.tip.generous)

d, out = fz:evalFor {service = 2, food=5}
print(fz:apPlot("tip", "ANS"))
--print(d)
-- Ap = require("matlib.asciiplot")
-- fig = Ap()
-- fig:setX {range={-5,5}}
-- fig:setX {range=fz.tip:range()}
-- fig:plot(out)
-- print(fig)
