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
-- show sets
Ap = require 'matlib.asciiplot'

-- make fuzzy set
a = Fz:linsmf(1, 2)
-- get values
ans = a(-1)                   -->  0

ans = a(2.5)                  -->  1

-- apply intersection, equal to
--  a | Fz:linzmf(3, 4)
b = a:andf( Fz:linzmf(3, 4) )
ans = b(5)                    -->  0

-- defuzzification
ans = b:defuzzify({0, 5}, 'bisector')  --.1>  2.5

-- show structure
print(b)

-- infirence system with default settings
fs = Fz()
-- add domains
fs:addDomain({0, 40}, 'temperature')   -- input
fan = fs:addDomain({0, 100}, 'fan_speed')  -- output
-- add fuzzy setts
fs.temperature.cold = Fz:zmf(10, 16)
fs.temperature.warm = Fz:trapmf(12, 19, 26, 34)
fs.temperature.hot = Fz:smf(26, 32)
-- equaly, add to domain variable
fan.slow = Fz:linzmf(20, 40)
fan.moderate = Fz:trimf(35, 50, 80)
fan.high = Fz:linsmf(70, 90)

-- get components
lst = fs.temperature:getNames()
ans = #lst                    -->  3

-- get range
lst = fan:getRange()
ans = lst[1]                  -->  0

-- draw all sets
print(fs:apPlot('temperature'))
-- draw specific set
print(fs:apPlot('temperature', 'cold'))

-- define rules (if .., then .., weight)
fs:addRule(fs.temperature.cold, fs.fan_speed.slow)
fs:addRule(fs.temperature.warm, fs.fan_speed.moderate)
fs:addRule(fs.temperature.hot, fs.fan_speed.high, 0.8)

-- evaluate and defuzzify
val, out_set = fs {temperature = 28}
ans = val                  --.1>  60.7

-- result in the field ANS
fig = fs:apPlot('fan_speed', 'ANS')
fig:addLine(val, 0, val, out_set(val), ':')
print(fig)

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

-- tags
local _tag = {
  MF="fuzzy_set", FIS="inference_system", DOM="domain"
}


--	INFO

local _help = SonataHelp or {}  -- optional
-- description
local _about = {
__module__ = "Fuzzy logic elements."
}


--	MODULE

-- Fuzzy set object.
local mt_set = {
  type = 'fuzzy_set',
  -- function default name
  _defaultName = 'user_mf',
  -- environment settings
  _defaultEnv = {
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
--  @param params List of parameters (optional).
--  @return new fuzzy set.
local function _newSet (s, op, nm, params)
  local o = {
    set=s,
    op=op,
    name=nm,
    param = params
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
  env = env or mt_set._defaultEnv
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
    return string.format("%s(%s)",
      self.name or mt_set._defaultName,
      self.param and table.concat(self.param, ',') or "")
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
--  @param method(='centroid') Defuzzification method.
--  @return value from the input set.
mt_set.defuzzify = function (self, rng, method)
  method = method or 'centroid'
  local res, a, b = 0, rng[1], rng[2]
  local n = 100  -- TODO adaptive search
  local dx = (b - a)/n
  if method == 'centroid' then
    -- center of gravity
    local num, denom = 0, 0
    for x = a, b, dx do
      local v = self(x)
      num = num + x*v
      denom = denom + v
    end
    res = (denom > 0) and (num/denom) or 0
  elseif method == 'bisector' then
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
    elseif method == 'lom' then
      -- largest of maximum
      local vmax = v[1]
      for i = 2, #v do
        if v[i][2] > vmax[2] then
          vmax = v[i]
        end
      end
      res = a + vmax[1]*dx
    elseif method == 'som' then
      -- smallest of maximum
      local vmin = v[1]
      for i = 2, #v do
        if v[i][2] < vmin[2] then
          vmin = v[i]
        end
      end
      res = a + vmin[1]*dx
    elseif method == 'mom' then
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


--- Get domain object copy.
--  @return New domain object.
mt_domain._copy = function (self)
  local o = {
    _name = self._name,
    _range = {self._range[1], self._range[2]},
    _set = {}
  }
  for k, v in pairs(self._set) do o._set[k] = v end
  return setmetatable(o, mt_domain)
end


--- Make new domain object.
--  @param rng Rangle table.
--  @param nm Domain name.
--  @return new domain.
mt_domain._new  = function (rng, nm)
  local o = {
    _range = rng,
    _name = nm,
    _set = {},
  }
  return setmetatable(o, mt_domain)
end


--- Get list of sets inside the domain.
--  @return list of names.
mt_domain.getNames = function (self)
  local t = {}
  for k in pairs(self._set) do t[#t+1] = k end
  return t
end


--- Get domain range.
--  @return range table.
mt_domain.getRange = function (self)
  return {self._range[1], self._range[2]}
end


local mt_rule = {
  type = "fuzzy_rule"
}

mt_rule._new = function (fin, fout, weight)
  local o = {
    input = fin,
    output = fout,
    weight = weight,
    _res = 0,
  }
  return setmetatable(o, mt_rule)
end


-- Fuzzy infirence system.
local fuzzy = {
-- mark
type = 'fuzzy',
}


_about["_bin"] = {"sets: a | b, a & b, ~a", nil, _help.META}


--- Get domain or fuzzy object method.
--  @param k Parameter name.
--  @return parameter value.
fuzzy.__index = function (self, k)
  return self._domain[k] or fuzzy[k]
end


fuzzy.__newindex = function (self, k, v)
  if v == nil then
    if self._domain[k] then
      self._domain[k] = v
    elseif self._rules[k] then
      self._rules[k] = v
    end
  end
end


--- Evaluate membership value for current system state.
--  @param x Input value.
--  @return membership.
fuzzy._aggregate = function (self, x)
  local env = self._env
  local implicate, aggregate = env.IMPL, env.AGG
  local res = 0
  for _, r in ipairs(self._rules) do
    local v = r.output:_eval(x, env)
    v = implicate(v, r._res)
    res = aggregate(res, v)
  end
  return res
end


--- Find oubput set and value for the given domain parameters.
--  @param p Domain values.
--  @return output value and fuzzy set.
fuzzy._evalFor = function (self, p)
  if #self._rules == 0 then return 0 end
  for _, r in ipairs(self._rules) do
    r._res = r.weight * r.input:_evalDomains(p, self._env)
  end
  local fset = _newSet(
    function (x) return self:_aggregate(x) end,
    nil, "aggregated")
  local nm = self._rules[1].output.domain
  fset.domain = nm
  self[nm]["ANS"] = fset
  local rng = self._domain[nm]._range
  local d = fset:defuzzify(rng, self._env.DEFUZ)
  return d, fset
end
fuzzy.__call = fuzzy._evalFor


--- Make new fuzzy infirence system.
--  @param env Environment settings.
fuzzy._new = function (env)
  env = env or {}
  local acc = {}
  for k, v in pairs(mt_set._defaultEnv) do
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
_about[fuzzy.addDomain] = {"S:addDomain(range_t, name_s) --> D",
  "Add new domain to system, return reference.", _tag.FIS}


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
  table.insert(self._rules, mt_rule._new(iset, oset, w))
end
_about[fuzzy.addRule] = {"S:addRule(in_F, out_F, weight_d=1)",
  "Add new rule to system.", _tag.FIS}


-- Intersection.
fuzzy.andf = mt_set.andf
_about[fuzzy.andf] = {"F:andf(F2) --> new_F",
  "Fuzzy set intersection. Equal to F & F2.", _tag.MF}


--- Plot fuzzy set from the given domain.
--  @param domain Domain name.
--  @param set Set name (optional).
--  @return asciiplot object.
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
_about[fuzzy.apPlot] = {"S:apPlot(domain_s, set_s=nil) --> fig",
  "Visualize fuzzy set with asciiplot. Plot all the sets when name not defined.",
  _tag.FIS}


fuzzy.defuzzify = mt_set.defuzzify
_about[fuzzy.defuzzify] = {"F:defuzzify(range_t, method_s=centroid) --> value_d",
  "Defuzzification. Available methods are: centroid, bisector, lom, som, mom.",
  _tag.MF}

--- Fuzzy set with difference of two sigmoidal membership funcitons.
--  @param k1 Tile coefficient of the first function.
--  @param m1 First inflection point.
--  @param k2 Tilt coefficient of the second function.
--  @param m2 Second inflection point.
--  @return set object.
fuzzy.dsigmf = function (_, k1, m1, k2, m2)
  assert(m1 <= m2, ERR_ORDER)
  local fn = function (x)
    local v1 = k1*(x-m1)
    local v2 = k2*(x-m2)
    return 1/(1 + math.exp(-v1)) - 1/(1 + math.exp(-v2))
  end
  return _newSet(fn, nil, 'dsigmf', {k1, m1, k2, m2})
end
_about[fuzzy.dsigmf] = {":dsigmf(tilt1_d, inflect1_d, tilt2_d, inflect2_d) --> F",
  "Make new fuzzy set, member function is difference of two sigmoidal functions.",
   _tag.MF}


--- Fuzzy set with Gaussian membership function.
--  @param sig Standard deviation.
--  @param mu Mean value.
--  @return set object.
fuzzy.gaussmf = function (_, sig, mu)
  local fn = function (x)
    local v = (x-mu)/sig
    return math.exp(-v*v*0.5)
  end
  return _newSet(fn, nil, 'gaussmf', {sig, mu})
end
_about[fuzzy.gaussmf] = {":gaussmf(sigma_d, mu_d) --> F",
  "Make new fuzzy set with Gaussian member function.", _tag.MF}


--- Fuzzy set with 2 Gaussian membership functions.
--  Function value in between the functions equal 1.
--  @param s1 First standard deviation.
--  @param m1 First mean.
--  @param s2 Second standard deviation.
--  @param m2 Second mean.
--  @return set object.
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
  return _newSet(fn, nil, 'gauss2mf', {s1, m1, s2, m2})
end
_about[fuzzy.gauss2mf] = {":gauss2mf(sigma1_d, mu1_d, sigma2_d, mu2_d) --> F",
  "Make new fuzzy set, member function combines two Gaussians.", _tag.MF}


--- Fuzzy set with generalized bell-shaped membership function.
--  @param w Width.
--  @param p Power.
--  @param m Peak position.
--  @return set object.
fuzzy.gbellmf = function (_, w, p, m)
  assert(w > 0, "Wrong width")
  local p = 2*p
  local fn = function (x)
    local v = math.abs((x-m)/w)
    return 1/(1 + v^p)
  end
  return _newSet(fn, nil, 'gbellmf', {w, p, m})
end
_about[fuzzy.gbellmf] = {":gbellmf(width_d, power_d, mean_d) --> F",
  "Make new fuzzy set with generalized bell-shaped member function.", _tag.MF}


fuzzy.getNames = mt_domain.getNames
_about[fuzzy.getNames] = {"D:getNames() --> sets_t",
  "Get list of set names in domain.", _tag.DOM}


fuzzy.getRange = mt_domain.getRange
_about[fuzzy.getRange] = {"D:getRange() --> range_t",
  "Get domain range.", _tag.DOM}


--- Fuzzy set with linear s-shaped saturation membership function.
--  @param a Last 0 point.
--  @param b First 1 point.
--  @return set object.
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
  return _newSet(fn, nil, 'linsmf', {a, b})
end
_about[fuzzy.linsmf] = {":linsmf(left_d, right_d) --> F",
  "Make new fuzzy set with linear s-shaped saturation member function.",
  _tag.MF}


--- Fuzzy set with linear z-shaped saturation member function.
--  @param a Last 1 point.
--  @param b First 0 point.
--  @return set object.
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
  return _newSet(fn, nil, 'linzmf', {a, b})
end
_about[fuzzy.linzmf] = {":linzmf(left_d, right_d) --> F",
  "Make new fuzzy set with linear z-shaped saturated member function.",
  _tag.MF}


--- Make fuzzy set with own member function
--  f(x) -> [0, 1] for all real x.
--  @param fn Member function.
--  @param name Function name (optional).
--  @return set object.
fuzzy.newmf = function (_, fn, name)
  return _newSet(fn, nil, name)
end
_about[fuzzy.newmf] = {":newmf(member_fn, name_s=nil) --> F",
  "Make new fuzzy set with user defined member function.", _tag.MF}


-- Complement.
fuzzy.notf = mt_set.notf
_about[fuzzy.notf] = {"F:notf() --> new_F",
  "Fuzzy set complement. Equal to ~F.", _tag.MF}


-- Union.
fuzzy.orf = mt_set.orf
_about[fuzzy.orf] = {"F:orf(F2) --> new_F",
  "Fuzzy set union. Equal to F | F2.", _tag.MF}


--- Fuzzy set with pi-shaped member function.
--  @param a,b,c,d Trapeze bounds.
--  @return set object.
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
  return _newSet(fn, nil, 'pimf', {a, b, c, d})
end
_about[fuzzy.pimf] = {":pimf(lowLeft_d, upLeft_d, upRight_d, lowRight_d) --> F",
  "Make new fuzzy set with pi-shaped member function.", _tag.MF}


--- Fuzzy set with product of two sigmoidal membership funcitons.
--  @param k1 Tilt coefficient of the first function.
--  @param m1 First inflection point.
--  @param k2 Tile coefficient of the second function.
--  @param m2 Second inflection point.
--  @return set object.
fuzzy.psigmf = function (_, k1, m1, k2, m2)
  assert(m1 <= m2, ERR_ORDER)
  local fn = function (x)
    local v1 = k1*(x-m1)
    local v2 = k2*(x-m2)
    return 1/(1 + math.exp(-v1))/(1 + math.exp(-v2))
  end
  return _newSet(fn, nil, 'psigmf', {k1, m1, k2, m2})
end
_about[fuzzy.psigmf] = {":psigmf(tilt1_d, inflect1_d, tilt2_d, inflect2_d) --> F",
  "Make new fuzzy set with product of two sigmoidal member functions.",
  _tag.MF}


--- Update environment settings.
--  @param env New environment settings.
fuzzy.setEnv = function (self, env)
  for k, v in ipairs(env) do
    self._env[k] = v
  end
end
_about[fuzzy.setEnv] = {"S:setEnv(params_t)",
  "Update system environment.", _tag.FIS}


--- Fuzzy set with sigmoidal member function.
--  @param k Tilt coefficient.
--  @param m Inflection point.
--  @return set object.
fuzzy.sigmf = function (_, k, m)
  local fn = function (x)
    local v = k*(x-m)
    return 1/(1 + math.exp(-v))
  end
  return _newSet(fn, nil, 'sigmf', {k, m})
end
_about[fuzzy.sigmf] = {":sigmf(tilt_d, inflection_d) --> F",
  "Make new fuzzy set with sigmoidal member function.", _tag.MF}


--- Fuzzy set with s-shaped saturation membership function.
--  @param a Last 0 point.
--  @param b First 1 point.
--  @return set object.
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
  return _newSet(fn, nil, 'smf', {a, b})
end
_about[fuzzy.smf] = {":smf(left_d, right_d) --> F",
  "Make new fuzzy set with s-shaped saturation member function.", _tag.MF}


--- Fuzzy set with trapeze member function.
--  @param a Left bottom point.
--  @param b Left top point.
--  @param c Right top point.
--  @param d Right bottom point.
--  @return set object.
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
  return _newSet(fn, nil, 'trapmf', {a, b, c, d})
end
_about[fuzzy.trapmf] = {":trapmf(lowLeft_d, upLeft_d, upRight_d, lowRight_d) --> F",
  "Make new fuzzy set with trapeze member function.", _tag.MF}


--- Fuzzy set with triangle member function.
--  @param a Left bottom point.
--  @param b Top point.
--  @param c Right bottom point.
--  @return set object.
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
  return _newSet(fn, nil, 'trimf', {a, b, c})
end
_about[fuzzy.trimf] = {":trimf(left_d, up_d, right_d) --> F",
  "Make new fuzzy set with triangle member function.", _tag.MF}


--- Fuzzy set with z-shaped saturation membership function.
--  @param a Last 1 point.
--  @param b First 0 point.
--  @return set object.
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
  return _newSet(fn, nil, 'zmf', {a, b})
end
_about[fuzzy.zmf] = {":zmf(left_d, right_d) --> F",
  "Make new fuzzy set with z-shaped saturation member function.", _tag.MF}


-- simplify constructor call
setmetatable(fuzzy, {
__call = function (self, v)
  return fuzzy:_new(v)
end})
_about[fuzzy] = {" (env_t=nil) --> F", "Create new fuzzy inference system.", _help.NEW}


-- Comment to remove descriptions
fuzzy.about = _about

return fuzzy

--======================================
--TODO: print rules

