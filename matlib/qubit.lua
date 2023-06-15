--[[		sonata/matlib/qubit.lua

--- Quantum computing simulation
--  </br></br><b>Authors</b>: Your Name

	module 'qubit'
--]]

-- Define here your tests, save results to 'ans',
-- use --> for the strict equality
-- and --n> for the n-digit precision in the case of floating numbers.
--[[TEST

-- use 'qubit'
Qb = require 'lib.qubit'

-- example
a = Qb()
-- check equality
ans = a.type                  -->  'qubit'

-- check relative equality ( ~10^(-2) )
ans = math.pi                --2> 355/113

--]]


--	LOCAL

local Ver = require('matlib.utils')
local Cross = Ver.cross
Ver = Ver.version

local Matrix = require('matlib.matrix')
local Complex = require('matlib.complex')

--	INFO

local help = SonataHelp or {}  -- optional
-- description
local about = {
__module__ = "Quantum computing simulation"
}


--	MODULE

local qubit = {
-- mark
type = 'qubit', isqubit = true,

_sign = {
-- basic
['0'] = 1, ['1'] = 1, 
-- hadamar
['+'] = 2, ['-'] = 2,
}
}
-- methametods
qubit.__index = qubit

local qgate = {
  type = 'qgate'
}
qgate.__index = qgate

--- Check object type.
--  @param v Object.
--  @return True if the object is qubit.
local function isqubit(v) return getmetatable(v) == qubit end


qubit._normalize = function (Q)
  local s = 0
  local vec = Q.vec
  for i = 1, vec:rows() do
    local v = vec[i][1]
    if getmetatable(v) == Complex then
      s = s + Cross.float(v * v:conj())
    else
      s = s + Cross.float(v)^2
    end
  end
  s = math.sqrt(s)
  for i = 1, vec:rows() do
    vec[i][1] = vec[i][1] / s
  end
end

qubit._randomize = function (Q)
  local vec = Q.vec
  local sum = 0
  for i = 1, vec:rows() do
    local v = Complex(2*math.random()-1, 2*math.random()-1)
    vec[i][1] = v
    sum = sum + v:re()^2 + v:im()^2
  end
  sum = math.sqrt(sum)
  for i = 1, vec:rows() do
    vec[i][1] = vec[i][1] / sum
  end
end


qubit._parse = function (state)
  k = k or 1
  local s = string.match(state, '^|(.+)>$')
  if not s then error("|..> is expected") end
  local n, sum, base = 0, 1, nil
  for w in string.gmatch(s, ".") do
    if not base then
      base = qubit._sign[w]
    else
      if qubit._sign[w] ~= base then error('wrong base') end
    end
    if w == '1' or w == '-' then
      sum = sum + 2^n
    end
    n = n + 1
  end
  local res = Matrix:zeros(2^n, 1)
  res[sum][1] = 1
  return res, base, n
end

--- Constructor example.
--  @param t Some value.
--  @return New object of qubit.
qubit.new = function(self, t)
  local o = {}
  -- your logic
  -- return object
  return setmetatable(o, self)
end
about[qubit.new] = {":new(t) --> Q", "Explicit constructor.", help.NEW}
-- begin from ':' to get 'Qb:new(t)'

qubit._init = function (v, tp, n)
  return setmetatable({vec=v, type=tp, n=n}, qubit)
end

qubit.__mul = function (C, Q)
  if isqubit(C) and not isqubit(Q) then
    return qubit.__mul(Q, C)
  end
  return qubit._init(C*Q.vec, Q.type, Q.n)
end

qubit.__add = function (Q1, Q2)
  if not (isqubit(Q1) and isqubit(Q2)) then error("Unexpected operation") end
  if Q1.type ~= Q2.type then error("Different bases") end
  if Q1.n ~= Q2.n then error("Different size") end
  return qubit._init(Q1.vec + Q2.vec, Q1.type, Q1.n)
end

qubit.combine = function (...)
  local qs = {...}
  local q1 = qs[1]
  if #qs <= 1 then return q1 end
  local v, n = q1.vec, q1.n
  for i = 2, #qs do
    local qi = qs[i]
    if not isqubit(qi) or qi.type ~= q1.type then error("Can't combine") end
    v = v:kron(qi.vec)
    n = n + qi.n
  end
  return qubit._init(v, q1.type, n)
end

qubit.__concat = function(Q1, Q2) return qubit.combine(Q1, Q2) end

qubit.size = function (Q) return Q.n end
qubit.__len = qubit.size

-- simplify constructor call
setmetatable(qubit, {
__call = function (self, state) 
  return qubit._init(qubit._parse(state))
end
})
about[qubit] = {" (t) --> Q", "Create new qubit.", help.NEW}
-- begin from ' ' to get 'Qb ()'


--- Method example.
--  It is good idea to define method for the copy creation.
--  @return Copy of the object.
qubit.copy = function (Q)
  -- some logic
  return qubit._init(Q.vec * 1, Q.type, Q.n)
end
about[qubit.copy] = {"Q:copy() --> cpy_Q",
  "Create a copy of the object."} -- third element is optional, default is 'base'
-- don't modify since start from letter


-- Comment to remove descriptions
qubit.about = about

--return qubit

--======================================
-- https://en.wikipedia.org/wiki/Quantum_logic_gate 
a = qubit('|1>')
b = qubit('|0>')
c = (0.5 * a + 0.5 * b) .. (0.4 * a + 0.6 * b)
c:_randomize()
print(c.vec)
print(c.vec:norm())
print(#c)
