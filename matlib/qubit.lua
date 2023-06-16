--[[		sonata/matlib/qubit.lua

--- Quantum computing simulation.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2023.

	module 'qubit'
--]]

---------------- Tests --------------
--[[TEST

-- use 'qubit'
Qb = require 'matlib.qubit'

a = 0.2*Qb'|0>' + 0.98*Qb'|1>'

--]]

--	LOCAL

local Ver = require('matlib.utils')
local Cross = Ver.cross
Ver = Ver.version

local Matrix = require('matlib.matrix')
local Complex = require('matlib.complex')


--- Rule for CNOT operation.
--  @param bits List of booleans.
--  @param slave Index to change.
--  @parma master Index to check.
local function ruleCnot (bits, slave, master)
  if bits[master] then
    bits[slave] = not bits[slave]
  end
end


--- Rule for swap operation.
--  @param bits List of booleans.
--  @param i1 Index of the first element.
--  @param i2 Index of the second element.
local function ruleSwap (bits, i1, i2)
  bits[i1], bits[i2] = bits[i2], bits[i1]
end


--	INFO

local help = SonataHelp or {}  -- optional
-- description
local about = {
__module__ = "Quantum computing simulation"
}


--	QUBIT

local qubit = {
-- mark
type = 'qubit', isqubit = true,
-- bases
_sign = {
-- basic
['0'] = 1, ['1'] = 1, 
-- hadamar
['+'] = 2, ['-'] = 2}
}
-- methametods
qubit.__index = qubit


--- Check object type.
--  @param v Object.
--  @return True if the object is qubit.
local function isqubit(v) return getmetatable(v) == qubit end


--- Q1 + Q2
--  @param Q1 First state.
--  @param Q2 Second state.
--  @return Combined state.
qubit.__add = function (Q1, Q2)
  if not (isqubit(Q1) and isqubit(Q2)) then error("Unexpected operation") end
  if Q1.type ~= Q2.type then error("Different bases") end
  if Q1.n ~= Q2.n then error("Different size") end
  return qubit._new(Q1.vec + Q2.vec, Q1.type, Q1.n)
end


--- k * Q
--  @param C Coefficient.
--  @param Q State.
--  @return Scaled state.
qubit.__mul = function (C, Q)
  if isqubit(C) and not isqubit(Q) then
    return qubit.__mul(Q, C)
  end
  return qubit._new(C*Q.vec, Q.type, Q.n)
end


--- Q1 .. Q2
--  @param Q1 First state.
--  @param Q2 Second state.
--  @return Combination of states.
qubit.__concat = function(Q1, Q2) return qubit.combine(Q1, Q2) end


--- Initialize new qubit system.
--  @param v State vector.
--  @param tp Type of basis.
--  @param n Number of qubits.
--  @return new object.
qubit._new = function (v, tp, n)
  return setmetatable({vec=v, type=tp, n=n}, qubit)
end


--- Get state from the string representation.
--  @param state String of the form '|xxx>'.
--  @return vector, base type, size
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


--- Combine qubits into the system.
--  @param ... Sequence of subsystems.
--  @return combined state.
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
  return qubit._new(v, q1.type, n)
end


--- Getting copy.
--  @return Copy of the object.
qubit.copy = function (Q)
  -- some logic
  return qubit._new(Q.vec * 1, Q.type, Q.n)
end
about[qubit.copy] = {"Q:copy() --> cpy_Q", "Create a copy of the object."}


--- Normalize coefficients to unit vector.
--  @param Q Qubit system.
qubit.normalize = function (Q)
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


--- Get number of qubits.
--  @param Q Qubit object.
--  @return number of qubits in the system.
qubit.size = function (Q) return Q.n end
qubit.__len = qubit.size


-- simplify constructor call
setmetatable(qubit, {
__call = function (self, state) 
  return qubit._new(qubit._parse(state))
end
})
about[qubit] = {" (t) --> Q", "Create new qubit.", help.NEW}


--	QGATE

local qgate = {
  type = 'qgate',
}
qgate.__index = qgate


--- Print gates.
--  @param G System of gates.
--  @return string representation.
qgate.__tostring = function (G)
  local acc = {}
  for i = G.n, 1, -1 do
    acc[#acc+1] = string.format('|x%d> %s', i-1, G.txt[i])
  end
  return table.concat(acc, '\n')
end


-- Basic matrices.
qgate._I22 = Matrix:eye(2)
qgate._X22 = Matrix{{0,1},{1,0}}
qgate._Y22 = Matrix{{0,Complex:i(-1)}, {Complex:i(1), 0}}
qgate._Z22 = Matrix{{1, 0}, {0, -1}}
qgate._S22 = Matrix{{1, 0}, {0, Complex:i(1)}}
qgate._T22 = Matrix{{1, 0}, {0, Complex:trig(1, math.pi/4)}}
qgate._H22 = Matrix{{1,1},{1,-1}}  -- multipy to 1/sqrt(2)


--- Update matrix and representation for basic components.
--  @param G Gate system.
--  @param lst List of gates (nil for I).
--  @param s Symbol to print.
qgate._fill = function (G, lst, s)
  local txt, res = G.txt, nil
  for i = G.n, 1, -1 do
    local gi = lst[i]
    if gi then
      txt[i] = string.format('%s %s', txt[i], s)
    else
      txt[i] = string.format('%s -', txt[i])
      gi = qgate._I22
    end
    res = res and res:kron(gi) or gi
  end
  G.mat = G.mat and (G.mat * res) or res
end


--- Generate function that works with basic gates.
--  @param M Gate matrix 2x2.
--  @param s Symbol to print.
--  @return function f(G,...)
qgate._makeGate = function (M, s)
  return function (G, ...)
    local ind, g = {...}, {}
    for _, i in ipairs(ind) do g[i+1] = M end
    if #ind == 0 then 
      for i = 1, G.n do g[i] = M end  
    end
    qgate._fill(G, g, s)
    return G
  end
end


--- Generate matrix based on rule.
--  @param G Gate system.
--  @param rule Funciton that update list of bits (booleans).
--  @param ... Additional parameters for the rule.
--  @return matrix for the given rule.
qgate._matrixFor = function (G, rule, ...)
  local bits, n = {}, G.n
  local nmax = 2^n
  local mat = Matrix:zeros(nmax, nmax)
  for k = 0, nmax-1 do
    -- to 'bits'
    local v, rst = k, 0
    for i = 1, n do
      v, rst = math.modf(v / 2.0)
      bits[i] = (rst > 0.1)  -- use true/false
    end
    -- apply rule
    rule(bits, ...)
    -- to column
    v = 1  -- reuse
    for i, b in ipairs(bits) do
      if b then v = v + 2^(i-1) end
    end
    mat[v][k+1] = 1
  end
  return mat
end


--- Prepare empty gate object.
--  @param n Number of qubits.
--  @return gate object.
qgate._new = function (n)
  local txt = {}
  for i = 1, n do txt[i] = '' end  -- inverted order
  return setmetatable({n=n, mat=nil, txt=txt}, qgate)
end


--- Add gate CNOT.
--  @param G Gate system.
--  @param slave_i Index to modify.
--  @param master_i Index to check.
--  @return updated object.
qgate.CNOT = function (G, slave_i, master_i)
  slave_i, master_i = slave_i + 1, master_i + 1
  local mat = G:_matrixFor(ruleCnot, slave_i, master_i)
  G.mat = G.mat and (G.mat * mat) or mat
  -- update view
  local txt = G.txt
  for i = 1, #txt do
    local s = '-'
    if i == slave_i then 
      s = 'X'
    elseif i == master_i then
      s = 'o'
    end
    txt[i] = string.format('%s %s', txt[i], s)
  end
  return G
end


--- Add gate H.
--  @param G Gate system.
--  @param ... Qubit indices to add gate.
--  @return updated object.
qgate.H = function (G, ...)
  local g, ind = {}, {...}
  for _, i in ipairs(ind) do g[i+1] = qgate._H22 end
  local p = #ind
  if p == 0 then
    for i = 1, G.n do g[i] = qgate._H22 end
    p = G.n
  end
  qgate._fill(G, g, 'H')
  G.mat = G.mat * math.pow(2, -0.5*p)
  return G
end


--- Inverse gate system.
--  @param G Initial system.
--  @return inverted version.
qgate.inverse = function (G)
  local res = qgate._new(G.n)
  res.mat = G.mat:H()
  for i = 1, G.n do
    res.txt[i] = string.reverse(G.txt[i])
  end
  return res
end


--- Swap qubits.
--  @param G Gate system.
--  @param i1 First index.
--  @param i2 Second index.
--  @return updated object.
qgate.SWAP = function (G, i1, i2)
  i1, i2 = i1 + 1, i2 + 1
  local mat = G:_matrixFor(ruleSwap, i1, i2)
  G.mat = G.mat and (G.mat * mat) or mat
  local txt = G.txt
  if i1 > i2 then
    i1, i2 = i2, i1
  end
  for i = 1, #txt do
    local s = '-'
    if i == i1 then 
      s = '^'
    elseif i == i2 then
      s = 'v'
    end
    txt[i] = string.format('%s %s', txt[i], s)
  end
  return G
end


--- Add gate X.
--  @param G Gate system.
--  @param ... Qubit indices to add gate.
--  @return updated object.
qgate.X = qgate._makeGate(qgate._X22, 'X')

-- Comment to remove descriptions
qubit.about = about

return qubit

--======================================
-- https://en.wikipedia.org/wiki/Quantum_logic_gate 
