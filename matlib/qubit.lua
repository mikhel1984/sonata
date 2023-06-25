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

-- define state
a = 0.2*Qb'|0>' + 0.98*Qb'|1>'
-- get number of qubits in system
ans = #a                      --> 1

-- get equal vector
av = a:matrix()
ans = av(1)                  --2> 0.2

-- norm
ans = a * a                  --2> 1.0

-- projection to |+>
k = 1 / math.sqrt(2)
plus = k*Qb'|0>' + k*Qb'|1>'
ans = plus * a               --2> (0.2+0.98)*k

-- system of qubits
-- allow to skip |>
b = Qb'00' + Qb'11'
b:normalize()
-- probability of the state |00>
ans = b:prob '00'            --2> 0.5

-- combine qubits
-- (same as a..b)
c = Qb:combine(a, b)
print(c)

-- do 'measurement'
-- result is calculated using probabilities
print(c:meas())

-- define Hadamard gate for 2 qubits
g1 = Qb:gates(2):H()
ans = #g1                     --> 2

-- show
print(g1)

-- check if it is unary
ans = g1:isUnitary()         --> true

-- as matrix
g1m = g1:matrix()
ans = g1m[1][1]              --2> 0.5

-- apply to qubits
d = g1(Qb'|00>')
-- check projection
ans = (plus..plus) * d       --2> 1.0

-- add X to 0-th, Y to 1-st and Z to both
-- indexation from zero
g1:X(0):Y(1):Z()
print(g1)

-- 3 qubit system
-- CNOT on 0 (2 for control), then swap 1 and 2
g2 = Qb:gates(3):CNOT(0,2):SWAP(1,2)
print(g2)

-- inverse system
g3 = g2:inverse()
mm = g2:matrix() * g3:matrix()
ans = mm[4][4]               --2> 1.0

--]]

--	LOCAL

local Ver = require('matlib.utils')
local Cross = Ver.cross
local Utils = Ver.utils
Ver = Ver.versions

local Matrix = require('matlib.matrix')
local Complex = require('matlib.complex')

local GATES = 'gates'


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


--- Find square value.
--  @param v Real or complex number.
--  @return v^2
local function square (v)
  return (getmetatable(v) == Complex) and (v:re()^2 + v:im()^2) or (v*v)
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
  if Q1.n ~= Q2.n then error("Different size") end
  return qubit._new(Q1.vec + Q2.vec, Q1.n)
end


--- Q1 .. Q2
--  @param Q1 First state.
--  @param Q2 Second state.
--  @return Combination of states.
qubit.__concat = function(Q1, Q2) return qubit.combine(nil, Q1, Q2) end


--- k * Q
--  @param C Coefficient.
--  @param Q State.
--  @return Scaled state.
qubit.__mul = function (C, Q)
  if isqubit(C) then
    if isqubit(Q) then
      if C.n ~= Q.n then error('Different size') end
      return C.vec:H() * Q.vec
    else
      return qubit.__mul(Q, C)
    end
  end
  return qubit._new(C*Q.vec, Q.n)
end


--- Text representation of qubits.
--  @param Q State.
--  @return string with description.
qubit.__tostring = function (Q)
  local bits, n = {}, Q.n
  for i = 1, n do bits[i] = 0 end
  local acc, vs = {}, {}
  for k = 0, Q.vec:rows()-1 do
    local vk = Q.vec[k+1][1]
    if not Cross.isZero(vk) then
      local v, rst = k, 0
      for i = n, 1, -1 do
        v, rst = math.modf(v * 0.5)
        bits[i] = (rst > 0.1) and '1' or '0'
      end
      vs[#vs+1] = (type(vk) == 'number') and Utils.numstr(vk) 
        or tostring(vk)
      acc[#acc+1] = table.concat(bits)
    end
  end
  -- exact value
  if #acc == 1 then return string.format('|%s>', acc[1]) end
  -- general
  for i, v in ipairs(vs) do
    acc[i] = string.format('(%s)|%s>', v, acc[i])
  end
  return table.concat(acc, ' + ')
end


--- Initialize new qubit system.
--  @param v State vector.
--  @param tp Type of basis.
--  @param n Number of qubits.
--  @return new object.
qubit._new = function (v, n)
  return setmetatable({vec=v, n=n}, qubit)
end


--- Get state from the string representation.
--  @param state String of the form '|xxx>'.
--  @return vector, base type, size
qubit._parse = function (state)
  local s = string.match(state, '^|(.+)>$')
  if not s then s = state end  -- try string directly
  local n, sum = 0, 1
  for w in string.gmatch(s, ".") do
    if w == '1' then
      sum = sum + 2^n
    elseif w == '0' then
      -- skip
    else
      error('Expected 0 or 1')
    end
    n = n + 1
  end
  local res = Matrix:zeros(2^n, 1)
  res[sum][1] = 1
  return res, n
end


--- Combine qubits into the system.
--  @param ... Sequence of subsystems.
--  @return combined state.
qubit.combine = function (self, ...)
  local qs = {...}
  local q1 = qs[1]
  if #qs <= 1 then return q1 end
  local v, n = q1.vec, q1.n
  for i = 2, #qs do
    local qi = qs[i]
    v = v:kron(qi.vec)
    n = n + qi.n
  end
  return qubit._new(v, n)
end
about[qubit.combine] = {":combine([Q1,Q2,..]) --> Q|nil", "Make a system of qubits. Same as Q1..Q2."}


--- Getting copy.
--  @return Copy of the object.
qubit.copy = function (Q)
  return qubit._new(Q.vec * 1, Q.n)
end
about[qubit.copy] = {"Q:copy() --> cpy_Q", "Create a copy of the object."}


--- Get corresponding vector.
--  @param Q Qubit system.
--  @return state vector.
qubit.matrix = function (Q)
  return Q.vec:copy()
end
about[qubit.matrix] = {"Q:matrix() --> M", "Get matrix representation."}


--- 'Measure' the qubit system state.
--  @param Q Qubit system.
--  @return found state.
qubit.meas = function (Q)
  local acc = {}
  for i = 1, Q.vec:rows() do
    local v = Q.vec[i][1]
    if not Cross.isZero(v) then
      local sum = (#acc > 0 and acc[#acc][1] or 0) + square(v)
      acc[#acc+1] = {sum, i}
    end
  end
  local _, pair = Utils.binsearch(acc, acc[#acc][1]*math.random(),
    function (t) return t[1] end)
  local vec = Matrix:zeros(Q.vec)
  vec[ pair[2] ][1] = 1
  return qubit._new(vec, Q.n)
end
about[qubit.meas] = {"Q:meas() --> state_Q", "Qubit state measurement."}


--- Normalize coefficients to unit vector.
--  @param Q Qubit system.
qubit.normalize = function (Q)
  local s = 0
  local vec = Q.vec
  for i = 1, vec:rows() do
    s = s + square(vec[i][1])
  end
  s = math.sqrt(s)
  for i = 1, vec:rows() do
    vec[i][1] = vec[i][1] / s
  end
end
about[qubit.normalize] = {"Q:normalize()", "Make norm equal to 1."}


--- Get probability of the given state.
--  @param Q Qubit system.
--  @param state_s - String with the state description.
--  @return Probability.
qubit.prob = function (Q, state_s)
  local v, n = qubit._parse(state_s)
  if n ~= Q.n then error('Different size') end
  for i = 1, v:rows() do
    if v[i][1] > 0.9 then  -- equal to 1
      return square(Q.vec[i][1])
    end
  end
  return 0
end
about[qubit.prob] = {"Q:prob(state_s) --> probatility_d", "Get probability for the given state."}


--- Get number of qubits.
--  @param Q Qubit object.
--  @return number of qubits in the system.
qubit.size = function (Q) return Q.n end
qubit.__len = qubit.size


-- constructor call
setmetatable(qubit, {
__call = function (self, state) 
  return qubit._new(qubit._parse(state))
end
})
about[qubit] = {" (t) --> Q", "Create new qubit.", help.NEW}


--	QGATE

local qgate = {
  type = 'qgate', isqgate = true,
}
qgate.__index = qgate


--- Initialize gate sequence.
--  @param n Number of inputs.
qubit.gates = function (self, n)
  if n <= 0 or not Ver.isInteger(n) then error('Wrong input number') end
  return qgate._new(n)
end
about[qubit.gates] = {":gates(input_n) --> G", "Initialize gates for the given numer of inputs.", GATES}


qgate.__call = function (G, Q)
  if G.n ~= Q.n then error('Different number of qubits') end
  return qubit._new(G.mat * Q.vec, Q.n)
end


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
    if getmetatable(G) ~= qgate then error('Not a gate object') end
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
  if getmetatable(G) ~= qgate then error('Not a gate object') end
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
qubit.CNOT = qgate.CNOT
about[qubit.CNOT] = {"G:CNOT(slave_i, master_i) --> upd_G", "Add CNOT gate.", GATES}


--- Add gate H.
--  @param G Gate system.
--  @param ... Qubit indices to add gate.
--  @return updated object.
qgate.H = function (G, ...)
  if getmetatable(G) ~= qgate then error('Not a gate object') end
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
qubit.H = qgate.H
about[qubit.H] = {"G:H([i1,i2,..]) --> upd_G", "Add Hadamard gate.", GATES}


--- Inverse gate system.
--  @param G Initial system.
--  @return inverted version.
qgate.inverse = function (G)
  if getmetatable(G) ~= qgate then error('Not a gate object') end
  local res = qgate._new(G.n)
  res.mat = G.mat:H()
  for i = 1, G.n do
    res.txt[i] = string.reverse(G.txt[i])
  end
  return res
end
qubit.inverse = qgate.inverse
about[qubit.inverse] = {"G:inverse() --> inv_G", "Get inverted gate sequence", GATES}


--- Check gate.
--  @param G Gate system.
--  @return true if the matrix is unitary.
qgate.isUnitary = function (G)
  if not G.mat then return false end
  local U = G.mat * G.mat:H()
  for i = 1, U:rows() do
    U[i][i] = U[i][i] - 1
  end
  return U:norm() < 1E-6
end
qubit.isUnitary = qgate.isUnitary
about[qubit.isUnitary] = {"G:isUnitary() --> bool", 
  "Check if the matrix is unitary", GATES}


--- Get matrix representation.
--  @param G Gate system.
--  @return corresponding matrix.
qgate.matrix = function (G)
  return G.mat:copy()
end


--- Get number of inputs/outputs
--  @param Gate system.
--  @return input number
qgate.size = function (G) return G.n end
qgate.__len = qgate.size


--- Swap qubits.
--  @param G Gate system.
--  @param i1 First index.
--  @param i2 Second index.
--  @return updated object.
qgate.SWAP = function (G, i1, i2)
  if getmetatable(G) ~= qgate then error('Not a gate object') end
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
      s = '/'
    elseif i == i2 then
      s = '\\'
    end
    txt[i] = string.format('%s %s', txt[i], s)
  end
  return G
end
qubit.SWAP = qgate.SWAP
about[qubit.SWAP] = {"G:SWAP(ind1, ind2) --> upd_G", "Add gate to swap 2 qubits.", GATES}


--- Add gate S.
--  @param G Gate system.
--  @param ... Qubit indices to add gate.
--  @return updated object.
qgate.S = qgate._makeGate(qgate._S22, 'S')
qubit.S = qgate.S
about[qubit.S] = {"G:S([ind1,ind2,..]) --> upd_G", "Add S gate.", GATES}


--- Add gate T.
--  @param G Gate system.
--  @param ... Qubit indices to add gate.
--  @return updated object.
qgate.T = qgate._makeGate(qgate._T22, 'T')
qubit.T = qgate.T
about[qubit.T] = {"G:T([ind1,ind2,..]) --> upd_G", "Add T gate.", GATES}


--- Add gate X.
--  @param G Gate system.
--  @param ... Qubit indices to add gate.
--  @return updated object.
qgate.X = qgate._makeGate(qgate._X22, 'X')
qgate.NOT = qgate.X
qubit.X = qgate.X
about[qubit.X] = {"G:X([ind1,ind2,..]) --> upd_G", "Add X gate.", GATES}


--- Add gate Y.
--  @param G Gate system.
--  @param ... Qubit indices to add gate.
--  @return updated object.
qgate.Y = qgate._makeGate(qgate._Y22, 'Y')
qubit.Y = qgate.Y
about[qubit.Y] = {"G:Y([ind1,ind2,..]) --> upd_G", "Add Y gate.", GATES}


--- Add gate Z.
--  @param G Gate system.
--  @param ... Qubit indices to add gate.
--  @return updated object.
qgate.Z = qgate._makeGate(qgate._Z22, 'Z')
qubit.Z = qgate.Z
about[qubit.Z] = {"G:Z([ind1,ind2,..]) --> upd_G", "Add Z gate.", GATES}


-- Comment to remove descriptions
qubit.about = about

return qubit

--======================================
-- https://en.wikipedia.org/wiki/Quantum_logic_gate 
-- TODO horizontal concatenation for gates using *, vertical with ..
-- TODO gate from truth table (get truth table from gate as well?)
-- TODO qubit equality
