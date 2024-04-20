--[[		sonata/matlib/qubit.lua

--- Quantum computing simulation.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2023.

	module 'qubit'
--]]

---------------- Tests --------------
--[[TEST_IT

-- use 'qubit'
Qb = require 'matlib.qubit'
-- for oracle definition
Mat = require 'matlib.matrix'

-- define state
a = 0.2*Qb'|0>' + 0.98*Qb'|1>'
-- get number of qubits in system
ans = #a                      -->  1

-- qubit as vector
av = a:matrix()
ans = av(1)                  --2>  0.2

-- norm
ans = a * a                  --2>  1.0

-- projection to |+>
k = 1 / math.sqrt(2)
plus = k*Qb'|0>' + k*Qb'|1>'
ans = plus * a               --2>  (0.2+0.98)*k

-- system of qubits
-- allow to skip |>
b = Qb'00' + Qb'11'
b:normalize()
-- probability of the state |00>
ans = b:prob '00'            --2>  0.5

-- combine qubits
-- (same as a..b)
c = Qb:combine(a, b)
print(c)

-- do 'measurement' of all states
-- result is calculated using probabilities
print(c:meas())

-- define Hadamard gate for 2 qubits
g1 = Qb:gates(2):H()
ans = #g1                     -->  2

-- show
print(g1)

-- check if it is unary
ans = g1:isUnitary()          -->  true

-- as matrix
g1m = g1:matrix()
ans = g1m[1][1]              --2>  0.5

-- apply to qubits
d = g1(Qb'|00>')
-- check projection
ans = (plus..plus) * d       --2>  1.0

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
ans = mm[4][4]               --2>  1.0

-- Deutsch-Jozsa algorithm
-- check for function f(x) = x, 1 is expected as output
g4 = Qb:gates(2):H()
-- oracle from table
g4:fromTable({
--  in   out
  {'00','00'},
  {'01','01'},
  {'10','11'},
  {'11','10'}
}):H()
-- apply for 01, measure at index 1
ans = g4(Qb'01'):meas(1)      -->  Qb'1'

-- Grover's algorithm
-- check funciton f(101) = 1, f(x) = 0 for others
g5 = Qb:gates(3):H()
-- oracle from matrix
mat_fn = Mat:eye(8); mat_fn[6][6] = -1
-- for the Groover diffusion gate (2|0><0| - 1)
mat_df = -Mat:eye(8); mat_df[1][1] = 1
-- combine
oracle_diffuse = Qb:gates(3)
  :fromMatrix(mat_fn)
  :H()
  :fromMatrix(mat_df)
  :H()
-- apply 2 times (~ sqrt(8)*pi/4)
g5 = g5 * oracle_diffuse^2
-- check result (it is random)
ans = g5(Qb'000'):meas()      --> Qb'101'

--]]

--	LOCAL

local Vinteger, Czero, Unumstr, Ubinsearch do
  local lib = require('matlib.utils')
  Vinteger = lib.versions.isInteger
  Czero = lib.cross.isZero
  Unumstr = lib.utils.numstr
  Ubinsearch = lib.utils.binsearch
end

-- dependencies
local Matrix = require('matlib.matrix')
local Complex = require('matlib.complex')

-- categories
local GATES = 'gates'

-- errors
local ERR_WRONG_SIZE = 'Different size'
local ERR_NOT_A_GATE = 'Not a gate object'


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

local qubit = { type = 'qubit' }
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
  if not (isqubit(Q1) and isqubit(Q2)) then 
    error "Unexpected operation" 
  end
  assert(Q1._n == Q2._n, ERR_WRONG_SIZE)
  return qubit._new(Q1._vec + Q2._vec, Q1._n)
end


--- Q1 .. Q2
--  @param Q1 First state.
--  @param Q2 Second state.
--  @return Combination of states.
qubit.__concat = function(Q1, Q2) return qubit.combine(nil, Q1, Q2) end


--- Q1 == Q2
--  @param Q1 First state.
--  @param Q2 Second state.
--  @return true on equal states.
qubit.__eq = function (Q1, Q2)
  return Q1._n == Q2._n and (Q1._vec - Q2._vec):norm() < 1E-6
end


--- k * Q
--  @param C Coefficient.
--  @param Q State.
--  @return Scaled state.
qubit.__mul = function (C, Q)
  if isqubit(C) then
    if isqubit(Q) then
      assert(C._n == Q._n, ERR_WRONG_SIZE)
      return C._vec:H() * Q._vec
    else
      return qubit.__mul(Q, C)
    end
  end
  return qubit._new(C*Q._vec, Q._n)
end


--- Q1 - Q2
--  @param Q1 First state.
--  @param Q2 Second state.
--  @return Combined state.
qubit.__sub = function (Q1, Q2)
  if not (isqubit(Q1) and isqubit(Q2)) then 
    error "Unexpected operation" 
  end
  assert(Q1._n == Q2._n, ERR_WRONG_SIZE)
  return qubit._new(Q1._vec - Q2._vec, Q1._n)
end


--- Text representation of qubits.
--  @return string with description.
qubit.__tostring = function (self)
  local bits, n = {}, self._n
  for i = 1, n do bits[i] = 0 end
  local acc, vs = {}, {}
  for k = 0, self._vec:rows()-1 do
    local vk = self._vec[k+1][1]
    if not Czero(vk) then
      local v, rst = k, 0
      for i = n, 1, -1 do
        v, rst = math.modf(v * 0.5)
        bits[i] = (rst > 0.1) and '1' or '0'
      end
      vs[#vs+1] = (type(vk) == 'number') and Unumstr(vk) or tostring(vk)
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


-- Metamethods
about['_ar'] = {"arithmetic: Q1+Q2, Q1-Q2, k*Q, G1*G2, G^n, G()", nil, help.META}
about['_cmp'] = {"comparison: Q1==Q2, Q1~=Q2", nil, help.META}
about['_obj'] = {"object: Q1..Q2, #Q, #G", nil, help.META}


--- Measure on qubit from the system.
--  Update system state.
--  @param pos Qubit index.
--  @return Qubit state.
qubit._measOne = function (self, pos)
  local sum0, sum1 = 0, 0
  local ones = {}
  for k = 0, self._vec:rows()-1 do
    local v, rst = k, 0
    for j = 0, pos do
      v, rst = math.modf(v * 0.5)  -- TODO use shift
    end
    if rst > 0.1 then
      sum1 = sum1 + self._vec[k+1][1]
      ones[#ones+1] = true
    else
      sum0 = sum0 + self._vec[k+1][1]
      ones[#ones+1] = false
    end
  end
  local isOne = (math.random()*(sum0+sum1) > sum0)
  for i, v in ipairs(ones) do
    if v ~= isOne then self._vec[i][1] = 0 end
  end
  qubit.normalize(self)
  return qubit._new(isOne and Matrix:V{0, 1} or Matrix:V{1, 0}, 1)
end


--- Initialize new qubit system.
--  @param v State vector.
--  @param tp Type of basis.
--  @param n Number of qubits.
--  @return new object.
qubit._new = function (v, n)
  return setmetatable({_vec=v, _n=n}, qubit)
end


--- System in random state.
--  @param n Number of qubits.
qubit._rand = function (n)
  local v = Matrix:zeros(2^n, 1)
  for i = 1, v:rows() do
    v[i][1] = Complex(2*math.random()-1, 2*math.random()-1)
  end
  v:vec():normalize()
  return qubit._new(v, n)
end


--- Get state from the string representation.
--  @param state String of the form '|xxx>'.
--  @return vector, size, index of 1
qubit._parse = function (state)
  local s = string.match(state, '^|(.+)>$')
  s = string.reverse(s or state)
  local n, sum = 0, 1
  for w in string.gmatch(s, ".") do
    if w == '1' then
      sum = sum + 2^n
    elseif w == '0' then
      -- skip
    else
      error 'Expected 0 or 1'
    end
    n = n + 1
  end
  local res = Matrix:zeros(2^n, 1)
  res[sum][1] = 1
  return res, n, sum
end


--- Combine qubits into the system.
--  @param ... Sequence of subsystems.
--  @return combined state.
qubit.combine = function (_, ...)
  local qs = {...}
  local q1 = qs[1]
  if #qs <= 1 then return q1 end
  local v, n = q1._vec, q1._n
  for i = 2, #qs do
    local qi = qs[i]
    v = v:kron(qi._vec)
    n = n + qi._n
  end
  return qubit._new(v, n)
end
about[qubit.combine] = {":combine([Q1,Q2,..]) --> Q|nil", "Make a system of qubits. Same as Q1..Q2."}


--- Getting copy.
--  @return Copy of the object.
qubit.copy = function (self)
  return qubit._new(self._vec:copy(), self._n)
end
about[qubit.copy] = {"Q:copy() --> cpy_Q", "Create a copy of the object."}


--- Get corresponding vector.
--  @return state vector.
qubit.matrix = function (self) return self._vec:copy() end
about[qubit.matrix] = {"Q:matrix() --> M", "Get matrix representation."}


--- 'Measure' the qubit system state. Update current object.
--  @param ind Qubit to measure.
--  @return found state.
qubit.meas = function (self, ind)
  if ind then return qubit._measOne(self, ind) end
  local acc = {}
  for i = 1, self._vec:rows() do
    local v = self._vec[i][1]
    if not Czero(v) then
      local sum = (#acc > 0 and acc[#acc][1] or 0) + square(v)
      acc[#acc+1] = {sum, i}
    end
  end
  local j = Ubinsearch(acc, acc[#acc][1]*math.random(),
    function (t) return t[1] end)
  j = acc[j][2]   -- reuse
  for i = 1, self._vec:rows() do
    self._vec[i][1] = (i == j) and 1 or 0
  end
  return self
end
about[qubit.meas] = {"Q:meas([index]) --> Q", "Qubit state measurement."}


--- Normalize coefficients to unit vector.
qubit.normalize = function (self) self._vec:vec():normalize() end
about[qubit.normalize] = {"Q:normalize()", "Make norm equal to 1."}


--- Get probability of the given state.
--  @param state_s - String with the state description.
--  @return Probability.
qubit.prob = function (self, state_s)
  local v, n = qubit._parse(state_s)
  assert(n == self._n, ERR_WRONG_SIZE)
  for i = 1, v:rows() do
    if v[i][1] > 0.9 then  -- equal to 1
      return square(self._vec[i][1])
    end
  end
  return 0
end
about[qubit.prob] = {"Q:prob(state_s) --> probatility_d", 
  "Get probability for the given state."}


--- Get number of qubits.
--  @return number of qubits in the system.
qubit.size = function (self) return self._n end
qubit.__len = qubit.size


-- constructor call
setmetatable(qubit, {
__call = function (self, state)
  if type(state) == 'number' then
    assert(Vinteger(state) and state > 0, 'Wrong size')
    return qubit._rand(state)
  end
  return qubit._new(qubit._parse(state))
end
})
about[qubit] = {" (state_s|num) --> Q", "Create new qubit.", help.NEW}


--	QGATE

local qgate = { type = 'qgate' }
qgate.__index = qgate


--- Initialize gate sequence.
--  @param n Number of inputs.
qubit.gates = function (_, n)
  if n <= 0 or not Vinteger(n) then 
    error 'Wrong input number'
  end
  return qgate._new(n)
end
about[qubit.gates] = {":gates(input_n) --> G", 
  "Initialize gates for the given numer of inputs.", GATES}


--- Apply transformation to qubit system.
--  @param G Gate system.
--  @parma Q Qubit system.
qgate.__call = function (G, Q)
  if G._n ~= Q._n then 
    error 'Different number of qubits'
  end
  return qubit._new(G._mat * Q._vec, Q._n)
end


--- Horizontal concatenation.
--  @param G1 Left gate system.
--  @param G2 Right gate system.
--  @return concatenated gates.
qgate.__mul = function (G1, G2)
  if getmetatable(G1) ~= qgate or getmetatable(G2) ~= qgate then
    error 'Undefined'
  end
  assert(G1._n == G2._n, ERR_WRONG_SIZE)
  local res = qgate._new(G1._n)
  if not G1._mat then
    res._mat, res._txt = G2._mat, G2._txt
  elseif not G2._mat then
    res._mat, res._txt = G1._mat, G1._txt
  else
    res._mat = G2._mat * G1._mat
    for i = 1, #res._txt do
      res._txt[i] = G1._txt[i]..G2._txt[i]
    end
  end
  return res
end


--- Q ^ n
--  @parma n Integer power.
--  @return n times concatenated gates.
qgate.__pow = function (self, n)
  if not Vinteger(n) then 
    error 'Integer is expected'
  end
  if n < -1 then 
    error "Wrong power"
  end
  if not self._mat and n ~= 0 then return qgate.__pow(self, 0) end
  if n == -1 and self._mat then return qgate.inverse(self) end
  local res = qgate._new(self._n)
  local txt = res._txt
  if n > 0 then
    res._mat = self._mat^n
    for i = 1, #txt do txt[i] = string.rep(self._txt[i], n) end
  else
    res._mat = Matrix:eye(2^self._n)
    for i = 1, #txt do txt[i] = ' -' end
  end
  return res
end


--- Print gates.
--  @return string representation.
qgate.__tostring = function (self)
  local acc = {}
  for i = self._n, 1, -1 do
    acc[#acc+1] = string.format('|x%d> %s', i-1, self._txt[i])
  end
  return table.concat(acc, '\n')
end


-- Basic matrices.
qgate._I22 = Matrix:eye(2)
qgate._X22 = Matrix{{0, 1}, {1, 0}}
qgate._Y22 = Matrix{{0, Complex:i(-1)}, {Complex:i(1), 0}}
qgate._Z22 = Matrix{{1, 0}, {0, -1}}
qgate._S22 = Matrix{{1, 0}, {0, Complex:i(1)}}
qgate._T22 = Matrix{{1, 0}, {0, Complex:cis(1, math.pi/4)}}
qgate._H22 = Matrix{{1, 1}, {1, -1}}  -- multipy to 1/sqrt(2)


--- Update matrix and representation for basic components.
--  @param lst List of gates (nil for I).
--  @param s Symbol to print.
qgate._fill = function (self, lst, s)
  local txt, res = self._txt, nil
  for i = self._n, 1, -1 do
    local gi = lst[i]
    if gi then
      txt[i] = string.format('%s %s', txt[i], s)
    else
      txt[i] = string.format('%s -', txt[i])
      gi = qgate._I22
    end
    res = res and res:kron(gi) or gi
  end
  self._mat = self._mat and (res * self._mat) or res
end


--- Generate function that works with basic gates.
--  @param M Gate matrix 2x2.
--  @param s Symbol to print.
--  @return function f(G,...)
qgate._makeGate = function (M, s)
  return function (G, ...)
    assert(getmetatable(G) == qgate, ERR_NOT_A_GATE)
    local ind, g = {...}, {}
    for _, i in ipairs(ind) do g[i+1] = M end
    if #ind == 0 then
      for i = 1, G._n do g[i] = M end
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
  local bits, n = {}, G._n
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
  return setmetatable({_n=n, _mat=nil, _txt=txt}, qgate)
end


--- Add gate CNOT.
--  @param slave_i Index to modify.
--  @param master_i Index to check.
--  @return updated object.
qgate.CNOT = function (self, slave_i, master_i)
  assert(getmetatable(self) == qgate, ERR_NOT_A_GATE)
  slave_i, master_i = slave_i + 1, master_i + 1
  local mat = self:_matrixFor(ruleCnot, slave_i, master_i)
  self._mat = self._mat and (mat * self._mat) or mat
  -- update view
  local txt = self._txt
  local sym = {[master_i]='o', [slave_i]='X'}
  for i = 1, #txt do
    txt[i] = string.format('%s %s', txt[i], sym[i] or '-')
  end
  return self
end
qubit.CNOT = qgate.CNOT
about[qubit.CNOT] = {"G:CNOT(slave_i, master_i) --> upd_G", "Add CNOT gate.", GATES}


--- Transform matrix to gate object.
--  @param m Matrix or the table of lists.
--  @return Updated gate object.
qgate.fromMatrix = function (self, m)
  assert(getmetatable(self) == qgate, ERR_NOT_A_GATE)
  m = (getmetatable(m) == Matrix) and m or Matrix(m)
  local n = 2^self._n
  if m:rows() ~= m:cols() or m:rows() ~= n then
    error(string.format('Expected %dx%d matrix', n, n))
  end
  local tmp = qgate._new(self._n)
  tmp._mat = m
  if not qgate.isUnitary(tmp) then
    -- TODO to warning
    error "Unitary matrix is expected"
  end
  local txt = self._txt
  for i = 1, #txt do
    txt[i] = string.format('%s U', txt[i])
  end
  self._mat = self._mat and (m * self._mat) or m
  return self
end
qubit.fromMatrix = qgate.fromMatrix
about[qubit.fromMatrix] = {"G:fromMatrix(mat) --> upd_G",
  "Make gate from matrix.", GATES}


--- Transform truth table to gate object.
--  @param t Table of string pairs.
--  @return Updated gate object.
qgate.fromTable = function (self, t)
  assert(getmetatable(self) == qgate, ERR_NOT_A_GATE)
  local acc, n = {}, 2^self._n
  local mat = Matrix:zeros(n, n)
  for _, p in ipairs(t) do
    local _, n1, ind1 = qubit._parse(p[1])
    assert(n1 == self._n, ERR_WRONG_SIZE)
    if acc[ind1] then 
      error(string.format('State %s is not unique', p[1])) 
    end
    local _, n2, ind2 = qubit._parse(p[2])
    assert(n2 == self._n, ERR_WRONG_SIZE)
    mat[ind2][ind1] = 1
    acc[ind1] = true
  end
  for i = 1, n do
    if not acc[i] then
      error('Unknown column '..tostring(i))
    end
  end
  return qgate.fromMatrix(self, mat)
end
qubit.fromTable = qgate.fromTable
about[qubit.fromTable] = {"G:fromTable(truth_t) --> upd_G",
  "Make gate from truth table.", GATES}


--- Add gate H.
--  @param ... Qubit indices to add gate.
--  @return updated object.
qgate.H = function (self, ...)
  assert(getmetatable(self) == qgate, ERR_NOT_A_GATE)
  local g, ind = {}, {...}
  for _, i in ipairs(ind) do g[i+1] = qgate._H22 end
  local p = #ind
  if p == 0 then
    for i = 1, self._n do g[i] = qgate._H22 end
    p = self._n
  end
  qgate._fill(self, g, 'H')
  self._mat = self._mat * 2^(-0.5*p)
  return self
end
qubit.H = qgate.H
about[qubit.H] = {"G:H([i1,i2,..]) --> upd_G", "Add Hadamard gate.", GATES}


--- Inverse gate system.
--  @return inverted version.
qgate.inverse = function (self)
  assert(getmetatable(self) == qgate, ERR_NOT_A_GATE)
  local res = qgate._new(self._n)
  res._mat = self._mat:H():copy()
  for i = 1, self._n do
    res._txt[i] = string.reverse(self._txt[i])
  end
  return res
end
qubit.inverse = qgate.inverse
about[qubit.inverse] = {"G:inverse() --> inv_G", "Get inverted gate sequence", GATES}


--- Check gate.
--  @param G Gate system.
--  @return true if the matrix is unitary.
qgate.isUnitary = function (G)
  if not G._mat then return false end
  local U = G._mat * G._mat:H()
  for i = 1, U:rows() do U[i][i] = U[i][i] - 1 end
  return U:norm() < 1E-6
end
qubit.isUnitary = qgate.isUnitary
about[qubit.isUnitary] = {"G:isUnitary() --> bool",
  "Check if the matrix is unitary", GATES}


--- Get matrix representation.
--  @param G Gate system.
--  @return corresponding matrix.
qgate.matrix = function (G) return G._mat:copy() end


--- Get number of inputs/outputs
--  @return input number
qgate.size = function (self) return self._n end
qgate.__len = qgate.size


--- Swap qubits.
--  @param i1 First index.
--  @param i2 Second index.
--  @return updated object.
qgate.SWAP = function (self, i1, i2)
  assert(getmetatable(self) == qgate, ERR_NOT_A_GATE)
  i1, i2 = i1 + 1, i2 + 1
  local mat = self:_matrixFor(ruleSwap, i1, i2)
  self._mat = self._mat and (mat * self._mat) or mat
  if i1 > i2 then
    i1, i2 = i2, i1
  end
  local txt = self._txt
  local sym = {[i1]='/', [i2]='\\'}
  for i = 1, #txt do
    txt[i] = string.format('%s %s', txt[i], sym[i] or '-')
  end
  return self
end
qubit.SWAP = qgate.SWAP
about[qubit.SWAP] = {"G:SWAP(ind1, ind2) --> upd_G", "Add gate to swap 2 qubits.", GATES}


-- Add gate S.
qgate.S = qgate._makeGate(qgate._S22, 'S')
qubit.S = qgate.S
about[qubit.S] = {"G:S([ind1,ind2,..]) --> upd_G", "Add S gate.", GATES}


-- Add gate T.
qgate.T = qgate._makeGate(qgate._T22, 'T')
qubit.T = qgate.T
about[qubit.T] = {"G:T([ind1,ind2,..]) --> upd_G", "Add T gate.", GATES}


-- Add gate X.
qgate.X = qgate._makeGate(qgate._X22, 'X')
qgate.NOT = qgate.X
qubit.X = qgate.X
about[qubit.X] = {"G:X([ind1,ind2,..]) --> upd_G", "Add X gate.", GATES}


-- Add gate Y.
qgate.Y = qgate._makeGate(qgate._Y22, 'Y')
qubit.Y = qgate.Y
about[qubit.Y] = {"G:Y([ind1,ind2,..]) --> upd_G", "Add Y gate.", GATES}


-- Add gate Z.
qgate.Z = qgate._makeGate(qgate._Z22, 'Z')
qubit.Z = qgate.Z
about[qubit.Z] = {"G:Z([ind1,ind2,..]) --> upd_G", "Add Z gate.", GATES}


-- Comment to remove descriptions
qubit.about = about

return qubit

--======================================
-- TODO vertical concatenation with ..
-- TODO qubit from vector
