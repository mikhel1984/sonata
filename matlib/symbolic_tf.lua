--[[		sonata/lib/symbase.lua

--- Basic symbolical operations.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2023-2026.

	module 'symbase'
--]]


--	LOCAL

local _utils = require("matlib.utils")
local _unpack = _utils.versions.unpack
local _move = _utils.versions.move
local _tsort, _tremove, _tconcat = table.sort, table.remove, table.concat
local _tinsert = table.insert


--	MODULE

local parents = {}


local symbolic = {
-- mark
type = "symbolic",
-- main types
_parentList = parents,
}


--- Check object type.
--  @param v Object.
--  @return True if the object is symbolic.
local function _issym(v) return getmetatable(v) == symbolic end


--- Condition for element sorting.
--  @param S1 Symbolic object.
--  @param S2 Symbolic object.
--  @return true when S1 < S2.
local function _compList (S1, S2)
  local v1 = _issym(S1[1]) and S1[1]._sign or 0
  local k1 = _issym(S1[2]) and S1[2]._sign or 0
  local v2 = _issym(S2[1]) and S2[1]._sign or 0
  local k2 = _issym(S2[2]) and S2[2]._sign or 0
  return v1 < v2 or (v1 == v2 and k1 < k2)
end


--- Check equality of 2 values.
--  @param a First symbolic or numeric value.
--  @param b Second symbolic or numeric value.
--  @return true when objects are equal.
local function _eql (a, b)
  if _issym(a) then
    return _issym(b) and a:pEq(b)
  else
    return not _issym(b) and a == b
  end
end


--- Try to combine (simplify) to pairs {k, S}
--  @param a First pair.
--  @param b Second pair.
--  @param tParent Parent reference.
local function _simpPairElements (a, b, tParent)
  local a1, b1 = a[1], b[1]
  if not (_issym(a1) or _issym(b1)) then
    -- c1*c2 + c3*c4  or  c1^c2 * c3^c4
    if tParent == parents.product then
      a[1] = a1*a[2] + b1*b[2]
    else  -- power
      a[1] = a1^a[2] * b1^b[2]
    end
    a[2], b[2] = 1, 0
  elseif _issym(a1) and _issym(b1) and b1:pEq(a1) then
    -- add coefficients
    a[2], b[2] = a[2] + b[2], 0
  end
end


-- Combine 'common' methods
local common = {
  -- Return false always.
  skip = function () return false end,
} -- common


--- Check equality for objects based on lists of pairs.
--  @param S1 First symbolic object.
--  @param S2 Second symbolic object.
--  @return true when objects are equal.
common.eqPairs = function (S1, S2)
  if S1._sign ~= S2._sign or S1._parent ~= S2._parent or #S1._ ~= #S2._ then
    return false
  end
  for i = 1, #S1._ do
    local si1, si2 = S1._[i], S2._[i]
    if not (_eql(si1[1], si2[1]) and _eql(si1[2], si2[2])) then
      return false
    end
  end
  return true
end


--- Evaluate expression for list of pairs.
--  @param S Symbolic object.
--  @param tEnv Table with elements for substitution.
--  @return New object.
common.evalPairs = function (S, tEnv)
  local res = symbolic:_newExpr(S._parent, {})
  for i, v in ipairs(S._) do
    res._[i] = {
      _issym(v[1]) and v[1]:pEval(tEnv) or v[1],
      _issym(v[2]) and v[2]:pEval(tEnv) or v[2]}
  end
  return res
end


common.rawget = function (S, n)
  return (n == nil) and S or nil
end


common.rawgetPair = function (S, n, m, ...)
  if n == nil then return S end
  if n <= #S._ then
    -- whole line
    if m == nil then
      return symbolic:_newExpr(S._parent, {S._[n]})
    end
    -- choose from pair
    if m == 1 then
      return S._[n][1]:pRawGet(...)
    elseif m == 2 then
      return symbolic:_newConst(S._[n][2]):pRawGet(...)
    end
  end
  return nil
end


common.rawsetPair = function (S, v, n, m, q, ...)
  if n and n <= #S._ then
    if m == nil or (q == nil and m == 1 and v._parent == S._parent) then
      -- insert here
      if v._parent == S._parent then
        _tremove(S._, n)
        for _, ln in ipairs(v._) do _tinsert(S._, ln) end
      else
        _tinsert(S._, {v, 1})
      end
    elseif m == 1 then
      -- update symbolic part
      if q == nil then
        S._[n][1] = v
      else
        return S._[n][1]:pRawSet(v, q, ...)
      end
    -- elseif m == 2 and v._parent == parents.const then
    --   S._[n][2] = v._
    else
      return false
    end
    S._sign = nil
  end
  return false
end


--- Find signature of an object based on list of pairs.
--  @param S Symbolic object.
--  @return true when update signature.
common.signaturePairs = function (S)
  -- check elements
  local found = false
  for _, v in ipairs(S._) do
    found = _issym(v[1]) and v[1]:pSignature() or found
    found = _issym(v[2]) and v[2]:pSignature() or found
  end
  if S._sign and not found then return false end
  _tsort(S._, _compList)
  local sum = S.pId
  for _, v in ipairs(S._) do
    if _issym(v[1]) then
      sum = (sum*8 + v[1]._sign) % 1000000
    end
    if _issym(v[2]) then
      sum = (sum*8 + v[2]._sign) % 1000000
    end
  end
  S._sign = sum
  return true
end


--- Simplify list of pairs (in place).
--  @param S Symbolic object.
--  @param tParent Parent reference.
common.simpPair = function (S, tParent)
  -- be sure that signature is found
  S:pSignature()
  -- update coefficients
  if tParent == parents.product then
    for _, v in ipairs(S._) do
      if _issym(v[1]) and v[1]._parent == tParent then
        local k = v[1]:pGetConst()
        if k ~= 1 then
          v[2] = v[2] * k
          v[1]:pSignature()
        end
      end
    end
  end
  -- align levels
  for i = #S._, 1, -1 do
    local si = S._[i]
    if _issym(si[1]) and si[1]._parent == S._parent then
      for _, v in ipairs(si[1]._) do
        _tinsert(S._, {v[1], v[2]*si[2]})
      end
      _tremove(S._, i)
      S._sign = nil
    end
  end
  -- combine elements
  for i, si in ipairs(S._) do
    if si[2] ~= 0 then
      for j = i+1, #S._ do
        local sj = S._[j]
        if sj[2] ~= 0 then
          _simpPairElements(si, sj, tParent)
        end
      end
    end
  end
  -- remove zeros
  for i = #S._, 1, -1 do
    local si = S._[i]
    if not _issym(si[2]) and si[2] == 0 then
      _tremove(S._, i)
      S._sign = nil
    end
  end
end


-- ============ FUNCTION CALL ============

parents.funcValue = {
  -- S._ = {fn, arg1, arg2, ...}
  pId = 2,
  pIsAtom = false,
  pSimp = function (S, bFull)
    for i, v in ipairs(S._) do
      S._[i] = _issym(v) and v:pSimp(bFull) or v
    end
    return S
  end,
}


--- Find signature of a list.
--  @param S Symbolic object.
--  @return true when update signature.
parents.funcValue.pSignature = function (S)
  local found = false
  for _, v in ipairs(S._) do
    if _issym(v) then
      found = v:pSignature() or found
    end
  end
  if S._sign and not found then return false end
  local sum = S.pId
  for _, v in ipairs(S._) do
    if _issym(v) then
      sum = (sum*8 + v._sign) % 1000000
    end
  end
  S._sign = sum
  return true
end


--- Check equality for objects in lists.
--  @param S1 First symbolic object.
--  @param S2 Second symbolic object.
--  @return true when objects are equal.
parents.funcValue.pEq = function (S1, S2)
  if S1._sign ~= S2._sign or S1._parent ~= S2._parent or #S1._ ~= #S2._ then
    return false
  end
  for i = 1, #S1._ do
    if not _eql(S1._[i], S2._[i]) then
      return false
    end
  end
  return true
end


parents.funcValue.pRawGet = function (S, n, ...)
  if n == nil then return S end
  if n <= #S._ then return S._[n]:pRawGet(...) end
  return nil
end


parents.funcValue.pRawSet = function (S, v, n, m, ...)
  if n and n <= #S._ and n > 1 then
    if m == nil then
      S._[n] = v
      S._sign = nil
      return true
    else
      return S._[n]:pRawSet(v, m, ...)
    end
  end
  return false
end


--- Differentiate function.
--  @param S1 Symbol list for function call.
--  @param S2 Variable.
--  @return Derivative.
parents.funcValue.pDiff = function (S1, S2)
  local res = 0
  local args = _move(S1._, 2, #S1._, 1, {})
  local diffs = symbolic._fnDiff[S1._[1]]
  if diffs then
    -- has predefined derivatives
    if #args ~= #diffs then
      error "Wrong arguments number"
    end
    for i, fn in ipairs(diffs) do
      local dx = args[i]:pDiff(S2)
      if dx ~= 0 then res = res + fn(_unpack(args)) * dx end
    end
  else
    -- derivative not defined
    local df = symbolic:_newExpr(
      parents.funcValue, {'diff', symbolic:_newSymbol(S1._[1]), S2})
    for i = 1, #args do
      local dx = args[i]:pDiff(S2)
      if dx ~= 0 then res = res + df * dx end
    end
  end
  return res
end


--- Evaluate function value.
--  @param S Symbolic object.
--  @param tEnv Elements for substitution.
--  @return New object.
parents.funcValue.pEval = function (S, tEnv)
  local t, val = {S._[1]}, {}
  for i = 2, #S._ do
    local v = S._[i]:pEval(tEnv)
    t[i] = v
    if not _issym(v) then
      val[#val+1] = v
    end
  end
  local body = symbolic._fnList[S._[1]]
  if #val + 1 == #t and body then
    -- evaluate
    return body(_unpack(val))
  else
    return symbolic:_newExpr(S._parent, t)
  end
end


--- Function internal struct.
--  @param S Symbolic object.
--  @param n Shift.
--  @return string representation.
parents.funcValue.pInternal = function (S, n)
  local t = {string.format("%sCALL %s", string.rep(" ", n), S._[1])}
  for i = 2, #S._ do
    local v = S._[i]
    t[#t+1] = _issym(v) and v:pInternal(n + 2)
      or string.rep(" ", n + 2)..tostring(v)
  end
  return _tconcat(t, "\n")
end


--- Text form.
--  @param S Symbolic object.
--  @return string.
parents.funcValue.pStr = function (S)
  local t = {}
  for i = 2, #S._ do
    t[#t+1] = _issym(S._[i]) and S._[i]:pStr() or tostring(S._[i])
  end
  return string.format("%s(%s)", S._[1], _tconcat(t, ","))
end


-- ============ PRODUCT ============

parents.product = {
  -- S._ = {{S1, pow1}, {S2, pow2}, ...}
  pId = 4,
  pIsAtom = false,
  pSignature = common.signaturePairs,
  pEq = common.eqPairs,
  pEval = common.evalPairs,
  pRawGet = common.rawgetPair,
  pRawSet = common.rawsetPair,
}


--- Split product to numeric and symbolic parts.
--  @param S Symbolic object.
--  @return Constant value.
parents.product.pGetConst = function (S)
  local k, v = 1, S._[1]
  if not _issym(v[1]) and not _issym(v[2]) then
    k = v[1] ^ v[2]
    -- update the rest of object
    _tremove(S._, 1)
    S._sign = nil
    if #S._ == 1 and S._[1][2] == 1 then  -- val^1
      -- simplify
      local g = S._[1][1]
      S._parent = g._parent
      S._sign = g.pIsAtom and g._sign or nil
      S._ = g._
    end
  end
  return k
end


--- Simplify product (in place).
--  @param S Symbolic object.
--  @param bFull Flag for recursive simplification.
parents.product.pSimp = function (S, bFull)
  if bFull then
    common.simpPair(S)  -- check for similar terms
    for _, v in ipairs(S._) do
      v[1] = _issym(v[1]) and v[1]:pSimp(bFull) or v[1]
      v[2] = _issym(v[2]) and v[2]:pSimp(bFull) or v[2]
    end
  end
  common.simpPair(S)
  -- empty list
  if #S._ == 0 then
    return 1
  elseif #S._ > 1 then
    _tsort(S._, _compList)
  end
  -- check constant
  if S._[1][1] == 0 then
    return 0
  elseif #S._ > 1 and (S._[1][1] == 1 or S._[1][2] == 0) then
    _tremove(S._, 1)
    S._sign = nil
  end
  -- change type
  if #S._ == 1 then
    local v = S._[1]
    if v[2] == 1 then   -- x^1
      return v[1]  -- extract expression
    elseif v[2] == 0 then  -- x^0
      return 1
    elseif v[1] == 1 or v[1] == 0 then  -- 1^x or 0^x
      return v[1]
    elseif not (_issym(v[2]) or _issym(v[1])) then
      return v[1]^v[2]  -- number
    end
  end
  return S
end


--- Sum object to string translation.
--  @param S Symbolic object.
--  @return String representation.
parents.product.pStr = function (S)
  local num, denom = {}, {}
  for _, v in ipairs(S._) do
    local k, x = v[2], _issym(v[1]) and v[1]:pStr() or tostring(v[1])
    if (#S._ > 1 or k ~= 1) and _issym(v[1]) and common.closed[v[1]._parent] then
      x = string.format("(%s)", x)
    end
    if _issym(k) then
      local w = k:pStr()
      if not k.pIsAtom then w = string.format("(%s)", w) end
      num[#num + 1] = string.format("%s^%s", x, w)
    elseif k > 0 then
      num[#num+1] =
        (k == 1) and x or string.format("%s^%s", x, tostring(k))
    else  -- v[1] < 0
      denom[#denom+1] =
        (k == -1) and x or string.format("%s^%s", x, tostring(-k))
    end
  end
  if #denom == 0 then
    return _tconcat(num, "*")
  else
    num = #num > 0 and _tconcat(num, "*") or "1"  -- reuse
    return string.format(
      #denom > 1 and "%s/(%s)" or "%s/%s", num, _tconcat(denom, "*"))
  end
end


--- Derivative of a product of symbols.
--  @param S1 Expression with products.
--  @param S2 Variable.
--  @return derivative.
parents.product.pDiff = function (S1, S2)
  local res = 0
  for i, v in ipairs(S1._) do
    local a, b, sum = v[1], v[2]
    local dx = _issym(a) and a:pDiff(S2) or 0
    if dx ~= 0 then
      sum = b * a^(b - 1) * dx
    end
    dx = _issym(b) and b:pDiff(S2) or 0
    if dx ~= 0 then
      local prod = a^b * symbolic:log(a)*dx
      sum = sum and (sum + prod) or prod
    end
    if sum and sum ~= 0 then
      local tmp = symbolic:_newExpr(parents.product, {})
      for j, w in ipairs(S1._) do
        if j ~= i then _tinsert(tmp._, w) end
      end
      tmp:pSignature()
      res = res + tmp * sum
    end
  end
  return res
end


--- Internal structure of a product object.
--  @param S Symbolic object.
--  @param n Shift size.
--  @return string representation.
parents.product.pInternal = function (S, n)
  local t = {string.format("%sPROD:", string.rep(" ", n))}
  local offset = string.rep(" ", n+2)
  for _, v in ipairs(S._) do
    t[#t+1] = _issym(v[1]) and v[1]:pInternal(n+2) or offset..tostring(v[1])
    if _issym(v[2]) then
      t[#t+1] = offset.."[^]"
      t[#t+1] = v[2]:pInternal(n+4)
    else
      t[#t+1] = string.format("%s[^ %s]", offset, tostring(v[2]))
    end
  end
  return _tconcat(t, "\n")
end


-- ============ SUM ============

parents.sum = {
  -- S._ = {{S1, k1}, {S2, k2}, ...}
  -- i.e. k1*S1 + k2*S2 + ...
  pId = 5,
  pIsAtom = false,
  pSignature = common.signaturePairs,
  pEq = common.eqPairs,
  pEval = common.evalPairs,
  pRawGet = common.rawgetPair,
  pRawSet = common.rawsetPair,
}


--- Derivative of a sum of symbols.
--  @param S1 Expression with sums.
--  @param S2 Variable.
--  @return derivative.
parents.sum.pDiff = function (S1, S2)
  local res = symbolic:_newExpr(parents.sum, {})
  for _, v in ipairs(S1._) do
    if _issym(v[1]) then
      _tinsert(res._, {v[1]:pDiff(S2), v[2]})
    end
  end
  local tmp = res:pSimp()
  if _issym(tmp) then
    res = tmp
  else
    return tmp
  end
  res:pSignature()
  return res
end


--- Internal structure of a product object.
--  @param S Symbolic object.
--  @param n Shift size.
--  @return string representation.
parents.sum.pInternal = function (S, n)
  local t = {string.format("%sSUM:", string.rep(" ", n))}
  local offset = string.rep(" ", n + 2)
  for _, v in ipairs(S._) do
    t[#t+1] = _issym(v[1]) and v[1]:pInternal(n + 2) or offset..tostring(v[1])
    t[#t+1] = string.format("%s[* %s]", offset, tostring(v[2]))
  end
  return _tconcat(t, "\n")
end


--- Simplify sum (in place).
--  @param S Symbolic object.
--  @param bFull Flag for recursive simplification.
parents.sum.pSimp = function (S, bFull)
  if bFull then
    common.simpPair(S, parents.product)  -- check for similar terms
    for _, v in ipairs(S._) do
      v[1] = _issym(v[1]) and v[1]:pSimp(bFull) or v[1]
    end
  end
  -- update structure
  common.simpPair(S, parents.product)
  -- empty list
  if #S._ == 0 then
    return 0
  end
  -- sort and remove zero constant
  if #S._ > 1 then
    _tsort(S._, _compList)
    if S._[1][1] == 0 or S._[1][2] == 0 then
      _tremove(S._, 1)
      S._sign = nil
    end
  end
  -- change type
  if #S._ == 1 then
    local v = S._[1]
    -- extract expression or make product
    return (v[2] == 1) and v[1] or (v[2]*v[1])
  end
  return S
end


--- Product object to string translation.
--  @param S Symbolic object.
--  @return String representation.
parents.sum.pStr = function (S)
  local plus, minus = {}, {}
  for _, v in ipairs(S._) do
    local k, x = v[2], _issym(v[1]) and v[1]:pStr() or tostring(v[1])
    if k > 0 then
      plus[#plus+1] =
        (k == 1) and x or string.format("%s*%s", tostring(k), x)
    else
      minus[#minus+1] =
        (k == -1) and x or string.format("%s*%s", tostring(-k), x)
    end
  end
  if #minus == 0 then
    return _tconcat(plus, "+")
  else
    return string.format(
      "%s-%s", _tconcat(plus, "+"), _tconcat(minus, "-"))
  end
end


-- ============ SYMBOL ============

parents.symbol = {
  -- S._ = name
  pId = 6,
  pIsAtom = true,
  pSignature = common.skip,
  pEq = function (S1, S2) return _issym(S1) and _issym(S2) and S1._ == S2._ end,
  pSimp = function (S) return S end,
  pInternal = function (S, n) return string.rep(" ", n) .. S._ end,
  pDiff = function (S1, S2) return S1._ == S2._ and 1 or 0 end,
  pEval = function (S, tEnv) return tEnv[S._] or S end,
  pStr = function (S) return S._ end,
  pRawGet = common.rawget,
  pRawSet = common.skip,
}


--- List of elements for printing in brackets.
common.closed = {
[parents.sum] = true,
[parents.product] = true,
}


--- Expand (a+b+c+..)^n
--  @param lst List of pairs {coef, obj}.
--  @param n Integer power.
--  @return sum of terms.
symbolic._binomial = function (lst, n)
  if n == 1 then return symbolic:_newExpr(parents.sum, lst) end
  local fl = _utils.calc.fl
  local nfl, m = fl(n), n + 1
  local res, pos, s = {}, {}, n
  repeat
    -- find group
    local tmp, sum = s, 0
    for i = 1, #lst do
      local v = math.modf(tmp / m)
      pos[i] = tmp - v*m
      tmp, sum = v, sum + pos[i]
    end
    -- add product
    if sum == n then
      local terms, p, q = {}, nfl, 1
      for i = 1, #lst do
        local pi = pos[i]
        if pi > 0 then
          terms[#terms+1] = {lst[i][1], pi}
          p = p / fl(pi)
          q = q * lst[i][2]^pi
        end
      end
      sum, tmp = math.modf(p*q)   -- reuse
      if tmp ~= 0 then sum = p*q end
      res[#res+1] = {symbolic:_newExpr(parents.product, terms), sum}
    end
    s = s + 1
  until pos[#pos] == n
  return symbolic:_newExpr(parents.sum, res)
end


--- Prepare structure for expression.
--  @param self Symbolic table.
--  @param parent Parent table.
--  @param v Argument.
--  @return Symbolic object.
symbolic._newExpr = function (self, parent, v)
  local o = {
    _parent = parent,
    _ = v,
  }
  return setmetatable(o, self)
end


--- Create symbolic variable.
--  @param self Symbolic table.
--  @param sName Variable name.
--  @return Symbolic object.
symbolic._newSymbol = function (self, sName)
  local sum = 0
  for i = 1, #sName do sum = (sum*8 + string.byte(sName, i, i)) % 100000 end
  local o = {
    _parent = parents.symbol,
    _sign = sum,
    _ = sName,
  }
  return setmetatable(o, self)
end


--- Collect terms of numerator or denomenator.
--  @param S Symbolic object.
--  @param k Flag, +1 or -1.
--  @return found terms or 1.
symbolic._ratGet = function (S, k)
  if S._parent ~= parents.product then return k > 0 and S or 1 end
  local acc = {}
  for _, v in ipairs(S._) do
    local t = v[2] * k
    if t > 0 then acc[#acc+1] = {v[1], t} end
  end
  if #acc == 1 and acc[1][2] == 1 then return acc[1][1] end
  return #acc > 0 and symbolic:_newExpr(parents.product, acc) or 1
end

-- list of predefined functions
symbolic._fnList = {}

-- list of derivatives
-- i-th position of a table corresponds to df/dxi
symbolic._fnDiff = {}

return symbolic
