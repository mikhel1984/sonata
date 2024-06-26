--[[		sonata/lib/complex.lua

--- Manipulations with complex numbers.
--
--  Object structure </br>
--  <code> {REAL, IMAGINARY} </code></br>
--  i.e. complex number is a table which consists of two elements.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2024.

	module 'complex'
--]]


---------------- Tests --------------
--[[TEST_IT

-- use 'complex'
Z = require 'matlib.complex'

-- real and imaginary pars
a = Z(1,2)
-- or just imaginary
b = Z(0,3)

-- imaginary unit
ans = 3+Z:i(4)                -->  Z(3,4)

-- use polar form
ans = 2*Z:E(1.57):im()     --2>  2.0

-- arithmetic
ans = a + b                   -->  Z(1,5)

ans = Z:i(3) - b              -->  0

ans = a * b                   -->  Z(-6,3)

-- call complex unit I = Z:i()
ans = a / I                   -->  Z(2,-1)

-- power can be complex
c = Z(1,1)^Z(2,-2)

-- real part
ans = c:re()                 --3>  6.147

-- imaginary part
ans = c:im()                 --1>  7.4

-- comparison
ans = (a == b)                -->  false

ans = (a ~= b)                -->  true

-- absolute value
ans = a:abs()                --3>  2.236

-- argument (angle, rad)
ans = a:arg()                --3>  1.107

-- conjugated number
ans = a:conj()                -->  Z(1,-2)

-- some functions after import
-- become default, such as
d = Z(-2):sqrt()
ans = d:im()                 --3>  1.414

-- exp
ans = d:exp():re()           --3>  0.156

-- log
ans = d:log():re()           --3>  0.3465

-- sin
ans = d:sin():im()           --3>  1.935

-- cos
ans = d:cos():re()           --3>  2.178

-- tan
ans = d:tan():re()           --1>  0

-- sinh
ans = d:sinh():re()          --1>  0

-- cosh
ans = d:cosh():re()          --3>  0.156

-- tanh
ans = d:tanh():im()          --3>  6.334

-- asin
z = Z(2,3)
ans = z:asin():im()          --3>  1.983

-- acos
ans = z:acos():re()          --2>  1.000

-- atan
ans = z:atan():im()          --3>  0.229

-- asinh
ans = z:asinh():re()         --3>  1.968

-- acosh
ans = z:acosh():im()         --1>  1.000

-- atanh
ans = z:atanh():re()         --3>  0.146

-- show
print(a)

-- update env on import
-- update some methods
ans = sqrt(-1)                -->  I

ans = log(-1):im()           --3>  math.pi

--]]


--	LOCAL

-- Compatibility with previous versions
local Ver = require("matlib.utils")
-- Inter-module functionality
local Cross = Ver.cross
local Utils = Ver.utils
Ver = Ver.versions

local Cfloat, Czero = Cross.float, Cross.isZero

-- help section
local FUNCTIONS = 'functions'


--- Number representation.
--  @param v Value.
--  @return String representation.
local function numStr(v)
  return type(v) == 'number' and Utils.numstr(v) or tostring(v)
end


--- Find exponential with type conversation.
--  @param v Value.
--  @return exp(float(v))
local function fexp(v) return math.exp(assert(Cfloat(v))) end


--- Hyperbolic cosine.
--  @param v Real value.
--  @return Hyperbolic cosine value.
local function ch (v) return 0.5*(fexp(v)+fexp(-v)) end


--- Hyperbolic sine.
--  @param v Real value.
--  @return Hyperbolic sine value.
local function sh (v) return 0.5*(fexp(v)-fexp(-v)) end


--- Find sinus with type conversation.
--  @param v Real value.
--  @return sin(float(v))
local function fsin(v) return math.sin(assert(Cfloat(v))) end


--- Find cosine with type conversation.
--  @param v Real value.
--  @return cos(float(v))
local function fcos(v) return math.cos(assert(Cfloat(v))) end


--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Manipulations with complex numbers."
}


--	MODULE

local complex = {
-- mark
type = 'complex',
-- for external modules
iscomplex = true,
-- simplification
_simp = function (C)
  return Czero(C._[2]) and Cross.simp(C._[1]) or C
end,
-- strip value, when uncommented
--STRIP = 1E-12,
}


--- Check if the value can be part of complex number.
--  @param v Value to check.
--  @return true of false.
local function compatible(v)
  return type(v) == 'number' or getmetatable(v).float ~= nil
end

--- Check object type.
--  @param c Object.
--  @return True if the object is a complex number.
local function iscomplex(v) return getmetatable(v) == complex end


--- Check values, make complex if need.
--  @param a Real part.
--  @param b Imaginary part.
--  @return complex or number.
local function numOrComp(a, b)
  return Czero(b) and Cross.simp(a) or complex._new(a, b)
end


--- C1 + C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Sum of numbers.
complex.__add = function (C1, C2)
  if not iscomplex(C2) then
    local v = complex._convert(C2)
    return v and C1 + v or C2.__add(C1, C2)
  elseif not iscomplex(C1) then
    local v = complex._convert(C1)
    return v and v + C2 or error('Not def')
  end
  local c1, c2 = C1._, C2._
  return numOrComp(c1[1]+c2[1], c1[2]+c2[2])
end


--- C1 / C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Ratio of numbers.
complex.__div = function (C1, C2)
  if not iscomplex(C2) then
    local v = complex._convert(C2)
    return v and C1 / v or C2.__div(C1, C2)
  elseif not iscomplex(C1) then
    local v = complex._convert(C1)
    return v and v / C2 or error('Not def')
  end
  local c11, c12 = Ver.unpack(C1._)
  local c21, c22 = Ver.unpack(C2._)
  local denom = c21*c21 + c22*c22
  return numOrComp(
    (c11*c21 + c12*c22)/denom, (c12*c21 - c11*c22)/denom)
end


--- C1 == C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return True if the real and complex parts are the same.
complex.__eq = function (C1, C2)
  if not (iscomplex(C1) and iscomplex(C2)) then
    local p = Cross.convert(C1, C2)
    if p then
      return C1 == p
    else
      return Cross.convert(C2, C1) == C2
    end
  end
  local c1, c2 = C1._, C2._
  return Cross.eq(c1[1], c2[1]) and Cross.eq(c1[2], c2[2])
end


-- methametods
complex.__index = complex


--- C1 * C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Product of numbers.
complex.__mul = function (C1, C2)
  if not iscomplex(C2) then
    local v = complex._convert(C2)
    return v and C1 * v or C2.__mul(C1, C2)
  elseif not iscomplex(C1) then
    local v = complex._convert(C1)
    return v and v * C2 or error('Not def')
  end
  local c1, c2 = C1._, C2._
  return numOrComp(c1[1]*c2[1] - c1[2]*c2[2], c1[1]*c2[2] + c1[2]*c2[1])
end


--- Don't set unknown key.
complex.__newindex = function () error("Immutable object") end


--- C1 ^ C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Power.
complex.__pow = function (C1, C2)
  if not iscomplex(C2) then
    local v = complex._convert(C2)
    return v and C1 ^ v or C2.__pow(C1, C2)
  elseif not iscomplex(C1) then
    local v = complex._convert(C1)
    return v and v ^ C2 or error('No def')
  end
  local a0, a1 = complex.abs(C1), complex.arg(C1)
  local k = (a0 >= 0) and  math.log(a0) or -math.log(-a0)
  local c1, c2 = C1._, C2._
  local abs = a0^(Cfloat(c2[1]))*math.exp(-a1*c2[2])
  local arg = k*c2[2]+c2[1]*a1
  return numOrComp(abs*fcos(arg), abs*fsin(arg))
end


--- C1 - C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Difference of numbers.
complex.__sub = function (C1, C2)
  if not iscomplex(C2) then
    local v = complex._convert(C2)
    return v and C1 - v or C2.__sub(C1, C2)
  elseif not iscomplex(C1) then
    local v = complex._convert(C1)
    return v and v - C2 or error('No def')
  end
  local c1, c2 = C1._, C2._
  return numOrComp(c1[1]-c2[1], c1[2]-c2[2])
end


--- String representation.
--  @return String with complex number elements.
complex.__tostring = function (self)
  local a, b = numStr(self._[1]), numStr(self._[2])
  return string.format("%s%s%si", a, (b:sub(1, 1) == '-' and '' or '+'), b)
end


--- -C
--  @return Negative value.
complex.__unm = function (self) return complex._new(-self._[1], -self._[2]) end


-- Metamethods
about['_ar'] = {"arithmetic: a+b, a-b, a*b, a/b, a^b, -a", nil, help.META}
about['_cmp'] = {"comparison: a==b, a~=b", nil, help.META}


--- Convert value into complex number.
--  Cross lib.
--  @param v Source value.
--  @return Complex number if possible.
complex._convert = function (v) return compatible(v) and complex._new(v, 0) end


--- Get deep copy of the complex number.
--  @return Equal number.
complex._copy = function (self)
  return complex._new(self._[1], self._[2])
end


--- Create new object, set metatable
--  @param vRe Real part.
--  @param vIm Imaginary part, default is 0.
--  @return Complex number.
complex._new = function (vRe, vIm)
  if complex.STRIP then
    if Cross.norm(vRe) < complex.STRIP then vRe = 0 end
    if Cross.norm(vIm) < complex.STRIP then vIm = 0 end
  end
  return setmetatable({_={vRe, vIm}}, complex)
end


--- Complex unit
complex._i = complex._new(0, 1)


--- Check if the complex number is 0.
--  Cross lib.
--  @return true when zero.
complex._isZero = function (self)
  return Czero(self._[1]) and Czero(self._[2])
end


--- Find object norm.
--  Cross lib.
--  @return Numerical value.
complex._norm = function (self)
  local a, b = Cfloat(self._[1]), Cfloat(self._[2])
  return math.sqrt(a*a + b*b)
end


--- Limit number of digits.
--  Cross lib.
--  @param tol Desired tolerance.
--  @return stripped complex or real number.
complex._round = function (self, tol)
  return numOrComp(Cross.round(self._[1], tol), Cross.round(self._[2], tol))
end


--- Module of complex number.
--  @return Module of the number.
complex.abs = complex._norm
about[complex.abs] = {"C:abs() --> float", "Return module of complex number."}


--- Inverse cosine.
--  @return Complex inverse cosine.
complex.acos = function (self)
  local v = complex._new(0, -1) * complex.log(self + complex.sqrt(self*self-1))
  return iscomplex(v) and v or complex._new(v, 0)
end
about[complex.acos] = {"C:acos() --> y_C", "Complex inverse cosine.", FUNCTIONS}


--- Inverse hyperbolic cosine.
--  @return Complex inverse hyperbolic cosine.
complex.acosh = function (self)
  return complex.log(self + complex.sqrt(self*self-1))
end
about[complex.acosh] = {"C:acosh() --> y_C",
  "Complex inverse hyperbolic cosine.", FUNCTIONS}


--- Argument of complex number.
--  @return Argument of the number.
complex.arg = function (self)
  return Ver.atan2(assert(Cfloat(self._[2])), assert(Cfloat(self._[1])))
end
about[complex.arg] = {"C:arg() --> float", "Return argument of complex number."}


--- Inverse sine.
--  @return Complex inverse sine.
complex.asin = function (self)
  local v = complex._new(0, -1) * complex.log(
    complex._i*self + complex.sqrt(1-self*self))
  return iscomplex(v) and v or complex._new(v, 0)
end
about[complex.asin] = {"C:asin() --> y_C", "Complex inverse sine.", FUNCTIONS}


--- Inverse hyperbolic sine.
--  @return Complex inverse hyperbolic sine.
complex.asinh = function (self)
  return complex.log(self + complex.sqrt(self*self+1))
end
about[complex.asinh] = {"C:asinh() --> y_C",
  "Complex inverse hyperbolic sine.", FUNCTIONS}


--- Inverse tangent.
--  @return Complex inverse tangent.
complex.atan = function (self)
  local iC = complex._i * self
  local v = complex._new(0, -0.5) * complex.log((1 + iC)/(1 - iC))
  return iscomplex(v) and v or complex._new(v, 0)
end
about[complex.atan] = {"C:atan() --> y_C",
  "Complex inverse tangent.", FUNCTIONS}


--- Inverse hyperbolic tangent.
--  @return Complex inverse hyperbolic tangent.
complex.atanh = function (self) return 0.5*complex.log((1 + self)/(1 - self)) end
about[complex.atanh] = {"C:atanh() --> y_C",
  "Complex inverse hyperbolic tangent.", FUNCTIONS}


--- Conjunction.
--  @return Conjunction to the given number.
complex.conj = function (self)
  return complex._new(self._[1], -self._[2])
end
about[complex.conj] = {"C:conj() --> conj_C",
  "Return the complex conjugate. Equal to ~C."}
complex.__bnot = complex.conj


--- Cosine
--  @return Complex cosine.
complex.cos = function (self)
  local c = self._
  return complex._new(fcos(c[1])*ch(c[2]), -fsin(c[1])*sh(c[2]))
end
about[complex.cos] = {"C:cos() --> y_C",
  "Return cosine of a complex number.", FUNCTIONS}


--- Hyperbolic cosine
--  @return Complex hyperbolic cosine.
complex.cosh = function (self)
  local v = 0.5*(complex.exp(self) + complex.exp(-self))
  return iscomplex(v) and v or complex._new(v, 0)
end
about[complex.cosh] = {"C:cosh() --> y_C",
  "Return hyperbolic cosine of a real or complex number.", FUNCTIONS}


--- Exponent
--  @return Complex exponent.
complex.exp = function (self)
  local r = fexp(self._[1])
  return complex._new(r*fcos(self._[2]), r*fsin(self._[2]))
end
about[complex.exp] = {"C:exp() --> y_C",
  "Return exponent in for complex argument.", FUNCTIONS}


--- Imaginary unit maker.
--  @param v Imaginary part.
--  @return i*v
complex.i = function (_, v)
  if v and not compatible(v) then
    error "Wrong argument"
  end
  return complex._new(0, v or 1)
end
about[complex.i] = {":i(x=1) --> new_C", "Return x*i.", help.STATIC}


--- Get imaginary part.
--  @return Imaginary part.
complex.im = function (self) return self._[2] end
about[complex.im] = {"C:im() --> var", "Get imaginary part."}


--- Natural logarithm
--  @param v Real or complex value.
--  @return Real or complex logarithm.
complex.log = function (v)
  if type(v) == 'number' then
    return v < 0 and complex._new(math.log(-v), math.pi) or math.log(v)
  else
    local c1, c2 = assert(Cfloat(v._[1])), assert(Cfloat(v._[2]))
    return complex._new(0.5*math.log(c1*c1 + c2*c2), Ver.atan2(c2, c1))
  end
end
about[complex.log] = {"C:log() --> y_C", "Complex logarithm.", FUNCTIONS}


--- Get real part.
--  @return Real part.
complex.re = function (self) return self._[1] end
about[complex.re] = {"C:re() --> var", "Get real part."}


--- Sinus
--  @return Complex sinus.
complex.sin = function (self)
  local c = self._
  return complex._new(fsin(c[1])*ch(c[2]), fcos(c[1])*sh(c[2]))
end
about[complex.sin] = {"C:sin() --> y_C",
  "Return sinus of a complex number.", FUNCTIONS}


--- Hyperbolic sinus
--  @return Complex hyperbolic sinus.
complex.sinh = function (self)
  local v = 0.5*(complex.exp(self) - complex.exp(-self))
  return iscomplex(v) and v or complex._new(v, 0)
end
about[complex.sinh] = {"C:sinh() --> y_C",
  "Return hyperbolic sinus of a complex number.", FUNCTIONS}


--- Square root with possibility of complex result.
--  @param v Real or complex number.
--  @return Real or complex square root.
complex.sqrt = function (v)
  if type(v) == 'number' then
    return v < 0 and complex._new(0, math.sqrt(-v)) or math.sqrt(v)
  else
    local c = v._
    local a, b = Cross.norm(c[1]), Cross.norm(c[2])
    local w = math.sqrt(0.5 * (a + math.sqrt(a*a + b*b)))
    return  w == 0 and complex._new(0, 0)
      or c[1] >= 0 and complex._new(w, c[2]/(2*w))
      or c[2] >= 0 and complex._new(b/(2*w), w)
                    or complex._new(b/(2*w), -w)
  end
end
about[complex.sqrt] = {"C:sqrt() --> y_C",
  "Return square root. Result can be real of complex.", FUNCTIONS}


--- Tangent
--  @return Complex tangent.
complex.tan = function (self)
  local c = self._
  local den = fcos(2*c[1]) + ch(2*c[2])
  return complex._new(fsin(2*c[1])/den, sh(2*c[2])/den)
end
about[complex.tan] = {"C:tan() --> y_C",
  "Return tangent of a complex number.", FUNCTIONS}


--- Hyperbolic tangent
--  @return Complex hyperbolic tangent.
complex.tanh = function (self)
  local v = complex.sinh(self) / complex.cosh(self)
  return iscomplex(v) and v or complex._new(v, 0)
end
about[complex.tanh] = {"C:tanh() --> y_C",
  "Return hyperbolic tangent of a complex number.", FUNCTIONS}


--- Polar form point position on the unit circle.
--  @param ang Angle.
--  @return Complex number.
complex.E = function (self, ang)
  return complex._new(fcos(ang), fsin(ang))
end
about[complex.E] = {":E(phy) --> cos(phy)+i*sin(phy)",
  "Make complex number exp(i*phy).", help.STATIC}


-- simplify constructor call
setmetatable(complex, {
__call = function (_, re, im)
  if iscomplex(re) then return re end
  re = re or 0
  im = im or 0
  assert(compatible(re), "Wrong real part")
  assert(compatible(im), "Wrong imaginary part")
  return complex._new(re, im)
end })
about[complex] = {" (re=0, im=0) --> new_C",
  "Create new complex number.", help.STATIC}


if Sonata
then  --=====================

  -- complex unit
  I = complex._i

  -- redefine square root
  local _sqrt = sqrt
  sqrt = function (a)
    return (iscomplex(a) or type(a) == 'number') and complex.sqrt(a) or _sqrt(a)
  end

  -- redefine logarithm
  local _log = log
  log = function (a)
    return (iscomplex(a) or type(a) == 'number') and complex.log(a) or _log(a)
  end

end   --=====================


-- Comment to remove descriptions
complex.about = about

return complex

--==========================
