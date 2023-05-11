--[[		sonata/lib/complex.lua

--- Manipulations with complex numbers.
--
--  Object structure </br>
--  <code> {REAL, IMAGINARY} </code></br>
--  i.e. complex number is a table which consists of two elements.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2023.

	module 'complex'
--]]


---------------- Tests --------------
--[[TEST

-- use 'complex'
_Z = require 'lib.complex'

-- real and imaginary pars
a = _Z(1,2)
-- or just imaginary
b = _Z(0,3)

-- imaginary unit
ans = 3+_Z:i(4)                --> _Z(3,4)

-- use trigonometrical form
ans = _Z:trig(2,0)            --> _Z(2,0)

-- arithmetic
ans = a + b                   --> _Z(1,5)

ans = _Z:i(3) - b             --> 0

ans = a * b                   --> _Z(-6,3)

ans = a / _Z:i()              --> _Z(2,-1)

-- power can be complex
c = _Z(1,1)^_Z(2,-2)

-- real part
ans = c:re()                 --3> 6.147

-- imaginary part
ans = c:im()                 --1> 7.4

-- comparison
ans = (a == b)                --> false

ans = (a ~= b)                --> true

-- absolute value
ans = a:abs()                --3> 2.236

-- argument
ans = a:arg()              --3> 1.107

-- conjugated number
ans = a:conj()                --> _Z(1,-2)

-- some functions after import
-- become default, such as
d = _Z(-2):sqrt()
ans = d:im()                  --3> 1.414

-- exp
ans = d:exp():re()            --3> 0.156

-- log
ans = d:log():re()            --3> 0.3465

-- sin
ans = d:sin():im()            --3> 1.935

-- cos
ans = d:cos():re()            --3> 2.178

-- tan
ans = d:tan():re()            --1> 0

-- sinh
ans = d:sinh():re()           --1> 0

-- cosh
ans = d:cosh():re()           --3> 0.156

-- tanh
ans = d:tanh():im()           --3> 6.334

-- asin
z = _Z(2,3)
ans = z:asin():im()          --3> 1.983

-- acos
ans = z:acos():re()          --2> 1.000

-- atan
ans = z:atan():im()          --3> 0.229

-- asinh
ans = z:asinh():re()         --3> 1.968

-- acosh
ans = z:acosh():im()         --1> 1.000

-- atanh
ans = z:atanh():re()         --3> 0.146

-- round in-place
z = _Z(1+1E-3, 2+1e-20)
z = z:round(5)
ans = z:re()                 --3> 1.001

ans = z:im()                  --> 2

-- show
print(a)

--]]


--	LOCAL

-- Compatibility with previous versions
local Ver = require("lib.utils")
-- Inter-module functionality
local Cross = Ver.cross
local Utils = Ver.utils
Ver = Ver.versions

local Cfloat, Ceq = Cross.float, Cross.eq

-- help section
local FUNCTIONS = 'functions'


--- Check imaginary part.
--  @param C complex number.
--  @return Simple number if possible.
local function numcomp(C)
  return Ceq(C._[2], 0) and Cross.simp(C._[1]) or C
end


--- Number representation.
--  @param v Value.
--  @return String representation.
local function numStr(v)
  return type(v) == 'number' and Utils.numstr(v) or tostring(v)
end


--- Find exponential with type conversation.
--  @param v Value.
--  @return exp(float(v))
local function fexp(v) return math.exp(Cfloat(v)) end


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
local function fsin(v) return math.sin(Cfloat(v)) end


--- Find cosine with type conversation.
--  @param v Real value.
--  @return cos(float(v))
local function fcos(v) return math.cos(Cfloat(v)) end


--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Manipulations with complex numbers."
}


--	MODULE

local complex = {
-- mark
type='complex', 
-- for external modules
iscomplex=true,
-- simplification
_simp = numcomp,
}

--- Check object type.
--  @param c Object.
--  @return True if the object is a complex number.
local function iscomplex(v) return getmetatable(v) == complex end

--- C1 + C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Sum of numbers.
complex.__add = function (C1, C2)
  if not (iscomplex(C1) and iscomplex(C2)) then
    local p = Cross.convert(C1, C2)
    return p and (C1 + p) or (Cross.convert(C2, C1) + C2)
  end
  local c1, c2 = C1._, C2._
  return numcomp(complex._new(c1[1]+c2[1], c1[2]+c2[2]))
end


--- C1 / C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Ratio of numbers.
complex.__div = function (C1, C2)
  if not (iscomplex(C1) and iscomplex(C2)) then
    local p = Cross.convert(C1, C2)
    return p and (C1 / p) or (Cross.convert(C2, C1) / C2)
  end
  local c11, c12 = Ver.unpack(C1._)
  local c21, c22 = Ver.unpack(C2._)
  local denom = c21*c21 + c22*c22
  return numcomp(complex._new(
    (c11*c21 + c12*c22)/denom, (c12*c21 - c11*c22)/denom))
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
  return Ceq(c1[1], c2[1]) and Ceq(c1[2], c2[2])
end


-- methametods
complex.__index = complex


--- C1 * C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Product of numbers.
complex.__mul = function (C1, C2)
  if not (iscomplex(C1) and iscomplex(C2)) then
    local p = Cross.convert(C1, C2)
    return p and (C1 * p) or (Cross.convert(C2, C1) * C2)
  end
  local c1, c2 = C1._, C2._
  return numcomp(
    complex._new(c1[1]*c2[1] - c1[2]*c2[2], c1[1]*c2[2] + c1[2]*c2[1]))
end


--- Set unknown key. Use proxy to access the element.
--  @param t Table.
--  @param k Key.
--  @param v Value.
complex.__newindex = function () error("Immutable object") end


--- C1 ^ C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Power.
complex.__pow = function (C1, C2)
  if not (iscomplex(C1) and iscomplex(C2)) then
    local p = Cross.convert(C1, C2)
    return p and (C1 ^ p) or (Cross.convert(C2, C1) ^ C2)
  end
  local a0, a1 = complex.abs(C1), complex.arg(C1)
  local k = (a0 >= 0) and  math.log(a0) or -math.log(-a0)
  local c1, c2 = C1._, C2._
  local abs = a0^(Cfloat(c2[1]))*math.exp(-a1*c2[2])
  local arg = k*c2[2]+c2[1]*a1
  return numcomp(complex._new(abs*fcos(arg), abs*fsin(arg)))
end


--- C1 - C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Difference of numbers.
complex.__sub = function (C1, C2)
  if not (iscomplex(C1) and iscomplex(C2)) then
    local p = Cross.convert(C1, C2)
    return p and (C1 - p) or (Cross.convert(C2, C1) - C2)
  end
  local c1, c2 = C1._, C2._
  return numcomp(complex._new(c1[1]-c2[1], c1[2]-c2[2]))
end


--- String representation.
--  @param C Complex number.
--  @return String with complex number elements.
complex.__tostring = function (C)
  local a, b = numStr(C._[1]), numStr(C._[2])
  return string.format("%s%s%si", a, (b:sub(1, 1) == '-' and '' or '+'), b)
end


--- -C
--  @param C Complex number.
--  @return Negative value.
complex.__unm = function (C) return complex._new(-C._[1], -C._[2]) end

about['_ar'] = {"arithmetic: a+b, a-b, a*b, a/b, a^b, -a", '', help.META}
about['_cmp'] = {"comparison: a==b, a~=b", '', help.META}


--- Convert value into complex number.
--  @param v Source value.
--  @return Complex number if possible.
complex._convert = function (v)
  return (type(v) == 'number' or type(v) == 'table' and v.float)
         and complex._new(v, 0)
end


--- Get deep copy of the complex number.
--  @param C Complex number.
--  @return Equal number.
complex._copy = function (C)
  return complex._new(C._[1], C._[2])
end


--- Create new object, set metatable
--  @param vRe Real part.
--  @param vIm Imaginary part, default is 0.
--  @return Complex number.
complex._new = function (vRe, vIm)
  return setmetatable({_={vRe, vIm}}, complex)
end


--- Complex unit
complex._i = complex._new(0, 1)


--- Find object norm.
--  @param C Complex number.
--  @return Numerical value.
complex._norm = function (C)
  return math.sqrt(Cfloat(C._[1])^2 + Cfloat(C._[2])^2)
end


--- Module of complex number.
--  @return Module of the number.
complex.abs = complex._norm
about[complex.abs] = {"Z:abs() --> float", "Return module of complex number."}


--- Inverse cosine.
--  @param C Complex number.
--  @return Complex inverse cosine.
complex.acos = function (C)
  return -complex._i * complex.log(C + complex.sqrt(C*C-1))
end
about[complex.acos] = {"Z:acos() --> y_Z", "Complex inverse cosine.", FUNCTIONS}


--- Inverse hyperbolic cosine.
--  @param C Complex number.
--  @return Complex inverse hyperbolic cosine.
complex.acosh = function (C) return complex.log(C + complex.sqrt(C*C-1)) end
about[complex.acosh] = {"Z:acosh() --> y_Z", 
  "Complex inverse hyperbolic cosine.", FUNCTIONS}


--- Argument of complex number.
--  @param C Complex number.
--  @return Argument of the number.
complex.arg = function (C)
  return Ver.atan2(Cfloat(C._[2]), Cfloat(C._[1]))
end
about[complex.arg] = {"Z:arg() --> float", "Return argument of complex number."}


--- Inverse sine.
--  @param C Complex number.
--  @return Complex inverse sine.
complex.asin = function (C)
  return -complex._i * complex.log(complex._i*C + complex.sqrt(1-C*C))
end
about[complex.asin] = {"Z:asin() --> y_Z", "Complex inverse sine.", FUNCTIONS}


--- Inverse hyperbolic sine.
--  @param C Complex number.
--  @return Complex inverse hyperbolic sine.
complex.asinh = function (C) return complex.log(C + complex.sqrt(C*C+1)) end
about[complex.asinh] = {"Z:asinh() --> y_Z", 
  "Complex inverse hyperbolic sine.", FUNCTIONS}


--- Inverse tangent.
--  @param C Complex number.
--  @return Complex inverse tangent.
complex.atan = function (C)
  local iC = complex._i * C
  return -0.5 * complex._i * complex.log((1 + iC)/(1 - iC))
end
about[complex.atan] = {"Z:atan() --> y_Z", 
  "Complex inverse tangent.", FUNCTIONS}


--- Inverse hyperbolic tangent.
--  @param C Complex number.
--  @return Complex inverse hyperbolic tangent.
complex.atanh = function (C) return 0.5*complex.log((1 + C)/(1 - C)) end
about[complex.atanh] = {"Z:atanh() --> y_Z", 
  "Complex inverse hyperbolic tangent.", FUNCTIONS}


--- Conjunction.
--  @param C Complex number.
--  @return Conjunction to the given number.
complex.conj = function (C)
  --return complex._new(Cross.copy(C._[1]), -Cross.copy(C._[2]))
  return complex._new(C._[1], -C._[2])
end
about[complex.conj] = {"Z:conj() --> conj_Z", 
  "Return the complex conjugate. Equal to ~C."}
complex.__bnot = complex.conj


--- Cosine
--  @param C Complex number.
--  @return Complex cosine.
complex.cos = function (C)
  local c = C._
  return numcomp(complex._new(fcos(c[1])*ch(c[2]), -fsin(c[1])*sh(c[2])))
end
about[complex.cos] = {"Z:cos() --> y_Z", 
  "Return cosine of a complex number.", FUNCTIONS}


--- Hyperbolic cosine
--  @param C Complex number.
--  @return Complex hyperbolic cosine.
complex.cosh = function (C) return 0.5*(complex.exp(C) + complex.exp(-C)) end
about[complex.cosh] = {"Z:cosh() --> y_Z", 
  "Return hyperbolic cosine of a real or complex number.", FUNCTIONS}


--- Exponent
--  @param C Complex number.
--  @return Complex exponent.
complex.exp = function (C)
  local r = fexp(C._[1])
  return numcomp(complex._new(r*fcos(C._[2]), r*fsin(C._[2])))
end
about[complex.exp] = {"Z:exp() --> y_Z", 
  "Return exponent in for complex argument.", FUNCTIONS}


-- Imaginary unit
complex.i = function (self, v)
  v = v or 1
  if not (type(v) == 'number' or type(v) == 'table' and v.float) then 
    error( "Wrong argument") 
  end
  return complex._new(0, v)
end
about[complex.i] = {":i(x=1) --> new_Z", "Return x*i.", help.STATIC}


--- Get imaginary part.
--  @param C Complex number.
--  @return Imaginary part.
complex.im = function (C) return C._[2] end
about[complex.im] = {"Z:im() --> var", "Get imaginary part."}


--- Natural logarithm
--  @param C Real or complex number.
--  @return Real or complex logarithm.
complex.log = function (C)
  if type(C) == "number" then
    return C <= 0 and complex._new(math.log(-C), math.pi) or math.log(C)
  else
    local c1, c2 = Cfloat(C._[1]), Cfloat(C._[2])
    return numcomp(complex._new(0.5*math.log(c1*c1 + c2*c2), Ver.atan2(c2, c1)))
  end
end
about[complex.log] = {"Z:log() --> y_Z", "Complex logarithm.", FUNCTIONS}


--- Get real part.
--  @param C Complex number.
--  @return Real part.
complex.re = function (C) return C._[1] end
about[complex.re] = {"Z:re() --> var", "Get real part."}


--- Round real and imaginary parts to some number of digits.
--  For non-float value round to 0.
--  @param C Complex number.
complex.round = function (C, N)
  N = N or 6
  local tol = 10^(-N)
  local a, b = C._[1], C._[2]
  a = type(a) == 'number' and Utils.round(a, tol) or
            Cross.norm(a) < tol and 0 or a
  b = type(b) == 'number' and Utils.round(b, tol) or
            Cross.norm(b) < tol and 0 or b
  return complex._new(a, b)
end
about[complex.round] = {"Z:round(N=6) --> rounded_Z", 
  "Round in-place to specified number of digits."}


--- Sinus
--  @param C Complex number.
--  @return Complex sinus.
complex.sin = function (C)
  local c = C._
  return numcomp(complex._new(fsin(c[1])*ch(c[2]), fcos(c[1])*sh(c[2])))
end
about[complex.sin] = {"Z:sin() --> y_Z", "Return sinus of a complex number.", FUNCTIONS}


--- Hyperbolic sinus
--  @param C Complex number.
--  @return Complex hyperbolic sinus.
complex.sinh = function (C) return 0.5*(complex.exp(C) - complex.exp(-C)) end
about[complex.sinh] = {"Z:sinh() --> y_Z", 
  "Return hyperbolic sinus of a complex number.", FUNCTIONS}


--- Square root with possibility of complex result.
--  @param C Real or complex number.
--  @return Real or complex square root.
complex.sqrt = function (C)
  if type(C) == "number" then
    return C < 0 and complex._new(0, math.sqrt(-C)) or math.sqrt(C)
  else
    return complex.__pow(C, 0.5)
  end
end
about[complex.sqrt] = {"Z:sqrt() --> y_Z", 
  "Return square root. Result can be real of complex.", FUNCTIONS}


--- Tangent
--  @param C Complex number.
--  @return Complex tangent.
complex.tan = function (C)
  local c = C._
  local den = fcos(2*c[1]) + ch(2*c[2])
  return numcomp(complex._new(fsin(2*c[1])/den, sh(2*c[2])/den))
end
about[complex.tan] = {"Z:tan() --> y_Z", 
  "Return tangent of a complex number.", FUNCTIONS}


--- Hyperbolic tangent
--  @param C Complex number.
--  @return Complex hyperbolic tangent.
complex.tanh = function (C) return complex.sinh(C) / complex.cosh(C) end
about[complex.tanh] = {"Z:tanh() --> y_Z", 
  "Return hyperbolic tangent of a complex number.", FUNCTIONS}


--- Create complex number from trigonometric representation.
--  @param self Do nothing.
--  @param vMod Module.
--  @param vArg Argument.
--  @return Complex number.
complex.trig = function (self, vMod, vArg)
  return complex._new(vMod*fcos(vArg), vMod*fsin(vArg))
end
about[complex.trig] = {":trig(module, angle) --> new_Z",
  "Create complex number using module and angle.", help.STATIC}


-- simplify constructor call
setmetatable(complex, {
__call = function (self, re, im)
  re = re or 0
  im = im or 0
  assert(type(re) == 'number' or type(re) == 'table' and re.float,
    "Wrong real part")
  assert(type(im) == 'number' or type(im) == 'table' and im.float,
    "Wrong imaginary part")
  return complex._new(re, im)
end })
about[complex] = {" (re=0, im=0) --> new_Z", 
  "Create new complex number.", help.STATIC}


if Sonata then  -- GLOBAL

-- complex unit
_i = complex._i

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

end   -- GLOBAL


-- Comment to remove descriptions
complex.about = about

return complex

--==========================
