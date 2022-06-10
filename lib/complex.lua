--[[		sonata/lib/complex.lua 

--- Manipulations with complex numbers.
--
--  Object structure </br>
--  <code> {REAL, IMAGINARY} </code></br>
--  i.e. complex number is a table which consists of two elements.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2022.

	module 'complex'
--]]

---------------- Tests --------------
--[[TEST

-- use 'complex'
Comp = require 'lib.complex'

-- real and imaginary pars
a = Comp(1,2)
-- or just imaginary
b = Comp(0,3)

-- imaginary unit
j = Comp._i
ans = 3+4*j                   --> Comp(3,4)

-- use trigonometrical form
ans = Comp.trig(2,0)          --> Comp(2,0)

-- arithmetic
ans = a + b                   --> Comp(1,5)

ans = Comp(0,3) - b           --> 0

ans = a * b                   --> Comp(-6,3)

ans = a / Comp._i             --> Comp(2,-1)

-- power can be complex
c = Comp(1,1)^Comp(2,-2)

-- real part
ans = c:re()                 --3> 6.147

-- imaginary part
ans = c:im()                 --1> 7.4

-- comparison
ans = (a == b)                --> false

ans = (a ~= b)                --> true

-- absolute value
ans = a:abs()                --3> 2.236

-- argument (angle)
ans = a:angle()              --3> 1.107

-- conjugated number
ans = a:conj()                --> Comp(1,-2)

-- some functions after import 
-- become default, such as
d = Comp.sqrt(-2)
ans = d:im()                  --3> 1.414

-- exp
ans = Comp.exp(d):re()        --3> 0.156

-- log
ans = Comp.log(d):re()        --3> 0.3465

-- sin 
ans = Comp.sin(d):im()        --3> 1.935

-- cos 
ans = Comp.cos(d)            --3> 2.178

-- tan
ans = Comp.tan(d):re()       --1> 0

-- sinh
ans = Comp.sinh(d):re()      --1> 0

-- cosh
ans = Comp.cosh(d)           --3> 0.156

-- tanh
ans = Comp.tanh(d):im()      --3> 6.334

-- asin
z = Comp(2,3)
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

-- show
print(a)

--]]

--	LOCAL

-- Compatibility with previous versions
local Ver = require("lib.utils")
-- Inter-module functionality
local Cross = Ver.cross
Ver = Ver.versions

-- help section 
local FUNCTIONS = 'functions'

--- Check object type.
--  @param c Object.
--  @return True if the object is a complex number.
local function iscomplex(v) return type(v) == 'table' and v.iscomplex end

--- Check imaginary part.
--  @param C complex number.
--  @return Simple number if possible.
local function numcomp(C) return Cross.eq(C._v[2], 0) and Cross.simp(C._v[1]) or C end

--- Number representation.
--  @param v Value.
--  @return String representation.
local function numStr(v) 
  return type(v) == 'number' and string.format('%.3f', v) or tostring(v) 
end

--- Find exponential with type conversation.
--  @param v Value.
--  @return exp(float(v))
local function fexp(v) return math.exp(Cross.float(v)) end

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
local function fsin(v) return math.sin(Cross.float(v)) end

--- Find cosine with type conversation.
--  @param v Real value.
--  @return cos(float(v))
local function fcos(v) return math.cos(Cross.float(v)) end

--	INFO

local help = SonataHelp or {new=function () return {} end}
-- description
local about = help:new("Manipulations with complex numbers.")

--	MODULE

local complex = {
-- mark
type='complex', iscomplex=true,
-- simplification
_simp_ = numcomp,
}

--- C1 + C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Sum of numbers.
complex.__add = function (C1,C2)
  if not (iscomplex(C1) and iscomplex(C2)) then
    local p = Cross.convert(C1, C2)
    if p then 
      return C1 + p
    else
      return Cross.convert(C2, C1) + C2
    end
  end
  local c1, c2 = C1._v, C2._v
  return numcomp(complex:_init_(c1[1]+c2[1], c1[2]+c2[2]))
end

--- C1 / C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Ratio of numbers.
complex.__div = function (C1,C2)
  if not (iscomplex(C1) and iscomplex(C2)) then
    local p = Cross.convert(C1, C2)
    if p then 
      return C1 / p
    else
      return Cross.convert(C2, C1) / C2
    end
  end
  local c1, c2 = C1._v, C2._v
  local denom = c2[1]*c2[1] + c2[2]*c2[2]
  return numcomp(complex:_init_((c1[1]*c2[1]+c1[2]*c2[2])/denom, (c1[2]*c2[1]-c1[1]*c2[2])/denom))
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
  local c1, c2 = C1._v, C2._v
  return Cross.eq(c1[1], c2[1]) and Cross.eq(c1[2], c2[2])
end

-- methametods
complex.__index = complex

--- C1 * C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Product of numbers.
complex.__mul = function (C1,C2)
  if not (iscomplex(C1) and iscomplex(C2)) then
    local p = Cross.convert(C1, C2)
    if p then 
      return C1 * p
    else
      return Cross.convert(C2, C1) * C2
    end
  end
  local c1, c2 = C1._v, C2._v
  return numcomp(complex:_init_(c1[1]*c2[1]-c1[2]*c2[2], c1[1]*c2[2]+c1[2]*c2[1]))
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
complex.__pow = function (C1,C2)
  if not (iscomplex(C1) and iscomplex(C2)) then
    local p = Cross.convert(C1, C2)
    if p then 
      return C1 ^ p
    else
      return Cross.convert(C2, C1) ^ C2
    end
  end
  local a0, a1 = complex.abs(C1), complex.angle(C1)
  local k = (a0 >= 0) and  math.log(a0) or -math.log(-a0)
  local c1, c2 = C1._v, C2._v
  local abs = a0^(Cross.float(c2[1]))*fexp(-a1*c2[2])
  local arg = k*c2[2]+c2[1]*a1
  return numcomp(complex:_init_(abs*fcos(arg), abs*fsin(arg)))
end

--- C1 - C2 
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Difference of numbers.
complex.__sub = function (C1,C2)
  if not (iscomplex(C1) and iscomplex(C2)) then
    local p = Cross.convert(C1, C2)
    if p then 
      return C1 - p
    else
      return Cross.convert(C2, C1) - C2
    end
  end
  local c1, c2 = C1._v, C2._v
  return numcomp(complex:_init_(c1[1]-c2[1], c1[2]-c2[2]))
end

--- String representation.
--  @param C Complex number.
--  @return String with complex number elements.
complex.__tostring = function (C)
  local a, b = numStr(C._v[1]), numStr(C._v[2])
  return string.format("%s%s%si", a, (b:sub(1,1) == '-' and '' or '+'), b)
end

--- -C
--  @param C Complex number.
--  @return Negative value.
complex.__unm = function (C) return complex:_init_(-C._v[1], -C._v[2]) end

complex.arithmetic = 'arithmetic'
about[complex.arithmetic] = {complex.arithmetic, "a+b, a-b, a*b, a/b, a^b, -a", help.META}

complex.comparison = 'comparison'
about[complex.comparison] = {complex.comparison, "a==b, a~=b", help.META}

--- Convert value into complex number.
--  @param v Source value.
--  @return Complex number if possible.
complex._convert_ = function (v)
  return (type(v) == 'number' or type(v) == 'table' and v.float) 
         and complex:_init_(v,0)
end

--- Create new object, set metatable
--  @param vRe Real part.
--  @param vIm Imaginary part, default is 0.
--  @return Complex number.
complex._init_ = function (self, vRe, vIm)  return setmetatable({_v={vRe, vIm}}, self) end

--- Find object norm.
--  @param C Complex number.
--  @return Numerical value.
complex._norm_ = function (C)
  return math.sqrt(Cross.norm(C._v[1])^2 + Cross.norm(C._v[2])^2)
end

-- Imaginary unit
complex._i  = complex:_init_(0,1)
about[complex._i] = {"_i", "Complex unit.", help.CONST}

--- Module of complex number.
--  @return Module of the number.
complex.abs = complex._norm_
about[complex.abs] = {"abs(C)", "Return module of complex number."}

--- Inverse cosine.
--  @param C Complex number.
--  @return Complex inverse cosine.
complex.acos = function (C) return -complex._i*complex.log(C+complex.sqrt(C*C-1)) end
about[complex.acos] = {"acos(C)", "Complex inverse cosine.", FUNCTIONS}

--- Inverse hyperbolic cosine.
--  @param C Complex number.
--  @return Complex inverse hyperbolic cosine.
complex.acosh = function (C) return complex.log(C+complex.sqrt(C*C-1)) end
about[complex.acosh] = {"acosh(C)", "Complex inverse hyperbolic cosine.", FUNCTIONS}

--- Argument of complex number.
--  @param C Complex number.
--  @return Argument of the number.
complex.angle = function (C) return Ver.atan2(Cross.float(C._v[2]), Cross.float(C._v[1])) end
about[complex.angle] = {"angle(C)", "Return argument of complex number."}

--- Inverse sine.
--  @param C Complex number.
--  @return Complex inverse sine.
complex.asin = function (C)
  local j = complex._i
  return -j*complex.log(j*C+complex.sqrt(1-C*C))
end
about[complex.asin] = {"asin(C)", "Complex inverse sine.", FUNCTIONS}

--- Inverse hyperbolic sine.
--  @param C Complex number.
--  @return Complex inverse hyperbolic sine.
complex.asinh = function (C) return complex.log(C+complex.sqrt(C*C+1)) end
about[complex.asinh] = {"asinh(C)", "Complex inverse hyperbolic sine.", FUNCTIONS}

--- Inverse tangent.
--  @param C Complex number.
--  @return Complex inverse tangent.
complex.atan = function (C)
  local j = complex._i
  return -0.5*j*complex.log((1+j*C)/(1-j*C))
end
about[complex.atan] = {"atan(C)", "Complex inverse tangent.", FUNCTIONS}

--- Inverse hyperbolic tangent.
--  @param C Complex number.
--  @return Complex inverse hyperbolic tangent.
complex.atanh = function (C) return 0.5*complex.log((1+C)/(1-C)) end
about[complex.atanh] = {"atanh(C)", "Complex inverse hyperbolic tangent.", FUNCTIONS}

--- Conjunction.
--  @param C Complex number.
--  @return Conjunction to the given number.
complex.conj = function (C) return complex:_init_(Cross.copy(C._v[1]), -Cross.copy(C._v[2])) end
about[complex.conj] = {"conj(C)", "Return the complex conjugate. Equal to ~C.", help.OTHER}
complex.__bnot = complex.conj

--- Cosine
--  @param C Complex number.
--  @return Complex cosine.
complex.cos = function (C)
  local c = C._v
  return numcomp(complex:_init_(fcos(c[1])*ch(c[2]), -fsin(c[1])*sh(c[2])))
end
about[complex.cos] = {"cos(C)", "Return cosine of a complex number.", FUNCTIONS}

--- Hyperbolic cosine
--  @param C Complex number.
--  @return Complex hyperbolic cosine.
complex.cosh = function (C) return 0.5*(complex.exp(C)+complex.exp(-C)) end
about[complex.cosh] = {"cosh(C)", "Return hyperbolic cosine of a real or complex number.", FUNCTIONS}

--- Exponent
--  @param C Complex number.
--  @return Complex exponent.
complex.exp = function (C)
  local r = fexp(C._v[1])
  return numcomp(complex:_init_(r*fcos(C._v[2]), r*fsin(C._v[2])))
end
about[complex.exp] = {"exp(C)", "Return exponent in for complex argument.", FUNCTIONS}

--- Get imaginary part.
--  @param C Complex number.
--  @return Imaginary part.
complex.im = function (C) return C._v[2] end
about[complex.im] = {"im(C)", "Get imaginary part."}

--- Natural logarithm
--  @param C Real or complex number.
--  @return Real or complex logarithm.
complex.log = function (C)
  if type(C) == "number" then
    return C <= 0 and complex:_init_(math.log(-C),math.pi) or math.log(C)
  else
    local c = C._v
    return numcomp(complex:_init_(
      0.5*math.log(Cross.float(c[1]^2 + c[2]^2)), 
      Ver.atan2(Cross.float(c[2]),Cross.float(c[1])))
    )
  end
end
about[complex.log] = {"log(C)", "Complex logarithm.", FUNCTIONS}

--- Get real part.
--  @param C Complex number.
--  @return Real part.
complex.re = function (C) return C._v[1] end
about[complex.re] = {"re(C)", "Get real part."}

--- Sinus
--  @param C Complex number.
--  @return Complex sinus.
complex.sin = function (C)
  local c = C._v
  return numcomp(complex:_init_(fsin(c[1])*ch(c[2]), fcos(c[1])*sh(c[2])))
end
about[complex.sin] = {"sin(C)", "Return sinus of a complex number.", FUNCTIONS}

--- Hyperbolic sinus
--  @param C Complex number.
--  @return Complex hyperbolic sinus.
complex.sinh = function (C) return 0.5*(complex.exp(C)-complex.exp(-C)) end
about[complex.sinh] = {"sinh(C)", "Return hyperbolic sinus of a complex number.", FUNCTIONS}

--- Square root with possibility of complex result.
--  @param C Real or complex number.
--  @return Real or complex square root.
complex.sqrt = function (C) 
  if type(C) == "number" then
    return C < 0 and complex:_init_(0,math.sqrt(-C)) or math.sqrt(C)
  else
    return complex.__pow(C, 0.5)
  end
end
about[complex.sqrt] = {"sqrt(C)", "Return square root. Result can be real of complex.", FUNCTIONS}

--- Tangent
--  @param C Complex number.
--  @return Complex tangent.
complex.tan = function (C)
  local c = C._v
  local den = fcos(2*c[1]) + ch(2*c[2])
  return numcomp(complex:_init_(fsin(2*c[1])/den, sh(2*c[2])/den))
end
about[complex.tan] = {"tan(C)", "Return tangent of a complex number.", FUNCTIONS}

--- Hyperbolic tangent
--  @param C Complex number.
--  @return Complex hyperbolic tangent.
complex.tanh = function (C) return complex.sinh(C) / complex.cosh(C) end
about[complex.tanh] = {"tanh(C)", "Return hyperbolic tangent of a complex number.", FUNCTIONS}

--- Create complex number from trigonometric representation.
--  @param vMod Module.
--  @param vArg Argument.
--  @return Complex number.
complex.trig = function (vMod,vArg) 
  return complex:_init_(vMod*fcos(vArg), vMod*fsin(vArg)) 
end
about[complex.trig] = {"trig(vModule,vAngle)", "Create complex number using module and angle."}

-- simplify constructor call
setmetatable(complex, {
__call = function (self, re, im)
  re = re or 0
  im = im or 0
  assert(type(re) == 'number' or type(re) == 'table' and re.float, "Wrong real part")
  assert(type(im) == 'number' or type(im) == 'table' and im.float, "Wrong imaginary part")
  return complex:_init_(re,im) 
end })
complex.Comp = 'Comp'
about[complex.Comp] = {"Comp([vRe=0,vIm=0])", "Create new complex number.", help.NEW}

--- Function for execution during the module import.
complex.onImport = function ()
  -- basic
  _i = complex._i
  local _sqrt = sqrt
  sqrt = function (a) return (iscomplex(a) or type(a) == 'number') and complex.sqrt(a) or _sqrt(a) end
  Main._updateHelp(sqrt,_sqrt)
  local _exp = exp 
  exp = function (a) return iscomplex(a) and complex.exp(a) or _exp(a) end
  Main._updateHelp(exp,_exp)
  local _log = log
  log = function (a) return (iscomplex(a) or type(a) == 'number') and complex.log(a) or _log(a) end
  Main._updateHelp(log,_log)

  -- trigonometric
  local _sin = sin
  sin = function (a) return iscomplex(a) and complex.sin(a) or _sin(a) end
  Main._updateHelp(sin,_sin)
  local _cos = cos
  cos = function (a) return iscomplex(a) and complex.cos(a) or _cos(a) end
  Main._updateHelp(sqrt,_sqrt)
  local _tan = tan
  tan = function (a) return iscomplex(a) and complex.tan(a) or _tan(a) end
  Main._updateHelp(tan,_tan)
  local _asin = asin 
  asin = function (a) return iscomplex(a) and complex.asin(a) or _asin(a) end
  Main._updateHelp(asin,_asin)
  local _acos = acos
  acos = function (a) return iscomplex(a) and complex.acos(a) or _acos(a) end
  Main._updateHelp(acos,_acos)
  local _atan = atan 
  atan = function (a) return iscomplex(a) and complex.atan(a) or _atan(a) end
  Main._updateHelp(atan,_atan)

  -- hyperbolic
  local _sinh = sinh
  sinh = function (a) return iscomplex(a) and complex.sinh(a) or _sinh(a) end
  Main._updateHelp(sinh,_sinh)
  local _cosh = cosh
  cosh = function (a) return iscomplex(a) and complex.cosh(a) or _cosh(a) end
  Main._updateHelp(cosh,_cosh)
  local _tanh = tanh
  tanh = function (a) return iscomplex(a) and complex.tanh(a) or _tanh(a) end
  Main._updateHelp(tanh,_tanh)
  local _asinh = asinh 
  asinh = function (a) return iscomplex(a) and complex.asinh(a) or _asinh(a) end
  Main._updateHelp(asinh,_asinh)
  local _acosh = acosh
  acosh = function (a) return iscomplex(a) and complex.acosh(a) or _acosh(a) end
  Main._updateHelp(acosh,_acosh)
  local _atanh = atanh 
  atanh = function (a) return iscomplex(a) and complex.atanh(a) or _atanh(a) end
  Main._updateHelp(atanh,_atanh)
end

-- Comment to remove descriptions
complex.about = about

return complex

--==========================
