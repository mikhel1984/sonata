--[[      sonatalib/complex.lua 

--- Manipulations with complex numbers.
--  
--  Object structure                </br>
--  <code> {REAL, IMAGINARY} </code></br>
--  i.e. complex number is a table which consists of two elements.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonatalib</a> collection, 2017-2019.

            module 'complex'
--]]

---------------- Tests --------------
--[[TEST

-- import 'complex'
Comp = require 'sonatalib.complex'

-- real and imaginary pars
a = Comp(1,2)
-- or just imaginary
b = Comp(3)
ans = b                        --> Comp(0,3)

-- imaginary unit
j = Comp._i
ans = 3+4*j                    --> Comp(3,4)

-- use trigonometrical form
ans = Comp.trig(2,0)           --> Comp(2,0)

-- arithmetic
ans = a + b                    --> Comp(1,5)

ans = Comp(3) - b              --> Comp(0)

ans = a * b                    --> Comp(-6,3)

ans = a / Comp._i              --> Comp(2,-1)

-- power can be complex
c = Comp(1,1)^Comp(2,-2)

-- real part
ans = c:real()                 --3> 6.147

-- imaginary part
ans = c:imag()                 --1> 7.4

-- comparison
ans = (a == b)                 --> false

ans = (a ~= b)                 --> true

-- absolute value
ans = a:abs()                  --3> 2.236

-- argument (angle)
ans = a:angle()                --3> 1.107

-- conjugated number
ans = a:conj()                 --> Comp(1,-2)

-- some functions after import 
-- become default, such as
d = Comp.sqrt(-2)
ans = d:imag()                 --3> 1.414

-- exp
ans = Comp.exp(d):real()       --3> 0.156

-- log
ans = Comp.log(d):real()       --3> 0.3465

-- sin 
ans = Comp.sin(d):imag()       --3> 1.935

-- cos 
ans = Comp.cos(d):imag()       --1> 0

-- tan
ans = Comp.tan(d):real()       --1> 0

-- sinh
ans = Comp.sinh(d):real()      --1> 0

-- cosh
ans = Comp.cosh(d):real()      --3> 0.156

-- tanh
ans = Comp.tanh(d):imag()      --3> 6.334

-- asin
z = Comp(2,3)
ans = z:asin():imag()          --3> 1.983

-- acos 
ans = z:acos():real()          --2> 1.000

-- atan
ans = z:atan():imag()          --3> 0.229

-- asinh
ans = z:asinh():real()         --3> 1.968

-- acosh
ans = z:acosh():imag()         --1> 1.000

-- atanh
ans = z:atanh():real()         --3> 0.146

-- make copy
ans = a:copy()                 --> a

-- show
print(a)

--]]

--	LOCAL

-- Compatibility with previous versions
local Ver = require "sonatalib.versions"

local REAL, IMAG = 1, 2

-- help section 
local FUNCTIONS = 'functions'

--- Check object type.
--  @param c Object.
--  @return True if the object is a complex number.
local function iscomplex(c) return type(c) == 'table' and c.iscomplex end

--- Hyperbolic cosine.
--  @param x Real number.
--  @return Hyperbolic cosine value.
local function ch (x) return 0.5*(math.exp(x)+math.exp(-x)) end
--- Hyperbolic sine.
--  @param x Real number.
--  @return Hyperbolic sine value.
local function sh (x) return 0.5*(math.exp(x)-math.exp(-x)) end

--	INFO

local help = LC_DIALOG and (require "sonatalib.help") or {new=function () return {} end}

--	MODULE

local complex = {
-- mark
type='complex', iscomplex=true,
-- description
about = help:new("Manipulations with complex numbers."),
}

complex.__index = complex

--- Create new object, set metatable
--  @param re Real part.
--  @param im Imaginary part, default is 0.
--  @return Complex number.
complex.new = function (self, re, im)   return setmetatable({im and re or 0, im or re}, self) end

--- Create complex number from trigonometric representation.
--  @param mod Module.
--  @param arg Argument.
--  @return Complex number.
complex.trig = function (mod,arg) return complex:new(mod*math.cos(arg), mod*math.sin(arg)) end
complex.about[complex.trig] = {"trig(module,angle)", "Create complex number using module and angle."}

--- Create copy of the complex number.
--  @param C Source value.
--  @return Complex number.
complex.copy = function (Z) return complex:new(Z[REAL], Z[IMAG]) end
complex.about[complex.copy] = {"copy(Z)", "Create copy of the complex number.", help.OTHER}

--- Correct arguments.
--  @param a Real or complex number.
--  @param b Real or complex number (optional).
--  @return Complex number(s).
complex._args_ = function (a,b)
   a = iscomplex(a) and a or complex:new(a,0)
   if b then
      b = iscomplex(b) and b or complex:new(b,0)
   end
   return a,b
end

--- Z1 + Z2
--  @param Z1 Real or complex number.
--  @param Z2 Real or complex number.
--  @return Sum of numbers.
complex.__add = function (Z1,Z2)
   Z1,Z2 = complex._args_(Z1,Z2)
   return complex:new(Z1[1]+Z2[1], Z1[2]+Z2[2])
end

--- Z1 - Z2 
--  @param Z1 Real or complex number.
--  @param Z2 Real or complex number.
--  @return Difference of numbers.
complex.__sub = function (Z1,Z2)
   Z1,Z2 = complex._args_(Z1,Z2)
   return complex:new(Z1[1]-Z2[1], Z1[2]-Z2[2])
end

--- Z1 * Z2
--  @param Z1 Real or complex number.
--  @param Z2 Real or complex number.
--  @return Product of numbers.
complex.__mul = function (Z1,Z2)
   Z1,Z2 = complex._args_(Z1,Z2)
   return complex:new(Z1[1]*Z2[1]-Z1[2]*Z2[2], Z1[1]*Z2[2]+Z1[2]*Z2[1])
end

--- Z1 / Z2
--  @param Z1 Real or complex number.
--  @param Z2 Real or complex number.
--  @return Ratio of numbers.
complex.__div = function (Z1,Z2)
   Z1,Z2 = complex._args_(Z1,Z2)
   local denom = Z2[1]*Z2[1] + Z2[2]*Z2[2]
   return complex:new((Z1[1]*Z2[1]+Z1[2]*Z2[2])/denom, (Z1[2]*Z2[1]-Z1[1]*Z2[2])/denom)
end

--- Z1 ^ Z2
--  @param Z1 Real or complex number.
--  @param Z2 Real or complex number.
--  @return Power.
complex.__pow = function (Z1,Z2)
   Z1,Z2 = complex._args_(Z1,Z2)
   local a0, a1 = complex.abs(Z1), complex.angle(Z1)
   local k = (a0 >= 0) and  math.log(a0) or -math.log(-a0)
   local abs = a0^(Z2[1])*math.exp(-a1*Z2[2])
   local arg = k*Z2[2]+Z2[1]*a1
   return complex:new(abs*math.cos(arg), abs*math.sin(arg))
end

--- -Z
--  @param Z Complex number.
--  @return Negative value.
complex.__unm = function (Z) return complex:new(-Z[1], -Z[2]) end

complex.arithmetic = 'arithmetic'
complex.about[complex.arithmetic] = {complex.arithmetic, "a+b, a-b, a*b, a/b, a^b, -a", help.META}

--- Z1 == Z2
--  @param Z1 Real or complex number.
--  @param Z2 Real or complex number.
--  @return True if the real and complex parts are the same.
complex.__eq = function (Z1, Z2)
   Z1,Z2 = complex._args_(Z1,Z2)
   return Z1[1] == Z2[1] and Z1[2] == Z2[2]
end

complex.comparison = 'comparison'
complex.about[complex.comparison] = {complex.comparison, "a==b, a~=b", help.META}

--- Argument of complex number.
--  @param Z Complex number.
--  @return Argument of the number.
complex.angle = function (Z) return Ver.atan2(Z[2], Z[1]) end
complex.about[complex.angle] = {"angle(Z)", "Return argument of complex number."}

--- Module of complex number.
--  @param Z Complex number.
--  @return Module of the number.
complex.abs = function (Z) return math.sqrt(Z[1]*Z[1]+Z[2]*Z[2]) end
complex.about[complex.abs] = {"abs(Z)", "Return module of complex number."}

--- Conjunction.
--  @param Z Complex number.
--  @return Conjunction to the given number.
complex.conj = function (Z) return complex:new(Z[1], -Z[2]) end
complex.about[complex.conj] = {"conj(Z)", "Return the complex conjugate. Equal to ~Z.", help.OTHER}
complex.__bnot = complex.conj

--- Real part of the number.
--  @param Z Complex value.
--  @return Real part.
complex.real  = function (Z) return Z[REAL] end
complex.about[complex.real] = {"real(Z)", "Return the real part.", help.OTHER}

--- Imaginary part of the number.
--  @param Z Complex value.
--  @return Imaginary part.
complex.imag  = function (Z) return Z[IMAG] end
complex.about[complex.imag] = {"imag(Z)", "Return the imaginary part.", help.OTHER}

--- String representation.
--  @param Z Complex number.
--  @return String with complex number elements.
complex.__tostring = function (Z) return string.format("%.3f%+.3fi", Z[REAL], Z[IMAG]) end

--- Square root with possibility of complex result.
--  @param Z Real or complex number.
--  @return Real or complex square root.
complex.sqrt = function (Z) 
   if type(Z) == "number" then
      return Z < 0 and complex:new(0,math.sqrt(-Z)) or math.sqrt(Z)
   else
      return complex.__pow(Z, 0.5)
   end
end
complex.about[complex.sqrt] = {"sqrt(Z)", "Return square root. Result can be real of complex.", FUNCTIONS}

--- Exponent
--  @param Z Complex number.
--  @return Complex exponent.
complex.exp = function (Z)
   local r = math.exp(Z[1])
   return complex:new(r*math.cos(Z[2]), r*math.sin(Z[2]))
end
complex.about[complex.exp] = {"exp(Z)", "Return exponent in for complex argument.", FUNCTIONS}

--- Natural logarithm
--  @param Z Real or complex number.
--  @return Real or complex logarithm.
complex.log = function (Z)
   if type(Z) == "number" then
      return Z <= 0 and complex:new(math.log(-Z),math.pi) or math.log(Z)
   else
      return complex:new(0.5*math.log(Z[1]^2+Z[2]^2), Ver.atan2(Z[2],Z[1]))
   end
end
complex.about[complex.log] = {"log(Z)", "Complex logarithm.", FUNCTIONS}

--- Sinus
--  @param Z Complex number.
--  @return Complex sinus.
complex.sin = function (Z)
   return complex:new(math.sin(Z[1])*ch(Z[2]), math.cos(Z[1])*sh(Z[2]))
end
complex.about[complex.sin] = {"sin(Z)", "Return sinus of a complex number.", FUNCTIONS}

--- Cosine
--  @param Z Complex number.
--  @return Complex cosine.
complex.cos = function (Z)
   return complex:new(math.cos(Z[1])*ch(Z[2]), -math.sin(Z[1])*sh(Z[2]))
end
complex.about[complex.cos] = {"cos(Z)", "Return cosine of a complex number.", FUNCTIONS}

--- Tangent
--  @param Z Complex number.
--  @return Complex tangent.
complex.tan = function (Z)
   local den = math.cos(2*Z[1]) + ch(2*Z[2])
   return complex:new(math.sin(2*Z[1])/den, sh(2*Z[2])/den)
end
complex.about[complex.tan] = {"tan(Z)", "Return tangent of a complex number.", FUNCTIONS}

--- Hyperbolic sinus
--  @param Z Complex number.
--  @return Complex hyperbolic sinus.
complex.sinh = function (Z) return 0.5*(complex.exp(Z)-complex.exp(-Z)) end
complex.about[complex.sinh] = {"sinh(Z)", "Return hyperbolic sinus of a complex number.", FUNCTIONS}

--- Hyperbolic cosine
--  @param Z Complex number.
--  @return Complex hyperbolic cosine.
complex.cosh = function (Z) return 0.5*(complex.exp(Z)+complex.exp(-Z)) end
complex.about[complex.cosh] = {"cosh(Z)", "Return hyperbolic cosine of a real or complex number.", FUNCTIONS}

--- Hyperbolic tangent
--  @param Z Complex number.
--  @return Complex hyperbolic tangent.
complex.tanh = function (Z) return complex.sinh(Z) / complex.cosh(Z) end
complex.about[complex.tanh] = {"tanh(Z)", "Return hyperbolic tangent of a complex number.", FUNCTIONS}

--- Inverse sine.
--  @param Z Complex number.
--  @return Complex inverse sine.
complex.asin = function (Z)  
   local j = complex._i
   return -j*complex.log(j*Z+complex.sqrt(1-Z*Z))
end
complex.about[complex.asin] = {"asin(Z)", "Complex inverse sine.", FUNCTIONS}

--- Inverse cosine.
--  @param Z Complex number.
--  @return Complex inverse cosine.
complex.acos = function (Z) return -complex._i*complex.log(Z+complex.sqrt(Z*Z-1)) end
complex.about[complex.acos] = {"acos(Z)", "Complex inverse cosine.", FUNCTIONS}

--- Inverse tangent.
--  @param Z Complex number.
--  @return Complex inverse tangent.
complex.atan = function (Z)
   local j = complex._i
   return -0.5*j*complex.log((1+j*Z)/(1-j*Z))
end
complex.about[complex.atan] = {"atan(Z)", "Complex inverse tangent.", FUNCTIONS}


--- Inverse hyperbolic sine.
--  @param Z Complex number.
--  @return Complex inverse hyperbolic sine.
complex.asinh = function (Z) return complex.log(Z+complex.sqrt(Z*Z+1)) end
complex.about[complex.asinh] = {"asinh(Z)", "Complex inverse hyperbolic sine.", FUNCTIONS}


--- Inverse hyperbolic cosine.
--  @param Z Complex number.
--  @return Complex inverse hyperbolic cosine.
complex.acosh = function (Z) return complex.log(Z+complex.sqrt(Z*Z-1)) end
complex.about[complex.acosh] = {"acosh(Z)", "Complex inverse hyperbolic cosine.", FUNCTIONS}


--- Inverse hyperbolic tangent.
--  @param Z Complex number.
--  @return Complex inverse hyperbolic tangent.
complex.atanh = function (Z) return 0.5*complex.log((1+Z)/(1-Z)) end
complex.about[complex.atanh] = {"atanh(Z)", "Complex inverse hyperbolic tangent.", FUNCTIONS}

-- Imaginary unit
complex._i   = complex:new(0,1)
complex.about[complex._i] = {"_i", "Complex unit.", help.CONST}

-- simplify constructor call
setmetatable(complex, {__call = function (self, re, im) return complex:new(re,im) end })
complex.Comp = 'Comp'
complex.about[complex.Comp] = {"Comp([a=0,]b)", "Create new complex number.", help.NEW}

--- Function for execution during the module import.
complex.onImport = function ()
   -- redefine functions and add complex variable _i
   -- basic
   _i = complex._i
   local _sqrt = sqrt
   sqrt = function (a) return (iscomplex(a) or type(a) == 'number') and complex.sqrt(a) or _sqrt(a) end
   local _exp = exp 
   exp = function (a) return iscomplex(a) and complex.exp(a) or _exp(a) end
   local _log = log
   log = function (a) return (iscomplex(a) or type(a) == 'number') and complex.log(a) or _log(a) end
   -- trigonometric
   local _sin = sin
   sin = function (a) return iscomplex(a) and complex.sin(a) or _sin(a) end
   local _cos = cos
   cos = function (a) return iscomplex(a) and complex.cos(a) or _cos(a) end
   local _tan = tan
   tan = function (a) return iscomplex(a) and complex.tan(a) or _tan(a) end
   local _asin = asin 
   asin = function (a) return iscomplex(a) and complex.asin(a) or _asin(a) end
   local _acos = acos
   acos = function (a) return iscomplex(a) and complex.acos(a) or _acos(a) end
   local _atan = atan 
   atan = function (a) return iscomplex(a) and complex.atan(a) or _atan(a) end
   -- hyperbolic
   local _sinh = sinh
   sinh = function (a) return iscomplex(a) and complex.sinh(a) or _sinh(a) end
   local _cosh = cosh
   cosh = function (a) return iscomplex(a) and complex.cosh(a) or _cosh(a) end
   local _tanh = tanh
   tanh = function (a) return iscomplex(a) and complex.tanh(a) or _tanh(a) end
   local _asinh = asinh 
   asinh = function (a) return iscomplex(a) and complex.asinh(a) or _asinh(a) end
   local _acosh = acosh
   acosh = function (a) return iscomplex(a) and complex.acosh(a) or _acosh(a) end
   local _atanh = atanh 
   atanh = function (a) return iscomplex(a) and complex.atanh(a) or _atanh(a) end
end

-- free memory if need
if not LC_DIALOG then complex.about = nil end

return complex

--==========================
