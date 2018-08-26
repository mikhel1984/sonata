--[[      liblc/complex.lua 

--- Manipulations with complex numbers.
--  
--  Object structure                </br>
--  <code> {REAL, IMAGINARY} </code></br>
--  i.e. complex number is a table which consists of two elements.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

            module 'complex'
--]]

---------------- Tests --------------
--[[!!
-- import 'complex'
Comp = require 'liblc.complex'

-- real and imag pars
a = Comp(1,2)
-- or just real                  
b = Comp(3)
ans = b                        --> Comp(3,0)

-- imaginary unit
j = Comp._i
ans = 3+4*j                    --> Comp(3,4)

-- use trigonometrical form
ans = Comp.trig(2,0)           --> Comp(2,0)

-- arithmetic
ans = a + b                    --> Comp(4,2)

ans = Comp(3) - b              --> Comp(0)

ans = a * b                    --> Comp(3,6)

ans = a / Comp._i              --> Comp(2,-1)

-- power could be complex
c = Comp(1,1)^Comp(2,-2)
-- real part
ans = c:Re()                   --~ 6.147

-- imaginary part
ans = c:Im()                   --~ 7.4

-- comparison
ans = (a == b)                 --> false

ans = (a ~= b)                 --> true

-- absolute value
ans = a:abs()                  --~ 2.236

-- argument (angle)
ans = a:arg()                  --~ 1.107

-- conjugated number
ans = a:conj()                 --> Comp(1,-2)

-- complex square root
-- after import becomes default
d = Comp.sqrt(-2)
ans = d:Im()                   --~ 1.414

-- make copy
ans = a:copy()                 --> a

-- show
print(a)
]]


--	LOCAL

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

local help = lc_version and (require "liblc.help") or {new=function () return {} end}

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
complex.new = function (self, re, im)   
   return setmetatable({re, im or 0}, self)
end

--- Create complex number from trigonometric representation.
--  @param m Module.
--  @param a Argument.
--  @return Complex number.
complex.trig = function (m,a)
   return complex:new(m*math.cos(a), m*math.sin(a))
end
complex.about[complex.trig] = {"trig(module,angle)", "Create complex number using module and angle."}

--- Create copy of the complex number.
--  @param C Source value.
--  @return Complex number.
complex.copy = function (C)
   return complex:new(C[REAL], C[IMAG])
end
complex.about[complex.copy] = {"copy(C)", "Create copy of the complex number.", help.OTHER}

--- Correct arguments.
--  @param a Real or complex number.
--  @param b Real or complex number (optional).
--  @return Complex number(s).
complex._args = function (a,b)
   a = iscomplex(a) and a or complex:new(a)
   if b then
      b = iscomplex(b) and b or complex:new(b)
   end
   return a,b
end

--- C1 + C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Sum of numbers.
complex.__add = function (C1,C2)
   C1,C2 = complex._args(C1,C2)
   return complex:new(C1[1]+C2[1], C1[2]+C2[2])
end

--- C1 - C2 
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Difference of numbers.
complex.__sub = function (C1,C2)
   C1,C2 = complex._args(C1,C2)
   return complex:new(C1[1]-C2[1], C1[2]-C2[2])
end

--- C1 * C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Product of numbers.
complex.__mul = function (C1,C2)
   C1,C2 = complex._args(C1,C2)
   return complex:new(C1[1]*C2[1]-C1[2]*C2[2], C1[1]*C2[2]+C1[2]*C2[1])
end

--- C1 / C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Ratio of numbers.
complex.__div = function (C1,C2)
   C1,C2 = complex._args(C1,C2)
   local denom = C2[1]*C2[1] + C2[2]*C2[2]
   return complex:new((C1[1]*C2[1]+C1[2]*C2[2])/denom, (C1[2]*C2[1]-C1[1]*C2[2])/denom)
end

--- C1 ^ C2
--  @param C1 Real or complex number.
--  @param C2 Real or complex number.
--  @return Power.
complex.__pow = function (C1,C2)
   C1,C2 = complex._args(C1,C2)
   local a0, a1 = complex.abs(C1), complex.arg(C1)
   local k = (a0 >= 0) and  math.log(a0) or -math.log(-a0)
   local abs = a0^(C2[1])*math.exp(-a1*C2[2])
   local arg = k*C2[2]+C2[1]*a1
   return complex:new(abs*math.cos(arg), abs*math.sin(arg))
end

--- -C
--  @param C Complex number.
--  @return Negative value.
complex.__unm = function (C)
   return complex:new(-C[1], -C[2])
end

complex.arithmetic = 'arithmetic'
complex.about[complex.arithmetic] = {complex.arithmetic, "a+b, a-b, a*b, a/b, a^b, -a", help.META}

-- a == b
complex.__eq = function (a, b)
   a,b = complex._args(a,b)
   return a[1] == b[1] and a[2] == b[2]
end

complex.comparison = 'comparison'
complex.about[complex.comparison] = {complex.comparison, "a==b, a~=b", help.META}

--- Argument of complex number.
--    @param v Complex number.
--    @return Argument of the number.
complex.arg = function (v) return math.atan(v[2], v[1]) end
complex.about[complex.arg] = {"arg(v)", "Return argument of complex number."}

--- Module of complex number.
--    @param v Complex number.
--    @return Module of the number.
complex.abs = function (v) return math.sqrt(v[1]*v[1]+v[2]*v[2]) end
complex.about[complex.abs] = {"abs(v)", "Return module of complex number."}

--- Conjunction.
--    @param v Complex number.
--    @return Conjunction to the given number.
complex.conj = function (v) return complex:new(v[1], -v[2]) end
complex.about[complex.conj] = {"conj(v)", "Return the complex conjugate.", help.OTHER}

--- Real part of the number.
--    @param v Complex value.
--    @return Real part.
complex.Re  = function (v) return v[REAL] end
complex.about[complex.Re] = {"Re(v)", "Return the real part.", help.OTHER}

--- Imaginary part of the number.
--    @param v Complex value.
--    @return Imaginary part.
complex.Im  = function (v) return v[IMAG] end
complex.about[complex.Im] = {"Im(v)", "Return the imaginary part.", help.OTHER}

-- String representation.
complex.__tostring = function (v)
   return string.format("%.3f%+.3fi", v[REAL], v[IMAG])
end

--- Square root with possibility of complex result.
--    @param v Real or complex number.
--    @return Real or complex square root.
complex.sqrt = function (v) 
   if type(v) == "number" then
      return v < 0 and complex:new(0,math.sqrt(-v)) or math.sqrt(v)
   else
      return complex.__pow(v, 0.5)
   end
end
complex.about[complex.sqrt] = {"sqrt(z)", "Return square root. Result can be real of complex.", FUNCTIONS}

-- Exponent
complex.exp = function (v)
   if iscomplex(v) then
      local r = math.exp(v[1])
      return complex:new(r*math.cos(v[2]), r*math.sin(v[2]))
   else
      return math.exp(v)
   end
end
complex.about[complex.exp] = {"exp(z)", "Return expenent in real or complex power.", FUNCTIONS}

complex.log = function (z)
   if type(v) == "number" then
      return z <= 0 and complex:new(math.log(-z),math.pi) or math.log(z)
   else
      return complex:new(0.5*math.log(z[1]^2+z[2]^2), math.atan(z[2],z[1]))
   end
end
complex.about[complex.log] = {"log(z)", "Complex logarithm.", FUNCTIONS}

-- sinus
complex.sin = function (v)
   if iscomplex(v) then
      return complex:new(math.sin(v[1])*ch(v[2]), math.cos(v[1])*sh(v[2]))
   else
      return math.sin(v)
   end
end
complex.about[complex.sin] = {"sin(z)", "Return sinus of real or complex number.", FUNCTIONS}

-- cosinus
complex.cos = function (z)
   if iscomplex(z) then
      return complex:new(math.cos(z[1])*ch(z[2]), -math.sin(z[1])*sh(z[2]))
   else
      return math.cos(z)
   end
end
complex.about[complex.cos] = {"cos(z)", "Return cosine of real or complex number.", FUNCTIONS}

-- tangent
complex.tan = function (z)
   if iscomplex(z) then
      local den = math.cos(2*z[1]) + ch(2*z[2])
      return complex:new(math.sin(2*z[1])/den, sh(2*z[2])/den)
   else
      return math.tan(z)
   end
end
complex.about[complex.tan] = {"tan(z)", "Return tangent of real or complex number.", FUNCTIONS}

-- hyperbolic
complex.sinh = function (z)
      return 0.5*(complex.exp(z)-complex.exp(-z))
end
complex.about[complex.sinh] = {"sinh(z)", "Return hyperbolic sinus of a real or complex number.", FUNCTIONS}

complex.cosh = function (z)
      return 0.5*(complex.exp(z)+complex.exp(-z))
end
complex.about[complex.cosh] = {"cosh(z)", "Return hyperbolic cosine of a real or complex nubmer.", FUNCTIONS}

complex.tanh = function (z)
   return complex.sinh(z) / complex.cosh(z)
end
complex.about[complex.tanh] = {"tanh(z)", "Return hyperbolic tangent of a real or complex number.", FUNCTIONS}

-- imaginary unit
complex._i   = complex:new(0,1)
complex.about[complex._i] = {"_i", "Complex unit.", help.CONST}

-- simplify constructor call
setmetatable(complex, {__call = function (self, re, im) return complex:new(re,im) end })
complex.Comp = 'Comp'
complex.about[complex.Comp] = {"Comp(a[,b])", "Create new complex number.", help.NEW}

--- Complex number serialization.
--    @param obj Complex number.
--    @return String, suitable for exchange.
complex.serialize = function (obj)
   local s = {}
   s[#s+1] = tostring(obj[REAL])
   s[#s+1] = tostring(obj[IMAG])
   s[#s+1] = "metatablename='Comp'"
   s[#s+1] = "modulename='complex'"
   return string.format("{%s}", table.concat(s, ','))
end
complex.about[complex.serialize] = {"serialize(obj)", "Save internal representation or complex object.", help.OTHER}

-- Function for execution during the module import.
complex.onImport = function ()
   -- redefine functions and add complex variable _i
   _i = complex._i
   sqrt = complex.sqrt
   exp = complex.exp
   log  = complex.log
   sin = complex.sin
   cos = complex.cos
   tan = complex.tan
   sh  = complex.sh
   ch  = complex.ch
   th  = complex.th
end

-- free memory if need
if not lc_version then complex.about = nil end

return complex

--==========================
-- TODO: define inverse trigonometric complex functions
