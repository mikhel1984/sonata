--[[      liblc/complex.lua 

--- Manipulations with complex numbers.
--  @author Stanislav Mikhel, 2017
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection.

            module 'complex'
--]]

---------------- Tests --------------
--[[!!
Cmp = require 'liblc.complex'

a = Cmp(1,2)                  
b = Cmp(3)
ans = b                        --> Cmp(3,0)

j = Cmp._i
ans = 3+4*j                    --> Cmp(3,4)

ans = Cmp.trig(2,0)            --> Cmp(2,0)

ans = a + b                    --> Cmp(4,2)

ans = Cmp(3) - b               --> Cmp(0)

ans = a * b                    --> Cmp(3,6)

ans = a / Cmp._i               --> Cmp(2,-1)

c = Cmp(1,1)^Cmp(2,-2)
ans = c:Re()                   --~ 6.147

ans = c:Im()                   --~ 7.4

ans = (a == b)                 --> false

ans = (a ~= b)                 --> true

ans = a:abs()                  --~ 2.236

ans = a:arg()                  --~ 1.107

ans = a:conj()                 --> Cmp(1,-2)

d = Cmp.sqrt(-2)
ans = d:Im()                   --~ 1.414

ans = a:copy()                 --> a

print(a)
]]

-------------------------------------------- 
-- @class table
-- @name complex
-- @field type Define object type string.
-- @field about Function description collection.
-- @field _i Complex unit.

local complex = {}
complex.__index = complex
-- mark object
complex.type = 'complex'
complex.iscomplex = true
-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
complex.about = help:new("Manipulations with complex numbers.")

--- Create new object, set metatable.
--    @param re Real part.
--    @param im Imaginary part.
--    @return Complex object.
function complex:new(re, im)   
   im = im or 0
   local o = {real = re, imag = im}
   setmetatable(o, self)
   return o
end

--- Create complex number from trigonometric representation.
--    @param m Module.
--    @param a Argument.
--    @return Complex number.
complex.trig = function (m,a)
   return complex:new(m*math.cos(a), m*math.sin(a))
end
complex.about[complex.trig] = {"trig(module,angle)", "Create complex number using module and angle.", help.BASE}

--- Create copy of the complex number.
--    @param c Source value.
--    @return Complex number.
complex.copy = function (c)
   return complex:new(c.real, c.imag)
end
complex.about[complex.copy] = {"copy(c)", "Create copy of the complex number.", help.OTHER}

--- Check object type.
--    <i>Private function.</i>
--    @param c Object for checking.
--    @return <code>true</code> if table is a complex number.
local function iscomplex(c) return type(c) == 'table' and c.iscomplex end

--- Argument type correction.
--    <i>Private function.</i>
--    @param a First complex or real number.
--    @param b Second complex, real number or <code>nil</code>.
--    @return Complex numbers.
local function args(a,b)
   a = iscomplex(a) and a or complex:new(a)
   if b then
      b = iscomplex(b) and b or complex:new(b)
   end
   return a,b
end

--- a + b
--    @param a First complex or real number.
--    @param b Second complex or real number.
--    @return Complex sum.
complex.__add = function (a,b)
   a,b = args(a,b)
   return complex:new(a.real+b.real, a.imag+b.imag)
end

--- a - b
--    @param a First complex or real number.
--    @param b Second complex or real number.
--    @return Complex difference.
complex.__sub = function (a,b)
   a,b = args(a,b)
   return complex:new(a.real-b.real, a.imag-b.imag)
end

--- a * b
--    @param a First complex or real number.
--    @param b Second complex or real number.
--    @return Complex product.
complex.__mul = function (a,b)
   a,b = args(a,b)
   return complex:new(a.real*b.real-a.imag*b.imag, a.real*b.imag+a.imag*b.real)
end

--- a / b
--    @param a First complex or real number.
--    @param b Second complex or real number.
--    @return Complex ratio.
complex.__div = function (a,b)
   a,b = args(a,b)
   local denom = b.real*b.real + b.imag*b.imag
   assert(denom ~= 0, "Denominator is zero!")
   return complex:new((a.real*b.real+a.imag*b.imag)/denom, (a.imag*b.real-a.real*b.imag)/denom)
end

--- a ^ b
--    @param a First complex or real number.
--    @param b Second complex or real number.
--    @return Complex power.
complex.__pow = function (a,b)
   a,b = args(a,b)
   local a0, a1 = complex.abs(a), complex.arg(a)
   local k = (a0 >= 0) and  math.log(a0) or -math.log(-a0)
   local abs = math.pow(a0, b.real)*math.exp(-a1*b.imag)
   local arg = k*b.imag+b.real*a1
   return complex:new(abs*math.cos(arg), abs*math.sin(arg))
end

--- -v
--    @param v Complex number.
--    @return Number with inverted signs.
complex.__unm = function (v)
   return complex:new(-v.real, -v.imag)
end

complex.arithmetic = 'arithmetic'
complex.about[complex.arithmetic] = {complex.arithmetic, "a+b, a-b, a*b, a/b, a^b, -a", help.BASE}

--- a == b
--    @param a First complex or real number.
--    @param b Second complex or real number.
--    @return <code>true</code> if the real and imaginary parts are equal.
complex.__eq = function (a, b)
   a,b = args(a,b)
   return a.real == b.real and a.imag == b.imag
end

--complex.__lt = function (a,b) return false end
--complex.__le = function (a,b) return complex.__eq(a,b) end

complex.comparison = 'comparison'
complex.about[complex.comparison] = {complex.comparison, "a==b, a~=b", help.BASE}

--- Argument of complex number.
--    @param v Complex number.
--    @return Argument of the number.
complex.arg = function (v) return math.atan(v.imag, v.real) end
complex.about[complex.arg] = {"arg(v)", "Return argument of complex number.", help.BASE}

--- Module of complex number.
--    @param v Complex number.
--    @return Module of the number.
complex.abs = function (v) return math.sqrt(v.real*v.real+v.imag*v.imag) end
complex.about[complex.abs] = {"abs(v)", "Return module of complex number.", help.BASE}

--- Conjunction.
--    @param v Complex number.
--    @return Conjunction to the given number.
complex.conj = function (v) return complex:new(v.real, -v.imag) end
complex.about[complex.conj] = {"conj(v)", "Return the complex conjugate.", help.OTHER}

--- Real part of the number.
--    @param v Complex value.
--    @return Real part.
complex.Re  = function (v) return v.real end
complex.about[complex.Re] = {"Re(v)", "Return the real part.", help.OTHER}

--- Imaginary part of the number.
--    @param v Complex value.
--    @return Imaginary part.
complex.Im  = function (v) return v.imag end
complex.about[complex.Im] = {"Im(v)", "Return the imaginary part.", help.OTHER}

--- String representation.
--    @param v Complex number.
--    @return String.
complex.__tostring = function (v)
   return string.format("%.3f%+.3fi", v.real, v.imag)
end

--- Square root with possibility of complex result.
--    @param v Real or complex number.
--    @return Real or complex square root.
complex.sqrt = function (v) 
   if type(v) == "number" then
      if v >= 0 then
         return math.sqrt(v)
      else
         return complex:new(0, math.sqrt(-v))
      end
   else
      return complex.__pow(v, 0.5)
   end
end
complex.about[complex.sqrt] = {"sqrt(v)", "Return square root. Result can be real of complex.", help.BASE}

-- imaginary unit
complex._i   = complex:new(0,1)
complex.about[complex._i] = {"_i", "Complex unit.", "constant"}

-- simplify constructor call
setmetatable(complex, {__call = function (self, re, im) return complex:new(re,im) end })
complex.Cmp = 'Cmp'
complex.about[complex.Cmp] = {"Cmp(a [,b])", "Create new complex number.", help.NEW}

--- Complex number serialization.
--    @param obj Complex number.
--    @return String, suitable for exchange.
complex.serialize = function (obj)
   local s = {}
   s[#s+1] = string.format("real=%a", obj.real)
   s[#s+1] = string.format("imag=%a", obj.imag)
   s[#s+1] = "metatablename='Cmp'"
   s[#s+1] = "modulename='complex'"
   return string.format("{%s}", table.concat(s, ','))
end
complex.about[complex.serialize] = {"serialize(obj)", "Save internal representation or complex object.", help.OTHER}

--- Function for execution during the module import.
--    Redefine function sqrt and add complex variable.
complex.onimport = function ()
   _i = complex._i
   sqrt = complex.sqrt
end

-- free memory if need
if not lc_version then complex.about = nil end

return complex
