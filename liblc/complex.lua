--[[      liblc/complex.lua 

--- Manipulations with complex numbers.
--  @author <a href="mailto:vpsys@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

            module 'complex'
--]]

---------------- Tests --------------
--[[!!
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

-------------------------------------------- 
-- @class table
-- @name complex
-- @field type Define object type string.
-- @field about Description of functions.
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
complex.about[complex.trig] = {"trig(module,angle)", "Create complex number using module and angle.", }

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
complex.about[complex.arithmetic] = {complex.arithmetic, "a+b, a-b, a*b, a/b, a^b, -a", }

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
complex.about[complex.comparison] = {complex.comparison, "a==b, a~=b", }

--- Argument of complex number.
--    @param v Complex number.
--    @return Argument of the number.
complex.arg = function (v) return math.atan(v.imag, v.real) end
complex.about[complex.arg] = {"arg(v)", "Return argument of complex number.", }

--- Module of complex number.
--    @param v Complex number.
--    @return Module of the number.
complex.abs = function (v) return math.sqrt(v.real*v.real+v.imag*v.imag) end
complex.about[complex.abs] = {"abs(v)", "Return module of complex number.", }

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
complex.about[complex.sqrt] = {"sqrt(v)", "Return square root. Result can be real of complex.", }

-- imaginary unit
complex._i   = complex:new(0,1)
complex.about[complex._i] = {"_i", "Complex unit.", "constant"}

-- simplify constructor call
setmetatable(complex, {__call = function (self, re, im) return complex:new(re,im) end })
complex.Comp = 'Comp'
complex.about[complex.Comp] = {"Comp(a[,b])", "Create new complex number.", help.NEW}

--- Complex number serialization.
--    @param obj Complex number.
--    @return String, suitable for exchange.
complex.serialize = function (obj)
   local s = {}
   s[#s+1] = string.format("real=%a", obj.real)
   s[#s+1] = string.format("imag=%a", obj.imag)
   s[#s+1] = "metatablename='Comp'"
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
