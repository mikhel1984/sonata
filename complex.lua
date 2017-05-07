-- Manipulations with complex numbers

local complex = {}
complex.__index = complex

-- description
complex.about = {}

-- constructor
function complex:new(re, im)   
   im = im or 0
   local o = {real = re, imag = im}
   setmetatable(o, self)
   return o
end
complex.about[complex.new] = [[
   : complex:new(re [,im])
Create a complex number. If im == 0 it can be omited.
]]

-- a + b
complex.__add = function (a,b)
   a = (type(a) == "number") and complex:new(a) or a
   b = (type(b) == "number") and complex:new(b) or b
   return complex:new(a.real+b.real, a.imag+b.imag)
end

-- a - b
complex.__sub = function (a,b)
   a = (type(a) == "number") and complex:new(a) or a
   b = (type(b) == "number") and complex:new(b) or b
   return complex:new(a.real-b.real, a.imag-b.imag)
end

-- a * b
complex.__mul = function (a,b)
   a = (type(a) == "number") and complex:new(a) or a
   b = (type(b) == "number") and complex:new(b) or b
   return complex:new(a.real*b.real-a.imag*b.imag, a.real*b.imag+a.imag*b.real)
end

-- a / b
complex.__div = function (a,b)
   a = (type(a) == "number") and complex:new(a) or a
   b = (type(b) == "number") and complex:new(b) or b
   local denom = b.real*b.real + b.imag*b.imag
   assert(denom ~= 0, "Denomerator is zero!")
   return complex:new((a.real*b.real+a.imag*b.imag)/denom, (a.imag*b.real-a.real*b.imag)/denom)
end

-- a ^ b
complex.__pow = function (a,b)
   a = (type(a) == "number") and complex:new(a) or a
   b = (type(b) == "number") and complex:new(b) or b
   local a0 = complex.abs(a)
   local a1 = complex.arg(a)
   local k = (a0 >= 0) and  math.log(a0) or -math.log(-a0)
   local abs = math.pow(a0, b.real)*math.exp(-a1*b.imag)
   local arg = k*b.imag+b.real*a1
   return complex:new(abs*math.cos(arg), abs*math.sin(arg))
end

-- -v
complex.__unm = function (v)
   return complex:new(-v.real, -v.imag)
end

-- a == b
complex.__eq = function (a, b)
   a = (type(a) == "number") and complex:new(a) or a
   b = (type(b) == "number") and complex:new(b) or b
   return a.real == b.real and a.imag == b.imag
end

-- argument of complex number
complex.arg = function (v) return math.atan(v.imag, v.real) end
complex.about[complex.arg] = [[
   : arg(v)
Return argument of complex number.
]]

-- module of complex number
complex.abs = function (v) return math.sqrt(v.real*v.real+v.imag*v.imag) end
complex.about[complex.abs] = [[
   : abs(v)
Return module of complex number.
]]

-- real part
complex.Re  = function (v) return v.real end
complex.about[complex.Re] = [[
   : Re(v)
Return the real part.
]]

-- imag part
complex.Im  = function (v) return v.imag end
complex.about[complex.Im] = [[
   : Im(v)
Return the imaginary part.
]]

-- number representation
complex.__tostring = function (v)
   return string.format("%f%+fi", v.real, v.imag)
end

-- square root with possible complex result
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
complex.about[complex.sqrt] = [[
   : sqrt(v)
Return square root. Result can be real of complex.
]]

-- imaginary unit
complex._i   = complex:new(0,1)

complex.about.complex = [[
Module for complex number computations
   : Base
+, -, *, /, ^, ==
   : Constructor
new(real [,imag])
   : Complex
arg(v), abs(v), Re(v), Im(v), sqrt(v)
   : Constants
_i
]]

return complex