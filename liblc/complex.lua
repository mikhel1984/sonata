------------  complex.lua ----------------
--
-- Manipulations with complex numbers.
--
-- This file is a part of liblc collection. 
-- Stanislav Mikhel, 2017.
----------------------------------------

---------------- Tests --------------

--[[!!
Cmp = require 'liblc.complex'
a = Cmp(1,2)                  
b = Cmp(3)
ans = b                        --> Cmp(3,0)

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
]]

local complex = {}
complex.__index = complex

complex.type = 'complex'

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
complex.about = help:new("Manipulations with complex numbers")

-- constructor
function complex:new(re, im)   
   im = im or 0
   assert(type(re) == 'number' and type(im) == 'number', "Numbers are expected")
   local o = {real = re, imag = im}
   setmetatable(o, self)
   return o
end

-- argument type correction
local function args(a,b)
   a = (type(a) == "table" and a.type == complex.type) and a or complex:new(a)
   if b then
      b = (type(b) == "table" and b.type == complex.type) and b or complex:new(b)
   end
   return a,b
end

-- a + b
complex.__add = function (a,b)
   a,b = args(a,b)
   return complex:new(a.real+b.real, a.imag+b.imag)
end

-- a - b
complex.__sub = function (a,b)
   a,b = args(a,b)
   return complex:new(a.real-b.real, a.imag-b.imag)
end

-- a * b
complex.__mul = function (a,b)
   a,b = args(a,b)
   return complex:new(a.real*b.real-a.imag*b.imag, a.real*b.imag+a.imag*b.real)
end

-- a / b
complex.__div = function (a,b)
   a,b = args(a,b)
   local denom = b.real*b.real + b.imag*b.imag
   assert(denom ~= 0, "Denomerator is zero!")
   return complex:new((a.real*b.real+a.imag*b.imag)/denom, (a.imag*b.real-a.real*b.imag)/denom)
end

-- a ^ b
complex.__pow = function (a,b)
   a,b = args(a,b)
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

complex.arithmetic = 'arithmetic'
complex.about[complex.arithmetic] = {complex.arithmetic, "a+b, a-b, a*b, a/b, a^b, -a", help.BASE}

-- a == b
complex.__eq = function (a, b)
   a,b = args(a,b)
   return a.real == b.real and a.imag == b.imag
end

complex.comparation = 'comparation'
complex.about[complex.comparation] = {complex.comparation, "a==b, a~=b", help.BASE}

-- argument of complex number
complex.arg = function (v) return math.atan(v.imag, v.real) end
complex.about[complex.arg] = {"arg(v)", "Return argument of complex number.", help.BASE}

-- module of complex number
complex.abs = function (v) return math.sqrt(v.real*v.real+v.imag*v.imag) end
complex.about[complex.abs] = {"abs(v)", "Return module of complex number.", help.BASE}

-- conjunction
complex.conj = function (v) return complex:new(v.real, -v.imag) end
complex.about[complex.conj] = {"conj(v)", "Return the complex conjugate.", help.OTHER}

-- real part
complex.Re  = function (v) return v.real end
complex.about[complex.Re] = {"Re(v)", "Return the real part.", help.OTHER}

-- imag part
complex.Im  = function (v) return v.imag end
complex.about[complex.Im] = {"Im(v)", "Return the imaginary part.", help.OTHER}

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
complex.about[complex.sqrt] = {"sqrt(v)", "Return square root. Result can be real of complex.", help.BASE}

-- imaginary unit
complex._i   = complex:new(0,1)
complex.about[complex._i] = {"_i", "Complex unit", "constant"}

-- simplify constructor call
setmetatable(complex, {__call = function (self, re, im) return complex:new(re,im) end })
complex.Cmp = 'Cmp'
complex.about[complex.Cmp] = {"Cmp(a [,b])", "Create new complex number", help.NEW}

-- object serialization
complex.serialize = function (obj)
   local s = {}
   s[#s+1] = string.format("real=%a", obj.real)
   s[#s+1] = string.format("imag=%a", obj.imag)
   s[#s+1] = "metatablename='Cmp'"
   s[#s+1] = "modulename='complex'"
   return string.format("{%s}", table.concat(s, ','))
end
complex.about[complex.serialize] = {"serialize(obj)", "Save internal representation or complex object", help.OTHER}

return complex
