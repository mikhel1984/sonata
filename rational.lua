-- Support of operations with rational numbers

local rational = {}
rational.__index = rational

rational.type = 'rational'

-- description
local help = require "help"
rational.about = help:new("Computations with rational numbers")

-- check if value can be an integer
rational.isint = function (x)
   assert(type(x) == 'number', "Number is expected")
   local _,a = math.modf(x)
   return a == 0
end
rational.about[rational.isint] = {"isint(v)", "Check if the number has fractional part.", help.OTHER}

-- greatest common devisor
rational.gcd = function (a,b)
   return (a == 0) and b or rational.gcd(b % a, a)
end
rational.about[rational.gcd] = {"gcd(a,b)", "Calculate the greatest common devisor for two integers.", help.OTHER}

-- constructor
function rational:new(n, dn)
   dn = dn or 1
   assert(rational.isint(n) and rational.isint(dn), "Natural numbers are expected!") 
   assert(dn ~= 0, "Denomerator is zero!")
   local g = rational.gcd(n,dn)
   local o = {num=n/g, denom=dn/g}   
   setmetatable(o, self)
   return o
end

local function args(a,b)
   a = (type(a) == "table" and a.type == rational.type) and a or rational:new(a)
   if b then
      b = (type(b) == "table" and b.type == rational.type) and b or rational:new(b)
   end
   return a,b
end

-- a + b
rational.__add = function (a, b)   
   a,b = args(a,b)
   return rational:new(a.num*b.denom+a.denom*b.num, a.denom*b.denom)
end

-- a - b
rational.__sub = function (a, b)   
   a,b = args(a,b)
   return rational:new(a.num*b.denom-a.denom*b.num, a.denom*b.denom)
end

-- a * b
rational.__mul = function (a, b)
   a,b = args(a,b)
   return rational:new(a.num*b.num, a.denom*b.denom)
end

-- a / b
rational.__div = function (a, b)
   a,b = args(a,b)
   return rational:new(a.num*b.denom, a.denom*b.num)
end

-- -v
rational.__unm = function (v)
   return rational:new(-a.num, a.denom)
end

-- a^b
rational.__pow = function (a, b)
   b = (type(b) == "number") and b or (b.num/b.denom)  -- to float point
   if type(a) == "number" then
      return math.pow(a, b)
   else
      assert(rational.isint(b) and b >= 0, "Power must be a nonnegative integer")
      return rational:new(math.pow(a.num, b), math.pow(a.denom, b)) 
   end
end

rational.about["arithmetic"] = {"arithmetic", "a+b, a-b, a*b, a/b, -a, a^b}", help.BASE}

--[=[
rational.pow = rational.__pow
rational.about[rational.pow] = [[ : pow(a,b) Get a power b. If a is rational, b must be nonnegative integer.  ]]
]=]

-- a == b
rational.__eq = function (a,b)
   a,b = args(a,b)
   return a.num == b.num and a.denom == b.denom
end

-- a < b
rational.__lt = function (a,b)
   a,b = args(a,b)
   return (a.num*b.denom) < (b.num*a.denom)
end

-- a <= b
rational.__le = function (a,b)
   a,b = args(a,b)
   return (a.num*b.denom) <= (b.num*a.denom)
end

rational.about["compare"] = {"comparation", "a<b, a<=b, a>b, a>=b, a==b, a~=b", help.BASE}

-- representation
rational.__tostring = function (v)
   return string.format("%d/%d", v.num, v.denom)
end

-- to float point
rational.decimal = function (v) return v.num / v.denom end
rational.about[rational.decimal] = {"decimal(v)", "Return rational number as decimal.", help.OTHER}

rational.Nu = function (v) return v.num end
rational.about[rational.Nu] = {"Nu(v)", "Return the numerator of rational number.", help.OTHER}

rational.De = function (v) return v.denom end
rational.about[rational.De] = {"De(v)", "Return the denominator of the rational number.", help.OTHER}

-- simplify constructor call
setmetatable(rational, {__call = function (self, n, d) return rational:new(n,d) end})
rational.about[help.NEW] = {"Rat(m [,n])", "Create rational number using num (and denom).", help.NEW}

return rational

