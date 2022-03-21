------------ Test cross-module functionality --------------
--[[TEST

-- RATIONAL and BIGINT
-----------------------------
Rat = require 'lib.rational'
Int = require 'lib.bigint'

-- check '%' operation
ans = Rat.gcd(Int(125), Int(65))  --> Int(5)

a = Rat(Int(1), Int(2))
b = Rat(Int(2))
ans = (a ~= nil) and (b ~= nil)  --> true

ans = (a + b):val()              --> 5/2

-- compare with default type
ans = (a == Rat(1,2))            --> true

-- operations
c = Rat(1,3)
ans = a + c                      --> Rat(5,6)

ans = 2 * a                      --> Int(1)

ans = a * b                      --> Int(1)

ans = a ^ 3                      --> Rat(1,8)

ans = 2 ^ a                     --3> 1.414

ans = (b == b)                   --> true

ans = (a >= b)                   --> false

-- copy
d = a:copy()
ans = (a:Nu().type == d:Nu().type) --> true

-- show
print(a)


-- RATIONAL and POLYNOMIAL
-----------------------------------
Poly = require 'lib.polynomial'

-- check '%' operation
ans = Rat.gcd(Poly{2,7,6}, Poly{3,10,8})  --> Poly{-0.5,-1}

a = Rat(Poly{1,2,1}, Poly{2,3})
b = Rat(Poly{2,3})
ans = (a ~= nil) and (b ~= nil)  --> true

ans = a + b                      --> Rat(Poly{20,56,40},Poly{8,12})

ans = a * b                      --> Poly{1,2,1}

ans = a + 1                      --> Rat(Poly{1,4,4},Poly{2,3})

a = Rat(Poly{1,1},Poly{1,2}) 
ans = a ^ 2                      --> a * a

d = a:copy() 
ans = (a:Nu().type == d:Nu().type)  --> true

a = Poly{1,2,3}
ans = Rat(5*a, 5)                   --> a


-- POLYNOMIAL and BIGINT
----------------------------------

a = Poly{Int(1),Int(2),Int(3)}
b = Poly{Int(1),Int(1)}
ans = (a ~= nil) and (b ~= nil)  --> true

ans = a(1)                       --> Int(6)

ans = a[1]                       --> Int(2)

ans = a + b                      --> Poly{Int(1),Int(3),Int(4)}

ans = a - b                      --> Poly{Int(1),Int(1),Int(2)}

ans = b * b                      --> Poly{Int(1),Int(2),Int(1)}

ans = a / b                      --> Poly{Int(1),Int(1)}

ans = a % b                      --> Int(2)

ans = b ^ 2                      --> b * b

ans = a:der()                    --> Poly{Int(2),Int(2)}

ans = Poly.build(Int(1),Int(1))  --> Poly{Int(1),Int(-2),Int(1)}










--]]
