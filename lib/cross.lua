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

ans = (a + b):float()            --> 5/2

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

-- rat & int 
ans = Rat(1,2) + Int(1)            --> Rat(3,2)

-- int & rat
ans = Int(1) - Rat(1,2)            --> Rat(1,2)


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

-- show
print(a:str())

-- poly & int
ans = Poly{1,2,3} * Int(2)       --> Poly{Int(2),Int(4),Int(6)}

-- int & poly
ans = Int(1) - Poly{1,0}         --> Poly{-1,1}


-- POLYNOMIAL and RATIONAL 
-----------------------------

a = Poly{Rat(1,2),Rat(1,3),Rat(1,4)}
b = Poly{Rat(2,3),Rat(4,5)} 
ans = (a ~= nil) and (b ~= nil)  --> true

ans = a(1)                      --> Rat(13,12) 

ans = a + b                     --> Poly{Rat(1,2),Rat(1),Rat(21,20)}

p = a / b
q = a % b
ans = p * b + q                 --> a

ans = a:der()                   --> Poly{1, Rat(1,3)}

ans = b:int()                   --> Poly{Rat(1,3),Rat(4,5),0}

-- show 
print(a:str())


-- COMPLEX and BIGINT
------------------------
Comp = require 'lib.complex'

a = Comp(Int(1),Int(2))
b = Comp(0, Int(3))
ans = (a ~= nil) and (b ~= nil)  --> true

ans = a + b                      --> Comp(Int(1),Int(5))

ans = b - b                      --> 0

ans = a * b                      --> Comp(-6,3)

ans = a / Comp._i                --> Comp(2, -1)

ans = a:abs()                   --3> 2.236

ans = a:angle()                 --3> 1.107

ans = a:conj()                   --> Comp(Int(1),Int(-2))

ans = (a^0.5).Re                --3> 1.272

ans = Comp.log(a).Re            --3> 0.805

ans = Comp.sin(a).Re            --3> 3.165

-- show
print(a)

-- comp & big
ans = Comp(1,2) + Int(3)         --> Comp(4,2)

-- big & comp 
ans = Int(2) * Comp(1,2)         --> Comp(2,4)


-- COMPLEX and RATIONAL
------------------------

a = Comp(Rat(1,2),Rat(2,3))
b = Comp(0, Rat(1,2))
ans = (a ~= nil) and (b ~= nil) --> true

ans = a + b                     --> Comp(Rat(1,2),Rat(7,6))

ans = a / Comp._i               --> Comp(Rat(2,3),Rat(-1,2))

ans = b:abs()                   --> 0.5

ans = a:angle()                --3> 0.927

ans = Comp.cos(a).Im           --3> -0.343

ans = (a^0.3).Re               --2> 0.91

-- show 
print(a)

-- QUATERNION and BIGINT
------------------------
Quat = require 'lib.quaternion'

a = Quat{Int(1),Int(2),Int(3),Int(4)}
b = Quat{w = Int(3), j = Int(5)}
ans = (a ~= nil) and (b ~= nil) --> true

ans = a + b                     --> Quat{4,2,8,4}

ans = a * b                     --> Quat{-12,-14,14,22}

ans = a + a:conj()              --> Int(2)

ans = a:abs()                  --3> 5.477

c = a * a:inv()
ans = c.w                       --> 1

c = a:copy()
ans = c.i                       --> Int(2)

c:normalize()
ans = c:abs()                  --3> 1.0

d = c ^ 0.5
ans = d.w                      --3> 0.768

ans = a ^ 3                     --> Quat{-86,-52,-78,-104}

-- show 
print(a)

-- quat & int
ans = a + Int(1)                --> Quat{2,2,3,4}

-- int & quat
ans = Int(2) * b                --> Quat{6,0,10,0}





--]]
