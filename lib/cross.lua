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

ans = (a^0.5):re()              --3> 1.272

ans = Comp.log(a):re()          --3> 0.805

ans = Comp.sin(a):re()          --3> 3.165

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

ans = Comp.cos(a):im()         --3> -0.343

ans = (a^0.3):re()             --2> 0.91

-- show 
print(a)

-- QUATERNION and BIGINT
------------------------
Quat = require 'lib.quaternion'

a = Quat{Int(1),Int(2),Int(3),Int(4)}
b = Quat{w = Int(3), y = Int(5)}
ans = (a ~= nil) and (b ~= nil) --> true

ans = a + b                     --> Quat{4,2,8,4}

ans = a * b                     --> Quat{-12,-14,14,22}

ans = a + a:conj()              --> Int(2)

ans = a:abs()                  --3> 5.477

c = a * a:inv()
ans = c:w()                     --> 1

c = a:normalize()
ans = c:abs()                  --3> 1.0

d = c ^ 0.5
ans = d:w()                    --3> 0.768

ans = a ^ 3                     --> Quat{-86,-52,-78,-104}

-- show 
print(a)

-- quat & int
ans = a + Int(1)                --> Quat{2,2,3,4}

-- int & quat
ans = Int(2) * b                --> Quat{6,0,10,0}


-- MATRIX and COMPLEX
---------------------
Mat = require 'lib.matrix'

-- prepare
a = Mat{{1, Comp(2,3)},{Comp(4,5), 6}}
b = Mat{{Comp(9,8), Comp(7,6)},{5, 4}}
j = Comp._i

-- transpose
c = a:T()
ans = c[2][1]                --> Comp(2,3)

-- sum
tmp = a + b
ans = tmp[1][1]              --> Comp(10,8)

-- add complex
tmp = a + j
ans = tmp[1][1]              --> Comp(1,1)

-- product
tmp = a * b
ans = tmp[1][1]              --> b[1][1] + a[1][2]*b[2][1]

-- multipy complex
tmp = b * j
ans = tmp[2][1]              --> Comp(0,5)

-- determinant
d = Mat {
  {1, 2*j, 3, -4+5*j},
  {7+8*j, 9, 10*j, 11},
  {12-13*j, 14*j, 15, -16*j},
  {17, 18, 19+20*j, 21}
}
tmp = d:det()
ans = tmp:re()              --1> -29932.0

-- inverse matrix
d2 = d:inv()
tmp = d * d2
ans = tmp[1][1]             --3> 1.0

-- ratio
tmp = a / b
ans = tmp[1][1]:im()        --1> -0.6

-- try to solve equation
tmp = Mat.rref(a .. Mat.V{4,5})
ans = tmp[1][2]             --3> 0.0

-- pseudoinverse
c = Mat{{1,2*j,3},{4*j,5,6*j}}
cc = c:pinv()
tmp = c * cc
ans = tmp[2][2]             --3> 1.0

-- LU
l,u,p = a:lu()
ans = u[2][1]               --3> 0.0

-- Cholesky
m = Mat{{3,j},{j,3}}
mm = m:chol()
tmp = mm * mm:T()
ans = tmp[1][1]             --3> m[1][1]

-- trace
ans = b:tr()                 --> Comp(13,8)

-- rank
ans = a:rank()               --> 2

-- MATRIX and RATIONAL
---------------------

-- prepare
a = Mat{{1, Rat(2,3)},{Rat(4,5), 6}}
b = Mat{{Rat(1,2), Rat(1,3)},{5, 4}}

-- transpose
c = a:T()
ans = c[2][1]                --> Rat(2,3)

-- sum
tmp = a + b
ans = tmp[1][1]              --> a[1][1] + b[1][1]

-- add rational
tmp = a + Rat(1,3)
ans = tmp[1][2]              --> 1

-- product
tmp = a * b
ans = tmp[1][1]              --> b[1][1] + a[1][2]*b[2][1]

-- multipy rational
tmp = b * Rat(3,2)
ans = tmp[1][2]              --> Rat(1,2)

-- determinant
d = Mat {
  {Rat(1,2), 2, 3, Rat(1,2)},
  {Rat(1,3), 9, 10, 11},
  {-3, Rat(1,2), 15, 16},
  {17, 18, Rat(1,3), 21}
}
tmp = d:det()
ans = tmp:denom()            --> 18

-- inverse matrix
d2 = d:inv()
tmp = d * d2
ans = tmp[2][2]             --2> 1.0

-- ratio
tmp = a / b
ans = tmp[1][1]             --1> 2.0

-- try to solve equation
tmp = Mat.rref(a .. Mat.V{4,5})
ans = tmp[1][2]             --3> 0.0

-- pseudoinverse
c = Mat{{1,Rat(1,2),3},{Rat(1,2),5,Rat(1,3)}}
cc = c:pinv()
tmp = c * cc
ans = tmp[2][2]             --3> 1.0

-- LU
l,u,p = a:lu()
ans = u[2][1]               --3> 0.0

-- Cholesky
m = Mat{{3,Rat(1,2)},{Rat(1,3),3}}
mm = m:chol()
tmp = mm * mm:T()
ans = tmp[1][1]             --3> m[1][1]

-- trace
ans = b:tr()                 --> Rat(9,2)

-- rank
ans = a:rank()               --> 2


--]]
