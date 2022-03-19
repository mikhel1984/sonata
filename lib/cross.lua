------------ Test cross-module functionality --------------
--[[TEST

-- RATIONAL and BIGINT
Rat = require 'lib.rational'
Int = require 'lib.bigint'

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




--]]
