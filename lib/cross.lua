------------ Test cross-module functionality --------------
--[[TEST

-- rational and bigint
Rat = require 'lib.rational'
Int = require 'lib.bigint'

a = Rat(Int(1), Int(2))
b = Rat(Int(2))
ans = (a ~= nil) and (b ~= nil)  --> true

ans = (a + b):val()              --> 5/2

-- compare with default type
ans = a + b                      --> Rat(5,2)

-- operation with default type
c = Rat(1,3)
ans = a + c                      --> Rat(5,6)

ans = 2 * a                      --> Int(1)




--]]
