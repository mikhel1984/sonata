
local matrix = {}
matrix.__index = matrix

matrix.type = 'matrix'

function matrix:init(r, c, m)
   m = m or {}
   m.cols, m.rows = c, r
   setmetatable(m, self)
   return m
end

local function checkindex(m, r, c)
   assert(m.type == matrix.type, "Matrix is expected")
   assert(c < m.cols and c > -m.cols, "Column number must be less then " .. m.cols)
   assert(r < m.rows and r > -m.rows, "Row number must be less then " .. m.rows)
   r = (r < 0) and (m.rows+r+1) or (r+1)
   c = (c < 0) and (m.cols+c+1) or (c+1)
   return r, c
end

matrix.get = function (m, r, c)
   r, c = checkindex(m, r, c)
   local v = m[r]
   return (v and v[c]) and v[c] or 0
end

matrix.set = function (m, r, c, val)
   r, c = checkindex(m, r, c)
   m[r] = m[r] or {}
   m[r][c] = val
end

matrix.size = function (m)
   assert(m.type == matrix.type, "Matrix is expected")
   return m.rows, m.cols
end

----------------

t = matrix:init(2,2)
print(t:size())

t:set(-1,0, 4)
t:set(0,-1, 2)
print(t:get(0,0), t:get(0,1), t:get(1,0), t:get(1,1))


