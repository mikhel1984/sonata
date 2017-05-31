
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
   if c == nil then
      if m.cols == 1 then 
         r, c = r, 0
      elseif m.rows == 1 then 
         r, c = 0, r
      end
   end
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

matrix.__call = function (m, r, c)
   return matrix.get(m, r, c)
end

matrix.set = function (m, r, c, val)
   r, c = checkindex(m, r, c)
   m[r] = m[r] or {}
   m[r][c] = val
end

matrix.transpose = function (m)
   local res = matrix:init(m.cols, m.rows)
   for r = 1, m.rows do
      for c = 1, m.cols do
         local v = m[r]
	 if v and v[c] then
	    res[c] = res[c] or {}
	    res[c][r] = v[c]
	 end
      end
   end
   return res
end

matrix.size = function (m)
   assert(m.type == matrix.type, "Matrix is expected")
   return m.rows, m.cols
end

matrix.vector = function (...)
   local v, res = {...}, {}
   for i = 1, #v do res[i] = {v[i]} end
   return matrix:init(#v, 1, res)
end

----------------
--[[
t = matrix:init(2,2)
print(t:size())

t:set(-1,0, 4)
t:set(0,-1, 2)
print(t(0,0), t(0,1), t(1,0), t(1,1))
]]

v = matrix.vector(1,2,3)
w = v:transpose()

print(v:size())
print(w:size())
print(v(1), w(1))
