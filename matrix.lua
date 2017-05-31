
local matrix = {}
matrix.__index = matrix

matrix.type = 'matrix'

function matrix:init(r, c, m)
   m = m or {}
   m.cols, m.rows = c, r
   setmetatable(m, self)
   return m
end

function matrix.new(...)
   local args = {...}
   local cols, rows = 0, #args
   for i = 1, rows do
      assert(type(args[i]) == 'table', "Row must be a table!")
      cols = (cols < #args[i]) and #args[i] or cols
   end
   return matrix:init(rows, cols, args)
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

local function getval(m, r, c)
   local v = m[r]
   if v then 
      return v[c] or 0
   else
      return 0
   end
end

matrix.get = function (m, r, c)
   r, c = checkindex(m, r, c)
   return getval(m, r, c)
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

matrix.__tostring = function (m)
   local sr = {}
   for r = 1, m.rows do
      local sc = {}
      for c = 1, m.cols do
         table.insert(sc, string.format("%d", getval(m, r, c)))
      end
      table.insert(sr, table.concat(sc, "  "))
   end
   return table.concat(sr, "\n")
end

----------------
--[[
v = matrix.vector(1,2,3)
w = v:transpose()

print(v:size())
print(w:size())
print(v(1), w(1))
]]

m = matrix.new({1,2}, {4,5,6}, {7,8})
print(m:size(), m(0,0), m(1,1), m(2,2))
print(m)
