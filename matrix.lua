
local matrix = {}
matrix.__index = matrix

matrix.type = 'matrix'

local function ismatrix(m) return type(m) == 'table' and m.type == matrix.type end

function matrix:init(r, c, m)
   assert(r > 0 and c > 0, "Wrong matrix size")
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
   assert(ismatrix(m), "Matrix is expected")
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

local function setval(m, r, c, v)
   if (m[r] and m[r][c]) or v ~= 0 then 
      m[r] = m[r] or {}
      m[r][c] = v
   end
end

local function kprod(k, m)
   local res = matrix:init(m.rows, m.cols)
   for r = 1, m.rows do
      for c = 1, m.cols do
      	 setval(res,r,c, k*getval(m,r,c))
      end
   end
   return res
end

local function gaussdown(m)
   local A = 1
   for k = 1, m.rows-1 do
      -- look for nonzero element
      local i = k+1
      while getval(m,k,k) == 0 and i <= m.rows do
         if getval(m,i,k) ~= 0 then m[i],m[k],A = m[k],m[i],-A end
	 i = i+1
      end
      local coef = getval(m,k,k)
      A = A * coef
      if coef ~= 0 then
         -- normalization
	 coef = 1/coef
         for c = k, m.cols do setval(m,k,c, getval(m,k,c)*coef) end
         -- substraction
         for r = (k+1), m.rows do
            local v = getval(m, r, k)
	    if v ~= 0 then
               for c = k, m.cols do setval(m,r,c, getval(m,r,c)-v*getval(m,k,c)) end
	    end
         end
      end
   end
   return m, A
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
	 setval(res,c,r, getval(m,r,c))
      end
   end
   return res
end

local function sum(a,b,sign)
   assert(a.cols == b.cols and a.rows == b.rows, "Wrong matrix size")
   local res = matrix:init(a.rows, a.cols)
   for r = 1, res.rows do
      for c = 1, res.cols do
         local sum = (sign == '-') and (getval(a,r,c)-getval(b,r,c)) or (getval(a,r,c)+getval(b,r,c))
	 setval(res,r,c, sum)
      end
   end
   return res
end

matrix.__add = function (a,b)
   return sum(a, b)
end

matrix.__sub = function (a,b)
   return sum(a, b, '-')
end

matrix.__unm = function (a)
   local tmp = matrix:init(a.rows, a.cols)
   return sum(tmp, a, '-')
end

matrix.size = function (m)
   assert(ismatrix(m), "Matrix is expected")
   return m.rows, m.cols
end

matrix.__len = function (m)
   return m.rows, m.cols
end

matrix.copy = function (m)
   assert(ismatrix(m), "Matrix is expected!")
   local res = matrix:init(m.rows, m.cols)
   for r = 1, res.rows do
      for c = 1, res.cols do setval(res,r,c, getval(m,r,c)) end
   end
   return res
end

matrix.__mul = function (a,b)
   assert(ismatrix(a) or ismatrix(b), "Wrong argemnt type")
   if not ismatrix(a) then return kprod(a, b) end
   if not ismatrix(b) then return kprod(b, a) end
   assert(a.cols == b.rows, "Impossible to get product: different size")
   local res = matrix:init(a.rows, b.cols)
   for r = 1, res.rows do
      for c = 1, res.cols do
         local sum = 0
	 for i = 1, a.cols do sum = sum + getval(a,r,i)*getval(b,i,c) end
	 setval(res,r,c, sum)
      end
   end
   return res
end

matrix.det = function (m)
   assert(m.rows == m.cols, "Square matrix is expected!")
   local tr, K = gaussdown(matrix.copy(m))
   return (K == 0) and 0 or K*tr[m.rows][m.cols]
end

matrix.vector = function (...)
   local v, res = {...}, {}
   for i = 1, #v do res[i] = {v[i]} end
   return matrix:init(#v, 1, res)
end

matrix.zeros = function (rows, cols)
   return matrix:init(rows, cols)
end

matrix.eye = function (rows, cols)
   local m = matrix:init(rows, cols)
   for i = 1, math.min(rows, cols) do setval(m,i,i, 1) end
   return m
end

matrix.sub = function (m, r1, r2, c1, c2)
   r1, c1 = checkindex(m, r1, c1)
   r2, c2 = checkindex(m, r2, c2)
   local res = matrix:init(r2-r1+1, c2-c1+1)
   local i, j = 1, 1
   for r = r1, r2 do
      for c = c1, c2 do
	 setval(res,i,j, getval(m,r,c))
	 j = j+1
      end
      j = 1
      i = i+1
   end
   return res
end

matrix.concat = function (a, b, dir)
   local res = nil
   if dir == 'h' then
      assert(a.rows == b.rows, "Different number of rows")
      res = matrix:init(a.rows, a.cols+b.cols)
   elseif dir == 'v' then
      assert(a.cols == b.cols, "Different number of columns")
      res = matrix:init(a.rows+b.rows, a.cols)
   else
      error("Unexpected type of concatenation")
   end
   for r = 1, res.rows do
      for c = 1, res.cols do
         local src = (r <= a.rows and c <= a.cols) and a or b
         local i = (r <= a.rows) and r or (r - a.rows)
	 local j = (c <= a.cols) and c or (c - a.cols)
	 setval(res,r,c, getval(src,i,j))
      end
   end
   return res
end

matrix.__concat = function (a,b)
   return matrix.concat(a,b,'h')
end

matrix.__idiv = function (a,b)
   return matrix.concat(a,b,'v')
end

matrix.__tostring = function (m)
   local srow = {}
   for r = 1, m.rows do
      local scol = {}
      for c = 1, m.cols do
         table.insert(scol, getval(m, r, c))
      end
      table.insert(srow, table.concat(scol, "  "))
   end
   return table.concat(srow, "\n")
end

----------------

a = matrix.new({0,2,3},{0,5,6},{7,8,9})

print(a:det())
print(a)
