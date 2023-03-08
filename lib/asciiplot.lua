--[[		sonata/lib/asciiplot.lua

--- Use pseudography for data visualization.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2023.

	module 'asciiplot'
--]]

--[[TEST

-- use 'asciiplot'
Ap = require 'lib.asciiplot'

-- figure with default size
fig1 = Ap()
--print(fig1.width, fig1.height)

-- print functions
fig1:setX {-3.14, 3.14}   -- default is {-1, 1}
fig1:plot(math.sin, 'sin', math.cos, 'cos')
fig1.title = 'Trigonometry'
print(fig1)

-- print data
x = {1,2,3,4,5}
y = {1,3,5,7,9}
print(fig1:plot(x,y))

-- combine different sources
fig1.yaxis = 'min'  -- left axis
fig1:plot(x,'single',x,y,'pair',math.log, 'function')
print(fig1)

-- define arbitrary figure size
-- odd is preferable
fig2 = Ap(21,11)    -- width=21, height = 11
print(fig2:plot(function (x) return 2*x end))

-- show table
tbl = {}
for x = 0, 3, 0.1 do
  --             x      y1          y2
  tbl[#tbl+1] = {x, math.sin(x), math.cos(x)}
end
fig2.xaxis = 'min'  -- down
print(fig2:tplot(tbl))

-- plot only y2, don't rescale
fig2.xrange = {-1, 4}
fig2.yrange = {0, 1}
print(fig2:tplot(tbl, {2, yfix=true}))

-- scale figure w.r.t. initial size
fig1:scale(0.8)
ans = fig1.width             --> 59 

-- horizontal concatenation
-- first
fig1:scale(0.5, true)
fig1.xrange = {0, 1.57}
fig1.yaxis = 'mid'; fig1.xaxis = 'min'
fig1:plot(sin, 'sin')
fig1.title = 'First'
-- second
fig2:scale(0.5, true)
fig2.xrange = {0, 1.57}
fig2:plot(cos, 'cos')
fig2.title = 'Second'
print(Ap:concat(fig1, fig2))   -- similar to fig1..fig2 for 2 objects

-- call 'API' functions
fig3 = Ap():scale(0.5)
fig3.xrange = {-2,2}
fig3.yrange = {-1,4}
-- no axes and limits
fig3.xaxis = nil; fig3.yaxis = nil
fig3:reset()
-- set function
for x = -1.2, 1.2, 0.1 do
  fig3:addPoint(x, x*x-0.5, Ap.char[1])
end
-- set to position
fig3:addPose(3, 13, '#')   -- characters
fig3:addPose(3, 24, '#')
fig3:addString(2,3,'Hi!')  -- text
print(fig3)

-- print surface contours
fig4 = Ap()
fig4.xrange = {-5, 5}
fig4.yrange = {-5, 5}
print(fig4:contour(function (x,y) return x*x - y*y end))

-- bar diagram
data = {}
k = 2*3.14/20
for i = 1, 20 do data[#data+1] = {k*i, math.sin(k*i)} end
fig5 = Ap()
print(fig5:bar(data))

--]]

--	LOCAL

--- Check object type.
--  @param v Object.
--  @return True if the object is asciiplot.
local function isasciiplot(v) return type(v)=='table' and v.isasciiplot end

-- default figure size
local WIDTH, HEIGHT = 73, 21
local STR_NORM, STR_LOG = ' %s ', ' 10^%d '


local mmodf = math.modf

local MANUAL = 'manual'

--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Use pseudography for data visualization."
}

--	MODULE

local axis = {
  log10 = math.log(10)
}
axis.__index = axis 

axis.new = function (N)
  if N < 9 then 
    print('Set minimal axis size:', N)
    N = 9
  elseif N % 2 == 0 then
    print('Change to odd size')
    return asis.new(N+1)
  end
  local x1, x2 = -1, 1
  local a = {
    size = N, 
    range = {x1, x2}, 
    diff = x2-x1, 
    _init = {x1, x2},
    _size = N,
  }
  return setmetatable(a, axis)
end

axis.setRange = function (A, t)
  A._init[1], A._init[2] = t[1], t[2]  -- exact limits
  if A.log then
    local p1 = math.log(t[1]) / axis.log10
    p1 = (p1 > 0) and math.ceil(p1) or math.floor(p1)
    local p2 = math.log(t[2]) / axis.log10
    p2 = (p2 > 0) and math.ceil(p2) or math.floor(p2)
    -- expected even number of intervals, add empty if need
    --if (p2 - p1) % 2 == 1 then p1 = p1 - 1 end
    A.range[1], A.range[2] = p1, p2
  else
    A.range[1], A.range[2] = t[1], t[2]
    local p = math.log(t[2] - t[1]) / axis.log10
    local n = (p >= 0) and math.floor(p) or math.ceil(p)
    local tol = 10^(n-1)
    local v, rest = mmodf(t[1] / tol)
    if rest ~= 0 then
      rest = (rest > 0) and 0 or 1
      A.range[1] = (v - rest) * tol
    end
    v, rest = mmodf(t[2] / tol)
    if rest ~= 0 then
      rest = (rest < 0) and 0 or 1
      A.range[2] = (v + rest) * tol
    end
  end
  A.diff = A.range[2] - A.range[1]
end

axis.setLog = function (A, isLog)
  if isLog and not A.log or not isLog and A.log then
    A.log = isLog
    A:setRange(A._init)  -- update limits
  end
end

axis.copy = function (A)
  local new = {
    size = A.size,
    log = A.log,
    range = {A.range[1], A.range[2]},
    diff = A.diff,
    _init = {A._init[1], A._init[2]},
    _size = A._size,
  }
  return setmetatable(new, axis)
end

axis.scale = function (A, factor)
  local int, frac = mmodf(A._size * factor)
  A.size = (int % 2 == 1) and int or (int + 1)
end

axis.proj = function (A, d, isX)
  d = A.log and math.log(d)/axis.log10 or d
  if d < A.range[1] or d > A.range[2] then return nil end
  local dx = (d - A.range[1]) / A.diff
  local int, frac = nil, nil
  if isX then
    int, frac = mmodf((A.size-1) * dx + 1)
  else
    int, frac = mmodf((1-A.size) * dx + A.size)
  end
  int = (frac > 0.5) and (int + 1) or int
  return (1 <= int and int <= A.size) and int or nil
end

axis.values = function (A, isX)
  local x0 = isX and A.range[1] or A.range[2]
  local k  = isX and A.diff / (A.size - 1) or -A.diff / (A.size - 1)
  local ind = 1
  return function ()
    if ind <= A.size then
      local x = (ind - 1) * k + x0
      local i = ind
      ind, x = ind + 1, A.log and math.pow(10, x) or x
      return i, x
    end
  end
end

axis.limit = function (A, s)
  local v = nil
  if s == 'min' then
    v = A.range[1]
  elseif s == 'max' then
    v = A.range[2]
  elseif s == 'mid' then
    v = (A.range[1]+A.range[2]) / 2
  else
    return nil
  end
  return A.log and string.format(' 10^%f ', v) 
    or string.format(' %s ', tostring(v))
end

axis.markerInterval = function (A)
  local dmin = 4
  local d = A.size - 1
  while d > dmin and d % 2 == 0 do d = d / 2 end
  return d
end

local asciiplot = {
-- mark
type = 'asciiplot', isasciiplot = true,
-- symbols
char = {'*', 'o', '#', '%', '~', 'x'},
lvls = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'},
}

if SONATA_USE_COLOR then
  local char = asciiplot.char
  for k, v in ipairs(char) do
    char[k] = string.format('\x1B[3%dm%s\x1B[0m', k, v)
  end
  char = asciiplot.lvls
  for k, v in ipairs(char) do
    char[k] = string.format('\x1B[3%dm%s\x1B[0m', k, v)
  end
end

-- Simplify call for two objects.
asciiplot.__concat = function (F1, F2) return asciiplot.concat(_, F1, F2) end

-- methametods
asciiplot.__index = asciiplot

--- String representation of the object.
--  @param F Figure object.
--  @return String.
asciiplot.__tostring = function (F)
  if #F.canvas == 0 then return 'empty' end
  local acc = {}
  -- title
  if F.title then
    -- to center
    acc[1] = asciiplot._format(F.title, F._x.size, true, false)
  end
  -- figure
  for i = 1, F._y.size do
    acc[#acc+1] = table.concat(F.canvas[i])
  end
  -- legend
  local sym = {}
  for k, _ in pairs(F.legend) do sym[#sym+1] = k end
  if #sym > 0 then table.sort(sym) end
  for _, k in ipairs(sym) do
    acc[#acc+1] = string.format("(%s) %s", k, F.legend[k])
  end
  return table.concat(acc, '\n')
end

--- Add points from a table.
--  First element of each row is x, the rest are yi.
--  @param F Figure object.
--  @param t Table to print.
--  @param tInd Allows to choose y columns.
asciiplot._addTable = function (F, t, tInd)
  for j = 1, #tInd do
    local c, k = asciiplot.char[j], tInd[j]
    for i = 1, #t do
      local row = t[i]
      asciiplot.addPoint(F, row[1], row[k], c)
    end
    F.legend[c] = 'column '..tostring(k)   -- default legend
  end
end

--- Add group of points.
--  @param F Figure object.
--  @param tX List of x coordinates.
--  @param tY List of y coordinates.
--  @param s Character.
asciiplot._addXY = function (F, tX, tY, s)
  for i = 1, #tX do
    asciiplot.addPoint(F, tX[i], tY[i], s)
  end
end

--- Add coordinate axes.
--  @param F Figure object.
asciiplot._axes = function (F)
  local vertical, horizontal, mark = '|', '-', '+'
  -- vertical line
  local n = nil
  if     F.yaxis == 'mid' then n = (F._x.size+1) / 2
  elseif F.yaxis == 'min' then n = 1
  elseif F.yaxis == 'max' then n = F._x.size end
  if n then
    for i = 1, F._y.size do
      F.canvas[i][n] = vertical
    end
    F.canvas[1][n] = 'A'
    -- markers
    local d = F._y:markerInterval()
    for i = d+1, F._y.size-1, d do F.canvas[i][n] = mark end
    n = nil
  end
  -- horizontal line
  if F.xaxis == 'mid' then n = (F._y.size+1) / 2
  elseif F.xaxis == 'max' then n = 1
  elseif F.xaxis == 'min' then n = F._y.size end
  if n then
    local row = F.canvas[n]
    for i = 1, F._x.size do
      row[i] = horizontal
    end
    row[F._x.size] = '>'
    -- markers
    local d = F._x:markerInterval()
    for i = d+1, F._x.size-1, d do row[i] = mark end
  end
end

--- Resize, clear canvas.
--  @param F Figure object.
asciiplot._clear = function (F)
  local white, width = ' ', F._x.size
  for i = 1, F._y.size do
    local row = F.canvas[i] or {}
    for j = 1, width do row[j] = white end
    if #row > width then
      for j = #row, width+1, -1 do row[j] = nil end
    end
    F.canvas[i] = row
  end
  F.legend = {}
  F.title = nil
end

--- Fill legend for contour object.
--  @param F Figure object.
--  @param s Axis name.
--  @param lvl List of levels.
asciiplot._cntLegend = function (F, s, lvl)
  local j, line = 1, {}
  for i = 1, #lvl do
    -- group levels
    line[#line+1] = string.format('%s(%.2f)', asciiplot.lvls[i], lvl[i])
    if i % 3 == 0 then
      F.legend[s..tostring(j)] = table.concat(line, '  ')
      j, line = j + 1, {}
    end
  end
  if #line > 0 then
    F.legend[s..tostring(j)] = table.concat(line, '  ')
  end
end

--- Get bounds of the table values.
--  @param t Table {{x1,y11,y12...}, {x2,y21,y22..}, ...}
--  @param tInd Table of y column indeces.
--  @return xrange, yrange
asciiplot._findRange = function (t, tInd)
  tInd = tInd or {}
  local xmax, ymax, xmin, ymin = -math.huge, -math.huge, math.huge, math.huge
  for i = 1, #t do
    local row = t[i]
    local v = row[1]
    if v > xmax then xmax = v end
    if v < xmin then xmin = v end
    if #tInd > 0 then
      for j = 1, #tInd do
        v = row[tInd[j]]
        if v > ymax then ymax = v end
        if v < ymin then ymin = v end
      end
    else
      for j = 2, #row do
        v = row[j]
        if v > ymax then ymax = v end
        if v < ymin then ymin = v end
      end
    end
  end
  return {xmin, xmax}, {ymin, ymax}
end

--- Get bounds of the vector.
--  @param t Table (vector).
--  @return Minimal and maximal values.
asciiplot._findVectorRange = function (t)
  local vmin, vmax = math.huge, -math.huge
  for i = 1, #t do
    local v = t[i]
    if v > vmax then vmax = v end
    if v < vmin then vmin = v end
  end
  return vmin, vmax
end

--- Find vectors X and Y for the given function.
--  @param F Figure object.
--  @param fn Function f(x).
--  @return Lists of coordinates X and Y.
asciiplot._fn2XY = function (F, fn)
  local X, Y = {}, {}
  for i, x in F._x:values(true) do
    X[i] = x
    Y[i] = fn(x)
  end
  return X, Y
end

--- Find height for each pair (x, y).
--  @param F Figure object.
--  @param fn Function f(x,y).
--  @return Vectors X, Y, 'matrix' Z, range of heights.
asciiplot._fn2Z = function (F, fn)
  local X, Y, Z = {}, {}, {}
  for i, x in F._x:values(true) do X[i] = x end
  local zmin, zmax = math.huge, -math.huge
  for ny, y in F._y:values(false) do
    local row = {}
    for nx = 1, #X do
      local z = fn(X[nx], y)
      if z > zmax then zmax = z end
      if z < zmin then zmin = z end
      row[nx] = z
    end
    Y[ny] = y
    Z[ny] = row
  end
  return X, Y, Z, {zmin, zmax}
end

--- Prepare string of the given length.
--  @param s Source string.
--  @param N Required length.
--  @param bCentr Flag to put to central position.
--  @param bLim   Flag to cut a long string.
asciiplot._format = function (s, N, bCentr, bCut)
  local res = s
  if #s < N then
    local n = N - #s
    if bCentr then
      local n1 = mmodf(n / 2)
      local n2 = n - n1
      res = string.rep(' ', n1) .. s .. string.rep(' ', n2)
    else
      res = s .. string.rep(' ', n)
    end
  end
  if #res > N and bCut then
    res = string.sub(res, 1, N)
  end
  return res
end

--- Add xrange and yrange.
--  @param F Figure object.
asciiplot._limits = function (F)
  -- horizontal
  local width, height = F._x.size, F._y.size
  local n = nil
  if F.xaxis == 'mid' then n = (height+1) / 2 + 1
  elseif F.xaxis == 'max' then n = 1
  elseif F.xaxis == 'min' then n = height end
  if n then
    -- min
    local row, beg = F.canvas[n], 0
    local s = F._x:limit('min')
    for i = 1, #s do row[beg+i] = string.sub(s, i, i) end
    -- max
    s = F._x:limit('max')
    beg = width - #s - 1
    for i = 1, #s do row[beg+i] = string.sub(s, i, i) end
    -- mid
    s = F._x:limit('mid')
    beg = (width + 1) / 2
    for i = 1, #s do row[beg+i] = string.sub(s, i, i) end
    n = nil
  end
  -- vertical
  if F.yaxis == 'mid' then n = (width + 1) / 2
  elseif F.yaxis == 'min' then n = 1
  elseif F.yaxis == 'max' then n = width end
  if n then
    -- min
    local s = F._y:limit('min')
    local beg = (n == width) and (width - #s - 1) or n
    local row = F.canvas[height-1]
    for i = 1, #s do row[beg+i] = string.sub(s, i, i) end
    -- max
    s = F._y:limit('max')
    row = F.canvas[2]
    for i = 1, #s do row[beg+i] = string.sub(s, i, i) end
    -- mid
    s = F._y:limit('mid')
    row = F.canvas[(height + 1) / 2]
    for i = 1, #s do row[beg+i] = string.sub(s, i, i) end
  end
end

--- Constructor example.
--  @param dwidth Figure width.
--  @param dheight Figure height.
--  @return New object of asciiplot.
asciiplot._new = function(self, dwidth, dheight)
  local o = {
    _x = axis.new(dwidth),
    _y = axis.new(dheight),
    _z = axis.new(dwidth),
    --_w0 = dwidth or WIDTH,
    --_h0 = dheight or HEIGHT,
    -- axes location
    xaxis  = 'mid',
    yaxis  = 'mid',
    -- image
    canvas = {},
    -- comments
    legend = {},
    -- title can be added
  }
  -- return object
  return setmetatable(o, self)
end

--- Find range of levels for surface plot.
--  @param v1 Begin of range.
--  @param vn End of range.
--  @param N Number of lines.
--  @param bScale Flag to use bounds in calculation.
--  @param bInt Flag for a number rounding.
--  @return List of levels and step value.
asciiplot._surfRange = function (v1, vn, N, bScale, bInt)
  local res, nn, h = {}, N, 0
  if bScale then
    nn = N - 1
    h = (vn - v1) / nn
    res[1] = v1
  else
    h = (vn - v1) / (N + 1)
  end
  for i = 1, nn do
    if bInt then
      local ind, rst = mmodf(1 + h * i)
      res[#res+1] = (rst > 0.5) and ind+1 or ind
    else
      res[#res+1] = v1 + h * i
    end
  end
  return res, h
end

--- Find XY projection of a contour.
--  @param tX X range table.
--  @param tY Y range table.
--- @param tZ Table of z(x,y) values.
--  @param tOpt List of options.
--  @return Figure object.
asciiplot._viewXY = function (F, tX, tY, tZ, tOpt)
  local ZERR = 0.05  -- deflection from expected level value
  local N = tOpt.level
  local lvl, h = asciiplot._surfRange(
    F._z.range[1], F._z.range[2], N, tOpt.minmax, false)
  h = h * ZERR
    -- prepare
  asciiplot._clear(F)
  asciiplot._axes(F)
  -- fill
  for i = 1, F._y.size do
    local row = tZ[i]
    for j = 1, F._x.size do
      local z = row[j]
      for k = 1, N do
        if math.abs(lvl[k] - z) <= h then
          F.canvas[i][j] = asciiplot.lvls[k]
          break
        end
      end
    end
  end
  asciiplot._limits(F)
  -- legend
  asciiplot._cntLegend(F, 'Z', lvl)
  F.title = 'X-Y view'
  return F
end

--- Find XZ projection of a contour.
--  @param tX X range table.
--  @param tY Y range table.
--- @param tZ Table of z(x,y) values.
--  @param tOpt List of options.
--  @return Figure object.
asciiplot._viewXZ = function (F, tX, tY, tZ, tOpt)
  local N = tOpt.level
  local lvl, h = asciiplot._surfRange(1, F._y.size, N, tOpt.minmax, true)
  F.yrange = (tOpt.view == 'XYZ') and {F._z.range[2], F._z.range[1]} 
    or F._z.range
    -- prepare
  asciiplot._clear(F)
  asciiplot._axes(F)
  -- fill
  for j = 1, N do
    local row = tZ[lvl[j]]
    local ch = asciiplot.lvls[N-j+1]
    for i = 1, #tX do
      asciiplot.addPoint(F, tX[i], row[i], ch)
    end
  end
  asciiplot._limits(F)
  -- legend
  local lvlXZ = {}
  for i = 1, N do lvlXZ[i] = tY[lvl[N-i+1]] end
  asciiplot._cntLegend(F, 'Y', lvlXZ)
  F.title = 'X-Z view'
  return F
end

--- Find YZ projection of a contour.
--  @param tX X range table.
--  @param tY Y range table.
--- @param tZ Table of z(x,y) values.
--  @param tOpt List of options.
--  @return Figure object.
asciiplot._viewYZ = function (F, tX, tY, tZ, tOpt)
  local N = tOpt.level
  local lvl, h = asciiplot._surfRange(1, F._x.size, N, tOpt.minmax, true)
  -- update ranges
  F._x.setRange(F._y._init)
  F._y.setRange(F._z._init)
  F._x.size, F._y.size = F._y.size, F._x.size
  -- prepare
  asciiplot._clear(F)
  asciiplot._axes(F)
  -- fill
  for i = 1, #tY do
    local y = tY[i]
    local zi = tZ[i]
    for j = #lvl, 1, -1 do
      asciiplot.addPoint(F, y, zi[lvl[j]], asciiplot.lvls[j])
    end
  end
  asciiplot._limits(F)
  -- legend
  local lvlZY = {}
  for i = 1, N do lvlZY[i] = tX[lvl[i]] end
  asciiplot._cntLegend(F, 'X', lvlZY)
  F.title = 'Y-Z view'
  return F
end

--- Scale and add a point to the figure.
--  @param F Figure object.
--  @param dx Coordinate x.
--  @param dy Coordinate y.
--  @param s Character.
asciiplot.addPoint = function (F, dx, dy, s)
  local nx = F._x:proj(dx, true)
  local ny = F._y:proj(dy, false)
  if nx and ny and (#s == 1 or SONATA_USE_COLOR) then
    print(nx, ny)
    F.canvas[ny][nx] = s
  end
end
about[asciiplot.addPoint] = {"F:addPoint(x_d, y_d, char_s) --> nil", 
  "Add point (x,y) using char.", MANUAL}

--- Set character to direct position.
--  @param F Figure object.
--  @param ir Row index.
--  @param ic Column index.
--  @param s Character.
asciiplot.addPose = function (F, ir, ic, s)
  if ir > 0 and ir <= F._y.size and ic > 0 and ic <= F._x.size
             and (#s == 1 or SONATA_USE_COLOR) then
    F.canvas[ir][ic] = s
  end
end
about[asciiplot.addPose] = {"F:addPose(row_N, col_N, char_s) --> nil", 
  "Add character to the given position.", MANUAL}

--- Set string to the given position.
--  @param F Figure object.
--  @param ir Row index.
--  @param ic Column index.
--  @param s String.
asciiplot.addString = function (F, ir, ic, s)
  for i = 1, #s do
    asciiplot.addPose(F, ir, ic+i-1, string.sub(s, i, i))
  end
end
about[asciiplot.addString] = {"F:addString(row_N, col_N, str) --> nil",
  "Set string from the given position.", MANUAL}

--- Plot bar graph.
--  @param F Figure object.
--  @param t Data table ({x,y} or x).
--  @param vy Index of y column or data table (optional).
--  @param ix Optional index of x values.
--  @return Updated figure object.
asciiplot.bar = function (F, t, vy, ix)
  ix, vy = ix or 1, vy or 2
  local ytbl = (type(vy) == 'table')   -- TODO Split into 2 functions
  -- size
  local iL = mmodf(F._x.size * 0.2)
  local iR = F._x.size - iL
  -- find limits
  local min, max = math.huge, -math.huge
  for i = 1, #t do
    local v = ytbl and vy[i] or t[i][vy]
    if v > max then max = v end
    if v < min then min = v end
  end
  local i0 = nil
  if min >= 0 and max >= 0 then
    i0, min = iL, 0
  elseif min <= 0 and max <= 0 then
    i0, max = iR, 0
  else
    local int, frac = mmodf(iL - min / (max - min) * (iR - iL))
    i0 = (frac > 0.5) and int + 1 or int
  end
  local dm = (iR - iL) / (max - min)
  -- data step
  local step = 1
  if #t > F._y.size then
    local int, frac = mmodf(#t / F._y.size)
    step = (frac > 0) and (int+1) or int
  end
  -- add values
  asciiplot._clear(F)
  local r = 1
  for i = 1, #t, step do
    -- text
    local x = tostring(ytbl and t[i] or t[i][ix])
    for c = 1, math.min(iL-2, #x) do F.canvas[r][c] = string.sub(x, c, c) end
    -- line
    x = ytbl and vy[i] or t[i][vy]
    local i1, i2 = mmodf(x * dm + i0)
    if x >= 0 then
      i2 = (i2 >= 0.5) and i1 + 1 or i1   -- right limit
      i1 = i0
    else
      i1 = (i2 >= 0.5) and i1 - 1 or i1   -- left limit
      i2 = i0
    end
    for c = i1, i2 do F.canvas[r][c] = '=' end
    -- value
    x = tostring(x)
    for c = 1, math.min(iL-2, #x) do
      F.canvas[r][iR+2+c] = string.sub(x, c, c)
    end
    r = r + 1
  end
end
about[asciiplot.bar] = {"F:bar(t, vy=2, x_N=1) --> nil",
  "Plot bar diargram for data. vy can be y index in t (optional) or table of y-s."}

--- Horizontal concatenation of figures.
--  @param self Do nothing.
--  @param ... List of figure objects.
--  @return String with figures.
asciiplot.concat = function (self, ...)
  -- collect info
  local ag = {...}
  local nlegend = 0
  for i, v in ipairs(ag) do
    if not isasciiplot(v) then
      error("Not asciiplot object at "..tostring(i))
    end
    if v._y.size ~= ag[1]._y.size then error('Different height') end
    local n = 0
    for k, _ in pairs(v.legend) do n = n + 1 end
    if n > nlegend then nlegend = n end
  end
  -- data
  local acc = {}
  local gap = '   '
  for i, v in ipairs(ag) do
    local k, width = 1, v._x.size
    -- title
    local row = acc[k] or {}
    row[#row+1] = asciiplot._format(v.title or '', width, true, true)
    row[#row+1] = gap
    acc[k] = row; k = k + 1
    -- content
    for j = 1, v.height do
      row = acc[k] or {}
      row[#row+1] = table.concat(v.canvas[j])
      row[#row+1] = gap
      acc[k] = row; k = k + 1
    end
    -- legend
    local n = 0
    local sym = {}
    for k, _ in pairs(v.legend) do sym[#sym+1] = k end
    if #sym > 0 then table.sort(sym) end
    for _, u in ipairs(sym) do
      row = acc[k] or {}
      row[#row+1] = asciiplot._format(
        string.format('(%s) %s', u, v.legend[u]), width-1+#u, false, true)
      row[#row+1] = gap
      acc[k] = row; k = k + 1
      n = n + 1
    end
    -- add empty lines
    for j = n+1, nlegend do
      row = acc[k] or {}
      row[#row+1] = asciiplot._format('', width, false, true)
      row[#row+1] = gap
      acc[k] = row; k = k + 1
    end
  end
  -- get strings
  for i = 1, #acc do acc[i] = table.concat(acc[i]) end
  return table.concat(acc, '\n')
end
about[asciiplot.concat] = {":concat(...) --> str",
  "Horizontal concatenation of figures with the same height. For two object operator '..' can be used.",
  help.STATIC}

--- Plot function of two arguments using contours.
--  @param F Figure object.
--  @param fn Function f(x,y).
--  @param tOpt Table of options: level - number of lines, zfix - flag for rescale, view - projection ('XY', 'XZ', 'YZ', 'XYZ' for its combination).
--  @return Figure object for single view or string for 'XYZ'.
asciiplot.contour = function (F, fn, tOpt)
  tOpt = tOpt or {}
  tOpt.level = tOpt.level or 5    -- TODO limit maximal and minimal values
  if tOpt.level > #asciiplot.lvls then 
    error('Max levle is '..tostring(#asciiplot.lvls)) 
  end
  local view = tOpt.view or 'XY'
  -- calculate
  if view == 'YZ' then
    -- increase resolution for default size
    F._x.size, F._y.size = F._y.size, F._x.size
  end
  local X, Y, Z, zrng = asciiplot._fn2Z(F, fn)
  if not tOpt.zfix then
    F._z:setRange(zrng)
  end
  local acc = {}
  if view == 'XY' or view == 'XYZ' then
    acc[#acc+1] = asciiplot._viewXY(asciiplot.copy(F), X, Y, Z, tOpt)
  end
  if view == 'XZ' or view == 'XYZ' then
    acc[#acc+1] = asciiplot._viewXZ(asciiplot.copy(F), X, Y, Z, tOpt)
  end
  if view == 'YZ' or view == 'XYZ' then
    acc[#acc+1] = asciiplot._viewYZ(asciiplot.copy(F), X, Y, Z, tOpt)
  end
  if view == 'XYZ' then
    local txt = {asciiplot.concat(nil, acc[1], acc[3]), tostring(acc[2])}
    return table.concat(txt, '\n\n')
  end
  return acc[1]
end
about[asciiplot.contour] = {"F:contour(fn, {view='XY'}) --> F|str",
  "Find contours of projection for a function fn(x,y). Views: XY, XZ, YZ, XYZ."}

--- Make a copy.
--  @param F Initial object.
--  @return Copy of the object.
asciiplot.copy = function (F)
  local o = {
    _x = F._x:copy(),
    _y = F._y:copy(),
    _z = F._z:copy(),
    title = F.title,
    xaxis = F.xaxis,
    yaxis = F.yaxis,
    canvas = {},
    legend = {},
  }
  for k, v in pairs(F.legend) do
    o.legend[k] = v
  end
  for i = 1, #F.canvas do
    local row = {}
    -- TODO use Ver.move
    local src = F.canvas[i]
    for j = 1, F._x.size do
      row[j] = src[j]
    end
    o.canvas[i] = row
  end
  return setmetatable(o, asciiplot)
end
about[asciiplot.copy] = {
  "F:copy() --> cpy_F", "Create a copy of the object.", help.OTHER}

--- Generalized plot funciton.
--  @param F Figure object.
--  @param ... is "t1", "t1,t2", "fn", "t1,name", "t1,t2,name" etc.
--  @return The updated figure object.
asciiplot.plot = function (F, ...)
  local ag, acc = {...}, {}
  local xmin, xmax = math.huge, -math.huge
  -- collect data
  local i = 1
  repeat
    local tx, ty = ag[i], ag[i+1]
    -- data
    if type(tx) == 'function' then
      -- save funciton, check region later
      ty = nil
      i = i + 1
    elseif type(tx) == 'table' then
      if type(ty) == 'table' then
        i = i + 2
      else
        ty, tx = tx, {}
        for i = 1, #ty do tx[i] = i end
        i = i + 1
      end
      local a, b = asciiplot._findVectorRange(tx)
      if a < xmin then xmin = a end
     if b > xmax then xmax = b end
    else
      error('Unexpected argument with position '..tostring(i))
    end
    -- legend
    local legend = ag[i]
    if type(legend) ~= 'string' then
      legend = 'line '..tostring(#acc+1)
    else
      i = i + 1
    end
    -- save
    acc[#acc+1] = {tx, ty, legend}
  until i > #ag
  -- check if there are no tables
  if xmin == math.huge then xmin = F._x._init[1] end
  if xmax == -math.huge then xmax = F._x._init[2] end
  F._x:setRange({xmin, xmax})

  -- update y range
  local ymin, ymax = math.huge, -math.huge
  for i = 1, #acc do
    local r = acc[i]
    if type(r[1]) == 'function' then
      r[1], r[2] = asciiplot._fn2XY(F, r[1])
    end
    local a, b = asciiplot._findVectorRange(r[2])
    if a < ymin then ymin = a end
    if b > ymax then ymax = b end
  end
  F._y:setRange({ymin, ymax})

  -- prepare
  asciiplot._clear(F)
  asciiplot._axes(F)
  -- 'plot'
  for i = 1, #acc do
    local c = asciiplot.char[i]
    local r = acc[i]
    asciiplot._addXY(F, r[1], r[2], c)
    F.legend[c] = r[3]
  end
  -- limits
  asciiplot._limits(F)
end
about[asciiplot.plot] = {"F:plot(...) --> nil",
  "Plot arguments in form 't', 't1,t1', 'fn,nm', 'fn1,fn2' etc." }

--- Prepare a clear canvas.
--  @param F Figure object.
asciiplot.reset = function (F)
  asciiplot._clear(F)
  asciiplot._axes(F)
  asciiplot._limits(F)
end
about[asciiplot.reset] = {"F:reset() --> nil", "Prepare a clear canvas.", MANUAL}

--- Scale xrange and yrange w.r.t. initial size.
--  @param F figure object.
--  @param factor Positive value.
--  @return Updated figure object.
asciiplot.scale = function (F, factor)
  assert(factor > 0)
  F._x:scale(factor)
  F._y:scale(factor)
  return F
end
about[asciiplot.scale] = {"F:scale(factor_d, isDefault=false) --> F",
  "Change figure size w.r.t. initial size."}

--- Plot data represented in form of table
--  {{x1,y11,y12,...}, {x2,y21,y22,...}, ...}
--  @param F Figure object.
--  @param t Data table.
--  @param tOpt (Optional) Table of column indeces, key 'yfix' allows to skip resizing.
--  @return The updated figure object.
asciiplot.tplot = function (F, t, tOpt)
  tOpt = tOpt or {}
  -- update range
  if not tOpt.yfix then
    local rx, ry = asciiplot._findRange(t, tOpt)
    F._x:setRange(rx)
    F._y:setRange(ry)
  end
  -- plot all by default
  if #tOpt == 0 then
    for i = 2, #t[1] do tOpt[#tOpt+1] = i end
  end
  -- prepare
  asciiplot._clear(F)
  asciiplot._axes(F)
  -- fill
  asciiplot._addTable(F, t, tOpt)
  -- limits
  asciiplot._limits(F)
end
about[asciiplot.tplot] = {"F:tplot(data_t, {yfix=false}) --> nil", 
  "Plot the table data, choose columns if need."}

-- Simplify the constructor call.
setmetatable(asciiplot, {
__call = function (self, w, h)
  -- size
  return asciiplot:_new(w or WIDTH, h or HEIGHT)
end})
about[asciiplot] = {" (width_N=73, height_N=21) --> new_F", 
  "Create new asciiplot.", help.STATIC}

asciiplot.setX = function (F, t) F._x:setRange(t) end

asciiplot.setY = function (F, t) F._y:setRange(t) end

asciiplot.setZ = function (F, t) F._z:setRange(t) end

asciiplot.logX = function (F, isLog) F._x:setLog(isLog) end

asciiplot.logY = function (F, isLog) F._y:setLog(isLog) end

asciiplot.logZ = function (F, isLog) F._z:setLog(isLog) end

asciiplot.axis = function (F, s)
  local a = F['_' .. string.lower(s)]
  if a then 
    local res = {
      a.size,
      a.log and math.pow(10, a.range[1]) or a.range[1],
      a.log and math.pow(10, a.range[2]) or a.range[2],
      a.log or false,
    }
    return res
  end
end

if Sonata then

-- Define simplified function call
Plot = function (...)
  local f = Ap()
  f._x:setRange({-5, 5})
  print(f:plot(...))
end
about[Plot] = {"Plot(...) --> nil",
  "Plot arguments in form 't', 't1,t1', 'fn,nm', 'fn1,fn2' etc.", help.OTHER}

end

-- Comment to remove descriptions
asciiplot.about = about

return asciiplot

--======================================
-- FIX contour concatenation when use color
-- TODO bar - choose location of zero
