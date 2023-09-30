--[[		sonata/lib/asciiplot.lua

--- Use pseudography for data visualization.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2023.

	module 'asciiplot'
--]]


--[[TEST_IT

-- use 'asciiplot'
Ap = require 'matlib.asciiplot'

-- figure with default size
fig1 = Ap()
info = fig1:axes()
ans = info.x.size             -->  Ap.WIDTH

-- default axis position
ans = info.x.view             -->  'mid'

-- print functions
fig1:setX {range={-3.14, 3.14}}   -- default is {-1, 1}
fig1:plot(math.sin, 'sin', math.cos, 'cos')
fig1:title 'Trigonometry'
print(fig1)

-- make a copy
fig11 = fig1:copy()
print(fig11)

-- print data
x = {1,2,3,4,5}
y = {1,3,5,7,9}
fig1:plot(x,y)
print(fig1)

-- combine different sources
fig1:setY {view='min'}  -- left axis
fig1:plot(x,'single',x,y,'pair',math.log,'function')
print(fig1)

-- define arbitrary figure size
-- odd is preferable
fig2 = Ap(21,11)    -- width=21, height = 11
fig2:plot(function (x) return 2*x end)
print(fig2)

-- show table
tbl = {}
for x = 0, 3, 0.1 do
  --             x      y1          y2
  tbl[#tbl+1] = {x, math.sin(x), math.cos(x)}
end
fig2:setX {view='min'}  -- down
fig2:tplot(tbl)
print(fig2)

-- plot only y2, don't rescale
fig2:setX {range={-1, 4}}
fig2:setY {range={0, 1}, fix=true}
fig2:tplot(tbl, {2})
print(fig2)

-- polar plot for the table
fig7 = Ap()
fig7:tplot(tbl, {polar=true})
fig7:legend {'sin', 'cos'}
print(fig7)

-- scale figure w.r.t. initial size
fig1:scale(0.8)
ans = fig1:axes().x.size      -->  59

-- horizontal concatenation
-- first
fig1:scale(0.5)   -- half of initial size
fig1:setX {range={0, 1.57}, view='min'}
fig1:setY {view='mid'}
fig1:plot(sin, 'sin')
fig1:title 'First'
-- second
fig2:scale(fig1)      -- set equal size
fig2:setX {range={0, 1.57}}
fig2:plot(cos, 'cos')
fig2:title 'Second'
str = Ap:concat(fig1, fig2)   -- similar to fig1..fig2 for 2 objects
print(str)

-- call 'API' functions
fig3 = Ap():scale(0.5)
-- no axes and limits
fig3:setX {range={-2,2}, view=false}
fig3:setY {range={-1,4}, view=false}
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
fig4:setX {range={-5, 5}}
fig4:setY {range={-5, 5}}
fig4:contour(function (x,y) return x*x - y*y end)
print(fig4)

-- bar diagram
data = {}
k = 2*3.14/20
for i = 1, 20 do data[#data+1] = {k*i, math.sin(k*i)} end
fig5 = Ap()
fig5:bar(data)
print(fig5)

-- use log scale
fig6 = Ap()
fig6:setX {range={-10, 10}}
fig6:setY {log=true}
fig6:plot(math.exp)
print(fig6)

-- simplified call, use range -1...1
-- change it via table if need
Plot(math.cos, 'cos', {-3,3}, {-1,1}, 'range correct')

--]]

--	LOCAL

local inform = Sonata and Sonata.warning or print
local mmodf = math.modf
local MANUAL, CONF = 'manual', 'settings'
local LOG10 = math.log(10)
local vmove = require('matlib/utils').versions.move

--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Use pseudography for data visualization."
}


--	MODULE

-- Axis object
local axis = {}
axis.__index = axis


--- Make a copy.
--  @param A Source axis object.
--  @return Axis copy.
axis.copy = function (self)
  local new = {
    size = self.size,
    log = self.log,
    range = {self.range[1], self.range[2]},
    diff = self.diff,
    _init = {self._init[1], self._init[2]},
    _size = self._size,
  }
  return setmetatable(new, axis)
end


--- Transform number to string.
--  @param A Axis object.
--  @param s Required value (min, mid, max).
--  @return Value as string or nil.
axis.limit = function (self, s)
  local v = nil
  if s == 'min' then
    v = self.range[1]
  elseif s == 'max' then
    v = self.range[2]
  elseif s == 'mid' then
    v = (self.range[1]+self.range[2]) / 2
  else
    return nil
  end
  return self.log and string.format(' 10^%.1f ', v)
    or string.format(' %s ', tostring(v))
end


--- Find available interval to show markers.
--  @return Interval value.
axis.markerInterval = function (self)
  local d = self.size - 1
  while d > 4 and d % 2 == 0 do d = d / 2 end  -- 'visual' parameter
  return d
end


--- Create new axis.
--  @param N Required width.
--  @return axis object.
axis.new = function (N)
  if N < 9 then
    inform('Set minimal axis size:'..tostring(N))
    N = 9
  elseif N % 2 == 0 then
    inform('Change to odd size')
    return axis.new(N+1)
  end
  local x1, x2 = -1, 1
  local a = {
    size = N,          -- current width
    range = {x1, x2},  -- current range
    diff = x2-x1,      -- range amplitude
    _init = {x1, x2},  -- initial range
    _size = N,         -- initial width
  }
  return setmetatable(a, axis)
end


--- Find number position maping on the axis.
--  @param d Number for projection.
--  @param isX Flag of direct projection.
--  @return Position (index) or nil if out of range.
axis.proj = function (self, d, isX)
  d = self.log and math.log(d)/LOG10 or d
  if d < self.range[1] or d > self.range[2] then return nil end
  local dx = (d - self.range[1]) / self.diff
  local int, frac = nil, nil
  if isX then
    int, frac = mmodf((self.size - 1)*dx + 1)
  else
    int, frac = mmodf(dx - self.size*(dx - 1))
  end
  return (frac > 0.5) and (int + 1) or int
end


--- Update axis width.
--  @param N New size.
axis.resize = function (self, N)
  if N < 9 then
    inform('Set minimal axis size:'..tostring(N))
    N = 9
  elseif N % 2 == 0 then
    inform('Change to odd size')
    return axis.resize(self, N+1)
  end
  self.size = N
  self._size = N
end


--- Update axis range.
--  @param t New range {min, max}.
axis.setRange = function (self, t)
  local a, b = t[1], t[2]
  -- check limits
  if a == b then error('Wrong range') end
  if a > b then
    a, b = b, a
  end
  if self.log then
    -- lorarithm mode
    if b <= 0 then b = 1 end
    if a <= 0 then a = b / 10 end
    local p1 = math.log(a) / LOG10
    p1 = (p1 > 0) and math.ceil(p1) or math.floor(p1)
    local p2 = math.log(b) / LOG10
    p2 = (p2 > 0) and math.ceil(p2) or math.floor(p2)
    self.range[1], self.range[2] = p1, p2
  else
    -- normal mode
    self.range[1], self.range[2] = a, b
    local n = math.log(b - a) / LOG10
    n = (n >= 0) and math.floor(n) or math.ceil(n)
    local tol = 10^(n-1)
    local v, rest = mmodf(a / tol)
    if rest ~= 0 then
      rest = (rest > 0) and 0 or 1
      self.range[1] = (v - rest) * tol
    end
    v, rest = mmodf(b / tol)
    if rest ~= 0 then
      rest = (rest < 0) and 0 or 1
      self.range[2] = (v + rest) * tol
    end
  end
  if a ~= t[1] or b ~= t[2] then
    inform(string.format('Change limits to %f %f', a, b))
  end
  self._init[1], self._init[2] = a, b  -- exact limits
  self.diff = self.range[2] - self.range[1]
end


--- Compare range to the initial, set the largest values.
--  @param t Pair {minlim, maxlim}.
axis.setMaxRange = function (self, t)
  axis.setRange(
    self, {math.min(self._init[1], t[1]), math.max(self._init[2], t[2])})
end


--- Apply logarithmic scale for the axis.
--  @param isLog Flag to set scale type.
axis.setLog = function (self, isLog)
  if isLog and not self.log or not isLog and self.log then
    self.log = isLog
    self:setRange(self._init)    -- update limits
  end
end


--- Change size w.r.t initial value.
--  @param factor Positive multiplier.
axis.scale = function (self, factor)
  local int, frac = mmodf(self._size * factor)
  axis.resize(self, (int % 2 == 1) and int or (int + 1))
end


--- Move over the axis elements.
--  @param isX Flag of direct iteration.
--  @return Iterator, it gives index and value.
axis.values = function (self, isX)
  local x0 = isX and self.range[1] or self.range[2]
  local k  = isX and self.diff / (self.size - 1) or -self.diff / (self.size - 1)
  local ind, x = 0, x0 - k
  return function ()
    if ind < self.size then
      ind, x = ind + 1, x + k
      return ind, self.log and (10^x) or x
    end
  end
end


-- Data visualization.
local asciiplot = {
-- mark
type = 'asciiplot',
-- const
WIDTH = 73, HEIGHT = 21,
-- symbols
char = {'*', 'o', '#', '+', '~', 'x'},
lvls = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'},
keys = {'_x','_y','_z','x','y','z'},
}


--- Check object type.
--  @param v Object.
--  @return True if the object is asciiplot.
local function isasciiplot(v) return getmetatable(v) == asciiplot end


-- update markers
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
asciiplot.__concat = function (F1, F2) return asciiplot.concat(nil, F1, F2) end


-- methametods
asciiplot.__index = asciiplot


--- String representation of the object.
--  @return String.
asciiplot.__tostring = function (self)
  if #self._canvas == 0 then return 'empty figure' end
  local acc = {}
  -- title
  if self._title then
    -- to center
    acc[1] = asciiplot._format(self._title, self._x.size, true, false)
  end
  -- figure
  for i = 1, self._y.size do
    acc[#acc+1] = table.concat(self._canvas[i])
  end
  -- legend
  local sym = {}
  for k, _ in pairs(self._legend) do sym[#sym+1] = k end
  if #sym > 0 then table.sort(sym) end
  for _, k in ipairs(sym) do
    acc[#acc+1] = string.format("(%s) %s", k, self._legend[k])
  end
  return table.concat(acc, '\n')
end


--- Transform points to polar system, add to figure.
--  @param t Data table.
--  @param tOpt List of column numbers.
asciiplot._addPolar = function (self, t, tOpt)
  local acc = {}
  -- transform data
  for i, row in ipairs(t) do
    local s, c = math.sin(row[1]), math.cos(row[1])
    for j, ind in ipairs(tOpt) do
      local curr = acc[j] or {}
      curr[i] = {row[ind]*c, row[ind]*s}
      acc[j] = curr
    end
  end
  -- update range
  if not self._yfix then
    for j = 1, #tOpt do
      local rx, ry = asciiplot._findRange(acc[j], {})
      self._x:setMaxRange(rx)
      self._y:setMaxRange(ry)
    end
  end
  -- prepare
  asciiplot._clear(self)
  asciiplot._axes(self)
  -- fill
  for j = 1, #tOpt do
    local xy, c = acc[j], asciiplot.char[j]
    for _, v in ipairs(xy) do
      asciiplot.addPoint(self, v[1], v[2], c)
    end
    self._legend[c] = 'column '..tostring(tOpt[j])
  end
  -- limits
  asciiplot._limits(self)
end


--- Add points from a table.
--  First element of each row is x, the rest are yi.
--  @param t Table to print.
--  @param tInd Allows to choose y columns.
asciiplot._addTable = function (self, t, tInd)
  local x = tInd.x 
  for j = 1, #tInd do
    local c, k = asciiplot.char[j], tInd[j]
    for i = 1, #t do
      local row = t[i]
      asciiplot.addPoint(self, row[x], row[k], c)
    end
    self._legend[c] = 'column '..tostring(k)   -- default legend
  end
end


--- Add group of points.
--  @param tX List of x coordinates.
--  @param tY List of y coordinates.
--  @param s Character.
asciiplot._addXY = function (self, tX, tY, s)
  for i = 1, #tX do
    asciiplot.addPoint(self, tX[i], tY[i], s)
  end
end


--- Add coordinate axes.
asciiplot._axes = function (self)
  local vertical, horizontal, mark, up, right = '|', '-', '+', 'A', '>'
  if SONATA_ASCIIPLOT_UNICODE then
    vertical, horizontal, mark, up, right = '│', '─', '┼', '▲', '▶'
  end
  -- vertical line
  local n = nil
  if     self._yaxis == 'mid' then n = (self._x.size+1) / 2
  elseif self._yaxis == 'min' then n = 1
  elseif self._yaxis == 'max' then n = self._x.size end
  if n then
    for i = 1, self._y.size do
      self._canvas[i][n] = vertical
    end
    self._canvas[1][n] = up
    -- markers
    local d = self._y:markerInterval()
    for i = d+1, self._y.size-1, d do self._canvas[i][n] = mark end
    n = nil
  end
  -- horizontal line
  if     self._xaxis == 'mid' then n = (self._y.size+1) / 2
  elseif self._xaxis == 'max' then n = 1
  elseif self._xaxis == 'min' then n = self._y.size end
  if n then
    local row = self._canvas[n]
    for i = 1, self._x.size do
      row[i] = horizontal
    end
    row[self._x.size] = right
    -- markers
    local d = self._x:markerInterval()
    for i = d+1, self._x.size-1, d do row[i] = mark end
  end
end


--- Resize, clear _canvas.
asciiplot._clear = function (self)
  local white, width = ' ', self._x.size
  for i = 1, self._y.size do
    local row = self._canvas[i] or {}
    for j = 1, width do row[j] = white end
    if #row > width then
      for j = #row, width+1, -1 do row[j] = nil end
    end
    self._canvas[i] = row
  end
  self._legend = {}
  self._title = nil
end


--- Fill legend for contour object.
--  @param s Axis name.
--  @param lvl List of levels.
asciiplot._cntLegend = function (self, s, lvl)
  local j, line = 1, {}
  for i = 1, #lvl do
    -- group levels
    line[#line+1] = string.format('%s(%.2f)', asciiplot.lvls[i], lvl[i])
    if i % 3 == 0 then
      self._legend[s..tostring(j)] = table.concat(line, '  ')
      j, line = j + 1, {}
    end
  end
  if #line > 0 then
    self._legend[s..tostring(j)] = table.concat(line, '  ')
  end
end


--- Get bounds of the table values.
--  @param t Table {{x1,y11,y12...}, {x2,y21,y22..}, ...}
--  @param tInd Table of y column indeces.
--  @return xrange, yrange
asciiplot._findRange = function (t, tInd)
  local xmax, ymax, xmin, ymin = -math.huge, -math.huge, math.huge, math.huge
  local x = tInd.x or 1
  for i = 1, #t do
    local row = t[i]
    local v = row[x]
    if v > xmax then xmax = v end
    if v < xmin then xmin = v end
    if #tInd > 0 then
      for j = 1, #tInd do
        v = row[tInd[j]]
        if v > ymax then ymax = v end
        if v < ymin then ymin = v end
      end
    else
      for j = 1, #row do
        if j ~= x then
          v = row[j]
          if v > ymax then ymax = v end
          if v < ymin then ymin = v end
        end
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
--  @param fn Function f(x).
--  @return Lists of coordinates X and Y.
asciiplot._fn2XY = function (self, fn)
  local X, Y = {}, {}
  for i, x in self._x:values(true) do
    X[i] = x
    Y[i] = fn(x)
  end
  return X, Y
end


--- Find height for each pair (x, y).
--  @param fn Function f(x,y).
--  @return Vectors X, Y, 'matrix' Z, range of heights.
asciiplot._fn2Z = function (self, fn)
  local X, Y, Z = {}, {}, {}
  for i, x in self._x:values(true) do X[i] = x end
  local zmin, zmax = math.huge, -math.huge
  for ny, y in self._y:values(false) do
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
asciiplot._limits = function (self)
  -- horizontal
  local width, height = self._x.size, self._y.size
  local n = nil
  if self._xaxis == 'mid' then n = (height + 3) / 2
  elseif self._xaxis == 'max' then n = 1
  elseif self._xaxis == 'min' then n = height end
  if n then
    -- min
    local row, beg = self._canvas[n], 0
    local s = self._x:limit('min')
    for i = 1, #s do row[beg+i] = string.sub(s, i, i) end
    -- max
    s = self._x:limit('max')
    beg = width - #s - 1
    for i = 1, #s do row[beg+i] = string.sub(s, i, i) end
    -- mid
    if width > 21 then  -- 'visual' parameter
      s = self._x:limit('mid')
      beg = (width + 1) / 2
      for i = 1, #s do row[beg+i] = string.sub(s, i, i) end
    end
    n = nil
  end
  -- vertical
  if self._yaxis == 'mid' then n = (width + 1) / 2
  elseif self._yaxis == 'min' then n = 1
  elseif self._yaxis == 'max' then n = width end
  if n then
    -- min
    local s = self._y:limit('min')
    local beg = (n == width) and (width - #s - 1) or n
    local row = self._canvas[height-1]
    for i = 1, #s do row[beg+i] = string.sub(s, i, i) end
    -- max
    s = self._y:limit('max')
    row = self._canvas[2]
    for i = 1, #s do row[beg+i] = string.sub(s, i, i) end
    -- mid
    if height > 11 then  -- 'visual' parameter
      s = self._y:limit('mid')
      row = self._canvas[(height + 1) / 2]
      for i = 1, #s do row[beg+i] = string.sub(s, i, i) end
    end
  end
end


--- Constructor example.
--  @param dwidth Figure width.
--  @param dheight Figure height.
--  @return New object of asciiplot.
asciiplot._new = function(dwidth, dheight)
  local pos = 'mid'
  local o = {
    _x = axis.new(dwidth),
    _y = axis.new(dheight),
    _z = axis.new(dwidth),
    -- axes location
    _xaxis = pos,
    _yaxis = pos,
    _zaxis = pos,
    -- automatic change scale
    _xfix = false,
    _yfix = false,
    _zfix = false,
    -- image
    _canvas = {},
    -- comments
    _legend = {},
    -- title can be added
  }
  -- return object
  return setmetatable(o, asciiplot)
end


--- Set axis settings.
--  @param t Table with parameters {range, log, view, fix, size}.
--  @param pref Prefix string.
asciiplot._setAxis = function (self, t, pref)
  -- range
  if t.range then self[pref]:setRange(t.range) end
  -- logarithmic scale
  if t.log ~= nil then self[pref]:setLog(t.log) end
  -- visualize
  if t.view ~= nil then self[pref..'axis'] = t.view end
  -- fix scale
  if t.fix ~= nil then self[pref..'fix'] = t.fix end
  -- length
  if t.size ~= nil then self[pref]:resize(t.size) end
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
--  @param tZ Table of z(x,y) values.
--  @param tOpt List of options.
--  @return Figure object.
asciiplot._viewXY = function (self, tX, tY, tZ, tOpt)
  local ZERR = 0.05  -- deflection from expected level value
  local N = tOpt.level
  local lvl, h = asciiplot._surfRange(
    self._z.range[1], self._z.range[2], N, tOpt.minmax, false)
  h = h * ZERR
    -- prepare
  asciiplot._clear(self)
  asciiplot._axes(self)
  -- fill
  local width = self._x.size
  for i = 1, self._y.size do
    local row = tZ[i]
    for j = 1, width do
      local z = row[j]
      for k = 1, N do
        if math.abs(lvl[k] - z) <= h then
          self._canvas[i][j] = asciiplot.lvls[k]
          break
        end
      end
    end
  end
  asciiplot._limits(self)
  -- legend
  asciiplot._cntLegend(self, 'Z', lvl)
  self._title = 'X-Y view'
  return self
end


--- Find XZ projection of a contour.
--  @param tX X range table.
--  @param tY Y range table.
--  @param tZ Table of z(x,y) values.
--  @param tOpt List of options.
--  @return Figure object.
asciiplot._viewXZ = function (self, tX, tY, tZ, tOpt)
  local N = tOpt.level
  local lvl, _ = asciiplot._surfRange(1, self._y.size, N, tOpt.minmax, true)
  -- copy settings
  self._y:setRange(self._z._init)
  self._y:setLog(self._z.log)
  self._yaxis = self._zaxis
    -- prepare
  asciiplot._clear(self)
  asciiplot._axes(self)
  -- fill
  for j = 1, N do
    local row = tZ[lvl[j]]
    local ch = asciiplot.lvls[N-j+1]
    for i = 1, #tX do
      asciiplot.addPoint(self, tX[i], row[i], ch)
    end
  end
  asciiplot._limits(self)
  -- legend
  local lvlXZ = {}
  for i = 1, N do lvlXZ[i] = tY[lvl[N-i+1]] end
  asciiplot._cntLegend(self, 'Y', lvlXZ)
  self._title = 'X-Z view'
  return self
end


--- Find YZ projection of a contour.
--  @param tX X range table.
--  @param tY Y range table.
--  @param tZ Table of z(x,y) values.
--  @param tOpt List of options.
--  @return Figure object.
asciiplot._viewYZ = function (self, tX, tY, tZ, tOpt)
  local N = tOpt.level
  local lvl, _ = asciiplot._surfRange(1, self._x.size, N, tOpt.minmax, true)
  local rotate = (tOpt.view == 'concat')
  -- update ranges
  if rotate then
    -- Z-Y
    self._x:setRange(self._z._init)
    self._x:setLog(self._z.log)
    self._xaxis = self._zaxis
  else
    -- Y-Z
    self._x:setRange(self._y._init)
    self._x:setLog(self._y.log)
    self._y:setRange(self._z._init)
    self._y:setLog(self._z.log)
    self._x.size, self._y.size = self._y.size, self._x.size
    self._xaxis, self._yaxis = self._yaxis, self._zaxis
  end
  -- prepare
  asciiplot._clear(self)
  asciiplot._axes(self)
  -- fill
  for i = 1, #tY do
    local y = tY[i]
    local zi = tZ[i]
    for j = #lvl, 1, -1 do
      if rotate then
        asciiplot.addPoint(self, zi[lvl[j]], y, asciiplot.lvls[j])
      else
        asciiplot.addPoint(self, y, zi[lvl[j]], asciiplot.lvls[j])
      end
    end
  end
  asciiplot._limits(self)
  -- legend
  local lvlZY = {}
  for i = 1, N do lvlZY[i] = tX[lvl[i]] end
  asciiplot._cntLegend(self, 'X', lvlZY)
  self._title = rotate and 'Z-Y view' or 'Y-Z view'
  return self
end


--- Scale and add a point to the figure.
--  @param dx Coordinate x.
--  @param dy Coordinate y.
--  @param s Character.
asciiplot.addPoint = function (self, dx, dy, s)
  local nx = self._x:proj(dx, true)
  local ny = self._y:proj(dy, false)
  if nx and ny then
    self._canvas[ny][nx] = s or '*'
  end
end
about[asciiplot.addPoint] = {"F:addPoint(x_d, y_d, char_s)",
  "Add point (x,y) using char.", MANUAL}


--- Set character to direct position.
--  @param ir Row index.
--  @param ic Column index.
--  @param s Character.
asciiplot.addPose = function (self, ir, ic, s)
  if ir > 0 and ir <= self._y.size and ic > 0 and ic <= self._x.size
            and (#s == 1 or SONATA_USE_COLOR) 
  then
    self._canvas[ir][ic] = s
  end
end
about[asciiplot.addPose] = {"F:addPose(row_N, col_N, char_s)",
  "Add character to the given position.", MANUAL}


--- Set string to the given position.
--  @param ir Row index.
--  @param ic Column index.
--  @param s String.
asciiplot.addString = function (self, ir, ic, s)
  for i = 1, #s do
    asciiplot.addPose(self, ir, ic+i-1, string.sub(s, i, i))
  end
end
about[asciiplot.addString] = {"F:addString(row_N, col_N, str)",
  "Set string from the given position.", MANUAL}


--- Get information about axes.
--  @return Table with parameters.
asciiplot.axes = function (self)
  local res = {}
  local key = asciiplot.keys
  for i = 1, 3 do
    local k = key[i]
    local a = self[k]
    res[key[i+3]] = {
      size = a.size,
      log = a.log or false,
      range = {
        a.log and (10^a.range[1]) or a.range[1],
        a.log and (10^a.range[2]) or a.range[2],
      },
      view = self[k..'axis'],
      fix = self[k..'fix'],
    }
  end
  return res
end
about[asciiplot.axes] = {"F:axes() --> tbl",
  "Get {size, log, range, view, fix} for each axis.", help.OTHER}


--- Plot bar graph.
--  @param t Data table {{x1,y1},{x2,y2}...}.
--  @param iy Index of y column (optional).
--  @param ix Index of x column (optional).
asciiplot.bar = function (self, t, iy, ix)
  ix, iy = ix or 1, iy or 2
  -- size
  local iL = mmodf(self._x.size * 0.2)
  if iL % 2 == 1 then iL = iL - 1 end
  local iR = self._x.size - iL
  local ax = self._x:copy()  -- temporary axis object
  ax:resize(iR - iL)
  -- find limits
  if not self._xfix then
    local min, max = math.huge, -math.huge
    for i = 1, #t do
      local v = t[i][iy]
      if v > max then max = v end
      if v < min then min = v end
    end
    ax:setRange({min, max})
  end
  -- data step
  local step = 1
  if #t > self._y.size then
    local int, frac = mmodf(#t / self._y.size)
    step = (frac > 0) and (int+1) or int
  end
  -- add values
  asciiplot._clear(self)
  local ch, r = '=', 1
  local lim = self._yaxis == 'min' and 1 or
    self._yaxis == 'max' and ax.size or (ax.size + 1)/2
  for i = 1, #t, step do
    local canvas = self._canvas[r]
    -- text
    local x = tostring(t[i][ix])
    for c = 1, math.min(iL-2, #x) do canvas[c] = string.sub(x, c, c) end
    -- show
    x = t[i][iy]
    local p = ax:proj(x, true)
    if not p then
      p = (x < ax.range[1]) and ax.range[1] or ax.range[2]  -- set limit
    end
    if p <= lim then
      for c = p, lim do canvas[iL + c] = ch end
    else
      for c = lim, p do canvas[iL + c] = ch end
    end
    -- value
    x = tostring(x)
    for c = 1, math.min(iL-2, #x) do
      canvas[iR+2+c] = string.sub(x, c, c)
    end
    r = r + 1
  end
end
about[asciiplot.bar] = {"F:bar(t, y_N=2, x_N=1)", "Plot bar diargram for data."}


--- Horizontal concatenation of figures.
--  @param ... List of figure objects.
--  @return String with figures.
asciiplot.concat = function (_, ...)
  -- collect info
  local ag = {...}
  local nlegend = 0
  for i, v in ipairs(ag) do
    if not isasciiplot(v) then
      error("Not asciiplot object at "..tostring(i))
    end
    if v._y.size ~= ag[1]._y.size then error('Different height') end
    local n = 0
    for _ in pairs(v._legend) do n = n + 1 end
    if n > nlegend then nlegend = n end
  end
  -- data
  local acc, gap = {}, '   '
  for _, v in ipairs(ag) do
    local k, width = 1, v._x.size
    -- title
    local row = acc[k] or {}
    row[#row+1] = asciiplot._format(v._title or '', width, true, true)
    row[#row+1] = gap
    acc[k] = row; k = k + 1
    -- content
    for j = 1, v._y.size do
      row = acc[k] or {}
      row[#row+1] = table.concat(v._canvas[j])
      row[#row+1] = gap
      acc[k] = row; k = k + 1
    end
    -- legend
    local n = 0
    local sym = {}
    for q, _ in pairs(v._legend) do sym[#sym+1] = q end
    if #sym > 0 then table.sort(sym) end
    for _, u in ipairs(sym) do
      row = acc[k] or {}
      row[#row+1] = asciiplot._format(
        string.format('(%s) %s', u, v._legend[u]), width-1+#u, false, true)
      row[#row+1] = gap
      acc[k] = row; k = k + 1
      n = n + 1
    end
    -- add empty lines
    for _ = n+1, nlegend do
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
--  @param fn Function f(x,y).
--  @param tOpt Table of options: level - number of lines, view - projection ('XY', 'XZ', 'YZ', 'concat' for its combination).
--  @return Figure object for single view or string for 'XYZ'.
asciiplot.contour = function (self, fn, tOpt)
  tOpt = tOpt or {}
  tOpt.level = tOpt.level or 5    -- TODO limit maximal and minimal values
  if tOpt.level > #asciiplot.lvls then
    error('Max levle is '..tostring(#asciiplot.lvls))
  end
  local view = tOpt.view or 'XY'
  -- calculate
  if view == 'YZ' then
    -- increase resolution for default size
    self._x.size, self._y.size = self._y.size, self._x.size
  end
  local X, Y, Z, zrng = asciiplot._fn2Z(self, fn)
  if not self._zfix then self._z:setRange(zrng) end
  if view == 'concat' then
    local XY = asciiplot._viewXY(asciiplot.copy(self), X, Y, Z, tOpt)
    local XZ = asciiplot._viewXZ(asciiplot.copy(self), X, Y, Z, tOpt)
    local YZ = asciiplot._viewYZ(asciiplot.copy(self), X, Y, Z, tOpt)
    local txt = {asciiplot.concat(nil, XY, YZ), tostring(XZ)}
    return table.concat(txt, '\n\n')
  end
  -- specific projection
  if view == 'XY' then asciiplot._viewXY(self, X, Y, Z, tOpt) end
  if view == 'XZ' then asciiplot._viewXZ(self, X, Y, Z, tOpt) end
  if view == 'YZ' then asciiplot._viewYZ(self, X, Y, Z, tOpt) end
end
about[asciiplot.contour] = {"F:contour(fn, {level=5, view='XY'}) --> nil|str",
  "Find contours of projection for a function fn(x,y). Views: XY, XZ, YZ. Use 'view=concat' for concatenated string output."}


--- Make a copy.
--  @return Copy of the object.
asciiplot.copy = function (self)
  local o = {
    _x = self._x:copy(),
    _y = self._y:copy(),
    _z = self._z:copy(),
    _title = self._title,
    _xaxis = self._xaxis,
    _yaxis = self._yaxis,
    _zaxis = self._zaxis,
    _xfix = self._xfix,
    _yfix = self._yfix,
    _zfix = self._zfix,
    _canvas = {},
    _legend = {},
  }
  for k, v in pairs(self._legend) do
    o._legend[k] = v
  end
  for i = 1, #self._canvas do
    o._canvas[i] = vmove(self._canvas[i], 1, self._x.size, 1, {})
  end
  return setmetatable(o, asciiplot)
end
about[asciiplot.copy] = {"F:copy() --> cpy_F",
  "Create a copy of the object.", help.OTHER}


--- Update legend.
--  @param str_t Table with strings.
asciiplot.legend = function (self, str_t)
  for i, c in ipairs(asciiplot.char) do
    local li = self._legend[c]
    if li then
      self._legend[c] = str_t[i] or li
    else break end
  end
end
about[asciiplot.legend] = {"F:legend(str_t)", "Update legend.", CONF}


--- Generalized plot funciton.
--  @param ... is "t1", "t1,t2", "fn", "t1,name", "t1,t2,name" etc.
--  @return The updated figure object.
asciiplot.plot = function (self, ...)
  local ag, acc = {...}, {}
  local vmin, vmax = math.huge, -math.huge
  -- collect data
  local i = 1
  repeat
    local tx, ty = ag[i], ag[i+1]
    -- data
    if type(tx) == 'function' then
      -- save funciton, check region later
      ty = nil
    elseif type(tx) == 'table' then
      if type(ty) == 'table' then
        i = i + 1
      else
        -- only Y's
        ty, tx = tx, {}
        for j = 1, #ty do tx[j] = j end
      end
      local a, b = asciiplot._findVectorRange(tx)
      if a < vmin then vmin = a end
      if b > vmax then vmax = b end
    else
      error('Unexpected argument in position '..tostring(i))
    end
    i = i + 1
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
  if not self._xfix then
    if vmin == math.huge then vmin = self._x._init[1] end
    if vmax == -math.huge then vmax = self._x._init[2] end
    self._x:setRange({vmin, vmax})
  end

  -- update y range
  vmin, vmax = math.huge, -math.huge
  for j = 1, #acc do
    local r = acc[j]
    if type(r[1]) == 'function' then
      r[1], r[2] = asciiplot._fn2XY(self, r[1])
    end
    local a, b = asciiplot._findVectorRange(r[2])
    if a < vmin then vmin = a end
    if b > vmax then vmax = b end
  end
  if not self._yfix then
    self._y:setRange({vmin, vmax})
  end

  -- prepare
  asciiplot._clear(self)
  asciiplot._axes(self)
  -- 'plot'
  for j = 1, #acc do
    local c = asciiplot.char[j]
    local r = acc[j]
    asciiplot._addXY(self, r[1], r[2], c)
    self._legend[c] = r[3]
  end
  -- limits
  asciiplot._limits(self)
end
about[asciiplot.plot] = {"F:plot(...)",
  "Plot arguments in form 't', 't1,t1', 'fn,nm', 'fn1,fn2' etc." }


--- Prepare a clear canvas.
asciiplot.reset = function (self)
  asciiplot._clear(self)
  asciiplot._axes(self)
  asciiplot._limits(self)
end
about[asciiplot.reset] = {"F:reset()", "Prepare a clear canvas.", MANUAL}


--- Scale xrange and yrange w.r.t. initial size.
--  @param factor Positive value or another figure object.
--  @return Updated figure object.
asciiplot.scale = function (self, factor)
  if isasciiplot(factor) then
    self._x:resize(factor._x.size)
    self._y:resize(factor._y.size)
  else
    assert(factor > 0)
    self._x:scale(factor)
    self._y:scale(factor)
  end
  return self
end
about[asciiplot.scale] = {"F:scale(factor_d | src_F) --> F",
  "Change figure size w.r.t. initial size.", CONF}


--- X axis settings.
--  @param t Table with parameters {range, log, view, fix}.
asciiplot.setX = function (self, t) asciiplot._setAxis(self, t, '_x') end
about[asciiplot.setX] = {"F:setX(par_t={range,view,log,fix})",
  "X axis configuration, set range ({a,b}), view ('min'/'mid'/'max'/false), logarithm (true/false), ragne fix (true/false).",
  CONF}


--- Y axis settings.
--  @param t Table with parameters {range, log, view, fix}.
asciiplot.setY = function (self, t) asciiplot._setAxis(self, t, '_y') end
about[asciiplot.setY] = {"F:setY(par_t={range,view,log,fix})",
  "Y axis configuration, set range ({a,b}), view ('min'/'mid'/'max'/false), logarithm (true/false), range fix (true/false).",
  CONF}


--- Z axis settings.
--  @param t Table with parameters {range, log, view, fix}.
asciiplot.setZ = function (self, t) asciiplot._setAxis(self, t, '_z') end
about[asciiplot.setZ] = {"F:setZ(par_t={range,view,log,fix})",
  "Z axis configuration, set range ({a,b}), view ('min'/'mid'/'max'/false), logarithm (true/false), range fix (true/false).",
  CONF}


--- Set title.
--  @param s New title.
asciiplot.title = function (self, s) self._title = s end
about[asciiplot.title] = {"F:title(str)", "Set new title.", CONF}


--- Plot data represented in form of table
--  {{x1,y11,y12,...}, {x2,y21,y22,...}, ...}
--  @param t Data table.
--  @param tOpt (Optional) Table of column indices.
--  @return The updated figure object.
asciiplot.tplot = function (self, t, tOpt)
  tOpt = tOpt or {}
  tOpt.x = tOpt.x or 1  -- choose independent variable
  -- plot all by default
  if #tOpt == 0 then
    for i = 1, #t[1] do 
      if i ~= tOpt.x then tOpt[#tOpt+1] = i end
    end
  end
  -- check type
  if tOpt.polar then
    return asciiplot._addPolar(self, t, tOpt)
  end
  -- update range
  if not self._yfix then
    local rx, ry = asciiplot._findRange(t, tOpt)
    self._x:setRange(rx)
    self._y:setRange(ry)
  end
  -- prepare
  asciiplot._clear(self)
  asciiplot._axes(self)
  -- fill
  asciiplot._addTable(self, t, tOpt)
  -- limits
  asciiplot._limits(self)
end
about[asciiplot.tplot] = {"F:tplot(data_t, cols_t={x=1, polar=false})",
  "Plot the table data, choose columns if need."}


-- Simplify the constructor call.
setmetatable(asciiplot, {
__call = function (self, w, h)
  -- size
  return asciiplot._new(w or asciiplot.WIDTH, h or asciiplot.HEIGHT)
end})
about[asciiplot] = {" (width_N=73, height_N=21) --> new_F",
  "Create new asciiplot.", help.STATIC}


if Sonata then  -- GLOBAL

-- Define simplified function call
Plot = function (...)
  local ap = Ap or require('matlib.asciiplot')
  local f = ap()
  f._x:setRange({-5, 5})
  f:plot(...)
  return tostring(f)
end
about[Plot] = {"Plot(...)",
  "Plot arguments in form 't', 't1,t1', 'fn,nm', 'fn1,fn2' etc.", help.OTHER}

end  -- GLOBAL


-- Comment to remove descriptions
asciiplot.about = about

return asciiplot

--======================================
-- FIX contour concatenation when use color
