--[[		sonata/lib/asciiplot.lua

--- Use pseudography for data visualization.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2025.

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
x = {1,2,3,4,5}
y = {1,3,5,7,9}
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
fig2:tplot(tbl, {sym='*+'})
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
fig3 = Ap()
fig3:scale(0.5)
-- no axes and limits
fig3:setX {range={-2,2}, view=false}
fig3:setY {range={-1,4}, view=false}
fig3:reset()
-- set function
for x = -1.2, 1.2, 0.1 do
  fig3:addPoint(x, x*x-0.5, '*')
end
-- set to position
fig3:addPose(3, 13, '#')   -- characters
fig3:addPose(3, 24, '#')
fig3:addString(2,3,'Hi!')  -- text
fig3:addLine(0, 1, 0, 1.5, '|')
print(fig3)

-- print surface contours
fig4 = Ap()
fig4:setX {range={-5, 5}}
fig4:setY {range={-5, 5}}
fig4:contour(function (x,y) return x*x - y*y end)
print(fig4)

-- bar diagram
tx, ty = {}, {}
k = 2*3.14/20
for i = 1, 20 do tx[i]=k*i; ty[i]=math.sin(k*i) end
fig5 = Ap()
fig5:bar(tx, ty)
print(fig5)

-- blocks
fig7 = Ap()
tx = {'v1', 'v2', 'v3', 'v4'}
ty = {10, 5, 2, 3}
fig7:blocks(tx, ty)
print(fig7)

-- use log scale
fig6 = Ap()
fig6:setX {range={-10, 10}}
fig6:setY {log=true}
fig6:plot(math.exp)
fig6:legend 'off'
print(fig6)

--]]

--	LOCAL

local _utils = require("matlib/utils")

local _vmove = _utils.versions.move
local _czero = _utils.cross.isZero
_utils = _utils.utils
local _tf = require("matlib.asciiplot_tf")

local _inform = Sonata and Sonata.warning or print
local _modf, _log, _floor = math.modf, math.log, math.floor
local _tag = { MANUAL='manual', CONF='settings' }
local _sonata_use_color = SONATA_USE_COLOR

--	INFO

local _help = SonataHelp or {}
-- description
local _about = {
__module__ = "Use pseudography for data visualization."
}


--	MODULE

-- Axis object
local axis = {}
axis.__index = axis


--- Create new axis.
--  @param N Required width.
--  @return axis object.
local function _axisNew (N)
  if N < 9 then
    _inform('Set minimal axis size:'..tostring(N))
    N = 9
  elseif N % 2 == 0 then
    _inform('Change to odd size')
    return _axisNew(N+1)
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


--- Find number position maping on the axis.
--  @param d Number for projection.
--  @param isX Flag of direct projection.
--  @return Position (index) or nil if out of range.
axis.proj = function (self, d, isX)
  d = self.log and _log(d, 10) or d
  local d0 = self.range[1]
  if d < d0 or d > self.range[2] then return nil end
  local dx = (d - d0) / self.diff
  return _modf(
    isX and ((self.size - 1)*dx + 1) or (dx - self.size*(dx - 1)))
end


--- Update axis width.
--  @param N New size.
axis.resize = function (self, N)
  if N < 9 then
    _inform('Set minimal axis size:'..tostring(N))
    N = 9
  elseif N % 2 == 0 then
    _inform('Change to odd size')
    return axis.resize(self, N+1)
  end
  self.size = N
  self._size = N
end


--- Update axis range.
--  @param t New range {min, max}.
axis.setRange = function (self, t)
  local a, b = t[1], t[#t]
  -- check limits
  if a == b then
    error 'Wrong range'
  end
  if a > b then
    a, b = b, a
  end
  if self.log then
    -- lorarithm mode
    if b <= 0 then b = 1 end
    if a <= 0 then a = b / 10 end
    local p1 = _log(a, 10)
    p1 = (p1 > 0) and math.ceil(p1) or _floor(p1)
    local p2 = _log(b, 10)
    p2 = (p2 > 0) and math.ceil(p2) or _floor(p2)
    self.range[1], self.range[2] = p1, p2
  else
    -- normal mode
    self.range[1], self.range[2] = a, b
    local n = _log(b - a, 10)
    n = (n >= 0) and _floor(n) or math.ceil(n)
    local tol = 10^(n-1)
    local v, rest = _modf(a / tol)
    if rest ~= 0 then
      rest = (rest > 0) and 0 or 1
      self.range[1] = (v - rest) * tol
    end
    v, rest = _modf(b / tol)
    if rest ~= 0 then
      rest = (rest < 0) and 0 or 1
      self.range[2] = (v + rest) * tol
    end
  end
  if a ~= t[1] or b ~= t[2] then
    _inform(string.format('Change limits to %f %f', a, b))
  end
  self._init[1], self._init[2] = a, b  -- exact limits
  self.diff = self.range[2] - self.range[1]
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
  local int, frac = _modf(self._size * factor)
  axis.resize(self, (int % 2 == 1) and int or (int + 1))
end


--- Move over the axis elements.
--  @param isX Flag of direct iteration.
--  @return Iterator, it returns index and value.
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
OUTLIERS = 2,  -- skip outliers over 2*std, ignore when non-positive
-- symbols
lvls = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'},
keys = {'_x','_y','_z','x','y','z'},
-- idea from
-- https://github.com/adam-younes/calculator
GRADE = {[0]="`", "'", "*", "~", "-", ".", ",", "_"}
}


-- update markers
if _sonata_use_color then
  local char = asciiplot.lvls
  for k, v in ipairs(char) do
    char[k] = string.format('\x1B[3%dm%s\x1B[0m', k, v)
  end
end


--- Check object type.
--  @param v Object.
--  @return True if the object is asciiplot.
local function _isasciiplot(v) return getmetatable(v) == asciiplot end


--- Check if the object can be called.
--  @param f Object to check.
--  @return true when function or has method __call.
local function _callable (f)
  return type(f) == 'function' or (type(f) == 'table' and f.__call)
end


--- Add symbol based on its height.
--  @param fig asciiplot object.
--  @param x Coordinate x.
--  @param y Coordinate y.
--  @param ind Color index.
local function _addGraded (fig, x, y, ind)
  local nx, fx = fig._x:proj(x, true)
  local ny, fy = fig._y:proj(y, false)
  if not (nx and ny) then return end
  nx = (fx > 0.5) and (nx + 1) or nx
  fy = fy + 0.5
  if fy > 1 then
    fy, ny = fy - 1, ny + 1
  end
  -- fy in (0, 1), multiply to (#GRADE - small_value)
  local k = _floor(fy*7.99)
  local s = asciiplot.GRADE[k]
  if _sonata_use_color then
    s = string.format('\x1B[3%dm%s\x1B[0m', ind, s)
  end
  fig._canvas[ny][nx] = s
end


--- Transform points to polar system, add to figure.
--  @param fig asciiplot object.
--  @param t Data table.
--  @param tOpt List of column numbers.
local function _addPolar (fig, t, tOpt)
  local acc = {}
  -- transform data
  local ii = tOpt.x
  for i, row in ipairs(t) do
    local s, c = math.sin(row[ii]), math.cos(row[ii])
    for j, ind in ipairs(tOpt) do
      local curr = acc[j] or {}
      curr[i] = {row[ind]*c, row[ind]*s}
      acc[j] = curr
    end
  end
  -- update range
  if not fig._yfix then
    local rxx, ryy = {}, {}
    for j = 1, #tOpt do
      local rx, ry = _tf.findRange(acc[j], {})
      rxx[1] = math.min(rxx[1] or rx[1], rx[1])
      rxx[2] = math.max(rxx[2] or rx[2], rx[2])
      ryy[1] = math.min(ryy[1] or ry[1], ry[1])
      ryy[2] = math.max(ryy[2] or ry[2], ry[2])
    end
    fig._x:setRange(rxx)
    fig._y:setRange(ryy)
  end
  -- prepare
  asciiplot._clear(fig)
  asciiplot._axes(fig)
  -- fill
  local sym = tOpt.sym
  if sym and #sym == 0 then sym = nil end
  for j = 1, #tOpt do
    local c = sym and (j <= #sym and string.sub(sym, j, j) or string.sub(sym, -1, -1))
    local xy = acc[j]
    for _, v in ipairs(xy) do
      if c then
        asciiplot.addPoint(fig, v[1], v[2], c)
      else
        _addGraded(fig, v[1], v[2], j)
      end
    end
    local s = string.char(string.byte('A') - 1 + j)
    if _sonata_use_color then
      s = string.format('\x1B[3%dm%s\x1B[0m', j, s)
    end
    for i = #xy, 1, -1 do
      if asciiplot.addPoint(fig, xy[i][1], xy[i][2], s) then break end
    end
    fig._legend[s] = 'column '..tostring(tOpt[j])
  end
  -- limits
  asciiplot._limits(fig)
  return fig
end


--- Add points from a table.
--  First element of each row is x, the rest are yi.
--  @param fig asciiplot object.
--  @param t Table to print.
--  @param tInd Allows to choose y columns.
local function _addTable (fig, t, tInd)
  local x, sym = tInd.x, tInd.sym
  if sym and #sym == 0 then sym = nil end
  for j = 1, #tInd do
    local c = sym and (j <= #sym and string.sub(sym, j, j) or string.sub(sym, -1, -1))
    local k = tInd[j]
    for i = 1, #t do
      local row = t[i]
      if c then
        asciiplot.addPoint(fig, row[x], row[k], c)
      else
        _addGraded(fig, row[x], row[k], j)
      end
    end
    local s = string.char(string.byte('A') - 1 + j)
    if _sonata_use_color then
      s = string.format('\x1B[3%dm%s\x1B[0m', j, s)
    end
    for i = #t, 1, -1 do
      local row = t[i]
      if asciiplot.addPoint(fig, row[x], row[k], s) then break end
    end
    fig._legend[s] = 'column '..tostring(k)   -- default legend
  end
end


--- Add group of points.
--  @param fig asciiplot object.
--  @param tX List of x coordinates.
--  @param tY List of y coordinates.
local function _addXY (fig, tX, tY, ind)
  for i = 1, #tX do
    _addGraded(fig, tX[i], tY[i], ind)
  end
  -- legend
  local s = string.char(string.byte('A') - 1 + ind)
  if _sonata_use_color then
    s = string.format('\x1B[3%dm%s\x1B[0m', ind, s)
  end
  for i = #tX, 1, -1 do
    if asciiplot.addPoint(fig, tX[i], tY[i], s) then break end
  end
  return s
end


--- Fill legend for contour object.
--  @param fig asciiplot object.
--  @param s Axis name.
--  @param lvl List of levels.
local function _cntLegend (fig, s, lvl)
  local j, line = 1, {}
  for i = 1, #lvl do
    -- group levels
    line[#line+1] = string.format('%s(%.2f)', asciiplot.lvls[i], lvl[i])
    if i % 3 == 0 then
      fig._legend[s..tostring(j)] = table.concat(line, '  ')
      j, line = j + 1, {}
    end
  end
  if #line > 0 then
    fig._legend[s..tostring(j)] = table.concat(line, '  ')
  end
end


--- Find vectors X and Y for the given function.
--  @param fig asciiplot object.
--  @param fn Function f(x).
--  @return Lists of coordinates X and Y.
local function _fn2XY (fig, fn)
  local X, Y = {}, {}
  for i, x in fig._x:values(true) do
    X[i] = x
    Y[i] = fn(x)
  end
  return X, Y
end


--- Find height for each pair (x, y).
--  @param fig asciiplot object.
--  @param fn Function f(x,y).
--  @return Vectors X, Y, 'matrix' Z, range of heights.
local function _fn2Z (fig, fn)
  local X, Y, Z = {}, {}, {}
  for i, x in fig._x:values(true) do X[i] = x end
  local zmin, zmax = math.huge, -math.huge
  for ny, y in fig._y:values(false) do
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


--- Fill block or sub-blocks.
--  @param fig asciiplot object.
--  @param node Tree node.
--  @param w1, w2 Start and end of width.
--  @param h1, h2 Start and end of height.
--  @param wdiv Flag to divide width.
--  @param acc Accumulator table.
local function _fillBlock (fig, node, w1, w2, h1, h2, wdiv, acc)
  if node.isleaf then
    -- draw
    local ch = string.char(string.byte('A') + #acc)
    if _sonata_use_color then
      ch = string.format('\x1B[3%dm%s\x1B[0m', (#acc+1) % 9, ch)
    end
    acc[#acc+1] = {ch, node.left}
    local nw1, fw1 = fig._x:proj(w1, true)
    nw1 = (fw1 > 0.5) and (nw1 + 1) or nw1
    local nw2, fw2 = fig._x:proj(w2, true)
    nw2 = (fw2 > 0.5) and (nw2 + 1) or nw2
    local nh1, fh1 = fig._y:proj(h1, false)
    nh1 = (fh1 > 0.5) and (nh1 + 1) or nh1
    local nh2, fh2 = fig._y:proj(h2, false)
    nh2 = (fh2 > 0.5) and (nh2 + 1) or nh2
    for ny = nh2, nh1 do
      local row = fig._canvas[ny]
      for nx = nw1, nw2 do row[nx] = ch end
    end
  else
    -- visit branches
    local kl = node.left.val / node.val
    if wdiv then
      local m = w1 + kl*(w2 - w1)
      _fillBlock(fig, node.left, w1, m, h1, h2, false, acc)
      _fillBlock(fig, node.right, m, w2, h1, h2, false, acc)
    else
      local m = h1 + kl*(h2 - h1)
      _fillBlock(fig, node.left, w1, w2, h1, m, true, acc)
      _fillBlock(fig, node.right, w1, w2, m, h2, true, acc)
    end
  end
end


--- Set axis settings.
--  @param fig asciiplot object.
--  @param t Table with parameters {range, log, view, fix, size}.
--  @param pref Prefix string.
local function _setAxis (fig, t, pref)
  -- range
  if t.range then fig[pref]:setRange(t.range) end
  -- logarithmic scale
  if t.log ~= nil then fig[pref]:setLog(t.log) end
  -- visualize
  if t.view ~= nil then fig[pref..'axis'] = t.view end
  -- fix scale
  if t.fix ~= nil then fig[pref..'fix'] = t.fix end
  -- length
  if t.size ~= nil then fig[pref]:resize(t.size) end
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
  if self._title then acc[1] = self._title end
  -- figure
  for i = 1, #self._canvas do
    acc[#acc+1] = table.concat(self._canvas[i])
  end
  -- legend
  if not self._legend._hide then
    local sym = {}
    for k, _ in pairs(self._legend) do sym[#sym+1] = k end
    if #sym > 0 then table.sort(sym) end
    for _, k in ipairs(sym) do
      acc[#acc+1] = string.format("(%s) %s", k, self._legend[k])
    end
  end
  return table.concat(acc, '\n')
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
    for i = 1, self._y.size do self._canvas[i][n] = vertical end
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
    for i = 1, self._x.size do row[i] = horizontal end
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
    -- clear the rest
    for j = #row, width+1, -1 do row[j] = nil end
    self._canvas[i] = row
  end
  -- remove the rest of rows
  for i = #self._canvas, self._y.size+1, -1 do self._canvas[i] = nil end
  self._legend = {}
  self._title = nil
end


--- Add xrange and yrange.
asciiplot._limits = function (self)
  -- horizontal
  local width, height = self._x.size, self._y.size
  local n = nil
  if     self._xaxis == 'mid' then n = (height + 3) / 2
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
      beg = (width + 1)/2
      for i = 1, #s do row[beg+i] = string.sub(s, i, i) end
    end
    n = nil
  end
  -- vertical
  if     self._yaxis == 'mid' then n = (width + 1) / 2
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
      row = self._canvas[(height + 1)/2]
      for i = 1, #s do row[beg+i] = string.sub(s, i, i) end
    end
  end
end


--- Object constructor.
--  @param dwidth Figure width.
--  @param dheight Figure height.
--  @return New object of asciiplot.
asciiplot._new = function(dwidth, dheight)
  local pos = 'mid'
  local o = {
    _x = _axisNew(dwidth),
    _y = _axisNew(dheight),
    _z = _axisNew(dwidth),
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
    -- last call
    _lastFn = nil,
    _lastArgs = nil,
    -- comments
    _legend = {},
    -- title can be added
  }
  -- return object
  return setmetatable(o, asciiplot)
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
  local lvl, h = _tf.surfRange(
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
  _cntLegend(self, 'Z', lvl)
  self._title = _tf.format('X-Y view', self._x.size, true, false)
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
  local lvl, _ = _tf.surfRange(1, self._y.size, N, tOpt.minmax, true)
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
  _cntLegend(self, 'Y', lvlXZ)
  self._title = _tf.format('X-Z view', self._x.size, true, false)
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
  local lvl, _ = _tf.surfRange(1, self._x.size, N, tOpt.minmax, true)
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
  _cntLegend(self, 'X', lvlZY)
  self._title = _tf.format(
    rotate and 'Z-Y view' or 'Y-Z view', self._x.size, true, false)
  return self
end


--- Plot line from one point to another.
--  @param x1 First point x coordinate.
--  @param y1 First point y coordinate.
--  @param x2 Second point x coordinate.
--  @param y2 Second point y coordinate.
--  @param s Character.
asciiplot.addLine = function (self, x1, y1, x2, y2, s)
  local dx, dy = x2-x1, y2-y1
  if math.abs(dx) >= math.abs(dy) then
    -- along x axis
    if dx == 0 then return end
    local step = (self._x.diff / self._x.size) * (dx >= 0 and 1 or -1)
    local k = dy/dx
    for x = x1, x2, step do self:addPoint(x, y1 + k*(x-x1), s) end
  else
    -- along y axis
    local step = (self._y.diff / self._y.size) * (dy >= 0 and 1 or -1)
    local k = dx/dy
    for y = y1, y2, step do self:addPoint(x1 + k*(y-y1), y, s) end
  end
end
_about[asciiplot.addLine] = {"F:addLine(x1_d, y1_d, x2_d, y2_d, char_s='*')",
  "Add line from (x1,y1) to (x2,y2).", _tag.MANUAL}


--- Scale and add a point to the figure.
--  @param dx Coordinate x.
--  @param dy Coordinate y.
--  @param s Character.
--  @return true when add point
asciiplot.addPoint = function (self, dx, dy, s)
  local nx, fx = self._x:proj(dx, true)
  local ny, fy = self._y:proj(dy, false)
  if nx and ny then
    nx = (fx > 0.5) and (nx + 1) or nx
    ny = (fy > 0.5) and (ny + 1) or ny
    self._canvas[ny][nx] = s or '*'
    return true
  end
  return false
end
_about[asciiplot.addPoint] = {"F:addPoint(x_d, y_d, char_s='*')",
  "Add point (x,y) using char.", _tag.MANUAL}


--- Set character to direct position.
--  @param ir Row index.
--  @param ic Column index.
--  @param s Character.
asciiplot.addPose = function (self, ir, ic, s)
  if ir > 0 and ir <= self._y.size and ic > 0 and ic <= self._x.size
            and (#s == 1 or _sonata_use_color)
  then
    self._canvas[ir][ic] = s or '*'
  end
end
_about[asciiplot.addPose] = {"F:addPose(row_N, col_N, char_s='*')",
  "Add character to the given position.", _tag.MANUAL}


--- Set string to the given position.
--  @param ir Row index.
--  @param ic Column index.
--  @param s String.
asciiplot.addString = function (self, ir, ic, s)
  for i = 1, #s do
    asciiplot.addPose(self, ir, ic+i-1, string.sub(s, i, i))
  end
end
_about[asciiplot.addString] = {"F:addString(row_N, col_N, str)",
  "Add string to the given position.", _tag.MANUAL}


--- Get information _about axes.
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
_about[asciiplot.axes] = {"F:axes() --> tbl",
  "Get {'size','log','range','view','fix'} for each axis.", _help.OTHER}


--- Plot bar graph.
--  @param tx X list (optional).
--  @param ty Y list.
--  @return updated figure object.
asciiplot.bar = function (self, tx, ty)
  -- check arguments
  if not ty then
    ty = {}
    for i = 1, #tx do ty[i] = i end
    tx, ty = ty, tx
  end
  if #tx ~= #ty then error("Different list size") end
  -- size
  local iL = _modf(self._x.size * 0.2)
  if iL % 2 == 1 then iL = iL - 1 end
  local iR = self._x.size - iL
  local ax = self._x:copy()  -- temporary axis object
  ax:resize(iR - iL)
  -- find limits
  if not self._xfix then
    local min, max = math.huge, -math.huge
    for i = 1, #ty do
      local v = ty[i]
      if v > max then max = v end
      if v < min then min = v end
    end
    ax:setRange({min, max})
  end
  -- data step
  local step = 1
  if #ty > self._y.size then
    local int, frac = _modf(#ty / self._y.size)
    step = (frac > 0) and (int+1) or int
  end
  -- add values
  asciiplot._clear(self)
  local ch, r = '=', 1
  local lim = self._xaxis == 'min' and 1 or
    self._xaxis == 'max' and ax.size or (ax.size + 1)/2
  local mmin, ssub = math.min, string.sub
  for i = 1, #ty, step do
    local canvas = self._canvas[r]
    -- text
    local x = tostring(tx[i])
    for c = 1, mmin(iL-2, #x) do canvas[c] = ssub(x, c, c) end
    -- show
    x = ty[i]
    local p, fp = ax:proj(x, true)
    if not p then
      p = (x < ax.range[1]) and ax.range[1] or ax.range[2]  -- set limit
    elseif fp > 0.5 then
      p = p + 1
    end
    if p <= lim then
      for c = p, lim do canvas[iL + c] = ch end
    else
      for c = lim, p do canvas[iL + c] = ch end
    end
    -- value
    x = tostring(x)
    for c = 1, mmin(iL-2, #x) do canvas[iR+2+c] = ssub(x, c, c) end
    r = r + 1
  end
  -- save and return
  self._lastFn = 'bar'
  self._lastArgs = {tx, ty}
  return self
end
_about[asciiplot.bar] = {"F:bar([tx,] ty) --> F",
  "Plot bar diargram for the given data."}


--- Show relation with blocks.
--  @param tx X list (optional).
--  @param ty Y list.
--  @return updated figure object.
asciiplot.blocks = function (self, tx, ty)
  -- check arguments
  if #tx == 0 then return self end
  if not ty then
    ty = {}
    for i = 1, #tx do ty[i] = i end
    tx, ty = ty, tx
  end
  if #tx ~= #ty then error("Different list size") end
  -- make weighted tree
  local tree = _tf.treeInit(tx[1], nil, ty[1])
  for i = 2, #tx do
    _tf.treeAdd(tree, tx[i], ty[i])
  end
  -- draw
  local acc = {}
  asciiplot._clear(self)
  _fillBlock(self, tree,
    self._x.range[1], self._x.range[2],
    self._y.range[1], self._y.range[2],
    true, acc)
  -- legend
  local i, s = 1, {}
  for n, p in ipairs(acc) do
    s[#s+1] = string.format('%s:%s', p[1], tostring(p[2]))
    if n % 4 == 0 then
      self._legend['L'..tostring(i)] = table.concat(s, ' ')
      i, s = i+1, {}
    end
  end
  return self
end
_about[asciiplot.blocks] = {"F:blocks([tx,] ty) --> F",
  "Plot data relations with blocks."}


--- Horizontal concatenation of 2 figures.
--  @param F1 First figure.
--  @param F2 Second figure.
--  @return String with figures.
asciiplot.concat = function (_, F1, F2)
  if not (_isasciiplot(F1) and _isasciiplot(F2)) then
    error 'Not asciiplot objects'
  end
  if F1._y.size ~= F2._y.size then
    error 'Different size'
  end
  local acc, gap = {}, '   '
  local w1, w2 = F1._x.size, F2._x.size
  -- title
  acc[1] = string.format('%s%s%s', F1._title, gap, F2._title)
  -- content
  for j = 1, F1._y.size do
    acc[#acc+1] = string.format('%s%s%s',
      table.concat(F1._canvas[j]), gap, table.concat(F2._canvas[j]))
  end
  -- legend
  local l1, l2 = {}, {}
  for k, v in pairs(F1._legend) do
    l1[#l1+1] = _tf.format(
      string.format('(%s) %s', k, v), w1-1+#k, false, true)
  end
  table.sort(l1)
  for k, v in pairs(F2._legend) do
    l2[#l2+1] = _tf.format(
      string.format('(%s) %s', k, v), w2-1+#k, false, true)
  end
  table.sort(l2)
  for i = 1, math.max(#l1, #l2) do
    acc[#acc+1] = string.format('%s%s%s',
      l1[i] or string.rep(' ', w1), gap, l2[i] or string.rep(' ', w2))
  end

  return table.concat(acc, '\n')
end
_about[asciiplot.concat] = {":concat(F1, F2) --> str",
  "Horizontal concatenation of figures with the same height. Equal to F1..F2.",
  _help.STATIC}


--- Plot function of two arguments using contours.
--  @param fn Function f(x,y).
--  @param tOpt Table of options: level - number of lines, view - projection ('XY', 'XZ', 'YZ').
--  @return updated figure object.
asciiplot.contour = function (self, fn, tOpt)
  tOpt = tOpt or {}
  tOpt.level = tOpt.level or 5
  if tOpt.level > #asciiplot.lvls then
    error ('Max levle is '..tostring(#asciiplot.lvls))
  elseif tOpt.level < 1 then
    error 'Too small levels'
  end
  local view = tOpt.view or 'XY'
  -- calculate
  if view == 'YZ' then
    -- increase resolution for default size
    self._x.size, self._y.size = self._y.size, self._x.size
  end
  local X, Y, Z, zrng = _fn2Z(self, fn)
  if not self._zfix then self._z:setRange(zrng) end
  -- specific projection
  local make = assert(asciiplot['_view'..view], "Wrong view "..view)
  local fig = asciiplot.copy(self)
  make(fig, X, Y, Z, tOpt)
  self._title = fig._title
  self._canvas = fig._canvas
  self._legend = fig._legend
  -- save and return
  self._lastFn = 'contour'
  self._lastArgs = {fn, tOpt}
  return self
end
_about[asciiplot.contour] = {"F:contour(fn, {level=5, view='XY'}) --> F",
  "Find contours of projection for a function fn(x,y). Views: XY, XZ, YZ."}


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
    o._canvas[i] = _vmove(self._canvas[i], 1, self._x.size, 1, {})
  end
  return setmetatable(o, asciiplot)
end
_about[asciiplot.copy] = {"F:copy() --> cpy_F",
  "Create a copy of the object.", _help.OTHER}


--- Update legend.
--  @param str_t Table with strings.
asciiplot.legend = function (self, str_t)
  if str_t == 'off' then
    self._legend._hide = true
    return
  end
  self._legend._hide = nil
  if str_t == 'on' then return end
  local ch = {}
  for i = 1, #str_t do
    local s = string.char(string.byte('A') - 1 + i)
    if _sonata_use_color then
      s = string.format('\x1B[3%dm%s\x1B[0m', i, s)
    end
    ch[#ch+1] = s
  end
  for i, c in ipairs(ch) do
    local li = self._legend[c]
    if li then
      self._legend[c] = str_t[i] or li
    else break end
  end
end
_about[asciiplot.legend] = {"F:legend(str_t|flag_s)",
  "Update legend. Use off/on to hide or show the legend.", _tag.CONF}


--- Generalized plot function.
--  @param ... is "t1", "t1,t2", "fn", "t1,name", "t1,t2,name" etc.
--  @return updated figure object.
asciiplot.plot = function (self, ...)
  local ag, acc = {...}, {}
  local vmin, vmax = math.huge, -math.huge
  -- collect data
  local i = 1
  repeat
    local tx, ty = ag[i], ag[i+1]
    -- data
    if _callable(tx) then
      -- save function, check region later
      ty = nil
    elseif type(tx) == 'table' then
      if type(ty) == 'table' then
        i = i + 1
      else
        -- only Y's
        ty, tx = tx, {}
        for j = 1, #ty do tx[j] = j end
      end
      local a, b = _tf.findVectorRange(tx)
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
  local isfun = {}
  for j = 1, #acc do
    local r = acc[j]
    if _callable(r[1]) then
      isfun[j] = true
      r[1], r[2] = _fn2XY(self, r[1])
    end
  end
  if not self._yfix then
    vmin, vmax = math.huge, -math.huge
    for j = 1, #acc do
      local r = acc[j]
      local a, b = _tf.findVectorRange(r[2])
      if isfun[j] and self.OUTLIERS > 0 then
        -- skip outliers
        local mean, std = _tf.statistics(r[2])
        a = math.max(a, mean - self.OUTLIERS*std)
        b = math.min(b, mean + self.OUTLIERS*std)
      end
      if a < vmin then vmin = a end
      if b > vmax then vmax = b end
    end
    self._y:setRange({vmin, vmax})
  end

  -- prepare
  asciiplot._clear(self)
  asciiplot._axes(self)
  -- 'plot'
  for j = 1, #acc do
    local r = acc[j]
    local c = _addXY(self, r[1], r[2], j)
    self._legend[c] = r[3]
  end
  -- limits
  asciiplot._limits(self)
  -- save and return
  self._lastFn = 'plot'
  self._lastArgs = ag
  return self
end
_about[asciiplot.plot] = {"F:plot(...) --> F",
  "Plot arguments in form 't', 't1,t1', 'fn,nm', 'fn1,fn2' etc." }


--- Repeat last draw command.
--  @return updated figure object or nil.
asciiplot.redraw = function (self)
  if self._lastFn then
    return asciiplot[self._lastFn](self, table.unpack(self._lastArgs))
  end
end
asciiplot.__bnot = asciiplot.redraw
_about[asciiplot.plot] = {"F:redraw() --> F|nil",
  "Apply the last draw command. Equal to ~F.", _help.OTHER}


--- Prepare a clear canvas.
asciiplot.reset = function (self)
  asciiplot._clear(self)
  asciiplot._axes(self)
  asciiplot._limits(self)
end
_about[asciiplot.reset] = {"F:reset()", "Prepare a clean canvas.", _tag.MANUAL}


--- Scale xrange and yrange w.r.t. initial size.
--  @param factor Positive value or another figure object.
--  @return Updated figure object.
asciiplot.scale = function (self, factor)
  if _isasciiplot(factor) then
    self._x:resize(factor._x.size)
    self._y:resize(factor._y.size)
  else
    assert(factor > 0)
    self._x:scale(factor)
    self._y:scale(factor)
  end
end
_about[asciiplot.scale] = {"F:scale(factor_d | src_F)",
  "Change figure size w.r.t. initial size.", _tag.CONF}


--- X axis settings.
--  @param t Table with parameters {range, log, view, fix}.
asciiplot.setX = function (self, t) _setAxis(self, t, '_x') end
_about[asciiplot.setX] = {"F:setX(par_t)",
  "X axis configuration, set 'range' ({a,b}), 'view' ('min'/'mid'/'max'/false), 'log'-arithm (true/false), 'fix' range (true/false), 'size'.",
  _tag.CONF}


--- Y axis settings.
--  @param t Table with parameters {range, log, view, fix}.
asciiplot.setY = function (self, t) _setAxis(self, t, '_y') end
_about[asciiplot.setY] = {"F:setY(par_t)",
  "Y axis configuration, set 'range' ({a,b}), 'view' ('min'/'mid'/'max'/false), 'log'-arithm (true/false), 'fix' range (true/false), 'size'.",
  _tag.CONF}


--- Z axis settings.
--  @param t Table with parameters {range, log, view, fix}.
asciiplot.setZ = function (self, t) _setAxis(self, t, '_z') end
_about[asciiplot.setZ] = {"F:setZ(par_t)",
  "Z axis configuration, set 'range' ({a,b}), 'view' ('min'/'mid'/'max'/false), 'log'-arithm (true/false), 'fix' range (true/false), 'size'.",
  _tag.CONF}


--- Visualize matrix elements
--  @param fn Condition function, returns true/false.
--  @return String with stars when condition is true.
asciiplot.stars = function (_, M, fn)
  fn = fn or function (x) return not _czero(x) end
  local acc, row, ncol = {}, {}, M:cols()
  for r = 1, M:rows() do
    local mr = M[r]
    for c = 1, ncol do
      row[c] = fn(mr[c]) and '*' or ' '
    end
    row[ncol+1] = '|'
    acc[r] = table.concat(row)
  end
  return table.concat(acc, '\n')
end
-- Deprecated


--- Set title.
--  @param s New title.
asciiplot.title = function (self, s)
  self._title = _tf.format(s, self._x.size, true, false)
end
_about[asciiplot.title] = {"F:title(str)", "Set new title.", _tag.CONF}


--- Plot data represented in form of table
--  {{x1,y11,y12,...}, {x2,y21,y22,...}, ...}
--  @param t Data table.
--  @param tOpt (Optional) Table of column indices.
--  @return updated figure object.
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
    return _addPolar(self, t, tOpt)
  end
  -- update range
  if not self._yfix then
    local rx, ry = _tf.findRange(t, tOpt)
    self._x:setRange(rx)
    self._y:setRange(ry)
  end
  -- prepare
  asciiplot._clear(self)
  asciiplot._axes(self)
  -- fill
  _addTable(self, t, tOpt)
  -- limits
  asciiplot._limits(self)
  -- save and return
  self._lastFn = 'tplot'
  self._lastArgs = {t, tOpt}
  return self
end
_about[asciiplot.tplot] = {"F:tplot(data_t, cols_t={x=1, polar=false, sym=nil}) --> F",
  "Plot the table data, choose columns if need."}


-- Simplify the constructor call.
setmetatable(asciiplot, {
__call = function (_, w, h)
  -- size
  return asciiplot._new(w or asciiplot.WIDTH, h or asciiplot.HEIGHT)
end})
_about[asciiplot] = {" (width_N=73, height_N=21) --> new_F",
  "Create new asciiplot.", _help.STATIC}


-- Comment to remove descriptions
asciiplot.about = _about
-- clear load data
_tag = nil

return asciiplot

--======================================
-- FIX contour concatenation when use color
-- TODO allow contour level definitions

