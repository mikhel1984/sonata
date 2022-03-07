--[[		sonata/lib/asciiplot.lua

--- Use pseudography for data visualization.

--  @author Stanislav Mikhel, 2022

	module 'asciiplot'
--]]

--[[TEST

-- use 'asciiplot'
Ap = require 'lib.asciiplot'

-- figure with default size
fig1 = Ap()
print(fig1.width, fig1.height)

-- print functions 
fig1.xrange = {-3.14, 3.14}   -- default is (-1,1)
fig1:plot(math.sin, 'sin', math.cos, 'cos')
fig1.title = 'Trigonometry'
print(fig1) 

-- print data 
x = {1,2,3,4,5}
y = {1,3,5,7,9}
print(fig1:plot(x,y)) 

-- combine different sources 
fig1:plot(x,'single',x,y,'pair',math.log, 'function')
print(fig1) 

-- define arbitrary figure size
-- odd is preferable 
fig2 = Ap(21,11)    -- width=21, height = 11

--]]

--	LOCAL

--- Check object type.
--  @param v Object.
--  @return True if the object is asciiplot.
local function isasciiplot(v) return type(v)=='table' and v.isasciiplot end

-- default figure size
local WIDTH, HEIGHT = 75, 23

local mmodf = math.modf

--	INFO

local help = SonataHelp or {new=function () return {} end}
-- description
local about = help:new("Use pseudography for data visualization.")

--	MODULE

local asciiplot = {
-- mark
type = 'asciiplot', isasciiplot = true,
-- symbols
char = {'+','o','*','#','%','~','a','c','e','n','s','u','z'}
}
-- methametods
asciiplot.__index = asciiplot

--- Constructor example.
--  @param dwidth Figure width.
--  @param dheight Figure height.
--  @return New object of asciiplot.
asciiplot.new = function(self,dwidth,dheight)
  local o = {
    width  = dwidth or WIDTH,
    height = dheight or HEIGHT,
    xrange = {-1,1},
    yrange = {-1,1}, 
    canvas = {},
    legend = {},
    -- title can be added
  }
  -- return object
  return setmetatable(o,self)
end

--- Resize, clear canvas.
--  @param F Figure object.
asciiplot._clear_ = function (F)
  local white = ' '
  for i = 1, F.height do
    local row = F.canvas[i] or {}
    for j = 1, F.width do row[j] = white end
    if #row > F.width then 
      for j = #row, F.width+1, -1 do row[j] = nil end
    end
    F.canvas[i] = row
  end
  F.legend = {}
  F.title = nil
end

--- Add coordinate axes.
--  @param F Figure object.
asciiplot._axes_ = function (F)
  local vertical, horizontal = '|', '-'
  -- vertical line
  local int, frac = mmodf((F.width-1) * 0.5 + 1)
  int = (frac > 0.5) and (int + 1) or int 
  for i = 1, F.height do
    F.canvas[i][int] = vertical
  end
  F.canvas[1][int] = 'A'
  -- horizontal line
  int, frac = mmodf((1-F.height) * 0.5 + F.height)
  int = (frac > 0.5) and (int + 1) or int 
  local row = F.canvas[int]
  for i = 1, F.width do 
    row[i] = horizontal
  end
  row[F.width] = '>'
end

--- Add xrange and yrange.
--  @param F Figure object.
asciiplot._limits_ = function (F)
  -- horizontal 
  local int, frac = mmodf((1-F.height) * 0.5 + F.height + 1)
  int = (frac > 0.5) and (int + 1) or int 
  local s = tostring(F.xrange[1]) 
  local row = F.canvas[int]
  for i = 1, #s do
    row[i] = string.sub(s,i,i)
  end
  s = tostring(F.xrange[2])
  row = F.canvas[int-2]
  local beg = F.width - #s 
  for i = 1, #s do
    row[beg+i] = string.sub(s,i,i)
  end
  -- vertical 
  int, frac = mmodf((F.width-1) * 0.5 + 2)
  beg = (frac > 0.5) and (int + 1) or int 
  s = tostring(F.yrange[2]) 
  row = F.canvas[1] 
  for i = 1, #s do
    row[beg+i] = string.sub(s,i,i)
  end
  s = tostring(F.yrange[1]) 
  beg = beg - 3 - #s 
  row = F.canvas[F.height]
  for i = 1, #s do
    row[beg+i] = string.sub(s,i,i)
  end
end

--- Prepare a clear canvas.
--  @param F Figure object.
--  @param bAxis Set false to skip axes, true by default.
--  @param bLimits Set false to skip limits, true by default.
asciiplot.reset = function (F, bAxis, bLimits)
  bAxis = bAxis or true
  bLimits = bLimits or true
  asciiplot._clear_(F)
  if bAxis   then asciiplot._axes_(F) end
  if bLimits then asciiplot._limits_(F) end
end
about[asciiplot.reset] = {"reset(F[,bAxis=true,bLimits=true])", "Prepare a clear canvas, define elements to print.", help.OTHER}

--- Scale xrange and yrange w.r.t. default values.
--  @param F figure object.
--  @param factor Positive value.
asciiplot.scale = function (F, factor)
  assert(factor > 0)
  local int, frac = mmodf(F.xrange[1] * factor)
  -- prefer odd numbers 
  F.xrange[1] = (int % 2 == 1) and int or (int + 1)
  int, frac = mmodf(F.xrange[2] * factor) 
  F.xrange[2] = (int % 2 == 1) and int or (int + 1)
end
about[asciiplot.scale] = {"scale(F,factor)", "Change figure size w.r.t. default values."}

--- Scale and add a point to the figure.
--  @param F Figure object.
--  @param dx Coordinate x.
--  @param dy Coordinate y.
--  @param s Character.
asciiplot._addPoint_ = function (F,dx,dy,s)
  local h, w = F.height, F.width
  local nx = (dx - F.xrange[1]) / (F.xrange[2] - F.xrange[1]) 
  local ny = (dy - F.yrange[1]) / (F.yrange[2] - F.yrange[1])
  local int, frac = mmodf((w-1) * nx + 1)
  nx = (frac > 0.5) and (int + 1) or int
  int, frac = mmodf((1-h) * ny + h)
  ny = (frac > 0.5) and (int + 1) or int
  if nx >= 1 and nx <= w and ny >= 1 and ny <= h then
    -- skip points out of range
    F.canvas[ny][nx] = s
  end
end

--- Add points from a table.
--  First element of each row is x, the rest are yi. 
--  @param F Figure object.
--  @param t Table to print.
--  @param tInd Allows to choose y columns.
asciiplot._addTable_ = function (F, t, tInd)
  for j = 1, #tInd do
    local c, k = asciiplot.char[j], tInd[j]
    for i = 1, #t do
      local row = t[i]
      asciiplot._addPoint_(F, row[1], row[k], c)
    end
    F.legend[c] = 'column '..tostring(k)   -- default legend
  end
end

--- Add group of points.
--  @param F Figure object.
--  @param tX List of x coordinates.
--  @param tY List of y coordinates.
--  @param s Character.
asciiplot._addXY_ = function (F, tX, tY, s)
  for i = 1, #tX do
    asciiplot._addPoint_(F, tX[i], tY[i], s)
  end
end

--- Find vectors X and Y for the given function.
--  @param F Figure object.
--  @param fn Function f(x).
--  @return Lists of coordinates X and Y.
asciiplot._fn2XY_ = function (F, fn)
  local d, x0 = (F.xrange[2] - F.xrange[1]) / (F.width - 1), F.xrange[1]
  local X, Y = {}, {}
  for nx = 1, F.width do
    local x = d * (nx - 1) + x0
    X[#X+1] = x
    Y[#Y+1] = fn(x)
  end
  return X, Y
end

--- Get bounds of the table values.
--  @param t Table {{x1,y11,y12...},{x2,y21,y22..},...}
--  @param tInd Table of y column indeces.
--  @return xrange, yrange
asciiplot._findRange_ = function (t, tInd)
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
asciiplot._findVectorRange_ = function (t)
  local vmin, vmax = math.huge, -math.huge
  for i = 1, #t do
    local v = t[i]
    if v > vmax then vmax = v end 
    if v < vmin then vmin = v end
  end
  return vmin, vmax
end

--- Plot data represented in form of table
--  {{x1,y11,y12,...},{x2,y21,y22,...},...}
--  @param F Figure object.
--  @param t Data table.
--  @param tOpt (Optional) Table of column indeces, key 'resize' allows to skip resizing.
--  @return The updated figure object.
asciiplot.tplot = function (F, t, tOpt)
  tOpt = tOpt or {resize = true}
  -- update range
  if tOpt.resize then
    F.xrange, F.yrange = asciiplot._findRange_(t, tOpt)
  end
  -- plot all by default
  if #tOpt == 0 then 
    for i = 2, #t[1] do tOpt[#tOpt+1] = i end
  end
  -- prepare 
  asciiplot._clear_(F)
  asciiplot._axes_(F)
  -- fill 
  asciiplot._addTable_(F, t, tOpt)
  -- limits
  asciiplot._limits_(F)
  -- show
  return F
end
about[asciiplot.tplot] = {"tplot(F,t[,tOpt={}])", "Plot the table data, choose columns if need."}

--- Generalized plot funciton.
--  @param F Figure object.
--  @param ... is "t1", "t1,t2", "fn", "t1,name", "t1,t2,name" etc.
--  @return The updated figure object.
asciiplot.plot = function (F, ...)
  local ag, acc = {...}, {}
  local xmin, xmax = math.huge, -math.huge
  local i = 1

  -- collect data
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
      local a,b = asciiplot._findVectorRange_(tx)
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
  if xmin == math.huge then xmin = F.xrange[1] end
  if xmax == -math.huge then xmax = F.xrange[2] end
  F.xrange = {xmin,xmax}

  -- update y range
  local ymin, ymax = math.huge, -math.huge
  for i = 1, #acc do 
    local r = acc[i]
    if type(r[1]) == 'function' then
      r[1], r[2] = asciiplot._fn2XY_(F, r[1])
    end
    local a, b = asciiplot._findVectorRange_(r[2]) 
    if a < ymin then ymin = a end
    if b > ymax then ymax = b end
  end
  F.yrange = {ymin, ymax}

  -- prepare 
  asciiplot._clear_(F)
  asciiplot._axes_(F)
  -- 'plot' 
  for i = 1, #acc do
    local c = asciiplot.char[i] 
    local r = acc[i]
    asciiplot._addXY_(F, r[1], r[2], c)
    F.legend[c] = r[3]
  end
  -- limits
  asciiplot._limits_(F)
  return F
end
about[asciiplot.plot] = {"plot(F,...)", "Plot arguments in form 't', 't1,t1', 'fn,nm', 'fn1,fn2' etc." }

--- String representation of the object.
--  @param F Figure object.
--  @return String.
asciiplot.__tostring = function (F)
  if #F.canvas == 0 then return 'empty' end
  local acc = {}
  -- title
  if F.title then
    -- to center
    local s = F.title 
    if #s < F.width then 
      local n = mmodf((F.width - #s) / 2) 
      acc[#acc+1] = string.rep(' ', n) .. s
    end
  end
  -- figure
  for i = 1, F.height do 
    acc[#acc+1] = table.concat(F.canvas[i])
  end
  -- legend
  for k, v in pairs(F.legend) do
    assert(type(k) == 'string' and #k == 1 and type(v) == 'string')
    acc[#acc+1] = string.format("  (%s) %s", k, v)
  end
  return table.concat(acc,'\n')
end


-- Simplify the constructor call.
setmetatable(asciiplot, {__call = function (self,w,h) return asciiplot:new(w,h) end})
asciiplot.Ap = 'Ap'
about[asciiplot.Ap] = {"Ap(dwidth,dheight)", "Create new asciiplot.", help.NEW}

--- Make a copy.
--  @param F Initial object.
--  @return Copy of the object.
asciiplot.copy = function (F)
  local o = {
    width = F.width, 
    height = F.height,
    xrange = {F.xrange[1], F.xrange[2]},
    yrange = {F.yrange[1], F.yrange[2]},
    canvas = {}, 
    legend = {},
    title = F.title,
  }
  for k,v in pairs(F.legend) do
    o.legend[k] = v
  end
  for i = 1, #F.canvas do
    local row = {}
    -- TODO use Ver.move
    local src = F.canvas[i]
    for j = 1, F.width do
      row[j] = src[j]
    end
    o.canvas[i] = row
  end
  return setmetatable(o, asciiplot)
end
about[asciiplot.copy] = {"copy(F)", "Create a copy of the object.", help.NEW} 

-- Comment to remove descriptions
asciiplot.about = about

return asciiplot

--======================================
-- TODO horizontal concatenation
-- TODO on import

