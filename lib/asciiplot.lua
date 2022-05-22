--[[		sonata/lib/asciiplot.lua

--- Use pseudography for data visualization.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2022.

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
fig1.yaxis = 'L'  -- left axis
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
fig2.xaxis = 'D'  -- down
print(fig2:tplot(tbl))

-- plot only y2, don't rescale
fig2.xrange = {-1, 4}
fig2.yrange = {0, 1}
print(fig2:tplot(tbl, {2, resize=false}))

-- scale figure w.r.t. initial size
fig1:scale(0.8)
ans = fig1.width             --> 61

-- horizontal concatenation 
-- first
fig1:scale(0.5, true)
fig1.xrange = {0, 1.57}
fig1.yaxis = 'C'; fig1.xaxis = 'D'
fig1:plot(sin, 'sin')
fig1.title = 'First'
-- second
fig2:scale(0.5, true)
fig2.xrange = {0, 1.57}
fig2:plot(cos, 'cos')
fig2.title = 'Second'
print(Ap.concat(fig1, fig2))   -- similar to fig1..fig2 for 2 objects

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

--]]

--	LOCAL

--- Check object type.
--  @param v Object.
--  @return True if the object is asciiplot.
local function isasciiplot(v) return type(v)=='table' and v.isasciiplot end

-- default figure size
local WIDTH, HEIGHT = 75, 23

local mmodf = math.modf

local MANUAL = 'manual'

--	INFO

local help = SonataHelp or {new=function () return {} end}
-- description
local about = help:new("Use pseudography for data visualization.")

--	MODULE

local asciiplot = {
-- mark
type = 'asciiplot', isasciiplot = true,
-- symbols
char = {'+','o','*','#','%','~','x','a','c','e','n','s','u','z'}
}
-- methametods
asciiplot.__index = asciiplot

-- Simplify call for two objects.
asciiplot.__concat = function (F1, F2) return asciiplot.concat(F1, F2) end

--- String representation of the object.
--  @param F Figure object.
--  @return String.
asciiplot.__tostring = function (F)
  if #F.canvas == 0 then return 'empty' end
  local acc = {}
  -- title
  if F.title then
    -- to center
    acc[1] = asciiplot._format_(F.title, F.width, true, false)
  end
  -- figure
  for i = 1, F.height do 
    acc[#acc+1] = table.concat(F.canvas[i])
  end
  -- legend
  for k, v in pairs(F.legend) do
    assert(type(k) == 'string' and #k == 1 and type(v) == 'string')
    acc[#acc+1] = string.format("(%s) %s", k, v)
  end
  return table.concat(acc,'\n')
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
asciiplot._addXY_ = function (F, tX, tY, s)
  for i = 1, #tX do
    asciiplot.addPoint(F, tX[i], tY[i], s)
  end
end

--- Add coordinate axes.
--  @param F Figure object.
asciiplot._axes_ = function (F)
  local vertical, horizontal = '|', '-'
  -- vertical line
  local n
  if F.yaxis == 'C' then n = asciiplot._ycentral_(F)
  elseif F.yaxis == 'L' then n = 1 
  elseif F.yaxis == 'R' then n = F.width end
  if n then
    for i = 1, F.height do
      F.canvas[i][n] = vertical
    end
    F.canvas[1][n] = 'A'
    n = nil
  end
  -- horizontal line
  if F.xaxis == 'C' then n = asciiplot._xcentral_(F)
  elseif F.xaxis == 'U' then n = 1
  elseif F.xaxis == 'D' then n = F.height end
  if n then
    local row = F.canvas[n]
    for i = 1, F.width do 
      row[i] = horizontal
    end
    row[F.width] = '>'
  end
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

--- Prepare string of the given length.
--  @param s Source string.
--  @param N Required length.
--  @param bCentr Flag to put to central position.
--  @param bLim   Flag to cut a long string.
asciiplot._format_ = function (s, N, bCentr, bCut)
  local res = ''
  if #s < N then
    local n = N - #s 
    if bCentr then 
      local n1 = mmodf(n / 2)
      local n2 = n - n1
      res = string.rep(' ', n1) .. s .. string.rep(' ', n2)
    else
      res = s .. string.rep(' ', n) 
    end
  else 
    res = s
  end
  if #res > N and bCut then
    res = string.sub(res,1,N)
  end
  return res
end

--- Add xrange and yrange.
--  @param F Figure object.
asciiplot._limits_ = function (F)
  -- horizontal 
  local n 
  if F.xaxis == 'C' then n = asciiplot._xcentral_(F) + 1
  elseif F.xaxis == 'U' then n = 1 
  elseif F.xaxis == 'D' then n = F.height end
  if n then
    local row, beg = F.canvas[n], 0
    local s = tostring(F.xrange[1]) 
    for i = 1, #s do
      row[beg+i] = string.sub(s,i,i)
    end
    s = tostring(F.xrange[2])
    beg = F.width - #s - 1
    for i = 1, #s do
      row[beg+i] = string.sub(s,i,i)
    end
    n = nil
  end
  -- vertical 
  if F.yaxis == 'C' then n = asciiplot._ycentral_(F) 
  elseif F.yaxis == 'L' then n = 1
  elseif F.yaxis == 'R' then n = F.width end
  if n then
    local s = tostring(F.yrange[1])
    local beg = (n == F.width) and (F.width - #s - 1) or (n + 1)
    local row = F.canvas[F.height-1]
    for i = 1, #s do
      row[beg+i] = string.sub(s,i,i)
    end
    s = tostring(F.yrange[2]) 
    beg = (n == F.width) and (F.width - #s - 1) or (n + 1)
    row = F.canvas[2] 
    for i = 1, #s do
      row[beg+i] = string.sub(s,i,i)
    end
  end
end

--- Horizontal line central position
--  @param F Figure object.
--  @return Index.
asciiplot._xcentral_ = function (F)
  local int, frac = mmodf((1-F.height) * 0.5 + F.height)
  return (frac > 0.5) and (int + 1) or int 
end

--- Vertical line central position
--  @param F Figure object.
--  @return Index.
asciiplot._ycentral_ = function (F)
  local int, frac = mmodf((F.width-1) * 0.5 + 1)
  return (frac > 0.5) and (int + 1) or int 
end

--- Scale and add a point to the figure.
--  @param F Figure object.
--  @param dx Coordinate x.
--  @param dy Coordinate y.
--  @param s Character.
asciiplot.addPoint = function (F,dx,dy,s)
  local h, w = F.height, F.width
  local nx = (dx - F.xrange[1]) / (F.xrange[2] - F.xrange[1]) 
  local ny = (dy - F.yrange[1]) / (F.yrange[2] - F.yrange[1])
  local int, frac = mmodf((w-1) * nx + 1)
  nx = (frac > 0.5) and (int + 1) or int
  int, frac = mmodf((1-h) * ny + h)
  ny = (frac > 0.5) and (int + 1) or int
  if nx >= 1 and nx <= w and ny >= 1 and ny <= h and #s == 1 then
    -- skip points out of range
    F.canvas[ny][nx] = s
  end
end
about[asciiplot.addPoint] = {"addPoint(F,dx,dy,s)", "Add point (dx,dy) using char 's'.", MANUAL}

--- Set character to direct position.
--  @param F Figure object.
--  @param ir Row index.
--  @param ic Column index.
--  @param s Character.
asciiplot.addPose = function (F,ir,ic,s)
  if ir >= 1 and ir <= F.height and ic > 0 and ic < F.width and #s == 1 then
    F.canvas[ir][ic] = s
  end
end
about[asciiplot.addPose] = {"addPose(F,ir,ic,s)", "Add character s to the given position.", MANUAL}

--- Set string to the given position.
--  @param F Figure object.
--  @param ir Row index.
--  @param ic Column index.
--  @param s String.
asciiplot.addString = function (F,ir,ic,s)
  for i = 1, #s do
    asciiplot.addPose(F, ir, ic+i-1, string.sub(s,i,i))
  end
end
about[asciiplot.addString] = {"addString(F,ir,ic,s)", "Set string from the given position.", MANUAL}

--- Plot bar graph.
--  @param F Figure object.
--  @param t Data table ({x,y} or x).
--  @param vy Index of y column or data table (optional).
--  @param ix Optional index of x values. 
--  @return Updated figure object.
asciiplot.bar = function (F, t, vy, ix)
  ix, vy = ix or 1, vy or 2
  local ytbl = (type(vy) == 'table')
  -- size
  local iL = mmodf(F.width * 0.2) 
  local iR = F.width - iL
  -- find limits
  local min, max = math.huge, -math.huge 
  for i = 1, #t do
    local v = ytbl and vy[i] or t[i][vy] 
    if v > max then max = v end 
    if v < min then min = v end
  end
  local i0   
  if min >= 0 and max >= 0 then
    i0, min = iL, 0
  elseif min <= 0 and max <= 0 then
    i0, max = iR, 0
  elseif min < 0 and max > 0 then
    local int, frac = mmodf(iL - min / (max - min) * (iR - iL))
    i0 = (frac > 0.5) and int + 1 or int
  end
  local dm = (iR - iL) / (max - min)
  -- data step
  local step = 1
  if #t > F.height then
    local int, frac = mmodf(#t / F.height) 
    step = (frac > 0) and (int+1) or int
  end
  -- add values
  asciiplot._clear_(F)
  local r = 1
  for i = 1, #t, step do
    -- text 
    local x = tostring(ytbl and t[i] or t[i][ix])
    for c = 1, math.min(iL-2, #x) do F.canvas[r][c] = string.sub(x,c,c) end 
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
    for c = 1, math.min(iL-2, #x) do F.canvas[r][iR+2+c] = string.sub(x,c,c) end
    r = r + 1
  end
  return F
end
about[asciiplot.bar] = {"bar(F,t,[vy=2,ix=1])", "Plot bar diargram for data. vy can be y index in t (optional) or table of y-s."}

--- Horizontal concatenation of figures.
--  @param ... List of figure objects.
--  @return String with figures.
asciiplot.concat = function (...)
  -- collect info
  local ag = {...}
  local nlegend = 0
  for i,v in ipairs(ag) do
    if not isasciiplot(v) then error("Not asciiplot objec at "..tostring(i)) end
    if v.height ~= ag[1].height then error('Different height') end
    if v.title then btitle = true end
    local n = 0 
    for k,_ in pairs(v.legend) do n = n + 1 end
    if n > nlegend then nlegend = n end
  end
  -- data
  local acc = {}
  local gap = '  '
  for i,v in ipairs(ag) do
    local k = 1
    -- title
    local row = acc[k] or {}
    row[#row+1] = asciiplot._format_(v.title or '', v.width, true, true)
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
    for u,w in pairs(v.legend) do
      row = acc[k] or {}
      row[#row+1] = asciiplot._format_(string.format('(%s) %s', u, w), v.width, false, true)
      row[#row+1] = gap
      acc[k] = row; k = k + 1
      n = n + 1
    end
    -- add empty lines
    for j = n+1, nlegend do 
      row = acc[k] or {}
      row[#row+1] = asciiplot._format_('', v.width, false, true)
      row[#row+1] = gap
      acc[k] = row; k = k + 1
    end
  end
  -- get strings 
  for i = 1, #acc do acc[i] = table.concat(acc[i]) end
  return table.concat(acc, '\n')
end
about[asciiplot.concat] = {"concat(...)", "Horizontal concatenation of figures with the same height. For two object operator '..' can be used.", help.OTHER}

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

--- Constructor example.
--  @param dwidth Figure width.
--  @param dheight Figure height.
--  @return New object of asciiplot.
asciiplot.new = function(self,dwidth,dheight)
  local o = {
    -- size
    width  = dwidth or WIDTH,
    height = dheight or HEIGHT,
    _w0 = dwidth or WIDTH,
    _h0 = dheight or HEIGHT,
    -- range
    xrange = {-1,1},
    yrange = {-1,1}, 
    -- axes location
    xaxis  = 'C',
    yaxis  = 'C',
    -- image
    canvas = {},
    -- comments
    legend = {},
    -- title can be added
  }
  -- return object
  return setmetatable(o,self)
end

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

--- Prepare a clear canvas.
--  @param F Figure object.
asciiplot.reset = function (F)
  asciiplot._clear_(F)
  asciiplot._axes_(F) 
  asciiplot._limits_(F) 
end
about[asciiplot.reset] = {"reset(F)", "Prepare a clear canvas.", MANUAL}

--- Scale xrange and yrange w.r.t. initial size.
--  @param F figure object.
--  @param factor Positive value.
--  @param bDefault set true to use defauld size instead
asciiplot.scale = function (F, factor, bDefault)
  assert(factor > 0)
  local w = bDefault and WIDTH or F._w0
  local h = bDefault and HEIGHT or F._h0
  local int, frac = mmodf(w * factor)
  -- prefer odd numbers 
  F.width = (int % 2 == 1) and int or (int + 1)
  int, frac = mmodf(h * factor) 
  F.height = (int % 2 == 1) and int or (int + 1)
  return F
end
about[asciiplot.scale] = {"scale(F,factor,[bDefault=false])", "Change figure size w.r.t. initial size."}

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
about[asciiplot.tplot] = {"tplot(F,t,[tOpt={}])", "Plot the table data, choose columns if need."}

-- Simplify the constructor call.
setmetatable(asciiplot, {__call = function (self,w,h) return asciiplot:new(w,h) end})
asciiplot.Ap = 'Ap'
about[asciiplot.Ap] = {"Ap([iWidth=75,iHeight=23])", "Create new asciiplot.", help.NEW}

-- Export funcitons.
asciiplot.onImport = function ()
  Plot = function (...)
    local f = Ap()
    print(f:plot(...))
  end
  Main.about[Plot] = {"Plot(...)", "Plot arguments in form 't', 't1,t1', 'fn,nm', 'fn1,fn2' etc.", help.OTHER }
end

-- Comment to remove descriptions
asciiplot.about = about

return asciiplot

--======================================
-- TODO synchronize with Gp
