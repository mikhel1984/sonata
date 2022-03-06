--[[		sonata/lib/asciiplot.lua

--- Use pseudography for data visualization.
--  @author Your Name

	module 'asciiplot'
--]]

-- Define here your tests, save results to 'ans', use --> for the strict equality 
-- and --n> for the n-digit precision in the case of floating numbers.
--[[TEST

-- use 'asciiplot'
Ap = require 'lib.asciiplot'

-- example
a = Ap()
ans = a.type   -->  'asciiplot'

ans = math.pi  --2> 355/113

--]]

--	LOCAL

--- Check object type.
--  @param t Object.
--  @return True if the object is asciiplot.
local function isasciiplot(v) return type(v)=='table' and v.isasciiplot end

--	INFO

local help = SonataHelp or {new=function () return {} end}
-- description
local about = help:new("Use pseudography for data visualization.")

--	MODULE

local asciiplot = {
-- mark
type = 'asciiplot', isasciiplot = true,
}
-- methametods
asciiplot.__index = asciiplot

--- Constructor example.
--  @param t Some value.
--  @return New object of asciiplot.
asciiplot.new = function(self,dwidth,dheight)
  local o = {
    width = dwidth or 75,
    heigth = dheight or 23,
    xrange = {-1,1},
    yrange = {-1,1}, 
    canvas = {},
    legend = {},
  }
  -- return object
  return setmetatable(o,self)
end

-- Resize, clear
asciiplot._reset_ = function (F)
  local white = ' '
  for i = 1, F.heigth do
    local row = F.canvas[i] or {}
    for j = 1, F.width do row[j] = white end
    if #row > F.width then 
      for j = #row, F.width+1, -1 do row[j] = nil end
    end
    F.canvas[i] = row
  end
  F.legend = {}
end

-- Add coordinate axes
asciiplot._axes_ = function (F)
  local vertical, horizontal = '|', '-'
  -- vertical line
  local int, frac = math.modf((F.width-1) * 0.5 + 1)
  int = (frac > 0.5) and (int + 1) or int 
  for i = 1, F.heigth do
    F.canvas[i][int] = vertical
  end
  F.canvas[1][int] = 'A'
  -- horizontal line
  int, frac = math.modf((1-F.heigth) * 0.5 + F.heigth)
  int = (frac > 0.5) and (int + 1) or int 
  local row = F.canvas[int]
  for i = 1, F.width do 
    row[i] = horizontal
  end
  row[F.width] = '>'
end

asciiplot._limits_ = function (F)
  -- horizontal 
  local int, frac = math.modf((1-F.heigth) * 0.5 + F.heigth + 1)
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
  int, frac = math.modf((F.width-1) * 0.5 + 2)
  beg = (frac > 0.5) and (int + 1) or int 
  s = tostring(F.yrange[2]) 
  row = F.canvas[1] 
  for i = 1, #s do
    row[beg+i] = string.sub(s,i,i)
  end
  s = tostring(F.yrange[1]) 
  beg = beg - 3 - #s 
  row = F.canvas[F.heigth]
  for i = 1, #s do
    row[beg+i] = string.sub(s,i,i)
  end
end

-- add point
asciiplot._add_ = function (F,dx,dy,s)
  local h, w = F.heigth, F.width
  local nx = (dx - F.xrange[1]) / (F.xrange[2] - F.xrange[1]) 
  local ny = (dy - F.yrange[1]) / (F.yrange[2] - F.yrange[1])
  local int, frac = math.modf((w-1) * nx + 1)
  nx = (frac > 0.5) and (int + 1) or int
  int, frac = math.modf((1-h) * ny + h)
  ny = (frac > 0.5) and (int + 1) or int
  --print(nx, ny, F.width / 2, F.heigth / 2)
  if nx >= 0 and nx <= w and ny >= 0 and ny <= h then
    F.canvas[ny][nx] = s
  end
end

asciiplot.__tostring = function (F)
  if #F.canvas == 0 then return '' end
  local acc = {}
  for i = 1, F.heigth do 
    acc[#acc+1] = table.concat(F.canvas[i])
  end
  for i = 1, #F.legend do
    acc[#acc+1] = F.legend[i]
  end
  return table.concat(acc,'\n')
end


-- simplify constructor call
setmetatable(asciiplot, {__call = function (self,v) return asciiplot:new(v) end})
asciiplot.Ap = 'Ap'
about[asciiplot.Ap] = {"Ap(t)", "Create new asciiplot.", help.NEW}

--- Method example.
--  It is good idea to define method for the copy creation.
--  @param t Initial object.
--  @return Copy of the object.
asciiplot.copy = function (t)
  -- some logic
  return asciiplot:new(argument)
end
about[asciiplot.copy] = {"copy(t)", "Create a copy of the object."} -- third element is optional, default is 'base'

-- Comment to remove descriptions
asciiplot.about = about

--return asciiplot

--======================================

fig1 = asciiplot:new(21,21)

fig1.xrange = {-10,10}
fig1.yrange = {-10,10}
fig1:_reset_()
fig1:_axes_()
fig1:_limits_()
for i = -10,10 do
  fig1:_add_(i, i, '*')
end
--fig1:_add_(0,0,'*')
print(fig1)
