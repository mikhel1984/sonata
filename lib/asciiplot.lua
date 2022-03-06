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

local WHITE = '.'

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

asciiplot._reset_ = function (F)
  -- resize, clear
  for i = 1, F.heigth do
    local row = F.canvas[i] or {}
    for j = 1, F.width do row[j] = WHITE end
    if #row > F.width then 
      for j = #row, F.width+1, -1 do row[j] = nil end
    end
    F.canvas[i] = row
  end
  F.legend = {}

end

-- add point
asciiplot._add_ = function (F,dx,dy,s)
  
end

asciiplot.__tostring = function (F)
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

fig1 = asciiplot:new()
fig1:_reset_()
print(fig1)
