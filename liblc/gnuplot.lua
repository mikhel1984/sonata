--[[      liblc/gnuplot.lua 

--- Call Gnuplot from Lua.
--  @author <a href="mailto:vpsys@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

            module 'gnuplot'
--]]

--------------- Tests ---------------
--[[ !!
Gnu = require 'liblc.gnuplot'

a = {{'sin(x)',title='Sinus x'},permanent=false}
-- use 'permanent=true' instead or not define it at all
-- 'permanent=false' is just for testing
Gnu.plot(a)

-- save as object
-- to simplify modification
g = Gnu(a)
g.xrange = {-10,10}
g:plot()

-- copy parameters to other object
b = g:copy()
print(b)
-- check correctness of the table
ans = b:isavailable()                             --> true

-- print Lua table
tmp = {{1,1},{2,2},{3,3},{4,4}}
Gnu.plot {{tmp,with='lines'},permanent=false}

-- print Lua function
fn1 = function (x) return x^2-x end
Gnu.plot {{fn1,with='lines',title='x^2-x'},permanent=false}
]]

-------------------------------------------- 
-- @class table
-- @name gnuplot
-- @field type Define object type string.
-- @field about Description of functions.
-- @field N Define number of points per interval, default is 100
-- @field options Gnuplot options that predefined in LuaCalculus
-- @field foptions Predefined function plot options

local gnuplot = {}
gnuplot.__index = gnuplot
-- mark object
gnuplot.type = 'gnuplot'
-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
gnuplot.about = help:new("Interface for calling Gnuplot from Lua.")

-- divide interval into given number of points
gnuplot.N = 100        
gnuplot.about[gnuplot.N] = {"N", "If no samples, divide interval into N points.", help.CONST}

-- basic common options
gnuplot.options = {'terminal','output','parametric','size','polar','grid','key','title',
                   'xlabel','ylabel','xrange','yrange','zrange','trange','samples'}
-- basic function options
gnuplot.foptions = {'title','with','linetype','linestyle','linewidth','ls','ln','lw'}

-- rules
local special = {
   output = function (x) return string.format('set output "%s"', x) end,
   xlabel = function (x) return string.format('set xlabel "%s"', x) end,
   ylabel = function (x) return string.format('set ylabel "%s"', x) end,
   title = function (x) return string.format('set title "%s"', x) end,
}
local main = {
   string = function (x,y) return string.format('set %s %s', x, y) end,
   number = function (x,y) return string.format('set %s %d', x, y) end,
   table = function (x,y) return string.format('set %s [%f:%f]', x, y[1],y[2]) end,
   boolean = function (x,y) return string.format('%s %s', y and 'set' or 'unset', x) end,
}

-- combine all options as keys
local function collect(t) 
   local res = {}
   for _,v in ipairs(t) do res[v] = true end
   return res
end

-- option checker
local acc = {options=collect(gnuplot.options), foptions=collect(gnuplot.foptions)}
acc.options.permanent = true
acc.options.surface = true
acc.options.raw = true
acc.foptions.file = true
acc.foptions.raw = true

--- Check if all options in table are available
--    @param g Table with optinos.
--    @return True if no wrong parameters.
gnuplot.isavailable = function (g) 
   local available = true
   for k,v in pairs(g) do
      if not acc.options[k] then
         if type(k) == 'number' and type(v) == 'table' then
            for p,q in pairs(v) do
	       if not acc.foptions[p] and type(p) ~= 'number' then
	          print(p .. ' is not predefined, use "raw" for this option!')
	          available = false
	       end -- if
	    end -- for p,q
	 else
	    print(k .. ' is not predefined, use "raw" for this option!')
	    available = false
	 end -- if type
      end -- if not
   end -- for k,v
   return available
end
gnuplot.about[gnuplot.isavailable] = {"isavailable(g)", "Check if all options in table are predefined in program.", help.OTHER}

-- prepare option string
local function command (k,v)
   return special[k] and special[k](v) or main[type(v)](k,v)
end

-- function option string
local function prepare(k,v)
   if k == 'title' then v = string.format('"%s"', v) end
   return k,v
end

-- save table to tmp file
local function tbl2file(t)
   local name = os.tmpname()
   local f = io.open(name, 'w')
   for _, row in ipairs(t) do
      for i,val in ipairs(row) do f:write(val,' ') end
      f:write('\n')
   end
   f:close()
   return string.format('"%s"', name)
end

-- save function result to tmp file
local function fn2file(fn,base)
   local name = os.tmpname()
   local xl = base.xrange and base.xrange[1] or (-10)
   local xr = base.xrange and base.xrange[2] or 10
   local N = base.samples or 100
   local dx = (xr-xl)/N
   local f = io.open(name, 'w')
   if base.surface then
      local yl = base.yrange and base.yrange[1] or (-10)
      local yr = base.yrange and base.yrange[2] or 10
      local dy = (yr-yl)/N
      for x = xl,xr,dx do 
         for y = yl,yr,dy do f:write(x,' ',y,' ',fn(x,y),'\n') end
      end -- for x
   else
      for x = xl,xr,dx do f:write(x,' ',fn(x),'\n') end
   end
   f:close()
   return string.format('"%s"', name)
end

-- prepare functions representation
local function_str = {table=tbl2file, ['function']=fn2file, string=function (x) return x end}

-- get function representation
local function getfunction (t,base)
   return t[1] and function_str[type(t[1])](t[1],base) or string.format('"%s"', t.file)
end

-- add function parameters
local function graph (t,base)
   -- function/file name
   local str = getfunction(t, base)
   -- prepare options
   for _,k in ipairs(gnuplot.foptions) do
      if t[k] then str = string.format('%s %s %s ', str, prepare(k,t[k])) end
   end
   if t.raw then str = string.format('%s %s', str, t.raw) end

   return str
end

--- Create new object, set metatable.
--    @param o Table with image parameters.
--    @return Gnuplot compatible object.
function gnuplot:new(o)
   local o = o or {}
   setmetatable(o, self)
   return o
end

--- Get copy of graph options.
--    @param g Initial table.
--    @return Copy of table.
gnuplot.copy = function (g)
   local cp = gnuplot:new()
   for k,v in pairs(g) do
      if type(v) == 'table' then
         local tmp = {}
	 for p,q in pairs(v) do tmp[p] = q end
	 cp[k] = tmp
      else
         cp[k] = v
      end
   end
   return cp
end
gnuplot.about[gnuplot.copy] = {"copy(g)", "Get copy of the plot options.", }

--- Plot graphic.
--    @param t Table with parameters of graphic.
--    @return Table which can be used for plotting.
gnuplot.plot = function (t)
   assert(gnuplot.isavailable(t), 'Options are not predefined!')
   if t.permanent == nil then t.permanent = true end
   -- open Gnuplot
   local handle = assert(io.popen('gnuplot' .. (t.permanent and ' -p' or ''), 'w'), 'Cannot open Gnuplot!')
   local cmd = {}
   -- save options
   for _,k in ipairs(gnuplot.options) do
      if t[k] or t[k] == false then
         cmd[#cmd+1] = command(k, t[k])
      end
   end
   if t.raw then cmd[#cmd+1] = t.raw end
   -- prepare functions
   local fn = {}
   for _,f in ipairs(t) do
      fn[#fn+1] = graph(f,t)
   end
   -- command
   if #fn > 0 then
      local cmd_plot = t.surface and 'splot ' or 'plot '
      cmd[#cmd+1] = cmd_plot .. table.concat(fn,',')
   end
   local res = table.concat(cmd, '\n')
   --print(res)
   -- send to Gnuplot
   handle:write(res,'\n')
   handle:close()
end
gnuplot.about[gnuplot.plot] = {"plot(g)", "Plot data, represented as Lua table.", }


--- Represent parameters of the graphic.
--    @param g Table with parameters.
--    @return String representation.
gnuplot.__tostring = function (g) 
   local res = {}
   for k,v in pairs(g) do
      if type(v) == 'table' then
         local tmp = {}
	 for p,q in pairs(v) do tmp[#tmp+1] = string.format('%s=%s', tostring(p), tostring(q)) end
         v = string.format('{%s}', table.concat(tmp,',')) 
      end
         res[#res+1] = string.format('%s=%s', tostring(k), tostring(v))
   end

   return string.format('{\n%s\n}', table.concat(res, ',\n'))
end

-- constructor
setmetatable(gnuplot, {__call=function (self,v) return gnuplot:new(v) end})
gnuplot.Gnu = 'Gnu'
gnuplot.about[gnuplot.Gnu] = {"Gnu([g])", "Transform given table into gnuplot object.", help.NEW}

gnuplot.keys = 'keys'
gnuplot.about[gnuplot.keys] = {'keys',
[[  Options description:
{'sin(x)'}                                   -- print sinus using Gnuplot functions
{math.sin, title='sinus'}                    -- plot using function, define in Lua; add legend
{file='sin.dat', ln=1, lw=2}                 -- plot data from file, use given color and width
{tbl, with='lines'}                          -- plot data from Lua table, use lines
title='Graph name'                           -- set title
xrange={0,10}                                -- range of x from 0 to 10
yrange={-2,2}                                -- range of y
zrange={0,5}                                 -- range of z
trange={1,2}                                 -- range for parametric functions
xtitle='A', ytitle='B'                       -- axes names
terminal='jpeg'                              -- save result as jpeg image
output='my_plot.jpg'                         -- file name
parametric=true                              -- create parametric plot
size='square'                                -- set square size
polar=true                                   -- use polar coordinate system
grid='polar'                                 -- polar grid
legend=false                                 -- don't use legend
surface=true                                 -- plot surface in 3D
samples=200                                  -- define number of points
permanent=true                               -- create in independent window
raw='set pm3d'                               -- set Gnuplot options manually
]],
}

-- free memory if need
if not lc_version then gnuplot.about = nil end

return gnuplot

--===========================================

