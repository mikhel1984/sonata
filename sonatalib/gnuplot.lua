--[[		sonatalib/gnuplot.lua 

--- Call Gnuplot from Sonata LC.
--
--  Object structure:
--  all parameters of the plot are saved in form of table, each function is in separate subtable.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonatalib</a> collection, 2021.

	module 'gnuplot'
--]]

--------------- Tests ---------------
--[[TEST

-- use 'gnuplot'
Gp = require 'sonatalib.gnuplot'
Gp.testmode = true -- just for testing

-- simple plot 
t1 = {1,2,3,4,5}
t2 = {2,4,6,4,2}
-- in the dialog call "plot(t1,t2,'some curve')"
Gp.plot(t1,t2,'some curve')

-- simplified function plot
Gp.plot(t1, math.sin, 'sin')

-- extended capabilities
a = Gp()
a:add {math.sin, title='sin'}
a:add {math.cos, title='cos'}
a:show()

-- additional parameters
a.xrange = {0,10}   -- add rangle
-- save to file 
a.terminal = 'png'
a.output = 'test.png'
a:show()

-- copy parameters to other object
b = a:copy()
print(b)

-- send 'raw' command to Gnuplot
c = Gp()
c.raw = 'plot x**2-2*x+1; set xlabel "X"; set ylabel "Y"'
c:show()

-- print Lua table
tmp = {{1,1},{2,2},{3,3},{4,4}}
d = Gp()
d:add {tmp,with='lines'}
d:show()

-- define function
fn1 = function (x) return x^2-x end
f = Gp()
f:add {fn1,with='lines',title='x^2-x'}
f:show()

--]]

--	LOCAL

-- special commands
local special = {
  output = function (x) return string.format('set output "%s"', x) end,
  xlabel = function (x) return string.format('set xlabel "%s"', x) end,
  ylabel = function (x) return string.format('set ylabel "%s"', x) end,
  title  = function (x) return string.format('set title "%s"', x) end,
}

-- main commands
local main = {
  string  = function (x,y) return string.format('set %s %s', x, y) end,
  number  = function (x,y) return string.format('set %s %d', x, y) end,
  table   = function (x,y) return string.format('set %s [%f:%f]', x, y[1],y[2]) end,
  boolean = function (x,y) return string.format('%s %s', y and 'set' or 'unset', x) end,
}

--- Prepare option string
--  @param k Key.
--  @param v Value.
--  @return String with command.
local function command (k,v)
  local spc = special[k]
  return spc and spc(v) or main[type(v)](k,v)
end

--- Function option string
--  @param k Key.
--  @param v Value.
--  @return String, prepared for usage in function.
local function prepare(k,v)
  if k == 'title' then v = string.format('"%s"', v) end
  if k == 'using' then v = string.format('%d:%d', v[1],v[2]) end
  return k,v
end

--	INFO
local help = LC_DIALOG and (require "sonatalib.help") or {new=function () return {} end}

--	MODULE

local gnuplot = {
type = 'gnuplot',
-- basic common options
options = {'terminal','output','parametric','size','polar','grid','key','title',
  'xlabel','ylabel','xrange','yrange','zrange','trange','samples'},
-- basic function options
foptions = {'using','title','with','linetype','linestyle','linewidth','ls','ln','lw'},
-- description
about = help:new("Interface for calling Gnuplot from Sonata LC."),
}
-- metha
gnuplot.__index = gnuplot

-- divide interval into given number of points
gnuplot.N = 100
gnuplot.about[gnuplot.N] = {"N[=100]", "If no samples, divide interval into N points.", help.CONST}

--- Save table to tmp file
--  @param t Lua table with numbers.
--  @return File name as string.
gnuplot._tbl2file_ = function (t)
  local name = os.tmpname()
  local f = io.open(name, 'w')
  for _, row in ipairs(t) do
    for _, val in ipairs(row) do f:write(val,' ') end
    f:write('\n')
  end
  f:close()
  return string.format('"%s"', name)
end

--- Save function result to tmp file
--  @param fn Lua function.
--  @param base Range and step.
--  @return File name as a string.
gnuplot._fn2file_ = function (fn,base)
  local name = os.tmpname()
  local xl = base.xrange and base.xrange[1] or (-10)
  local xr = base.xrange and base.xrange[2] or 10
  local N = base.samples or gnuplot.N
  local dx = (xr-xl)/N
  local f = io.open(name, 'w')
  if base.surface then
    local yl = base.yrange and base.yrange[1] or (-10)
    local yr = base.yrange and base.yrange[2] or 10
    local dy = (yr-yl)/N
    for x = xl,xr,dx do 
      for y = yl,yr,dy do f:write(x,' ',y,' ',fn(x,y),'\n') end
    end 
  else
    for x = xl,xr,dx do
      f:write(x,' ',fn(x),'\n')
    end
  end
  f:close()
  return string.format('"%s"', name)
end

--- Add function parameters
--  @param t Table with function definition.
--  @param base Argument range.
--  @return String representation of the plot command.
gnuplot._graph_ = function (t,base)
  -- function/file name
  local fn, str, nm = t[1], ''
  if type(fn) == 'table' then
    nm = gnuplot._tbl2file_(fn,base)
  elseif type(fn) == 'function' then
    nm = gnuplot._fn2file_(fn,base)
  else -- type(fn) == 'string' !!
    nm = string.format('"%s"', fn)
  end
  -- prepare options
  for _,k in ipairs(gnuplot.foptions) do
    if t[k] then str = string.format('%s %s %s ', str, prepare(k,t[k])) end
  end
  return string.format('%s %s', nm, str)
end

--- Create new object, set metatable.
--  @param self Pointer to parent table.
--  @param o Table with parameters or nil.
--  @return New 'gnuplot' object.
gnuplot.new = function (self) return setmetatable({}, self) end

--- Add new curve to the plot.
--  @param G Gnuplot object.
--  @param tCurve Table with function/table and parameters.
gnuplot.add = function (G, tCurve) G[#G+1] = tCurve end

--- Get copy of graph options.
--  @param G Initial table.
--  @return Copy of table.
gnuplot.copy = function (G)
  local cp = gnuplot:new()
  for k,v in pairs(G) do
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
gnuplot.about[gnuplot.copy] = {"copy(G)", "Get copy of the plot options."}

--- Plot graphic.
--  @param G Table with parameters of graphic.
--  @return Table which can be used for plotting.
gnuplot.show = function (G)
  -- open Gnuplot
  local handle = assert(
    io.popen('gnuplot' .. (gnuplot.testmode and '' or ' -p'), 'w'), 'Cannot open Gnuplot!')
  -- save options
  local cmd = {}
  for _,k in ipairs(gnuplot.options) do
    if G[k] ~= nil then cmd[#cmd+1] = command(k, G[k]) end
  end
  if G.raw then cmd[#cmd+1] = G.raw end
  -- prepare functions
  local fn = {}
  for i,f in ipairs(G) do
    fn[i] = gnuplot._graph_(f,G)
  end
  -- command
  if #fn > 0 then
    local cmd_plot = G.surface and 'splot ' or 'plot '
    cmd[#cmd+1] = cmd_plot .. table.concat(fn,',')
  end
  local res = table.concat(cmd, '\n')
  -- send to Gnuplot
  handle:write(res,'\n')
  handle:close()
end
gnuplot.about[gnuplot.show] = {"show(G)", "Plot data, represented as Lua table." }

--- Represent parameters of the graphic.
--  @param G Gnuplot object.
--  @return String with object properties.
gnuplot.__tostring = function (G) 
  local res = {}
  for k,v in pairs(G) do
    if type(v) == 'table' then
      local tmp = {}
      for p,q in pairs(v) do 
        tmp[#tmp+1] = string.format('%s=%s', tostring(p), tostring(q)) 
      end
      v = string.format('{%s}', table.concat(tmp,',')) 
    end
    res[#res+1] = string.format('%s=%s', tostring(k), tostring(v))
  end
  return string.format('{\n%s\n}', table.concat(res, ',\n'))
end

--- Save one or two lists into tmp file
--  @param t1 First Lua table (list).
--  @param t2 Second Lua table (list) or nil.
--  @return File name.
gnuplot._lst2file_ = function (t1,t2)
  local name = os.tmpname()
  local f = io.open(name, 'w')
  if t2 then 
    if type(t2) == 'table' then
      assert(#t1 == #t2, 'Different talbe length!')
      for i, v1 in ipairs(t1) do f:write(v1, ' ', t2[i], '\n') end
    else -- must be function 
      for i, v1 in ipairs(t1) do f:write(v1, ' ', t2(v1), '\n') end
    end
  else
    for i, v1 in ipairs(t1) do f:write(i, ' ', v1, '\n') end
  end  
  f:close()
  return name
end

--- Simplified plotting
--  @param ... Can be "t", "t1,t2", "t1,fn", "t1,t2,name", "t,name" etc.
gnuplot.plot = function (...)
  local ag = {...}
  local cmd = gnuplot:new()
  local i, n = 1, 1
  repeat 
    -- ag[i] have to be table
    local var, name, legend = ag[i+1], nil, nil
    if type(var) == 'table' or type(var) == 'function' then
      name = gnuplot._lst2file_(ag[i], var)
      i = i+2
    else
      name = gnuplot._lst2file_(ag[i])
      i = i+1
    end
    if type(ag[i]) == 'string' then
      legend = ag[i]
      i = i+1
    else 
      legend = tostring(n)
    end
    cmd[#cmd+1] = {name, with='lines', title=legend}
    n = n + 1
  until i > #ag
  cmd.grid = true
  cmd:show()
end
gnuplot.about[gnuplot.plot] = {"plot(x1,[y1,[nm,[x2,..]]])", "'x' is list of numbers, 'y' is either list or functin, 'nm' - curve name."}

-- constructor
setmetatable(gnuplot, {__call=function (self,v) return gnuplot:new(v) end})
gnuplot.Gp = 'Gp'
gnuplot.about[gnuplot.Gp] = {"Gp()", "Prepare Gnuplot object.", help.NEW}

gnuplot.keys = 'keys'
gnuplot.about[gnuplot.keys] = {'keys',
[[  Options / examples:
{math.sin, title='sin'}           -- plot using function, define in Lua; add legend
{'sin.dat', ln=1, lw=2}           -- plot data from file, use given color and width
{tbl, with='lines'}               -- plot data from Lua table, use lines
title='Graph name'                -- set title
xrange={0,10}                     -- range of x from 0 to 10
yrange={-2,2}                     -- range of y
zrange={0,5}                      -- range of z
trange={1,2}                      -- range for parametric functions
xtitle='A', ytitle='B'            -- axes names
terminal='jpeg'                   -- save result as jpeg image
output='my_plot.jpg'              -- file name
parametric=true                   -- create parametric plot
size='square'                     -- set square size
polar=true                        -- use polar coordinate system
grid='polar'                      -- polar grid
legend=false                      -- don't use legend
surface=true                      -- plot surface in 3D
samples=200                       -- define number of points
raw='set pm3d'                    -- set Gnuplot options manually
]]
}

--- Function for execution during the module import.
gnuplot.onImport = function ()
  plot = gnuplot.plot
  lc.about[plot] = {"plot(x1,[y1,[nm,[x2,..]]])", gnuplot.about[gnuplot.plot][2]}
end

-- free memory if need
if not LC_DIALOG then gnuplot.about = nil end

return gnuplot

--===========================================
--TODO: plot matrix columns (rows)