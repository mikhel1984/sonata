--[[		sonata/lib/gnuplot.lua

--- Call Gnuplot from Sonata.
--
--  Object structure:
--  all parameters of the plot are saved in form of table,
--  each function is in separate subtable.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2024.

	module 'gnuplot'
--]]


--------------- Tests ---------------
--[[TEST_IT

-- use 'gnuplot'
Gp = require 'matlib.gnuplot'
Gp.testmode = true -- just for testing

-- simple plot
t1 = {1,2,3,4,5}
t2 = {2,4,6,4,2}
-- in the dialog call "plot(t1,t2,'some curve')"
Gp:plot(t1,t2,'some curve')

-- simplified function plot
Gp:plot(t1, math.sin, 'sin')

-- name can be skipped
Gp:plot(t1, math.sin, t1, math.cos)

-- plot table (or matrix, or datafile)
arr = {}
for i = 1,50 do
  x = 0.1*i
  arr[i] = {x, math.sin(x), math.cos(x)}
end
-- all columns by default (for table and matrix)
Gp:tplot(arr)

-- the same, but with explicit column specification
Gp:tplot(arr,1,2,3)

-- in polar coordinate system
Gp:polarplot(t1,t2,'some curve')

-- from table
Gp:tpolar(arr,1,2)

-- plot surface
function fun(x,y) return x*x + y*y end
Gp:surfplot(t1,t2,fun,'some surf')

-- from table
arr2 = {}
for _,v1 in ipairs(t1) do
  for _,v2 in ipairs(t2) do
    arr2[#arr2+1] = {v1,v2,fun(v1,v2)}
  end
end
Gp:tsurf(arr2)

-- direct use Gp object
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
tmp = {
  {1,1},
  {2,2},
  {3,3},
  {4,4}
}
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

local isWindows = package.config:sub(1,1) == '\\'
local TEMP = '\\AppData\\Local\\VirtualStore'


-- special commands
local special = {
  output = function (s) return string.format("set output '%s'", s) end,
  xlabel = function (s) return string.format("set xlabel '%s'", s) end,
  ylabel = function (s) return string.format("set ylabel '%s'", s) end,
  title  = function (s) return string.format("set title '%s'", s) end,
}


-- main commands
local main = {
  string  = function (x,y) return string.format('set %s %s', x, y) end,
  number  = function (x,y) return string.format('set %s %d', x, y) end,
  table   = function (x,y)
    return string.format('set %s [%f:%f]', x, y[1],y[2])
  end,
  boolean = function (x,y)
   return string.format('%s %s', y and 'set' or 'unset', x)
  end,
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
--  @param s Key.
--  @param v Value.
--  @return String, prepared for usage in function.
local function prepare(s,v)
  if s == 'title' then v = string.format("'%s'", v) end
  if s == 'using' then v = table.concat(v,':') end
  return s,v
end


--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Interface for calling Gnuplot from Sonata."
}


--	MODULE

local gnuplot = {
type = 'gnuplot',
-- basic common options
options = {'terminal','output','parametric','size','polar','grid','key',
  'title', 'xlabel','ylabel','xrange','yrange','zrange','trange','samples'},
-- basic function options
foptions = {'using','title','with','linetype','linestyle','linewidth','ls',
  'ln','lw'},
}


-- metha
gnuplot.__index = gnuplot


--- Represent parameters of the graphic.
--  @param G Gnuplot object.
--  @return String with object properties.
gnuplot.__tostring = function (self)
  local res = {}
  for k,v in pairs(self) do
    if type(v) == 'table' then
      local tmp = {}
      for p,q in pairs(v) do
        tmp[#tmp+1] = string.format('%s=%s', tostring(p), tostring(q))
      end
      v = string.format('{%s}', table.concat(tmp, ','))
    end
    res[#res+1] = string.format('%s=%s', tostring(k), tostring(v))
  end
  return string.format('{\n%s\n}', table.concat(res, ',\n'))
end


--- Save function result to tmp file.
--  @param fn Lua function.
--  @param tBase Range and step.
--  @return File name.
gnuplot._fn2file = function (fn, tBase)
  local name = os.tmpname()
  local xl = tBase.xrange and tBase.xrange[1] or (-10)
  local xr = tBase.xrange and tBase.xrange[2] or 10
  local N = tBase.samples or 100
  local dx = (xr-xl) / N
  local f = assert(io.open(name, 'w'), "Can't save to file")
  if tBase.surface then
    local yl = tBase.yrange and tBase.yrange[1] or (-10)
    local yr = tBase.yrange and tBase.yrange[2] or 10
    local dy = (yr-yl)/N
    for x = xl,xr,dx do
      for y = yl, yr, dy do f:write(x, ' ', y, ' ', fn(x,y), '\n') end
    end
  else
    for x = xl, xr, dx do
      f:write(x, ' ', fn(x), '\n')
    end
  end
  f:close()
  if isWindows then
    name = string.format(
      '%s%s%s', os.getenv('userprofile'), TEMP, name)
  end
  return name
end


--- Add function parameters
--  @param t Table with function definition.
--  @param base Argument range.
--  @return String representation of the plot command.
gnuplot._graph = function (t, tBase)
  -- function/file name
  local fn, str, nm = t[1], '', nil
  if type(fn) == 'table' then
    nm = fn.ismatrix and gnuplot._mat2file(fn) or gnuplot._tbl2file(fn)
  elseif type(fn) == 'function' then
    nm = gnuplot._fn2file(fn,tBase)
  else  -- type(fn) == 'string' !!
    nm = fn
  end
  -- prepare options
  for _,k in ipairs(gnuplot.foptions) do
    if t[k] then str = string.format('%s %s %s ', str, prepare(k, t[k])) end
  end
  return nm, str
end


--- Create new object, set metatable.
--  @param o Table with parameters or nil.
--  @return New 'gnuplot' object.
gnuplot._new = function () return setmetatable({}, gnuplot) end


--- Save one or two lists into tmp file
--  @param t1 First Lua table (list).
--  @param t2 Second Lua table (list) or nil.
--  @param fn Function of two arguments.
--  @return File name.
gnuplot._lst2file = function (t1, t2, fn)
  local name = os.tmpname()
  local f = assert(io.open(name, 'w'), "Can't save to file")
  if fn then -- must be function
    for _, v1 in ipairs(t1) do
      for _, v2 in ipairs(t2) do
        f:write(v1, ' ', v2, ' ', fn(v1,v2), '\n')
      end
    end
  elseif t2 then
    if type(t2) == 'table' then
      assert(#t1 == #t2, 'Different table length!')
      for i, v1 in ipairs(t1) do f:write(v1, ' ', t2[i], '\n') end
    else -- must be function
      for i, v1 in ipairs(t1) do f:write(v1, ' ', t2(v1), '\n') end
    end
  else
    for i, v1 in ipairs(t1) do f:write(i, ' ', v1, '\n') end
  end
  f:close()
  if isWindows then
    name = string.format(
      '%s%s%s', os.getenv('userprofile'), TEMP, name)
  end
  return name
end


--- Save matrix to tmp file.
--  @param t Sonata matrix.
--  @return File name.
gnuplot._mat2file = function (t)
  local name = os.tmpname()
  local f = assert(io.open(name, 'w'), "Can't save to file")
  for i = 1, t.rows do
    local row = t[i]
    for j = 1, t.cols do f:write(row[j], ' ') end
    f:write('\n')
  end
  f:close()
  if isWindows then
    name = string.format(
      '%s%s%s', os.getenv('userprofile'), TEMP, name)
  end
  return name
end


--- Save table to tmp file.
--  @param t Lua table with numbers.
--  @return File name.
gnuplot._tbl2file = function (t)
  local name = os.tmpname()
  local f = assert(io.open(name, 'w'), "Can't save to file")
  for _, row in ipairs(t) do
    for _, val in ipairs(row) do f:write(val, ' ') end
    f:write('\n')
  end
  f:close()
  if isWindows then
    name = string.format(
      '%s%s%s', os.getenv('userprofile'), TEMP, name)
  end
  return name
end


--- Prepare arguments for table or matrix.
--  @param v Table, matrix or dat-file.
--  @param ... Column indexes for plotting (e.g. 1,4,9), all by default
gnuplot._vecPrepare = function (v, ...)
  local ag = {...}
  if type(v) == 'table' then
    if #ag == 0 then
      -- show all
      local n = v.ismatrix and v.cols or #v[1] -- column #
      ag = {}
      for i = 1,n do ag[#ag+1] = i end
    end
    v = v.ismatrix and gnuplot._mat2file(v) or gnuplot._tbl2file(v)
  end
  return v, ag
end


--- Add new curve to the plot.
--  @param tCurve Table with function/table and parameters.
gnuplot.add = function (self, tCurve) self[#self+1] = tCurve end
about[gnuplot.add] = {"G:add(curve_v)", "Add new curve to figure."}


--- Get copy of graph options.
--  @return Copy of table.
gnuplot.copy = function (self)
  local cp = gnuplot._new()
  for k, v in pairs(self) do
    if type(v) == 'table' then
      local tmp = {}
      for p, q in pairs(v) do tmp[p] = q end
      cp[k] = tmp
    else
      cp[k] = v
    end
  end
  return cp
end
about[gnuplot.copy] = {"G:copy() --> cpy_G", "Get copy of the plot options."}


--- Matlab-like plotting
--  @param ... Can be "t", "t1,t2", "t1,fn", "t1,t2,name", "t,name" etc.
gnuplot.plot = function (_, ...)
  local ag = {...}
  local cmd = gnuplot._new()
  local i, n = 1, 1
  repeat
    -- ag[i] have to be table
    local var, name, legend = ag[i+1], nil, nil
    if type(var) == 'table' or type(var) == 'function' then
      name = gnuplot._lst2file(ag[i], var)
      i = i+2
    else
      name = gnuplot._lst2file(ag[i])
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
about[gnuplot.plot] = {":plot(x1_t, [y1_t, nm_s, x2_t,..])",
  "'x' is list of numbers, 'y' is either list or functin, 'nm' - curve name."}


--- Polar plot.
--  @param ... List of type x1,y1,nm1 or x1,y1,x2,y2 etc.
gnuplot.polarplot = function(_, ...)
  local ag, i, n = {...}, 1, 1
  local cmd = gnuplot._new()
  repeat
    local name = gnuplot._lst2file(ag[i], ag[i+1])
    i = i + 2
    local legend = nil
    if type(ag[i]) == 'string' then
      legend = ag[i]
      i = i + 1
    else
      legend = tostring(n)
    end
    cmd[#cmd+1] = {name, title=legend, with='lines'}
    n = n + 1
  until i > #ag
  cmd.polar = true
  cmd.grid = 'polar'
  cmd:show()
end
about[gnuplot.polarplot] = {':polarplot(x1_t, y1_t, [nm_s, x2_t, y2_t,..])',
  "Make polar plot. 'x' is list of numbers, 'y' is either list or functin, 'nm' - curve name."}


--- Plot graphic.
--  @return Table which can be used for plotting.
gnuplot.show = function (self)
  -- open Gnuplot
  local handle = assert(
    io.popen('gnuplot' .. (gnuplot.testmode and '' or ' -p'), 'w'),
    'Cannot open Gnuplot!')
  -- save options
  local cmd = {}
  for _,k in ipairs(gnuplot.options) do
    if self[k] ~= nil then cmd[#cmd+1] = command(k, self[k]) end
  end
  if self.raw then cmd[#cmd+1] = self.raw end
  -- prepare functions
  local fn = {}
  for i, f in ipairs(self) do
    fn[i] = string.format("'%s' %s", gnuplot._graph(f, self))
  end
  -- command
  if #fn > 0 then
    local cmd_plot = self.surface and 'splot ' or 'plot '
    cmd[#cmd+1] = cmd_plot .. table.concat(fn, ',')
  end
  local res = table.concat(cmd, '\n')
  -- send to Gnuplot
  handle:write(res, '\n')
  handle:close()
end
about[gnuplot.show] = {"G:show()", "Plot data, represented as Lua table." }


--- Surface plot.
--  @param ... List of type x1,y1,fn1,nm1 or x1,y1,fn1,x2,y2,fn2 etc.
gnuplot.surfplot = function(_, ...)
  local ag, i, n = {...}, 1, 1
  local cmd = gnuplot._new()
  repeat
    local name = gnuplot._lst2file(ag[i], ag[i+1], ag[i+2])
    i = i + 3
    local legend = nil
    if type(ag[i]) == 'string' then
      legend = ag[i]
      i = i + 1
    else
      legend = tostring(n)
    end
    cmd[#cmd+1] = {name, title=legend}
    n = n + 1
  until i > #ag
  cmd.surface = true
  cmd:show()
end
about[gnuplot.surfplot] = {':surfplot(x1_t, y1_t, fn1, [nm_s, x2_t, y2_t,..])',
  "Make surfacе plot. 'x' and 'y' are lists of numbers, 'fn' is functin, 'nm' - surface name."}


--- Plot table of data file.
--  @param v Table, matrix or dat-file.
--  @param ... Column indexes for plotting (e.g. 1,4,9), all by default
gnuplot.tplot = function (_, v, ...)
  local f, ag = gnuplot._vecPrepare(v, ...)
  local cmd = gnuplot._new()
  if #ag > 1 then
    for i = 2,#ag do
      cmd[#cmd+1] = {f, using={ag[1], ag[i]}, with='lines',
        title=string.format("%d:%d", ag[1], ag[i])}
    end
  else
    cmd[#cmd+1] = {f, with='lines', title='1:2'}
  end
  cmd.grid = true
  cmd:show()
end
about[gnuplot.tplot] = {":tplot(var, [x_N, y1_N, y2_N,..])",
  "Plot table, matrix or data file. Optional elements define columns."}


--- Polar plot table of data file.
--  @param v Table, matrix or dat-file.
--  @param ... Column indexes for plotting (e.g. 1,4,9), all by default
gnuplot.tpolar = function (_, v, ...)
  local f, ag = gnuplot._vecPrepare(v, ...)
  local cmd = gnuplot._new()
  if #ag > 1 then
    for i = 2,#ag do
      cmd[#cmd+1] = {f, using={ag[1], ag[i]}, with='lines',
        title=string.format("%d:%d", ag[1], ag[i])}
    end
  else
    cmd[#cmd+1] = {f, with='lines', title='1:2'}
  end
  cmd.polar = true
  cmd.grid = 'polar'
  cmd:show()
end
about[gnuplot.tpolar] = {":tpolar(var, [x_N, y1_N, y2_N,..])",
  "Polar plot for table, matrix or data file. Optional elements define columns."}


--- Sufrace plot from table of data file.
--  @param v Table, matrix or dat-file.
--  @param ... Column indexes for plotting (e.g. 1,4,9), all by default
gnuplot.tsurf = function (_, v, ...)
  local f, ag = gnuplot._vecPrepare(v, ...)
  local cmd = gnuplot._new()
  if #ag > 2 then
    for i = 3, #ag do
      cmd[#cmd+1] = {f, using={ag[1], ag[2], ag[i]},
        title=string.format("%d:%d:%d", ag[1], ag[2], ag[i])}
    end
  else
    cmd[#cmd+1] = {f, title='1:2:3'}
  end
  cmd.surface = true
  cmd:show()
end
about[gnuplot.tsurf] = {":tsurf(var, [x_N, y_N, z1_N, z2_N,..])",
  "Surface plot for table, matrix or data file. Optional elements define columns."}


-- constructor
setmetatable(gnuplot, {__call=function (_) return gnuplot._new() end})
about[gnuplot] = {" () --> new_G", "Prepare Gnuplot object.", help.NEW}


gnuplot.keys = 'keys'
about[gnuplot.keys] = {'.keys',
[[  Options / examples:
{math.sin, title='sin'}       -- plot using function, define in Lua; add legend
{'sin.dat', ln=1, lw=2}       -- plot data from file, use given color and width
{tbl, with='lines'}           -- plot data from Lua table, use lines
title='Graph name'            -- set title
xrange={0,10}                 -- range of x from 0 to 10
yrange={-2,2}                 -- range of y
zrange={0,5}                  -- range of z
trange={1,2}                  -- range for parametric functions
xtitle='A', ytitle='B'        -- axes names
terminal='jpeg'               -- save result as jpeg image
output='my_plot.jpg'          -- file name
parametric=true               -- create parametric plot
size='square'                 -- set square size
polar=true                    -- use polar coordinate system
grid='polar'                  -- polar grid
legend=false                  -- don't use legend
surface=true                  -- plot surface in 3D
samples=200                   -- define number of points
raw='set pm3d'                -- set Gnuplot options manually
]]
}


-- Comment to remove descriptions
gnuplot.about = about

return gnuplot

--===========================================
