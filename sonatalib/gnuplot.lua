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
Gnu = require 'sonatalib.gnuplot'
Gnu.testmode = true -- just for testing

-- table of functions
a = {{math.sin, title='sin'},{math.cos, title='cos'}}
Gnu.plot(a)

-- save as object
-- to simplify modification
g = Gnu(a)
g.xrange = {0,10}   -- add rangle
-- save to file 
g.terminal = 'png'
g.output = 'test.png'
g:plot()

-- copy parameters to other object
b = g:copy()
print(b)

-- check correctness of the table
ans = b:isAvailable()              --> true

-- send 'raw' command to Gnuplot 
Gnu.plot {raw='plot x**2-2*x+1; set xlabel "X"; set ylabel "Y"'}

-- print Lua table
tmp = {{1,1},{2,2},{3,3},{4,4}}
Gnu.plot {{tmp,with='lines'}}

-- define function
fn1 = function (x) return x^2-x end
Gnu.plot {{fn1,with='lines',title='x^2-x'}}

-- simple plot 
t1 = {1,2,3,4,5}
t2 = {2,4,6,4,2}
-- can be called as "plot(t1,t2,'some curve')"
Gnu.simple(t1,t2,'some curve')

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
  return k,v
end

--- Combine all options as keys
--  @param t Table with parameters.
--  @return List of options.
local function collect(t) 
  local res = {}
  for _,v in ipairs(t) do res[v] = true end
  return res
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
foptions = {'title','with','linetype','linestyle','linewidth','ls','ln','lw'},
-- description
about = help:new("Interface for calling Gnuplot from Lua."),
}
-- metha
gnuplot.__index = gnuplot

-- divide interval into given number of points
gnuplot.N = 100
gnuplot.about[gnuplot.N] = {"N[=100]", "If no samples, divide interval into N points.", help.CONST}

-- option checker
local acc = {options=collect(gnuplot.options), foptions=collect(gnuplot.foptions)}
acc.options.surface = true
acc.options.raw = true
acc.foptions.file = true
acc.foptions.raw = true

--- Check if all options in the table are available
--  @param G Table with optinos.
--  @return True if no unexpected parameters.
gnuplot.isAvailable = function (G) 
  local available = true
  for k,v in pairs(G) do
    -- find options which were not predefined
    if not acc.options[k] then
      if type(k) == 'number' and type(v) == 'table' then
        for p,q in pairs(v) do
          if not acc.foptions[p] and type(p) ~= 'number' then
            io.write(p, ' is not predefined, use "raw" for this option!\n')
            available = false
          end -- if
        end -- for p,q
      else
        io.write(k, ' is not predefined, use "raw" for this option!\n')
        available = false
      end -- type
    end -- acc.options
  end -- k,v
  return available
end
gnuplot.about[gnuplot.isAvailable] = {"isAvailable(G)", "Check if all options in table are predefined in program.", help.OTHER}

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
    for x = xl,xr,dx do f:write(x,' ',fn(x),'\n') end
  end
  f:close()
  return string.format('"%s"', name)
end

-- Prepare functions representation
--gnuplot._str_ = {
--  table=gnuplot._tbl2file_,
--  ['function']=gnuplot._fn2file_,
--  string=function (x) return string.format('"%s"', x) end
--}

--- Add function parameters
--  @param t Table with function definition.
--  @param base Argument range.
--  @return String representation of the plot command.
gnuplot._graph_ = function (t,base)
  -- function/file name
  local fn, str = t[1]
  if type(fn) == 'table' then
    str = gnuplot._tbl2file_(fn,base)
  elseif type(fn) == 'function' then
    str = gnuplot._fn2file_(fn,base)
  else -- type(fn) == 'string' !!
    str = string.format('"%s"', fn)
  end
  --local str = gnuplot._str_[type(fn)](fn,base)
  -- prepare options
  for _,k in ipairs(gnuplot.foptions) do
    if t[k] then str = string.format('%s %s %s ', str, prepare(k,t[k])) end
  end
  if t.raw then str = string.format('%s %s', str, t.raw) end

  return str
end

--- Create new object, set metatable.
--  @param self Pointer to parent table.
--  @param o Table with parameters or nil.
--  @return New 'gnuplot' object.
gnuplot.new = function (self,o) return setmetatable(o or {}, self) end

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
gnuplot.plot = function (G)
  if not gnuplot.isAvailable(G) then return end
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
gnuplot.about[gnuplot.plot] = {"plot(G)", "Plot data, represented as Lua table." }

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
    for i, v1 in ipairs(t1) do f:write(v1, ' ', t2[i], '\n') end
  else
    for i, v1 in ipairs(t1) do f:write(i, ' ', v1, '\n') end
  end  
  f:close()
  return name
end

--- Simplified plotting
--  @param ... Can be "t", "t1,t2", "t1,t2,name", "t,name" etc.
gnuplot.simple = function (...)
  local ag, cmd = {...}, {grid=true}
  local i, n = 1, 1
  repeat 
    -- ag[i] have to be table
    local name, legend
    if type(ag[i+1]) == 'table' then
      assert(#ag[i] == #ag[i+1], "Different table length!")
      name = gnuplot._lst2file_(ag[i], ag[i+1])
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
  -- show
  gnuplot.plot(cmd)
end
gnuplot.about[gnuplot.simple] = {"simple(x1,[y1,[nm,[x2,..]]])", "Simplified, Matlab-like plot function. Each argument is either the list of numbers or the curve name."}

-- constructor
setmetatable(gnuplot, {__call=function (self,v) return gnuplot:new(v) end})
gnuplot.Gnu = 'Gnu'
gnuplot.about[gnuplot.Gnu] = {"Gnu([G])", "Transform given table into gnuplot object.", help.NEW}

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
  plot = gnuplot.simple
  lc.about[plot] = {"plot(x1,[y1,[nm,[x2,..]]])", gnuplot.about[gnuplot.simple][2]}
end

-- free memory if need
if not LC_DIALOG then gnuplot.about = nil end

return gnuplot

--===========================================
--TODO: plot matrix columns (rows)
--TODO: optional (x,f(x)) in simple ?