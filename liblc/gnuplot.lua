
-- Call Gnuplot from Lua

local gnuplot = {}
gnuplot.__index = gnuplot

gnuplot.type = 'gnuplot'

local help = require "liblc.help"
gnuplot.about = help:new("Interface for calling Gnuplot from Lua")

gnuplot.N = 100        -- devide interval into given number of points
gnuplot.about[gnuplot.N] = {"N", "If no step, devide interval into N number of points", help.CONST}

-- constructor
function gnuplot:new(o)
   local o = o or {}
   setmetatable(o, self)
   return o
end

-- table to file
local function tmptable(t)
   local name = os.tmpname()
   local f = io.open(name, 'w')
   for _, row in ipairs(t) do
      for i, val in ipairs(row) do f:write((i>1 and ',' or ''), val) end
      f:write('\n'); f:flush()
   end
   f:close()
   return name
end

-- parametric function to file
local function tmpparametric(fx, fy, from, to, step)
   local name = os.tmpname()
   local f = io.open(name, 'w')
   for t = from, to, step do f:write(fx(t), ',', fy(t), '\n') end
   f:flush(); f:close()
   return name
end

-- function to file
local function tmpfunction(fn, from, to, step)
   local name = os.tmpname()
   local f = io.open(name, 'w')
   for x = from, to, step do f:write(x, ',', fn(x), '\n') end
   f:flush(); f:close()
   return name
end

-- plot graphic
gnuplot.plot2d = function (g)
   -- reopen window
   local handle = io.popen('gnuplot' .. (g.permanent and ' -p' or ''), 'w')
   -- settings
   local cmd = {}
   g.xrange = g.xrange or {-10,10}
   g.separator = g.separator or ','
   g.N = g.N or gnuplot.N
   if g.xrange then cmd[#cmd+1] = string.format('set xrange [%f:%f]', g.xrange[1], g.xrange[2]) end
   if g.yrange then cmd[#cmd+1] = string.format('set yrange [%f:%f]', g.yrange[1], g.yrange[2]) end
   if g.trange then cmd[#cmd+1] = string.format('set trange [%f:%f]', g.trange[1], g.trange[2]) end
   if g.xlabel then cmd[#cmd+1] = string.format('set xlabel "%s"', g.xlabel) end
   if g.ylabel then cmd[#cmd+1] = string.format('set ylabel "%s"', g.ylabel) end
   if g.title then cmd[#cmd+1] = string.format('set title "%s"', g.title) end
   if g.add then cmd[#cmd+1] = table.concat(g.add, '\n') end
   cmd[#cmd+1] = string.format('set datafile separator "%s"', g.separator)

   -- command 'plot'
   local plot = {}
   local file_list = {}
   for _,f in ipairs(g) do
      local str = ''
      if not f.type then
         if type(f[1]) == 'string' then str = f[1]
	 elseif type(f[1]) == 'function' then
	    g.xrange[3] = g.xrange[3] or (g.xrange[2]-g.xrange[1])/g.N
	    str = tmpfunction(f[1], table.unpack(g.xrange))
	    table.insert(file_list, str)
	    str = '\"'..str..'\" smooth unique'
	 end
      elseif f.type == 'data' then
         if type(f[1]) == 'table' then
	    str = tmptable(f[1])
	    table.insert(file_list, str)
	 elseif type(f[1]) == 'string' then
	    str = f[1]
	 end
	 str = '\"'..str..'\"'
	 if f.smooth then str = str..' smooth '..f.smooth end
      elseif f.type == 'parametric' then
         if type(f[1]) == 'string' then 
	    str = f[1]; cmd[#cmd+1] = "set parametric\n"
	 elseif type(f[1]) == 'function' and type(f[2]) == 'function' then
	    g.trange[3] = g.trange[3] or (g.trange[2]-g.trange[1])/g.N
	    str = tmpparametric(f[1], f[2], table.unpack(g.trange))
	    table.insert(file_list, str)
	    str = '\"'..str..'\"'
	 end
      end
      if f.title then str = string.format('%s title "%s"', str, f.title) end
      plot[#plot+1] = str
   end
   if #plot > 0 then cmd[#cmd+1] = 'plot ' .. table.concat(plot, ',') end
   -- call Gnuplot
   handle:write(table.concat(cmd,'\n'),'\n')
   handle:flush()
   -- free resources
   if #file_list > 0 then
      os.execute('sleep 1')  -- waiting for image window
      for _, f in ipairs(file_list) do os.remove(f) end
   end
   handle:close()
   
   return getmetatable(g) and g or gnuplot:new(g)
end
gnuplot.about[gnuplot.plot2d] = {"plot2d(g)", "Plot data and parameters, represented as Lua table", help.BASE}

-- represent as table
gnuplot.__tostring = function (g) 
   local res = {}
   for _, f in ipairs(g) do
      local ftbl = {}
      for i = 1, #f do table.insert(ftbl, tostring(f[i])) end
      if f.type then table.insert(ftbl, "type='"..f.type.."'") end
      if f.title then table.insert(ftbl,"title='"..f.title.."'") end
      if f.smooth then table.insert(ftbl, "smooth='"..f.smooth.."'") end
      res[#res+1] = ' {'..table.concat(ftbl, ', ')..'}'
   end
   if g.title then res[#res+1] = " title='"..g.title.."'" end
   if g.xrange then res[#res+1] = ' xrange={'..table.concat(g.xrange,',')..'}' end
   if g.yrange then res[#res+1] = ' yrangle={'..table.concat(g.yrange,',')..'}' end
   if g.trange then res[#res+1] = ' trange={'..table.concat(g.trange,',')..'}' end
   if g.xlabel then res[#res+1] = " xlabel='"..g.xlabel.."'" end
   if g.ylabel then res[#res+1] = " ylable='"..g.ylabel.."'" end
   if g.separator then res[#res+1] = " separator='"..g.separator.."'" end
   res[#res+1] = ' permanent='..(g.permanent==true and 'true' or 'false')
   -- g.add ...
   return '{\n'..table.concat(res, ',\n')..'\n}'
end

setmetatable(gnuplot, {__call=function (self,v) return gnuplot:new(v) end})
gnuplot.about[help.NEW] = {"Gnu([g])", "Transfor given table into gnuplot object", help.NEW}

gnuplot.about['keys'] = {'keys',
[[ Table description:
{'sin(x)'}                                   -- print sinus using Gnuplot functions
{math.sin, title='sinus'}                    -- plot using function, define in Lua; add legend
{'sin.dat', type='data', smooth='unique'}    -- plot data from file, use special type of smoothing
{tbl, type='data', tible='Table of results'} -- plot data from Lua table, no smoothing
{math.sin, math.cos, type='parametric'}      -- use parametric functions
{'sin(t), cos(t)', type='parametric'}        -- parametric function from Gnuplot
title='Graph name'                           -- set title
xrange={0,10,0.1}                            -- range of x from 0 to 10, calculate Lua function values with step 0.1
yrange={-2,2}                                -- range of y
trange={1,2}                                 -- range for parametric functions
xtitle='A', ytitle='B'                       -- axes names
separator=','                                -- separator for data files
]],
help.BASE}

return gnuplot