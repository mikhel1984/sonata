

local gnuplot = {}
gnuplot.__index = gnuplot

gnuplot.type = 'gnuplot'

local FUNCTION, DATA = 'function', 'data'

local function tocsv(fname, tbl)
   local f = io.open(fname, 'w')
   for _, row in ipairs(tbl) do
      for i, val in ipairs(row) do
         f:write((i>1 and ',' or ''), val)
      end
      f:write('\n'); f:flush()
   end
   f:close()
end

function gnuplot:new(o)
   local o = o or {}
   setmetatable(o, self)
   return o
end

gnuplot.cmd = function (g, cmd)
   if g.handle then
      g.handle:write(cmd, '\n')
      g.handle:flush()
   end
end

gnuplot.set_fn = function (g, f, l) g[#g+1] = {f, legend=l, type=FUNCTION} end

gnuplot.set_src = function (g, s, l) g[#g+1] = {s, legend=l, type=DATA} end

gnuplot.set_xrange = function (g, x1, x2, dx) g.xrange = {x1, x2, dx} end

gnuplot.set_yrange = function (g, y1, y2, dy) g.yrange = {y1, y2, dy} end

gnuplot.set_xlabel = function (g, lbl) g.xlabel = lbl end

gnuplot.set_ylabel = function (g, lbl) g.ylabel = lbl end

gnuplot.set_title = function (g, txt) g.title = txt end

gnuplot.set_smooth = function (g, sm) g.smooth = sm end

gnuplot.set_separator = function (g, sep) g.separator = sep end

gnuplot.set_permanent = function (g, b) g.permanent = (b or true) end

gnuplot.close = function (g) 
   if g.handle then g.handle:close() end 
end

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

local function tmpfunction(fn, from, to, step)
   local name = os.tmpname()
   local f = io.open(name, 'w')
   for x = from, to, step do f:write(x, ',', fn(x), '\n') end
   f:flush(); f:close()
   return name
end

gnuplot.plot = function (g)
   -- reopen window
   if g.handle then g.handle:close() end
   g.handle = io.popen('gnuplot' .. (g.permanent and ' -p' or ''), 'w')
   -- settings
   local cmd = {}
   g.xrange = g.xrange or {-10,10}
   if g.xrange then cmd[#cmd+1] = string.format('set xrange [%d:%d]', g.xrange[1], g.xrange[2]) end
   if g.yrange then cmd[#cmd+1] = string.format('set yrange [%d:%d]', g.yrange[1], g.yrange[2]) end
   if g.xlabel then cmd[#cmd+1] = string.format('set xlabel "%s"', g.xlabel) end
   if g.ylabel then cmd[#cmd+1] = string.format('set ylabel "%s"', g.ylabel) end
   if g.title then cmd[#cmd+1] = string.format('set title "%s"', g.title) end
   cmd[#cmd+1] = string.format('set datafile separator "%s"', g.separator or ',')

   -- prepare command set
   local plot = {}
   local file_list = {}
   for _,f in ipairs(g) do
      local str = ''
      if not f.type or f.type == FUNCTION then
         if type(f[1]) == 'string' then str = f[1]
	 elseif type(f[1]) == 'function' then
	    g.xrange = g.xrange or {-10,10}
	    g.xrange[3] = g.xrange[3] or (g.xrange[2]-g.xrange[1])/100
	    str = tmpfunction(f[1], table.unpack(g.xrange))
	    table.insert(file_list, str)
	    str = '\"'..str..'\" smooth unique'
	 end
      elseif f.type == DATA then
         if type(f[1]) == 'table' then
	    str = tmptable(f[1])
	    table.insert(file_list, str)
	 elseif type(f[1]) == 'string' then
	    str = f[1]
	 end
	 str = '\"'..str..'\" smooth unique'
      end
      if f.legend then str = string.format('%s title "%s"', str, f.legend) end
      plot[#plot+1] = str
   end
   local plot_fn = 'plot ' .. table.concat(plot, ',')
   cmd[#cmd+1] = plot_fn
   gnuplot.cmd(g, table.concat(cmd,'\n'))
   --print(table.concat(cmd, '\n'))
   os.execute('sleep 1')  -- wait for image window
   for _, f in ipairs(file_list) do os.remove(f) end
   
   return getmetatable(g) and g or gnuplot:new(g)
end

----------------------------------------

--[[
g = gnuplot:new()
g:set_xrange(0, 5)
g:set_xlabel("A")
g:set_ylabel("B")
g:set_permanent()
g:set_fn('sin(x)*x', 'legend')
g:set_fn('cos(x)', 'other')
g:set_title("Test")

g:plot()
]]

g = gnuplot:new()
g:set_permanent()
g:set_fn(math.sin, "sinus")
g:plot()
