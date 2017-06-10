

local gnuplot = {}
gnuplot.__index = gnuplot

gnuplot.type = 'gnuplot'

local FUNCTION, DATA = 'function', 'data'

function gnuplot:new(o)
   local o = o or {}
   setmetatable(o, self)
   return o
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
   local handle = io.popen('gnuplot' .. (g.permanent and ' -p' or ''), 'w')
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
      if not f.type then
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
      if f.title then str = string.format('%s title "%s"', str, f.title) end
      plot[#plot+1] = str
   end
   local plot_fn = 'plot ' .. table.concat(plot, ',')
   cmd[#cmd+1] = plot_fn
   -- execute
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

gnuplot.__tostring = function (g) 
   local res = {'{'}
   for _, f in ipairs(g) do
      local ftbl = {}
      for i = 1, #f do table.insert(ftbl, tostring(f[i])) end
      if f.title then table.insert(ftbl,"'"..f.title.."'") end
      res[#res+1] = ' {'..table.concat(ftbl, ', ')..'}'
   end
   if g.title then res[#res+1] = " title='"..g.title.."'" end
   if g.xrange then res[#res+1] = ' xrange={'..table.concat(g.xrange,',')..'}' end
   if g.yrange then res[#res+1] = ' yrangle={'..table.concat(g.yrange,'.')..'}' end
   if g.xlabel then res[#res+1] = " xlabel='"..g.xlabel.."'" end
   if g.ylabel then res[#res+1] = " ylable='"..g.ylabel.."'" end
   res[#res+1] = ' permanent='..(g.permanent==true and 'true' or 'false')
   res[#res+1] = '}'
   return table.concat(res, '\n')
end

----------------------------------------

g = gnuplot.plot
{
   {'sin(x)', title ='sinus x'},
   {'cos(x)', title ='cosinus x'},
   title = "Test",
   xrange = {0,5},
   yrange = {-1,1},
   xlabel = "X",
   ylabel = "Y",
   permanent=true,
}

print(g)
