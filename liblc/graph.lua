--[[       liblc/graph.lua

--- Operations with graphs.
--  @author My Name

           module 'graph'
--]]

--------------- Tests ------------
-- Define here your tests, save results to 'ans', use --> for equality and --~ for estimation.
--[[!!
Graph = require 'liblc.graph'

-- example
a = Graph()
ans = a.type                   --> 'graph'
]]

---------------------------------
-- @class table
-- @name graph
-- @field about Description of functions.
local graph = {}
graph.__index = graph

-- mark
graph.type = 'graph'
graph.isgraph = true
local function isgraph(t) return type(t)=='table' and t.isgraph end

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
graph.about = help:new("Operations with graphs.")

-- assume that single node is just name
-- table with two elements is underected edge with unit weight
-- edge with weight must be included for both direction independently
local function addnode(tbl,n1,n2,w1,w2)
   tbl[n1] = tbl[n1] or {}
   if n2 then
      tbl[n2] = tbl[n2] or {}
      if not (w1 or w2) then w1 = 1; w2 = 1 end
      tbl[n1][n2] = w1
      tbl[n2][n1] = w2
   end
end

local function remnode(tbl,n1,n2)
   if n2 then
      local w = tbl[n1][n2]
      tbl[n1][n2] = nil
      if tbl[n2][n1] == w then
         tbl[n2][n1] = nil
      end
   else
      for k in pairs(tbl[n1]) do tbl[k][n1] = nil end -- remove edges
      tbl[k] = nil
   end
end

--- Constructor example
--    @param t Some value.
--    @return New object of graph.
function graph:new(t)
   local o = {}
   -- add nodes
   for _,elt in ipairs(t) do graph.add(elt) end
   setmetatable(o,self)
   return o
end

graph.add = function (g, t)
   if type(t) == 'table' then
      local t1,t2,w12,w21 = t[1],t[2],t[3],t[4]
      g[t1] = g[t1] or {}
      g[t2] = g[t2] or {}
      if not (t3 or t4) then
         local w = t.w or 1
	 g[t1][t2] = w; g[t2][t1] = w
      else
         g[t1][t2] = w12; g[t2][t1] = w21
      end
   else
      g[t] = g[t] or {}
   end
end

graph.remove = function (g, t)
   if type(t) == 'table' then
      remnode(g, table.unpack(t))
   else
      remnode(g, t)
   end
end

-- simplify constructor call
setmetatable(graph, {__call = function (self,v) return graph:new(v) end})
graph.Graph = 'Graph'
graph.about[graph.Graph] = {"Graph(t)", "Create new graph.", help.NEW}

graph.nodes = function (g)
   local res = {}
   for k in pairs(g) do
      res[#res+1] = k
   end
   return res
end

graph.edges = function (g)
   local nodes = graph.nodes(g)
   local res = {}
   for i = 1,#nodes do
      local ni = nodes[i]
      for j = i,#nodes do
         local nj = nodes[j]
         local w = g[ni][nj]
	 if w then 
	    res[#res+1] = {ni,nj} 
	    if g[nj][ni] ~= w then res[#res+1] = {nj,ni} end
	 end
      end
   end
   return res
end

--[[
--- Method example
--   It is good idea to define method for the copy creation.
--   @param t Initial object.
--   @return Copy of the object.
graph.copy = function (t)
   -- some logic
   return graph:new(argument)
end
graph.about[graph.copy] = {"copy(t)", "Create a copy of the object.", help.BASE}
]]

graph.__len = function (g)
   local n = 0
   for i in pairs(g) do n = n+1 end
   return n
end

graph.__tostring = function (g)
   local nd = graph.nodes(g)
   local res
   if #nd <= 5 then 
      res = string.format('Graph {%s}', table.concat(nd,','))
   else
      res = string.format('Graph {%s - %d - %s}', 
                          tostring(nd[1]), #nd-2, tostring(nd[#nd]))
   end
   return res
end

-- free memory in case of standalone usage
if not lc_version then graph.about = nil end

return graph

--[[
a = graph {{'a','b'},{'a','c',2},{'c','b',3,4}}

a:add('pp')
a:add({'t','u'})

a:remove({'t','u'})

n = a:nodes()
for _,v in ipairs(n) do print(v) end

print(a)
]]
