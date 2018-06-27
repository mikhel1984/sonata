--[[       liblc/graph.lua

--- Operations with graphs.
--  @author My Name

           module 'graph'
--]]

--------------- Tests ------------
-- Define here your tests, save results to 'ans', use --> for equality and --~ for estimation.
--[[!!
Graph = require 'liblc.graph'

-- build graph
-- single names - nodes, names in brackets - edges 
-- letter w denotes weight of non directed edge
-- numbers are weigths of directed edges
a = Graph {'a','b',{'a','c'},{'d','e',w=2},{'d','b',4,3}}

-- list of nodes
nd = a:nodes()
ans = #nd                      --> 5

-- list of edges
ed = a:edges()
ans = #ed                      --> 3

-- add node
a:add('h') 
-- add edge
a:add {'a','d'}
-- check size
ans = #a                       --> 6

-- remove edge
a:remove {'a','d'}
-- remove node
a:remove('a')
-- new edge number
ed = a:edges()
ans = #ed                      --> 2

-- make copy
b = a:copy()

-- show
print(b)

]]

--	LOCAL

local SEARCH = 'search'

-- Get key with minimum value, remove it
local function getmin(tbl)
   local minval, key = math.huge 
   -- find new minimal value
   for k,v in pairs(tbl) do
      if v < minval then 
         key = k; minval = v
      end
   end
   if key then tbl[key] = nil end
   return key, minval
end

-- marker
local function isgraph(t) return type(t)=='table' and t.isgraph end

-- check element
local function isedge(m) return type(m) == 'table' end

-- count elements
local function tbllen (m) 
   local n = 0
   for k in pairs(m) do n = n+1 end
   return n
end

--	INFO
local help = lc_version and (require "liblc.help") or {new=function () return {} end}

--	MODULE
local graph = {
type='graph', isgraph=true,
-- description
about = help:new("Operations with graphs."),
}
-- meta
graph.__index = graph
-- external library
graph.lc_struct = require 'liblc.struct'

-- Constructor example
--    @param t Some value.
--    @return New object of graph.
graph.new = function (self,t)
   local o = {}
   -- add nodes
   for _,elt in ipairs(t) do graph.add(o,elt) end
   return setmetatable(o,self)
end

--- Add node or edge.
--    @param g Graph.
--    @param t New element.
graph.add = function (g, t)
   if isedge(t) then
      local t1,t2,w12,w21 = t[1], t[2], (t[3] or t.w12), (t[4] or t.w21)
      g[t1] = g[t1] or {}
      g[t2] = g[t2] or {}
      if not (t3 or t4) then
         -- no weights
         local w = t.w or 1
	 g[t1][t2] = w; g[t2][t1] = w
      else
         g[t1][t2] = w12; g[t2][t1] = w21
      end
   else
      -- node
      g[t] = g[t] or {}
   end
end
graph.about[graph.add] = {"add(e)","Add new node or edge. Node denoted as a single name, edge is a table of names (and weights if need)."}

--- Remove node or edge.
--    @param g Graph.
--    @param t Element to remove.
graph.remove = function (g, t)
   if isedge(t) then
      local t1,t2 = t[1], t[2]
      g[t1][t2] = nil
      if not t.single then        -- change keyword ???
         g[t2][t1] = nil
      end
   else
      -- node
      for _,v in pairs(g) do v[t] = nil end
      g[t] = nil
   end
end
graph.about[graph.remove] = {"remove(e)", "Remove node or edge. Node is a single name, edge - talbe of names."}

-- simplify constructor call
setmetatable(graph, {__call = function (self,v) return graph:new(v) end})
graph.Graph = 'Graph'
graph.about[graph.Graph] = {"Graph(t)", "Create new graph.", help.NEW}

--- Get graph nodes.
--    @param g Graph.
--    @return List of nodes.
graph.nodes = function (g)
   local res = {}
   for k in pairs(g) do res[#res+1] = tostring(k) end
   return res
end
graph.about[graph.nodes] = {"nodes()","List of graph nodes."}

--- Get graph edges.
--    @param g Graph.
--    @return List of edges.
graph.edges = function (g)
   local nodes, res = graph.nodes(g), {}
   for i = 1,#nodes do
      -- all nodes
      local ni = nodes[i]
      local Wi = g[ni]
      for j = i,#nodes do
         -- all pairs
         local nj = nodes[j]
         local w = Wi[nj]
	 if w then 
	    res[#res+1] = {ni,nj} 
	    -- different edges
	    if g[nj][ni] ~= w then res[#res+1] = {nj,ni} end
	 end -- if
      end -- for
   end
   return res
end
graph.about[graph.edges] = {"edges()", "List of graph edges."}

--- Make the graph copy.
--    @param g Graph.
--    @return Deep copy of the graph.
graph.copy = function (g)
   local res = graph:new({})
   for k,v in pairs(g) do
      res[k] = {}
      for n,w in pairs(v) do res[k][n] = w end
   end
   return res
end
graph.about[graph.copy] = {"copy()", "Get copy of the graph."}

-- Get number of nodes.
graph.__len = function (g) return tbllen(g) end

-- String representation
graph.__tostring = function (g)
   local nd, res = graph.nodes(g)
   if #nd <= 5 then 
      res = string.format('Graph {%s}', table.concat(nd,','))
   else
      res = string.format('Graph {%s -%d- %s}', 
                          tostring(nd[1]), #nd-2, tostring(nd[#nd]))
   end
   return res
end

--- Check graph completeness.
--    @param g Graph.
--    @return true if graph is complete.
graph.iscomplete = function (g)
   local n = tbllen(g)
   for _,v in ipairs(g) do
      if n ~= tbllen(v) then return false end
   end
   return true
end
graph.about[graph.iscomplete] = {'iscomplete(g)', 'Check completeness of the graph.', help.OTHER}

--- Bredth first search.
--    @param g Graph.
--    @param start Initial node.
--    @param goal Goal node.
--    @return Result and found path.
graph.bfs = function (g, start, goal)
   local visited,path = {},{}
   -- use queue
   local queue = graph.lc_struct.Queue()
   queue:add(start); visited[start] = true
   -- run
   while not queue:isempty() do
      local node = queue:rem()
      path[#path+1] = node
      -- found
      if node == goal then return true, path end
      local previous = queue:size()
      -- add sucessors
      for v in pairs(g[node]) do
         if not visited[v] then
	    queue:add(v); visited[v] = true
	 end
      end
      if queue:size() == previous then path[#path] = nil end  -- remove last node
   end
   return false, path
end
graph.about[graph.bfs] = {"bfs(g,start,goal)","Bredth first search. Return result and found path.", SEARCH}

--- Depth first search.
--    @param g Graph.
--    @param start Initial node.
--    @param goal Goal node.
--    @return Result and found path.
graph.dfs = function (g, start, goal)
   local visited, path = {}, {}
   -- use stack
   local stack = graph.lc_struct.Stack()
   stack:push(start); visited[start] = true
   -- run
   while not stack:isempty() do
      local node = stack:pop()
      path[#path+1] = node
      -- found
      if node == goal then return true, path end
      local previous = stack:size()
      -- add sucessors
      for v in pairs(g[node]) do
         if not visited[v] then
	    stack:push(v); visited[v] = true
	 end
      end
      if stack:size() == previous then path[#path] = nil end -- remove last node
   end
   return false, path
end
graph.about[graph.dfs] = {"dfs(start,goal)","Depth first search. Return result and found path.", SEARCH}

--- Shortest path search using Dijkstra algorithm.
--    @param g Graph.
--    @param start Initial node.
--    @param goal Goal node.
--    @return Table of distances and predecessors or path and its length.
graph.Dijkstra = function(g,start,goal)
   -- define local set 
   local set = {}
   for k in pairs(g) do set[k] = math.huge end
   set[start] = 0
   -- save results
   local prev, dist = {}, {}
   -- run
   while true do
      local current, val = getmin(set)
      if not current then break end
      -- update minimal distance
      dist[current] = val
      for k,v in pairs(g[current]) do
         local alt = val + v
	 if set[k] and set[k] > alt then
	    set[k] = alt; prev[k] = current
	 end -- if
      end -- for
   end
   -- result
   if goal then
      -- find path
      local p, k = {goal},goal 
      while prev[k] do
         k = prev[k]; p[#p+1] = k
      end
      -- change order
      local rev = {}
      for i = 1,#p do rev[#p-i+1] = p[i] end
      return dist[goal], rev
   else
      return dist, prev
   end
end
graph.about[graph.Dijkstra] = {'Dijkstra(g,start[,goal]', "Find shortest path using Dijkstra's algorithm. Return table of distances and predecessors. If goal is defined, return path and its length.", SEARCH}

--- Find shortest path with Bellman-Ford algorithm/
--    @param g Graph.
--    @param start Initial node.
--    @param goal Goal node.
--    @return Table of distances and predecessors or path and its length.
graph.BellmanFord = function (g, start,goal)
   -- initialize
   local prev, dist = {}, {}
   local N = 0      -- number of nodes
   -- initialize
   for k in pairs(g) do dist[k] = math.huge; N = N+1 end
   dist[start] = 0
   -- relax
   for i = 1,N-1 do
      for u in pairs(g) do
         for v,d in pairs(g[u]) do
            local alt = dist[u] + d
            if alt < dist[v] then
	       dist[v] = alt; prev[v] = u
	    end -- if
         end -- for
      end -- for
   end
--[[
   -- check for negative circles
   for u in pairs(g) do
      for v,d in pairs(g[u]) do
         assert(dist[u] + d >= dist[v], 'Negative circle!')
      end
   end
]]
   -- result
   if goal then
      -- save path
      local p,k = {goal},goal
      while prev[k] and #p < N do
         k = prev[k]; p[#p+1] = k
      end
      -- revert
      local rev = {}
      for i = 1,#p do rev[#p-i+1] = p[i] end
      return dist[goal], rev
   else
      return dist, prev
   end
end
graph.about[graph.BellmanFord] = {'BellmanFord(start[,goal]','Shortest path search using Bellman-Ford algorithm.', SEARCH}

--- "Smart" function that choose which algorithm to use base on weights.
--    @param g Graph.
--    @param start Initial node.
--    @param goal Goal node.
--    @return Table of distances and predecessors or path and its length.
graph.spath = function (g,start,goal)
   -- check weights
   local positive = true
   for u in pairs(g) do
      for _,d in pairs(g[u]) do
         if d < 0 then positive = false; break end
      end
      if not positive then break end
   end
   -- search
   if positive then 
      return graph.Dijkstra(g,start,goal) 
   else 
      return graph.BellmanFord(g,start,goal)
   end
end
graph.about[graph.spath] = {'spath(start[,goal])', "Find shortest path using algorithm of Dijkstra of Bellman-Ford.", SEARCH}

-- free memory in case of standalone usage
if not lc_version then graph.about = nil end

return graph

--=========================================
-- TODO: multiple edges for the same verteces
-- TODO: add heap to Dijkstra's algorithm
