--[[       sonatalib/graph.lua

--- Operations with graphs.
--
--  Object structure:                                    </br>
--  <code>{node1 = {nodeA=weightA, nodeB=weightB, ... }, </br>
--  { ... }                                              </br>
--  {nodeN = {nodeP=weightP, nodeQ=weightQ, ...}}</code> </br>
--  i.e. each node has links to adjucent nodes.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonatalib</a> collection, 2017-2019.

           module 'graph'
--]]

--------------- Tests ------------
--[[TEST

-- import 'graph'
Graph = require 'sonatalib.graph'

-- build graph
-- single name - node, names in brackets - edges 
-- letter w denotes weight of non directed edge
-- numbers are weights of directed edges
a = Graph {'a','b',{'a','c'},{'d','e',w=2},{'d','b',4,3}}

-- list of nodes
nd = a:nodes()
ans = #nd                      --> 5

-- list of edges
-- if an edge has different weight for different sizes
-- it is represented twice
ed = a:edges()
ans = #ed                      --> 4

-- has directed edges
ans = a:isDirected()           --> true

-- add node
a:add('h') 
-- add edge
a:add {'a','d'}
-- check size
-- (same as #a)
ans = a:size()                 --> 6

-- remove edge
a:remove {'a','d'}
-- remove node
a:remove('a')
-- new edge number
ed = a:edges()
ans = #ed                      --> 3

-- directed edges
-- second way to define
-- from first to second node
a:add {'c','p',w12=2} 
-- and vise versa
a:add {'c','q',w21=3}
ed = a:edges()
ans = #ed                      --> 5

-- make copy
b = a:copy()

-- completeness 
ans = b:isComplete()           --> false

-- prepare graph
c = Graph {{'a','b'},{'a','c'},{'b','d'},{'b','e'},{'c','f'},{'f','g'},{'f','h'},{'e','g'}}

-- is it weighted 
ans = c:isWeighted()           --> false

-- breadth first search
_,path = c:bfs('e','h')
ans = path[3]                  --> 'f'

-- depth first search
found,path = c:dfs('d','c')
ans = found                    --> true

-- update weight
-- (default is 1)
-- use 'add' for it
c:add{'a','b',w=0.5}
c:add{'b','e',w=0.4}
c:add{'c','f',w=2}

-- Dijkstra path search
dist,prev = c:pathD('a') 
ans = dist['g']                --> 1.9

-- Bellman-Ford path search
c:add{'f','h',w=-0.5}
dist,prev = c:pathBF('a')
ans = dist['h'] 

-- check negative edges
ans = c:isNegative()           --> true

-- show
print(b)

--]]

--	LOCAL

local SEARCH = 'search'

--- Get key with minimum value, remove it
--  @param tbl Table of pairs {node, weight}.
--  @return Node and correspondent weight.
local function getMin(tbl)
   local minval, key = math.huge 
   -- find new minimal value
   for k,v in pairs(tbl) do
      if v < minval then 
         key = k; minval = v
      end
   end
   -- exclude minimal value
   if key then tbl[key] = nil end
   return key, minval
end

--- Check object type.
--  @param t Object to check.
--  @return True if the object is graph.
local function isgraph(t) return type(t)=='table' and t.isgraph end

--- Check if the element is edge.
--  @param n Element to check.
--  @return True in case of edge.
local function isEdge(n) return type(n) == 'table' end

--- Count elements in table.
--  @param tbl Table to check.
--  @return Total number of the elements.
local function tblLen (tbl) 
   local n = 0
   for k in pairs(tbl) do n = n+1 end
   return n
end

--- Find path in the set of connected nodes.
--  @param tPrev Table where each value is a previous element for the key.
--  @param v Last node.
--  @return Table with sequence of nodes.
local function getPath(tPrev,v)
   local res = {v}
   -- get sequence
   while tPrev[v] ~= v do
      v = tPrev[v]
      res[#res+1] = v
   end
   -- invert
   local L = #res
   local n = math.modf(L/2)
   for i = 1,n do
      res[i], res[L-i+1] = res[L-i+1], res[i]
   end
   return res
end

--	INFO

local help = LC_DIALOG and (require "sonatalib.help") or {new=function () return {} end}

--	MODULE

local graph = {
type='graph', isgraph=true,
-- description
about = help:new("Operations with graphs."),
}
-- meta
graph.__index = graph

-- external library
graph.lc_struct = require 'sonatalib.struct'

--- Constructor example
--  @param t Some value.
--  @return New object of graph.
graph.new = function (self,t)
   local o = {}
   -- add nodes
   for _,elt in ipairs(t) do graph.add(o,elt) end
   return setmetatable(o,self)
end

--- Add node or edge.
--  @param G Graph.
--  @param t New element.
graph.add = function (G, t)
   if isEdge(t) then
      local t1,t2 = t[1], t[2]
      local w12,w21 = (t[3] or t.w12), (t[4] or t.w21)
      G[t1] = G[t1] or {}
      G[t2] = G[t2] or {}
      if w12 or w21 then
         G[t1][t2] = w12; G[t2][t1] = w21
      else
         -- no weights
         local w = t.w or 1
	 G[t1][t2] = w; G[t2][t1] = w
      end
   else
      -- node
      G[t] = G[t] or {}
   end
end
graph.about[graph.add] = {"add(G,e)","Add new node or edge to graph G. Node denoted as a single name, edge is a table of names (and weights if need)."}

--- Remove node or edge.
--  @param G Graph.
--  @param t Element to remove.
graph.remove = function (G, t)
   if isEdge(t) then
      local t1,t2 = t[1], t[2]
      G[t1][t2] = nil
      if not t.single then        -- change keyword ???
         G[t2][t1] = nil
      end
   else
      -- node
      for _,v in pairs(G) do v[t] = nil end
      G[t] = nil
   end
end
graph.about[graph.remove] = {"remove(G,e)", "Remove node or edge from the graph G. Node is a single name, edge - table of names."}

-- simplify constructor call
setmetatable(graph, {__call = function (self,v) return graph:new(v) end})
graph.Graph = 'Graph'
graph.about[graph.Graph] = {"Graph(t)", "Create new graph.", help.NEW}

--- Get graph nodes.
--  @param G Graph.
--  @return List of nodes.
graph.nodes = function (G)
   local res = {}
   for k in pairs(G) do res[#res+1] = tostring(k) end
   return res
end
graph.about[graph.nodes] = {"nodes(G)","List of graph nodes."}

--- Get graph edges.
--  @param G Graph.
--  @return List of edges.
graph.edges = function (G)
   local nodes, res = graph.nodes(G), {}
   for i = 1,#nodes do
      local ni = nodes[i]
      local Wi = G[ni]
      for j = i,#nodes do
         local nj = nodes[j]
	 local wij = Wi[nj]
	 local wji = G[nj][ni]
	 if wij and wji then
	    res[#res+1] = {ni,nj}
	    if wij ~= wji then res[#res+1] = {nj,ni} end
	 elseif wij then res[#res+1] = {ni,nj}
	 elseif wji then res[#res+1] = {nj,ni}
	 end
      end
   end
   return res
end
graph.about[graph.edges] = {"edges(G)", "List of graph edges."}

--- Make the graph copy.
--  @param G Graph.
--  @return Deep copy of the graph.
graph.copy = function (G)
   local res = graph:new({})
   for k,v in pairs(G) do
      local tmp = {}
      for n,w in pairs(v) do tmp[n] = w end
      res[k] = tmp
   end
   return res
end
graph.about[graph.copy] = {"copy(G)", "Get copy of the graph."}

--- Get number of nodes.
--  @param G Graph object.
--  @return Number of nodes.
graph.size = function (G) return tblLen(G) end
graph.__len = size

--- String representation
--  @param G Graph object.
--  @return String with compressed graph structure.
graph.__tostring = function (G)
   local nd, res = graph.nodes(G)
   if #nd <= 5 then 
      res = string.format('Graph {%s}', table.concat(nd,','))
   else
      res = string.format('Graph {%s -%d- %s}', 
                          tostring(nd[1]), #nd-2, tostring(nd[#nd]))
   end
   return res
end

--- Check graph completeness.
--  @param G Graph.
--  @return true if graph is complete.
graph.isComplete = function (G)
   local n = tblLen(G)
   for _,v in pairs(G) do
      if n ~= tblLen(v) then return false end
   end
   return true
end
graph.about[graph.isComplete] = {'isComplete(G)', 'Check completeness of the graph.', help.OTHER}

--- Check if graph has negative weights.
--  @param G Graph object.
--  @return True if found at least one negative edge.
graph.isNegative = function (G)
   for _,adj in pairs(G) do
      for _,v in pairs(adj) do
         if v < 0 then return true end
      end
   end
   return false
end
graph.about[graph.isNegative] = {'isNegative(G)', 'Check if the graph has negative edges.', help.OTHER}

--- Check if the graph is directed.
--  @param G Graph object.
--  @return True if found directed edge.
graph.isDirected = function (G)
   for n1,adj in pairs(G) do
      for n2,v in pairs(adj) do
         if G[n2][n1] ~= v then return true end
      end
   end
   return false
end
graph.about[graph.isDirected] = {'isDirected(G)', 'Check if the graph has directed edges.', help.OTHER}

--- Check if the graph has weights different from default value.
--  @param G Graph object.
--  @return True if found edge not equal to 1.
graph.isWeighted = function (G)
   for _,adj in pairs(G) do
      for _,v in pairs(adj) do
         if v ~= 1 then return true end
      end
   end
   return false
end
graph.about[graph.isWeighted] = {'isWeighted(G)', 'Check if any edge has weight different from 1.', help.OTHER}

--- Breadth first search.
--  @param G Graph.
--  @param start Initial node.
--  @param goal Goal node.
--  @return Result and found path.
graph.bfs = function (G, start, goal)
   local pred = {[start]=start}
   -- use queue
   local queue = graph.lc_struct.Queue()
   queue:push(start)
   -- run
   while not queue:isEmpty() do
      local node = queue:pop()
      -- found
      if node == goal then return true, getPath(pred, goal) end
      -- add successors
      for v in pairs(G[node]) do
         if not pred[v] then
	    queue:push(v) 
	    pred[v] = node
	 end
      end
   end
   return false
end
graph.about[graph.bfs] = {"bfs(G,start,goal)","Breadth first search. Return result and found path.", SEARCH}

--- Depth first search.
--  @param G Graph.
--  @param start Initial node.
--  @param goal Goal node.
--  @return Result and found path.
graph.dfs = function (G, start, goal)
   local pred = {[start]=start}
   -- use stack
   local stack = graph.lc_struct.Stack()
   stack:push(start) 
   -- run
   while not stack:isEmpty() do
      local node = stack:pop()
      -- found
      if node == goal then return true, getPath(pred, goal) end
      -- add successors
      for v in pairs(G[node]) do
         if not pred[v] then
	    stack:push(v)
	    pred[v] = node
	 end
      end
   end
   return false
end
graph.about[graph.dfs] = {"dfs(G,start,goal)","Depth first search. Return result and found path.", SEARCH}

--- Shortest path search using Dijkstra algorithm.
--  @param G Graph.
--  @param start Initial node.
--  @param goal Goal node.
--  @return Table of distances and predecessors or path and its length.
graph.pathD = function(G,start,goal)
   -- define local set 
   local set = {}
   for k in pairs(G) do set[k] = math.huge end
   set[start] = 0
   -- save results
   local prev, dist = {[start]=start}, {}
   -- run
   while true do
      local current, val = getMin(set)
      if not current then break end
      -- update minimal distance
      dist[current] = val
      for k,v in pairs(G[current]) do
         local alt = val + v
	 if set[k] and set[k] > alt then
	    set[k] = alt; prev[k] = current
	 end -- if
      end -- for
   end
   -- result
   if goal then
      return dsit[goal], getPath(prev,goal)
   else
      return dist, prev
   end
end
graph.about[graph.pathD] = {'pathD(G,start[,goal])', "Find shortest path using Dijkstra's algorithm. Return table of distances and predecessors. If goal is defined, return path and its length.", SEARCH}

--- Find shortest path with Bellman-Ford algorithm/
--  @param G Graph.
--  @param start Initial node.
--  @param goal Goal node.
--  @return Table of distances and predecessors or path and its length.
graph.pathBF = function (G, start,goal)
   -- initialize
   local prev, dist = {[start]=start}, {}
   local N = 0      -- number of nodes
   -- initialize
   for k in pairs(G) do dist[k] = math.huge; N = N+1 end
   dist[start] = 0
   -- relax
   for i = 1,N-1 do
      for u in pairs(G) do
         for v,d in pairs(G[u]) do
            local alt = dist[u] + d
            if alt < dist[v] then
	       dist[v] = alt; prev[v] = u
	    end -- if
         end -- for
      end -- for
   end
--[[
   -- check for negative circles
   for u in pairs(G) do
      for v,d in pairs(G[u]) do
         assert(dist[u] + d >= dist[v], 'Negative circle!')
      end
   end
]]
   -- result
   if goal then
      return dist[goal], getPath(prev,goal)
   else
      return dist, prev
   end
end
graph.about[graph.pathBF] = {'pathBF(G,start[,goal])','Shortest path search using Bellman-Ford algorithm.', SEARCH}

-- free memory in case of standalone usage
if not LC_DIALOG then graph.about = nil end

return graph

--=========================================
--TODO: add heap to Dijkstra's algorithm
