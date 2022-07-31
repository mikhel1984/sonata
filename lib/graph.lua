--[[		sonata/lib/graph.lua

--- Operations with graphs.
--
--  Object structure: </br>
--  <code>{node1 = {nodeA=weightA, nodeB=weightB, ... }, </br>
--  { ... } </br>
--  {nodeN = {nodeP=weightP, nodeQ=weightQ, ...}}</code> </br>
--  i.e. each node has links to adjucent nodes.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.lib</a> collection, 2017-2022.

	module 'graph'
--]]

--------------- Tests ------------
--[[TEST

-- use 'graph'
Graph = require 'lib.graph'

-- build graph
-- single name - node, names in brackets - edges 
-- letter w denotes weight of non directed edge
-- numbers are weights of directed edges
a = Graph {'a','b',
  {'a','c'},
  {'d','e',w=2},
  {'d','b',4,3}
}

-- list of nodes
nd = a:nodes()
ans = #nd                     --> 5

-- list of edges
-- if an edge has different weight for different sizes
-- it is represented twice
ed = a:edges()
ans = #ed                     --> 4

-- has directed edges
ans = a:isDirected()          --> true

-- add node
a:add('h') 
-- add edge
a:add {'a','d'}
-- check size
-- (same as #a)
ans = a:size()                --> 6

-- remove edge
a:remove {'a','d'}
-- remove node
a:remove('a')
-- new edge number
ed = a:edges()
ans = #ed                     --> 3

-- directed edges
-- second way to define
-- from first to second node
a:add {'c','p',w12=2} 
-- and vise versa
a:add {'c','q',w21=3}
ed = a:edges()
ans = #ed                     --> 5

-- make copy
b = a:copy()

-- completeness 
ans = b:isComplete()          --> false

-- prepare graph
c = Graph {
  {'a','b'},
  {'a','c'},
  {'b','d'},
  {'b','e'},
  {'c','f'},
  {'f','g'},
  {'f','h'},
  {'e','g'}
}

-- is it weighted 
ans = c:isWeighted()          --> false

-- breadth first search
_,path = c:bfs('e','h')
ans = path[3]                 --> 'f'

-- depth first search
found,path = c:dfs('d','c')
ans = found                   --> true

-- update weight (default is 1)
-- use 'add' method
c:add{'a','b',w=0.5}
c:add{'b','e',w=0.4}
c:add{'c','f',w=2}

-- Dijkstra path search
dist,prev = c:pathD('a') 
ans = dist['g']               --> 1.9

-- Bellman-Ford path search
c:add{'f','h',w=-0.5}
dist,prev = c:pathBF('a')
ans = dist['h'] 

-- check negative edges
ans = c:isNegative()          --> true

-- show
print(b)

--]]

--	LOCAL

local SEARCH = 'search'

--- Get key with minimum value, remove it
--  @param t Table of pairs {node, weight}.
--  @return Node and correspondent weight.
local function getMin(t)
  local minval, key = math.huge 
  -- find new minimal value
  for k,v in pairs(t) do
    if v < minval then 
      key, minval = k, v
    end
  end
  -- exclude minimal value
  if key then t[key] = nil end
  return key, minval
end

--- Check object type.
--  @param t Object to check.
--  @return True if the object is graph.
local function isgraph(v) return type(v)=='table' and v.isgraph end

--- Check if the element is edge.
--  @param v Element to check.
--  @return True in case of edge.
local function isEdge(v) return type(v) == 'table' end

--- Count elements in table.
--  @param t Table to check.
--  @return Total number of the elements.
local function tblLen (t) 
  local n = 0
  for k in pairs(t) do n = n+1 end
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

-- Make queue from two stacks 
local Queue = {
  -- create object
  new = function () return {{},{}} end, 
  -- add element
  push = function (Q, v) table.insert(Q[1], v) end,
  -- check content
  isEmpty = function (Q) return #Q[2] == 0 and #Q[1] == 0 end
}

-- remove element
function Queue.pop(Q)
  if #Q[2] == 0 then
    while #Q[1] > 0 do
      table.insert(Q[2], table.remove(Q[1]))
    end
  end
  return table.remove(Q[2])
end

--	INFO

local help = SonataHelp or {new=function () return {} end}
-- description
local about = help:new("Operations with graphs.")

--	MODULE

local graph = {
type='graph', isgraph=true,
}

-- meta
graph.__index = graph

--- String representation
--  @param G Graph object.
--  @return String with compressed graph structure.
graph.__tostring = function (G)
  local nd = graph.nodes(G)
  if #nd <= 5 then 
    return string.format('Graph {%s}', table.concat(nd,','))
  else
    return string.format('Graph {%s -%d- %s}',
      tostring(nd[1]), #nd-2, tostring(nd[#nd]))
  end
end

--- Constructor example
--  @param t Table with nodes and edges.
--  @return New object of graph.
graph._new_ = function (self,t)
  local o = {}
  -- add nodes
  for _,elt in ipairs(t) do graph.add(o,elt) end
  return setmetatable(o,self)
end

--- Add node or edge.
--  @param G Graph.
--  @param v New element.
graph.add = function (G, v)
  if isEdge(v) then
    local t1,t2 = v[1], v[2]
    local w12,w21 = (v[3] or v.w12), (v[4] or v.w21)
    G[t1] = G[t1] or {}
    G[t2] = G[t2] or {}
    if w12 or w21 then
      G[t1][t2] = w12 
      G[t2][t1] = w21
    else
      -- no weights
      local w = v.w or 1
      G[t1][t2] = w 
      G[t2][t1] = w
    end
  else
    -- node
    G[v] = G[v] or {}
  end
end
about[graph.add] = {"add(v)","Add new node or edge to graph G. Node denoted as a single name, edge is a table of names (and weights if need)."}

--- Breadth first search.
--  @param G Graph.
--  @param start Initial node.
--  @param goal Goal node.
--  @return Result and found path.
graph.bfs = function (G, vStart, vGoal)
  local pred = {[vStart]=vStart}
  -- use queue
  local q = Queue.new()
  Queue.push(q, vStart)
  -- run
  repeat 
    local node = Queue.pop(q)
    if node == vGoal then 
      -- found
      return true, getPath(pred, vGoal) 
    end
    -- add successors
    for v in pairs(G[node]) do
      if not pred[v] then
        Queue.push(q, v) 
        pred[v] = node
      end
    end
  until Queue.isEmpty(q)
  return false
end
about[graph.bfs] = {"bfs(vStart,vGoal)","Breadth first search. Return result and found path.", SEARCH}

--- Make the graph copy.
--  @param G Graph.
--  @return Deep copy of the graph.
graph.copy = function (G)
  local res = graph:_new_({})
  for k,v in pairs(G) do
    local tmp = {}
    for n,w in pairs(v) do tmp[n] = w end
    res[k] = tmp
  end
  return res
end
about[graph.copy] = {"copy()", "Get copy of the graph."}

--- Depth first search.
--  @param G Graph.
--  @param vStart Initial node.
--  @param vGoal Goal node.
--  @return Result and found path.
graph.dfs = function (G, vStart, vGoal)
  local pred = {[vStart]=vStart}
  -- use stack
  local stack = {}
  table.insert(stack, vStart) 
  -- run
  repeat
    local node = table.remove(stack)
    if node == vGoal then 
      -- found
      return true, getPath(pred, vGoal) 
    end
    -- add successors
    for v in pairs(G[node]) do
      if not pred[v] then
        table.insert(stack, v)
        pred[v] = node
      end
    end
  until #stack == 0
  return false
end
about[graph.dfs] = {"dfs(vStart,vGoal)","Depth first search. Return result and found path.", SEARCH}

--- Get graph edges.
--  @param G Graph.
--  @return List of edges.
graph.edges = function (G)
  local nodes, res = graph.nodes(G), {}
  for i = 1,#nodes do
    local ni = nodes[i]
    local Wi = G[ni]             -- what if node is not string?
    for j = i,#nodes do
      local nj = nodes[j]
      local wij = Wi[nj]
      local wji = G[nj][ni]
      if wij and wji then
        res[#res+1] = {ni,nj}
        if wij ~= wji then res[#res+1] = {nj,ni} end
      elseif wij then 
        res[#res+1] = {ni,nj}
      elseif wji then 
        res[#res+1] = {nj,ni}
      end
    end
  end
  return res
end
about[graph.edges] = {"edges()", "List of graph edges."}

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
about[graph.isComplete] = {'isComplete()', 'Check completeness of the graph.', help.OTHER}

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
about[graph.isDirected] = {'isDirected()', 'Check if the graph has directed edges.', help.OTHER}

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
about[graph.isNegative] = {'isNegative()', 'Check if the graph has negative edges.', help.OTHER}

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
about[graph.isWeighted] = {'isWeighted()', 'Check if any edge has weight different from 1.', help.OTHER}

--- Get graph nodes.
--  @param G Graph.
--  @return List of nodes.
graph.nodes = function (G)
  local res = {}
  for k in pairs(G) do res[#res+1] = tostring(k) end
  return res
end
about[graph.nodes] = {"nodes()","List of graph nodes."}

--- Find shortest path with Bellman-Ford algorithm/
--  @param G Graph.
--  @param start Initial node.
--  @param goal Goal node.
--  @return Table of distances and predecessors or path and its length.
graph.pathBF = function (G, vStart, vGoal)
  -- initialize
  local prev, dist = {[vStart]=vStart}, {}
  local N = 0    -- number of nodes
  -- initialize
  for k in pairs(G) do dist[k] = math.huge; N = N+1 end
  dist[vStart] = 0
  -- relax
  for i = 1,N-1 do
    for u in pairs(G) do
      for v,d in pairs(G[u]) do
        local alt = dist[u] + d
        if alt < dist[v] then
          dist[v] = alt 
          prev[v] = u
        end 
      end -- v,d
    end -- u
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
  if vGoal then
    return dist[vGoal], getPath(prev,vGoal)
  else
    return dist, prev
  end
end
about[graph.pathBF] = {'pathBF(vStart,[vGoal])','Shortest path search using Bellman-Ford algorithm.', SEARCH}

--- Shortest path search using Dijkstra algorithm.
--  @param G Graph.
--  @param start Initial node.
--  @param goal Goal node.
--  @return Table of distances and predecessors or path and its length.
graph.pathD = function(G,vStart,vGoal)
  -- define local set 
  local set = {}
  for k in pairs(G) do set[k] = math.huge end
  set[vStart] = 0
  -- save results
  local prev, dist = {[vStart]=vStart}, {}
  -- run
  while true do
    local current, val = getMin(set)
    if not current then break end
    -- update minimal distance
    dist[current] = val
    for k,v in pairs(G[current]) do
      local alt = val + v
      if set[k] and set[k] > alt then
        set[k] = alt 
        prev[k] = current
      end 
    end -- for
  end
  -- result
  if vGoal then
    return dsit[vGoal], getPath(prev,vGoal)
  else
    return dist, prev
  end
end
about[graph.pathD] = {'pathD(vStart,[vGoal])', "Find shortest path using Dijkstra's algorithm. Return table of distances and predecessors. If goal is defined, return path and its length.", SEARCH}

--- Remove node or edge.
--  @param G Graph.
--  @param v Element to remove.
graph.remove = function (G, v)
  if isEdge(v) then
    local t1,t2 = v[1], v[2]
    G[t1][t2] = nil
    if not v.single then      -- change keyword ???
      G[t2][t1] = nil
    end
  else
    -- node
    for _,u in pairs(G) do u[v] = nil end
    G[v] = nil
  end
end
about[graph.remove] = {"remove(v)", "Remove node or edge from the graph G. Node is a single name, edge - table of names."}

--- Get number of nodes.
--  @param G Graph object.
--  @return Number of nodes.
graph.size = function (G) return tblLen(G) end
graph.__len = size

-- simplify constructor call
setmetatable(graph, {__call = function (self,v) return graph:_new_(v) end})
graph.Graph = 'Graph'
about[graph.Graph] = {"Graph {v1,v2,..}", "Create new graph.", help.NEW}

-- Comment to remove descriptions
graph.about = about

return graph

--=========================================
--TODO: add heap to Dijkstra's algorithm
