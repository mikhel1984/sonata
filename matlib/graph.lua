--[[		sonata/lib/graph.lua

--- Operations with graphs.
--
--  Object structure: </br>
--  <code>{node1 = {nodeA=weightA, nodeB=weightB, ... }, </br>
--  { ... } </br>
--  {nodeN = {nodeP=weightP, nodeQ=weightQ, ...}}</code> </br>
--  i.e. each node has links to adjucent nodes.
--  "false" is used to mark paired of absent edges.
--
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2023.

	module 'graph'
--]]


--------------- Tests ------------
--[[TEST_IT

-- use 'graph'
Graph = require 'matlib.graph'

-- build undirected graph
a = Graph()
a:addNodes {'a', 'b', 'c', 'd', 'e'}
-- list of nodes
nd = a:nodes()
ans = #nd                     -->  5

-- list of edges
ed = a:edges()
ans = #ed                     -->  0

-- has directed edges
ans = a:isDirected()          -->  false

-- add node
a:add('h')
-- add edge
a:add('a', 'd')
-- check size
-- (same as #a)
ans = a:size()                -->  6

-- remove node
a:remove('a')
-- new edge number
ed = a:edges()
ans = #ed                     -->  0

-- directed graph
d = Graph:dir()
d:add('c', 'p', 2)
-- and vise versa
d:add('p', 'c', 3)
ans = d:edge('c', 'p')        -->  2

-- make copy
b = a:copy()

-- completeness
ans = b:isComplete()          -->  false

-- prepare graph
c = Graph() 
c:addEdges {
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
ans = c:isWeighted()          -->  false

-- print in 'dot' notation
print(c:dot())

-- save svg image
c:toSvg('test_c')

-- breadth first search
_,path = c:bfs('e','h')
ans = path[3]                 -->  'f'

-- depth first search
found,path = c:dfs('d','c')
ans = found                   -->  true

-- update weight (default is 1)
-- use 'add' method
c:add('a','b',0.5)
c:add('b','e',0.4)
c:add('c','f',2)

-- Dijkstra path search
dist,prev = c:pathD('a')
ans = dist['g']               -->  1.9

-- show
print(b)

--]]


--	LOCAL

local SEARCH = 'search'


--- Get key with minimum value, remove it
--  @param t Table of pairs {node, weight}.
--  @return Node and correspondent weight.
local function getMin(t)
  local minval, key = math.huge, nil
  -- find new minimal value
  for k, v in pairs(t) do
    if v < minval then
      key, minval = k, v
    end
  end
  -- exclude minimal value
  if key then t[key] = nil end
  return key, minval
end


--- Count elements in table.
--  @param t Table to check.
--  @return Total number of the elements.
local function tblLen (t)
  local n = 0
  for _ in pairs(t) do n = n+1 end
  return n
end


--- Find path in the set of connected nodes.
--  @param tPrev Table where each value is a previous element for the key.
--  @param v Last node.
--  @return Table with sequence of nodes.
local function getPath(tPrev, v)
  local res = {v}
  -- get sequence
  while tPrev[v] ~= v do
    v = tPrev[v]
    res[#res+1] = v
  end
  -- invert
  local L = #res
  local n = math.modf(L/2)
  for i = 1, n do
    res[i], res[L-i+1] = res[L-i+1], res[i]
  end
  return res
end


-- Make queue from two stacks
local Queue = {
  -- create object
  new = function () return {{}, {}} end,
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

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Operations with graphs."
}


--	MODULE

local graph = { type='graph'  }


-- meta
graph.__index = graph


--- String representation
--  @param G Graph object.
--  @return String with compressed graph structure.
graph.__tostring = function (G)
  local nd = graph.nodes(G)
  local nm = G._dir and 'Digraph' or 'Graph'
  if #nd <= 5 then
    return string.format('%s {%s}', nm, table.concat(nd, ','))
  else
    return string.format('%s {%s -%d- %s}', 
      nm, tostring(nd[1]), #nd-2, tostring(nd[#nd]))
  end
end


--- Prepare empty undirected graph.
--  @return Undirected graph.
graph._new = function ()
  local o = {_={}, _dir=false}
  return setmetatable(o, graph)
end


--- Add node or edge.
--  @param G Graph object.
--  @param n1 Node.
--  @param n2 Second node in the edge.
--  @param w Edge weight.
graph.add = function (G, n1, n2, w)
  local g = G._
  g[n1] = g[n1] or {}
  if n2 then
    w = w or 1
    g[n2] = g[n2] or {}
    g[n1][n2] = w
    g[n2][n1] = G._dir and g[n2][n1] or false
  end
end
about[graph.add] = {"G:add(n1, n2=nil, w_d=1)", "Add new node or edge."}


--- Import edges from list.
--  @param G Graph object.
--  @param t List with edges in form {node1, node2, weight}.
graph.addEdges = function (G, t)
  for _, v in ipairs(t) do graph.add(G, v[1], v[2], v[3]) end
end
about[graph.addEdges] = {"G:addEdges(list_t)", 
  "Import edges and weights from list."}


--- Import nodes from list.
--  @param G Graph object.
--  @param t List with nodes.
graph.addNodes = function (G, t)
  local g = G._
  for _, v in ipairs(t) do g[v] = g[v] or {} end
end
about[graph.addNodes] = {"G:addNodes(list_t)", 
  "Import nodes from list."}


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
    for v in pairs(G._[node]) do
      if not pred[v] then
        Queue.push(q, v)
        pred[v] = node
      end
    end
  until Queue.isEmpty(q)
  return false
end
about[graph.bfs] = {"G:bfs(startNode, goalNode) --> isFound, path_t",
  "Breadth first search. Return result and found path.", SEARCH}


--- Make the graph copy.
--  @param G Graph.
--  @return Deep copy of the graph.
graph.copy = function (G)
  local res = G._dir and graph:dir() or graph._new()
  for k, v in pairs(G._) do
    local tmp = {}
    for n, w in pairs(v) do tmp[n] = w end
    res._[k] = tmp
  end
  return res
end
about[graph.copy] = {"G:copy() --> cpy_G", "Get copy of the graph."}


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
    for v in pairs(G._[node]) do
      if not pred[v] then
        table.insert(stack, v)
        pred[v] = node
      end
    end
  until #stack == 0
  return false
end
about[graph.dfs] = {"G:dfs(startNote, goalNode) --> isFound, path_t",
  "Depth first search. Return result and found path.", SEARCH}


--- Prepare empty directed graph.
--  @return Directed graph.
graph.dir = function (self)
  local res = graph._new()
  res._dir = true
  return res
end
about[graph.dir] = {":dir() --> new_G", "Create directed graph.", help.NEW}


--- Show graph structure in dot notation.
--  @param G Graph object.
--  @return String with structure.
graph.dot = function (G)
  local txt = {G._dir and "digraph {" or "graph {"} 
  local line = G._dir and "->" or "--"
  -- edges
  for n1, adj in pairs(G._) do
    local nstr, empty = tostring(n1), true
    for n2, v in pairs(adj) do
      empty = false
      if v then 
        txt[#txt+1] = string.format("  %s %s %s;", nstr, line, tostring(n2))
      end
    end
    if empty then
      txt[#txt+1] = string.format("  %s;", nstr)
    end
  end
  txt[#txt+1] = "}"
  return table.concat(txt, '\n')
end
about[graph.dot] = {"G:dot() --> str", 
  "Return graph structure in dot notation."}


--- Get edge weight.
--  @param G Graph object.
--  @param n1 First node.
--  @param n2 Second node.
--  @return Weight or nil.
graph.edge = function (G, n1, n2)
  local g = G._
  return g[n1][n2] or (not G._dir and g[n2][n1]) or nil
end
about[graph.edge] = {"G:edge() --> weight_d|nil", "Get weight of the edge."}


--- Get list of edges.
--  @param G Graph object.
--  @return List of node pairs.
graph.edges = function (G)
  local res = {}
  for n, adj in pairs(G._) do
    for m, v in pairs(adj) do
      if v then res[#res+1] = {n, m} end
    end
  end
  return res
end
about[graph.edges] = {"G:edges() --> edges_t", "Get list of edges."}


--- Check graph completeness.
--  @param G Graph.
--  @return true if graph is complete.
graph.isComplete = function (G)
  if G._dir then return false end
  local n = tblLen(G._)
  for _, v in pairs(G._) do
    if n ~= tblLen(v) then return false end
  end
  return true
end
about[graph.isComplete] = {'G:isComplete() --> bool',
  'Check completeness of the graph.', help.OTHER}


--- Check if the graph is directed.
--  @param G Graph object.
--  @return True if found directed edge.
graph.isDirected = function (G)
  return G._dir
end
about[graph.isDirected] = {'G:isDirected() --> bool',
  'Check if the graph is directed.', help.OTHER}


--- Check if graph has negative weights.
--  @param G Graph object.
--  @return True if found at least one negative edge.
graph.isNegative = function (G)
  for _, adj in pairs(G._) do
    for _, v in pairs(adj) do
      if v and v < 0 then return true end
    end
  end
  return false
end
about[graph.isNegative] = {'G:isNegative() --> bool',
  'Check if the graph has negative edges.', help.OTHER}


--- Check if the graph has weights different from default value.
--  @param G Graph object.
--  @return True if found edge not equal to 1.
graph.isWeighted = function (G)
  for _, adj in pairs(G._) do
    for _, v in pairs(adj) do
      if v and v ~= 1 then return true end
    end
  end
  return false
end
about[graph.isWeighted] = {'G:isWeighted() --> bool',
  'Check if any edge has weight different from 1.', help.OTHER}


--- Get graph nodes.
--  @param G Graph.
--  @return List of nodes.
graph.nodes = function (G)
  local res = {}
  for k in pairs(G._) do res[#res+1] = k end
  return res
end
about[graph.nodes] = {"G:nodes() --> node_t", "List of nodes."}


--- Shortest path search using Dijkstra algorithm.
--  @param G Graph.
--  @param start Initial node.
--  @param goal Goal node.
--  @return Table of distances and predecessors or path and its length.
graph.pathD = function(G, vStart, vGoal)
  -- define local set
  local set = {}
  for k in pairs(G._) do set[k] = math.huge end
  set[vStart] = 0
  -- save results
  local prev, dist = {[vStart]=vStart}, {}
  -- run
  while true do
    local current, val = getMin(set)
    if not current then break end
    -- update minimal distance
    dist[current] = val
    for k, v in pairs(G._[current]) do
      local alt = val + (v or G._[k][current])
      if set[k] and set[k] > alt then
        set[k] = alt
        prev[k] = current
      end
    end -- for
  end
  -- result
  if vGoal then
    return dist[vGoal], getPath(prev, vGoal)
  else
    return dist, prev
  end
end
about[graph.pathD] = {'G:pathD(startNode, [goalNode]) --> dist_d, path_t|prev_t',
  "Find shortest path using Dijkstra's algorithm. Return table of distances and predecessors. If goal is defined, return path and its length.",
  SEARCH}


--- Remove node or edge.
--  @param G Graph.
--  @param n1 Node.
--  @param n2 Second node.
graph.remove = function (G, n1, n2)
  if n2 then 
    -- edge
    G._[n1][n2] = nil
    G._[n2][n1] = nil
  else
    -- node
    for n3 in pairs(G._[n1]) do
      G._[n3][n1] = nil
    end
    G._[n1] = nil
  end
end
about[graph.remove] = {"G:remove(n1, [n2])",
  "Remove node or edge from the graph."}


--- Save graph as svg image.
--  @param G Graph object.
--  @param name File name.
graph.toSvg = function (G, name)
  local cmd = string.format('dot -Tsvg -o %s.svg', name)
  local handle = assert(io.popen(cmd, 'w'), "Can't open dot!") 
  handle:write(graph.dot(G))
  handle:close()
end
about[graph.toSvg] = {"G:toSvg(name_s)", 
  "Convert graph to SVG image using Graphviz."}


--- Get number of nodes.
--  @param G Graph object.
--  @return Number of nodes.
graph.size = function (G) return tblLen(G._) end
graph.__len = graph.size
about[graph.size] = {"G:size() --> nodes_N", "Get node number.", help.OTHER}


-- simplify constructor call
setmetatable(graph, {__call = function (self) return graph._new() end})
about[graph] = {" () --> new_G", "Create undirected graph.", help.NEW}


-- Comment to remove descriptions
graph.about = about

return graph

--=========================================
--TODO: add heap to Dijkstra's algorithm
--TODO: A* path search
