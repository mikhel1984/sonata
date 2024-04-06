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
d = Graph {dir=true}
d:add('c', 'p', 5)
-- and vise versa
d:add('p', 'c', 3)
ans = d:edge {'c', 'p'}       -->  5

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
c:add('a', 'b', 0.5)
c:add('b', 'e', 0.4)
c:add('c', 'f', 2)

-- Dijkstra path search
dist, prev = c:dijkstra('a')
ans = dist['g']               -->  1.9

-- show
print(b)

--]]


--	LOCAL

local SEARCH = 'search'
local PROPERTY = 'property'
local EXPORT = 'export'


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
local function getPath (tPrev, v)
  local res = {v}
  -- get sequence
  while tPrev[v] ~= v do
    v = tPrev[v]
    res[#res+1] = v
  end
  -- reverse
  local L = #res
  local n = math.modf(L/2)
  for i = 1, n do
    res[i], res[L-i+1] = res[L-i+1], res[i]
  end
  return res
end


--- Make list of names.
--  @param n Expected node number.
--  @return list of strings.
local function makeNodes (n, s)
  assert(n > 0, 'Wrong node size')
  local res = {}
  for i = 1, n do res[#res+1] = string.format('%s%d', s, i) end
  return res
end


-- Show undirected edge
local mt_edge = {
__tostring = function (t)
  return string.format('%s -- %s', tostring(t[1]), tostring(t[2]))
end
}


-- Show directed edge
local mt_dir_edge = {
__tostring = function (t)
  return string.format('%s -> %s', tostring(t[1]), tostring(t[2]))
end
}


-- Make queue from two stacks
local Queue = {
  -- create object
  new = function () return {{}, {}} end,
  -- add element
  push = function (self, v) table.insert(self[1], v) end,
  -- check content
  isEmpty = function (self) return #self[2] == 0 and #self[1] == 0 end
}


-- remove element
function Queue.pop(self)
  local q1, q2 = self[1], self[2]
  if #q2 == 0 then
    while #q1 > 0 do
      table.insert(q2, table.remove(q1))
    end
  end
  return table.remove(q2)
end


--	INFO

local help = SonataHelp or {}
-- description
local about = {
__module__ = "Operations with graphs."
}


--	MODULE

local graph = { type='graph' }


-- meta
graph.__index = graph


--- String representation
--  @return String with compressed graph structure.
graph.__tostring = function (self)
  local nd = graph.nodes(self)
  local nm = self._dir and 'Digraph' or 'Graph'
  if #nd <= 5 then
    return string.format('%s {%s}', nm, table.concat(nd, ','))
  else
    return string.format('%s {%s -%d- %s}',
      nm, tostring(nd[1]), #nd-2, tostring(nd[#nd]))
  end
end


--- Make loop.
--  @param g Empty graph.
--  @param nodes Node number or list of names.
--  @return loop.
graph._C = function (g, nodes)
  for i = 2, #nodes do 
    graph.add(g, nodes[i], nodes[i-1]) 
  end
  graph.add(g, nodes[1], nodes[#nodes])
  return g
end


--- Find graph component.
--  @param G Source graph.
--  @parma n0 Initial node.
--  @return subgraph.
graph._component = function (G, n0)
  local res = graph._new(false)
  local src, dst = G._, res._
  local stack = {n0}
  repeat
    local node = table.remove(stack)
    if not dst[node] then
      local t = {}
      for k, v in pairs(src[node]) do
        t[k] = v
        table.insert(stack, k)
      end
      dst[node] = t
    end
  until #stack == 0
  return res
end


--- Make complete graph.
--  @param g Empty graph.
--  @param nodes List of names.
--  @return complete graph.
graph._K = function (g, nodes)
  for i = 1, #nodes do
    local ni = nodes[i]
    for j = i+1, #nodes do
      local nj = nodes[j]
      graph.add(g, ni, nj)
      if g._dir then graph.add(g, nj, ni) end
    end
  end
  return g
end


--- Prepare empty undirected graph.
--  @param dir Boolean for directed graph.
--  @return Undirected graph.
graph._new = function (dir)
  local o = {_={}, _dir=dir}
  return setmetatable(o, graph)
end


--- Make chain.
--  @param g Empty graph.
--  @param nodes List of names.
--  @return chain.
graph._P = function (g, nodes)
  for i = 2, #nodes do 
    graph.add(g, nodes[i], nodes[i-1]) 
  end
  return g
end


--- Add node or edge.
--  @param n1 Node.
--  @param n2 Second node in the edge.
--  @param w Edge weight.
graph.add = function (self, n1, n2, w)
  local g = self._
  g[n1] = g[n1] or {}
  if n2 then
    w = w or 1
    g[n2] = g[n2] or {}
    g[n1][n2] = w
    g[n2][n1] = self._dir and g[n2][n1] or false
  end
end
about[graph.add] = {"G:add(n1, n2=nil, w_d=1)", "Add new node or edge."}


--- Import edges from list.
--  @param t List with edges in form {node1, node2, weight}.
graph.addEdges = function (self, t)
  for _, v in ipairs(t) do graph.add(self, v[1], v[2], v[3]) end
end
about[graph.addEdges] = {"G:addEdges(list_t)",
  "Import edges and weights from list."}


--- Import nodes from list.
--  @param t List with nodes.
graph.addNodes = function (self, t)
  local g = self._
  for _, v in ipairs(t) do g[v] = g[v] or {} end
end
about[graph.addNodes] = {"G:addNodes(list_t)",
  "Import nodes from list."}


--- Breadth first search.
--  @param start Initial node.
--  @param goal Goal node.
--  @return Result and found path.
graph.bfs = function (self, vStart, vGoal)
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
    for v in pairs(self._[node]) do
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


--- Get list of components.
--  @return list with subgraphs.
graph.components = function (self)
  local res = {}
  for k in pairs(self._) do
    local new = true
    for _, v in ipairs(res) do
      if v._[k] then 
        new = false
        break
      end
    end
    if new then
      res[#res+1] = graph._component(self, k)
    end
  end
  return res
end
about[graph.components] = {"G:components() --> G_t", 
  "Get list of connected components."}


--- Combine several graphs into the single object.
--  @param gs List of graphs.
--  @return concatenated graph.
graph.concat = function (self, gs)
  local res = graph._new(gs[1]._dir)
  local dst = res._
  for i = 1, #gs do
    local g = gs[i]
    if g._dir ~= res._dir then error('Different types') end
    for k, adj in pairs(g._) do
      local tmp = dst[k] or {}
      for n, w in pairs(adj) do tmp[n] = w end
      dst[k] = tmp
    end
  end
  return res
end
graph.__concat = function (G1, G2) return graph:concat {G1, G2} end
about[graph.concat] = {":concat(G_t) --> new_G",
  "Combine graphs into one object."}


--- Make the graph copy.
--  @return Deep copy of the graph.
graph.copy = function (self)
  local res = graph._new(self._dir)
  for k, v in pairs(self._) do
    local tmp = {}
    for n, w in pairs(v) do tmp[n] = w end
    res._[k] = tmp
  end
  return res
end
about[graph.copy] = {"G:copy() --> cpy_G", "Get copy of the graph."}


--- Depth first search.
--  @param vStart Initial node.
--  @param vGoal Goal node.
--  @return Result and found path.
graph.dfs = function (self, vStart, vGoal)
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
    for v in pairs(self._[node]) do
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


--- Shortest path search using Dijkstra algorithm.
--  @param start Initial node.
--  @param goal Goal node.
--  @return Table of distances and predecessors or path and its length.
graph.dijkstra = function(self, vStart, vGoal)
  -- define local set
  local set = {}
  for k in pairs(self._) do set[k] = math.huge end
  set[vStart] = 0
  -- save results
  local prev, dist = {[vStart]=vStart}, {}
  -- run
  while true do
    local current, val = getMin(set)
    if not current then break end
    -- update minimal distance
    dist[current] = val
    for k, v in pairs(self._[current]) do
      local alt = val + (v or self._[k][current])
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
about[graph.dijkstra] = {
  'G:dijkstra(startNode, [goalNode]) --> dist_d, path_t|prev_t',
  "Find shortest path using Dijkstra's algorithm. Return table of distances and predecessors. If goal is defined, return path and its length.",
  SEARCH}


--- Show graph structure in dot notation.
--  @return String with structure.
graph.dot = function (self)
  local txt = {self._dir and "digraph {" or "graph {"}
  local line = self._dir and "->" or "--"
  -- edges
  for n1, adj in pairs(self._) do
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
  "Return graph structure in dot notation.", EXPORT}


--- Get edge weight.
--  @param t Pair of nodes.
--  @return Weight or nil.
graph.edge = function (self, t)
  local g = self._
  local n1, n2 = t[1], t[2]
  return g[n1][n2] or (not self._dir and g[n2][n1]) or nil
end
about[graph.edge] = {"G:edge(pair_t) --> weight_d|nil", "Get weight of the edge."}


--- Get list of edges.
--  @return List of node pairs.
graph.edges = function (self)
  local res = {}
  for n, adj in pairs(self._) do
    for m, v in pairs(adj) do
      if v then 
        local t = setmetatable({n, m}, self._dir and mt_dir_edge or mt_edge)
        res[#res+1] = t
      end
    end
  end
  return res
end
about[graph.edges] = {"G:edges() --> edges_t", "Get list of edges."}


--- Get adjucent input nodes.
--  @param node Current node.
--  @return list of inputs.
graph.nin = function (self, node)
  local res, t = {}, self._[node]
  if self._dir then
    for k, v in pairs(t) do
      if not v then res[#res+1] = k end
    end
  else
    for k in pairs(t) do res[#res+1] = k end
  end
  return res
end
about[graph.nin] = {"G:nin(node) --> nodes_t",
  "Find adjucent input nodes."}


--- Check graph completeness.
--  @return true if the graph is complete.
graph.isComplete = function (self)
  if self._dir then return false end
  local n = tblLen(self._)
  for _, adj in pairs(self._) do
    if n ~= tblLen(adj) then return false end
  end
  return true
end
about[graph.isComplete] = {'G:isComplete() --> bool',
  'Check completeness of the graph.', PROPERTY}


--- Check if the graph is directed.
--  @return True if found directed edge.
graph.isDirected = function (self) return self._dir end
about[graph.isDirected] = {'G:isDirected() --> bool',
  'Check if the graph is directed.', PROPERTY}


--- Check if the graph has Euler circle.
--  @return true when has Euler cirlce.
graph.isEuler = function (self)
  -- check degree
  if self._dir then
    for _, adj in pairs(self._) do
      local p, q = 0, 0    -- inputs and outputs
      for _, w in pairs(adj) do
        if w then p = p + 1 else q = q + 1 end
      end
      if p ~= q then return false end
    end
  else
    for _, adj in pairs(self._) do
      if tblLen(adj) % 2 ~= 0 then return false end
    end
  end
  -- check components
  local c = graph.components(self)
  for i = 1, #c do c[i] = graph.size(c[i]) end
  table.sort(c)
  return c[#c] > 0 and (#c == 1 or c[#c-1] == 1)
end
about[graph.isEuler] = {"G:isEuler() --> bool",
  "Check if the graph has Euler circle.", PROPERTY}


--- Check if the graph is tree.
--  @return true when tree.
graph.isTree = function (self)
  local m = #graph.edges(self)
  local n = #graph.nodes(self)
  return n - m == 1 
    and #graph.components(self) == 1
end
about[graph.isTree] = {'G:isTree() --> bool',
  'Check if the graph is tree.', PROPERTY}


--- Check if the graph has weights different from default value.
--  @return True if found edge not equal to 1.
graph.isWeighted = function (self)
  for _, adj in pairs(self._) do
    for _, v in pairs(adj) do
      if v and v ~= 1 then return true end
    end
  end
  return false
end
about[graph.isWeighted] = {'G:isWeighted() --> bool',
  'Check if any edge has weight different from 1.', PROPERTY}


--- Get matrix of weights.
--  @return weight matrix and corresponding node list.
graph.matrix = function (self)
  graph.ext_matrix = graph.ext_matrix or require('matlib.matrix')
  local mat = graph.ext_matrix
  local ns = graph.nodes(self)
  local m = mat:zeros(#ns)
  for i, v in ipairs(ns) do
    for j = i+1, #ns do
      local w = ns[j]
      m[i][j] = graph.edge(self, {v, w}) or 0
      m[j][i] = graph.edge(self, {w, v}) or 0
    end
  end
  return m, ns
end
about[graph.matrix] = {"G:matrix() --> weight_M, nodes_t",
  "Get weights of adjucent edges.", help.OTHER}


--- Get graph nodes.
--  @return List of nodes.
graph.nodes = function (self)
  local res = {}
  for k in pairs(self._) do res[#res+1] = k end
  return res
end
about[graph.nodes] = {"G:nodes() --> node_t", "List of nodes."}


--- Get adjucent output nodes.
--  @param node Current node.
--  @return list of outputs.
graph.nout = function (self, node)
  local res, t = {}, self._[node]
  if self._dir then
    for k, v in pairs(t) do
      if v then res[#res+1] = k end
    end
  else
    for k in pairs(t) do res[#res+1] = k end
  end
  return res
end
about[graph.nout] = {"G:nout(node) --> nodes_t",
  "Find adjucent output nodes."}


--- Generate random edges.
--  @param N Edge number.
graph.random = function (self, N)
  -- check / reset 
  local ns = {}
  local g = self._
  for i, v in pairs(g) do
    if next(v) then g[i] = {} end
    ns[#ns+1] = i
  end
  -- max number of edges
  local p = #ns
  p = p*(p-1) / (self._dir and 1 or 2)
  if N <= 0 or N > p then error('Wrong edge number') end
  -- fill
  if N <= p / 2 then
    -- add 
    for u = 1, N do
      local a, b = nil, nil
      repeat
        a = ns[math.random(#ns)]
        b = ns[math.random(#ns)]
      until g[a][b] == nil
      graph.add(self, a, b)
    end
  else
    -- make complete, then remove
    graph._K(self, ns)
    for u = p-1, N, -1 do
      local a, b = nil, nil
      repeat 
        a = ns[math.random(#ns)]
        b = ns[math.random(#ns)]
      until g[a][b] or (not self._dir and g[b][a])
      graph.remove(self, a, b)
    end
  end
end
about[graph.random] = {"G:random(edge_N)", 
  "Fill graph with random edges.", help.OTHER}

 
--- Remove node or edge.
--  @param n1 Node.
--  @param n2 Second node.
graph.remove = function (self, n1, n2)
  if n2 then
    -- edge
    self._[n1][n2] = nil
    self._[n2][n1] = nil
  else
    -- node
    for n3 in pairs(self._[n1]) do
      self._[n3][n1] = nil
    end
    self._[n1] = nil
  end
end
about[graph.remove] = {"G:remove(n1, [n2])",
  "Remove node or edge from the graph."}


--- Save graph as svg image.
--  @param name File name.
graph.toSvg = function (self, name)
  local cmd = string.format('dot -Tsvg -o %s.svg', name)
  local handle = assert(io.popen(cmd, 'w'), "Can't open dot!")
  handle:write(graph.dot(self))
  handle:close()
end
about[graph.toSvg] = {"G:toSvg(name_s)",
  "Convert graph to SVG image using Graphviz.", EXPORT}


--- Get number of nodes.
--  @return Number of nodes.
graph.size = function (self) return tblLen(self._) end
graph.__len = graph.size
about[graph.size] = {"G:size() --> nodes_N", "Get node number.", help.OTHER}


-- simplify constructor call
setmetatable(graph, {
__call = function (self, t) 
  t = t or {}
  local g = graph._new(t.dir or false)
  local name = t.name or 'n'
  if t.O then   
    -- only nodes
    local ns = (type(t.O) == 'number') and makeNodes(t.O, name) or t.O
    graph.addNodes(g, ns)
  elseif t.K then  
    -- complete
    local ns = (type(t.K) == 'number') and makeNodes(t.K, name) or t.K
    return graph._K(g, ns)
  elseif t.C then  
    -- loop
    local ns = (type(t.C) == 'number') and makeNodes(t.C, name) or t.C
    return graph._C(g, ns)
  elseif t.P then  
    -- chain
    local ns = (type(t.P) == 'number') and makeNodes(t.P, name) or t.P
    return graph._P(g, ns)
  end
  return g
end})
about[graph] = {" ([params_t]) --> new_G", 
  "Create graph. Parameters are {dir=bool, O|K|C|P=number|names_t, name='n'}.", 
  help.NEW}


-- Comment to remove descriptions
graph.about = about

return graph

--=========================================
--TODO: add heap to Dijkstra's algorithm
