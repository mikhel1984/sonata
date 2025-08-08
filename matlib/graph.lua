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
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2025.

	module 'graph'
--]]


--------------- Tests ------------
--[[TEST_IT

-- use 'graph'
Graph = require 'matlib.graph'
-- for pack/unpack
D = require 'matlib.data'

-- build undirected graph
a = Graph()
a:addNodes {'a', 'b', 'c', 'd', 'e'}
-- list of nodes
nd = a:nodes()
ans = #nd                     -->  5

-- add node
a:add('h')
-- add edge
a:add('a', 'd')
-- check size
-- (same as #a)
ans = a:size()                -->  6

-- group of edges
a:addEdges {
  {'b', 'e'},
  {'x', 'y'},
  {'e', 'c', 2},  -- with waigth
}
-- remove node
a:remove('a')
-- new edge number
ed = a:edges()
ans = #ed                     -->  3

-- check properties
ans = a:isWeighted()          --> true

ans = a:isConnected()         --> false

ans = a:isDirected()          --> false

ans = a:isTree()              --> false

-- generate
b = Graph {K=5}
ans = b:isComplete()          --> true

ans = b:isEuler()             --> true

-- directed graph
c = Graph {C=4, dir=true, name='c'}
ans = c:edge {'c2', 'c1'}     --> 1

-- no edge
ans = (c:edge {'c1', 'c2'} == nil)  --> true

-- check node
ans = c:has 'c3'              --> true

-- input nodes
ans = #c:nin 'c1'             --> 1

-- output nodes
ans = #c:nout 'c1'            --> 1

-- same in undirected graph
ans = (#b:nin('n1') == #b:nout('n1'))  --> true

-- list of components
cps = a:components()
ans = #cps                    --> 4

-- concatenate graphs
aa = Graph:concat(cps)
ans = #aa                     --> #a

-- make copy
b1 = b:copy()
ans = (b == b1)               --> true

-- change
b1:remove('n1', 'n2')
ans = (b == b1)               --> false

-- adjacency matrix
print(c:matrix())

-- random graph
d = Graph{O=3, name='a'} .. Graph{O=3, name='b'}
-- generate edges with probability 0.5
d:randp(0.5)
print(d:edges())

-- set 8 random edges
d:rand(8)
ans = #d:edges()              --> 8

-- print in 'dot' notation
print(d:dot())

-- save svg image
d:toSvg('test_d')

-- search
g = Graph {K={1,2,3,4,5,6}}
ans = #g:search(1, 4, 'bfs')  --> 2

ans = (#g:search(1, 4, 'dfs') > 2)  --> true

-- dijkstra algorithm
-- update weight 1-4
g:add(1, 4, 10)
ans = #g:search(1, 4, 'dijkstra')   --> 3

-- data pack
t = D:pack(b)
ans = type(t)                 -->  'string'

-- data unpack
ans = D:unpack(t)             --> b

--]]


--	LOCAL

local Utils do
  local lib = require('matlib.utils')
  Utils = lib.utils
end

local PROPERTY = 'property'
local EXPORT = 'export'


--- Get key with minimum value, remove it
--  @param t Table of pairs {node, weight}.
--  @return Node and correspondent weight.
local function _getMin(t)
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
local function _tblLen (t)
  local n = 0
  for _ in pairs(t) do n = n+1 end
  return n
end


--- Find path in the set of connected nodes.
--  @param tPrev Table where each value is a previous element for the key.
--  @param v Last node.
--  @return Table with sequence of nodes.
local function _getPath (tPrev, v)
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
local function _makeNodes (n, s)
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
Queue.pop = function (self)
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


--- Find graph component.
--  @param G Source graph.
--  @parma n0 Initial node.
--  @return subgraph.
local function _component (G, n0)
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


-- Metamethods
graph.__index = graph


--- Check equality of two graphs.
--  @param G1 First object.
--  @param G2 Second object.
--  @return true when the graphs are equal.
graph.__eq = function (G1, G2)
  local mt = getmetatable(G1)
  if mt ~= graph or mt ~= getmetatable(G2) then return false end
  if G1._dir ~= G2._dir then return false end
  local g2 = G2._
  if _tblLen(G1._) ~= _tblLen(g2) then return false end
  -- check edges
  for n1, adj in pairs(G1._) do
    if not g2[n1] then return false end
    for n2 in pairs(adj) do
      local e = {n1, n2}
      if graph.edge(G1, e) ~= graph.edge(G2, e) then
        return false
      end
    end
  end
  return true
end


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


about['_mt'] = {"operations: g1==g2, g1~=g2, g1..g2, #g", nil, help.META}


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


--- Dump to binary string.
--  @param acc Accumulator table.
--  @return String with object representation.
graph._pack = function (self, acc)
  local t = {string.pack('B', acc['graph'])}
  local ns, p = {}, 1
  t[#t+1] = string.pack('B', self._dir and 1 or 0)
  -- nodes
  for k in pairs(self._) do
    local s = tostring(k)
    t[#t+1] = Utils.pack_str(s, acc)
    ns[s], p = p, p+1
  end
  t[#t+1] = '\0'
  -- edges
  for k, v in pairs(self._) do
    local s = tostring(k)
    t[#t+1] = Utils.pack_num(ns[s], acc)
    for q, w in pairs(v) do
      if w then   -- TODO simplify for directed graph
        local u = ns[tostring(q)]
        t[#t+1] = Utils.pack_num(u, acc)
        t[#t+1] = Utils.pack_num(w, acc)
      end
    end
    t[#t+1] = '\0'
  end
  return table.concat(t)
end


--- Undump from binary string.
--  @param src Source string.
--  @param pos Start position.
--  @param acc Accumulator table.
--  @param ver Pack algorithm version.
--  @return Graph object.
graph._unpack = function (src, pos, acc, ver)
  local ns, n, dir = {}, nil, nil
  dir, pos = string.unpack('B', src, pos)
  -- get nodes
  while string.byte(src, pos) ~= 0 do
    n, pos = string.unpack('B', src, pos)
    ns[#ns+1], pos = Utils.unpack_str(src, pos, acc[n], ver)
  end
  pos = pos + 1
  local gr, i, j, w = graph._new(dir == 1), nil, nil, nil
  -- get edges
  for i = 1, #ns do
    n, pos = string.unpack('B', src, pos)
    i, pos = Utils.unpack_num(src, pos, acc[n], ver)
    if string.byte(src, pos) == 0 then graph.add(gr, ns[i]) end
    while string.byte(src, pos) ~= 0 do
      n, pos = string.unpack('B', src, pos)
      j, pos = Utils.unpack_num(src, pos, acc[n], ver)
      n, pos = string.unpack('B', src, pos)
      w, pos = Utils.unpack_num(src, pos, acc[n], ver)
      graph.add(gr, ns[i], ns[j], w)
    end
    pos = pos + 1
  end
  return gr, pos
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
      res[#res+1] = _component(self, k)
    end
  end
  return res
end
about[graph.components] = {"G:components() --> G_t",
  "Get list of connected components.", help.OTHER}


--- Combine several graphs into the single object.
--  @param gs List of graphs.
--  @return concatenated graph.
graph.concat = function (self, gs)
  local res = graph._new(gs[1]._dir)
  local dst = res._
  for i = 1, #gs do
    local g = gs[i]
    if g._dir ~= res._dir then
      error 'Different types'
    end
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
  "Combine graphs into one object.", help.OTHER}


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
about[graph.copy] = {"G:copy() --> cpy_G",
  "Get copy of the graph.", help.OTHER}


--- Show graph structure in dot notation.
--  @param fname File name (optional).
--  @return String with structure.
graph.dot = function (self, fname)
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
  line = table.concat(txt, '\n')  -- reuse
  if fname then
    local f = assert(io.open(fname, 'w'))
    f:write(line)
    f:close()
    return 'Done'
  else
    return line
  end
end
about[graph.dot] = {"G:dot(fname_s=nil) --> str",
  "Save or return graph structure in dot notation.", EXPORT}


--- Get edge weight.
--  @param t Pair of nodes.
--  @return Weight or nil.
graph.edge = function (self, t)
  local g = self._
  local n1, n2 = t[1], t[2]
  return g[n1] and (g[n1][n2] or (not self._dir and g[n2][n1]) or nil)
end
about[graph.edge] = {"G:edge(pair_t) --> weight_d|nil", "Get weight of the edge."}


--- Get list of edges.
--  @return List of node pairs.
graph.edges = function (self)
  local res = {}
  for n, adj in pairs(self._) do
    for m, v in pairs(adj) do
      if v then
        local t = setmetatable({n, m, v}, self._dir and mt_dir_edge or mt_edge)
        res[#res+1] = t
      end
    end
  end
  return res
end
about[graph.edges] = {"G:edges() --> edges_t", "Get list of edges."}


--- Check if the graph has node.
--  @param n Node name.
--  @return true when node found
graph.has = function (self, n) return self._[n] and true or false end
about[graph.has] = {"G:has(node) --> bool", "Check if the graph has the node."}


--- Get adjucent input nodes.
--  @param node Current node.
--  @return list of inputs.
graph.nin = function (self, node)
  local res, t = {}, self._[node]
  if self._dir then
    for k, v in pairs(t) do
      if v then
        if self._[k][node] then res[#res+1] = k end
      else
        res[#res+1] = k
      end
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
  local n = _tblLen(self._) - 1
  for _, adj in pairs(self._) do
    if n ~= _tblLen(adj) then return false end
  end
  return true
end
about[graph.isComplete] = {'G:isComplete() --> bool',
  'Check completeness of the graph.', PROPERTY}


--- Check if the graph is connected.
--  @return true when it has 1 component.
graph.isConnected = function (self)
  local n = next(self._)
  if not n then return false end  -- empty
  local c = _component(self, n)
  return _tblLen(self._) == _tblLen(c._)
end
about[graph.isConnected] = {"G:isConnected() --> bool",
  "Check if the graph is connected.", PROPERTY}


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
      if _tblLen(adj) % 2 ~= 0 then return false end
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


--- Get adjacency matrix.
--  @return adjacency matrix and corresponding node list.
graph.matrix = function (self)
  graph.ext_matrix = graph.ext_matrix or require('matlib.matrix')
  local mat = graph.ext_matrix
  local ns = graph.nodes(self)
  local m = mat:zeros(#ns)
  for i, v in ipairs(ns) do
    for j = i+1, #ns do
      local w = ns[j]
      m[i][j] = graph.edge(self, {v, w}) and 1 or 0
      m[j][i] = graph.edge(self, {w, v}) and 1 or 0
    end
  end
  return m, ns
end
about[graph.matrix] = {"G:matrix() --> adjacency_M, nodes_t",
  "Get adjacency matrix and node list.", help.OTHER}


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


--- Generate random graph.
--  @param N Edge number.
graph.rand = function (self, N)
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
  if N <= 0 or N > p then
    error 'Wrong edge number'
  end
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
about[graph.rand] = {"G:rand(edge_N)",
  "Fill graph with random edges.", help.OTHER}


--- Generate random graph.
--  @param p Edge probability.
graph.randp = function (self, p)
  if p <= 0 or p > 1 then
    error "Wrong probability value"
  end
  -- check / reset
  local ns = {}
  local g = self._
  for i, v in pairs(g) do
    if next(v) then g[i] = {} end
    ns[#ns+1] = i
  end
  -- fill
  for i = 1, #ns do
    local a = ns[i]
    for j = i+1, #ns do
      local b = ns[j]
      if math.random() < p then graph.add(self, a, b) end
      if self._dir and math.random() < p then graph.add(self, b, a) end
    end
  end
end
about[graph.randp] = {"G:randp(probability_d)",
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
about[graph.remove] = {"G:remove(n1, n2=nil)",
  "Remove node or edge from the graph."}


--- Get number of nodes.
--  @return Number of nodes.
graph.size = function (self) return _tblLen(self._) end
graph.__len = graph.size
about[graph.size] = {"G:size() --> nodes_N",
  "Get node number. Equal to #G.", help.OTHER}


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


local search = {}

--- Breadth first search.
--  @param G Graph.
--  @param start Initial node.
--  @param goal Goal node.
--  @return table of predecessors.
search.bfs = function (G, vStart, vGoal)
  local pred = {[vStart]=vStart}
  -- use queue
  local q = Queue.new()
  Queue.push(q, vStart)
  -- run
  repeat
    local node = Queue.pop(q)
    -- found ?
    if node == vGoal then return pred end
    -- add successors
    for k, v in pairs(G._[node]) do
      if not pred[k] and (v or not G._dir) then
        Queue.push(q, k)
        pred[k] = node
      end
    end
  until Queue.isEmpty(q)
  return nil
end


--- Depth first search.
--  @param G Graph.
--  @param vStart Initial node.
--  @param vGoal Goal node.
--  @return table of predecessors.
search.dfs = function (G, vStart, vGoal)
  local pred = {}
  local stack = {{vStart, vStart}}
  -- run
  repeat
    -- add successors
    local curr = table.remove(stack)
    local node = curr[1]
    if not pred[node] then
      pred[node] = curr[2]
      -- found ?
      if node == vGoal then return pred end
      for k, v in pairs(G._[node]) do
        if not pred[k] and (v or not G._dir) then
          table.insert(stack, {k, node})
        end
      end
    end
  until #stack == 0
  return nil
end


--- Shortest path search using Dijkstra algorithm.
--  @param G Graph.
--  @param start Initial node.
--  @param goal Goal node.
--  @return table of predecessors.
search.dijkstra = function(G, vStart, vGoal)
  local set = {}
  for k in pairs(G._) do set[k] = math.huge end
  set[vStart] = 0
  -- save results
  local prev, dist = {[vStart]=vStart}, {}
  -- run
  while true do
    local current, val = _getMin(set)
    if not current then break end
    -- update minimal distance
    dist[current] = val
    for k, v in pairs(G._[current]) do
      if v or not G._dir then
        local alt = val + (v or G._[k][current])
        if set[k] and set[k] > alt then
          set[k] = alt
          prev[k] = current
        end
      end
    end
  end
  return prev[vGoal] and prev
end


--- Find path between 2 nodes.
--  @param a First node.
--  @param b Second node.
--  @param method String with method name.
--  @return found path or nil.
graph.search = function (self, a, b, method)
  local fn = assert(search[method], 'Method not found')
  local res = fn(self, a, b)
  return res and _getPath(res, b)
end
about[graph.search] = {"G:search(node1, node2, method_s) --> path_t|nil",
  "Find path between two nodes. Methods are: bfs, dfs, dijkstra.", help.OTHER}


-- simplify constructor call
setmetatable(graph, {
__call = function (self, t)
  t = t or {}
  local g = graph._new(t.dir or false)
  local name = t.name or 'n'
  if t.O then
    -- only nodes
    local ns = (type(t.O) == 'number') and _makeNodes(t.O, name) or t.O
    graph.addNodes(g, ns)
  elseif t.K then
    -- complete
    local ns = (type(t.K) == 'number') and _makeNodes(t.K, name) or t.K
    return graph._K(g, ns)
  elseif t.C then
    -- loop
    local ns = (type(t.C) == 'number') and _makeNodes(t.C, name) or t.C
    return graph._C(g, ns)
  elseif t.P then
    -- chain
    local ns = (type(t.P) == 'number') and _makeNodes(t.P, name) or t.P
    return graph._P(g, ns)
  end
  return g
end})
about[graph] = {" (params_t={}) --> new_G",
  "Create graph. Parameters are {dir=bool, O|K|C|P=number|names_t, name='n'}.",
  help.NEW}


-- Comment to remove descriptions
graph.about = about

return graph

--=========================================
--TODO: add heap to Dijkstra's algorithm
