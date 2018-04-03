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
ans = #ed                      --> 5

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
ans = #ed                      --> 5

-- make copy
b = a:copy()

-- show
print(b)

]]

---------------------------------
-- @class table
-- @name graph
-- @field about Description of functions.
local graph = {type='graph', isgraph=true}
graph.__index = graph

-- marker
local function isgraph(t) return type(t)=='table' and t.isgraph end

graph.lc_struct = require 'liblc.struct'

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
graph.about = help:new("Operations with graphs.")

local function isedge(m) return type(m) == 'table' end

local function tbllen (m) 
   local n = 0
   for k in pairs(m) do n = n+1 end
   return n
end

--- Constructor example
--    @param t Some value.
--    @return New object of graph.
function graph:new(t)
   local o = {}
   -- add nodes
   for _,elt in ipairs(t) do graph.add(o,elt) end
   setmetatable(o,self)
   return o
end

graph.add = function (g, t)
   if isedge(t) then
      local t1,t2,w12,w21 = t[1], t[2], t[3] or t.w12, t[4] or t.w21
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
graph.about[graph.add] = {"add(e)","Add new node or edge. Node denoted as a single name, edge is a table of names (and weights if need)."}

graph.remove = function (g, t)
   if isedge(t) then
      local t1,t2 = t[1], t[2]
      g[t1][t2] = nil
      if not t.single then        -- change keyword ???
         g[t2][t1] = nil
      end
   else
      for _,v in pairs(g) do v[t] = nil end
      g[t] = nil
   end
end
graph.about[graph.remove] = {"remove(e)", "Remove node or edge. Node is a single name, edge - talbe of names."}

-- simplify constructor call
setmetatable(graph, {__call = function (self,v) return graph:new(v) end})
graph.Graph = 'Graph'
graph.about[graph.Graph] = {"Graph(t)", "Create new graph.", help.NEW}

graph.nodes = function (g)
   local res = {}
   for k in pairs(g) do res[#res+1] = k end
   return res
end
graph.about[graph.nodes] = {"nodes()","List of graph nodes."}

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
graph.about[graph.edges] = {"edges()", "List of graph edges."}

graph.copy = function (g)
   local res = graph:new({})
   for k,v in pairs(g) do
      res[k] = {}
      for n,w in pairs(v) do res[k][n] = w end
   end
   return res
end
graph.about[graph.copy] = {"copy()", "Get copy of the graph."}

graph.__len = function (g)
   return tbllen(g)
end

graph.__tostring = function (g)
   local nd = graph.nodes(g)
   local res
   if #nd <= 5 then 
      res = string.format('Graph {%s}', table.concat(nd,','))
   else
      res = string.format('Graph {%s -%d- %s}', 
                          tostring(nd[1]), #nd-2, tostring(nd[#nd]))
   end
   return res
end

graph.iscomplete = function (g)
   local n = tbllen(g)
   for _,v in ipairs(g) do
      if n ~= tbllen(v) then return false end
   end
   return true
end
graph.about[graph.iscomplete] = {'iscomplete(g)', 'Check completeness of the graph.', help.OTHER}

graph.bfs = function (g, start, goal)
   local visited,path = {},{}
   local queue = graph.lc_struct.Queue()
   queue:add(start); visited[start] = true
   while not queue:isempty() do
      local node = queue:rem()
      path[#path+1] = node
      if node == goal then
         return true
      end
      local previous = queue:size()
      for v in pairs(g[node]) do
         if not visited[v] then
	    queue:add(v); visited[v] = true
	 end
      end
      if queue:size() == previous then path[#path] = nil end
   end
   return nil
end

graph.dfs = function (g, start, goal)
   local visited, path = {}, {}
   local stack = graph.lc_struct.Stack()
   stack:push(start); visited[start] = true
   while not stack:isempty() do
      local node = stack:pop()
      path[#path+1] = node
      if node == goal then
         return path
      end
      local previous = stack:size()
      for v in pairs(g[node]) do
         if not visited[v] then
	    stack:push(v); visited[v] = true
	 end
      end
      if stack:size() == previous then path[#path] = nil end
   end
   return nil
end

-- free memory in case of standalone usage
if not lc_version then graph.about = nil end

return graph

-- TODO: multible edges for the same verteces

--[[
a = graph {{'a','b'},{'a','c',w=2},{'c','b',3,4}}
print(a)
b = a:copy()
print(b)
a:add('pp')
a:add({'t','u'})

a:remove({'t','u'})
a:remove('a')

n = a:edges()
for _,v in ipairs(n) do print(v[1],v[2]) end

print(a)
]]
