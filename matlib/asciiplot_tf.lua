--[[		sonata/lib/asciiplot.lua

--- Use pseudography for data visualization.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.matlib</a> collection, 2017-2025.

	module 'asciiplot'
--]]


local _utils = require("matlib/utils")
local _tree = _utils.tree
local _modf = math.modf


local function _treeInit (l, r, v)
  if l == nil and r == nil or l ~= nil and r ~= nil then
    return _tree.new(l, r, v or 0, false)
  else
    return _tree.new(l or r, nil, v or 0, true)
  end
end


local transform = {}


--- Get bounds of the table values.
--  @param t Table {{x1,y11,y12...}, {x2,y21,y22..}, ...}
--  @param tInd Table of y column indeces.
--  @return xrange, yrange
transform.findRange = function (t, tInd)
  local xmax, ymax, xmin, ymin = -math.huge, -math.huge, math.huge, math.huge
  local x = tInd.x or 1
  for i = 1, #t do
    local row = t[i]
    local v = row[x]
    if v > xmax then xmax = v end
    if v < xmin then xmin = v end
    if #tInd > 0 then
      for j = 1, #tInd do
        v = row[tInd[j]]
        if v > ymax then ymax = v end
        if v < ymin then ymin = v end
      end
    else
      for j = 1, #row do
        if j ~= x then
          v = row[j]
          if v > ymax then ymax = v end
          if v < ymin then ymin = v end
        end
      end
    end
  end
  return {xmin, xmax}, {ymin, ymax}
end


--- Get bounds of the vector.
--  @param t Table (vector).
--  @return Minimal and maximal values.
transform.findVectorRange = function (t)
  local vmin, vmax = math.huge, -math.huge
  for i = 1, #t do
    local v = t[i]
    if v > vmax then vmax = v end
    if v < vmin then vmin = v end
  end
  return vmin, vmax
end


--- Prepare string of the given length.
--  @param s Source string.
--  @param N Required length.
--  @param bCentr Flag to put to central position.
--  @param bCut   Flag to cut a long string.
transform.format = function (s, N, bCentr, bCut)
  local res = s
  if #s < N then
    local n = N - #s
    if bCentr then
      local n1 = _modf(n / 2)
      local n2 = n - n1
      res = string.rep(' ', n1) .. s .. string.rep(' ', n2)
    else
      res = s .. string.rep(' ', n)
    end
  end
  if #res > N and bCut then
    res = string.sub(res, 1, N)
  end
  return res
end


transform.statistics = function (t)
  if #t == 0 then return 0, 0 end
  local sum, sq = 0, 0
  for i = 1, #t do
    local v = t[i]
    sum = sum + v
    sq = sq + v*v
  end
  local mean = sum / #t
  return mean, math.sqrt(sq/#t - mean*mean)
end


--- Find range of levels for surface plot.
--  @param v1 Begin of range.
--  @param vn End of range.
--  @param N Number of lines.
--  @param bScale Flag to use bounds in calculation.
--  @param bInt Flag for a number rounding.
--  @return List of levels and step value.
transform.surfRange = function (v1, vn, N, bScale, bInt)
  local res, nn, h = {}, N, 0
  if bScale then
    nn = N - 1
    h = (vn - v1) / nn
    res[1] = v1
  else
    h = (vn - v1) / (N + 1)
  end
  for i = 1, nn do
    if bInt then
      local ind, rst = _modf(1 + h * i)
      res[#res+1] = (rst > 0.5) and ind+1 or ind
    else
      res[#res+1] = v1 + h * i
    end
  end
  return res, h
end


--- New weighted tree.
--  @param l Left element.
--  @param r Right element.
--  @param v Weight.
--  @return new node or leaf.
transform.treeInit = _treeInit


--- Add element to the tree.
--  @param node Node to add new object.
--  @param obj New object.
--  @param w Weight of the object.
transform.treeAdd = function (node, obj, w)
  if node.isleaf then
    node.left = _treeInit(node.left, nil, node.val)
    node.right = _treeInit(obj, nil, w)
    node.isleaf = false
  else
    local vl, vr = node.left.val, node.right.val
    if vl+vr <= w then
      node.left = _treeInit(node.left, node.right, vl+vr)
      node.right = _treeInit(obj, nil, w)
    else
      if vl < vr then
        transform.treeAdd(node.left, obj, w)
      else
        transform.treeAdd(node.right, obj, w)
      end
    end
  end
  node.val = node.left.val + node.right.val
end

return transform
