--[[       liblc/files.lua

--- Routines for working with data files..
--  @author My Name

           module 'files'
--]]

--------------- Tests ------------
--[[   !!
File = require 'liblc.files'

-- example
a = File()
ans = a.type                   --> 'files'
]]

---------------------------------
-- @class table
-- @name files
-- @field about Description of functions.
local files = {}

--[[
files.__index = files
-- mark
files.type = 'files'
files.isfiles = true
local function isfiles(t) return type(t)=='table' and t.isfiles end
]]

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
files.about = help:new("Routines for working with data files.")

files.split = function (str, delim)
   local i,j,k = 1,1,0
   return function ()
      if not str or i > #str then return nil end
      local res
      j,k = string.find(str, delim, k+1)
      if j then
         res = string.sub(str, i, j-1)
	 i = k+1
      else
         res = string.sub(str, i)
	 i = #str+1
      end
      return res
   end
end

-- save Lua table in file, use given delimeter
files.tblwrite = function (t, fname, delim)
   assert(type(t) == 'table', 'Table is expected, got '..type(t))
   local f = assert(io.open(fname,'w'), "Can't create file "..tostring(fname))
   delim = delim or ','
   for _,v in ipairs(t) do
      if type(v) == 'table' then v = table.concat(v,delim) end
      f:write(v,'\n')
   end
   f:close()
   print('Done')
end

-- import data from text file, use given delimeter
files.tblread = function (fname, delim)
   local f = assert(io.open(fname, 'r'), "Can't open file "..fname)
   delim = delim or ','
   local res = {}
   for s in f:lines('l') do
      s = string.match(s,'^%s*(.*)%s*$')
      local i,j = #res+1,1
      if #s > 0 then
         res[i] = {}
	 for p in files.split(s,delim) do
	    res[i][j], j = tonumber(p) or p, j+1
	 end
	 if #res[i] > 0 then
	    if #res[i] == 1 then res[i] = res[i][1] end
	    i = i+1
	 end
      end
   end
   f:close()
   return res
end

-- Returns text of the file
files.read = function (fname)
   local str
   local f = io.open(fname, 'r')
   if f then
      str = f:read('a')
      f:close()
   end
   return str
end

-- Load Lua table from file
files.tblimport = function (fname)
   local str = files.read(fname)
   return str and load('return '..str) or nil
end

--[[
--- Constructor example
--    @param t Some value.
--    @return New object of files.
function files:new(t)
   local o = {}
   -- some logic
   setmetatable(o,self)
   return o
end

-- simplify constructor call
setmetatable(files, {__call = function (self,v) return files:new(v) end})
files.File = 'File'
files.about[files.File] = {"File(t)", "Create new files.", help.NEW}

--- Method example
--   It is good idea to define method for the copy creation.
--   @param t Initial object.
--   @return Copy of the object.
files.copy = function (t)
   -- some logic
   return files:new(argument)
end
files.about[files.copy] = {"copy(t)", "Create a copy of the object.", help.BASE}
]]

-- free memory in case of standalone usage
if not lc_version then files.about = nil end

return files
