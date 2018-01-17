--[[       liblc/files.lua

--- Routines for working with data files..
--  @author My Name

           module 'files'
--]]

--------------- Tests ------------
--[[   !!
File = require 'liblc.files'

]]

---------------------------------
-- @class table
-- @name files
-- @field about Description of functions.
local files = {}

-- description
local help = lc_version and (require "liblc.help") or {new=function () return {} end}
files.about = help:new("Routines for working with data files.")

--- Split string into substring based on given delimeter
--    @param str Initial string.
--    @param delim Delimeter string.
--    @return Iterator over substrings.
files.split = function (str, delim)
   local i,j,k = 1,1,0
   -- return iterator
   return function ()
      if not str or i > #str then return nil end
      local res
      j,k = string.find(str, delim, k+1)
      if j then
         res = string.sub(str, i, j-1)
	 i = k+1
      else  -- no more delimeters
         res = string.sub(str, i)
	 i = #str+1
      end
      return res
   end
end
files.about[files.split] = {"split(str,delim)", "Return iterator over substrings separated by the delimeter.", help.OTHER}

--- Save Lua table in file, use given delimeter
--    @param t Lua table.
--    @param fname File name.
--    @param delim Delimeter.
files.dsvwrite = function (t, fname, delim)
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
files.about[files.dsvwrite] = {"dsvwrite(tbl,fname,del)", "Save Lua table as delimeter separated data into file."}

--- Import data from text file, use given delimeter
--    @param fname File name.
--    @param delim Delimeter.
--    @return Lua table with data.
files.dsvread = function (fname, delim)
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
	    if #res[i] == 1 then res[i] = res[i][1] end -- ??
	    i = i+1
	 end
      end
   end
   f:close()
   return res
end
files.about[files.dsvread] = {"dsvread(fname,del)", "Read delimeter separated data as Lua table."}

--- Returns text of the file.
--    @param fname
--    @return String or nil.
files.read = function (fname)
   local str
   local f = io.open(fname, 'r')
   if f then
      str = f:read('*a')
      f:close()
   end
   return str
end

--- Load Lua table from file.
--    @param fname File name.
--    @return Lua table or nil.
files.tblimport = function (fname)
   local str,f = files.read(fname)
   if str then f = load('return '..str) end
   return f and f() or nil
end
files.about[files.tblimport] = {"tblimport(fname)", "Import Lua table, writen into file."}

-- free memory in case of standalone usage
if not lc_version then files.about = nil end

return files

--==================================
-- TODO: improve dsv read/write according the specification
