--[[       liblc/files.lua

--- Routines for working with data files.
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/lc">liblc</a> collection, 2017-2018.

           module 'files'
--]]

--------------- Tests ------------
--[[!!
File = require 'liblc.files'
nm = os.tmpname()

-- save table 
-- separate elements with ';'
t = {{1,2,3},{4,5,6}}
File.dsvWrite(nm, t, ';')

-- read table from file
-- with separator ';'
tt = File.dsvRead(nm, ';')
ans = tt[2][2]                       --> 5

-- get file text as string
s = File.read(nm)
ans = string.sub(s,1,5)              --> '1;2;3'

-- serialize table to file
a = {1, 2.0, a = 'pqr', b = {3,4,c='abc'}}
File.tblExport(nm, a)

-- deserialize table from file
aa = File.tblImport(nm)
ans = aa.b.c                         --> 'abc'

-- string split
str = 'abc//defg//hijkl//mnop'
ans = 0
for s in File.split(str, '//') do ans = ans+1 end  --> 4

]]

--	LOCAL

-- compatibility
local Ver = require "liblc.versions"

-- serialization rules
local val2str = {
   ['string'] =   function (x) return string.format('"%s"', x) end,
   ['number'] =   function (x) return Ver.mathType(x) == 'float' and string.format('%a',x) or string.format('%d',x) end,
   ['function'] = function (x) x = tostring(x); print('Bad value: '..x); return x end,
   ['boolean'] =  function (x) return tostring(x) end,
   ['nil'] =      function (x) return 'nil' end,
}
val2str['table'] = function (x) return val2str.tbl(x) end
val2str['thread'] = val2str['function']

-- Table to string conversation.
val2str.tbl =  function (t)
   local res = {}
   for _,val in ipairs(t) do res[#res+1] = val2str[type(val)](val) end
   for k,val in pairs(t) do
      if not Ver.isInteger(k) then res[#res+1] = string.format("['%s']=%s", tostring(k), val2str[type(val)](val)) end
   end
   return string.format('{%s}', table.concat(res,','))
end

--	INFO
local help = lc_version and (require "liblc.help") or {new=function () return {} end}

--	MODULE

local files = {
-- description
about = help:new("Routines for working with data files."),
}

--- Split string into substrings based on the given delimiter
--    @param str Initial string.
--    @param delim Delimiter string.
--    @return Iterator over substrings.
files.split = function (str, delim)
   local i,j,k = 1,1,0
   -- return iterator
   return function ()
      if not str or i > #str then return nil end
      j,k = string.find(str, delim, k+1)
      local res
      if j then
         res = string.sub(str, i, j-1)
	 i = k+1
      else  -- no more delimiters
         res = string.sub(str, i)
	 i = #str+1
      end
      return res
   end
end
files.about[files.split] = {"split(str,delim)", "Return iterator over substrings separated by the delimiter.", help.OTHER}

--- Save Lua table in file, use given delimiter
--    @param t Lua table.
--    @param fname File name.
--    @param delim Delimiter, default is coma.
files.dsvWrite = function (fname, t, delim)
   local f = assert(io.open(fname,'w'), "Can't create file "..tostring(fname))
   delim = delim or ','
   for _,v in ipairs(t) do
      if type(v) == 'table' then v = table.concat(v,delim) end
      f:write(v,'\n')
   end
   f:close()
   print('Done')
end
files.about[files.dsvWrite] = {"dsvWrite(fname,tbl,del)", "Save Lua table as delimiter separated data into file."}

--- Import data from text file, use given delimiter
--    @param fname File name.
--    @param delim Delimiter, default is coma.
--    @return Lua table with data.
files.dsvRead = function (fname, delim)
   local f = assert(io.open(fname, 'r'), "Can't open file "..fname)
   delim = delim or ','
   local res = {}
   for s in f:lines('l') do
      -- read data
      s = string.match(s,'^%s*(.*)%s*$')
      local i,j = #res+1,1
      if #s > 0 then
         res[i] = {}
	 -- read string elements
	 for p in files.split(s,delim) do
	    res[i][j], j = (tonumber(p) or p), j+1
	 end
      end
   end
   f:close()
   return res
end
files.about[files.dsvRead] = {"dsvRead(fname,del)", "Read delimiter separated data as Lua table."}

--- Returns text of the file.
--    @param fname
--    @return String or nil.
files.read = function (fname)
   local f, str = io.open(fname, 'r')
   if f then
      str = f:read('*a')
      f:close()
   end
   return str
end

--- Load Lua table from file.
--    @param fname File name.
--    @return Lua table or nil.
files.tblImport = function (fname)
   local str,f = files.read(fname)
   -- use Lua default import
   if str then f = Ver.loadStr('return '..str) end
   return f and f() or nil
end
files.about[files.tblImport] = {"tblImport(fname)", "Import Lua table, written into file."}

--- Save Lua table to the file.
--    @param fname File name.
--    @param t Lua table.
files.tblExport = function (fname, t)
   assert(fname and t, 'Wrong arguments!')
   local str = val2str.tbl(t)
   local f = assert(io.open(fname,'w'), "Can't create file "..tostring(fname))
   f:write(str)
   f:close()
   print('Done')
end
files.about[files.tblExport] = {"tblExport(fname,tbl)", "Save Lua table into file."}

-- free memory in case of standalone usage
if not lc_version then files.about = nil end

return files

--==================================
-- TODO: improve dsv read/write according the specification
-- TODO: fix export negative and fractional keys
-- TODO: json read/write
