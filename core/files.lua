--[[		sonata/core/files.lua

--- Routines for working with files and text.
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata</a> collection, 2021.

	module 'files'
--]]

--------------- Tests ------------
--[[TEST

-- use 'files'
File = require 'core.files'
-- prepare file name
nm = os.tmpname()

-- save table 
-- separate elements with ';'
t = {{1,2,3},{4,5,6}}
File.dsvWrite(nm, t, ';')

-- read table from file
-- with separator ';'
tt = File.dsvRead(nm, ';')
ans = tt[2][2]                --> 5

-- get file text as string
s = File.read(nm)
ans = string.sub(s,1,5)       --> '1;2;3'

-- read table from file
f = io.open(nm,'w')
f:write("{1,2.0,a='pqr',b={3,4,c='abc'}}")
f:close()
aa = File.tblImport(nm)
ans = aa.b.c                  --> 'abc'

-- string split
str = 'abc//defg//hijkl//mnop'
ans = 0
-- iterate over string parts
for s in File.split(str, '//') do
  ans = ans+1 
end                           --> 4

--]]

--	LOCAL

local READ, WRITE = 'read', 'write'

--	INFO

local help = LC_DIALOG and (require "core.help") or {new=function () return {} end}

local loadStr = (_VERSION < 'Lua 5.3') and loadstring or load

--	MODULE

local files = {
-- description
about = help:new("Routines for working with files and text."),
}

--- Split string into substrings based on the given delimiter.
--  @param str Initial string.
--  @param delim Delimiter string.
--  @return Iterator over substrings.
files.split = function (str, delim)
  local i,j,k = 1,1,0
  -- return iterator
  return function ()
    if not str or i > #str then 
      return nil 
    end
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

--- Save Lua table in file, use given delimiter.
--  @param tbl Lua table.
--  @param fName File name.
--  @param delim Delimiter, default is coma.
files.dsvWrite = function (fName, tbl, delim)
  local f = assert(io.open(fName,'w'))
  delim = delim or ','
  for _,v in ipairs(tbl) do
    if type(v) == 'table' then v = table.concat(v,delim) end
    f:write(v,'\n')
  end
  f:close()
  io.write('Done\n')
end
files.about[files.dsvWrite] = {"dsvWrite(fname,tbl[,delim=','])", "Save Lua table as delimiter separated data into file.", WRITE}

--- Import data from text file, use given delimiter.
--  @param fName File name.
--  @param delim Delimiter, default is coma.
--  @return Lua table with data.
files.dsvRead = function (fName, delim)
  local f = assert(io.open(fName, 'r'))
  delim = delim or ','
  local res = {}
  for s in f:lines('l') do
    -- read data
    s = string.match(s,'^%s*(.*)%s*$')
    if #s > 0 then
      local tmp = {}
      -- read string elements
      for p in files.split(s,delim) do
        tmp[#tmp+1] = tonumber(p) or p
      end
      res[#res+1] = tmp
    end
  end
  f:close()
  return res
end
files.about[files.dsvRead] = {"dsvRead(fName[,delim=','])", "Read delimiter separated data as Lua table.", READ}

--- Returns text of the file.
--  @param fName
--  @return String or nil.
files.read = function (fName)
  local f, str = io.open(fName, 'r')
  if f then
    str = f:read('*a')
    f:close()
  end
  return str
end
files.about[files.read] = {"read(fName)", "Return file content as a text.", READ}

--- Load Lua table from file.
--  @param fName File name.
--  @return Lua table or nil.
files.tblImport = function (fName)
  local str,f = files.read(fName)
  -- use Lua default import
  if str then f = loadStr('return '..str) end
  return f and f() or nil
end
files.about[files.tblImport] = {"tblImport(fName)", "Import Lua table, written into file.", READ}

-- Uncomment to remove descriptions
--files.about = nil

return files

--==================================
--TODO: json read/write (?)
--TODO: table export
--TODO: LC data import/export
