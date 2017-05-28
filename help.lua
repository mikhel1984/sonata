

local help = {}
help.__index = help

local TITLE, DESCRIPTION, CATEGORY = 1, 2, 3

function help:new()
   local o = {}
   setmetatable(o, self)
   return o
end

local function funclist(tbl)
   local res = {}
   for k, v in pairs(tbl) do
      if not v.link then
         local category = v[CATEGORY] or ""
         res[category] = res[category] or {}
         table.insert(res[category], v[TITLE])
      end
   end
   return res
end

function help:print(fn)
   if fn then
      local v = self[fn]
      if v.link then
         print(v[1])
	 return v.link:print()
      else
         print(string.format("  :%s\n%s", v[TITLE], v[DESCRIPTION]))
      end
   else
      local t = funclist(self)
      for cat, n in pairs(t) do
         print(string.format("  :%s", cat))
	 for i, v in ipairs(n) do
	    io.write(v, (i ~= #n and ', ' or ''))
	 end
	 print()
      end
   end
end

function help:add(tbl)
   for k, v in pairs(tbl) do self[k] = v end
end


test = help:new()

test['a'] = {'title 1', 'description 1', 'cat1'}
test['b'] = {'title 2', 'description 2', 'cat1'}
test['c'] = {'title 3', 'description 3', 'cat2'}
test[test] = {"This is a table for test", link=test}

bb = help:new()
bb['m1'] = {'title 4', 'description 4', 'cat2'}
bb['m2'] = {'tible 5', 'description 5', 'cat3'}
bb[bb] = {"This is second test table", link=bb}

test:add(bb)

test:print(bb)
