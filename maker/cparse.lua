
-- References
local _find, _match = string.find, string.match
local _sub, _gsub = string.sub, string.gsub

-- Constants
local TYPE_SYMBOLS, SPACE = '([%*%[%]])', '%s+'
local NAME, END, SIZE, TYPE, START = '#name', '#end', '#size', '#type', '#start'

-- 'Basic' table to simplify lexer definition.
-- Provide access to * and variable name.
local star_name = {['*']=true, [NAME]=true}
star_name.__index = star_name

-- Transition graph and properties.
-- Defines available continuations.
local _lex = {
  [START] = {int=true, unsigned=true, long=true, short=true, float=true, 
             void=true, [TYPE]=true, char=true, signed=true, double=true, 
             struct=true, const=true},
  int = setmetatable({_int=true}, star_name),
  char = setmetatable({_char=true}, star_name),
  ['*'] = setmetatable({_ptr=true}, star_name),
  unsigned = {long=true, short=true, int=true, char=true, _int=true},
  long = setmetatable({long=true, int=true, double=true}, star_name),
  short = setmetatable({int=true, _int=true}, star_name),
  float = setmetatable({_float=true}, star_name),
  ['['] = {[']']=true, [SIZE]=true, _arr=true},
  [']'] = {[END]=true, ['[']=true},
  void = setmetatable({_void=true}, star_name),
  struct = {[TYPE]=true, _table=true},
  [NAME] = {[END]=true, ['[']=true},
  [SIZE] = {[']']=true},
  [TYPE] = {['*']=true, [NAME]=true},
}
_lex['signed'] = _lex['unsigned']
_lex['double'] = _lex['float']
_lex['const'] = _lex[START]

-- Structure data marker
local mt_struct = {}

-- Split string using delimiter.
local function _split (str, delim)
  local i, j, k = 1, 1, 0
  local acc = {}
  while true do
    if not str or i > #str then
      return acc
    end
    j, k = _find(str, delim, k+1)
    local res
    if j then
      res = _sub(str, i, j-1)
      i = k+1
    else  -- no more delimiters
      res = _sub(str, i)
      i = #str+1
    end
    if j ~= 1 then  -- skip first empty string 
      acc[#acc+1] = res
    end
  end
end

-- Wrap symbol in whitespace.
local function _addSpace (s)
  return string.format(' %s ', s)
end

-- Remove file extention
local function _removeExt (s)
  return _match(s, '(.+)%.h[p]*')
end

-- Remove multiline comments.
-- Keep text in {}, past text from [[]].
local function _removeMultiline (s)
  local has_compiler = false
  local res = _gsub(s, '(/%*.-%*/)', function (m) 
    if _find(m, '/%*{.-}%*/') then   -- attributes
      if _find(m, 'compiler') then has_compiler = true end
      return m
    else  -- check raw string
      return _match(m, '/%*%[%[(.-)%]%]%*/') or ''
    end
  end)
  return res, has_compiler
end

-- Remove line comments.
local function _removeLine (s)
  return _gsub(s, '(//.-)\n', '')
end

-- Get tokens of a function definition.
local function _parseFunction (s, pos)
  local a, b = assert(_find(s, '%b()', pos))
  -- find and process function name
  local name = _sub(s, pos, a-1)
  name = _gsub(name, TYPE_SYMBOLS, _addSpace)
  name = _split(name, SPACE)
  -- find and process function arguments
  local args = _sub(s, a+1, b-1)
  args = _gsub(args, TYPE_SYMBOLS, _addSpace)
  args = _split(args, ',')
  for i = 1, #args do
    args[i] = _split(args[i], SPACE)
  end
  return name, args, b
end

-- Get tokens of a structure definition.
local function _parseStruct (str)
  local full, body, short = _match(str, 'struct(.-){(.-)}(.-);')
  -- name(s)
  full = _match(full, '([%w_]+)')
  short = _match(short, '([%w_]+)')
  -- fields
  body = _gsub(body, TYPE_SYMBOLS, _addSpace)
  body = _split(body, ';')
  for i = 1, #body do
    body[i] = _split(body[i], SPACE)
  end
  -- last element usualy is empty
  if #body[#body] == 0 then table.remove(body) end
  return full, short, body
end

-- Get tokens of typedef definition.
local function _parseTypedef (str)
  str = _gsub(str, TYPE_SYMBOLS, _addSpace)
  local tokens = _split(str, SPACE)
  local name = table.remove(tokens)  -- last value
  table.remove(tokens, 1)  -- 'typedef'
  return name, tokens
end


-- Parser class
local parser = {}

-- Save variable parameters
parser.var = function (var, tplua, tpc, nptr, arr, qual)
  return {
    var_name = var,
    C = tpc,
    Lua = tplua,
    ptr_count = nptr or 0,
    arr_size = arr or {}, 
    cnst = qual, }
end

-- Analyze sequence of tokens, get table of parameters.
local function _resolveLine (ln, types)
  local i, name_pos, num_ptr = 1, #ln, 0
  local char, int, float, void, cnst
  local var_name, arr_size, struct_name = '', {}, nil
  -- analyze tokens
  local expect = _lex[START]
  while i <= #ln do
    local token = ln[i]
    if expect[token] then
      -- keywords
      expect = _lex[token]
      if token == 'const' then cnst = true end
    else
      -- specific token
      if expect[TYPE] then
        local nm = (ln[i-1] == 'struct') and 'struct '..token or token
        local tp = assert(types[nm], 'Unknown type: '..nm)
        if getmetatable(tp) == mt_struct then
          struct_name = nm
          expect = _lex[TYPE]
        else
          -- insert values
          local acc = {}
          for j = 1, i-1 do acc[#acc+1] = ln[j] end
          for j = 1, #tp do acc[#acc+1] = tp[j] end
          for j = i+1, #ln do acc[#acc+1] = ln[j] end
          ln = acc  -- process updated table
          i = i-1   -- repeat for the same position
          expect = _lex[START]  -- TODO define correct initial types
        end
      elseif expect[NAME] then
        var_name = token
        name_pos = i
        expect = _lex[NAME]
      elseif expect[SIZE] then
        arr_size[#arr_size] = assert(tonumber(token), 'Unable to get array size')
        expect = _lex[SIZE]
      elseif expect[END] then
        assert(i == #ln, "Expected end of expression")
      else
        local tmp = {}
        for j = i, #ln do tmp[#tmp+1] = ln[j] end
        error('Unexpected tokens: '..table.concat(tmp, ' '))
      end
    end
    -- check properties
    if expect._char  then char = true end
    if expect._int   then int = true end
    if expect._float then float = true end
    if expect._arr   then arr_size[#arr_size+1] = 0 end
    if expect._void  then void = true end  -- ?
    if expect._ptr   then num_ptr = num_ptr + 1 end
    i = i + 1
  end
  -- prepare result
  local t = parser.var(var_name, nil, nil, num_ptr, arr_size, cnst)
  -- make C type
  local ctype = {}
  for i = 1, name_pos-1 do 
    if ln[i] == '*' then break end
    ctype[i] = ln[i]
  end
  t.C = table.concat(ctype, ' ')
  -- make Lua type
  if int then 
    t.Lua = 'int'
  elseif float then 
    t.Lua = 'float'
  elseif struct_name then 
    t.Lua = 'table'
  elseif char then 
    t.Lua = cnst and num_ptr == 1 and 'string' or 'int'
  end
  -- print "----"
  -- print(t.var_name)
  -- print(t.ptr_count, t.struct_name)
  -- print(table.concat(t.arr_size, ':'))
  -- print('C', t.C)
  -- print('Lua', t.Lua)
  return t
end

-- Analyze tokens in structure, get tables with properties.
local function _resolveStruct (struct, types)
  local t = {}
  for i = 1, #struct do
    t[i] = _resolveLine(struct[i], types)
  end
  return t
end

-- Make search map for the given list.
local function _listToMap (src)
  local t = {}
  for i = 1, #src do t[src[i]] = true end
  return t
end

-- Read file, remove comments.
parser.prepare = function (fname)
  local f = assert(io.open(fname, 'r'), "Unable to open "..fname)
  local src = f:read('a')
  local s, has_compiler = _removeMultiline(src)
  assert(has_compiler, "Compiler options not found")
  return _removeLine(s)
end

-- Find struct and typedef elements.
parser.getTypes = function (s)
  local types = {}
  local a, b = 1, 0
  -- structure definition
  while true do
    a, b = _find(s,
      'struct[%w%s_]+%b{}[%w%s_]*;', 
      b+1)
    if not a then break end
    local full, short, body = _parseStruct(_sub(s, a, b))
    setmetatable(body, mt_struct)  -- set marker
    if full then types['struct '..full] = body end
    if short then types[short] = body end
  end
  -- type definitions
  a, b = 1, 0
  while true do
    a, b = _find(s, 'typedef[^;]+;', b+1)
    if not a then break end
    local name, tokens = _parseTypedef(_sub(s, a, b-1))
    -- save, ignore known structures
    if not types[name] then types[name] = tokens end
  end
  return types
end

-- Find properties for all the structures.
parser.resolveStruct = function (types)
  local list = {}
  for tp, val in pairs(types) do
    if getmetatable(val) == mt_struct then
      list[tp] = _resolveStruct(val, types)
    end
  end
  return list
end

-- Find properties for all the functions.
parser.resolveFunctions = function (lst, types)
  for i, grp in ipairs(lst) do
    -- name
    grp[1] = _resolveLine(grp[1], types)
    -- args
    local args = grp[2]
    for j = 1, #args do
      args[j] = _resolveLine(args[j], types)
    end
  end
end

-- Get list of functions for Lua library.
parser.getFunctions = function (s)
  local lst, a, b = {}, 1, 0
  local lib_attr = nil
  while true do
    a, b = _find(s, '/%*.-%*/', b+1)
    if not a then break end
    -- read table
    local txt = assert(_match(s, '({.-})', a))
    local t = assert(load('return '..txt), "Wrong syntax")()
    if t.compiler then 
      lib_attr = t
    else
      local name, args
      if t['function'] then
        name, args = _parseFunction(t['function'], 1)
      else
        name, args, b = _parseFunction(s, b+1)
      end
      t.out = t.out and _listToMap(_split(t.out, SPACE)) or {}
      t.inout = t.inout and _listToMap(_split(t.inout, SPACE)) or {}
      t.ptr = t.ptr and _listToMap(_split(t.ptr), SPACE) or {}
      lst[#lst+1] = {name, args, t}
    end
  end
  return lst, assert(lib_attr, "Compiler not found")
end

-- Analyze file, get functions for library.
parser.parse = function (fname)
  local str = parser.prepare(fname)
  local types = parser.getTypes(str)
  local fn_list, attr = parser.getFunctions(str)
  attr.fname = _removeExt(fname)  -- save source file name
  attr.fname_h = fname
  local struct_list = parser.resolveStruct(types)
  parser.resolveFunctions(fn_list, types)
  return attr, fn_list, struct_list
end

-- Parser introspection.
parser.view = function (fname)
  local attr, fn_list, structs = parser.parse(fname)
  print "== Library attributes =="
  for k, v in pairs(attr) do print(k, "=", v) end
  
  print ""
  print "== Structures =="
  for k, v in pairs(structs) do
    print(k)
    for _, f in ipairs(v) do
      print(string.format(
        "-%s\tC='%s%s' Lua='%s' %s", 
        f.var_name, f.C, 
        f.ptr_count > 0 and string.rep("*", f.ptr_count) or "", 
        f.Lua, 
        #f.arr_size > 0 and "["..table.concat(f.arr_size, ":").."]" or ""))
    end
  end
  
  print ""
  print "== Functions =="
  for _, fn in ipairs(fn_list) do 
    local fname, fargs, fattr = fn[1], fn[2], fn[3]
    print(fname.var_name)
    for _, v in ipairs(fargs) do 
      local t = {'in'}
      if fattr.out[v.var_name] then t[1] = 'out' elseif fattr.inout[v.var_name] then t[1] = 'inout' end
      print(string.format(
        "- %s\t[%s] C='%s%s' Lua='%s' %s", 
        v.var_name, 
        table.concat(t, ','),
        v.C, 
        v.ptr_count > 0 and string.rep("*", v.ptr_count) or "", 
        v.Lua, 
        #v.arr_size > 0 and "["..table.concat(v.arr_size, ":").."]" or ""))
    end
    if fname.C ~= "void" or fname.ptr_count > 0 then
      print "\treturn"
      print(string.format(
        "- \t[out] C='%s%s' Lua='%s' %s", 
        fname.C, 
        fname.ptr_count > 0 and string.rep("*", fname.ptr_count) or "", 
        fname.Lua, 
        #fname.arr_size > 0 and "["..table.concat(fname.arr_size, ":").."]" or ""))
    end
  end
end

return parser

