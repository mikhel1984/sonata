--[[		sonata/core/main.lua 

--- Define aliases for standard operations and add some new common functions.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata</a> collection, 2021.

	module 'main'
--]]

---------------- Tests ---------------------
--[[TEST

lc = require 'core.main'

-- constants starts from _
ans = _pi                     --> math.pi

-- standard functions 
ans = exp(0)+sin(_pi/2)+cosh(0)  --1> 3.0

-- round number
ans = lc.round(0.9)           --> 1.0

-- save 2 digits
ans = lc.round(math.pi, 2)    --> 3.14

-- random between 0 and 1
p = rand()
ans = (p >= 0) and (p <= 1)   --> true

-- random integer (1 to 10)
p = randi(10)
ans = (p >= 1) and (p <= 10)  --> true

-- normal distributed random
print(randn())

-- get object type
-- "knows" types for Sonata objects
ans = lc.type(25)             --> 'integer'

-- modified print function 
a = {a=1,b=2, 3,4,5}
lc.print(a, 0.123)

-- generate 'sequence'
b = lc.range(3)
ans = b[3]                    --> 3

-- even numbers
b = lc.range(2,10,2)
ans = b[2]                    --> 4

-- calculate function values
c = lc.map(sin, b)
ans = c[1]                   --3> 0.909

-- use Lua functions if need
ans = math.floor(_pi)

ans = math.deg(_pi)

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

-- read table from file
f = io.open(nm,'w')
f:write("{1,2.0,a='pqr',b={3,4,c='abc'}}")
f:close()
aa = File.tblImport(nm)
ans = aa.b.c                  --> 'abc'

--]]

--	LOCAL

local TRIG = 'trigonometry'
local HYP = 'hyperbolic'
local FILES = 'files'
local LOGNAME = 'log.note'

local EV_QUIT, EV_ERROR, EV_CMD, EV_RES = 1, 2, 3, 4

-- compatibility
local Ver = {}
if _VERSION < 'Lua 5.3' then
  Ver.loadStr = loadstring
  Ver.atan2 = math.atan2
  Ver.mathType = function (x)
    local n = tonumber(x)
    if not n then return nil end
    local _,p = math.modf(n)
    return (p == 0.0) and 'integer' or 'float'
  end
else
  Ver.loadStr = load
  Ver.atan2 = math.atan
  Ver.mathType = math.type
end

--- Process command string.
--  @param cmd String with Lua expression.
--  @return Status of processing and rest of command.
local function _evaluate_(cmd, nextCmd)
  if nextCmd == 'quit' then return EV_QUIT end
  -- check if multiline
  local partCmd = string.match(nextCmd, "(.*)\\%s*")
  if partCmd ~= nil then
    -- expected next line 
    return EV_CMD, string.format("%s%s\n", cmd, partCmd)
  end
  cmd = cmd..nextCmd
  -- 'parse'
  local fn, err = Ver.loadStr('return '..cmd)  -- either 'return expr'
  if err then
    fn, err = Ver.loadStr(cmd)                 -- or 'expr'
  end
  -- get result
  if err then
    return EV_ERROR, err
  else
    local ok, res = pcall(fn)
    if ok then 
      _ans = res       -- save last result
       return EV_RES, res
    else
      -- evaluation error
      return EV_ERROR, res
    end
  end
end

--	INFO

-- lc_help
lc_help = require "core.help"
about = lc_help:new("Lua based mathematics.")

--	MODULE

local main = {}

-- Commonly used methods
abs = math.abs;   about[abs] =  {"abs(x)", "Absolute value."}
exp = math.exp;   about[exp] =  {"exp(x)", "Exponent."}
log = math.log;   about[log] =  {"log(x)", "Natural logarithm."}
sqrt = math.sqrt;  about[sqrt] = {"sqrt(a)", "Square root."}

-- Trigonometrical
sin = math.sin;   about[sin] =  {"sin(x)", "Sinus x.", TRIG}
cos = math.cos;   about[cos] =  {"cos(x)", "Cosine x.", TRIG}
tan = math.tan;   about[tan] =  {"tan(x)", "Tangent x.", TRIG}
asin = math.asin;  about[asin] = {"asin(x)", "Inverse sine x.", TRIG}
acos = math.acos;  about[acos] = {"acos(x)", "Inverse cosine x.", TRIG}
atan = math.atan;  about[atan] = {"atan(x)", "Inverse tangent x.", TRIG}

atan2 = Ver.atan2 
about[atan2] = {"atan2(y,x)", "Inverse tangent of y/x, use signs.", TRIG}

-- Hyperbolic
cosh = function (x) return 0.5*(math.exp(x)+math.exp(-x)) end
about[cosh] = {"cosh(x)", "Hyperbolic cosine.", HYP}

sinh = function (x) return 0.5*(math.exp(x)-math.exp(-x)) end  
about[sinh] = {"sinh(x)", "Hyperbolic sinus.", HYP}

tanh = function (x) t = math.exp(2*x); return (t-1)/(t+1) end
about[tanh] = {"tanh(x)", "Hyperbolic tangent.", HYP}

-- Hyperbolic inverse 
asinh = function (x) return math.log(x+math.sqrt(x*x+1)) end
about[asinh] = {"asinh(x)", "Hyperbolic inverse sine.", HYP}

acosh = function (x) return math.log(x+math.sqrt(x*x-1)) end
about[acosh] = {"acosh(x)", "Hyperbolic arc cosine.", HYP}

atanh = function (x) return 0.5*math.log((1+x)/(1-x)) end
about[atanh] = {"atanh(x)", "Hyperbolic inverse tangent.", HYP}

-- Constants
_pi = math.pi;   about[_pi] = {"_pi", "Number pi.", lc_help.CONST}
_e  = 2.718281828459;   about[_e]  = {"_e", "Euler number.", lc_help.CONST}
-- result 
_ans = 0;   about[_ans] = {"_ans", "Result of the last operation."}

-- random
rand = function () return math.random() end
about[rand] = {"rand()", "Random number between 0 and 1."}

randi = function (N) return math.random(1,N) end
about[randi] = {"randi(N)", "Random integer in range from 1 to N."}

randn = function () 
  -- use Box-Muller transform
  local u,v,s
  repeat
    u = 2*math.random()-1
    v = 2*math.random()-1
    s = u*u + v*v
  until s <= 1 and s > 0
  return u * math.sqrt(-2*math.log(s)/s)
end
about[randn] = {"randn()", "Normal distributed random value with 0 mean and variance 1."}

--- Round to closest integer.
--  @param x Real number.
--  @param n Number of decimal digits.
--  @return Rounded number.
main.round = function (x,n)
  local k = 10^(n or 0)
  local p,q = math.modf(x*k)
  if q >= 0.5 then 
    p = p+1
  elseif q <= -0.5 then 
    p = p-1
  end
  return p / k
end
about[main.round] = {'lc.round(x[,n=0])', 'Round value, define number of decimal digits.', lc_help.OTHER}

--- Print element, use 'scientific' form for float numbers.
--  @param v Value to print.
main._showElt_ = function (v)
  local tp = Ver.mathType(v)
  if tp == 'float' or tp == 'integer' and math.abs(v) >= 1000 then
    return string.format('%.2E', v)  -- 'scientific' format
  else
    return tostring(v)
  end
end

--- Show elements of the table.
--  @param t Table to print.
main._showTable_ = function (t)
  local N, nums = 10, {}
  -- dialog
  local function continue(n)
    io.write(n, ' continue? (y/n) ')
    return string.lower(io.read()) == 'y'
  end
  io.write('\n{ ')
  -- list elements
  for i,v in ipairs(t) do
    io.write(main._showElt_(v), ', ')
    nums[i] = true
    if i % N == 0 then
      io.write('\n')
      if not continue(i) then break end
    end
  end
  -- hash table elements
  local count = 0
  for k,v in pairs(t) do
    if not nums[k] then
      io.write('\n', tostring(k), ' = ', main._showElt_(v), ', ')
      count = count + 1
      if count % N == 0 and not continue("") then break end
    end
  end
  io.write(' }\n')
end

--- Show table content and scientific form of numbers.
--  @param ... List of arguments.
main.print = function (...)
  for i,v in ipairs({...}) do
    if type(v) == 'table' then
      local mt = getmetatable(v)
      if mt and mt.__tostring then
        -- has representation
        local tmp = tostring(v)
        if string.find(tmp,'\n') then
          io.write('\n', tmp, '\n')
        else
          io.write(tmp, '\t')
        end
      else
        -- require representation
        main._showTable_(v)
      end
    else
      -- show value
      io.write(main._showElt_(v), '\t')
    end
  end
  io.write('\n')
end
about[main.print] = {"lc.print(...)", "Extenden print function, it shows elements of tables and scientific form of numbers.", lc_help.OTHER}

--- Show type of the object.
--  @param t Some Lua or Sonata object.
--  @return String with type value.
function main.type(t)
  local v = type(t)
  if v == 'table' then
    v = t.type or v
  elseif v == 'number' then
    v = Ver.mathType(t) 
  end
  return v
end
about[main.type] = {'lc.type(t)', 'Show type of the object.', lc_help.OTHER}

--- Generate sequence of values.
--  @param from Beginning of range (default is 1).
--  @param to End of range.
--  @param step Step value (default is 1).
--  @return Table with numbers.
main.range = function (from,to,step)
  step = step or 1
  if not to then to = from; from = 1 end
  assert((to-from)*step > 0)
  local res = {}
  for i = from,to,step do res[#res+1] = i end
  return res
end
about[main.range] = {'lc.range([from=1,]to[,step=1])','Generate table with sequence of numbers.', lc_help.OTHER}

--- Generate list of function values.
--  @param fn Function to apply.
--  @param tbl Table with arguments.
--  @return Table with result of evaluation.
main.map = function (fn, tbl)
  local res = {}
  for _,v in ipairs(tbl) do res[#res+1] = fn(v) end
  return res
end
about[main.map] = {'lc.map(fn,tbl)','Evaluate function for each table element.', lc_help.OTHER}

-- "In the game of life the strong survive..." (Scorpions) ;)
--  board - matrix with 'ones' as live cells
main.life = function (board)
  assert(board.type == 'matrix', 'Matrix is expected!')
  local rows,cols = board:size() 
  local src = board
  local gen = 0
  -- make decision about current cell
  local islive = function (r,c)
      local n = src[r-1][c-1] + src[r][c-1] + src[r+1][c-1] + src[r-1][c] + src[r+1][c] + src[r-1][c+1] + src[r][c+1] + src[r+1][c+1]
      return (n==3 or n==2 and src[r][c]==1) and 1 or 0
    end
  -- evaluate
  repeat
    local new = board:zeros()   -- empty matrix of the same size
    gen = gen+1
    -- update 
    for r = 1,rows do
      for c = 1,cols do
        new[r][c] = gen > 1 and islive(r,c) or src[r][c] ~= 0 and 1 or 0
        io.write(new[r][c] == 1 and '*' or ' ')
      end
      io.write('|\n')
    end
    if gen > 1 and new == src then 
      return print('~~ Game Over ~~') 
    end
    src = new
    io.write(string.format('#%-3d continue? (y/n) ', gen))
  until 'n' == io.read()
end

--- Save Lua table in file, use given delimiter.
--  @param tbl Lua table.
--  @param fName File name.
--  @param delim Delimiter, default is coma.
main.dsvWrite = function (fName, tbl, delim)
  local f = assert(io.open(fName,'w'))
  delim = delim or ','
  for _,v in ipairs(tbl) do
    if type(v) == 'table' then v = table.concat(v,delim) end
    f:write(v,'\n')
  end
  f:close()
  io.write('Done\n')
end
about[main.dsvWrite] = {"lc.dsvWrite(fname,tbl[,delim=','])", "Save Lua table as delimiter separated data into file.", FILES}

--- Import data from text file, use given delimiter.
--  @param fName File name.
--  @param delim Delimiter, default is coma.
--  @return Lua table with data.
main.dsvRead = function (fName, delim)
  local f = assert(io.open(fName, 'r'))
  local Test = require('core.test')
  delim = delim or ','
  local res = {}
  for s in f:lines('l') do
    -- read data
    s = string.match(s,'^%s*(.*)%s*$')
    if #s > 0 then
      local tmp = {}
      -- read string elements
      for p in Test.split(s,delim) do
        tmp[#tmp+1] = tonumber(p) or p
      end
      res[#res+1] = tmp
    end
  end
  f:close()
  return res
end
about[main.dsvRead] = {"lc.dsvRead(fName[,delim=','])", "Read delimiter separated data as Lua table.", FILES}

main.tblImport = lc_help.tblImport
about[main.tblImport] = {"lc.tblImport(fName)", "Import Lua table, saved into file.", FILES}

--- Session logging.
--  @param flat Value 'on'/true to start and 'off'/false to stop.
logging = function (flag)
  if flag == 'on' or flag == true then
    if not main._logFile_ then
      main._logFile_ = io.open(LOGNAME, 'a')
      local d = os.date('*t')
      main._logFile_:write(string.format('\n--\tSession\n-- %d-%d-%d %d:%d\n\n', d.day, d.month, d.year, d.hour, d.min))
      main._logFile_:write('-- ')  -- prepare comment for 'logging on'
    end
  elseif flag == 'off' or flag == false then
    if main._logFile_ then
      main._logFile_:close() 
      main._logFile_ = nil
    end
  else
    io.write('Unexpected argument!\n')
  end
end
about[logging] = {'logging(flag)', "Save session into the log file. Use 'on'/true to start and 'off'/false to stop.", lc_help.OTHER}

--- Read-Evaluate-Write circle as a Lua program.
--  Call 'quit' to exit this function.
main.evalDialog = function ()
  local invA, invB = lc_help.CMAIN..'lc: '..lc_help.CRESET, lc_help.CMAIN..'..: '..lc_help.CRESET
  local invite, cmd = invA, ""
  local ERROR = lc_help.CERROR.."ERROR: "
  -- start dialog
  while true do
    io.write(invite)
    -- command processing
    local newLine = io.read()
    local status, res = _evaluate_(cmd, newLine)
    if status == EV_RES then
      if res ~= nil then print(res) end
      invite = invA; cmd = ""
    elseif status == EV_CMD then
      invite = invB; cmd = res
    elseif status == EV_ERROR then
      print(ERROR, res, lc_help.CRESET)
      invite = invA; cmd = ""
    else -- status == EV_QUIT
      break
    end
    -- logging
    if main._logFile_ then
      main._logFile_:write(newLine,'\n')
      if status == EV_RES and res then 
        main._logFile_:write('--[[ ', res, ' ]]\n\n') 
      elseif status == EV_ERROR then
        main._logFile_:write('--[[ ERROR ]]\n\n')
      end
    end
  end
  if main._logFile_ then main._logFile_:close() end
  main._exit_()
end

--- Evaluate 'note'-file.
--  @param fname Script file name.
main.evalDemo = function (fname)
  local ERROR = lc_help.CERROR.."ERROR: "
  local cmd = ""
  local templ = lc_help.CBOLD..'\t%1'..lc_help.CNBOLD
  local invA, invB = '?> ', '>> '
  -- read lines
  io.write("Run file ", fname, "\n")
  -- read
  local f = assert(io.open(fname, 'r'))
  local txt = f:read('*a'); f:close()
  txt = string.gsub(txt, '%-%-%[(=*)%[.-%]%1%]', '')  -- remove long comments
  for line in string.gmatch(txt, '([^\n]+)\r?\n?') do
    if string.find(line, '^%s*%-%-%s*PAUSE') then 
      -- call dialog
      local lcmd, lquit = "", false
      local invite = invA
      while true do
        io.write(invite)
        local newCmd = io.read()
        if newCmd == "" then break end  -- continue file evaluation
        local status, res = _evaluate_(lcmd, newCmd)
        if status == EV_RES then
          if res ~= nil then print(res) end
          invite = invA; lcmd = ""
        elseif status == EV_CMD then
          invite = invB; lcmd = res
        elseif status == EV_ERROR then
          print(ERROR, res, lc_help.CRESET)
          invite = invA; lcmd = ""
        else --  EV_QUIT
          lquit = true
          break
        end
      end
      if lquit then break end
    elseif string.find(line, '^%s*%-%-') then
      -- highlight line comments
      line = string.gsub(line, '\t(.+)', templ)
      line = string.format("%s%s%s\n", lc_help.CHELP, line, lc_help.CRESET)
      io.write(line)
    else
      -- print line and evaluate
      io.write(lc_help.CMAIN, '@ ', lc_help.CRESET, line, '\n')
      local status, res = _evaluate_(cmd, line)
      if status == EV_RES then
        if res ~= nil then print(res) end
        cmd = ""
      elseif status == EV_CMD then
        cmd = res
      else -- EV_ERROR 
        print(ERROR, res, lc_help.CRESET)
        break
      end
    end
  end
end

-- save link to help info
main.about = about

--- Update help reference when redefine function.
--  @param fnNew New function.
--  @param fnOld Old function.
main._updateHelp = function (fnNew, fnOld)
  main.about[fnNew] = main.about[fnOld]
  main.about[fnOld] = nil
end

-- command line arguments of Sonata LC and their processing
main._args_ = {

-- run tests
['--test'] = {
description = 'Apply the unit tests to the desired module. Call all modules if the name is not defined.',
example = '--test array',
process = function (args)
  local Test = require('core.test')
  if args[2] then
    Test.module(string.format('%slib/%s.lua',(LC_ADD_PATH or ''),args[2]))
  else
    for m in pairs(use) do
      Test.module(string.format('%slib/%s.lua',(LC_ADD_PATH or ''),m))
    end
  end
  Test.summary()
end,
exit = true},

-- localization file
['--lang'] = {
description = 'Creating/updating a file for localization.',
example = '--lang eo',
process = function (args)
  if args[2] then
    LC_DIALOG = true -- load help info
    lc_help.prepare(args[2], use)
  else 
    print('Current localization file: ', LC_LOCALIZATION)
  end
end,
exit = true},

-- generate 'help.html'
['--doc'] = {
description = 'Creating/updating a documentation file.',
example = '--doc ru',
process = function (args)
  LC_DIALOG = true   -- load help info
  if args[2] then
    LC_LOCALIZATION = args[2]..'.lng'
  end
  lc_help.generateDoc(LC_LOCALIZATION, use) 
end,
exit = true},

-- new module
['--new'] = {
description = 'Create a template for a new module.',
example = '--new  signal  Sig  "Signal processing functions."',
process = function (args) lc_help.newModule(args[2],args[3],args[4]) end,
exit = true},

-- process files
['default'] = {
--description = 'Evaluate file(s).',
process = function (args) 
  for i = 1,#args do 
    if string.find(args[i], '%.note$') then
      LC_DIALOG = true
      main.evalDemo(args[i])
    else
      dofile(args[i]) 
    end
  end 
end,
exit = true},

}
-- show help
main._args_['--help'] = {
process = function () print(main._arghelp_()) end,
exit = true}
main._args_['-h'] = main._args_['--help']

-- string representation of the help info
main._arghelp_ = function ()
  local txt = {  
    "\n'Sonata LC' is a Lua based program for mathematical calculations.",
    "",
    "USAGE:",
    "\tlua [-i] sonata.lua [flag] [arg1 arg2 ...]",
    "(option '-i' can be used for working in native Lua interpreter)",
    "",
    "FLAGS:",
    "\t--help, -h - Get this help message.",
    "\t\t{Development}",
  }
  for k,v in pairs(main._args_) do 
    if v.description then
      txt[#txt+1] = string.format('\t%-8s - %s', k, v.description)
      if v.example then
        txt[#txt+1] = string.format('\t  (e.g. %s)', v.example)
      end
    end
  end
  txt[#txt+1] = "\t No flag  - Evaluate file(s)."
  txt[#txt+1] = "\nVERSION: "..lc_local.version
  txt[#txt+1] = ""
  local modules = {}
  for k in pairs(use) do modules[#modules+1] = k end
  txt[#txt+1] = string.format("MODULES: %s.\n", table.concat(modules,', '))
  txt[#txt+1] = "BUGS: mail to 'SonataLC@yandex.ru'\n"
  return table.concat(txt,'\n')
end

main._exit_ = function () print(lc_help.CMAIN.."\n             --======= Bye! =======--\n"..lc_help.CRESET); os.exit() end

return main

--===============================