--[[		sonatalib/main.lua 

--- Define aliases for standard operations and add some new common functions.
--
--  @author <a href="mailto:sonatalc@yandex.ru">Stanislav Mikhel</a>
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonatalib</a> collection, 2021.

	module 'main'
--]]

---------------- Tests ---------------------
--[[TEST

lc = require 'sonatalib.main'

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

-- show table components
a = {a=1,b=2;3,4,5}
lc.show(a)

-- show "scientific" view
lc.sci(1234.56789)

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

--]]

--	LOCAL

local TRIG = 'trigonometry'
local HYP = 'hyperbolic'
local LOGNAME = 'sonata.log'

local EV_QUIT, EV_ERROR, EV_CMD, EV_RES = 1, 2, 3, 4

-- compatibility
local Ver = require "sonatalib.versions"

--- Process command string.
--  @param cmd String with Lua expression.
--  @return Status of processing and rest of command.
local function _evaluate_(cmd)
  if cmd == 'quit' then return EV_QUIT end 
  -- remove line comments
  local tmp = string.match(cmd, '^(.*)%-%-.*$')
  if tmp then cmd = tmp end
  -- parse 
  local fn, err = Ver.loadStr('return '..cmd)
  local expected_next = string.find(err or '', 'expected near')
  if err then
    fn, err = Ver.loadStr(cmd)
  end
  if err then
    if string.find(err, 'error') or not expected_next then
      -- parsing error
      return EV_ERROR, err
    else
      -- expected rest of command
      return EV_CMD, cmd..' '
    end
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
lc_help = require "sonatalib.help"
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

--- Print the contents of a Lua table.
--  @param t Lua table (not necessarily).
--  @param N Number of fields in a listing, default is 10.
main.show = function (t,N)
  if type(t) ~= 'table' then print(t) end
  -- show table
  N = N or 10
  local count = 1
  -- dialog
  local function continue(n)
    io.write(n, ' - continue? (y/n) ')
    return string.lower(io.read()) == 'y'
  end
  io.write('{\n')
  -- keys/values
  for k,v in pairs(t) do
    if Ver.mathType(k) ~= 'integer' or k < 1 then
      if count % N == 0 and not continue(count) then break end
      print(tostring(k)..' = '..tostring(v))
      count = count + 1
    end
  end
  -- numbers
  for i,v in ipairs(t) do
    io.write(tostring(v),', ')
    if i % N == 0 then
      io.write('\n')
      if not continue(i) then break end
    end
  end
  print(#t > 0 and '\n}' or '}')
end
about[main.show] = {"lc.show(t[,N=10])", "Print Lua object. In case of table, ask about continuation after each N elements.", lc_help.OTHER}

--- Print 'scientific' representation of the number
--  @param x Number to show.
main.sci = function (x) print(string.format('%.2E',x)) end
about[main.sci] = {"lc.sci(x)", "'Scientific' representation of the number.", lc_help.OTHER}

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

--- Simple implementation of 'The Life' game ;)
--  Prepare your initial board in form of matrix.
--  @param board Matrix with 'ones' as live cells.
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

--- Session logging.
--  @param flat Value 'on'/true to start and 'off'/false to stop.
logging = function (flag)
  if flag == 'on' or flag == true then
    if not main._logFile_ then
      main._logFile_ = io.open(LOGNAME, 'a')
      local d = os.date('*t')
      main._logFile_:write(string.format('\n-- Session %d-%d-%d %d:%d \n\n', d.day, d.month, d.year, d.hour, d.min))
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
  local ERROR, log = lc_help.CERROR.."ERROR: "
  -- start dialog
  while true do
    io.write(invite)
    -- command processing
    local newLine = io.read()
    local status, res = _evaluate_(string.format("%s%s",cmd,newLine))
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
  local f = assert(io.open(fname))
  local text = f:read('*a'); f:close()
  local ERROR = lc_help.CERROR.."ERROR: "
  local cmd = ""
  local templ = lc_help.CBOLD..'\t%1'..lc_help.CNBOLD
  -- read lines
  io.write("Run file ", fname)
  for line in string.gmatch(text, '([^\n]+)\r?\n?') do
    if string.find(line, '^%s*%-%-%s*PAUSE') then 
      -- run dialog
      local lcmd, lquit = "", false
      local invA, invB = '?> ', '>> '
      local invite = invA
      while true do
        io.write(invite)
        lcmd = lcmd .. io.read()
        if lcmd == "" then break end  -- continue file evaluation
        local status, res = _evaluate_(lcmd)
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
      --io.write(lc_help.CHELP, line, lc_help.CRESET, '\n')
      line = string.gsub(line, '\t(.+)', templ)
      line = string.format("%s%s%s\n", lc_help.CHELP, line, lc_help.CRESET)
      io.write(line)
    else
      -- print line and evaluate
      io.write(lc_help.CMAIN, '@ ', lc_help.CRESET, line, '\n')
      local status, res = _evaluate_(string.format('%s %s', cmd, line))
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
description = 'Apply unit tests to desired module, or all modules if the name is not defined.',
process = function (args)
  local Test = require 'sonatalib.test'
  if args[2] then
    Test.module(string.format('%ssonatalib/%s.lua',(LC_ADD_PATH or ''),args[2]))
  else
    for m in pairs(import) do
      Test.module(string.format('%ssonatalib/%s.lua',(LC_ADD_PATH or ''),m))
    end
  end
  Test.summary()
end,
exit = true},

-- localization file
['--lang'] = {
description = 'Create/update file for localization.',
process = function (args)
  if args[2] then
    LC_DIALOG = true -- load help info
    lc_help.prepare(args[2], import)
  else 
    print('Current localization file: ', LC_LOCALIZATION)
  end
end,
exit = true},

-- generate 'help.html'
['--doc'] = {
description = 'Create/update documentation file.',
process = function ()
  LC_DIALOG = true   -- load help info
  lc_help.generateDoc(LC_LOCALIZATION, import) 
end,
exit = true},

-- new module
['--new'] = {
description = 'Generate template for a new module.',
process = function (args) lc_help.newModule(args[2],args[3],args[4]) end,
exit = true},

-- process files
['default'] = {
--description = 'Evaluate file(s).',
process = function (args) 
  for i = 1,#args do 
    if string.find(args[i], '%.note?$') then
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
    "(option '-i' could be used for working in native Lua interpreter)",
    "",
    "FLAGS:",
    "\t--help, -h - Get this help message.",
    "\t\t(Development)",
  }
  for k,v in pairs(main._args_) do 
    if v.description then
      txt[#txt+1] = string.format('\t%-8s - %s', k, v.description)
    end
  end
  txt[#txt+1] = "\t No flag  - Evaluate file(s)."
  txt[#txt+1] = "\nVERSION: "..lc_local.version
  txt[#txt+1] = ""
  local modules = {}
  for k in pairs(import) do modules[#modules+1] = k end
  txt[#txt+1] = string.format("MODULES: %s.\n", table.concat(modules,', '))
  txt[#txt+1] = "BUGS: mail to 'SonataLC@yandex.ru'\n"
  return table.concat(txt,'\n')
end

main._exit_ = function () print(lc_help.CMAIN.."\n          --======= Bye! =======--\n"..lc_help.CRESET); os.exit() end

return main

--===============================
--TODO: save last command as well
