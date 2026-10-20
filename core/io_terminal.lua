--[[		sonata/core/io_terminal.lua

--- Run Sonata in terminal.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2026.

	module 'io_terminal'
--]]

--	LOCAL

local _ext = {
  base = require("core.io_base"),
  cmds = require("core.commands"),
  -- rl = require('readline'),
}


-- Redefine format lines.
local _txtCodes = {
  [_ext.base.FORMAT_V1] = SonataHelp.CHELP,
  [_ext.base.FORMAT_V2] = SonataHelp.CHELP .. SonataHelp.CBOLD,
  [_ext.base.FORMAT_CLR] = SonataHelp.CRESET,
}


-- Invite lines.
local _invite = {
  SonataHelp.CMAIN.."##"..SonataHelp.CRESET,
  SonataHelp.CMAIN..".."..SonataHelp.CRESET}


--- Allow all the commands.
local commands = _ext.cmds.init()


--- User input reader.
--  @param str Text to show.
--  @param env Thread environment.
--  @return input string.
local function reader (str, env)
  io.write(str, " ")
  return io.read()
end


--- Program output writer.
--  @param res Evaluation result.
--  @param status Status of evaluation.
local function writer (res, status)
  if _ext.base.isformatted(res) then
    for i, v in ipairs(res) do
      res[i] = _txtCodes[v] or v
    end
    res = table.concat(res)
  else
    res = tostring(res)
  end
  -- status dependent view
  if status == _ext.base.EV_ERR then
    res = string.format("%sERROR %s%s", SonataHelp.CERROR, res, SonataHelp.CRESET)
  elseif status == _ext.base.EV_WARN then
    res = "WARNING "..res
  end
  io.write(res, "\n")
end


--- Generator for the file processing function.
--  @param t List of files to read.
--  @return function for state initialization.
local function _makePreprocFiles (t)
  -- preprocessor function
  return function (state, env)
    -- read files
    local cmd = state.commands.o
    local ind = (t[1] == "-i") and 2 or 1
    for i = ind, #t do
      local _, res = cmd(t[i], state, env)
      print(res)
    end
    -- self destroy
    state.preProc.main = nil
  end
end


--- Generate function to close the program after the last file.
--  @param t List of files to read.
--  @return exit condition function.
local function _makePostprocFiles (t)
  if t[1] ~= "-i" then
    -- postprocessor function
    local exit = false
    return function (state, env)
      if exit then
        state.run = false
      elseif state.run then
        exit = state.input == "" and not state.notes:ready()
      end
    end
  end
  -- else nil
end


if SONATA_READLINE
then  --====================================

-- Load library.
_ext.rl = require("readline")

-- Reference to the coroutine environment table.
local local_env = {}


--- Check if the name is module name.
--  @param s Module name or alias.
--  @return true when argument is a name.
local function _is_module (s)
  for k, v in pairs(use) do
    if k == s or v == s then
      return true
    end
  end
end


--- Find list of completions for the given input.
--  @param text Text string.
--  @param from Section begin.
--  @param to Section end.
--  @return list of possible strings.
local function _complete (text, from, to)
  local phrase = string.sub(text, from, to)
  -- check parts
  local match = {}
  local p, q = string.find(phrase, '[.:]')
  if p then
    -- var.method
    local w1 = string.sub(phrase, 1, p-1)
    local ns = local_env[w1]
    if ns then
      local w2 = string.sub(phrase, q+1, #phrase)
      -- check methods
      local t = _is_module(w1) and ns or getmetatable(ns)
      w1 = w1 .. string.sub(phrase, p, q)
      for k in pairs(t or {}) do
        if w2 == string.sub(k, 1, #w2) then
          match[#match+1] = w1 .. k
        end
      end
    end
  else
    -- local variable
    for k in pairs(local_env) do
      if type(k) == 'string' and phrase == string.sub(k, 1, #phrase) then
        match[#match+1] = k
      end
    end
    -- global variable
    for k in pairs(_ENV) do
      if type(k) == 'string' and phrase == string.sub(k, 1, #phrase) then
        match[#match+1] = k
      end
    end
  end
  return match
end

-- Configuration
_ext.rl.set_options {histfile='~/.sonata_history'}
_ext.rl.set_readline_name('sonata')
_ext.rl.set_complete_function(_complete)


-- Updated reader function.
reader = function (str, env)
  local_env = env
  return _ext.rl.readline(str.." ")
end

end  --======= SONATA_READLINE =========


return {
  -- Interaction with user.
  repl = function ()
    _ext.base.mainLoop(reader, writer, commands, _invite)
  end,
  -- File evaluation.
  replFiles = function (t)
    _ext.base.mainLoop(reader, writer, commands, _invite, _makePreprocFiles(t), _makePostprocFiles(t))
  end
}

--===================================

