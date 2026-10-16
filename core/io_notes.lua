--[[		sonata/core/io_notes.lua

--- Functions for note file processing.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2017-2026.

	module 'io_note'
--]]

--	LOCAL

-- Core logic.
local _extBase = require("core.io_base")


--- Read file, split into text blocks.
--  Separator is the "pause" word.
--  @param fname File name.
--  @return table with the text blocks.
local function _blocks (fname)
  local f = io.open(fname, 'r')
  if not f then return nil end
  local txt = f:read('*a'); f:close()
  -- remove long comments
  txt = string.gsub(txt, '%-%-%[(=*)%[.-%]%1%]', '')
  local init, res = 1, {}
  while true do
    local i1, i2 = string.find(txt, '%-%-%s-[Pp][Aa][Uu][Ss][Ee].-\n?', init)
    if i1 then
      res[#res+1] = string.sub(txt, init, i1 - 1)
      init = i2 + 1
    else
      break
    end
  end
  local last = string.sub(txt, init, #txt)
  if string.find(last, "%w?") then
    res[#res+1] = last
  end
  return res
end


-- Invite text symbols.
local _invite = {
  SonataHelp.CMAIN.."(##) "..SonataHelp.CRESET,
  SonataHelp.CMAIN.."(..) "..SonataHelp.CRESET}


-- Highlight template.
local NOTE_TEMPL = SonataHelp.CBOLD..'\t%1'..SonataHelp.CNBOLD


--  MODULE

local io_notes = {}
io_notes.__index = io_notes


--- Remove all notes from memory.
io_notes.clear = function (self)
  self.blocks = {}
  self.queue = {}
end


--- Wrap index for the given block size.
--  Index could be negative out of the block list size.
io_notes.clip = function (self, ind)
  if ind < 0 then ind = #self.blocks + ind + 1 end
  if ind < 1 then
    return 1
  elseif ind > #self.blocks then
    return #self.blocks
  end
  return ind
end


--- Add sequence to queue.
--  @param new Flag to prepare new queue.
--  @param a Start index.
--  @param b End index.
io_notes.enqueue = function (self, new, a, b)
  if new then self.queue = {} end
  for i = a, b do table.insert(self.queue, i) end
end


--- Evaluate the given block of notes.
--  @param block Text block.
--  @param state Executor state.
--  @param env Thread environment.
--  @return table with inputs and outputs.
io_notes.evalBlock = function (self, block, state, env)
  local read, ok, status, res
  local acc = {}

  for line in string.gmatch(block, "([^\n]+)\r?\n?") do
    -- read
    local input = nil
    if string.find(line, '^%s*%-%-') then
      -- highlight line comments
      line = string.gsub(line, '\t(.+)', NOTE_TEMPL)
      acc[#acc+1] = SonataHelp.CHELP..line..SonataHelp.CRESET
    elseif not read or coroutine.status(read) == "dead" then
      -- finish current input
      acc[#acc+1] = _invite[1]..line
      read = coroutine.create(_extBase.doRead)
      ok, status, res = coroutine.resume(read, line, env)
      input = line
    else
      -- continue input
      acc[#acc+1] = _invite[2]..line
      ok, status, res = coroutine.resume(read, line)
      input = status
    end
    -- evaluate
    if status ~= _extBase.EV_CMD and input then
      ok, res = _extBase.doCommand(input, self.commands, state, env)
      if ok then
        if string.find(input, ";%s-$") then res = nil end
        if res ~= nil then
          env.ANS = res
          acc[#acc+1] = _extBase.simpleString(res)
        end
      else
        acc[#acc+1] = SonataHelp.CERROR..res..SonataHelp.CRESET
      end
    end
  end
  return acc
end


--- Get next block of notes.
--  @return block text and its index.
io_notes.getNext = function (self)
  local index = table.remove(self.queue, 1)
  return self.blocks[index], index
end


--- Get list of block heads.
--  @return list of first lines for each block.
io_notes.heads = function (self)
  local acc = {}
  for i = 1, #self.blocks do
    local s = ''
    for line in string.gmatch(self.blocks[i], '[^\n\r]+') do
      if string.find(line, "[^%s]+") then s = line; break end
    end
    acc[#acc+1] = string.format("%d)   %s", i, s)
  end
  return acc
end


--- Create new notes object.
--  @param commands List of available commands for execution.
--  @return notes object.
io_notes.init = function (commands)
  local o = {
    blocks = {},  -- list of blocks
    queue = {},   -- sequence of indices
    commands = commands,
  }
  return setmetatable(o, io_notes)
end


--- Read file, update state.
--  @param fname File name.
--  @return true on success.
io_notes.readFile = function (self, fname)
  local bs = _blocks(fname)
  if not bs then return false end
  local n = #self.blocks
  for i = 1, #bs do
    table.insert(self.blocks, bs[i])
    table.insert(self.queue, n + i)
  end
  return true
end


--- Get size of block of notes.
--  @return number of note blocks.
io_notes.size = function (self) return #self.blocks end


--- Check if note sequence is not empty.
--  @return true when have block to evaluate.
io_notes.ready = function (self)
  return #self.queue > 0
end


--- Show block of text.
--  @param ind Block index.
--  @return block content.
io_notes.text = function (self, ind)
  ind = ind or self.queue[1]
  if ind then
    local t = self.blocks[ind]
    if t then
      return string.format("\t[ %d / %d ]\n%s", ind, #self.blocks, t)
    end
  end
end

return io_notes

--=========================================================
-- TODO read lua files
