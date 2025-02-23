--[[		sonata/core/io_readline.lua

--- Load and configure 'readline' library.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2025.

	module 'io_readline'
]]

-- Get external library
local rl = require('readline')

-- Configuration
rl.set_options {histfile='~/.sonata_history'}
rl.set_readline_name('sonata')

local function is_module (s)  
  for k, v in pairs(use) do
    if k == s or v == s then
      return true
    end
  end
end

local function complete (text, from, to)
  local phrase = string.sub(text, from, to)
  -- check parts
  local match = {}
  local p, q = string.find(phrase, '[.:]')
  if p then
    -- var.method
    local w1 = string.sub(phrase, 1, p-1) 
    local ns = _ENV[w1]
    if ns then
      local w2 = string.sub(phrase, q+1, #phrase)
      -- check methods
      local t = is_module(w1) and ns or getmetatable(ns)
      w1 = w1 .. string.sub(phrase, p, q)
      for k in pairs(t or {}) do
        if w2 == string.sub(k, 1, #w2) then
          match[#match+1] = w1 .. k
        end
      end
    end
  else
    -- variable
    for k in pairs(_ENV) do
      if type(k) == 'string' and phrase == string.sub(k, 1, #phrase) then
        match[#match+1] = k
      end
    end
  end
  return match
end

rl.set_complete_function(complete)

return {
  reader = rl.readline
}
