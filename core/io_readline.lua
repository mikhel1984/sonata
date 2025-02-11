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

return {
  reader = rl.readline
}
