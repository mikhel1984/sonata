--[[    sonata/core/io_tcp.lua

--- Run Sonata as a TCP server.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2026.

	module 'io_tcp'
--]]

--	LOCAL

local _ext = {
  socket = require("socket"),
  -- sonata
  base = require("core.io_base"),
  cmds = require("core.commands"),
}

-- Define format lines.
local _txtCodes = {
  [_ext.base.FORMAT_V1] = SonataHelp.CHELP,
  [_ext.base.FORMAT_V2] = SonataHelp.CHELP .. SonataHelp.CBOLD,
  [_ext.base.FORMAT_CLR] = SonataHelp.CRESET,
}


-- Invite lines.
local _invite = {
  SonataHelp.CMAIN.."##"..SonataHelp.CRESET,
  SonataHelp.CMAIN..".."..SonataHelp.CRESET}


-- Redefine function to protect server.
function quit () return "Enter :q to quit" end

-- Redefine function to avoid printing into the server terminal.
print = _ext.base.doPrint


--- Command for remote stop of the server.
function _stopServer ()
  io.write("Receive request to stop server")
  os.exit()
end


-- Exclude some commands.
local commands = _ext.cmds.init(
  -- note files
  "o", "rm", "ls",
  -- others
  "log", "w", "sh")


--  MODULE

-- TCP server object
local io_tcp = {}


--- Generator for input reader.
--  @param client Tcp clinet object.
--  @return reader function.
local function makeReader (client)
  -- Send string and wait for user input.
  return function (str, env)
    client:send(str.." ")
    return coroutine.yield()
  end
end


--- Generator for output sender.
--  @param client Tcp client object.
--  @return writer function.
local function makeWriter (client)
  -- Send result of execution.
  return function (res, status)
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
    client:send(res.."\n")
  end
end


--- Make new server.
--  @param host Host address.
--  @param port Port number.
io_tcp.new = function (self, host, port)
  local tcp = assert(_ext.socket.tcp())
  assert(tcp:bind(host, port))
  self.tcp = tcp
  io.write("Run tcp server on port ", tostring(port), "\n")
  assert(tcp:listen())
  tcp:settimeout(0.01)  -- seconds, accept timeout
end


--- Server evaluation loop.
io_tcp.repl = function (self)
  local clients, tcp = {}, self.tcp
  local closed = {}
  -- main loop
  while true do
    -- check new clients
    local cli, err = tcp:accept()
    if err and err ~= "timeout" then
      io.write(err, '\n')
    elseif cli then
      -- save
      cli:settimeout(0.0)  -- non-blocking
      local group = {
        cli = cli,
        co = coroutine.create(_ext.base.mainLoop),
      }
      clients[#clients+1] = group
      cli:send(Sonata.getTitle())
      coroutine.resume(group.co, makeReader(cli), makeWriter(cli), commands, _invite)
    end
    -- process requests
    for i = 1, #clients do
      local group = clients[i]
      if coroutine.status(group.co) ~= "dead" then
        local cmd, e = group.cli:receive('*l')
        if e and e ~= "timeout" then
          io.write(e, '\n')
          closed[#closed+1] = i
        elseif cmd then
          coroutine.resume(group.co, cmd)
        end
      else
        group.cli:send("Close connection")
        closed[#closed+1] = i
      end
    end
    -- remove
    while #closed > 0 do
      local ids = table.remove(closed)
      local group = table.remove(clients, ids)
      io.write("Close client from ", group.cli:getsockname(), "\n")
      group.cli:close()
    end
  end
end


return io_tcp

--=================================

