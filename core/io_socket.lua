--[[    sonata/core/io_socket.lua

--- Run Sonata as a TCP server.
--
--  <br>The software is provided 'as is', without warranty of any kind, express or implied.</br>
--  </br></br><b>Authors</b>: Stanislav Mikhel
--  @release This file is a part of <a href="https://github.com/mikhel1984/sonata">sonata.core</a> collection, 2025.

	module 'io_socket'
--]]

local socket = require("socket")
local ev = require("core.evaluate")


--- Redefine command to protect server.
function quit () 
  return 'Enter :q to quit'
end


--- Command for remote stop of the server.
function _stopServer ()
  io.write('Get request to stop server')
  os.exit()
end


--- Send response and get the next invite string.
--  @param status Status of evaluation.
--  @param res Result of evaluation.
--  @param env List of client links.
--  @return Invite string.
local function sendAndNext(status, res, env)
  if status == ev.EV_RES then
    if res ~= nil then
      local out = ev.islist(res) and ev._toText(res) or res
      env.cli:send(out .. '\n')
    end
  elseif status == ev.EV_CMD then
    return ev.INV_CONT
  elseif status == ev.EV_ERR then
    env.cli:send('ERROR ' .. res .. '\n')
  elseif status == ev.EV_ASK then
    if res[2] then
      env.cli:send(res[2])
    end
    return res[1]
  elseif status == ev.EV_WRN then
    env.cli:send('WARNING ' .. res .. '\n')
  elseif status == ev.EV_INF then
    enf.cli:send(res)
  end
  return ev.INV_MAIN
end


-- TCP server object
local tcp_server = {}


--- Make new server.
--  @param host Host address.
--  @param port Port number.
tcp_server.new = function (self, host, port)
  local tcp = assert(socket.tcp())
  assert(tcp:bind(host, port))
  self.tcp = tcp
  io.write('Run tcp server on port ', tostring(port), '\n')
  assert(tcp:listen())
  tcp:settimeout(0.05)  -- seconds, accept timeout
end


--- Server evaluation loop.
tcp_server.repl = function (self)
  local clients, tcp = {}, self.tcp
  -- update print function
  if not ev.oO then
    ev.oO, print = print, ev._simpPrint
  end
  -- main loop 
  while true do
    -- check new clients
    local cli, err = tcp:accept()
    if err and err ~= 'timeout' then
      io.write(err, '\n')
    elseif cli then
      -- save
      cli:settimeout(0.05)  -- seconds
      local group = {
        cli = cli,
        invite = ev.INV_MAIN,
        co = ev.evalThread(),
      }
      clients[#clients+1] = group
      cli:send(group.invite)
    end
    -- process requests
    local closed = {}
    for i = 1, #clients do
      local group = clients[i]
      local cmd, err = group.cli:receive()
      if err and err ~= 'timeout' then
        io.write(err, '\n')
        closed[#closed+1] = i
      elseif cmd then
        if cmd ~= ':q' then
          local _, status, res = coroutine.resume(group.co, cmd)
          group.invite = sendAndNext(status, res, group)
          group.cli:send(group.invite)
        else
          group.cli:send('close connection')
          closed[#closed+1] = i
        end
      end
    end
    -- remove
    for i = 1, #closed do
      local group = table.remove(clients, closed[i])
      local addr, port = group.cli:getsockname()
      io.write('close client ', addr, ' ', port, '\n')
      group.cli:close()
    end
  end
end


return tcp_server

--=================================
