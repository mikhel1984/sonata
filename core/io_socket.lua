
local socket = require("socket")
local ev = require("core.evaluate")

local tcp_server = {}

function quit () end

function _stopServer ()
  io.write('Get request to stop server')
  os.exit()
end

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

tcp_server.new = function (self, host, port)
  local tcp = assert(socket.tcp())
  assert(tcp:bind(host, port))
  self.tcp = tcp
  io.write('Run tcp server on port ', tostring(port), '\n')
  assert(tcp:listen())
  tcp:settimeout(0.05)  -- seconds, accept timeout
end

tcp_server.repl = function (self)
  local clients, tcp = {}, self.tcp
  if not ev.oO then
    ev.oO, print = print, ev._simpPrint
  end
  
  while true do
    -- check new clients
    local cli, err = tcp:accept()
    if err and err ~= 'timeout' then
      ev.oO(err)
    elseif cli then
      -- save
      cli:settimeout(0.2)  -- seconds
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
        ev.oO(err)
        group.cli:close()
        closed[#closed+1] = i
      elseif cmd then
        if cmd ~= ':q' then
          local _, status, res = coroutine.resume(group.co, cmd)
          group.invite = sendAndNext(status, res, group)
          group.cli:send(group.invite)
        else
          group.cli:send('close connection')
          group.cli:close()
          closed[#closed+1] = i
        end
      end
    end
    -- remove
    for i = 1, #closed do
      ev.oO('close', closed[i])
      table.remove(clients, closed[i])
    end
  end
end

return tcp_server
