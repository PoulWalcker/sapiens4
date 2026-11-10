require_relative 'http_peer'

# A real HTTP boundary around the real host or relay. A scenario can delay/fail a response
# without importing UI code, replacing fetch, or exposing a test endpoint.
class HostProxy
  attr_reader :url
  def initialize(host, updates: false, &filter)
    upgrade = lambda do |req, res|
      if updates.respond_to?(:call) && !updates.call
        res.status = 404
      else
        forward_socket(host, req, res)
      end
    end if updates
    @server = HTTPPeer.new(upgrade: upgrade) do |request|
      authorization = {'Authorization' => request['authorization']}.compact
      status, body, headers = Contract.http(host.url + request.unparsed_uri, request.request_method, request.body, authorization)
      response = [status, body, headers.transform_values { |values| values.join(', ') }.reject { |key, _| %w[content-length transfer-encoding connection].include?(key) }]
      filter.call(request, response) || response
    end
    @url = @server.url
  end
  def close; @server.close; end

  private

  def forward_socket(host, request, response)
    raise 'Unexpected WebSocket origin' unless request['origin'] == @url
    upstream = URI(host.url)
    socket = TCPSocket.new(upstream.host, upstream.port)
    socket.write("GET #{request.unparsed_uri} HTTP/1.1\r\nHost: #{upstream.authority}\r\nOrigin: #{host.url}\r\n" \
      "Connection: Upgrade\r\nUpgrade: websocket\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: #{request['sec-websocket-key']}\r\n\r\n")
    raise 'Upstream upgrade failed' unless socket.gets.to_s.start_with?('HTTP/1.1 101')
    while (line = socket.gets) && line != "\r\n"
      key, value = line.split(':', 2)
      response[key] = value.strip if key.downcase == 'sec-websocket-accept'
    end
    response.status = 101
    response.upgrade!('websocket')
    response.body = lambda do |downstream|
      peers = [socket, downstream]
      loop do
        IO.select(peers).first.each { |src| (src == socket ? downstream : socket).write(src.readpartial(16384)) }
      end
    rescue EOFError, IOError, SystemCallError
    ensure
      socket.close
    end
  rescue StandardError
    socket&.close
    raise
  end
end
