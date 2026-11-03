require 'socket'
require 'json'
require 'uri'
require 'fileutils'
require 'thread'

# Independent external HTTP service: its durable ledger is the delivery oracle.
class EffectPeer
  attr_reader :url, :requests, :ledger
  attr_accessor :delivery_mode, :receipt_mode, :receipt_override

  def initialize(directory)
    FileUtils.mkdir_p(directory)
    @ledger = File.join(directory, 'deliveries.jsonl')
    File.write(@ledger, '')
    @requests, @pages, @workers, @clients = [], {}, [], []
    @mutex, @release = Mutex.new, Queue.new
    @delivery_mode, @receipt_mode = :confirm, :confirm
    @server = TCPServer.new('127.0.0.1', 0)
    @url = "http://127.0.0.1:#{@server.addr[1]}"
    @thread = Thread.new do
      loop do
        socket = @server.accept
        @mutex.synchronize { @clients << socket }
        @workers << Thread.new(socket) { |client| serve(client) }
      end
    rescue IOError, Errno::EBADF
    end
  end

  def page(cursor, records:, next_cursor:, complete: true, status: 200)
    @mutex.synchronize do
      @pages[cursor] = [status, {schema: 1, source: 'sales-feed', cursor: cursor, nextCursor: next_cursor, complete: complete, records: records}]
    end
  end

  def deliveries
    @mutex.synchronize { read_deliveries }
  end

  def release
    10.times { @release << true }
  end

  def close
    release
    @server.close unless @server.closed?
    @mutex.synchronize { @clients.each { |socket| socket.close unless socket.closed? } }
    raise 'Effect peer acceptor did not stop' unless @thread.join(5)
    @workers.each { |worker| raise 'Effect peer request did not stop' unless worker.join(5) }
  end

  private

  def read_deliveries
    File.readlines(@ledger).map { |line| JSON.parse(line) }
  end

  def serve(socket)
    request = socket.gets
    return unless request
    method, target, = request.split
    headers = {}
    while (line = socket.gets) && line != "\r\n"
      name, value = line.split(':', 2)
      headers[name.downcase] = value.to_s.strip
    end
    raw = socket.read(Integer(headers.fetch('content-length', '0')))
    uri = URI(target)
    query = URI.decode_www_form(uri.query.to_s).to_h
    @mutex.synchronize { @requests << {method: method, path: uri.path, query: query, body: raw, headers: headers} }
    if method == 'GET' && uri.path == '/feed'
      status, body = @mutex.synchronize { @pages.fetch(query['cursor'], [503, {error: 'Unknown source cursor'}]) }
      respond(socket, status, body)
    elsif method == 'POST' && uri.path == '/deliveries'
      payload = JSON.parse(raw)
      mode = @delivery_mode
      if mode == :redirect
        respond(socket, 307, {error: 'Do not follow'}, 'Location' => @url + '/redirected-delivery')
        return
      end
      @release.pop if mode == :hold_before_commit
      receipt = @mutex.synchronize do
        value = {schema: 1, receiver: 'daily-inbox', operationKey: payload.fetch('operationKey'), scope: payload.fetch('scope'), profileHash: payload.fetch('profileHash'), requestHash: payload.fetch('requestHash'), status: 'confirmed', receiptId: "d#{read_deliveries.length + 1}"}
        File.open(@ledger, 'a') { |file| file.puts(JSON.generate(request: payload, receipt: value)); file.flush; file.fsync }
        value
      end
      @release.pop if mode == :hold_after_commit
      return if mode == :drop_ack
      respond(socket, 200, receipt)
    elsif method == 'GET' && uri.path == '/receipts'
      if @receipt_mode == :unavailable
        respond(socket, 503, {error: 'Receipt service unavailable'})
      elsif @receipt_mode == :not_found
        respond(socket, 404, {error: 'No receipt visible'})
      else
        stored = deliveries.find { |item| item.dig('request', 'operationKey') == query['operationKey'] }
        stored ? respond(socket, 200, @receipt_override || stored.fetch('receipt')) : respond(socket, 404, {error: 'No receipt visible'})
      end
    else
      respond(socket, 404, {error: 'Unknown peer operation'})
    end
  rescue IOError, Errno::ECONNRESET, Errno::EPIPE
    # Stop and actual host death may close a live peer connection.
  ensure
    socket.close unless socket.closed?
  end

  def respond(socket, status, body, headers = {})
    bytes = body.is_a?(String) ? body : JSON.generate(body)
    status_text = {200 => 'OK', 307 => 'Temporary Redirect', 404 => 'Not Found', 503 => 'Service Unavailable'}.fetch(status, 'Response')
    extra = headers.map { |key, value| "#{key}: #{value}\r\n" }.join
    socket.write("HTTP/1.1 #{status} #{status_text}\r\nContent-Type: application/json\r\nContent-Length: #{bytes.bytesize}\r\nConnection: close\r\n#{extra}\r\n#{bytes}")
  end
end
