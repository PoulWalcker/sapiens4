require 'socket'
require 'json'

# A controllable local whisper-server: replies can wait for assertions, fail, and then recover.
# Tracks real audio uploads and request counts without loading a transcription model.
class WhisperStub
  attr_reader :url, :requests

  def initialize(text, status: 200, before_reply: nil, before_body: nil)
    @before_reply = before_reply
    @before_body = before_body
    @text, @status, @audio, @requests = text, status, false, 0
    @server = TCPServer.new('127.0.0.1', 0)
    @url = "http://127.0.0.1:#{@server.addr[1]}"
    @thread = Thread.new do
      loop { serve(@server.accept) }
    rescue IOError, Errno::EBADF
      nil
    end
  end

  def audio_received; @audio; end

  def respond_with(text, status: 200)
    @text, @status = text, status
  end

  def close
    @server.close
    @thread.join(2)
  end

  private

  def serve(client)
    request_line = client.gets.to_s
    headers = {}
    while (line = client.gets) && line != "\r\n"
      name, value = line.split(':', 2)
      headers[name.downcase] = value.to_s.strip
    end
    body = headers['transfer-encoding'].to_s.include?('chunked') ? chunked(client) : client.read(headers['content-length'].to_i)
    @audio ||= request_line.include?('/inference') && body.to_s.b.include?('RIFF')
    @requests += 1
    @before_reply&.call
    reply = JSON.generate(@status == 200 ? {text: @text} : {error: 'stub failure'})
    client.write("HTTP/1.1 #{@status} #{@status == 200 ? 'OK' : 'Error'}\r\nContent-Type: application/json\r\nContent-Length: #{reply.bytesize}\r\nConnection: close\r\n\r\n")
    @before_body&.call
    client.write(reply)
  rescue Errno::EPIPE, Errno::ECONNRESET, Errno::ECONNABORTED
    # The host may shut down while a controlled transcription response is waiting.
    nil
  ensure
    client.close
  end

  def chunked(client)
    body = +''
    loop do
      size = client.gets.to_s.strip.to_i(16)
      break client.gets if size.zero?
      body << client.read(size)
      client.gets
    end
    body
  end
end
