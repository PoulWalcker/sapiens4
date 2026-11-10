require 'socket'
require 'securerandom'
require_relative 'process'

class Relay
  attr_reader :root, :url, :token
  def initialize(beta: false)
    @root = Dir.mktmpdir('sapiens relay ')
    socket = TCPServer.new('127.0.0.1', 0)
    port = socket.addr[1]
    socket.close
    @port, @beta = port, beta
    @url, @token = "http://127.0.0.1:#{port}", SecureRandom.urlsafe_base64(32)
    start
  rescue Exception
    close
    raise
  end
  def start
    @process = Contract::Child.new(Contract::BINARY, 'relay',
      '--port', @port.to_s, '--origin', url, '--db', File.join(root, 'relay.sqlite3'),
      env: {'SAPIENS_RELAY_ADMIN_TOKEN' => token, 'SAPIENS_RELAY_BETA' => @beta ? '1' : nil,
        'RAILWAY_ENVIRONMENT_ID' => nil, 'PYTHONUTF8' => '1'})
    Contract.wait(description: 'relay startup') { @process.stdout.include?('Remote relay ready') }
    Contract.wait do
      begin
        request('GET', '/healthz').first == 200
      rescue Errno::ECONNREFUSED
        false
      end
    end
  end
  def stop
    @process.signal(Gem.win_platform? ? 'KILL' : 'TERM') if @process
    @process.close if @process
    @process = nil
  end
  def request(method, path, body = nil, bearer: token, headers: {})
    body = {} if method == 'POST' && body.nil?
    authorization = bearer ? {'Authorization' => "Bearer #{bearer}"} : {}
    Contract.http(url + path, method, body, authorization.merge(headers))
  end
  def room
    status, body = request('POST', '/api/rooms', {})
    raise "Room creation failed: #{status}: #{body}" unless body['room']
    body
  end
  def close
    stop
    FileUtils.remove_entry(root) if root && File.exist?(root)
  end
end

class RemotePeer
  def initialize
    binary = File.join(Contract::ROOT, '.build/bin', Gem.win_platform? ? 'sapiens-peer.exe' : 'sapiens-peer')
    @input, @output, @error, @process = Open3.popen3([binary, binary])
    @errors = Thread.new { @error.read }
  end
  def call(op, **fields)
    @input.puts(JSON.generate({op: op}.merge(fields)))
    line = Timeout.timeout(12) { @output.gets }
    raise 'Remote peer exited unexpectedly' unless line
    value = JSON.parse(line)
    raise value['error'] if value['error']
    value['value']
  end
  def close
    @input.close
    unless @process.join(5)
      begin
        Process.kill('KILL', @process.pid)
      rescue Errno::ESRCH
        # The peer exited between the timed join and the signal.
      end
      @process.join
    end
    @output.close
    @errors.join
    @error.close
  end
end

# A second implementation of the public wire protocol, independent of the app.
# Browser persistence and reconnection are exercised separately through Blindly4.
class PairedDevice
  def initialize
    @peer = RemotePeer.new
    @keys = @peer.call('keys')
  end

  def pair(invitation)
    @invitation = invitation
    @peer.call('connect', name: 'device', url: invitation['relay'], room: invitation['room'], token: invitation['token'])
    send_request(kind: 'pair', secret: invitation['secret'])
  end

  def send_request(**fields)
    frame = @peer.call('pack', private: @keys['private'], public: @invitation['desktop'], room: @invitation['room'],
      body: {id: SecureRandom.hex(8), kind: 'request', expires: Time.now.to_f + 120}.merge(fields))
    @peer.call('call', name: 'device', body: {type: 'send', frame: frame})
  end

  def receive
    delivery = @peer.call('read', name: 'device', timeout: 7000)
    raise "No encrypted response: #{delivery.inspect}" unless delivery && delivery['messages']
    item = delivery['messages'].first
    @peer.call('call', name: 'device', body: {type: 'ack', message: item['id']})
    @peer.call('unpack', private: @keys['private'], public: @invitation['desktop'], frame: item['frame']).fetch('result')
  end

  def request(path, method: 'GET', body: nil)
    send_request(op: 'api', path: path, method: method, data: body)
    receive
  end

  def close; @peer.close; end
end
