require_relative 'support/relay'
require_relative 'support/voice'
require 'sqlite3'
require 'digest'

RSpec.describe 'Encrypted remote service contracts' do
  include VoiceAudio
  before do
    @relay = Relay.new
    @host = Host.new
    @peer = RemotePeer.new
    @keys = @peer.call('keys')
  end
  after do
    @peer.close if @peer
    @host.close if @host
    @relay.close if @relay
  end

  def invite
    @invitation = host.api('POST', '/api/remote/pair', {relay: @relay.url, token: @relay.token}, status: 201)['qr']
  end
  def pack(body)
    @peer.call('pack', private: @keys['private'], public: @invitation['desktop'], room: @invitation['room'],
      body: {id: SecureRandom.hex(8), kind: 'request', expires: Time.now.to_f + 120}.merge(body))
  end
  def send_frame(frame)
    @peer.call('call', name: 'phone', body: {type: 'send', frame: frame})
  end
  def receive
    delivery = @peer.call('read', name: 'phone', timeout: 7000)
    raise "No encrypted reply: #{delivery.inspect}" unless delivery && delivery['messages']
    item = delivery['messages'].first
    @peer.call('call', name: 'phone', body: {type: 'ack', message: item['id']})
    body = @peer.call('unpack', private: @keys['private'], public: @invitation['desktop'], frame: item['frame'])
    expect(body).to include('room' => @invitation['room'], 'direction' => 'desktop-to-phone', 'v' => 'sapiens-remote-box-v1')
    body['result']
  end
  def pair
    invite
    expect(@peer.call('connect', name: 'phone', url: @relay.url, room: @invitation['room'], token: @invitation['token'])).to include('type' => 'ready')
    send_frame(pack(kind: 'pair', secret: @invitation['secret']))
    Contract.wait { host.api('GET', '/api/remote')['pending'] }
    host.api('POST', '/api/remote/approve', {peer: @keys['public']})
    expect(receive).to include('paired' => true)
  end

  def legacy_receipt_digest(frame)
    saved = JSON.parse(File.read(File.join(host.root, 'remote-device.json')))
    plaintext = @peer.call('unpack', private: saved['private'], public: @keys['public'], frame: frame)
    canonical = lambda do |value|
      case value
      when Hash then '{' + value.keys.sort.map { |key| key.to_json(ascii_only: true) + ': ' + canonical.call(value[key]) }.join(', ') + '}'
      when Array then '[' + value.map { |item| canonical.call(item) }.join(', ') + ']'
      else value.to_json(ascii_only: true)
      end
    end
    Digest::SHA256.hexdigest(canonical.call(plaintext))
  end

  it 'requires the invitation secret and explicit approval of the full phone identity' do
    invite
    @peer.call('connect', name: 'phone', url: @relay.url, room: @invitation['room'], token: @invitation['token'])
    send_frame(pack(kind: 'pair', secret: 'wrong'))
    send_frame(pack(op: 'api', method: 'POST', path: "/api/agents/#{chief}/messages", data: {text: 'Unauthorized'}))
    send_frame(pack(kind: 'pair', secret: @invitation['secret']))
    pending = Contract.wait { host.api('GET', '/api/remote')['pending'] }
    expect(pending['confirmation_code']).to match(/\A\w{4} · \w{4}\z/)
    expect(host.calls).to be_empty
    expect(host.state['turns']).to be_empty
    expect(host.request('POST', '/api/remote/approve', {peer: 'substituted'}).first).to eq(409)
    host.api('POST', '/api/remote/approve', {peer: @keys['public']})
    expect(receive).to include('paired' => true)
    expect(host.api('GET', '/api/remote')['invitation']).to be_nil
  end

  it 'sends complete empty workspace arrays over the encrypted relay for older clients' do
    pair
    send_frame(pack(op: 'api', method: 'GET', path: '/api/state',
      device_info: {name: 'iPhone', platform: 'iOS', client: 'Sapiens4 Home Screen app'}))
    result = receive
    expect(result['status']).to eq(200)
    snapshot = result.fetch('body')
    workspace = snapshot.fetch('preferences').fetch('workspaces').fetch(snapshot.fetch('main_agent_id'))
    expect(workspace.fetch('tabs')).to eq([])
    expect(workspace.fetch('bookmarks').map { |bookmark| bookmark['title'] }).to eq(['Notes.html'])
    expect(host.state['turns']).to be_empty
  end

  it 'keeps older pairings usable without inventing missing device details or dates' do
    pair
    host.stop
    path = File.join(host.root, 'remote-device.json')
    saved = JSON.parse(File.read(path))
    %w[paired_at last_connected_at last_connection_id device_info].each { |key| saved.delete(key) }
    File.write(path, JSON.generate(saved))
    host.start
    expect(host.api('GET', '/api/remote')).to include('paired' => true, 'paired_at' => nil, 'last_connected_at' => nil, 'device_info' => {})
    send_frame(pack(op: 'resume'))
    expect(receive).to include('paired' => true)
    expect(host.api('GET', '/api/remote')['paired_at']).to be_nil
    expect(host.api('GET', '/api/remote')['last_connected_at']).to be_a(Numeric)
  end

  it 'persists pairing and authenticated device connection details without counting relay reconnects' do
    pair
    first = host.api('GET', '/api/remote')
    expect(first['paired_at']).to be_between(Time.now.to_f - 30, Time.now.to_f)
    expect(first['last_connected_at']).to be_nil
    info = {name: 'iPhone', platform: 'iOS 26.0', client: 'Sapiens4 Home Screen app'}
    send_frame(pack(op: 'resume', connection_id: 'phone-session-one', device_info: info))
    receive
    saved = host.api('GET', '/api/remote')
    expect(saved['device_info']).to eq(info.transform_keys(&:to_s))
    expect(saved['last_connected_at']).to be >= saved['paired_at']
    host.restart
    restored = host.api('GET', '/api/remote')
    expect(restored.values_at('paired_at', 'last_connected_at', 'device_info')).to eq(saved.values_at('paired_at', 'last_connected_at', 'device_info'))
    send_frame(pack(op: 'api', method: 'GET', path: '/api/state', connection_id: 'phone-session-one'))
    receive
    expect(host.api('GET', '/api/remote')['last_connected_at']).to eq(saved['last_connected_at'])
    send_frame(pack(op: 'resume', connection_id: 'phone-session-two', device_info: info))
    receive
    reconnected = host.api('GET', '/api/remote')
    expect(reconnected['last_connected_at']).to be > saved['last_connected_at']
    expect(reconnected['paired_at']).to eq(saved['paired_at'])
    host.api('POST', '/api/remote/revoke', {})
    expect(host.api('GET', '/api/remote')).to include('paired' => false, 'paired_at' => nil, 'last_connected_at' => nil, 'device_info' => {})
  end

  it 'executes an encrypted write once across exact retries and restart and returns authoritative state' do
    pair
    agent = chief
    frame = pack(op: 'api', method: 'POST', path: "/api/agents/#{agent}/messages", data: {text: 'Encrypted phone message'})
    expect(JSON.generate(frame)).not_to include('Encrypted phone message', @invitation['secret'])
    send_frame(frame)
    receipt = receive
    expect(receipt['status']).to eq(202)
    host.turn(receipt['body']['id'])
    send_frame(frame)
    expect(receive).to eq(receipt)
    host.restart
    send_frame(frame)
    expect(receive).to eq(receipt)
    expect(host.calls.size).to eq(1)
    send_frame(pack(op: 'api', method: 'GET', path: '/api/state'))
    state = receive['body']
    expect(state['turns'].first).to include('input' => 'Encrypted phone message', 'output' => 'Contract test reply')
    expect(state['corpora']).to eq(host.state['corpora'])
  end

  it 'uploads a large attachment through the encrypted channel without stalling the host' do
    pair
    bytes = 'x' * 1_048_576
    send_frame(pack(op: 'api', method: 'POST', path: "/api/agents/#{chief}/attachments",
      data: {kind: 'document', name: 'large.txt', data: [bytes].pack('m0')}))
    upload = receive
    expect(upload['status']).to eq(201)
    expect(upload['body']).to include('name' => 'large.txt', 'size' => bytes.bytesize)
    send_frame(pack(op: 'api', method: 'GET', path: '/api/state'))
    expect(receive['body']).to have_key('turns')
  end

  it 'rejects a large changed command reusing a request ID without stalling the host' do
    pair
    upload = lambda do |content|
      pack(id: 'large-reused', op: 'api', method: 'POST', path: "/api/agents/#{chief}/attachments",
        data: {kind: 'document', name: 'large.txt', data: [content].pack('m0')})
    end
    send_frame(upload.call('a' * 1_048_576))
    expect(receive['status']).to eq(201)
    send_frame(upload.call('b' * 1_048_576))
    send_frame(pack(op: 'api', method: 'GET', path: '/api/state'))
    expect(receive['body']).to have_key('turns')
    stored = Dir.glob(File.join(host.root, 'uploads', '**', 'large.txt'))
    expect(stored.map { |path| File.read(path, 1) }).to eq(['a'])
  end

  %w[canonical python].each do |format|
    it "rejects oversized mismatched retries of a pre-v2 #{format} receipt without stalling" do
      pair
      upload = lambda do |content|
        pack(id: 'pre-v2-large', op: 'api', method: 'POST', path: "/api/agents/#{chief}/attachments",
          data: {kind: 'document', name: 'legacy-large.txt', data: [content].pack('m0')})
      end
      original = upload.call('a' * 1_048_576)
      changed = upload.call('b' * 1_048_576)
      send_frame(original)
      receipt = receive
      expect(receipt['status']).to eq(201)
      host.stop
      database = File.join(host.root, 'remote-receipts.sqlite3')
      python_digest = legacy_receipt_digest(original) if format == 'python'
      before = SQLite3::Database.open(database) do |db|
        digest = python_digest || db.get_first_value('SELECT hash FROM receipts WHERE id=?', ['pre-v2-large']).delete_prefix('v2:')
        db.execute('UPDATE receipts SET hash=? WHERE id=?', [digest, 'pre-v2-large'])
        db.get_first_row('SELECT hash,reply FROM receipts WHERE id=?', ['pre-v2-large'])
      end
      host.start
      send_frame(changed)
      send_frame(pack(op: 'api', method: 'GET', path: '/api/state'))
      expect(receive['body']).to have_key('turns')
      send_frame(original)
      if format == 'canonical'
        expect(receive).to eq(receipt)
      else
        # An oversized Python-format retry also stays bounded, even if unchanged.
        send_frame(pack(op: 'api', method: 'GET', path: '/api/state'))
        expect(receive['body']).to have_key('turns')
      end
      stored = Dir.glob(File.join(host.root, 'uploads', '**', 'legacy-large.txt'))
      expect(stored.size).to eq(1)
      expect(File.binread(stored.first)).to eq('a' * 1_048_576)
      SQLite3::Database.open(database) do |db|
        expect(db.get_first_row('SELECT hash,reply FROM receipts WHERE id=?', ['pre-v2-large'])).to eq(before)
      end
    end
  end

  it 'streams voice pieces through the encrypted channel into one-minute voice messages' do
    pair
    recording = SecureRandom.uuid
    piece = ->(index) { {recording: recording, index: index, final: index == 30, data: voice_piece(2)} }
    replies = (0..30).map do |index|
      send_frame(pack(op: 'api', method: 'POST', path: "/api/agents/#{chief}/voice", data: piece.(index)))
      receive
    end
    expect(replies.map { |reply| reply['status'] }).to all(eq(200))
    messages = replies.flat_map { |reply| reply['body']['messages'] }
    expect(messages.map { |m| [m['kind'], m['part'], m['duration_ms']] }).to eq([['voice', 0, 60000], ['voice', 1, 2000]])
    # The phone resends the last piece after a reconnect, with a new request id.
    send_frame(pack(op: 'api', method: 'POST', path: "/api/agents/#{chief}/voice", data: piece.(30)))
    expect(receive['body']).to include('duplicate' => true, 'messages' => replies.last['body']['messages'])
  end

  it 'keeps voice audio and its request unreadable on the relay' do
    pair
    audio = voice_piece(2)
    send_frame(pack(op: 'api', method: 'POST', path: "/api/agents/#{chief}/voice", data: {recording: SecureRandom.uuid, index: 0, final: false, data: audio}))
    expect(receive['status']).to eq(200)
    stored = Dir[File.join(@relay.root, 'relay.sqlite3*')].map { |file| File.binread(file) }.join
    aggregate_failures do
      expect(stored).not_to include(audio[0, 64])
      expect(stored).not_to include([0, 1000, 0, -1000].pack('s<*') * 8)
      expect(stored).not_to include("/api/agents/#{chief}/voice")
    end
  end

  it 'uploads an attachment through the encrypted channel and sends it with a message' do
    pair
    data = ['Total: 42'].pack('m0')
    frame = pack(op: 'api', method: 'POST', path: "/api/agents/#{chief}/attachments", data: {kind: 'document', name: 'notes.txt', data: data})
    expect(JSON.generate(frame)).not_to include('notes.txt', data)
    send_frame(frame)
    upload = receive
    expect(upload['status']).to eq(201)
    doc = upload['body']
    expect(doc).to include('kind' => 'document', 'name' => 'notes.txt', 'size' => 9)
    expect(File.read(doc['value'])).to eq('Total: 42')
    send_frame(pack(op: 'api', method: 'POST', path: "/api/agents/#{chief}/messages", data: {text: 'Summarize', attachments: [doc['id']]}))
    reply = receive
    expect(reply['status']).to eq(202)
    turn = host.turn(reply['body']['id'])
    expect(turn['attachments'].map { |item| item['id'] }).to eq([doc['id']])
    context = JSON.parse(host.calls.last['prompt'].split("\nContext:\n", 2).fetch(1))
    request = context.fetch('currentCall').fetch('request')
    marker = 'Attached references (JSON data; read the files/links when needed for this message):'
    references = JSON.parse(request.split(marker, 2).fetch(1))
    expect(references).to include('kind' => 'document', 'name' => 'notes.txt', 'value' => doc.fetch('value'))
  end

  it 'restores an unapproved QR, hides discovery on approval, and reuses saved relay setup after revocation' do
    invite
    address = @invitation['public_id']
    expect(address).to match(/\Asapi4_[a-z]{3}[0-9]{3}[a-z]{3}\z/)
    host.restart
    expect(host.api('GET', '/api/remote')['invitation']).to eq(@invitation)
    @peer.call('connect', name: 'phone', url: @relay.url, room: @invitation['room'], token: @invitation['token'])
    send_frame(pack(kind: 'pair', secret: @invitation['secret']))
    Contract.wait { host.api('GET', '/api/remote')['pending'] }
    host.api('POST', '/api/remote/approve', {peer: @keys['public']})
    receive
    expect(@relay.request('GET', '/api/corpora/' + address).first).to eq(404)
    host.api('POST', '/api/remote/revoke', {})
    replacement = host.api('POST', '/api/remote/pair', {saved: true}, status: 201)['qr']
    expect(replacement['public_id']).to eq(address)
    expect(replacement['desktop']).not_to eq(@invitation['desktop'])
    expect(@relay.request('GET', '/api/corpora/' + address)[1]['desktop']).to eq(replacement['desktop'])
    expect(JSON.generate(host.api('GET', '/api/remote'))).not_to include(@relay.token)
  end

  [:expired, :unreachable_relay].each do |failure|
    it "refuses pending phone approval with #{failure} and grants no authority" do
      invite
      @peer.call('connect', name: 'phone', url: @relay.url, room: @invitation['room'], token: @invitation['token'])
      send_frame(pack(kind: 'pair', secret: @invitation['secret']))
      Contract.wait { host.api('GET', '/api/remote')['pending'] }
      if failure == :expired
        host.stop
        path = File.join(host.root, 'remote-device.json')
        saved = JSON.parse(File.read(path)).merge('expires' => Time.now.to_f - 1)
        Contract.atomic_json(path, saved)
        host.start
        expect(host.api('GET', '/api/remote')['invitation']).to be_nil
      else
        @relay.close
        @relay = nil
      end
      expect(host.request('POST', '/api/remote/approve', {peer: @keys['public']}).first).to eq(failure == :expired ? 409 : 500)
      expect(host.api('GET', '/api/remote')['paired']).to eq(false)
      expect(host.calls).to be_empty
      expect(host.state['turns']).to be_empty
    end
  end

  it 'never re-executes a write whose durable receipt says its previous outcome is uncertain' do
    pair
    frame = pack(op: 'api', method: 'POST', path: "/api/agents/#{chief}/messages", data: {text: 'One external effect'})
    send_frame(frame)
    receipt = receive
    host.turn(receipt['body']['id'])
    host.stop
    SQLite3::Database.open(File.join(host.root, 'remote-receipts.sqlite3')) do |db|
      db.execute('UPDATE receipts SET reply=NULL')
    end
    host.start
    send_frame(frame)
    expect(receive['error']).to include('uncertain')
    expect(host.calls.size).to eq(1)
    expect(host.state['turns'].size).to eq(1)
  end

  it 'reuses a Python-format receipt hash with nested Unicode data after migration' do
    pair
    frame = pack(id: 'legacy-receipt', expires: Time.now.to_i + 120, op: 'api', method: 'POST',
      path: "/api/agents/#{chief}/messages", data: {text: 'Résumé 中文 😎'},
      metadata: {z: [1.25, -0.0, 1, 1.0, 12345678901234567890, nil, false], a: {second: true, first: "é\n\"quoted\"\\"}})
    send_frame(frame)
    receipt = receive
    host.turn(receipt['body']['id'])
    host.stop
    digest = legacy_receipt_digest(frame)
    SQLite3::Database.open(File.join(host.root, 'remote-receipts.sqlite3')) do |db|
      db.execute('UPDATE receipts SET hash=? WHERE id=?', [digest, 'legacy-receipt'])
    end
    host.start
    send_frame(frame)
    expect(receive).to eq(receipt)
    expect(host.calls.size).to eq(1)
    expect(host.state['turns'].size).to eq(1)
  end

  it 'rejects a changed command reusing a prior request ID and transfers a large authoritative snapshot' do
    pair
    first = pack(id: 'stable-request', op: 'api', method: 'POST', path: "/api/agents/#{chief}/messages", data: {text: 'Original'})
    send_frame(first)
    host.turn(receive['body']['id'])
    send_frame(pack(id: 'stable-request', op: 'api', method: 'POST', path: "/api/agents/#{chief}/messages", data: {text: 'Substituted'}))
    drafts = (1..30).to_h { |n| ["draft-#{n}", 'x' * 16000] }
    host.api('PUT', '/api/preferences', {drafts: drafts})
    send_frame(pack(op: 'api', method: 'GET', path: '/api/state'))
    state = receive['body']
    expect(state['preferences']['drafts']).to eq(drafts)
    expect(state['turns'].map { |turn| turn['input'] }).to eq(['Original'])
    expect(host.calls.size).to eq(1)
  end

  it 'resumes a trusted phone after invitation expiry but refuses pairing copied from another workspace' do
    pair
    identity = host.state['corpora']
    host.stop
    path = File.join(host.root, 'remote-device.json')
    saved = JSON.parse(File.read(path))
    saved['expires'] = Time.now.to_f - 8 * 86400
    File.write(path, JSON.generate(saved))
    host.start
    send_frame(pack(op: 'resume'))
    expect(receive).to include('paired' => true, 'corpora' => identity)
    other = Host.new(start: false)
    FileUtils.cp(path, File.join(other.root, 'remote-device.json'))
    other.start
    expect(other.api('GET', '/api/remote')).to include('enabled' => false, 'error' => a_string_including('invalid'))
    expect(other.state['corpora']['id']).not_to eq(identity['id'])
  ensure
    other.close if other
  end

  it 'refuses to replace a corrupt workspace identity silently' do
    host.stop
    File.write(File.join(host.root, 'corpora-identity.json'), '{"version":1,"corpora_id":"broken"}')
    _, error, code = Contract.capture(Contract::BINARY, '--port', '0', '--data-dir', host.root, env: host.environment)
    expect(code).not_to eq(0)
    expect(error).to include('Restore corpora-identity.json')
  end

  it 'bounds the relay mailbox and lets only the desktop establish trust' do
    room = @relay.room
    path = "/api/rooms/#{room['room']}"
    frame = {sender: ['p' * 32].pack('m0'), nonce: ['n' * 24].pack('m0'), ciphertext: ['e' * 64].pack('m0')}
    100.times { expect(@relay.request('POST', path + '/messages', frame, bearer: room['phone']).first).to eq(200) }
    expect(@relay.request('POST', path + '/messages', frame, bearer: room['phone']).first).to eq(429)
    expect(@relay.request('POST', path + '/trust', {}, bearer: room['phone']).first).to eq(404)
    expect(@relay.request('POST', path + '/trust', {}, bearer: room['desktop']).first).to eq(200)
  end

  it 'rejects wrong workspace, forbidden routes and unknown operations inside the encrypted boundary' do
    pair
    [
      {op: 'api', method: 'POST', path: "/api/agents/#{chief}/messages", data: {text: 'Wrong workspace'}, corpora_id: 'another-corpora'},
      {op: 'shell', text: 'Never execute'},
      {op: 'api', method: 'GET', path: '/api/remote'},
      {op: 'api', method: 'POST', path: '/api/remote/revoke', data: {}},
      {op: 'api', method: 'GET', path: 'https://evil.example/api/state'},
      {op: 'api', method: 'POST', path: "/api/agents/#{chief}/browser", data: {}},
      {op: 'api', method: 'GET', path: '/api/agents/../remote'}
    ].each do |request|
      send_frame(pack(request))
      expect(receive).to have_key('error')
    end
    expect(host.calls).to be_empty
    expect(host.state['turns']).to be_empty
  end

  it 'discards expired, tampered and wrong-context frames before accepting a valid request' do
    pair
    body = {op: 'api', method: 'POST', path: "/api/agents/#{chief}/messages", data: {text: 'Rejected write'}}
    send_frame(pack(body.merge(expires: Time.now.to_f - 1)))
    send_frame(pack(body.merge(direction: 'desktop-to-phone')))
    send_frame(pack(body.merge(room: 'other-room')))
    send_frame(pack(body).merge('ciphertext' => ['x' * 64].pack('m0')))
    send_frame(pack(op: 'api', method: 'GET', path: '/api/state'))
    expect(receive['body']['turns']).to be_empty
    expect(host.calls).to be_empty
  end

  it 'publishes only discovery metadata, keeps credentials out of status, and revokes locally when offline' do
    invite
    code, found = @relay.request('GET', '/api/corpora/' + @invitation['public_id'])
    expect(code).to eq(200)
    expect(found.keys).to contain_exactly('public_id', 'desktop', 'expires')
    expect(found['expires'] - Time.now.to_f).to be_between(259190, 259201)
    expect(JSON.generate(host.api('GET', '/api/remote'))).not_to include(@relay.token, @keys['private'])
    identity = host.state['corpora']
    @relay.close
    @relay = nil
    result = host.api('POST', '/api/remote/revoke', {})
    expect(result).to include('revoked' => true, 'relay_deleted' => false)
    host.restart
    expect(host.state['corpora']).to eq(identity)
    expect(host.api('GET', '/api/remote')['enabled']).to eq(false)
  end

  ['{broken', 'null', '[]', '1', '"invalid"'].each do |raw|
    it "keeps local service usable with corrupt saved pairing #{raw}" do
      host.stop
      File.write(File.join(host.root, 'remote-device.json'), raw)
      host.start
      expect(host.api('GET', '/api/remote')).to include('enabled' => false, 'error' => a_string_including('invalid'))
      expect(host.api('GET', '/api/health')['status']).to eq('ok')
    end
  end

  it 'persists unacknowledged ciphertext, replays it on reconnect, and acknowledges only the recipient direction' do
    room = @relay.room
    args = {url: @relay.url, room: room['room']}
    @peer.call('connect', **args, name: 'desktop', token: room['desktop'])
    @peer.call('connect', **args, name: 'phone', token: room['phone'])
    frame = {sender: ['p' * 32].pack('m0'), nonce: ['n' * 24].pack('m0'), ciphertext: ['e' * 350000].pack('m0')}
    expect(send_frame(frame)).to have_key('result')
    item = @peer.call('read', name: 'desktop', timeout: 5000)['messages'].first
    expect(item['frame']).to eq(JSON.parse(JSON.generate(frame)))
    @peer.call('close', name: 'desktop')
    @peer.call('connect', **args, name: 'desktop2', token: room['desktop'])
    expect(@peer.call('read', name: 'desktop2', timeout: 5000)['messages'].first).to eq(item)
    @relay.request('DELETE', "/api/rooms/#{room['room']}/messages/#{item['id']}", nil, bearer: room['phone'])
    expect(@relay.request('GET', "/api/rooms/#{room['room']}/messages", nil, bearer: room['desktop'])[1]['messages'].size).to eq(1)
    @peer.call('call', name: 'desktop2', body: {type: 'ack', message: item['id']})
    expect(@relay.request('GET', "/api/rooms/#{room['room']}/messages", nil, bearer: room['desktop'])[1]['messages']).to be_empty
  end

  it 'rejects unauthorized relay access and plaintext and disconnects old tabs and revoked peers' do
    room = @relay.room
    args = {url: @relay.url, room: room['room'], token: room['phone']}
    expect(@relay.request('POST', '/api/rooms', {}, bearer: 'wrong').first).to eq(401)
    expect(@relay.request('DELETE', "/api/rooms/#{room['room']}", nil, bearer: room['phone']).first).to eq(404)
    expect(@peer.call('connect', **args, name: 'bad', token: 'b' * 43)).to have_key('closed')
    @peer.call('connect', **args, name: 'phone')
    expect(send_frame(text: 'plaintext')).to have_key('error')
    @peer.call('connect', **args, name: 'new-phone')
    Contract.wait { @peer.call('status', name: 'phone')['closed'] }
    expect(@peer.call('status', name: 'phone')['closed']).to eq(4002), @peer.call('status', name: 'phone').inspect
    @relay.request('DELETE', "/api/rooms/#{room['room']}", nil, bearer: room['desktop'])
    Contract.wait { @peer.call('status', name: 'new-phone')['closed'] }
    expect(@peer.call('status', name: 'new-phone')['closed']).to eq(4001)
  end

  it 'serves public installation assets without credentials and refuses private host routes or foreign origins' do
    expect(@relay.request('GET', '/healthz', nil, headers: {'Host' => 'healthcheck.railway.app'}).first).to eq(200)
    expect(@relay.request('GET', '/', nil, headers: {'Host' => 'evil.example'}).first).to eq(403)
    expect(@relay.request('GET', '/', nil, headers: {'Origin' => 'https://evil.example'}).first).to eq(403)
    %w[/api/state /api/remote /relay.connection.json /corpora/host/Service.hs /remote/relay/Server.hs /corpora/prompts/conversation.md /build/BuildUI.hs /build/Build.hs /submodules/libs4/package.json].each do |path|
      expect(@relay.request('GET', path).first).to eq(404)
    end
    manifest = @relay.request('GET', '/manifest.webmanifest')[1]
    expect(manifest).to include('display' => 'standalone', 'start_url' => '/', 'id' => '/')
    [180, 192, 512].each do |size|
      status, png = @relay.request('GET', "/app-icon-#{size}.png")
      expect(status).to eq(200)
      expect(png.byteslice(16, 8).unpack('NN')).to eq([size, size])
    end
  end

  %w[ios android mac windows].each do |platform|
    it "serves the #{platform} app with its own launch URL and the shared app identity" do
      manifest = @relay.request('GET', '/manifest.webmanifest')[1]
      path = "/manifest.webmanifest?platform=#{platform}"
      status, platform_manifest = @relay.request('GET', path)
      expect(status).to eq(200)
      expect(platform_manifest).to eq(manifest.merge('start_url' => "/#{platform}"))
      ["/#{platform}", "/#{platform}/"].each do |entry|
        status, html = @relay.request('GET', entry)
        expect(status).to eq(200)
        expect(html).to include(%(href="#{path}"))
      end
    end
  end
end
