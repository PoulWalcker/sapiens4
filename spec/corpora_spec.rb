require_relative 'support/relay'

RSpec.describe 'Corpora lifecycle and device registry', :host do
  def transition(action, metadata = host.api('GET', '/api/corpora'), operation: SecureRandom.uuid)
    host.api('PUT', '/api/corpora/lifecycle', action: action, revision: metadata['revision'], operation: operation)
  end

  it 'persists pause, keeps management reachable and rejects new work until activation' do
    identity = host.api('GET', '/api/corpora')
    expect(identity).to include('status' => 'active', 'chief_id' => chief)
    paused = transition('pause', identity)
    expect(paused).to include('status' => 'paused', 'stopping' => false)
    expect(host.request('POST', "/api/agents/#{chief}/messages", {text: 'Should not start'}).first).to eq(409)
    host.restart
    expect(host.api('GET', '/api/corpora')).to include('id' => identity['id'], 'status' => 'paused')
    expect(host.state['pulse']['running']).to eq(false)
    expect(transition('activate')).to include('status' => 'active')
    expect(host.state['pulse']['running']).to eq(true)
    turn = host.submit(chief, 'Update the corpora settings.')
    expect(host.turn(turn)['output']).to eq('Contract test reply')
  end

  it 'deduplicates lifecycle commands and exposes their outcome without repeating them' do
    metadata = host.api('GET', '/api/corpora')
    operation = SecureRandom.uuid
    first = transition('pause', metadata, operation: operation)
    expect(transition('pause', metadata, operation: operation)['revision']).to eq(first['revision'])
    expect(host.api('GET', "/api/corpora/operations/#{operation}")).to include('applied' => true)
    expect(host.api('GET', '/api/corpora/operations/missing')).to include('applied' => false)
    expect(host.request('PUT', '/api/corpora/lifecycle', {action: 'activate', revision: metadata['revision'], operation: operation}).first).to eq(409)
    expect(host.request('PUT', '/api/corpora/lifecycle', {action: 'activate', revision: metadata['revision'], operation: SecureRandom.uuid}).first).to eq(409)
  end

  it 'replays an old pause receipt after activation and restart without pausing again' do
    original = host.api('GET', '/api/corpora')
    operation = SecureRandom.uuid
    transition('pause', original, operation: operation)
    active = transition('activate')
    host.restart
    replayed = transition('pause', original, operation: operation)
    expect(replayed).to include('status' => 'active', 'revision' => active['revision'])
    expect(host.state['pulse']['running']).to eq(true)
    expect(host.request('PUT', '/api/corpora/lifecycle', {action: 'activate', revision: original['revision'], operation: operation}).first).to eq(409)
    expect(host.api('GET', '/api/corpora')['revision']).to eq(active['revision'])
  end

  it 'lets only ChiefSapi update names, scope and archive state with revision checks' do
    other = host.agent
    initial = host.control(chief, 'corpora_get')
    expect(host.request('POST', "/api/agents/#{other}/control", {op: 'corpora_update', name: 'Unauthorized', revision: initial['revision']}).first).to eq(403)
    updated = host.control(chief, 'corpora_update', name: 'Studio', device: 'Office Mac', scope: 'Research and write reports.', revision: initial['revision'])
    expect(updated).to include('name' => 'Studio', 'device' => 'Office Mac', 'scope' => 'Research and write reports.')
    archived = host.control(chief, 'corpora_update', archived: true, revision: updated['revision'])
    expect(archived['status']).to eq('archived')
    expect(host.state['pulse']['running']).to eq(false)
    expect(host.request('PUT', '/api/corpora/lifecycle', {action: 'activate', revision: archived['revision'], operation: SecureRandom.uuid}).first).to eq(409)
    restored = host.control(chief, 'corpora_update', archived: false, revision: archived['revision'])
    expect(restored['status']).to eq('paused')
    host.restart
    expect(host.api('GET', '/api/corpora')).to include('name' => 'Studio', 'scope' => 'Research and write reports.', 'status' => 'paused')
  end

  it 'interrupts current work and never replays interrupted effects on activation' do
    host.configure(block: true)
    owner = host.agent
    turn = host.submit(owner, 'Work until paused')
    Contract.wait { host.calls.any? }
    transition('pause')
    host.turn(turn, 'interrupted')
    count = host.calls.length
    transition('activate')
    host.release
    sleep 2.2
    expect(host.calls.length).to eq(count)
    expect(host.state['turns'].find { |row| row['id'] == turn }['status']).to eq('interrupted')
  end

  it 'creates isolated local corpora without moving or renaming the existing workspace' do
    capabilities = host.api('GET', '/api/host')
    expect(capabilities['local_available']).to eq(true) if RUBY_PLATFORM.match?(/linux|darwin/) || Gem.win_platform?
    unless capabilities['local_available']
      expect(capabilities['corpora']).to be_empty
      expect(host.request('POST', '/api/host/local', {name: 'Studio', scope: 'Design'}).first).to eq(403)
      next
    end
    original = host.state
    created = host.api('POST', '/api/host/local', {name: 'Studio', scope: 'A long scope of work. ' * 12}, status: 201)
    expect(created).to include('name' => 'Studio', 'status' => 'paused', 'location' => 'Local')
    route = "/api/host/corpora/#{created['id']}"
    child = host.api('GET', route + '/api/state')
    expect(child['main_agent_id']).not_to eq(original['main_agent_id'])
    expect(child['agents'].first['name']).to eq('ChiefSapi')
    expect(host.state['corpora']['id']).to eq(original['corpora']['id'])
    expect(host.request('GET', route + '/api/host').first).to eq(400)
    host.restart
    expect(host.api('GET', '/api/host')['corpora'].map { |item| item['id'] }).to contain_exactly(original['corpora']['id'], created['id'])
    expect(host.api('GET', route + '/api/state')['corpora']['status']).to eq('paused')
  end

  it 'pairs a native remote connection, persists it and routes only to its authenticated corpora' do
    relay = Relay.new
    other = Host.new
    invitation = other.api('POST', '/api/remote/pair', {relay: relay.url, token: relay.token}, status: 201)['qr']
    pairing = host.api('POST', '/api/host/pair', invitation, status: 202)
    pending = Contract.wait { other.api('GET', '/api/remote')['pending'] }
    expect(pairing['code']).to eq(pending['confirmation_code'])
    other.api('POST', '/api/remote/approve', {peer: pending['peer']})
    result = Contract.wait { status = host.api('GET', "/api/host/pair/#{pairing['id']}"); status if status['state'] != 'waiting' }
    expect(result['state']).to eq('approved'), result.inspect
    cancelled_receipt = host.api('POST', "/api/host/pair/#{pairing['id']}", {})
    expect(cancelled_receipt).to eq(result.merge('state' => 'cancelled'))
    identity = other.state['corpora']['id']
    route = "/api/host/corpora/#{identity}"
    expect(host.api('GET', route + '/api/state')['corpora']['id']).to eq(identity)
    metadata = host.api('GET', route + '/api/corpora')
    saved_path = File.join(host.root, 'host-corpora.json')
    saved_at = File.mtime(saved_path)
    3.times { expect(host.api('GET', route + '/api/state')['corpora']).to eq(metadata) }
    expect(File.mtime(saved_path)).to eq(saved_at)
    host.api('PUT', route + '/api/corpora/lifecycle', action: 'pause', revision: metadata['revision'], operation: SecureRandom.uuid)
    expect(other.state['corpora']['status']).to eq('paused')
    expect(host.request('GET', route + '/api/remote').first).to eq(403)
    host.restart
    expect(host.api('GET', '/api/host')['corpora']).to include(hash_including('id' => identity, 'location' => 'Remote', 'status' => 'paused'))
    expect(host.api('GET', route + '/api/state')['corpora']['id']).to eq(identity)
  ensure
    other.close if other
    relay.close if relay
  end
  it 'keeps cancelled pairing terminal when the other device subsequently approves' do
    relay = Relay.new
    other = Host.new
    invitation = other.api('POST', '/api/remote/pair', {relay: relay.url, token: relay.token}, status: 201)['qr']
    pairing = host.api('POST', '/api/host/pair', invitation, status: 202)
    pending = Contract.wait { other.api('GET', '/api/remote')['pending'] }
    path = "/api/host/pair/#{pairing['id']}"
    cancelled = host.api('POST', path, {})
    expect(cancelled).to include('state' => 'cancelled', 'id' => pairing['id'], 'code' => pairing['code'])
    other.api('POST', '/api/remote/approve', {peer: pending['peer']})
    expect(host.api('POST', path, {})).to eq(cancelled)
    expect(host.api('GET', path)).to eq(cancelled)
    host.restart
    expect(host.api('GET', '/api/host')['corpora'].none? { |c| c['location'] == 'Remote' }).to eq(true)
  ensure
    other.close if other
    relay.close if relay
  end

end
