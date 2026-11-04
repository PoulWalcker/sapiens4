RSpec.describe 'Local service contracts' do
  it 'persists identity, replies, drafts and execution mode without replaying completed work' do
    agent = host.agent
    original = host.state
    host.api('PUT', "/api/agents/#{agent}", name: 'Researcher', role: 'Research', execution: {mode: 'deep'})
    turn = host.submit(agent, 'Hello 😎 — quote " and newline\n')
    expect(host.turn(turn)).to include('output' => 'Contract test reply')
    host.api('PUT', '/api/preferences', {selected: agent, drafts: {agent => 'Keep this'}, panes: {chat: false}})
    host.restart
    state = host.state
    aggregate_failures do
      expect(state['corpora']).to eq(original['corpora'])
      expect(state['agents']).to eq(original['agents'])
      expect(state['preferences']).to include('selected' => agent, 'drafts' => {agent => 'Keep this'})
      expect(state['orchestration'][agent]['execution']).to include('mode' => 'deep', 'timeout_seconds' => 1800)
      expect(host.turn(turn)['output']).to eq('Contract test reply')
      expect(host.calls.size).to eq(1)
      expect(host.calls.first['args']).to include('model_reasoning_effort="xhigh"')
    end
  end

  it 'reports health, serves the complete UI and refuses a second host for the same workspace' do
    code, health, headers = host.request('GET', '/api/health')
    expect(code).to eq(200)
    expect(health).to eq('status' => 'ok', 'provider' => 'codex')
    expect(headers.fetch('server').first).to start_with('Sapiens4')
    %w[/workspace/ /workspace/app.js /workspace/styles.css].each do |path|
      code, body, headers = host.request('GET', path)
      expect(code).to eq(200)
      expect(body.size).to be > 100
      expect(headers['x-content-type-options']).to eq(['nosniff'])
    end
    _, error, code = Contract.capture(Contract::BINARY, '--port', '0', '--data-dir', host.root, env: host.environment)
    expect(code).not_to eq(0)
    expect(error).to include('Another Sapiens4 server')
  end

  {
    'foreign origin' => ['POST', '/api/agents', {}, {'Origin' => 'https://evil.example'}, 403],
    'opaque origin' => ['GET', '/api/state', nil, {'Origin' => 'null'}, 403],
    'foreign host' => ['GET', '/api/state', nil, {'Host' => 'evil.example'}, 403],
    'missing local header' => ['POST', '/api/agents', {}, {'X-Sapiens-Local' => nil}, 403],
    'wrong content type' => ['POST', '/api/agents', {}, {'Content-Type' => 'text/plain'}, 403],
    'invalid JSON' => ['POST', '/api/agents', '{', {}, 400],
    'JSON array' => ['POST', '/api/agents', '[]', {}, 400],
    'database download' => ['GET', '/corpora.sqlite3', nil, {}, 404],
    'repository files' => ['GET', '/.git/config', nil, {}, 404],
    'backend source files' => ['GET', '/corpora/host/Service.hs', nil, {}, 404],
    'UI build source' => ['GET', '/build/BuildUI.hs', nil, {}, 404],
    'build entrypoint' => ['GET', '/build/Build.hs', nil, {}, 404],
    'relay server source' => ['GET', '/remote/relay/Server.hs', nil, {}, 404],
    'model prompt files' => ['GET', '/corpora/prompts/conversation.md', nil, {}, 404],
    'dependency manifests' => ['GET', '/submodules/libs4/package.json', nil, {}, 404]
  }.each do |behavior, (method, path, body, headers, status)|
    it "rejects #{behavior} without changing the team" do
      before = host.state['agents']
      expect(host.request(method, path, body, headers).first).to eq(status)
      expect(host.state['agents']).to eq(before)
    end
  end

  it 'rejects an oversized declared body before reading it' do
    require 'socket'
    uri = URI(host.url)
    socket = TCPSocket.new(uri.host, uri.port)
    socket.write("POST /api/agents HTTP/1.0\r\nHost: #{uri.host}:#{uri.port}\r\nContent-Type: application/json\r\nX-Sapiens-Local: 1\r\nContent-Length: 2000001\r\n\r\n")
    expect(Timeout.timeout(5) { socket.gets }).to include('413')
  ensure
    socket.close if socket
  end

  %w[nova Nova@Test Éclair A/b 1Nova].each do |name|
    it "rejects invalid name #{name.inspect} atomically" do
      before = host.state['agents']
      expect(host.request('POST', '/api/agents', {name: name, role: 'Research'}).first).to eq(400)
      expect(host.request('PUT', "/api/agents/#{chief}", {name: name, role: 'Research'}).first).to eq(400)
      expect(host.state['agents']).to eq(before)
    end
  end

  it 'creates a team idempotently, reports a partial batch, and rejects management cycles' do
    created = host.control(chief, 'create_agent', name: 'Scout', role: 'Research')
    child = created.fetch('agent').fetch('id')
    expect(host.control(chief, 'create_agent', name: 'Scout', role: 'Research')).to include('created' => false)
    expect(host.request('POST', "/api/agents/#{child}/control", {op: 'create_agent', name: 'Forbidden', role: 'Research'}).first).to eq(403)
    batch = host.control(chief, 'batch', operations: [
      {op: 'create_agent', name: 'Writer', role: 'Writing'},
      {op: 'create_agent', name: 'Scout', role: 'Different role'},
      {op: 'create_agent', name: 'Unreached', role: 'Research'}])
    expect(batch).to include('saved' => false, 'partial' => true, 'failed_index' => 1)
    expect(host.state['agents'].map { |a| a['name'] }).to contain_exactly('ChiefSapi', 'Scout', 'Writer')
    expect(host.request('POST', "/api/agents/#{chief}/control", {op: 'manager', manager: child}).first).to eq(400)
    expect(host.state['orchestration'][child]['manager']).to eq(chief)
  end

  it 'retires and restores an agent with its history, files and identity intact' do
    agent = host.agent
    host.turn(host.submit(agent))
    file = File.join(host.workspace(agent), 'saved.txt')
    File.write(file, 'Saved work')
    host.control(chief, 'retire_agent', target: agent, reason: 'Finished')
    expect(host.request('POST', "/api/agents/#{agent}/messages", {text: 'No new work'}).first).to eq(409)
    host.restart
    expect(host.state['agents'].find { |a| a['id'] == agent }['retired']).to eq(true)
    expect(host.request('POST', "/api/agents/#{agent}/control", {op: 'rehire_agent', target: agent}).first).to eq(409)
    expect(host.request('POST', "/api/agents/#{chief}/control", {op: 'retire_agent', target: chief}).first).to eq(400)
    host.control(chief, 'rehire_agent', target: agent)
    host.restart
    expect(host.state['agents'].find { |a| a['id'] == agent }['retired']).to eq(false)
    expect(File.read(file)).to eq('Saved work')
    expect(host.calls.size).to eq(1)
    expect(host.state['turns'].size).to eq(1)
  end

  it 'keeps failed work reviewable, retries only explicitly, and cancels a failed retry' do
    agent = chief
    host.configure(fail: true)
    failed = host.submit(agent, 'First')
    expect(host.turn(failed, 'failed')).to include('output' => nil, 'error' => a_string_including('provider stdout and stderr omitted'))
    host.configure(fail: false)
    host.turn(host.submit(agent, 'New independent work'))
    expect(host.turn(failed, 'failed')['input']).to eq('First')
    host.api('POST', "/api/agents/#{agent}/turns/#{failed}/retry", {})
    host.turn(failed)
    host.configure(fail: true)
    cancelled = host.submit(agent, 'Again')
    host.turn(cancelled, 'failed')
    host.api('POST', "/api/agents/#{agent}/turns/#{cancelled}/cancel", {})
    expect(host.turn(cancelled, 'cancelled')['output']).to be_nil
    expect(host.calls.size).to eq(4)
  end

  it 'interrupts uncertain running work after a process crash and requires explicit retry' do
    agent = chief
    host.configure(block: true)
    turn = host.submit(agent, 'May already have acted')
    Contract.wait { host.calls.size == 1 }
    host.crash
    host.release
    host.configure(block: false)
    host.start
    expect(host.turn(turn, 'interrupted')['output']).to be_nil
    expect(host.calls.size).to eq(1)
    host.api('POST', "/api/agents/#{agent}/turns/#{turn}/retry", {})
    host.turn(turn)
    expect(host.calls.size).to eq(2)
  end

  it 'bounds independent work, preserves queued input on shutdown, and never replays interrupted calls' do
    agents = [chief] + %w[Second Third Fourth Fifth].map { |name| host.agent(name) }
    host.configure(block: true)
    turns = agents.map { |id| host.submit(id, "Work for #{id}") }
    Contract.wait { host.calls.size == 4 }
    snapshot = host.state
    expect(snapshot['turns'].count { |t| t['status'] == 'queued' }).to eq(1)
    expect(snapshot['computer']['owner']).to be_nil
    queued = snapshot['turns'].find { |t| t['status'] == 'queued' }['id']
    host.stop
    host.configure(block: false)
    host.start
    host.turn(queued)
    expect(host.calls.size).to eq(5)
    expect(host.state['turns'].reject { |t| t['id'] == queued }.map { |t| t['status'] }).to all(eq('interrupted'))
    expect(host.state['turns'].map { |t| t['id'] }).to match_array(turns)
  end

  it 'holds the computer exclusively during active work and releases it on cancellation' do
    first, second = chief, host.agent
    host.configure(block: true)
    a, b = host.submit(first), host.submit(second)
    Contract.wait { host.calls.size == 2 }
    expect(host.control(first, 'computer_acquire')).to eq('owner' => first)
    expect(host.control(first, 'computer_acquire')).to eq('owner' => first)
    expect(host.request('POST', "/api/agents/#{second}/control", {op: 'computer_acquire'}).first).to eq(409)
    host.api('POST', "/api/agents/#{first}/turns/#{a}/cancel", {})
    Contract.wait { host.state['computer']['owner'].nil? }
    expect(host.control(second, 'computer_acquire')).to eq('owner' => second)
    host.release
    host.turn(b)
    expect(host.state['computer']['owner']).to be_nil
  end

  it 'bounds one Sapi to two concurrent batches and cancels queued input without starting it' do
    owner = chief
    host.configure(block: true)
    first = host.submit(owner, 'First batch')
    Contract.wait { host.calls.size == 1 }
    second = host.submit(owner, 'Second batch')
    Contract.wait { host.calls.size == 2 }
    third = host.submit(owner, 'Cancelled queued batch')
    pulse = host.state['pulse']['numbers']['bpm60']
    Contract.wait { host.state['pulse']['numbers']['bpm60'] >= pulse + 2 }
    expect(host.calls.size).to eq(2)
    expect(host.state['turns'].find { |t| t['id'] == third }['status']).to eq('queued')
    expect(host.control(owner, 'computer_acquire', agencyRun: first)).to eq('owner' => owner)
    expect(host.request('POST', "/api/agents/#{owner}/control", {op: 'computer_acquire', agencyRun: second}).first).to eq(409)
    host.api('POST', "/api/agents/#{owner}/turns/#{third}/cancel", {})
    host.release
    host.turn(first)
    host.turn(second)
    host.turn(third, 'cancelled')
    expect(host.calls.size).to eq(2)
    expect(host.state['computer']['owner']).to be_nil
  end

  it 'publishes concurrent replies in completion order and does not replay them on restart' do
    owner = chief
    host.configure(messages: [
      {contains: 'FIRST_UNIQUE', rule: {block: true, gate: 'first', response: decision('FitsSpecialization', mode: 'Repl', reply: 'First reply')}},
      {contains: 'SECOND_UNIQUE', rule: {block: true, gate: 'second', response: decision('FitsSpecialization', mode: 'Repl', reply: 'Second reply')}}])
    first = host.submit(owner, 'FIRST_UNIQUE')
    Contract.wait { host.calls.size == 1 }
    second = host.submit(owner, 'SECOND_UNIQUE')
    Contract.wait { host.calls.size == 2 }
    host.release('second')
    second_result = host.turn(second)
    expect(host.state['turns'].find { |t| t['id'] == first }['status']).to eq('running')
    host.release('first')
    first_result = host.turn(first)
    expect(second_result['output_order']).to be < first_result['output_order']
    host.restart
    expect(host.calls.size).to eq(2)
    expect(host.state['turns'].sort_by { |t| t['output_order'] }.map { |t| t['output'] }).to eq(['Second reply', 'First reply'])
  end

  it 'streams a large Unicode prompt over stdin and stops detached provider descendants when cancelled' do
    owner = chief
    host.configure(rules: {'Assessing' => {block: true, child: true}})
    text = "😎 résumé \"quoted\" and newline\n" + ('∑' * 12000)
    id = host.submit(owner, text)
    call = Contract.wait { host.calls.first }
    Contract.wait { File.exist?(call['heartbeat']) }
    expect(call['prompt'].bytesize).to be > 32767
    expect(JSON.parse(call['prompt'].split("\nContext:\n", 2).last)['originalIntent']).to eq(text)
    expect(call['args'].last).to eq('-')
    expect(call['args']).not_to include(text)
    expect(call['agency_run']).to eq(id)
    host.api('POST', "/api/agents/#{owner}/turns/#{id}/cancel", {})
    host.turn(id, %w[interrupted cancelled])
    modified = File.mtime(call['heartbeat'])
    pulse = host.state['pulse']['numbers']['bpm120']
    Contract.wait { host.state['pulse']['numbers']['bpm120'] > pulse }
    expect(File.mtime(call['heartbeat'])).to eq(modified)
  end

  it 'archives a legacy installation without executing old blocked work or trusting cached model results' do
    owner = chief
    host.configure(block: true)
    id = host.submit(owner, 'Review before retry')
    Contract.wait { host.calls.size == 1 }
    workspace = host.workspace(owner)
    host.stop
    file = File.join(host.root, 'agentpy', 'agents', owner, 'state.json')
    legacy = JSON.parse(File.read(file))
    legacy.merge!('schema_version' => 2, 'budgets' => {'old' => {'spent' => 999999999}}, 'limits' => {'tokens_per_call' => 1})
    legacy['turns'].first.merge!('status' => 'budget_blocked', 'tokens' => 999, 'reserved' => 999)
    raw = JSON.generate(legacy)
    File.write(file, raw)
    cached = File.join(workspace, 'recent-context.json')
    File.write(cached, '[{"answer":"STALE_TOOL_RESULT"}]')
    host.configure(block: false)
    host.start
    host.turn(id, 'interrupted')
    expect(File.read(File.join(File.dirname(file), 'legacy-state-v2.json'))).to eq(raw)
    expect(host.calls.size).to eq(1)
    host.api('POST', "/api/agents/#{owner}/turns/#{id}/retry", {})
    host.turn(id)
    expect(host.calls.last['prompt']).not_to include('STALE_TOOL_RESULT')
    expect(File.read(cached)).to eq('[{"answer":"STALE_TOOL_RESULT"}]')
  end

  it 'carries uploaded and linked attachments to the provider while preserving the original message' do
    agent = chief
    doc = host.api('POST', "/api/agents/#{agent}/attachments", {kind: 'document', name: 'notes.txt', data: ['Total: 42'].pack('m0')}, status: 201)
    link = host.api('POST', "/api/agents/#{agent}/attachments", {kind: 'link', value: 'https://example.com/report'}, status: 201)
    host.api('PUT', '/api/preferences', {attachment_drafts: {agent => [doc['id']]}})
    host.restart
    expect(host.state['attachment_drafts'][agent].first['id']).to eq(doc['id'])
    turn = host.turn(host.submit(agent, 'Summarize', attachments: [doc['id'], link['id']]))
    expect(turn['input']).to eq('Summarize')
    expect(turn['attachments'].map { |a| a['id'] }).to eq([doc['id'], link['id']])
    expect(File.read(doc['value'])).to eq('Total: 42')
    context = JSON.parse(host.calls.last['prompt'].split("\nContext:\n", 2).last)
    content = context['conversation']['inputs'].map { |item| item['content'] }.join("\n")
    expect(content).to include(doc['value'].to_json[1...-1], link['value'])
    other = host.agent
    expect(host.request('POST', "/api/agents/#{other}/messages", {text: 'Steal', attachments: [doc['id']]}).first).to eq(400)
  end

  [
    {kind: 'link', value: 'javascript:alert(1)'}, {kind: 'link', value: 'https://user:secret@example.com'},
    {kind: 'document', name: '../escape', data: 'WA=='}, {kind: 'document', name: 'bad.txt', data: '***'},
    {kind: 'image', name: 'not.png', data: 'WA=='}
  ].each do |attachment|
    it "rejects unsafe attachment #{attachment.inspect}" do
      expect(host.request('POST', "/api/agents/#{chief}/attachments", attachment).first).to eq(400)
    end
  end

  %w[jobs workspaces execution].each do |field|
    it "refuses to grant runtime authority through the #{field} preference" do
      expect(host.request('PUT', '/api/preferences', {field => {}}).first).to eq(400)
    end
  end
  it 'preserves provider transcripts and actor attribution through success, failure and restart' do
    owner = chief
    child = host.control(owner, 'create_agent', name: 'Auditor', role: 'Review')['agent']['id']
    host.configure(mode: 'Exec')
    completed = host.submit(owner)
    host.turn(completed)
    read_run = ->(id) { JSON.parse(File.read(File.join(host.root, 'agentpy/corpora/archive', owner, 'runs', "#{id}.json"))) }
    saved = read_run.call(completed)
    expect(saved['logs'].map { |entry| entry['role'] }).to eq(%w[Assessing ReadyToWork Execution])
    expect(saved['logs']).to all(include('session' => 'fixture-session', 'answer' => a_kind_of(String)))
    expect(host.state['events'][owner]).to include(hash_including('event' => 'task.completed', 'actor' => owner, 'agency' => 'chatInput'))
    expect(host.state['events'][child]).to include(hash_including('event' => 'sapi.created', 'actor' => owner))
    host.configure(fail: true)
    failed = host.submit(owner, 'Keep the unsuccessful call reviewable')
    host.turn(failed, 'failed')
    rejected = read_run.call(failed)
    expect(rejected['logs'].first).to include('role' => 'Assessing', 'prompt' => a_string_including('Keep the unsuccessful call reviewable'))
    expect(rejected['error']).to include('provider stdout and stderr omitted')
    host.restart
    expect(read_run.call(completed)).to eq(saved)
    expect(read_run.call(failed)).to eq(rejected)
  end

  it 'keeps the designated Chief after identity edits, creation and restart' do
    original = chief
    member = host.agent('Researcher')
    host.api('PUT', "/api/agents/#{original}", {name: 'Director', role: 'Coordinate work'})
    host.restart
    expect(chief).to eq(original)
    expect(host.api('GET', '/api/corpora')['chief_id']).to eq(original)
    expect(host.state['orchestration'][member]['manager']).to eq(original)
    expect(host.request('POST', "/api/agents/#{original}/control", {op: 'manager', manager: member}).first).to eq(400)
    expect(host.request('POST', "/api/agents/#{original}/control", {op: 'retire_agent', target: original}).first).to eq(400)
  end

  it 'rejects an invalid batch before executing its valid prefix' do
    before = host.state['agents']
    result = host.request('POST', "/api/agents/#{chief}/control", {op: 'batch', operations: [
      {op: 'create_agent', name: 'Uncreated', role: 'Research'}, {op: 'workspace_focus', unexpected: true}]})
    expect(result.first).to eq(400)
    expect(host.state['agents']).to eq(before)
    expect(host.request('POST', "/api/agents/#{chief}/control", {op: 'batch', operations: [{op: 'batch', operations: [{op: 'status'}]}]}).first).to eq(400)
  end

  it 'requires active reports to be reassigned before retirement and repairs a retired manager on restore' do
    manager = host.agent('Manager')
    report = host.control(chief, 'create_agent', name: 'Reporter', role: 'Research', manager: manager)['agent']['id']
    expect(host.request('POST', "/api/agents/#{chief}/control", {op: 'retire_agent', target: manager}).first).to eq(409)
    host.control(chief, 'retire_agent', target: report)
    host.control(chief, 'retire_agent', target: manager)
    host.control(chief, 'rehire_agent', target: report)
    host.restart
    expect(host.state['orchestration'][report]['manager']).to eq(chief)
    expect(host.state['agents'].find { |a| a['id'] == manager }['retired']).to eq(true)
  end

end
