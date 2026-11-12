require 'sqlite3'
require 'yaml'

RSpec.describe 'Domain validation through saved installation fixtures', :host do
  before do
    @owner = chief
    host.api('PUT', '/api/corpora/lifecycle', {action: 'pause', operation: 'fixture-pause', revision: host.state['corpora']['revision']})
    @file = File.join(host.root, 'agentpy/agents', @owner, 'state.json')
    @state = JSON.parse(File.read(@file))
    @turn = {id: 'fixture-call', flow: 'chat', input: 'Saved request', status: 'failed', attempt: 3,
      created: '2026-10-27T12:00:00+00:00', extension: {kept: true}}
  end

  {
    'unknown status' => {status: 'runing'}, 'unknown flow' => {flow: 'unknown'},
    'nontext input' => {input: true}, 'text attempt' => {attempt: 'three'},
    'negative attempt' => {attempt: -1}, 'fractional attempt' => {attempt: 1.5}
  }.each do |behavior, fields|
    it "reports #{behavior} in saved work instead of substituting a default" do
      Contract.atomic_json(@file, @state.merge('turns' => [@turn.merge(fields)]))
      code, body = host.request('GET', '/api/state')
      expect(code).to eq(500)
      expect(body['error']).not_to be_empty
      expect(host.calls).to be_empty
    end
  end

  it 'rejects a malformed saved turns collection' do
    Contract.atomic_json(@file, @state.merge('turns' => 'invalid'))
    expect(host.request('GET', '/api/state').first).to eq(500)
    expect(host.calls).to be_empty
  end

  it 'preserves unknown turn metadata when a lifecycle action saves the record' do
    Contract.atomic_json(@file, @state.merge('turns' => [@turn]))
    host.api('PUT', '/api/corpora/lifecycle', {action: 'activate', operation: 'fixture-resume', revision: host.state['corpora']['revision']})
    host.api('POST', "/api/agents/#{@owner}/turns/fixture-call/cancel", {})
    saved = JSON.parse(File.read(@file))['turns'].first
    expect(saved).to eq(JSON.parse(JSON.generate(@turn.merge(status: 'cancelled'))))
    expect(host.calls).to be_empty
  end

  it 'archives settled history before trimming it and retains the current cancelled turn' do
    settled = 110.times.map { |n| @turn.merge(id: "past-#{n}", status: 'done') }
    chat = settled.map { |turn| {role: 'agent', turn: turn[:id], content: "Saved #{turn[:id]}"} }
    Contract.atomic_json(@file, @state.merge('turns' => settled + [@turn], 'chat' => chat))
    host.api('PUT', '/api/corpora/lifecycle', {action: 'activate', operation: 'fixture-resume', revision: host.state['corpora']['revision']})
    host.api('POST', "/api/agents/#{@owner}/turns/fixture-call/cancel", {})
    saved = JSON.parse(File.read(@file))
    expect(saved['turns'].length).to eq(100)
    expect(saved['turns'].last).to include('id' => 'fixture-call', 'status' => 'cancelled', 'extension' => {'kept' => true})
    expect(saved['chat'].length).to eq(100)
    archive = File.join(host.root, 'agentpy/corpora/archive', @owner)
    archived_turns = Dir[File.join(archive, 'turns/*.json')].flat_map { |file| JSON.parse(File.read(file)) }
    archived_chat = Dir[File.join(archive, 'chat/*.json')].flat_map { |file| JSON.parse(File.read(file)) }
    expect(archived_turns.map { |turn| turn['id'] }).to eq(11.times.map { |n| "past-#{n}" })
    expect(archived_chat.map { |message| message['turn'] }).to eq(10.times.map { |n| "past-#{n}" })
    expect(host.calls).to be_empty
    host.restart
    expect(JSON.parse(File.read(@file))['turns'].last['id']).to eq('fixture-call')
  end

  it 'reports an unknown saved execution mode instead of silently choosing normal mode' do
    Contract.atomic_json(File.join(File.dirname(@file), 'run-settings.json'), {mode: 'deeep'})
    expect(host.request('GET', '/api/state').first).to eq(500)
    expect(host.calls).to be_empty
  end

  def saved_workload(**fields)
    call = {callId: 'saved-call', addressedTo: @owner, node: 'Outcome', state: 'Completed',
      request: 'Saved request', causedBy: nil, output: 'Saved answer', extension: {kept: true}}.merge(fields)
    work = {workloadId: 'saved-work', tracked: true, calls: [call], originalIntent: 'Saved request', extension: {kept: true}}
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.execute('INSERT INTO workloads VALUES (?, ?)', ['saved-work', JSON.generate(work)])
    end
    JSON.parse(JSON.generate(work))
  end

  {
    'completed without output' => {output: nil},
    'unresolved without output' => {state: 'Unresolved', output: nil},
    'waiting without a question' => {state: 'WaitingForAdmin', node: 'WaitingForAdmin', output: nil},
    'delegated without a reply' => {state: 'Delegated', node: 'Delegation', output: nil},
    'queued with output' => {state: 'Queued', node: 'Assessing'},
    'running with output' => {state: 'Running', node: 'Execution'},
    'completed at the wrong node' => {node: 'Execution'},
    'waiting at the wrong node' => {state: 'WaitingForAdmin'}
  }.each do |behavior, fields|
    it "reports an inconsistent saved call: #{behavior}" do
      saved_workload(**fields)
      code, body = host.request('GET', '/api/state')
      expect(code).to eq(500)
      expect(body['error']).not_to be_empty
      expect(host.request('GET', '/api/health').first).to eq(200)
      expect(host.calls).to be_empty
    end
  end

  {
    'completed' => {state: 'Completed', node: 'Outcome'},
    'unresolved' => {state: 'Unresolved', node: 'Outcome'},
    'delegated' => {state: 'Delegated', node: 'Delegation'},
    'waiting' => {state: 'WaitingForAdmin', node: 'WaitingForAdmin'},
    'queued' => {state: 'Queued', node: 'Assessing', output: nil},
    'failed' => {state: 'Failed', node: 'Execution', output: nil},
    'interrupted' => {state: 'Interrupted', node: 'Execution', output: nil},
    'cancelled' => {state: 'Cancelled', node: 'Execution', output: nil}
  }.each do |behavior, fields|
    it "reads an existing #{behavior} workload without changing its identity or metadata" do
      work = saved_workload(**fields)
      expected = work['calls'].first.reject { |key, value| key == 'output' && value.nil? }
      read = -> { YAML.safe_load(host.api('GET', "/api/agents/#{@owner}/tasks"))['tasks'].first }
      expect(read.call['calls']).to eq([expected])
      host.restart
      expect(read.call['calls']).to eq([expected])
      expect(host.calls).to be_empty
    end
  end

  it 'rejects an empty saved workload history and keeps the health endpoint available' do
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.execute('INSERT INTO workloads VALUES (?, ?)', ['fixture-work', JSON.generate(workloadId: 'fixture-work', tracked: true, calls: [])])
    end
    code, body = host.request('GET', '/api/state')
    expect(code).to eq(500)
    expect(body['error']).not_to be_empty
    expect(host.request('GET', '/api/health').first).to eq(200)
  end
  it 'rejects an incomplete v7 database without upgrading or recreating missing tables' do
    host.stop
    database = File.join(host.root, 'corpora.sqlite3')
    SQLite3::Database.open(database) do |db|
      db.execute('DROP TABLE scheduled_routines')
      db.execute('PRAGMA user_version=7')
    end
    _, error, code = Contract.capture(Contract::BINARY, 'check-data', host.root, 'existing')
    expect(code).not_to eq(0)
    expect(error).to include('scheduled_routines')
    SQLite3::Database.open(database, readonly: true) do |db|
      expect(db.get_first_value('PRAGMA user_version')).to eq(7)
      expect(db.get_first_value("SELECT name FROM sqlite_master WHERE name='scheduled_routines'")).to be_nil
    end
  end

  {
    'unknown status' => [{id: 'broken', flow: 'chat', input: 'Saved request', status: 'unknown'}],
    'object-valued turns' => {'broken' => 'saved history'}
  }.each do |description, turns|
    it "rejects legacy history with #{description} before creating a backup" do
      host.stop
      Contract.atomic_json(@file, @state.merge('schema_version' => 2, 'turns' => turns))
      original = File.binread(@file)
      _, error, code = Contract.capture(Contract::BINARY, 'check-data', host.root, 'existing')
      expect(code).not_to eq(0)
      expect(error).to include('agent state')
      expect(File.binread(@file)).to eq(original)
      expect(File).not_to exist(File.join(File.dirname(@file), 'legacy-state-v2.json'))
    end
  end

  it 'migrates schema two before recovery and retains the original backup across restarts' do
    host.stop
    old = @state.merge('schema_version' => 2, 'turns' => [@turn.merge(status: 'budget_blocked')],
      'chat' => [{'role' => 'user', 'content' => 'Saved request', 'job' => 'fixture-call'}])
    Contract.atomic_json(@file, old)
    host.start
    saved = JSON.parse(File.read(@file))
    expect(saved['schema_version']).to eq(3)
    expect(saved['turns'].first).to include('id' => 'fixture-call', 'status' => 'interrupted')
    expect(saved['chat'].first).to include('turn' => 'fixture-call')
    expect(saved['chat'].first).not_to have_key('job')
    backup = File.join(File.dirname(@file), 'legacy-state-v2.json')
    expect(JSON.parse(File.read(backup))).to eq(JSON.parse(JSON.generate(old)))
    host.restart
    expect(JSON.parse(File.read(backup))).to eq(JSON.parse(JSON.generate(old)))
    expect(host.calls).to be_empty
  end

end
