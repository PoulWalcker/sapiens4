require 'sqlite3'

RSpec.describe 'Personal routine lifecycle', :host do
  before { @owner = host.agent }
  def fields
    {title: 'Product comparison', goal: 'Compare the products.', criteria: ['Return a sourced comparison.']}
  end
  def create(operation = 'create', **changes)
    host.api('POST', "/api/agents/#{@owner}/routines", fields.merge(operation: operation).merge(changes)).fetch('routine')
  end
  def command(id, action, operation, **changes)
    host.api('POST', "/api/agents/#{@owner}/routines/#{id}/#{action}", {operation: operation, revision: 1}.merge(changes))
  end
  def definition(id)
    host.state.fetch('personal_routines').find { |row| row['id'] == id }
  end
  def occurrence(id, run, status)
    Contract.wait(description: "routine occurrence #{run}: #{status}") do
      row = definition(id).fetch('runs').find { |entry| entry['id'] == run }
      row if row && row['status'] == status
    end
  end

  it 'replays admissions across restart, pins revisions and distinguishes changed inputs' do
    id = create
    expect(create).to eq(id)
    run = command(id, 'run', 'run-a', inputs: 'First dataset').fetch('run')
    occurrence(id, run, 'done')
    command(id, 'edit', 'edit', **fields.merge(goal: 'Compare another dataset.'))
    expect(definition(id)['revision']).to eq(2)
    host.restart
    expect(command(id, 'run', 'run-a', inputs: 'First dataset')['run']).to eq(run)
    expect(occurrence(id, run, 'done')['snapshot']).to include('goal' => fields[:goal], 'revision' => 1, 'inputs' => 'First dataset', 'owner' => @owner)
    path = "/api/agents/#{@owner}/routines/#{id}/run"
    expect(host.request('POST', path, operation: 'run-a', revision: 1, inputs: 'Different dataset').first).to eq(409)
    second = command(id, 'run', 'run-b', revision: 2, inputs: 'Different dataset').fetch('run')
    expect(second).not_to eq(run)
    occurrence(id, second, 'done')
    host.restart
    expect(host.state.fetch('events').fetch(@owner)).to include(include('event' => 'routine.command_refused'))
    expect(definition(id)['runs'].length).to eq(2)
  end

  it 'retains successful history when a later provider attempt fails and the host restarts' do
    id = create
    completed = 2.times.map do |index|
      run = command(id, 'run', "run-#{index}").fetch('run')
      occurrence(id, run, 'done')
      run
    end
    host.configure(fail: true)
    failed = command(id, 'run', 'failed').fetch('run')
    occurrence(id, failed, 'failed')
    host.restart
    completed.each { |run| expect(occurrence(id, run, 'done')['result']).to eq('Verified comparison') }
    expect(occurrence(id, failed, 'failed')['error']).not_to be_empty
  end

  it 'preserves completed results on deletion and refuses new work durably' do
    id = create
    run = command(id, 'run', 'run').fetch('run')
    occurrence(id, run, 'done')
    command(id, 'delete', 'delete')
    expect(definition(id)['deleted']).to eq(true)
    expect(occurrence(id, run, 'done')['result']).to eq('Verified comparison')
    path = "/api/agents/#{@owner}/routines/#{id}/run"
    expect(host.request('POST', path, operation: 'deleted-run', revision: 2).first).to eq(409)
    host.restart
    expect(host.state['routine_errors']).to include(include('id' => 'deleted-run', 'code' => 409))
  end

  it 'retains failed creation receipts without a definition and rejects forged ownership' do
    path = "/api/agents/#{@owner}/routines"
    request = fields.merge(operation: 'invalid-create', owner: 'another-owner')
    first = host.request('POST', path, request)
    expect(first.first).to eq(400)
    host.restart
    expect(host.request('POST', path, request)).to eq(first)
    expect(host.state['routine_errors']).to include(include('id' => 'invalid-create', 'owner' => @owner))
    expect(host.state['personal_routines']).to be_empty
  end
  def delegate_to(worker, blocked: false, inputs: [])
    host.configure(mode: 'Exec', rules: {
      "#{@owner}:Execution" => {response: decision('OutsideSpecialization')},
      'ChiefTriage' => {response: decision('SpecialistSelected', target: worker)},
      'ReadyToWork' => {response: decision('TaskPrepared', taskType: 'automated', specification: {
        objective: fields[:goal], inputs: inputs, expectedOutputs: fields[:criteria], completionCriteria: fields[:criteria]})},
      "#{worker}:Execution" => {block: blocked}
    })
  end

  it 'runs a delegated manual occurrence without waiting for the interval scheduler' do
    @owner = chief
    worker = host.agent('Delegate')
    delegate_to(worker)
    id = create
    run = command(id, 'run', 'delegated').fetch('run')
    result = occurrence(id, run, 'done')
    expect(result).to include('currentOwner' => worker, 'result' => 'Verified comparison')
    expect(host.calls.map { |call| call['owner'] }).to include(worker)
  end

  it 'stops the canonical delegation after editable child origin is removed and leaves neighbors running' do
    @owner = chief
    worker = host.agent('Delegate')
    neighbor = host.agent('Neighbor')
    delegate_to(worker, blocked: true)
    id = create
    run = command(id, 'run', 'delegated').fetch('run')
    call = Contract.wait do
      host.calls.find { |entry| entry['owner'] == worker && entry['node'] == 'Execution' }
    end
    child = call.fetch('agency_run')
    file = File.join(host.root, 'agentpy/agents', worker, 'state.json')
    state = JSON.parse(File.read(file))
    state.fetch('turns').find { |entry| entry['id'] == child }.delete('origin')
    Contract.atomic_json(file, state)
    command(id, 'stop', 'stop', run: run)
    expect(occurrence(id, run, 'stopped')['result']).to be_nil
    expect(host.request('POST', "/api/agents/#{worker}/control", op: 'computer_acquire', agencyRun: child).first).to eq(409)
    expect(host.request('POST', "/api/agents/#{worker}/turns/#{child}/retry", {}).first).to eq(409)
    host.release
    host.turn(host.submit(neighbor))
    host.restart
    expect(occurrence(id, run, 'stopped')['result']).to be_nil
  end

  it 'migrates schema nine while retaining existing managed history' do
    legacy = host.submit(@owner, 'Existing managed request', managed: true)
    before = host.turn(legacy)
    interval = host.control(@owner, 'routine_create', minutes: 60, prompt: 'Existing interval work')['routine']
    host.stop
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.execute('DROP TABLE command_receipts')
      db.execute('PRAGMA user_version=9')
    end
    host.start
    expect(host.turn(legacy)).to eq(before)
    expect(host.state['routines']).to include(interval)
    id = create
    run = command(id, 'run', 'first-after-upgrade').fetch('run')
    occurrence(id, run, 'done')
    host.restart
    expect(host.turn(legacy)).to eq(before)
    expect(host.state['routines']).to include(interval)
    expect(host.state['routines'].map { |row| row['id'] }).not_to include(id)
    expect(occurrence(id, run, 'done')['result']).to eq('Verified comparison')
  end

  it 'deletes active work and preserves a completed occurrence without permitting its Stop' do
    id = create
    completed = command(id, 'run', 'completed').fetch('run')
    occurrence(id, completed, 'done')
    expect(host.request('POST', "/api/agents/#{@owner}/routines/#{id}/stop", operation: 'late-stop', revision: 1, run: completed).first).to eq(409)
    host.configure(block: true)
    active = command(id, 'run', 'active').fetch('run')
    occurrence(id, active, 'running')
    command(id, 'delete', 'delete')
    host.release
    host.restart
    expect(occurrence(id, completed, 'done')['result']).to eq('Verified comparison')
    expect(occurrence(id, active, 'stopped')['result']).to be_nil
    expect(host.request('POST', "/api/agents/#{@owner}/turns/#{active}/retry", {}).first).to eq(409)
  end

  it 'replays accepted operations while paused and retains refused new admissions' do
    id = create
    run = command(id, 'run', 'accepted').fetch('run')
    occurrence(id, run, 'done')
    config = host.api('GET', '/api/corpora')
    host.api('PUT', '/api/corpora/lifecycle', action: 'pause', operation: 'pause-fixture', revision: config['revision'])
    expect(command(id, 'run', 'accepted')['run']).to eq(run)
    path = "/api/agents/#{@owner}/routines/#{id}/run"
    expect(host.request('POST', path, operation: 'paused-run', revision: 1).first).to eq(409)
    host.restart
    expect(host.state['routine_errors']).to include(include('id' => 'paused-run', 'code' => 409))
    expect(definition(id)['runs'].length).to eq(1)
  end

  def database
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) { |db| yield db }
  end

  it 'rolls back the occurrence and managed root when saving its receipt fails' do
    id = create
    database { |db| db.execute("CREATE TRIGGER reject_receipt BEFORE INSERT ON command_receipts WHEN NEW.id='atomic-run' BEGIN SELECT RAISE(ABORT, 'fixture receipt failure'); END") }
    expect(host.request('POST', "/api/agents/#{@owner}/routines/#{id}/run", operation: 'atomic-run', revision: 1).first).to eq(500)
    expect(definition(id)['runs']).to be_empty
    expect(host.state['turns']).to be_empty
    expect(host.calls).to be_empty
    database { |db| db.execute('DROP TRIGGER reject_receipt') }
    run = command(id, 'run', 'atomic-run').fetch('run')
    occurrence(id, run, 'done')
    expect(definition(id)['runs'].length).to eq(1)
  end

  it 'recovers an accepted occurrence after a crash with its file projection unavailable', unless: Gem.win_platform? do
    id = create
    directory = File.join(host.root, 'agentpy/agents', @owner)
    File.chmod(0500, directory)
    run = command(id, 'run', 'crash-run').fetch('run')
    host.crash
    File.chmod(0700, directory)
    host.start
    expect(command(id, 'run', 'crash-run')['run']).to eq(run)
    occurrence(id, run, 'done')
    expect(definition(id)['runs'].length).to eq(1)
    expect(host.calls.length).to eq(1)
  ensure
    File.chmod(0700, directory) if directory && File.directory?(directory)
  end

  [nil, 'unsupported'].each do |trigger|
    it "rejects a stored malformed trigger #{trigger.inspect} rather than scheduling it" do
      id = create
      database do |db|
        row = JSON.parse(db.get_first_value('SELECT value FROM scheduled_routines WHERE sequence=?', id.split('_').last.to_i))
        row['trigger'] = trigger
        db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?', [JSON.generate(row), id.split('_').last.to_i])
      end
      expect(host.request('GET', '/api/state').first).to eq(500)
      expect(host.calls).to be_empty
    end
  end

  it 'keeps the accepted task specification through an active edit followed by delegation' do
    @owner = chief
    worker = host.agent('Delegate')
    delegate_to(worker, inputs: ['Dataset A'])
    host.configure(rules: host.provider.fetch('rules').merge(
      "#{@owner}:Execution" => {block: true, response: decision('OutsideSpecialization')}))
    id = create
    run = command(id, 'run', 'pinned', inputs: 'Dataset A').fetch('run')
    Contract.wait { host.calls.any? { |call| call['node'] == 'Execution' } }
    command(id, 'edit', 'edit-active', **fields.merge(goal: 'A different future goal.'))
    host.release
    occurrence(id, run, 'done')
    prompt = host.calls.find { |call| call['owner'] == worker && call['node'] == 'Execution' }.fetch('prompt')
    preparation = host.calls.find { |call| call['owner'] == worker && call['node'] == 'ReadyToWork' }.fetch('prompt')
    expect(preparation).to include('copy that entire specification unchanged')
    context = JSON.parse(prompt.split("\nContext:\n", 2).last)
    expect(context.fetch('task').fetch('specification')).to eq(
      'objective' => fields[:goal], 'inputs' => ['Dataset A'],
      'expectedOutputs' => fields[:criteria], 'completionCriteria' => fields[:criteria])
    expect(definition(id)['goal']).to eq('A different future goal.')
  end

  it 'keeps a waiting clarification active until Stop closes the occurrence' do
    @owner = chief
    host.configure(rules: {
      "#{@owner}:Execution" => {response: decision('OutsideSpecialization')},
      'ChiefTriage' => {response: decision('RequestUnclear', question: 'Which products?')}
    })
    id = create
    run = command(id, 'run', 'waiting').fetch('run')
    expect(occurrence(id, run, 'waiting')['message']).to eq('Which products?')
    command(id, 'stop', 'stop-waiting', run: run)
    occurrence(id, run, 'stopped')
    expect(host.request('POST', "/api/agents/#{@owner}/messages", text: 'Products A and B', workload: run).first).to eq(409)
  end

  it 'fails closed on a malformed persisted occurrence gate' do
    host.configure(block: true)
    id = create
    run = command(id, 'run', 'gate').fetch('run')
    occurrence(id, run, 'running')
    original = nil
    database do |db|
      original = db.get_first_value('SELECT value FROM workloads WHERE id=?', run)
      value = JSON.parse(original).merge('executionGate' => 'unknown')
      db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(value), run])
    end
    expect(host.request('GET', '/api/state').first).to eq(500)
    expect(host.request('POST', "/api/agents/#{@owner}/control", op: 'computer_acquire', agencyRun: run).first).to eq(500)
    database { |db| db.execute('UPDATE workloads SET value=? WHERE id=?', [original, run]) }
    command(id, 'stop', 'stop-invalid-gate', run: run)
    occurrence(id, run, 'stopped')
  end

  it 'finishes answered work without treating the consumed question as an active call' do
    @owner = chief
    worker = host.agent('Delegate')
    host.configure(rules: {
      "#{@owner}:Execution" => {response: decision('OutsideSpecialization')},
      'ChiefTriage' => {response: decision('RequestUnclear', question: 'Which products?')}
    })
    id = create
    run = command(id, 'run', 'answerable').fetch('run')
    expect(occurrence(id, run, 'waiting')['message']).to eq('Which products?')
    delegate_to(worker)
    host.submit(@owner, 'Products A and B', workload: run)
    occurrence(id, run, 'done')
    expect(host.request('POST', "/api/agents/#{@owner}/routines/#{id}/stop", operation: 'stop-finished', revision: 1, run: run).first).to eq(409)
    command(id, 'delete', 'delete-answered')
    host.restart
    expect(occurrence(id, run, 'done')['result']).to eq('Verified comparison')
  end

end
