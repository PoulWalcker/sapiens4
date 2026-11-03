require 'sqlite3'
require 'time'

RSpec.describe 'Durable interval occurrences', :host do
  def database(&block)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3'), &block)
  end

  def create_routine
    host.api('POST', "/api/agents/#{chief}/routines", operation: 'create-routine',
      title: 'Product comparison', goal: 'Compare the products.',
      criteria: ['Return a sourced comparison.']).fetch('routine')
  end

  def definition(identity)
    host.state.fetch('personal_routines').find { |row| row['id'] == identity }
  end

  def command(identity, action, operation, revision, **fields)
    host.api('POST', "/api/agents/#{chief}/routines/#{identity}/#{action}",
      {operation: operation, revision: revision}.merge(fields))
  end

  def overdue(identity)
    host.stop
    saved = nil
    database do |db|
      saved = JSON.parse(db.get_first_value('SELECT value FROM scheduled_routines WHERE sequence=?', identity.split('_').last.to_i))
      saved['nextDue'] = '2020-01-01T00:00:00.000000+00:00'
      db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?', [JSON.generate(saved), identity.split('_').last.to_i])
    end
    saved
  end

  def wait_for_run(identity, status = nil)
    Contract.wait(timeout: 85, description: "interval occurrence #{identity}: #{status}") do
      run = definition(identity).fetch('runs').last
      run if run && (!status || run['status'] == status)
    end
  end

  def restore_deadline(saved)
    database do |db|
      db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?',
        [JSON.generate(saved), saved.fetch('id').split('_').last.to_i])
    end
  end

  it 'configures the same definition through UI and host tools and rejects a stale editor' do
    id = create_routine
    command(id, 'schedule', 'schedule-routine', 1, minutes: 60)
    expect(definition(id)).to include('trigger' => 'interval', 'minutes' => 60, 'revision' => 2)
    changed = host.control(chief, 'routine_update', id: id, minutes: 5, prompt: 'Compare the products.').fetch('routine')
    expect(changed).to include('id' => id, 'minutes' => 5, 'revision' => 3, 'criteria' => ['Return a sourced comparison.'])
    expect(host.control(chief, 'routines').fetch('routines').map { |row| row['id'] }).to eq([id])
    expect(host.request('POST', "/api/agents/#{chief}/routines/#{id}/schedule",
      operation: 'stale-schedule', revision: 2, minutes: 10).first).to eq(409)
    command(id, 'pause', 'pause-schedule', 3)
    run = command(id, 'run', 'manual-while-paused', 4).fetch('run')
    host.turn(run)
    expect(definition(id)).to include('paused' => true, 'runCount' => 0)
    expect(definition(id).fetch('runs').last).to include('id' => run, 'trigger' => 'manual', 'status' => 'done')
    host.restart
    expect(definition(id)).to include('id' => id, 'revision' => 4, 'paused' => true, 'minutes' => 5)
  end

  it 'adds a schedule to an existing explicit definition through the legacy tool without replacing its contract' do
    id = create_routine
    changed = host.control(chief, 'routine_update', id: id, minutes: 5, prompt: 'A future comparison.').fetch('routine')
    expect(changed).to include('id' => id, 'revision' => 2, 'goal' => 'A future comparison.',
      'criteria' => ['Return a sourced comparison.'], 'trigger' => 'interval')
    other = host.agent('Other')
    expect(host.request('POST', "/api/agents/#{other}/control", op: 'routine_update', id: id,
      minutes: 10, prompt: 'Wrong owner').first).to eq(404)
    expect(definition(id)['revision']).to eq(2)
  end

  it 'keeps legacy interval displays and command acknowledgments populated without a second stored instruction' do
    id = create_routine
    command(id, 'schedule', 'schedule-routine', 1, minutes: 60)
    expect(host.state['routines'].find { |row| row['id'] == id }['prompt']).to eq('Compare the products.')
    reply = host.turn(host.submit(chief, "Pause Scheduled Routine\n@#{id}"))
    expect(reply['output']).to include('Execute: Compare the products.')
    stored = host.control(chief, 'routines').fetch('routines').find { |row| row['id'] == id }
    expect(stored).to include('goal' => 'Compare the products.', 'paused' => true)
    expect(stored).not_to have_key('prompt')
  end

  it 'normalizes the legacy interval trigger in the public snapshot without rewriting storage' do
    id = host.control(chief, 'routine_create', minutes: 60, prompt: 'Compare the products.').fetch('routine').fetch('id')
    stored = nil
    database { |db| stored = db.get_first_value('SELECT value FROM scheduled_routines WHERE sequence=?', id.split('_').last.to_i) }
    expect(JSON.parse(stored)).not_to have_key('trigger')
    expect(definition(id)).to include('trigger' => 'interval', 'minutes' => 60, 'criteria' => nil,
      'nextDue' => JSON.parse(stored).fetch('nextDue'))
    database { |db| expect(db.get_first_value('SELECT value FROM scheduled_routines WHERE sequence=?', id.split('_').last.to_i)).to eq(stored) }
  end

  it 'preserves completed legacy history after an answered waiting ancestor and deletion' do
    owner = chief
    id = host.control(owner, 'routine_create', minutes: 60, prompt: 'Compare the products.').fetch('routine').fetch('id')
    identity = 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
    child = 'cccccccccccccccccccccccccccccccc'
    root_call = {'callId' => identity, 'addressedTo' => owner, 'node' => 'WaitingForAdmin',
      'state' => 'WaitingForAdmin', 'request' => 'Compare the products.', 'causedBy' => nil,
      'attempt' => 1, 'output' => 'Which products?'}
    work = {'workloadId' => identity, 'tracked' => true,
      'calls' => [root_call, root_call.merge('callId' => child, 'causedBy' => identity,
        'node' => 'Outcome', 'state' => 'Completed', 'output' => 'Preserved old result')],
      'originalIntent' => 'Compare the products.', 'task' => {'taskId' => "task_#{identity}",
        'taskType' => 'automated', 'specification' => nil}, 'origin' => {'routine' => id, 'routineOwner' => owner},
      'decisions' => [], 'outcome' => {'reply' => 'Preserved old result', 'author' => owner}}
    host.stop
    database { |db| db.execute('INSERT INTO workloads(id,value) VALUES (?,?)', [identity, JSON.generate(work)]) }
    host.start
    expect(wait_for_run(id, 'done')['result']).to eq('Preserved old result')
    expect(host.request('POST', "/api/agents/#{owner}/routines/#{id}/stop",
      operation: 'stop-finished', revision: 1, run: identity).first).to eq(409)
    command(id, 'delete', 'delete-completed', 1)
    host.restart
    expect(definition(id)['deleted']).to eq(true)
    expect(wait_for_run(id, 'done')['result']).to eq('Preserved old result')
  end

  it 'preserves timer pause and fences the manual request after stopping an admitted unsafe occurrence', :slow do
    id = create_routine
    command(id, 'schedule', 'schedule-routine', 1, minutes: 60)
    overdue(id)
    host.configure(block: true)
    host.start
    timer = wait_for_run(id, 'running')
    expect(timer['trigger']).to eq('interval')
    Contract.wait(description: 'unsafe timer provider admission') { host.calls.length == 1 }
    command(id, 'pause', 'pause-active', 2)
    expect(wait_for_run(id)['status']).to eq('running')
    manual = command(id, 'run', 'manual-during-pause', 3).fetch('run')
    expect(manual).not_to eq(timer['id'])
    command(id, 'stop', 'stop-timer', 3, run: timer['id'])
    host.release
    # Stopping an admitted unsafe invocation cannot prove absence of effects.
    failed = host.turn(manual, 'failed')
    expect(failed.fetch('error')).to include('independent absence proof')
    expect(host.calls.length).to eq(1)
    evidence = host.api('GET', "/api/agents/#{chief}/routines/#{id}/runs/#{timer.fetch('id')}")
    expect(evidence.fetch('routineEffectUnknown')).not_to be_nil
    runs = definition(id).fetch('runs').to_h { |run| [run['id'], run] }
    expect(runs[timer['id']]).to include('status' => 'stopped', 'result' => nil)
    expect(runs[manual]).to include('status' => 'failed', 'trigger' => 'manual')
    expect(definition(id)).to include('paused' => true, 'runCount' => 1)
  end

  it 'rolls back admission when advancing the deadline fails', :slow do
    id = create_routine
    command(id, 'schedule', 'schedule-routine', 1, minutes: 60)
    saved = overdue(id)
    database do |db|
      db.execute("CREATE TRIGGER reject_deadline BEFORE UPDATE ON scheduled_routines WHEN NEW.sequence=#{id.split('_').last.to_i} BEGIN SELECT RAISE(ABORT, 'fixture deadline failure'); END")
    end
    host.start
    Contract.wait(timeout: 85, description: 'real minute pulse attempts the admission') do
      host.state.dig('pulse', 'numbers', 'bph60').to_i >= 1
    end
    expect(definition(id)).to include('nextDue' => saved['nextDue'], 'runCount' => 0, 'runs' => [])
    expect(host.state['turns']).to be_empty
    expect(host.calls).to be_empty
  ensure
    database { |db| db.execute('DROP TRIGGER IF EXISTS reject_deadline') } if @host
  end

  it 'recovers committed queued work when its file projection was unavailable', :slow, unless: Gem.win_platform? do
    owner = chief
    id = create_routine
    command(id, 'schedule', 'schedule-routine', 1, minutes: 60)
    overdue(id)
    directory = File.join(host.root, 'agentpy/agents', owner)
    host.start
    File.chmod(0500, directory)
    run = wait_for_run(id, 'queued')
    expect(definition(id)['runCount']).to eq(1)
    host.crash
    File.chmod(0700, directory)
    host.start
    completed = wait_for_run(id, 'done')
    expect(completed['id']).to eq(run['id'])
    expect(definition(id).fetch('runs').length).to eq(1)
    expect(host.calls.length).to eq(1)
  ensure
    File.chmod(0700, directory) if directory && File.directory?(directory)
  end

  it 'does not re-execute an interrupted occurrence when its old deadline is seen again', :slow do
    id = create_routine
    command(id, 'schedule', 'schedule-routine', 1, minutes: 60)
    saved = overdue(id)
    host.configure(block: true)
    host.start
    run = wait_for_run(id, 'running')
    Contract.wait(description: 'provider started before the crash') { host.calls.length == 1 }
    host.crash
    restore_deadline(saved)
    host.start
    expect(wait_for_run(id, 'interrupted')['id']).to eq(run['id'])
    Contract.wait(timeout: 85, description: 'duplicate deadline is consumed without duplicate work') do
      definition(id)['runCount'] == 1 && definition(id)['nextDue'] != saved['nextDue']
    end
    expect(definition(id).fetch('runs').map { |entry| entry['id'] }).to eq([run['id']])
    expect(host.calls.length).to eq(1)
    expect(wait_for_run(id)['status']).to eq('interrupted')
  end

  it 'preserves a waiting question and its identity across restart', :slow do
    id = create_routine
    command(id, 'schedule', 'schedule-routine', 1, minutes: 60)
    overdue(id)
    host.configure(rules: {
      'Execution' => {response: decision('OutsideSpecialization')},
      'ChiefTriage' => {response: decision('RequestUnclear', question: 'Which products?')}
    })
    host.start
    run = wait_for_run(id, 'waiting')
    command(id, 'pause', 'pause-waiting', 2)
    host.restart
    expect(wait_for_run(id, 'waiting')).to include('id' => run['id'], 'message' => 'Which products?')
    command(id, 'stop', 'stop-waiting', 3, run: run['id'])
    expect(wait_for_run(id, 'stopped')['result']).to be_nil
    expect(host.request('POST', "/api/agents/#{chief}/messages", text: 'Products A and B', workload: run['id']).first).to eq(409)
  end

  # Saved pre-managed installations may have stopped between the old file and
  # Workload writes. These are storage fixtures, not calls to application internals.
  %w[queued_turn terminal_turn orphan_work waiting_child queued_child missing_running_child].each do |shape|
    it "retains legacy admission evidence for #{shape}", :slow do
      owner = chief
      id = host.control(owner, 'routine_create', minutes: 60, prompt: 'Compare the products.').fetch('routine').fetch('id')
      expect(id).to eq('routine_sch_00001')
      overdue(id)
      identity = '9cd9bdeb56215ea181afae827fe77b8d'
      child = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
      origin = {'routine' => id, 'routineOwner' => owner}
      turn = {'id' => identity, 'flow' => 'chat', 'input' => 'Compare the products.',
        'status' => 'queued', 'attempt' => 1, 'batchable' => true,
        'created' => '2020-01-01T00:00:00+00:00', 'origin' => origin}
      call = {'callId' => identity, 'addressedTo' => owner, 'node' => 'ReadyToWork',
        'state' => 'Queued', 'request' => 'Compare the products.', 'causedBy' => nil, 'attempt' => 1}
      work = {'workloadId' => identity, 'tracked' => true, 'calls' => [call],
        'originalIntent' => 'Compare the products.', 'task' => {'taskId' => "task_#{identity}",
          'taskType' => 'automated', 'specification' => nil}, 'origin' => origin,
        'decisions' => [], 'outcome' => nil}
      file = File.join(host.root, 'agentpy/agents', owner, 'state.json')
      state = JSON.parse(File.read(file))
      case shape
      when 'queued_turn', 'terminal_turn'
        turn['status'] = 'done' if shape == 'terminal_turn'
        state.fetch('turns') << turn
        if shape == 'terminal_turn'
          state.fetch('chat') << {'role' => 'agent', 'content' => 'Unvalidated old reply',
            'turn' => identity, 'time' => turn['created']}
        end
      when 'waiting_child', 'queued_child', 'missing_running_child'
        call['state'] = 'Delegated'
        child_state = {'waiting_child' => 'WaitingForAdmin', 'queued_child' => 'Queued', 'missing_running_child' => 'Running'}.fetch(shape)
        successor = call.merge('callId' => child, 'causedBy' => identity,
          'state' => child_state)
        successor['output'] = 'Which products?' if shape == 'waiting_child'
        work['calls'] << successor
        state.fetch('turns') << turn.merge('id' => child, 'status' => 'done') if shape == 'waiting_child'
        state.fetch('turns') << turn.merge('status' => 'done') if shape == 'missing_running_child'
      end
      Contract.atomic_json(file, state)
      database { |db| db.execute('INSERT INTO workloads(id,value) VALUES (?,?)', [identity, JSON.generate(work)]) } unless shape.end_with?('_turn')
      host.start
      Contract.wait(timeout: 85, description: "legacy deadline #{shape}") { definition(id)['runCount'] == 1 }
      expect(definition(id).fetch('runs').map { |run| run['id'] }).to eq([identity])
      case shape
      when 'queued_turn', 'queued_child'
        expect(wait_for_run(id, 'done')['result']).to eq('Verified comparison')
        expect(host.calls.length).to eq(2)
        expect(host.state['turns'].all? { |entry| entry['managed'].nil? }).to eq(true)
      when 'waiting_child'
        expect(wait_for_run(id, 'waiting')).to include('message' => 'Which products?')
        expect(host.calls).to be_empty
      else
        expect(wait_for_run(id, 'unresolved')['error']).not_to be_empty
        expect(host.calls).to be_empty
        if shape == 'terminal_turn'
          detail = host.api('GET', "/api/agents/#{owner}/routines/#{id}/runs/#{identity}")
          expect(detail.fetch('executionBinding')).to include('status' => 'legacy_unknown')
          expect(host.state['turns'].find { |entry| entry['id'] == identity }['output']).to eq('Unvalidated old reply')
        end
      end
    end
  end

  [false, true].each do |reject_deadline|
    it "#{reject_deadline ? 'rolls back' : 'retains'} group request admission with its deadline", :slow do
      owner = chief
      member = host.agent('Member')
      group = host.api('POST', '/api/groups', {name: 'Scheduled team', description: 'Compare products',
        lead: owner, members: [owner, member]}, status: 201)
      path = "/api/groups/#{group['id']}"
      host.api('POST', path + '/messages', {text: "Add New Scheduled Routine:\nFrequency: every 60 minutes\nExecute: Compare the products."}, status: 202)
      id = host.state['routines'].find { |row| row['owner'] == group['id'] }.fetch('id')
      saved = overdue(id)
      if reject_deadline
        database do |db|
          db.execute("CREATE TRIGGER reject_group_deadline BEFORE UPDATE ON scheduled_routines BEGIN SELECT RAISE(ABORT, 'fixture group deadline failure'); END")
        end
      end
      host.start
      Contract.wait(timeout: 85, description: 'group minute admission') { host.state.dig('pulse', 'numbers', 'bph60').to_i >= 1 }
      requests = host.api('GET', path).fetch('requests').select { |request| request.dig('origin', 'routine') == id }
      current = host.state['routines'].find { |row| row['id'] == id }
      if reject_deadline
        expect(requests).to be_empty
        expect(current).to include('nextDue' => saved['nextDue'], 'runCount' => 0)
        expect(host.calls).to be_empty
      else
        expect(requests.length).to eq(1)
        expect(current['runCount']).to eq(1)
        host.restart
        expect(host.api('GET', path).fetch('requests').select { |request| request.dig('origin', 'routine') == id }.map { |request| request['id'] }).to eq(requests.map { |request| request['id'] })
      end
    ensure
      database { |db| db.execute('DROP TRIGGER IF EXISTS reject_group_deadline') } if @host && reject_deadline
    end
  end
end
