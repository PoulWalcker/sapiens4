require 'sqlite3'
require 'digest'
require 'time'
require 'securerandom'

RSpec.describe 'Routine retry admission', :host do
  def retry_occurrence_id(routine_id, due)
    namespace = ['6ba7b8119dad11d180b400c04fd430c8'].pack('H*')
    bytes = Digest::SHA1.digest(namespace + "#{routine_id}:#{due}").bytes.first(16)
    bytes[6] = (bytes[6] & 15) | 80
    bytes[8] = (bytes[8] & 63) | 128
    bytes.pack('C*').unpack1('H*')
  end

  it 'rejects retry while another occurrence is queued or running, and permits it after that occurrence stops', :slow do
    owner = chief
    routine = host.control(owner, 'routine_create', minutes: 60, prompt: 'Return a sourced comparison.',
      missedPolicy: 'run_latest').fetch('routine')
    paused = host.control(owner, 'routine_pause', id: routine.fetch('id')).fetch('routine')
    host.configure(rules: {'Execution' => {block: true, gate: 'routine-execution'}})
    host.stop

    # Saved installation: an older failed Run and a newer queued Run.
    anchor = Time.now.utc
    created = anchor.iso8601(6).sub('Z', '+00:00')
    snapshots = [120, 60].map { |minutes| routine.merge('nextDue' => (anchor - minutes * 60).iso8601(6).sub('Z', '+00:00')) }
    older, newer = snapshots.map { |snapshot| retry_occurrence_id(snapshot.fetch('id'), snapshot.fetch('nextDue')) }
    unrelated = SecureRandom.hex(16)
    turns = snapshots.zip([older, newer], %w[failed queued]).map do |snapshot, id, status|
      {id: id, flow: 'chat', input: snapshot.fetch('prompt'), status: status, attempt: 1,
        created: created, batchable: true, error: status == 'failed' ? 'Previous execution failed' : nil,
        origin: {routine: snapshot.fetch('id'), routineOwner: owner, due: snapshot.fetch('nextDue'),
          title: snapshot.fetch('title'), minutes: 60, taskType: 'automated', executionPrompt: snapshot.fetch('prompt')}}
    end
    turns << {id: unrelated, flow: 'chat', input: 'Independent work.', status: 'failed', attempt: 1,
      created: created, batchable: true, origin: nil, error: 'Previous independent execution failed'}
    state_path = File.join(host.root, 'agentpy', 'agents', owner, 'state.json')
    state = JSON.parse(File.read(state_path))
    state['turns'].concat(turns)
    Contract.atomic_json(state_path, state)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.transaction do
        db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?',
          [JSON.generate(paused.merge('runCount' => 2)), routine.fetch('id').split('_').last.to_i])
        snapshots.zip([older, newer]).each do |snapshot, id|
          value = {id: id, routine: snapshot, target: {kind: 'personal', agent: owner}, created: created}
          db.execute("INSERT INTO routine_occurrences(id,value,status) VALUES (?,?,'admitted_ready')", [id, JSON.generate(value)])
        end
      end
    end
    host.start
    saved = JSON.parse(File.read(state_path)).fetch('turns').find { |turn| turn['id'] == older }
    retry_path = "/api/agents/#{owner}/turns/#{older}/retry"
    expect(host.request('POST', retry_path, {}).first).to eq(409)
    expect(JSON.parse(File.read(state_path)).fetch('turns').find { |turn| turn['id'] == older }).to eq(saved)

    host.api('POST', "/api/agents/#{owner}/turns/#{unrelated}/retry", {})
    host.turn(unrelated)
    Contract.wait(timeout: 85, description: 'newer Routine occurrence is executing at the provider gate') do
      host.calls.any? { |call| call['agency_run'] == newer && call['node'] == 'Execution' }
    end
    host.turn(newer, 'running')
    calls = host.calls
    expect(host.request('POST', retry_path, {}).first).to eq(409)
    expect(host.turn(older, 'failed')).to include('input' => saved.fetch('input'), 'error' => saved.fetch('error'))
    expect(host.calls).to eq(calls)
    expect(JSON.parse(File.read(state_path)).fetch('turns').find { |turn| turn['id'] == older }).to eq(saved)

    host.api('POST', "/api/agents/#{owner}/turns/#{newer}/cancel", {})
    host.turn(newer, 'interrupted')
    stopped_at = Time.now.to_f
    host.api('POST', retry_path, {})
    queued = JSON.parse(File.read(state_path)).fetch('turns').find { |turn| turn['id'] == older }
    expect(queued).to include('id' => older, 'status' => 'queued', 'attempt' => 2, 'origin' => saved.fetch('origin'))
    retried = Contract.wait(timeout: 85, description: 'explicit retry executes after the newer occurrence has stopped') do
      host.calls.find { |call| call['agency_run'] == older && call['node'] == 'Execution' }
    end
    expect(retried.fetch('started')).to be >= stopped_at
    expect(host.state.fetch('routines').find { |item| item['id'] == routine.fetch('id') }).to include('runCount' => 2, 'paused' => true)
    expect(host.state.fetch('turns').count { |turn| turn.dig('origin', 'routine') == routine.fetch('id') }).to eq(2)
    host.api('POST', "/api/agents/#{owner}/turns/#{older}/cancel", {})
    host.turn(older, 'interrupted')
  end

  it 'rejects direct agent-turn retry of a failed Group Routine task without changing its history', :aggregate_failures do
    owner, member = chief, host.agent('GroupMember')
    group = host.api('POST', '/api/groups', {name: 'Retry Group', description: 'Recover scheduled group work.',
      lead: owner, members: [owner, member]}, status: 201)
    other_group = host.api('POST', '/api/groups', {name: 'Other Retry Group', description: 'Separate routine owner.',
      lead: owner, members: [owner, member]}, status: 201)
    host.stop

    # Saved terminal Group Run with a settled request and failure result.
    anchor = Time.now.utc
    created = anchor.iso8601(6).sub('Z', '+00:00')
    routine = {'id' => 'routine_sch_00001', 'owner' => group.fetch('id'), 'title' => 'Scheduled comparison',
      'prompt' => 'Return a sourced comparison.', 'minutes' => 60, 'nextDue' => (anchor + 3600).iso8601(6).sub('Z', '+00:00'),
      'created' => created, 'runCount' => 1, 'paused' => true,
      'policySelection' => {'kind' => 'selected', 'policy' => 'run_latest'}}
    snapshot = routine.merge('nextDue' => (anchor - 3600).iso8601(6).sub('Z', '+00:00'), 'paused' => false, 'runCount' => 0)
    id = retry_occurrence_id(routine.fetch('id'), snapshot.fetch('nextDue'))
    task_id = "gtask_#{id}"
    error = 'Previous Group execution failed'
    origin = {routine: routine.fetch('id'), routineOwner: group.fetch('id'), due: snapshot.fetch('nextDue'),
      title: routine.fetch('title'), minutes: 60, taskType: 'automated', executionPrompt: routine.fetch('prompt'),
      group: group.fetch('id'), author: 'admin'}
    turn = {id: id, flow: 'chat', input: routine.fetch('prompt'), status: 'failed', attempt: 1,
      created: created, batchable: true, error: error, origin: origin}
    request = {id: id, message: id, root: id, target: owner, status: 'failed', settled: true,
      task: task_id, task_revision: 1, created: created, error: error, origin: origin}
    task = {id: task_id, title: routine.fetch('title'), body: routine.fetch('prompt'), assignee: owner,
      state: 'backlog', deleted: false, revision: 2, taskType: 'automated', routine: routine.fetch('id'),
      created: created, author: 'admin', updated_by: owner, history: [], results: [
        {turn: id, author: owner, text: nil, error: error, revision: 1, stale: false, created: created}]}
    group.fetch('requests') << request
    group.fetch('tasks') << task
    group.fetch('messages') << {id: id, author: 'admin', text: routine.fetch('prompt'), created: created, routine: routine.fetch('id')}
    group.fetch('events') << {id: SecureRandom.hex(16), actor: owner, action: 'call failed', detail: id, created: created}
    state_path = File.join(host.root, 'agentpy', 'agents', owner, 'state.json')
    state = JSON.parse(File.read(state_path))
    state.fetch('turns') << turn
    Contract.atomic_json(state_path, state)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.transaction do
        db.execute('INSERT INTO scheduled_routines(sequence,value) VALUES (1,?)', [JSON.generate(routine)])
        occurrence = {id: id, routine: snapshot, target: {kind: 'group', group: group.fetch('id'), agent: owner}, created: created}
        db.execute("INSERT INTO routine_occurrences(id,value,status) VALUES (?,?,'admitted_ready')", [id, JSON.generate(occurrence)])
        db.execute('UPDATE groups SET value=? WHERE id=?', [JSON.generate(group), group.fetch('id')])
      end
    end
    host.start
    saved_group = host.api('GET', "/api/groups/#{group.fetch('id')}")
    saved_turn = JSON.parse(File.read(state_path)).fetch('turns').find { |item| item['id'] == id }
    saved_events = host.state.fetch('events')
    response = host.request('POST', "/api/agents/#{owner}/turns/#{id}/retry", {})
    expect(response.first).to eq(409)
    expect(response[1].fetch('error')).to eq('Start the current task revision from Work instead')
    expect(JSON.parse(File.read(state_path)).fetch('turns').find { |item| item['id'] == id }).to eq(saved_turn)
    expect(host.api('GET', "/api/groups/#{group.fetch('id')}")).to eq(saved_group)
    expect(host.state.fetch('events')).to eq(saved_events)
    expect(host.state.fetch('routines')).to eq([routine])
    expect(host.calls).to be_empty

    host.configure(block: true)
    before_work_state = JSON.parse(File.read(state_path))
    revised = host.api('PUT', "/api/groups/#{group.fetch('id')}/tasks/#{task_id}",
      {revision: 2, assignee: member, state: 'in_progress', body: 'Execute this revised Work instruction.'})
    restarted = host.api('POST', "/api/groups/#{group.fetch('id')}/tasks/#{task_id}/run", {revision: revised.fetch('revision')}, status: 202)
    restarted_group = host.api('GET', "/api/groups/#{group.fetch('id')}")
    restarted_request = restarted_group.fetch('requests').last
    aggregate_failures('Work restart provenance') do
      expect(restarted_request['origin']).to eq(JSON.parse(JSON.generate(origin)).merge('executionPrompt' => revised.fetch('body')))
      expect(restarted_request).to include('target' => member, 'task' => task_id, 'task_revision' => restarted.fetch('revision'))
      expect(restarted_group.fetch('requests').first).to eq(saved_group.fetch('requests').first)
    end
    host.stop
    Contract.atomic_json(state_path, before_work_state)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.execute('UPDATE groups SET value=? WHERE id=?', [JSON.generate(saved_group), group.fetch('id')])
    end
    host.start

    aggregate_failures('invalid Group provenance') do
      [
        ['different Group owner', :owner],
        ['missing Group context and request', :context],
        ['task belongs to another Routine', :task]
      ].each do |name, damage|
        host.stop
        damaged_group = JSON.parse(JSON.generate(saved_group))
        damaged_turn = JSON.parse(JSON.generate(saved_turn))
        if damage == :owner
          damaged_turn.fetch('origin')['routineOwner'] = other_group.fetch('id')
          damaged_group.fetch('requests').find { |item| item['id'] == id }.fetch('origin')['routineOwner'] = other_group.fetch('id')
        elsif damage == :context
          damaged_turn.fetch('origin').delete('group')
          damaged_group.fetch('requests').reject! { |item| item['id'] == id }
        else
          damaged_group.fetch('tasks').first['routine'] = 'routine_sch_00002'
        end
        state = JSON.parse(File.read(state_path))
        state['turns'] = state.fetch('turns').map { |item| item['id'] == id ? damaged_turn : item }
        Contract.atomic_json(state_path, state)
        SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
          db.execute('UPDATE groups SET value=? WHERE id=?', [JSON.generate(damaged_group), group.fetch('id')])
        end
        host.start
        before_group = host.api('GET', "/api/groups/#{group.fetch('id')}")
        before_state = JSON.parse(File.read(state_path))
        before_events = host.state.fetch('events')
        before_calls = host.calls
        aggregate_failures(name) do
          expect(host.request('POST', "/api/agents/#{owner}/turns/#{id}/retry", {}).first).to eq(500)
          expect(host.request('POST', "/api/groups/#{group.fetch('id')}/tasks/#{task_id}/run", {revision: 2}).first).to eq(500)
          expect(JSON.parse(File.read(state_path))).to eq(before_state)
          expect(host.api('GET', "/api/groups/#{group.fetch('id')}")).to eq(before_group)
          expect(host.state.fetch('events')).to eq(before_events)
          expect(host.state.fetch('routines')).to eq([routine])
          expect(host.calls).to eq(before_calls)
        end
        if damage == :task
          host.stop
          queued_group = JSON.parse(JSON.generate(before_group))
          queued_group.fetch('requests').first.merge!('status' => 'queued', 'settled' => false)
          SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
            db.execute('UPDATE groups SET value=? WHERE id=?', [JSON.generate(queued_group), group.fetch('id')])
          end
          host.start
          rejected = Contract.wait(description: 'inconsistent Routine request is quarantined without settling another Task') do
            latest = host.api('GET', "/api/groups/#{group.fetch('id')}")
            latest if latest.fetch('requests').first['settled']
          end
          expect(rejected.fetch('requests').first).to include('status' => 'failed')
          expect(rejected.fetch('tasks')).to eq(before_group.fetch('tasks'))
          expect(host.calls).to eq(before_calls)
        end
      end
    end
  end
end
