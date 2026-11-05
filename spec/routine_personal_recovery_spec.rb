require_relative 'support/desktop'
require 'sqlite3'
require 'digest'
require 'time'

RSpec.describe 'Personal routine historical recovery', :desktop, :slow do
  def recovery_outcome(reply)
    decision('Outcome', reply: reply, outcome: {status: 'Completed', criterionResults: [
      {id: 'history', result: 'Satisfied', evidence: 'The requested journal operation completed.'}], artifacts: []})
  end

  def recovery_specification(objective)
    decision('TaskPrepared', taskType: 'research', specification: {
      objective: objective, inputs: [], expectedOutputs: ['A persisted journal operation'], completionCriteria: [
        {id: 'history', requirement: 'Complete the requested journal operation.', applicability: {kind: 'Required'}}]})
  end

  def recovery_context(call)
    JSON.parse(call.fetch('prompt').split("\nContext:\n", 2).last)
  end

  def recovery_executions(routine_id)
    host.calls.select do |call|
      call['node'] == 'Execution' && recovery_context(call).dig('origin', 'routine') == routine_id
    end
  end

  def recovery_runs(routine_id)
    host.state.fetch('turns').select { |turn| turn.dig('origin', 'routine') == routine_id }
      .sort_by { |turn| Time.iso8601(turn.fetch('origin').fetch('due')) }
  end

  def recovery_routine(routine_id)
    host.state.fetch('routines').find { |routine| routine['id'] == routine_id }
  end

  def recovery_journal(path)
    File.exist?(path) ? File.readlines(path).map { |line| JSON.parse(line) } : []
  end

  def recovery_schedules
    desktop.click('Work menu')
    desktop.click('Automation menu')
    desktop.click('Routines')
  end

  def recovery_restart
    desktop.close
    @desktop = nil
    host.restart
    @desktop = Gem.win_platform? ? Desktop.new(process: host.process) : Desktop.new(host.url)
    desktop.click('Open PersonalHistorian')
  end

  def recovery_identity(routine_id, due)
    namespace = ['6ba7b8119dad11d180b400c04fd430c8'].pack('H*')
    bytes = Digest::SHA1.digest(namespace + "#{routine_id}:#{due}").bytes.first(16)
    bytes[6] = (bytes[6] & 15) | 80
    bytes[8] = (bytes[8] & 63) | 128
    bytes.pack('C*').unpack1('H*')
  end

  def install_personal_recovery(routine, owner, deadlines, next_due, observed)
    raise 'Recovery installation fixture requires a stopped host' if host.process
    identities = deadlines.map { |due| recovery_identity(routine.fetch('id'), due) }
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      raise 'Recovery fixture only supports current schema9' unless db.get_first_value('PRAGMA user_version') == 9
      db.transaction do
        current = routine.merge('nextDue' => next_due, 'runCount' => 1)
        db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?',
          [JSON.generate(current), routine.fetch('id').split('_').last.to_i])
        deadlines.each_with_index do |due, index|
          value = {'id' => identities[index], 'routine' => routine.merge('nextDue' => due),
            'target' => {'kind' => 'personal', 'agent' => owner}, 'created' => observed}
          unless index.zero?
            value.merge!('through' => due, 'reason' => {'kind' => 'active_run', 'run' => identities.first})
          end
          db.execute('INSERT INTO routine_occurrences(id,value,status) VALUES (?,?,?)',
            [identities[index], JSON.generate(value), index.zero? ? 'admitted_pending' : 'deferred'])
        end
      end
    end
    identities
  end

  it 'recovers each historical deadline once and keeps admitted instructions immutable across edits and restarts' do
    @host = desktop_host
    old_prompt = 'Historical journal version OLD: append the original scheduled deadline to personal-history.jsonl and report OLD.'
    new_prompt = 'Historical journal version NEW: append the original scheduled deadline to personal-history.jsonl and report NEW.'
    request = "Create PersonalHistorian, a historical journal specialist. Have it create a personal routine every 30 minutes. " \
      "Recover every missed deadline chronologically, using its original scheduled time. Its execution instructions are: #{old_prompt}"
    host.configure(mode: 'Exec', rules: {
      "#{chief}:Assessing" => {response: decision('OutsideSpecialization')},
      'ChiefTriage' => {response: decision('NewSpecialistNeeded')},
      'CreatingSapi' => {response: decision('SapiSpecified', name: 'PersonalHistorian', role: 'Historical journal specialist')},
      'Delegation' => {response: decision('HandoffPrepared', request: request)},
      'ReadyToWork' => {response: recovery_specification('Create the requested personal historical journal routine.')},
      'Execution' => {actions: [{control: {op: 'routine_create', minutes: 30, prompt: old_prompt,
        missedPolicy: 'recover_each'}, receipt: 'setup-receipt.json'}], response: recovery_outcome('Historical journal routine saved.')}
    })
    start_desktop
    desktop.fill('Message ChiefSapi', request)
    desktop.click('Send message')
    saved = Contract.wait(timeout: 85, description: 'Chief delegates and specialist persists its historical routine') do
      snapshot = host.state
      owner = snapshot.fetch('agents').find { |agent| agent['name'] == 'PersonalHistorian' }
      routine = snapshot.fetch('routines').find { |item| item['owner'] == owner&.fetch('id') }
      [owner.fetch('id'), routine] if owner && routine
    end
    owner, routine = saved
    expect(routine).to include('prompt' => old_prompt, 'minutes' => 30,
      'policySelection' => {'kind' => 'selected', 'policy' => 'recover_each'})
    Contract.wait(timeout: 85, description: 'routine setup result published before installing recovery fixture') do
      snapshot = host.state
      snapshot.fetch('turns').none? { |turn| %w[queued running output_pending].include?(turn['status']) } &&
        snapshot.fetch('workloads').all? { |work| work['state'] == 'Completed' }
    end
    setup_call = host.calls.find { |call| call['owner'] == owner && call['node'] == 'Execution' }
    journal = File.join(setup_call.fetch('cwd'), 'personal-history.jsonl')
    expect(recovery_runs(routine.fetch('id'))).to be_empty
    desktop.close
    @desktop = nil
    host.stop

    # Saved installation: one admitted snapshot and two deferred deadlines.
    anchor = Time.at((Time.now.to_i / 60) * 60).utc
    deadlines = [-75, -45, -15].map { |minutes| (anchor + minutes * 60).iso8601(6).sub('Z', '+00:00') }
    future_due = (anchor + 15 * 60).iso8601(6).sub('Z', '+00:00')
    identities = install_personal_recovery(routine, owner, deadlines, future_due, anchor.iso8601(6).sub('Z', '+00:00'))
    host.configure(messages: [
      {contains: old_prompt, node: 'Execution', rule: {response: recovery_outcome('Recovered OLD historical deadline.')}},
      {contains: new_prompt, node: 'Execution', rule: {response: recovery_outcome('Recovered NEW historical deadline.')}}
    ], rules: {
      'ReadyToWork' => {block: true, gate: 'personal-prompt-edited',
        response: recovery_specification('Append exactly one historical deadline to the journal.')},
      'Execution' => {actions: [{appendContext: journal, fields: ['origin', 'originalIntent']}]}
    })
    host.start
    start_desktop
    desktop.click('Open PersonalHistorian')
    recovery_schedules
    desktop.click('Edit routine')
    edit = "Edit @#{routine.fetch('id')} ( #{routine.fetch('title')} )\n" \
      "Edit Frequency (prev every 30 minutes): new every 30 minutes\nEdit Execution Prompt as: #{new_prompt}"
    desktop.fill('Message PersonalHistorian', edit)
    desktop.click('Send message')
    Contract.wait(description: 'current routine prompt edited before first recovered execution') do
      recovery_routine(routine.fetch('id')).fetch('prompt') == new_prompt
    end
    expect(recovery_executions(routine.fetch('id'))).to be_empty
    host.release('personal-prompt-edited')

    runs = Contract.wait(timeout: 420, description: 'three historical results published on real minute pulses') do
      current = recovery_runs(routine.fetch('id'))
      failed = current.find { |turn| %w[failed interrupted cancelled].include?(turn['status']) }
      raise "Historical recovery failed: #{failed.inspect}" if failed
      current if current.length == 3 && current.all? { |turn| turn['status'] == 'done' }
    end
    executions = recovery_executions(routine.fetch('id'))
    expected_prompts = [old_prompt, new_prompt, new_prompt]
    expect(runs.map { |turn| turn['id'] }).to eq(identities)
    expect(runs.map { |turn| turn.fetch('origin').fetch('due') }).to eq(deadlines)
    expect(runs.map { |turn| turn['input'] }).to eq(expected_prompts)
    expect(runs.map { |turn| turn['output'] }).to eq([
      'Recovered OLD historical deadline.', 'Recovered NEW historical deadline.', 'Recovered NEW historical deadline.'
    ])
    expect(runs).to all(include('agent' => owner, 'origin' => include('routineOwner' => owner, 'taskType' => 'automated')))
    expect(executions.map { |call| recovery_context(call).fetch('originalIntent') }).to eq(expected_prompts)
    entries = recovery_journal(journal)
    expect(entries.map { |entry| entry.fetch('origin').fetch('due') }).to eq(deadlines)
    expect(entries.map { |entry| entry.fetch('originalIntent') }).to eq(expected_prompts)
    expect(entries.map { |entry| entry.fetch('origin').fetch('executionPrompt') }).to eq(expected_prompts)
    expect(entries).to all(include('origin' => include('routine' => routine.fetch('id'), 'routineOwner' => owner, 'taskType' => 'automated')))
    runs.each_cons(2).with_index do |(previous, _following), index|
      expect(executions.fetch(index + 1).fetch('started')).to be >= Time.iso8601(previous.fetch('output_at')).to_f
    end
    tasks = host.api('GET', "/api/agents/#{owner}/tasks?format=json").fetch('tasks')
      .select { |task| identities.include?(task['id']) }
    expect(tasks.size).to eq(3)
    expect(tasks).to all(include('owner' => owner, 'taskType' => 'automated', 'state' => 'Completed'))
    expect(recovery_routine(routine.fetch('id'))).to include('runCount' => 3)
    expect(recovery_routine(routine.fetch('id')).fetch('deferredCount', 0)).to eq(0)
    desktop.see('Recovered NEW historical deadline.')

    recovery_schedules
    desktop.click('Pause routine')
    desktop.value('Message PersonalHistorian', "Pause Scheduled Routine\n@#{routine.fetch('id')}")
    desktop.click('Send message')
    Contract.wait(description: 'pause command submitted through personal conversation') do
      recovery_routine(routine.fetch('id')).fetch('paused')
    end
    before = recovery_journal(journal)
    recovery_restart
    pulse_number = host.state.fetch('pulse').fetch('numbers').fetch('bph60')
    Contract.wait(timeout: 85, description: 'subsequent real minute pulse after the second restart') do
      host.state.fetch('pulse').fetch('numbers').fetch('bph60') > pulse_number
    end
    expect(recovery_runs(routine.fetch('id'))).to eq(runs)
    expect(recovery_executions(routine.fetch('id')).size).to eq(3)
    expect(recovery_journal(journal)).to eq(before)
    expect(recovery_routine(routine.fetch('id'))).to include('paused' => true, 'runCount' => 3)
  end
end
