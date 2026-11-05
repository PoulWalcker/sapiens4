require_relative 'support/routine_crash_recovery'

RSpec.describe 'Routine recovery with unknown external outcomes', :desktop, :slow do
  include RoutineCrashRecovery

  it 'requires an explicit Personal retry after SIGKILL and lets the Admin dismiss the interrupted Group call without replaying effects' do
    skip 'SIGKILL and owned-process identity checks require POSIX; Windows crash recovery is not covered here' if Gem.win_platform?
    @host = desktop_host
    host.configure(mode: 'Exec')
    start_desktop
    crash_chief_request('Create CrashPersonal and CrashLead as Research specialists.', [
      {control: {op: 'create_agent', name: 'CrashPersonal', role: 'Research'}},
      {control: {op: 'create_agent', name: 'CrashLead', role: 'Research'}}
    ])
    people = host.state.fetch('agents').to_h { |row| [row.fetch('name'), row.fetch('id')] }
    personal, lead = people.values_at('CrashPersonal', 'CrashLead')
    expect([personal, lead]).to all(be_a(String))
    expect(personal).not_to eq(lead)
    crash_chief_request('Create Crash Group led by CrashLead with CrashPersonal as its other member.', [
      {control: {op: 'group_create', name: 'Crash Group', description: 'Inspect uncertain external effects before retrying.',
        lead: lead, members: [lead, personal]}}
    ])
    @crash_group_id = host.state.fetch('groups').find { |row| row['name'] == 'Crash Group' }.fetch('id')
    personal_prompt = 'CRASH_PERSONAL_EFFECT: append one durable external effect; never repeat an uncertain attempt automatically. Skip missed deadlines.'
    group_prompt = 'CRASH_GROUP_EFFECT: append one durable external effect; never repeat an uncertain attempt automatically. Skip missed deadlines.'
    crash_select_personal
    personal_routine = crash_save_policy('CrashPersonal', personal, personal_prompt, 'Execution')
    crash_select_group
    group_routine = crash_save_policy('Crash Group', @crash_group_id, group_prompt, 'group')
    personal_id, group_id = personal_routine.fetch('id'), group_routine.fetch('id')
    personal_journal = File.join(host.root, 'uncertain-personal-effects.jsonl')
    group_journal = File.join(host.root, 'uncertain-group-effects.jsonl')
    personal_reply = 'Explicit retry completed; review both external effect attempts.'
    host.configure(messages: [
      {contains: 'CRASH_PERSONAL_EFFECT:', node: 'Execution', rule: {actions: [
        {appendContext: personal_journal, fields: %w[origin originalIntent]}, {gate: 'personal-external-action-return'}
      ], response: decision('Outcome', reply: personal_reply, outcome: {status: 'Completed', criterionResults: [
        {id: 'comparison', result: 'Satisfied', evidence: 'The explicitly requested external attempt was recorded.'}], artifacts: []})}},
      {contains: 'CRASH_GROUP_EFFECT:', node: 'group', rule: {actions: [
        {appendContext: group_journal, fields: %w[self_id request]}, {gate: 'group-external-action-return'}
      ], response: 'Group external effect completed.'}}
    ])
    crash_select_personal
    crash_start_minute_schedule('CrashPersonal', personal_routine)
    crash_select_group
    crash_start_minute_schedule('Crash Group', group_routine)
    # These gates run AFTER the append and BEFORE any model reply. This is a
    # process crash with an unknown external outcome, not a saved-state fixture.
    Contract.wait(timeout: 150, description: 'both scheduled performers append an effect and wait before replying') do
      crash_entries(personal_journal).size == 1 && crash_entries(group_journal).size == 1
    end
    personal_turn = crash_turns(personal_id).fetch(0)
    group_turn = crash_turns(group_id).fetch(0)
    expect([personal_turn, group_turn]).to all(include('status' => 'running'))
    personal_run, group_run = personal_turn.fetch('id'), group_turn.fetch('id')
    expect(crash_task(personal, personal_run).fetch('execution')).to include('attempt' => 1)
    calls = host.calls.select { |call| [personal_run, group_run].include?(call['agency_run']) && %w[Execution group].include?(call['node']) }
    expect(calls.size).to eq(2)
    expect(calls.map { |call| call.fetch('owner') }).to match_array([personal, lead])
    expect(calls).to all(satisfy { |call| !call.key?('finished') })
    calls.each { |call| crash_own_provider(call) }
    crash_select_personal
    crash_pause('CrashPersonal', personal_id)
    crash_select_group
    crash_pause('Crash Group', group_id)
    expect(crash_turns(personal_id).first).to include('status' => 'running')
    expect(crash_turns(group_id).first).to include('status' => 'running')
    admissions = [personal_run, group_run].to_h { |id| [id, crash_occurrence(id)] }
    expect(admissions.values).to all(satisfy { |row| row && row[1] == 'admitted_ready' })
    previous_personal = File.binread(personal_journal)
    previous_group = File.binread(group_journal)
    personal_effect = crash_entries(personal_journal).fetch(0)
    expect(personal_effect).to include('originalIntent' => personal_prompt,
      'origin' => include('routine' => personal_id, 'due' => personal_turn.fetch('origin').fetch('due')))
    expect(crash_entries(group_journal)).to contain_exactly(include('self_id' => lead,
      'request' => include('id' => group_run, 'origin' => group_turn.fetch('origin'))))
    desktop.close
    @desktop = nil
    host.process.signal('KILL')
    # Kill only the exact fixture processes captured above; otherwise their
    # inherited pipes can keep the dead host's stdout readers alive.
    crash_cleanup_providers
    host.crash
    host.start
    start_desktop

    recovered = host.turn(personal_run, 'interrupted')
    expect(recovered).to include('origin' => personal_turn.fetch('origin'))
    expect(recovered.fetch('error')).to include('inspect effects before retrying')
    task = crash_task(personal, personal_run)
    expect(task.fetch('execution')).to include('state' => 'Interrupted', 'currentCall' => personal_run, 'attempt' => 1)
    expect(task['outcome']).to be_nil
    interrupted_group = Contract.wait(description: 'Group reconciles interruption without accepting the unknown external outcome') do
      group = crash_group
      request = group.fetch('requests').find { |row| row['id'] == group_run }
      group if request && request['status'] == 'interrupted'
    end
    group_request = interrupted_group.fetch('requests').find { |row| row['id'] == group_run }
    expect(group_request.fetch('origin')).to eq(group_turn.fetch('origin'))
    group_task = interrupted_group.fetch('tasks').find { |row| row['id'] == group_request.fetch('task') }
    expect(group_task).to include('state' => 'backlog', 'assignee' => lead)
    expect(group_task.fetch('results')).to contain_exactly(include('turn' => group_run, 'text' => nil,
      'error' => a_string_including('inspect effects before retrying')))
    expect(interrupted_group.fetch('messages').none? { |row| row['turn'] == group_run }).to be(true)
    crash_select_personal
    crash_click_visible('Interrupted')
    desktop.see('inspect effects before retrying')
    crash_select_group
    desktop.see('CrashLead · Interrupted')
    pulse = host.state.fetch('pulse').fetch('numbers').fetch('bph60')
    Contract.wait(timeout: 85, description: 'one real minute pulse leaves both unknown external outcomes interrupted') do
      host.state.fetch('pulse').fetch('numbers').fetch('bph60') > pulse
    end
    expect(crash_turns(personal_id)).to contain_exactly(include('id' => personal_run, 'status' => 'interrupted'))
    expect(crash_turns(group_id)).to contain_exactly(include('id' => group_run, 'status' => 'interrupted'))
    expect(File.binread(personal_journal)).to eq(previous_personal)
    expect(File.binread(group_journal)).to eq(previous_group)
    expect(host.calls.select { |call| [personal_run, group_run].include?(call['agency_run']) && %w[Execution group].include?(call['node']) }.size).to eq(2)

    crash_click_visible('Dismiss')
    Contract.wait(description: 'Admin explicitly dismisses only the interrupted Group call') do
      crash_group.fetch('requests').find { |row| row['id'] == group_run }['status'] == 'cancelled'
    end
    crash_select_personal
    # The warning disclosure remains expanded across conversation navigation.
    crash_click_visible('Retry')
    Contract.wait(timeout: 85, description: 'explicit Personal retry reaches the external action gate as attempt two') do
      crash_entries(personal_journal).size == 2 && crash_task(personal, personal_run).fetch('execution')['attempt'] == 2
    end
    expect(crash_turns(personal_id).first).to include('id' => personal_run, 'origin' => personal_turn.fetch('origin'))
    retry_calls = host.calls.select { |call| call['agency_run'] == personal_run && call['node'] == 'Execution' }
    expect(retry_calls.size).to eq(2)
    expect(retry_calls.last.fetch('started')).to be > calls.find { |call| call['owner'] == personal }.fetch('started')
    host.release('personal-external-action-return')
    host.turn(personal_run, 'done', timeout: 85)
    desktop.see(personal_reply)
    completed = crash_task(personal, personal_run)
    expect(completed).to include('workloadId' => task.fetch('workloadId'))
    expect(completed.fetch('execution')).to include('state' => 'Completed', 'currentCall' => personal_run, 'attempt' => 2)
    expect(completed.fetch('decisions').count { |row| row['accepted'] && row.dig('response', 'event') == 'Outcome' }).to eq(1)
    expect(completed.fetch('calls').map { |row| row['callId'] }).to eq(task.fetch('calls').map { |row| row['callId'] })
    expect(crash_turns(personal_id).map { |row| row['id'] }).to eq([personal_run])
    expect(crash_entries(personal_journal)).to eq([personal_effect] * 2)
    expect(File.binread(personal_journal)).to start_with(previous_personal)
    expect(File.binread(group_journal)).to eq(previous_group)
    final_calls = host.calls.select { |call| [personal_run, group_run].include?(call['agency_run']) && %w[Execution group].include?(call['node']) }
    expect(final_calls.map { |call| call.fetch('agency_run') }).to match_array([personal_run, personal_run, group_run])
    expect(final_calls.count { |call| call.key?('finished') }).to eq(1)
    [personal_id, group_id].each { |id| expect(crash_routine(id)).to include('paused' => true, 'runCount' => 1) }
    admissions.each { |id, admission| expect(crash_occurrence(id)).to eq(admission) }
    final_group = crash_group
    expect(final_group.fetch('requests').select { |row| row.dig('origin', 'routine') == group_id })
      .to contain_exactly(include('id' => group_run, 'status' => 'cancelled', 'target' => lead))
    expect(final_group.fetch('tasks').find { |row| row['id'] == group_task.fetch('id') }).to eq(group_task)
    expect(final_group.fetch('messages')).to include(*interrupted_group.fetch('messages'))
    crash_select_group
    expect(desktop.text).not_to include('Group external effect completed.')
  ensure
    crash_cleanup_providers
  end
end
