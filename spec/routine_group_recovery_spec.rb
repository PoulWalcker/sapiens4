require_relative 'support/routine_group_recovery'

RSpec.describe 'Group routine routing after recovery', :desktop, :slow do
  include RoutineGroupRecovery

  it 'restores an admitted run at the current Lead while paused and skips deferred work before the next fresh deadline' do
    @host = desktop_host
    host.configure(mode: 'Exec')
    start_desktop
    chief_request('Create OldLead and CurrentLead as Research specialists.', [
      {control: {op: 'create_agent', name: 'OldLead', role: 'Research'}},
      {control: {op: 'create_agent', name: 'CurrentLead', role: 'Research'}}
    ])
    people = host.state.fetch('agents').to_h { |row| [row.fetch('name'), row.fetch('id')] }
    old_lead, current_lead = people.values_at('OldLead', 'CurrentLead')
    expect([old_lead, current_lead]).to all(be_a(String))
    chief_request('Create Recovery Group led by OldLead with CurrentLead as its other member.', [
      {control: {op: 'group_create', name: 'Recovery Group', description: 'Recover admitted work and skip stale deadlines.',
        lead: old_lead, members: [old_lead, current_lead]}}
    ])
    @group_id = host.state.fetch('groups').find { |row| row['name'] == 'Recovery Group' }.fetch('id')
    @journal = File.join(host.root, 'group-effects.jsonl')
    policy_file = File.join(host.root, 'group-policy.json')
    policy_receipt = File.join(host.root, 'group-policy-receipt.json')
    execution = 'GROUP_RECOVERY_EFFECT: record one Group observation. Skip missed deadlines; never replay stale observations.'
    host.configure(messages: [
      {contains: 'Configure the missed-deadline policy', node: 'group', rule: {
        block: true, gate: 'group-policy-ready', response: 'Saved skip_missed for this Group routine.',
        actions: [{controlFrom: policy_file, receipt: policy_receipt}]}},
      {contains: 'GROUP_RECOVERY_EFFECT:', node: 'group', rule: {
        response: 'Group recovery result: one current Lead observation recorded.',
        actions: [{appendContext: @journal, fields: %w[self_id request]}]}}
    ])
    desktop.click('Groups')
    desktop.click(/Open Recovery Group/)
    desktop.click('Work menu')
    desktop.click('Automation menu')
    desktop.click('Routines')
    desktop.click('+ New')
    desktop.value('Message Recovery Group', "Add New Scheduled Routine:\nFrequency: every XXX minutes\nExecute: PROMPT")
    group_message("Add New Scheduled Routine:\nFrequency: every 2 minutes\nExecute: #{execution}")
    routine = Contract.wait(description: 'Group routine saved through its conversation') do
      host.state.fetch('routines').find { |row| row['owner'] == @group_id }
    end
    @routine_id = routine.fetch('id')
    expect(routine.fetch('policySelection')).to eq('kind' => 'awaiting_selection')
    Contract.atomic_json(policy_file, {op: 'routine_policy', id: @routine_id, expectedPrompt: routine.fetch('prompt'),
      expectedSelection: routine.fetch('policySelection'), missedPolicy: 'skip_missed'})
    host.release('group-policy-ready')
    Contract.wait(description: 'model saves Group skip_missed policy') do
      group_routine['policySelection'] == {'kind' => 'selected', 'policy' => 'skip_missed'} && File.exist?(policy_receipt)
    end
    expect(JSON.parse(File.read(policy_receipt))).to include('saved' => true)
    host.idle
    desktop.see('Saved skip_missed for this Group routine.')
    group_message("Pause Scheduled Routine\n@#{@routine_id}")
    Contract.wait(description: 'Group schedule paused in UI') { group_routine['paused'] }
    desktop.click('Sapi settings')
    lead = desktop.find('Lead', role: %w[AXPopUpButton AXComboBox])
    desktop.click_node(lead)
    desktop.key('home')
    2.times { desktop.key('down') }
    desktop.key('return')
    Contract.wait(description: 'Lead selector shows CurrentLead before saving') do
      desktop.label(desktop.find('Lead', role: %w[AXPopUpButton AXComboBox])).include?('CurrentLead')
    end
    desktop.click('Save Group')
    Contract.wait(description: 'UI transfers Group leadership') { group_record['lead'] == current_lead }
    desktop.click('Sapi settings')
    desktop.click('Archive Group')
    desktop.see('Archived · history is preserved.')
    archived = group_record
    expect(archived).to include('archived' => true, 'lead' => current_lead)
    routine = group_routine
    expect(routine).to include('paused' => true, 'runCount' => 0)
    desktop.close
    @desktop = nil
    host.stop
    fixture = install_group_recovery_fixture(routine, old_lead)
    host.start
    @desktop = Gem.win_platform? ? Desktop.new(process: host.process) : Desktop.new(host.url)
    desktop.click('Open chat with Recovery Group')
    desktop.see('Archived · history is preserved.')
    desktop.click('Updates')
    desktop.see('routine.recovery_blocked')
    desktop.click('routine.recovery_blocked')
    desktop.see('restore this archived Group to continue the same Run')
    expect(group_record.fetch('requests').none? { |row| row['id'] == @admitted_id }).to be(true)
    expect(group_record.fetch('tasks').none? { |row| row['routine'] == @routine_id }).to be(true)
    expect(effect_entries).to be_empty
    pending = group_occurrences.find { |row| row[:id] == @admitted_id }
    expect(pending).to include(status: 'admitted_pending', value: JSON.parse(JSON.generate(fixture.fetch(:admitted))))
    desktop.click('Sapi settings')
    desktop.click('Restore Group')
    desktop.click('Chat')
    recovered = await_group_result(@admitted_id, description: 'paused admitted Group run executes and publishes at the current Lead')
    desktop.see('Group recovery result: one current Lead observation recorded.')
    expect(group_routine).to include('paused' => true, 'runCount' => 1)
    request = recovered.fetch('requests').find { |row| row['id'] == @admitted_id }
    expect(request).to include('target' => current_lead, 'task' => "gtask_#{@admitted_id}")
    expect(request.fetch('origin')).to include('group' => @group_id, 'routine' => @routine_id,
      'routineOwner' => @group_id, 'due' => fixture.fetch(:admitted_due), 'executionPrompt' => execution)
    expect(effect_entries.size).to eq(1)
    first_journal = File.binread(@journal)
    expect(group_occurrences.find { |row| row[:id] == @deferred_id }).to include(status: 'deferred')
    group_message("Resume Scheduled Routine\n@#{@routine_id}")
    resumed = Contract.wait(description: 'Group schedule resumed in UI') { row = group_routine; row unless row['paused'] }
    next_due = resumed.fetch('nextDue')
    # Pause before publication's pulse can admit the next deadline.
    fresh = Contract.wait(timeout: 200, description: 'old deferred range closes without effects and exactly one fresh deadline is admitted') do
      rows = group_record.fetch('requests').select { |row| row.dig('origin', 'routine') == @routine_id }
      rows.find { |row| row['id'] != @admitted_id }
    end
    group_message("Pause Scheduled Routine\n@#{@routine_id}")
    Contract.wait(description: 'fresh Group run schedule paused again') { group_routine['paused'] }
    recovered_task = recovered.fetch('tasks').find { |row| row['id'] == request.fetch('task') }
    expect(host.request('POST', "/api/groups/#{@group_id}/tasks/#{recovered_task.fetch('id')}/run",
      {revision: recovered_task.fetch('revision')}).first).to eq(409)
    expect(group_record.fetch('tasks').find { |row| row['id'] == recovered_task.fetch('id') }).to eq(recovered_task)
    finished = await_group_result(fresh.fetch('id'), description: 'fresh Group result publishes on the following minute pulse')
    desktop.see('Group recovery result: one current Lead observation recorded.')
    requests = finished.fetch('requests').select { |row| row.dig('origin', 'routine') == @routine_id }
    expected_runs = [@admitted_id, fresh.fetch('id')]
    expect(requests.map { |row| row['id'] }).to match_array(expected_runs)
    expect(requests.map { |row| row['target'] }).to eq([current_lead, current_lead])
    expect(fresh.fetch('origin')).to include('due' => next_due, 'executionPrompt' => execution, 'routineOwner' => @group_id)
    tasks = finished.fetch('tasks').select { |row| row['routine'] == @routine_id }
    expect(tasks.map { |row| row['id'] }).to match_array(expected_runs.map { |id| "gtask_#{id}" })
    tasks.each do |task|
      expect(task).to include('assignee' => current_lead, 'state' => 'done', 'taskType' => 'automated')
      expect(task.fetch('results').size).to eq(1)
      expect(task.fetch('results').first).to include('author' => current_lead, 'stale' => false)
    end
    expected_runs.each do |id|
      expect(finished.fetch('messages').count { |row| row['turn'] == id }).to eq(1)
    end
    rows = group_occurrences
    expect(rows.map { |row| row[:id] }).to match_array([@admitted_id, @deferred_id, fresh.fetch('id')])
    closed = rows.find { |row| row[:id] == @deferred_id }
    expect(closed).to include(status: 'closed')
    expect(closed.fetch(:value)).to include('closure' => {'kind' => 'skipped'})
    expect(Time.iso8601(closed.fetch(:value).fetch('through'))).to eq(Time.iso8601(fixture.fetch(:last_deferred)))
    expect(closed.fetch(:value).dig('routine', 'nextDue')).to eq(fixture.fetch(:first_deferred))
    expect(closed.fetch(:value)).not_to have_key('reason')
    expect(rows.find { |row| row[:id] == @admitted_id }.fetch(:value)).to eq(JSON.parse(JSON.generate(fixture.fetch(:admitted))))
    expect(rows.reject { |row| row[:id] == @deferred_id }.map { |row| row[:status] }).to eq(%w[admitted_ready admitted_ready])
    journal = effect_entries
    expect(File.binread(@journal)).to start_with(first_journal)
    expect(journal.map { |row| row.fetch('request').fetch('id') }).to match_array(expected_runs)
    expect(journal.map { |row| row.fetch('self_id') }).to eq([current_lead, current_lead])
    expect(journal.map { |row| row.fetch('request').fetch('origin').fetch('due') }).to eq([fixture.fetch(:admitted_due), next_due])
    expect(group_routine).to include('paused' => true, 'runCount' => 2)
    calls = host.calls.select { |row| expected_runs.include?(row['agency_run']) }
    expect(calls.size).to eq(2)
    expect(calls.map { |row| row.fetch('owner') }).to eq([current_lead, current_lead])
    desktop.click('Sapis')
    desktop.click('Open OldLead')
    desktop.see('Message OldLead')
    expect(desktop.text).not_to include('Group recovery result:', 'GROUP_RECOVERY_EFFECT:')
    desktop.click('Open CurrentLead')
    desktop.see('Message CurrentLead')
    expect(desktop.text).not_to include('Group recovery result:', 'GROUP_RECOVERY_EFFECT:')
    memo_calls_before_work = host.calls.count { |row| row['node'] == 'group-memo' }
    restarted = host.api('POST', "/api/groups/#{@group_id}/tasks/#{recovered_task.fetch('id')}/run",
      {revision: recovered_task.fetch('revision')}, status: 202)
    work_request = group_record.fetch('requests').last
    expect(work_request.fetch('origin')).to eq(request.fetch('origin'))
    expect(work_request).to include('target' => current_lead, 'task_revision' => restarted.fetch('revision'))
    work_message = group_record.fetch('messages').find { |row| row['id'] == work_request.fetch('message') }
    expect(work_message).not_to have_key('memo_pending')
    host.turn(work_request.fetch('id'), 'done', timeout: 150)
    Contract.wait(description: 'Work revision settles without requesting a Group memo') do
      group_record.fetch('requests').find { |row| row['id'] == work_request.fetch('id') }['settled']
    end
    work_calls = host.calls.select { |row| row['agency_run'] == work_request.fetch('id') }
    expect(work_calls.size).to eq(1)
    expect(work_calls.first).to include('node' => 'group', 'owner' => current_lead)
    expect(work_calls.first.fetch('prompt')).to include('scheduledExecutionPrompt', execution)
    expect(group_record.fetch('requests').select { |row| row['root'] == work_request.fetch('root') && row['memo'] }).to be_empty
    expect(host.calls.count { |row| row['node'] == 'group-memo' }).to eq(memo_calls_before_work)
    expect(group_routine).to include('paused' => true, 'runCount' => 2)
  end
end
