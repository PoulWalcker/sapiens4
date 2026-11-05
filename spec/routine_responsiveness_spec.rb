require_relative 'support/desktop'
require 'time'

RSpec.describe 'Routine responsiveness and pending deadlines', :desktop, :slow do
  def send_routine_message(name, text)
    desktop.fill("Message #{name}", text)
    desktop.click('Send message')
  end

  def select_routine_owner(name)
    desktop.control('activate')
    desktop.click("Open #{name}")
    desktop.see("Message #{name}")
  end

  def show_routines(period = 'Planned')
    desktop.click('Work menu')
    desktop.click('Automation menu')
    desktop.click('Routines')
    if period == 'Paused'
      desktop.click('Routine history')
      desktop.click('Paused')
    end
  end

  def saved_routine(id, snapshot = host.state)
    snapshot.fetch('routines').find { |routine| routine['id'] == id } || raise("Missing routine #{id}")
  end

  def routine_turns(id, snapshot = host.state)
    snapshot.fetch('turns').select { |turn| turn.dig('origin', 'routine') == id }
  end

  def routine_context(call)
    JSON.parse(call.fetch('prompt').split("\nContext:\n", 2).last)
  end

  def routine_executions(owner)
    host.calls.select { |call| call['owner'] == owner && call['node'] == 'Execution' && routine_context(call).dig('origin', 'routine') }
  end

  def create_routine_specialist(name, prompt, sources)
    request = "Create a #{name} specialist and delegate setup to it. It must save its own routine every 10 minutes, " \
      "using run_latest for missed deadlines. Its per-occurrence instructions are:\n#{prompt}"
    host.configure(mode: 'Exec', rules: {
      "#{chief}:Assessing" => {response: decision('OutsideSpecialization')},
      'ChiefTriage' => {response: decision('NewSpecialistNeeded')},
      'CreatingSapi' => {response: decision('SapiSpecified', name: name, role: 'Local inventory reporting')},
      'Delegation' => {response: decision('HandoffPrepared', request: request)},
      'Execution' => {actions: sources.map { |path, content| {write: path, content: content} } + [
        {control: {op: 'routine_create', minutes: 10, prompt: prompt, missedPolicy: 'run_latest'}, receipt: 'routine-receipt.json'}
      ]}
    })
    select_routine_owner('ChiefSapi')
    send_routine_message('ChiefSapi', request)
    snapshot = Contract.wait(timeout: 30, description: "#{name} saves its delegated routine") do
      current = host.state
      owner = current.fetch('agents').find { |agent| agent['name'] == name }
      current if owner && current.fetch('routines').any? { |routine| routine['owner'] == owner['id'] }
    end
    host.idle
    owner = snapshot.fetch('agents').find { |agent| agent['name'] == name }.fetch('id')
    routine = snapshot.fetch('routines').find { |item| item['owner'] == owner }
    workspace = host.calls.find { |call| call['owner'] == owner && call['node'] == 'Execution' }.fetch('cwd')
    receipt = JSON.parse(File.read(File.join(workspace, 'routine-receipt.json')))
    expect(receipt.fetch('routine')).to include('id' => routine.fetch('id'), 'owner' => owner, 'prompt' => prompt)
    [owner, routine, workspace]
  end

  def edit_routine(name, id, minutes, prompt, previous_minutes:, period: 'Planned')
    show_routines(period)
    desktop.click('Edit routine')
    template = desktop.find("Message #{name}").fetch('value')
    expect(template).to include("Edit @#{id}", "prev every #{previous_minutes} minutes", 'new every XXX minutes')
    text = "Edit @#{id} ( Inventory report )\n" \
      "Edit Frequency (prev every #{previous_minutes} minutes): new every #{minutes} minutes\n" \
      "Edit Execution Prompt as: #{prompt}"
    send_routine_message(name, text)
    Contract.wait(description: "#{id} current instructions saved") do
      current = saved_routine(id)
      current if current['minutes'] == minutes && current['prompt'] == prompt
    end
  end

  def pause_routine(name, id)
    show_routines
    desktop.click('Pause routine')
    desktop.value("Message #{name}", "Pause Scheduled Routine\n@#{id}")
    desktop.click('Send message')
    Contract.wait(description: "#{id} paused through chat") { current = saved_routine(id); current if current['paused'] }
  end

  def routine_outcome(reply)
    decision('Outcome', reply: reply, outcome: {status: 'Completed', criterionResults: [
      {id: 'comparison', result: 'Satisfied', evidence: 'Read the local source and appended its report.'}
    ], artifacts: ['effects.txt']})
  end

  it 'keeps Chief and another routine responsive while coalescing busy deadlines with admission-time instructions' do
    @host = desktop_host
    start_desktop
    old_prompt = "Busy inventory report v1. Read source.txt. Append 'old report: ' and its full contents to effects.txt. Reply Old inventory report complete."
    new_prompt = "Busy inventory report v2. Read revised-source.txt. Append 'new report: ' and its full contents to effects.txt. Reply New inventory report complete."
    independent_prompt = "Independent orchard check. Read source.txt. Append 'independent report: ' and its full contents to effects.txt. Reply Independent check complete."
    busy_owner, busy, busy_workspace = create_routine_specialist('InventoryReporter', old_prompt,
      {'source.txt' => "Apricot inventory: 7 crates.\n", 'revised-source.txt' => "Pear inventory: 11 crates.\n"})
    other_owner, other, other_workspace = create_routine_specialist('OrchardChecker', independent_prompt,
      {'source.txt' => "Lime inventory: 2 crates.\n"})
    busy_id, other_id = busy.fetch('id'), other.fetch('id')
    busy_journal = File.join(busy_workspace, 'effects.txt')
    other_journal = File.join(other_workspace, 'effects.txt')

    host.configure(rules: {
      "#{chief}:Assessing" => {response: decision('FitsSpecialization', mode: 'Repl', reply: 'Chief remains responsive during the inventory report.')}
    }, messages: [
      {contains: 'Busy inventory report v1.', node: 'Execution', rule: {block: true, gate: 'busy-old-release', actions: [
        {append: 'effects.txt', contentFrom: 'source.txt', prefix: 'old report: '}
      ], response: routine_outcome('Old inventory report complete.')}},
      {contains: 'Busy inventory report v2.', node: 'Execution', rule: {actions: [
        {append: 'effects.txt', contentFrom: 'revised-source.txt', prefix: 'new report: '}
      ], response: routine_outcome('New inventory report complete.')}},
      {contains: 'Independent orchard check.', node: 'Execution', rule: {actions: [
        {append: 'effects.txt', contentFrom: 'source.txt', prefix: 'independent report: '}
      ], response: routine_outcome('Independent check complete.')}}
    ])

    select_routine_owner('InventoryReporter')
    edit_routine('InventoryReporter', busy_id, 1, old_prompt, previous_minutes: 10)
    select_routine_owner('OrchardChecker')
    edit_routine('OrchardChecker', other_id, 1, independent_prompt, previous_minutes: 10)

    old_call = Contract.wait(timeout: 150, description: 'busy routine reaches its external execution gate') do
      routine_executions(busy_owner).first
    end
    expect(routine_context(old_call).fetch('originalIntent')).to eq(old_prompt)
    old_turn = routine_turns(busy_id).fetch(0)
    expect(old_turn.fetch('origin')).to include('executionPrompt' => old_prompt, 'minutes' => 1)
    expect(File.exist?(busy_journal)).to eq(false)

    select_routine_owner('ChiefSapi')
    send_routine_message('ChiefSapi', 'Can you still answer while InventoryReporter is busy?')
    desktop.see('Chief remains responsive during the inventory report.')
    expect(routine_turns(busy_id).map { |turn| turn['id'] }).to eq([old_turn.fetch('id')])
    expect(File.exist?(busy_journal)).to eq(false)

    other_turn = Contract.wait(timeout: 85, description: 'independent routine is admitted while the first stays gated') do
      routine_turns(other_id).first
    end
    select_routine_owner('OrchardChecker')
    pause_routine('OrchardChecker', other_id)
    host.turn(other_turn.fetch('id'), 'done', timeout: 85)
    desktop.click('Chat')
    desktop.see('Independent check complete.')
    expect(File.read(other_journal)).to eq("independent report: Lime inventory: 2 crates.\n")
    expect(routine_executions(other_owner).length).to eq(1)
    expect(File.exist?(busy_journal)).to eq(false)

    Contract.wait(timeout: 150, description: 'at least two minute deadlines accumulate without an overlapping run') do
      snapshot = host.state
      expect(routine_turns(busy_id, snapshot).map { |turn| turn['id'] }).to eq([old_turn.fetch('id')])
      expect(saved_routine(busy_id, snapshot).fetch('runCount')).to eq(1)
      expect(routine_executions(busy_owner).length).to eq(1)
      expect(File.exist?(busy_journal)).to eq(false)
      saved_routine(busy_id, snapshot).fetch('deferredCount', 0) >= 2
    end

    select_routine_owner('InventoryReporter')
    paused = pause_routine('InventoryReporter', busy_id)
    pending_count = paused.fetch('deferredCount')
    deferred = host.state.fetch('events').fetch(busy_owner).select do |event|
      event['entity'] == busy_id && event['event'] == 'routine.deferred'
    end
    latest_pending_due = deferred.map { |event| event.fetch('details').fetch('through') }.max_by { |due| Time.iso8601(due) }
    expect(pending_count).to be >= 2
    expect(latest_pending_due).not_to be_nil
    edited = edit_routine('InventoryReporter', busy_id, 10, new_prompt, previous_minutes: 1, period: 'Paused')
    expect(edited).to include('paused' => true, 'runCount' => 1, 'deferredCount' => pending_count)
    expect(routine_turns(busy_id).fetch(0).fetch('origin')).to include('executionPrompt' => old_prompt, 'minutes' => 1)
    expect(File.exist?(busy_journal)).to eq(false)

    show_routines('Paused')
    desktop.click('Resume routine')
    desktop.value('Message InventoryReporter', "Resume Scheduled Routine\n@#{busy_id}")
    desktop.click('Send message')
    resumed = Contract.wait(description: 'edited routine resumes with its pending deadlines intact') do
      current = saved_routine(busy_id)
      current unless current['paused']
    end
    expect(resumed).to include('prompt' => new_prompt, 'minutes' => 10, 'runCount' => 1, 'deferredCount' => pending_count)
    expect(File.exist?(busy_journal)).to eq(false)
    host.release('busy-old-release')

    new_call = Contract.wait(timeout: 150, description: 'run_latest admits the retained latest deadline after the gated run') do
      routine_executions(busy_owner).find { |call| routine_context(call).fetch('originalIntent') == new_prompt }
    end
    expect(routine_context(new_call).fetch('originalIntent')).to eq(new_prompt)
    new_turn = Contract.wait(description: 'second busy routine turn is visible') do
      routine_turns(busy_id).find { |turn| turn['id'] != old_turn.fetch('id') }
    end
    expect(new_turn.fetch('origin')).to include('executionPrompt' => new_prompt, 'due' => latest_pending_due, 'minutes' => 1)
    expect(Time.iso8601(new_turn.fetch('origin').fetch('due'))).to be < Time.iso8601(resumed.fetch('nextDue'))
    pause_routine('InventoryReporter', busy_id)
    old_result = host.turn(old_turn.fetch('id'), 'done', timeout: 20)
    expect(new_call.fetch('started')).to be >= Time.iso8601(old_result.fetch('output_at')).to_f
    host.turn(new_turn.fetch('id'), 'done', timeout: 85)
    desktop.click('Chat')
    desktop.see('Old inventory report complete.')
    desktop.see('New inventory report complete.')

    snapshot = host.state
    expect(File.read(busy_journal)).to eq("old report: Apricot inventory: 7 crates.\nnew report: Pear inventory: 11 crates.\n")
    expect(routine_turns(busy_id, snapshot).map { |turn| turn['id'] }).to contain_exactly(old_turn.fetch('id'), new_turn.fetch('id'))
    executions = routine_executions(busy_owner)
    expect(executions.map { |call| routine_context(call).fetch('originalIntent') }).to eq([old_prompt, new_prompt])
    expect(executions.first.fetch('finished')).to be <= executions.last.fetch('started')
    expect(saved_routine(busy_id, snapshot)).to include('runCount' => 2, 'paused' => true)
    expect(saved_routine(busy_id, snapshot).fetch('deferredCount', 0)).to eq(0)
    closures = snapshot.fetch('events').fetch(busy_owner).select do |event|
      event['entity'] == busy_id && event['event'] == 'routine.dues_closed'
    end.map { |event| event.fetch('details') }
    expect(closures).to all(include('reason' => {'kind' => 'superseded', 'run' => new_turn.fetch('id')}))
    expect(closures.sum { |details| details.fetch('count') }).to eq(pending_count - 1)
  end
end
