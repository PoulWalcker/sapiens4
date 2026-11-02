require_relative 'support/desktop'
require_relative 'support/proxy'
require 'sqlite3'
require 'time'

RSpec.describe 'Routine list, results and settings in the native app', :desktop do
  def click_button(label)
    roles = %w[AXButton AXCheckBox AXRadioButton AXLink AXDisclosureTriangle AXPopUpButton]
    node = desktop.find(label, role: roles)
    desktop.control('scroll-to', '--path', node.fetch('path'))
    desktop.click_node(desktop.find(label, role: roles))
  end

  def open_routines
    click_button('Work menu')
    click_button('Automation menu')
    click_button('Routines')
  end

  def create_routine(name = 'Daily comparison')
    click_button('+ New routine')
    desktop.fill('Name (up to 60 characters)', name)
    desktop.fill('What to do', 'Compare the products.')
    desktop.fill('Completion criteria (one per line)', 'Return a sourced comparison.')
    click_button('Create routine')
    desktop.see('No results yet')
  end

  it 'shows sourced evidence and unknown usage without starting work, and saves run feedback' do
    @host = desktop_host
    owner = host.state.fetch('main_agent_id')
    routine = host.api('POST', "/api/agents/#{owner}/routines", operation: 'evidence-create', title: 'Evidence example', goal: 'Compare the products.', criteria: ['Return a sourced comparison.']).fetch('routine')
    run = host.api('POST', "/api/agents/#{owner}/routines/#{routine}/run", operation: 'evidence-run', revision: 1).fetch('run')
    host.turn(run)
    Contract.wait(description: 'background analysis preflight settles before read-only observations') do
      routine_snapshot(routine).dig('analysis', 'status') == 'blocked'
    end
    before = host.state.fetch('personal_routines').find { |row| row['id'] == routine }
    calls = host.calls.length
    start_desktop
    open_routines
    click_button('Evidence example')
    desktop.see('Assessment: Accepted')
    click_button('Evidence and usage')
    desktop.see('Model reports satisfied')
    desktop.see('Input tokens: Unknown')
    desktop.see('Price: Unknown')
    desktop.see('external effects')
    click_button('History')
    click_button(/Run 1 ·/)
    click_button('Evidence and usage')
    expect(host.calls.length).to eq(calls)
    expect(host.state.fetch('personal_routines').find { |row| row['id'] == routine }).to eq(before)
    click_button('Reject result…')
    desktop.fill('Reason for rejecting (up to 4000 characters)', 'The comparison missed my requested context.')
    click_button('Reject result')
    desktop.see('Assessment: Rejected')
    click_button('Evidence and usage')
    desktop.see('Original assessment: Accepted')
    desktop.see('Admin feedback: The comparison missed my requested context.')
    expect(host.calls.length).to eq(calls)
  end

  it 'shows owner analysis, saved usage and feedback invalidation, and stops explicit reconsideration' do
    @host = desktop_host
    owner = host.state.fetch('main_agent_id')
    answer = decision('AutomationAnalyzed', decision: {status: 'Ready', automatablePart: 'Compare the supplied records.', parameters: [{name: 'records', description: 'Current source records'}]})
    host.configure(version: 'codex-cli 0.162.1', rules: {'AutomationAnalysis' => {response: answer, usage: {input_tokens: 71, cached_input_tokens: 7, output_tokens: 19}}})
    routine = host.api('POST', "/api/agents/#{owner}/routines", operation: 'analysis-create', title: 'Analysis example', goal: 'Compare the products.', criteria: ['Return a sourced comparison.']).fetch('routine')
    path = "/api/agents/#{owner}/routines/#{routine}"
    run = host.api('POST', "#{path}/run", operation: 'analysis-run', revision: 1).fetch('run')
    Contract.wait { routine_snapshot(routine).dig('analysis', 'status') == 'Ready' }
    calls = host.calls.length
    start_desktop
    open_routines
    click_button('Analysis example')
    desktop.see('Ready for later preparation')
    desktop.see('Compare the supplied records.')
    desktop.see('records: Current source records')
    click_button('Analysis usage')
    desktop.see('Purpose: automation analysis')
    desktop.see('Input tokens: 71')
    click_button('History')
    click_button(/Run 1 ·/)
    desktop.see('Verified comparison')
    expect(host.calls.length).to eq(calls)
    click_button('Reject result…')
    desktop.fill('Reason for rejecting (up to 4000 characters)', 'The source omitted an important product.')
    click_button('Reject result')
    desktop.see('Analysis needs reconsideration')
    desktop.see('Assessment: Rejected')
    expect(host.calls.length).to eq(calls)
    host.configure(rules: {'AutomationAnalysis' => {block: true, gate: 'held-analysis', response: answer}})
    click_button('Reconsider automation…')
    click_button('Analyze evidence')
    desktop.see('Analysis in progress')
    click_button('Stop analysis…')
    click_button('Stop analysis')
    desktop.see('Analysis stopped')
    host.release('held-analysis')
    expect(routine_snapshot(routine).fetch('runs').map { |entry| entry['id'] }).to eq([run])
    host.configure(rules: {'AutomationAnalysis' => {response: decision('AutomationAnalyzed', decision: {status: 'NeedMoreEvidence', missingObservations: ['A complete source record.']})}})
    click_button('Reconsider automation…')
    click_button('Analyze evidence')
    desktop.see('More evidence needed')
    desktop.see('A complete source record.')
    host.configure(rules: {'AutomationAnalysis' => {response: decision('AutomationAnalyzed', decision: {status: 'Decline', reconsiderCondition: 'A stable source format becomes available.'})}})
    click_button('Reconsider automation…')
    click_button('Analyze evidence')
    desktop.see('Automation declined')
    desktop.see('A stable source format becomes available.')
    expect(routine_snapshot(routine).fetch('runs').map { |entry| entry['id'] }).to eq([run])
  end

  it 'uses separate list, latest result, history, run and inline settings screens' do
    @host = desktop_host
    start_desktop
    open_routines
    create_routine
    click_button('Manage Daily comparison')
    click_button('Settings')
    desktop.fill('Name (up to 60 characters)', 'Weekly comparison')
    click_button('Save changes')
    desktop.see('Weekly comparison')
    click_button('Manage Weekly comparison')
    click_button('Settings')
    click_button('Schedule settings')
    desktop.fill('Interval, minutes', '60')
    click_button('Enable interval')
    desktop.see('Every 60 minutes')
    expect(host.state['personal_routines'].length).to eq(1)
    click_button('Run now')
    desktop.fill('Inputs for this run', 'Today’s products')
    click_button('Run now')
    desktop.see('Verified comparison')
    desktop.see('Definition revision 3')
    click_button('History')
    click_button(/Run 1 ·/)
    desktop.see('Today’s products')
    host.configure(block: true)
    click_button('Run now')
    click_button('Run now')
    desktop.see('Work is in progress.')
    click_button('Stop this run…')
    click_button('Stop run')
    desktop.see('Stopped')
    click_button('Manage Weekly comparison')
    click_button('Delete…')
    click_button('Delete routine and stop runs')
    click_button('Weekly comparison')
    click_button('History')
    click_button(/Run 1 ·/)
    desktop.see('Verified comparison')
    desktop.reload
    click_button('Deleted')
    click_button('Weekly comparison')
    click_button('History')
    click_button(/Run 1 ·/)
    desktop.see('Verified comparison')
  end

  it 'preserves inline edits across polling and Sapi changes, and confirms explicit discard' do
    @host = desktop_host
    host.agent('OtherSapi')
    start_desktop
    open_routines
    create_routine
    click_button('Manage Daily comparison')
    click_button('Settings')
    desktop.fill('What to do', 'An unsaved request')
    click_button('Open OtherSapi')
    click_button('Open ChiefSapi')
    open_routines
    desktop.value('What to do', 'An unsaved request')
    click_button('Cancel')
    desktop.see('Your changes have not been saved.')
    click_button('Discard changes')
    click_button('Manage Daily comparison')
    click_button('Settings')
    desktop.value('What to do', 'Compare the products.')
  end

  it 'keeps same-title legacy intervals visible without merging them with manual definitions' do
    @host = desktop_host
    owner = host.state.fetch('main_agent_id')
    interval = host.control(owner, 'routine_create', minutes: 60, prompt: 'Same title')['routine']
    host.api('POST', "/api/agents/#{owner}/routines", operation: 'manual-fixture', title: 'Same title', goal: 'Compare the products.', criteria: ['Return a sourced comparison.'])
    start_desktop
    open_routines
    desktop.see('On request')
    desktop.see('Every 60 minutes')
    buttons = desktop.tree.select { |node| node['role'] == 'AXButton' && desktop.label(node) == 'Same title' }
    expect(buttons.length).to eq(2)
    schedule = desktop.find(/Every 60 minutes/)
    target = buttons.select { |node| node['position']['y'] < schedule['position']['y'] }.max_by { |node| node['position']['y'] }
    expect(target).not_to be_nil
    desktop.click_node(target)
    click_button('Manage Same title')
    click_button('Settings')
    desktop.see('prepares its completion criteria when it runs')
    click_button('Edit instructions through chat')
    expect(desktop.find('Message ChiefSapi')['value']).to include("Edit @#{interval['id']} (", 'Edit Frequency')
  end
  it 'recovers the same accepted save after switching corpora while its response is delayed', unless: Gem.win_platform? do
    @host = desktop_host
    original_name = host.state['corpora']['name']
    host.api('POST', '/api/host/local', {name: 'Other Studio'}, status: 201)
    requested, release = Queue.new, Queue.new
    requests = []
    proxy = HostProxy.new(host) do |request, response|
      if request.request_method == 'POST' && request.path.end_with?('/routines')
        requests << JSON.parse(request.body)
        if requests.length == 1
          requested << true
          Timeout.timeout(30) { release.pop }
        end
      end
      response
    end
    start_desktop(proxy.url)
    open_routines
    click_button('+ New routine')
    desktop.fill('Name (up to 60 characters)', 'One saved routine')
    desktop.fill('What to do', 'Compare the products.')
    desktop.fill('Completion criteria (one per line)', 'Return a sourced comparison.')
    click_button('Create routine')
    Timeout.timeout(10) { requested.pop }
    desktop.click('Local')
    desktop.click('Other Studio')
    desktop.click('Close corpora menu')
    desktop.find('Other Studio', role: 'AXHeading')
    release << true
    desktop.click('Local')
    desktop.click(original_name)
    desktop.click('Close corpora menu')
    open_routines
    desktop.see('The connection changed while saving.')
    click_button('Create routine')
    desktop.see('No results yet')
    expect(requests.length).to eq(2)
    expect(requests.first).to eq(requests.last)
    expect(host.state['personal_routines'].map { |row| row['title'] }).to eq(['One saved routine'])
  ensure
    release << true if release
    proxy&.close
  end

  it 'opens the existing chat composer when a run needs the user’s reply' do
    @host = desktop_host
    owner = host.state.fetch('main_agent_id')
    host.configure(rules: {
      "#{owner}:Execution" => {response: decision('OutsideSpecialization')},
      'ChiefTriage' => {response: decision('RequestUnclear', question: 'Which products?')}
    })
    start_desktop
    open_routines
    create_routine
    click_button('Run now')
    click_button('Run now')
    desktop.see('Which products?')
    click_button('Reply to ChiefSapi')
    desktop.fill('Message ChiefSapi', 'Products A and B')
    desktop.value('Message ChiefSapi', 'Products A and B')
  end

  def scheduled_fixture(minutes = 30)
    owner = host.state.fetch('main_agent_id')
    id = host.api('POST', "/api/agents/#{owner}/routines", operation: 'create-fixture', title: 'Scheduled comparison', goal: 'Compare the products.', criteria: ['Return a sourced comparison.'])['routine']
    host.api('POST', "/api/agents/#{owner}/routines/#{id}/schedule", operation: 'schedule-fixture', revision: 1, minutes: minutes)
    [owner, id]
  end

  def routine_snapshot(id)
    host.state.fetch('personal_routines').find { |row| row['id'] == id }
  end

  it 'conflicts on a stale interval edit and keeps the active run snapshot unchanged' do
    @host = desktop_host
    owner, id = scheduled_fixture
    host.configure(block: true)
    run = host.api('POST', "/api/agents/#{owner}/routines/#{id}/run", operation: 'active-fixture', revision: 2)['run']
    host.turn(run, 'running')
    start_desktop
    open_routines
    click_button('Scheduled comparison')
    click_button('Manage Scheduled comparison')
    click_button('Settings')
    click_button('Schedule settings')
    desktop.fill('Interval, minutes', '5')
    host.control(owner, 'routine_update', id: id, minutes: 90, prompt: 'Compare the products.')
    click_button('Save interval')
    desktop.see('Routine changed')
    click_button('Reload current schedule')
    desktop.value('Interval, minutes', '90')
    desktop.fill('Interval, minutes', '5')
    click_button('Save interval')
    desktop.see('Every 5 minutes')
    desktop.see('Next:')
    desktop.see('Definition revision 2')
    value = routine_snapshot(id)
    expect(value['minutes']).to eq(5)
    expect(value['runs'].find { |row| row['id'] == run }['snapshot']).to include('revision' => 2, 'minutes' => 30)
    click_button('Stop this run…')
    click_button('Stop run')
    desktop.see('Stopped')
  end

  it 'keeps pause, a manual run and stopping the current timer occurrence separate' do
    @host = desktop_host
    owner, id = scheduled_fixture(1)
    host.configure(block: true)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      sequence = id.split('_').last.to_i
      saved = JSON.parse(db.get_first_value('SELECT value FROM scheduled_routines WHERE sequence=?', sequence))
      saved['nextDue'] = (Time.now.utc - 60).strftime('%Y-%m-%dT%H:%M:%S.000000+00:00')
      db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?', [JSON.generate(saved), sequence])
    end
    start_desktop
    open_routines
    click_button('Scheduled comparison')
    timer = Contract.wait(timeout: 90, description: 'real minute pulse starts a scheduled occurrence') do
      routine_snapshot(id)['runs'].find { |run| run['trigger'] == 'interval' && run['status'] == 'running' }
    end
    desktop.see('Scheduled run')
    click_button('Pause schedule')
    desktop.see('Schedule paused')
    desktop.see('Work is in progress.')
    click_button('Run now')
    desktop.fill('Inputs for this run', 'A manual request while paused')
    click_button('Run now')
    desktop.see('Manual run')
    click_button('History')
    click_button(/Run 1 ·/)
    desktop.see('Scheduled run')
    click_button('Stop this run…')
    click_button('Stop run')
    desktop.see('Stopped')
    host.release
    manual = Contract.wait do
      routine_snapshot(id)['runs'].find { |run| run['trigger'] == 'manual' && run['status'] == 'done' }
    end
    click_button('History')
    click_button(/Run 2 ·/)
    desktop.see('Verified comparison')
    desktop.see('Schedule paused')
    value = routine_snapshot(id)
    expect(value['paused']).to eq(true)
    expect(value['runs'].find { |run| run['id'] == timer['id'] }['status']).to eq('stopped')
    expect(manual['snapshot']['inputs']).to eq('A manual request while paused')
    expect(value['runs'].length).to eq(2)
    click_button('Resume schedule')
    desktop.see('Next:')
    expect(routine_snapshot(id)['paused']).to eq(false)
  end

  it 'retries an unconfirmed pause unchanged and refreshes a stale resume deliberately', unless: Gem.win_platform? do
    @host = desktop_host
    owner, id = scheduled_fixture
    requests = []
    frozen = nil
    proxy = HostProxy.new(host) do |request, response|
      if request.request_method == 'GET' && request.path.end_with?('/api/state') && frozen
        [200, frozen, {'Content-Type' => 'application/json'}]
      elsif request.request_method == 'POST' && request.path.match?(%r{/routines/[^/]+/(pause|resume)$})
        requests << {path: request.path, body: JSON.parse(request.body)}
        if requests.length == 1
          [503, {error: 'Fixture response lost after acceptance'}, {'Content-Type' => 'application/json'}]
        else
          response
        end
      end
    end
    start_desktop(proxy.url)
    open_routines
    click_button('Scheduled comparison')
    click_button('Pause schedule')
    desktop.see('Fixture response lost after acceptance')
    click_button('Retry unchanged')
    desktop.see('Schedule paused')
    expect(requests[0]).to eq(requests[1])
    frozen = host.state
    host.control(owner, 'routine_update', id: id, minutes: 45, prompt: 'Compare the products.')
    click_button('Resume schedule')
    desktop.see('Routine changed')
    frozen = nil
    click_button('Refresh current schedule')
    click_button('Resume schedule')
    desktop.see('Next:')
    expect(requests[2][:body]['operation']).not_to eq(requests[3][:body]['operation'])
    expect(requests[3][:body]['revision']).to be > requests[2][:body]['revision']
    expect(routine_snapshot(id)).to include('paused' => false, 'minutes' => 45)
  ensure
    proxy&.close
  end

  it 'does not apply a delayed settings reload to another Sapi or replace newer draft input', unless: Gem.win_platform? do
    @host = desktop_host
    owner, id = scheduled_fixture
    other = host.agent('OtherSapi')
    host.api('POST', "/api/agents/#{other}/routines", operation: 'other-fixture', title: 'Other routine', goal: 'Another request', criteria: ['Provide a result.'])
    held, returned = Queue.new, Queue.new
    blocking = false
    released = false
    proxy = HostProxy.new(host) do |request, response|
      if blocking && request.request_method == 'GET' && request.path.end_with?('/api/state')
        held << true
        Timeout.timeout(30) { sleep 0.02 until released }
        returned << true
      end
      response
    end
    start_desktop(proxy.url)
    open_routines
    click_button('Scheduled comparison')
    click_button('Manage Scheduled comparison')
    click_button('Settings')
    click_button('Schedule settings')
    desktop.fill('Interval, minutes', '5')
    host.control(owner, 'routine_update', id: id, minutes: 90, prompt: 'Compare the products.')
    click_button('Save interval')
    desktop.see('Routine changed')
    blocking = true
    click_button('Reload current schedule')
    Timeout.timeout(10) { held.pop }
    desktop.fill('Interval, minutes', '7')
    click_button('Open OtherSapi')
    open_routines
    click_button('Other routine')
    blocking = false
    released = true
    Timeout.timeout(10) { returned.pop }
    click_button('Manage Other routine')
    click_button('Settings')
    desktop.value('What to do', 'Another request')
    click_button('Open ChiefSapi')
    open_routines
    desktop.value('Interval, minutes', '7')
    expect(host.state['personal_routines'].length).to eq(2)
    expect(routine_snapshot(id)['minutes']).to eq(90)
  ensure
    released = true
    proxy&.close
  end

  it 'does not invent a definition revision for saved history without a snapshot' do
    @host = desktop_host
    owner, id = scheduled_fixture
    run = host.api('POST', "/api/agents/#{owner}/routines/#{id}/run", operation: 'history-fixture', revision: 2)['run']
    host.turn(run)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      work = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', run))
      work.delete('routineSnapshot')
      db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(work), run])
    end
    start_desktop
    open_routines
    click_button('Scheduled comparison')
    desktop.see('Verified comparison')
    desktop.see('Definition revision not recorded')
    expect(desktop.text).not_to include('Definition revision 0')
  end

end
