require 'open3'
require 'sqlite3'
require 'time'
require_relative 'support/candidate_fixtures'
require_relative 'support/desktop'

RSpec.describe 'Durable finite workflow time waits' do
  include CandidateFixtures
  K5_CRITERIA = (CandidateFixtures::CRITERIA + ['Honor each explicitly selected wait before final completion.']).freeze
  USAGE = {input_tokens: 17, output_tokens: 7}.freeze
  before(:all) do
    @docker = File.expand_path('~/.orbstack/bin/docker')
    @socket = 'unix://' + File.expand_path('~/.orbstack/run/docker.sock')
    out, err, status = Open3.capture3(@docker, '--host', @socket, 'ps', '--all', '--quiet', '--no-trunc')
    raise err unless status.success?
    @containers = out.lines.map(&:strip).sort
  end
  after(:all) do
    out, err, status = Open3.capture3(@docker, '--host', @socket, 'ps', '--all', '--quiet', '--no-trunc')
    raise err unless status.success?
    expect(out.lines.map(&:strip).sort).to eq(@containers)
  end

  before do
    repo = ENV.fetch('SAPIENS_TEST_REPOSITORY', Contract::ROOT)
    @host = Host.new(start: false, repo: repo, command: [File.join(repo, '.build/bin/sapiens4')], environment: {'SAPIENS_GENERATED_DOCKER' => @docker, 'SAPIENS_GENERATED_SOCKET' => @socket})
    host.configure(version: 'codex-cli 0.162.1', model_catalog: [{slug: 'gpt-6-sol', shell_type: 'unified_exec', apply_patch_tool_type: 'freeform', experimental_supported_tools: [], tool_mode: 'direct', supports_search_tool: false, supported_reasoning_levels: [{effort: 'high', description: 'high'}]}])
    host.start
    @owner = host.agent('WaitOwner')
    rules('Execution' => {response: decision('Outcome', reply: 'Observed sales.', outcome: {status: 'Completed', satisfiedCriteria: K5_CRITERIA, artifacts: []}), usage: USAGE},
      'AutomationAnalysis' => {response: decision('AutomationAnalyzed', decision: {status: 'Ready', automatablePart: 'Compute sales with optional interpretation.', parameters: [{name: 'csv', description: 'Sales'}]}), usage: USAGE},
      'PreparationFixtures' => {response: {fixtures: workflow_fixtures}, usage: USAGE},
      'PreparationGeneration' => {response: {workflow: graph}, usage: USAGE},
      'PreparationReview' => {response: {accepted: true, reason: 'Composed independent results match both branches.'}, usage: USAGE},
      'WorkflowStep' => {response: expected_second, usage: USAGE},
      'CodeAssessment' => {response: {verdict: 'accepted', reply: 'Actual sales satisfy the original criteria.', reason: 'The whole composed result matches.', evidence: ['Actual selected node outputs.'], acceptance: {status: 'Completed', satisfiedCriteria: K5_CRITERIA, artifacts: []}, activationAudit: {accepted: true, reason: 'Own composed fixtures and authorized occurrence support this exact workflow.'}}, usage: USAGE})
    @routine = create('sales', 'Waiting sales')
  end
  after { @desktop&.close; @host&.close }
  def rules(changes); host.configure(rules: host.provider.fetch('rules', {}).merge(changes)); end
  def route(id = @routine); "/api/agents/#{@owner}/routines/#{id}"; end
  def saved(id = @routine); host.state.fetch('personal_routines').find { |row| row['id'] == id }; end
  def create(operation, title)
    id = host.api('POST', "/api/agents/#{@owner}/routines", operation: operation, title: title, goal: CandidateFixtures::GOAL + ' Preserve the result through the supplied finite conditional waits.', criteria: K5_CRITERIA).fetch('routine')
    run(id, "observe-#{operation}", observed_input)
    Contract.wait { saved(id)['analysis'] if saved(id).dig('analysis', 'status') == 'Ready' }
    id
  end
  def run(id, operation, input, wait: true)
    identity = host.api('POST', "#{route(id)}/run", operation: operation, revision: saved(id).fetch('revision'), inputs: JSON.generate(input)).fetch('run')
    host.turn(identity) if wait
    identity
  end
  def prepare(operation, id = @routine)
    host.api('POST', "#{route(id)}/prepare", operation: operation, revision: saved(id).fetch('revision'), analysis: saved(id).fetch('analysis').fetch('id')).fetch('preparation')
  end
  def prepared(identity, id = @routine, status: 'awaiting_occurrence')
    Contract.wait(timeout: 90, description: "preparation #{status}") do
      value = host.api('GET', "#{route(id)}/preparations/#{identity}")
      raise "Preparation #{value['status']}: #{value['error']}" if status == 'awaiting_occurrence' && %w[failed stopped interrupted].include?(value['status'])
      value if value['status'] == status
    end
  end
  def detail(identity, id = @routine); host.api('GET', "#{route(id)}/runs/#{identity}"); end
  def calls; host.calls.select { |call| call['node'] == 'WorkflowStep' }; end
  def second_input; {csv: CandidateFixtures::HELDOUT, start: '2026-12-05', end: '2026-12-06'}; end
  def expected_second; {status: 'ok', value: {by_category_minor: {art: 0, tools: 200}, end: '2026-12-06', row_count: 3, start: '2026-12-05', total_minor: 200}}; end
  def workflow_fixtures
    [independent_fixtures[0], independent_fixtures[2], independent_fixtures[4]].map do |row|
      row.merge(input: row.fetch(:input).merge(first: true, second: true, wait1: 0, wait2: 0), modelStubs: {left: row.fetch(:expected)})
    end
  end
  def condition(field)
    "import json,sys\nv=json.load(sys.stdin)\nprint(json.dumps({'value':v.get('#{field}',False),'reason':'Use the accepted #{field} condition.'}))\n"
  end
  def graph
    {entry: 'calculate', nodes: {
      calculate: {kind: 'script', source: candidate_source, input: {input: true}, next: 'split'},
      split: {kind: 'fork', children: ['left', 'right'], join: 'joined'},
      left: {kind: 'model', instruction: 'Return the verified calculation unchanged.', input: {output: 'calculate'}, next: 'joined'},
      right: {kind: 'script', source: "import json,sys\nprint(json.dumps(json.load(sys.stdin)))\n", input: {output: 'calculate'}, next: 'joined'},
      joined: {kind: 'join', fork: 'split', input: {array: [{output: 'left'}, {output: 'right'}, {output: 'calculate'}]}, next: 'combine'},
      combine: {kind: 'script', source: "import json,sys\nv=json.load(sys.stdin)\nassert v[0]==v[1]==v[2]\nprint(json.dumps(v[2]))\n", input: {output: 'joined'}, next: 'condition1'},
      condition1: {kind: 'script', source: condition('first'), input: {input: true}, next: 'choice1'},
      choice1: {kind: 'branch', input: {output: 'condition1'}, true: 'wait1', false: 'condition2'},
      wait1: {kind: 'defer', input: {object: {afterMilliseconds: {input: true, pointer: '/wait1'}, progress: {literal: 'First checkpoint retained'}, value: {output: 'combine'}}}, next: 'condition2'},
      condition2: {kind: 'script', source: condition('second'), input: {input: true}, next: 'choice2'},
      choice2: {kind: 'branch', input: {output: 'condition2'}, true: 'wait2', false: 'finish'},
      wait2: {kind: 'defer', input: {object: {afterMilliseconds: {input: true, pointer: '/wait2'}, progress: {literal: 'Second checkpoint retained'}, value: {output: 'combine'}}}, next: 'finish'},
      finish: {kind: 'finish', input: {output: 'combine'}}}}
  end
  def wait_record(identity, node)
    Contract.wait(timeout: 90) do
      value = detail(identity)
      raise value.inspect if %w[failed cancelled stopped interrupted].include?(value['status'])
      row = value.fetch('workflowWaits').find { |w| w['node'] == node }
      row if row && row['status'] == 'waiting'
    end
  end

  it 'checks K5 conditions and child results, restarts during real waits and resumes the same pins without repeated completed work' do
    candidate = prepared(prepare('prepare-k5'))
    expect(candidate.fetch('checks')).to all(include('matched' => true))
    fixture_nodes = candidate.fetch('workflowFixtures').values.flat_map { |trace| trace.fetch('nodes').values }
    expect(fixture_nodes.select { |row| row.dig('request', 'node', 'kind') == 'defer' }).to all(include('fixtureSimulation' => true))
    started = Process.clock_gettime(Process::CLOCK_MONOTONIC)
    identity = run(@routine, 'real-k5', second_input.merge(first: true, second: true, wait1: 8000, wait2: 3000), wait: false)
    first = wait_record(identity, 'wait1')
    retained = detail(identity)
    pin = retained.dig('codeExecution', 'reference')
    hash = retained.dig('codeExecution', 'structureHash')
    deadline = retained.dig('codeExecution', 'workflowDeadline')
    active = retained.dig('workflowParallel', 'activeMilliseconds')
    expect(host.state.fetch('run_activity').keys).not_to include(identity)
    expect(calls.length).to eq(1)
    host.restart
    after_restart = detail(identity)
    expect(after_restart.fetch('workflowWaits').find { |row| row['node'] == 'wait1' }.fetch('due')).to eq(first.fetch('due'))
    expect(after_restart.dig('codeExecution', 'reference')).to eq(pin)
    expect(after_restart.dig('codeExecution', 'structureHash')).to eq(hash)
    expect(after_restart.dig('codeExecution', 'workflowDeadline')).to eq(deadline)
    expect(after_restart.dig('workflowParallel', 'activeMilliseconds')).to eq(active)
    expect(calls.length).to eq(1)
    independent = host.agent('IndependentDuringWait')
    rules("#{independent}:Assessing" => {response: decision('FitsSpecialization', mode: 'Repl', reply: 'Ordinary work continues during the timer.'), usage: USAGE})
    host.turn(host.submit(independent, 'Complete this independent observation.'))
    second = wait_record(identity, 'wait2')
    expect(Time.iso8601(second.fetch('due'))).to be > Time.iso8601(first.fetch('due'))
    host.turn(identity, 'done', timeout: 60)
    final = detail(identity)
    expect(final.fetch('workflowWaits')).to all(include('status' => 'completed'))
    expect(final.dig('codeExecution', 'activation', 'status')).to eq('activated')
    expect(final.dig('codeExecution', 'reference')).to eq(pin)
    expect(final.dig('codeExecution', 'workflowDeadline')).to eq(deadline)
    expect(final.dig('codeExecution', 'workflowRun', 'nodes').values.none? { |row| row['fixtureSimulation'] }).to be(true)
    elapsed = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - started) * 1000
    expect(final.dig('workflowParallel', 'activeMilliseconds')).to be < elapsed - 5000
    expect(calls.length).to eq(1)
    duplicate = host.api('POST', "#{route}/run", operation: 'real-k5', revision: saved.fetch('revision'), inputs: JSON.generate(second_input.merge(first: true, second: true, wait1: 8000, wait2: 3000)))
    expect(duplicate.fetch('run')).to eq(identity)
    host.restart
    expect(calls.length).to eq(1)
    expect(detail(identity).fetch('workflowWaits')).to eq(final.fetch('workflowWaits'))
  end
  def edit_work(identity)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.busy_timeout = 5000
      value = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', identity))
      yield value
      db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(value), identity])
    end
  end

  it 'refuses invalid time, Stops known waiting work and terminalizes an expired original deadline without another admission' do
    bad = graph
    bad[:nodes][:wait1][:input] = {literal: {at: '2026-11-03T12:00:00', progress: 'Missing explicit timezone', value: nil}}
    rules('PreparationGeneration' => {response: {workflow: bad}, usage: USAGE})
    failed = prepared(prepare('invalid-time'), status: 'failed')
    expect(failed.fetch('checks')).to eq([])
    expect(failed.fetch('error')).to include('timezone')
    rules('PreparationGeneration' => {response: {workflow: graph}, usage: USAGE})
    prepared(prepare('stop-wait-candidate'))
    identity = run(@routine, 'stop-during-timer', second_input.merge(first: true, second: true, wait1: 60000, wait2: 0), wait: false)
    first = wait_record(identity, 'wait1')
    before = detail(identity).dig('codeExecution', 'workflowRun', 'nodes').fetch('calculate')
    host.api('POST', "/api/agents/#{@owner}/turns/#{identity}/cancel")
    host.turn(identity, %w[cancelled interrupted])
    stopped = Contract.wait { row = detail(identity); row if row.fetch('workflowWaits').find { |w| w['id'] == first['id'] }['status'] == 'cancelled' }
    expect(stopped.dig('codeExecution', 'workflowRun', 'nodes').keys).not_to include('condition2', 'finish')
    expect(stopped.dig('codeExecution', 'workflowRun', 'nodes').fetch('calculate')).to eq(before)
    count = calls.length
    host.restart
    expect(calls.length).to eq(count)
    expect(detail(identity).fetch('workflowWaits')).to eq(stopped.fetch('workflowWaits'))

    Contract.wait { saved['analysis'] if saved.dig('analysis', 'status') == 'Ready' }
    prepared(prepare('deadline-candidate'))
    expired = run(@routine, 'expire-original-wall', second_input.merge(first: true, second: true, wait1: 60000, wait2: 0), wait: false)
    wait_record(expired, 'wait1')
    count = calls.length
    # Explicit original-deadline fixture on a real known wait, not a thirty-minute burn.
    edit_work(expired) { |work| work.fetch('codeExecution')['workflowDeadline'] = (Time.now.utc - 1).strftime('%Y-%m-%dT%H:%M:%S.%6N+00:00') }
    blocked = Contract.wait(timeout: 30) do
      row = detail(expired)
      row if row.fetch('workflowWaits').any? { |w| w['status'] == 'blocked' }
    end
    expect(blocked.fetch('workflowWaits').find { |w| w['node'] == 'wait1' }.fetch('reason')).to include('wall deadline expired')
    expect(blocked.dig('codeExecution', 'workflowRun', 'nodes').keys).not_to include('condition2', 'finish')
    host.restart
    expect(calls.length).to eq(count)
    expect(detail(expired).fetch('workflowWaits')).to eq(blocked.fetch('workflowWaits'))
  end

  it 'shows due, saved progress and the blocked or stopped wait in the native history', :desktop do
    prepared(prepare('visible-timer'))
    identity = run(@routine, 'visible-wait', second_input.merge(first: true, second: true, wait1: 60000, wait2: 0), wait: false)
    wait_record(identity, 'wait1')
    @desktop = Desktop.new(host.url)
    ['Open WaitOwner', 'Work menu', 'Automation menu', 'Routines', 'Waiting sales'].each { |label| @desktop.click(label) }
    @desktop.see('Fixture timing is simulated; authorized runs use real waits.')
    @desktop.see('Waiting steps')
    @desktop.see('wait1 · waiting')
    @desktop.see('Due:')
    @desktop.see('Progress: First checkpoint retained')
    @desktop.see('Next step: condition2')
    ['Stop this run…', 'Stop run'].each do |label|
      roles = %w[AXButton AXCheckBox AXRadioButton AXLink AXDisclosureTriangle AXPopUpButton]
      @desktop.control('scroll-to', '--path', @desktop.find(label, role: roles).fetch('path'))
      @desktop.click_node(@desktop.find(label, role: roles))
    end
    @desktop.see('wait1 · cancelled')
    @desktop.see('Stopped before wake')
    host.turn(identity, %w[cancelled interrupted])
    expect(calls.length).to eq(1)
  rescue Exception
    if @desktop
      directory = ENV.fetch('SAPIENS_WAIT_EVIDENCE', File.join(Contract::ROOT, 'tmp'))
      FileUtils.mkdir_p(directory)
      File.write(File.join(directory, 'workflow-wait-native-accessibility.json'), JSON.pretty_generate(@desktop.tree))
      File.write(File.join(directory, 'workflow-wait-native-detail.json'), JSON.pretty_generate(detail(identity))) if identity
      File.write(File.join(directory, 'workflow-wait-native-state.json'), JSON.pretty_generate(host.state))
    end
    raise
  end

end
