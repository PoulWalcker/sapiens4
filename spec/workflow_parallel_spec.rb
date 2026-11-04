require 'open3'
require 'sqlite3'
require_relative 'support/candidate_fixtures'
require_relative 'support/desktop'

RSpec.describe 'Shared-runtime finite workflow fork and join' do
  include CandidateFixtures
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
    @owner = host.agent('ParallelOwner')
    rules('Execution' => {response: decision('Outcome', reply: 'Observed sales.', outcome: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}), usage: USAGE},
      'PreparationFixtures' => {response: {fixtures: workflow_fixtures}, usage: USAGE},
      'PreparationGeneration' => {response: {workflow: graph}, usage: USAGE},
      'PreparationReview' => {response: {accepted: true, reason: 'Composed independent results match both branches.'}, usage: USAGE},
      'WorkflowStep' => {response: expected_second, usage: USAGE},
      'CodeAssessment' => {response: {verdict: 'accepted', reply: 'Actual sales satisfy the original criteria.', reason: 'The whole composed result matches.', evidence: ['Actual selected node outputs.'], acceptance: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}, activationAudit: {accepted: true, reason: 'Own composed fixtures and authorized occurrence support this exact workflow.'}}, usage: USAGE})
    @routine = create('sales', 'Parallel sales')
  end
  after { @desktop&.close; @host&.close }
  def rules(changes); host.configure(rules: host.provider.fetch('rules', {}).merge(changes)); end
  def route(id = @routine); "/api/agents/#{@owner}/routines/#{id}"; end
  def saved(id = @routine); host.state.fetch('personal_routines').find { |row| row['id'] == id }; end
  def create(operation, title)
    id = host.api('POST', "/api/agents/#{@owner}/routines", operation: operation, title: title, goal: CandidateFixtures::GOAL, criteria: CandidateFixtures::CRITERIA).fetch('routine')
    id
  end
  def run(id, operation, input, wait: true)
    identity = host.api('POST', "#{route(id)}/run", operation: operation, revision: saved(id).fetch('revision'), inputs: JSON.generate(input)).fetch('run')
    host.turn(identity) if wait
    identity
  end
  def prepare(operation, id = @routine)
    if saved(id).fetch('runs').empty?
      run(id, "observe-#{operation}", observed_input)
      Contract.wait(description: 'automatic workflow preparation admission') { saved(id).dig('preparation', 'id') }
    else
      host.api('POST', "#{route(id)}/prepare", operation: operation, revision: saved(id).fetch('revision')).fetch('preparation')
    end
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
      row.merge(modelStubs: {left: row.fetch(:expected), rightA: row.fetch(:expected), rightB: row.fetch(:expected)})
    end
  end
  def graph
    {entry: 'calculate', nodes: {
      calculate: {kind: 'script', source: candidate_source, input: {input: true}, next: 'split'},
      split: {kind: 'fork', children: ['left', 'rightFork'], join: 'joined'},
      left: {kind: 'model', instruction: 'Return the verified sales envelope unchanged.', input: {output: 'calculate'}, next: 'joined'},
      rightFork: {kind: 'fork', children: ['rightA', 'rightB'], join: 'rightJoin'},
      rightA: {kind: 'model', instruction: 'Return the verified sales envelope unchanged.', input: {output: 'calculate'}, next: 'rightJoin'},
      rightB: {kind: 'model', instruction: 'Return the verified sales envelope unchanged.', input: {output: 'calculate'}, next: 'rightJoin'},
      rightJoin: {kind: 'join', fork: 'rightFork', input: {array: [{output: 'rightA'}, {output: 'rightB'}]}, next: 'right'},
      right: {kind: 'script', source: "import json,sys\nv=json.load(sys.stdin)\nassert v[0]==v[1]\nprint(json.dumps(v[0]))\n", input: {output: 'rightJoin'}, next: 'joined'},
      joined: {kind: 'join', fork: 'split', input: {object: {calculation: {output: 'calculate'}, children: {array: [{output: 'left'}, {output: 'right'}]}}}, next: 'combine'},
      combine: {kind: 'script', source: "import json,sys\nv=json.load(sys.stdin)\nassert all(x==v['calculation'] for x in v['children'])\nprint(json.dumps(v['calculation']))\n", input: {output: 'joined'}, next: 'finish'},
      finish: {kind: 'finish', input: {output: 'combine'}}}}
  end

  it 'checks and activates a nested ordered join, releases parent capacity and retains completed children across restart' do
    accepted = {accepted: true, reason: 'Whole composed actual fixtures agree.'}
    rules('PreparationReview' => {attempts: [{terminal_failure: 'unexpected status 503 Service Unavailable: controlled post-join review refusal'}, {response: accepted, usage: USAGE}]})
    candidate = prepared(prepare('nested-preparation'))
    expect(candidate.fetch('checks').length).to eq(3)
    expect(candidate.fetch('maintenanceBudget').fetch('reservations').length).to eq(2) # one before wait, one resumed segment; never one per child.
    expect(candidate.fetch('checks')).to all(include('matched' => true))
    rules('WorkflowStep' => {response: expected_second, usage: USAGE, event_delay: 2, variants: [
      {contains: '"step":"left"', block: true, gate: 'left'},
      {contains: '"step":"rightA"', block: true, gate: 'rightA'},
      {contains: '"step":"rightB"', block: true, gate: 'rightB'}]})
    identity = run(@routine, 'parallel-trial', second_input, wait: false)
    begin
      Contract.wait(timeout: 60) { calls.length == 2 }
    rescue RuntimeError
      File.write('/tmp/sapiens21-join-diagnostic.json', JSON.pretty_generate(detail: detail(identity), calls: calls))
      raise
    end
    pending = detail(identity)
    expect(pending.fetch('status')).to eq('queued')
    expect(host.state.fetch('run_activity').keys).not_to include(identity)
    expect(pending.dig('workflowParallel', 'joins')).to include(include('id' => 'joined', 'status' => 'waiting'))
    independent = host.agent('IndependentOwner')
    rules("#{independent}:Assessing" => {response: decision('FitsSpecialization', mode: 'Repl', reply: 'Independent work remains available.'), usage: USAGE})
    host.turn(host.submit(independent, 'Complete independent work while another owner waits for its join.'))
    host.release('left')
    Contract.wait(timeout: 60) { calls.length == 3 }
    left = Contract.wait { row = detail(identity).dig('workflowParallel', 'children').find { |c| c['entry'] == 'left' }; row if row && row['status'] == 'Completed' }
    expect(left.fetch('id')).not_to be_empty
    host.release('rightA'); host.release('rightB')
    host.turn(identity, 'done', timeout: 90)
    result = detail(identity)
    expect(result.dig('codeExecution', 'activation', 'status')).to eq('activated')
    expect(result.dig('codeExecution', 'runtimeResult', 'actual')).to eq(JSON.parse(JSON.generate(expected_second)))
    expect(result.dig('workflowParallel', 'children')).to all(include('status' => 'Completed'))
    expect(result.dig('workflowParallel', 'resourceStatus')).to eq('settled')
    expect(result.dig('workflowParallel', 'activeMilliseconds')).to be > 0
    model_milliseconds = result.fetch('codeExecution').fetch('operations').select { |op| op['purpose'] == 'workflow_response' }.sum { |op| op.dig('evidence', 'durationMs') || 0 }
    expect(result.dig('workflowParallel', 'activeMilliseconds')).to be < model_milliseconds # known overlap is charged once, not summed.
    expect(calls.length).to eq(3)
    host.restart
    expect(calls.length).to eq(3)
    expect(detail(identity).dig('workflowParallel', 'children')).to eq(result.dig('workflowParallel', 'children'))
    expect(saved.dig('activeCode', 'reference')).to eq(candidate.dig('version', 'reference'))
  end
  def edit_work(identity)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.busy_timeout = 5000
      value = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', identity))
      yield value
      db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(value), identity])
    end
  end

  it 'cancels queued and live descendants, keeps completed receipts and enforces the last original preparation resource without a fresh child allowance' do
    prepared(prepare('stop-preparation'))
    rules('WorkflowStep' => {response: expected_second, usage: USAGE, variants: [
      {contains: '"step":"rightA"', block: true, gate: 'stop-rightA'},
      {contains: '"step":"rightB"', block: true, gate: 'stop-rightB'}]})
    identity = run(@routine, 'cancel-waiting-parent', second_input, wait: false)
    Contract.wait(timeout: 60) { calls.length == 3 }
    completed = Contract.wait { row = detail(identity).dig('workflowParallel', 'children').find { |c| c['entry'] == 'left' }; row if row && row['status'] == 'Completed' }
    host.api('POST', "/api/agents/#{@owner}/turns/#{identity}/cancel")
    host.release('stop-rightA'); host.release('stop-rightB')
    host.turn(identity, %w[cancelled interrupted])
    final = Contract.wait(timeout: 30) { row = detail(identity); row if row.dig('workflowParallel', 'resourceStatus') == 'settled' }
    expect(final.dig('workflowParallel', 'children')).to include(completed)
    expect(final.dig('workflowParallel', 'children').none? { |row| %w[Queued Running].include?(row['status']) }).to be(true)
    expect(saved['activeCode']).to be_nil
    host.restart
    expect(calls.length).to eq(3)
    expect(detail(identity).dig('workflowParallel', 'children')).to include(completed)

    other = create('last-resource', 'Last finite resource')
    accepted = {accepted: true, reason: 'Retained composed fixtures match.'}
    rules('PreparationReview' => {attempts: [{terminal_failure: 'unexpected status 503 Service Unavailable: controlled post-join refusal'}, {response: accepted, usage: USAGE}]})
    candidate = prepare('last-resource-preparation', other)
    waiting = prepared(candidate, other, status: 'waiting_provider')
    expect(host.request('POST', "#{route(other)}/prepare", operation: 'competing-preparation', revision: saved(other).fetch('revision')).first).to eq(409)
    host.stop
    ceiling = nil
    # Explicit persisted finite-resource fixture, not a thirty-minute burn or grant.
    edit_work(candidate) do |work|
      budget = work.fetch('maintenanceBudget')
      ceiling = budget.fetch('spentMilliseconds') + 1
      budget['limitMilliseconds'] = ceiling
    end
    reviews = host.calls.count { |call| call['node'] == 'PreparationReview' }
    host.start
    exhausted = prepared(candidate, other, status: 'failed')
    expect(exhausted.fetch('maintenanceBudget').fetch('limitMilliseconds')).to eq(ceiling)
    expect(exhausted.fetch('maintenanceBudget').fetch('spentMilliseconds')).to eq(ceiling)
    expect(exhausted.fetch('maintenanceBudget').fetch('reservations').none? { |r| r['status'] == 'reserved' }).to be(true)
    expect(host.calls.count { |call| call['node'] == 'PreparationReview' }).to eq(reviews)
    expect(calls.length).to eq(3)
    host.restart
    expect(calls.length).to eq(3)
  end

  it 'shows child states, a waiting join and separate union activity in the native history', :desktop do
    prepared(prepare('visible-parallel'))
    rules('WorkflowStep' => {response: expected_second, usage: USAGE, block: true, gate: 'visible-children'})
    identity = run(@routine, 'visible-join', second_input, wait: false)
    Contract.wait(timeout: 60) { calls.length == 2 }
    @desktop = Desktop.new(host.url)
    @desktop.click('Open ParallelOwner')
    @desktop.see('Message ParallelOwner')
    @desktop.click('Work menu')
    @desktop.see('Automation menu')
    @desktop.click('Automation menu')
    @desktop.see('Routines')
    @desktop.click('Routines')
    @desktop.see('Parallel sales')
    @desktop.click('Parallel sales')
    @desktop.see('Workflow resource and child steps')
    @desktop.see('Join joined · waiting')
    @desktop.see('Child left · Running')
    @desktop.see('Active time (union):')
    @desktop.see('Provider usage is summed separately')
    host.release('visible-children')
    host.turn(identity, 'done', timeout: 90)
    @desktop.reload
    @desktop.click('Parallel sales')
    @desktop.see('Join joined · ready')
    @desktop.see('Child left · Completed')
  end

end
