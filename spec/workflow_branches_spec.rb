require 'open3'
require_relative 'support/candidate_fixtures'
require_relative 'support/desktop'

RSpec.describe 'Prepared finite workflow branches' do
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
    host.control(chief, 'corpora_update', revision: host.state.fetch('corpora').fetch('revision'), doctor_id: nil)
    @owner = host.agent('WorkflowOwner')
    rules('Execution' => {response: decision('Outcome', reply: 'Observed sales.', outcome: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}), usage: USAGE},
      'AutomationAnalysis' => {response: decision('AutomationAnalyzed', decision: {status: 'Ready', automatablePart: 'Compute sales with optional interpretation.', parameters: [{name: 'csv', description: 'Sales'}]}), usage: USAGE},
      'PreparationFixtures' => {response: {fixtures: workflow_fixtures}, usage: USAGE},
      'PreparationGeneration' => {response: {workflow: graph}, usage: USAGE},
      'PreparationReview' => {response: {accepted: true, reason: 'Composed independent results match both branches.'}, usage: USAGE},
      'WorkflowStep' => {response: expected_second, usage: USAGE},
      'CodeAssessment' => {response: {verdict: 'accepted', reply: 'Actual sales satisfy the original criteria.', reason: 'The whole composed result matches.', evidence: ['Actual selected node outputs.'], acceptance: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}, activationAudit: {accepted: true, reason: 'Own composed fixtures and authorized occurrence support this exact workflow.'}}, usage: USAGE})
    @routine = create('sales', 'Branched sales')
  end
  after { @desktop&.close; @host&.close }
  def rules(changes); host.configure(rules: host.provider.fetch('rules', {}).merge(changes)); end
  def route(id = @routine); "/api/agents/#{@owner}/routines/#{id}"; end
  def saved(id = @routine); host.state.fetch('personal_routines').find { |row| row['id'] == id }; end
  def create(operation, title)
    id = host.api('POST', "/api/agents/#{@owner}/routines", operation: operation, title: title, goal: CandidateFixtures::GOAL, criteria: CandidateFixtures::CRITERIA).fetch('routine')
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
    independent_fixtures + [{input: second_input.merge(model: true), expected: expected_second, modelStubs: {interpret: expected_second, summarize: expected_second}}]
  end
  def graph
    {entry: 'calculate', nodes: {
      calculate: {kind: 'script', source: candidate_source, input: {input: true}, next: 'predicate'},
      predicate: {kind: 'script', source: "import json,sys\nv=json.load(sys.stdin)\nprint(json.dumps({'value':v.get('model',False),'reason':'The accepted input explicitly selects optional interpretation.'}))\n", input: {input: true}, next: 'choice'},
      choice: {kind: 'branch', input: {output: 'predicate'}, true: 'interpret', false: 'finish_calculation'},
      interpret: {kind: 'model', instruction: 'Retain the supplied verified sales envelope unchanged.', input: {output: 'calculate'}, next: 'summarize'},
      summarize: {kind: 'model', instruction: 'Return the verified sales envelope unchanged; do not recompute.', input: {output: 'interpret'}, next: 'finish_model'},
      finish_model: {kind: 'finish', input: {output: 'summarize'}},
      finish_calculation: {kind: 'finish', input: {output: 'calculate'}}}}
  end

  it 'activates its own checked composite, retains completed branch across process restart and selects the other path without a new structure' do
    # Ordinary observation above is unchanged; a plain candidate remains valid too.
    rules('PreparationGeneration' => {response: {language: 'python', profile: 'python-3.13.15-linux-arm64', source: candidate_source, interface: 'json-stdin-result-v1', dependencies: [], capabilities: []}, usage: USAGE})
    plain = prepared(prepare('plain-compatibility'))
    expect(plain.dig('version', 'workflow')).to be_nil
    run(@routine, 'activate-plain', second_input)
    rules('PreparationGeneration' => {response: {workflow: graph}, usage: USAGE})
    Contract.wait { saved['analysis'] if saved.dig('analysis', 'status') == 'Ready' }
    candidate = prepared(prepare('prepare-workflow'))
    expect(candidate.fetch('checks')).to all(include('matched' => true))
    expect(calls.length).to eq(0) # model stubs belong only to independent fixtures.
    identity = run(@routine, 'activate-workflow', second_input.merge(model: true))
    result = detail(identity)
    expect(result.dig('codeExecution', 'activation', 'status')).to eq('activated')
    expect(result.dig('codeExecution', 'runtimeResult', 'actual')).to eq(JSON.parse(JSON.generate(expected_second)))
    choice = result.fetch('executionStages').find { |row| row['id'] == 'choice' }
    expect(choice).to include('status' => 'completed', 'selected' => 'interpret')
    expect(choice.fetch('input')).to include('value' => true, 'reason' => /explicitly selects/)
    expect(calls.length).to eq(2)
    pin = saved.dig('activeCode', 'reference')
    hash = candidate.dig('version', 'structureHash')
    host.restart
    expect(detail(identity).fetch('executionStages')).to eq(result.fetch('executionStages'))
    expect(calls.length).to eq(2)
    other = run(@routine, 'opposite-path', second_input.merge(model: false))
    stages = detail(other).fetch('executionStages')
    expect(stages.find { |row| row['id'] == 'choice' }).to include('selected' => 'finish_calculation')
    expect(stages.find { |row| row['id'] == 'interpret' }).to include('status' => 'skipped')
    expect(calls.length).to eq(2)
    expect(saved.dig('activeCode', 'reference')).to eq(pin)
    expect(detail(other).dig('codeExecution', 'structureHash')).to eq(hash)
  end

  it 'rejects invalid structure and selectors before runtime, holds unknown after restart and Stops before a successor' do
    invalid = graph
    invalid[:nodes][:calculate][:next] = 'missing'
    rules('PreparationGeneration' => {response: {workflow: invalid}, usage: USAGE})
    rejected = prepared(prepare('missing-reference'), status: 'failed')
    expect(rejected.fetch('checks')).to eq([])
    invalid = graph; invalid[:nodes][:predicate][:next] = 'calculate'
    rules('PreparationGeneration' => {response: {workflow: invalid}, usage: USAGE})
    expect(prepared(prepare('cycle'), status: 'failed').fetch('checks')).to eq([])
    invalid = graph; invalid[:nodes][:predicate][:input] = {output: 'summarize'}
    rules('PreparationGeneration' => {response: {workflow: invalid}, usage: USAGE})
    expect(prepared(prepare('future-output'), status: 'failed').fetch('checks')).to eq([])
    # Valid structure, but an unresolved real model predicate is never false.
    unknown = {entry: 'judge', nodes: {judge: {kind: 'model', instruction: 'Return the explicit condition with reason.', input: {input: true}, next: 'choice'}, choice: {kind: 'branch', input: {output: 'judge'}, true: 'yes', false: 'no'}, yes: {kind: 'finish', input: {literal: expected_second}}, no: {kind: 'finish', input: {literal: expected_second}}}}
    fixtures = independent_fixtures.map { |row| row.merge(expected: expected_second, modelStubs: {judge: {value: false, reason: 'Fixture-only condition.'}}) }
    rules('PreparationFixtures' => {response: {fixtures: fixtures}, usage: USAGE}, 'PreparationGeneration' => {response: {workflow: unknown}, usage: USAGE}, 'WorkflowStep' => {response: {value: nil, reason: 'Unresolved actual condition.'}, usage: USAGE})
    prepared(prepare('unknown-predicate'))
    identity = run(@routine, 'unknown-condition', second_input, wait: false)
    host.turn(identity, %w[failed interrupted])
    trace = detail(identity).dig('codeExecution', 'workflowRun', 'nodes')
    expect(trace.fetch('choice').fetch('status')).to eq('unknown')
    expect(trace.keys).not_to include('yes', 'no')
    count = calls.length
    host.restart
    expect(calls.length).to eq(count)
    expect(detail(identity).dig('codeExecution', 'workflowRun', 'nodes')).to eq(trace)
    other = create('stop-routine', 'Stopped workflow')
    rules('WorkflowStep' => {response: {value: true, reason: 'Late result.'}, usage: USAGE, block: true, gate: 'stop-node'})
    prepared(prepare('stop-candidate', other), other)
    stopped = run(other, 'stop-live-step', second_input, wait: false)
    Contract.wait { calls.length > count }
    host.api('POST', "#{route(other)}/stop", operation: 'stop-workflow', revision: saved(other).fetch('revision'), run: stopped)
    host.release('stop-node')
    host.turn(stopped, %w[interrupted cancelled failed])
    stopped_nodes = detail(stopped, other).dig('codeExecution', 'workflowRun', 'nodes')
    expect(stopped_nodes.keys).to eq(['judge'])
    host.restart
    expect(calls.length).to eq(count + 1)
    expect(detail(stopped, other).dig('codeExecution', 'workflowRun', 'nodes').keys).to eq(['judge'])
  end

  it 'shows selected and skipped steps with retained predicate input and reason in the native history', :desktop do
    prepared(prepare('visible-workflow'))
    run(@routine, 'visible-branch', second_input.merge(model: false))
    @desktop = Desktop.new(host.url)
    ['Open WorkflowOwner', 'Work menu', 'Automation menu', 'Routines', 'Branched sales'].each { |label| @desktop.click(label) }
    @desktop.see('Execution steps')
    @desktop.see('choice · Completed')
    @desktop.see('interpret · Skipped')
    @desktop.see('Selected branch: finish_calculation')
    @desktop.see('The accepted input explicitly selects optional interpretation.')
    @desktop.see('Predicate input:')
  end
end
