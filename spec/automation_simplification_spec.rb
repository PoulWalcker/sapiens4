require 'yaml'
require_relative 'support/candidate_fixtures'
require_relative 'support/host'

RSpec.describe 'Unified owner preparation changed behavior' do
  include CandidateFixtures
  before do
    repo = ENV.fetch('SAPIENS_TEST_REPOSITORY', Contract::ROOT)
    @host = Host.new(start: false, repo: repo, environment: {
      'SAPIENS_GENERATED_DOCKER' => File.expand_path('~/.orbstack/bin/docker'),
      'SAPIENS_GENERATED_SOCKET' => 'unix://' + File.expand_path('~/.orbstack/run/docker.sock')})
    host.configure(version: 'codex-cli 0.162.1', mode: 'Exec', model_catalog: [{slug: 'gpt-6-sol', shell_type: 'unified_exec', apply_patch_tool_type: 'freeform', experimental_supported_tools: [], tool_mode: 'direct', supports_search_tool: false, supported_reasoning_levels: [{effort: 'high', description: 'high'}]}])
    host.start
    @owner = host.agent('UnifiedOwner')
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", operation: 'create', title: 'Exact sales', goal: CandidateFixtures::GOAL, criteria: CandidateFixtures::CRITERIA).fetch('routine')
    rules('Execution' => {response: decision('Outcome', reply: 'Observed total5000.', outcome: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []})},
      'PreparationFixtures' => {response: {fixtures: independent_fixtures}},
      'PreparationGeneration' => {response: {language: 'python', profile: 'python-3.13.15-linux-arm64', source: candidate_source, interface: 'json-stdin-result-v1', dependencies: [], capabilities: []}},
      'PreparationReview' => {response: {accepted: true, reason: 'Independent examples matched.'}},
      'CodeAssessment' => {response: accepted_result})
  end
  after do
    if RSpec.current_example.exception && @host
      dir = ENV.fetch('SAPIENS_SIMPLIFICATION_EVIDENCE', '/tmp/sapiens-simplification-failures')
      FileUtils.mkdir_p(dir)
      prefix = File.join(dir, RSpec.current_example.metadata[:line_number].to_s)
      File.write(prefix + '-state.json', JSON.pretty_generate(host.state)) rescue nil
      File.write(prefix + '-calls.json', JSON.pretty_generate(host.calls)) rescue nil
    end
    @host&.close
  end
  def rules(changes); host.configure(rules: host.provider.fetch('rules', {}).merge(changes.transform_values { |rule| {usage: {input_tokens: 19, output_tokens: 9}}.merge(rule) })); end
  def path; "/api/agents/#{@owner}/routines/#{@routine}"; end
  def routine; host.state.fetch('personal_routines').find { |r| r['id'] == @routine }; end
  def calls(node); host.calls.select { |c| c['node'] == node }; end
  def run(operation, input = observed_input); host.api('POST', "#{path}/run", operation: operation, revision: routine.fetch('revision'), inputs: JSON.generate(input)).fetch('run'); end
  def preparation(status)
    Contract.wait(timeout: 90, description: "preparation #{status}") do
      p = routine['preparation']; p if p && p['status'] == status
    end
  end
  def accepted_result
    {verdict: 'accepted', reply: 'Exact sales accepted.', reason: 'Actual result matches original criteria.', evidence: ['Actual complete result.'], acceptance: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}, activationAudit: {accepted: true, reason: 'Independent checks and authorized occurrence agree.'}}
  end
  def assert_current_owner
    expect(host.state.fetch('agents').map { |a| a['name'] }).to contain_exactly('ChiefSapi', 'UnifiedOwner')
  end

  it 'automatically admits once, retains suitability across restart, and Stop fences retries on the same allowance' do
    rules('PreparationFixtures' => {response: {status: 'insufficient_evidence', reason: 'Need a representative second input.'}})
    host.turn(run('observe'))
    first = preparation('insufficient_evidence')
    expect(first['reason']).to eq('Need a representative second input.')
    expect(calls('PreparationGeneration')).to be_empty
    assert_current_owner
    host.restart
    pulse = host.state.dig('pulse', 'numbers', 'bpm60')
    Contract.wait { host.state.dig('pulse', 'numbers', 'bpm60') > pulse + 2 }
    expect(routine.dig('preparation', 'id')).to eq(first['id'])
    expect(calls('PreparationFixtures').length).to eq(1)
    rules('PreparationFixtures' => {response: {status: 'unsuitable', reason: 'No reliable deterministic subset.'}})
    second_id = host.api('POST', "#{path}/prepare", operation: 'explicit-reconsider', revision: routine['revision']).fetch('preparation')
    second = preparation('unsuitable')
    expect(second['id']).to eq(second_id)
    expect(second['maintenanceBudget']['limitMilliseconds']).to eq(first['maintenanceBudget']['limitMilliseconds'])
    expect(second['maintenanceBudget']['spentMilliseconds']).to be >= first['maintenanceBudget']['spentMilliseconds']
    rules('PreparationFixtures' => {response: {fixtures: independent_fixtures}, block: true, gate: 'stop-preparation'})
    third = host.api('POST', "#{path}/prepare", operation: 'explicit-third', revision: routine['revision']).fetch('preparation')
    Contract.wait { calls('PreparationFixtures').length == 3 }
    host.api('POST', "#{path}/stop-preparation", operation: 'stop-third', revision: routine['revision'], preparation: third)
    preparation('stopped')
    host.api('POST', "#{path}/prepare", operation: 'must-not-refill', revision: routine['revision'], status: 409)
    host.restart
    pulse = host.state.dig('pulse', 'numbers', 'bpm60')
    Contract.wait { host.state.dig('pulse', 'numbers', 'bpm60') > pulse + 2 }
    expect(calls('PreparationFixtures').length).to eq(3)
    expect(calls('PreparationGeneration')).to be_empty
  end

  it 'retains an unsuitable verdict across new ordinary results but allows explicit reconsideration' do
    rules('PreparationFixtures' => {response: {status: 'unsuitable', reason: 'This contract requires live browser observation.'}})
    host.turn(run('first-observation'))
    first = preparation('unsuitable')
    host.turn(run('second-observation'))
    pulse = host.state.dig('pulse', 'numbers', 'bpm60')
    Contract.wait { host.state.dig('pulse', 'numbers', 'bpm60') > pulse + 2 }
    expect(calls('Execution').length).to eq(2)
    expect(calls('PreparationFixtures').length).to eq(1)
    expect(routine.dig('preparation', 'id')).to eq(first['id'])
    second_id = host.api('POST', "#{path}/prepare", operation: 'explicit-reconsider-unsuitable', revision: routine['revision']).fetch('preparation')
    Contract.wait { routine.dig('preparation', 'id') == second_id && routine.dig('preparation', 'status') == 'unsuitable' }
    expect(second_id).not_to eq(first['id'])
    expect(routine.dig('preparation', 'maintenanceBudget', 'limitMilliseconds')).to eq(first.dig('maintenanceBudget', 'limitMilliseconds'))
    expect(routine.dig('preparation', 'maintenanceBudget', 'spentMilliseconds')).to be >= first.dig('maintenanceBudget', 'spentMilliseconds')
    expect(calls('PreparationFixtures').length).to eq(2)
    expect(calls('PreparationGeneration')).to be_empty
  end

  it 'reassesses the retained exact result without regeneration or another business execution' do
    host.turn(run('observe'))
    first = preparation('awaiting_occurrence')
    rules('CodeAssessment' => {response: {unexpected: 'invalid response'}, block: true, gate: 'invalid-assessment'},
      'CodeAssessmentCorrection' => {response: {unexpected: 'still invalid'}})
    original = run('trial')
    Contract.wait { calls('CodeAssessment').length == 1 }
    rules('CodeAssessment' => {response: accepted_result})
    host.release('invalid-assessment')
    host.turn(original, %w[failed done], timeout: 60)
    recovered = Contract.wait(timeout: 90) { value = routine['repair']; value if value && value['status'] == 'recovered' }
    expect(recovered['kind']).to eq('saved_result_audit')
    expect(recovered['auditSource']).to eq(original)
    expect(recovered['recoveredReference']).to eq(first['reference'])
    detail = host.api('GET', "#{path}/runs/#{original}")
    expect(detail.fetch('codeExecution').fetch('operations').count { |op| op['purpose'] == 'code_execution' }).to eq(1)
    expect(calls('PreparationGeneration').length).to eq(1)
    expect(calls('CodeAssessment').length).to eq(2)
    assert_current_owner
  end

  it 'uses common standalone construction with a conserved allowance across versions and original task completion' do
    contract = {objective: CandidateFixtures::GOAL, inputs: [JSON.generate(observed_input)], expectedOutputs: ['Exact sales result.'], completionCriteria: CandidateFixtures::CRITERIA}
    graph = {entry: 'calculate', nodes: {calculate: {kind: 'script', source: candidate_source, input: {input: true}, next: 'finish'}, finish: {kind: 'finish', input: {output: 'calculate'}}}}
    rules('ReadyToWork' => {response: decision('TaskPrepared', taskType: 'work', specification: contract)},
      'Execution' => {response: decision('ComposeWorkflow', operation: 'first-version')},
      'PreparationGeneration' => {response: {workflow: graph}})
    original = host.submit(@owner, 'Calculate these sales.')
    host.turn(original, 'done', timeout: 90)
    first = host.api('GET', "/api/agents/#{@owner}/workflows").first
    expect(first.dig('trial', 'status')).to eq('activated')
    rules('Execution' => {response: decision('ComposeWorkflow', operation: 'second-version', previous: first.fetch('reference'))})
    second_task = host.submit(@owner, 'Calculate these sales again with another checked version.')
    host.turn(second_task, 'done', timeout: 90)
    versions = host.api('GET', "/api/agents/#{@owner}/workflows")
    expect(versions.length).to eq(2)
    second = versions.find { |v| v['id'] != first['id'] }
    expect(second.dig('source', 'workload')).to eq(original)
    expect(second.dig('maintenanceBudget', 'limitMilliseconds')).to eq(first.dig('maintenanceBudget', 'limitMilliseconds'))
    expect(second.dig('maintenanceBudget', 'spentMilliseconds')).to be > first.dig('maintenanceBudget', 'spentMilliseconds')
    expect(second.dig('trial', 'status')).to eq('activated')
    tasks = YAML.safe_load(host.api('GET', "/api/agents/#{@owner}/tasks")).fetch('tasks')
    [original, second_task].each { |id| expect(tasks.find { |t| t['workloadId'] == id }.dig('outcome', 'status')).to eq('Completed') }
    assert_current_owner
  end
  it 'prepares, activates and replaces a faulty version through the current owner flow from empty data' do
    broken = candidate_source.sub('request = json.load(sys.stdin)', "request = json.load(sys.stdin)\nif request.get('force_failure'): raise ValueError('Business-only implementation fault')")
    generation = {language: 'python', profile: 'python-3.13.15-linux-arm64', source: broken, interface: 'json-stdin-result-v1', dependencies: [], capabilities: []}
    rules('PreparationGeneration' => {response: generation})
    host.turn(run('observe'))
    first = preparation('awaiting_occurrence')
    host.turn(run('activate'))
    expect(routine.dig('activeCode', 'reference')).to eq(first.fetch('reference'))
    rules('PreparationGeneration' => {response: generation.merge(source: candidate_source)},
      'LocalFallback' => {response: {resolved: true, result: {status: 'ok', value: {total_minor: 5000}}, reason: 'Retained input safely completes this occurrence.'}})
    failed = run('business-fault', observed_input.merge(force_failure: true))
    host.turn(failed)
    repair = Contract.wait(timeout: 90) { r = routine['repair']; r if r && r['status'] == 'awaiting_verification' }
    expect(repair.fetch('evidence').map { |e| e['run'] }).to include(failed)
    replacement = host.api('GET', "#{path}/preparations/#{repair.fetch('candidate')}")
    expect(replacement.fetch('checks')).to all(include('matched' => true))
    expect(routine.dig('activeCode', 'reference')).to eq(first.fetch('reference'))
    host.api('POST', "#{path}/repair-grant", operation: 'no-unneeded-refill', revision: routine['revision'], case: repair['id'], minutes: 1, reason: 'A fresh active case must not receive another allowance.', status: 409)
    authorized = run('activate-replacement')
    host.turn(authorized)
    recovered = Contract.wait { r = routine['repair']; r if r && r['status'] == 'recovered' }
    expect(recovered['verificationRun']).to eq(authorized)
    expect(routine.dig('activeCode', 'reference')).to eq(replacement.dig('version', 'reference'))
    expect(recovered['recoveredReference']).not_to eq(first.fetch('reference'))
    expect(calls('PreparationGeneration').length).to eq(2)
    assert_current_owner
  end

end
