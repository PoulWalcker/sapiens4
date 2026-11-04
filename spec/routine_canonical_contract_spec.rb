require_relative 'support/candidate_fixtures'

RSpec.describe 'Canonical contract for a Sapi-created routine' do
  include CandidateFixtures

  after { @host&.close }

  def path; "/api/agents/#{@owner}/routines/#{@routine}"; end
  def routine; host.state.fetch('personal_routines').find { |row| row['id'] == @routine }; end
  def calls(node); host.calls.select { |call| call['node'] == node }; end
  def run(operation, input = nil)
    fields = {operation: operation, revision: routine.fetch('revision')}
    fields[:inputs] = JSON.generate(input) unless input.nil?
    host.api('POST', "#{path}/run", **fields).fetch('run')
  end

  it 'pins the first full contract and reuses checked code with fresh inputs without another TaskPrepared' do
    @host = Host.new(start: false, repo: ENV.fetch('SAPIENS_TEST_REPOSITORY', Contract::ROOT), environment: {
      'SAPIENS_GENERATED_DOCKER' => File.expand_path('~/.orbstack/bin/docker'),
      'SAPIENS_GENERATED_SOCKET' => 'unix://' + File.expand_path('~/.orbstack/run/docker.sock')})
    @conditional = 'If input is malformed, report the rejected input.'
    criteria = CandidateFixtures::CRITERIA + [@conditional]
    contract = {objective: CandidateFixtures::GOAL, inputs: [JSON.generate(observed_input)],
      expectedOutputs: ['JSON with exact total_minor, by_category_minor, row_count, start and end.'],
      completionCriteria: criteria, conditionalCriteria: {@conditional => 'Input is malformed.'}}
    outcome = {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA,
      notApplicableCriteria: {@conditional => 'This occurrence supplies valid input.'}, artifacts: []}
    result_contract = {input: 'Object with csv string and ISO start/end date strings.',
      output: 'ok.value object: start/end strings, row_count integer, by_category_minor object of integer sums, total_minor integer; rejected.error string.', mode: 'script'}
    rules = {
      'ReadyToWork' => {response: decision('TaskPrepared', taskType: 'work', specification: contract)},
      'Execution' => {response: decision('Outcome', reply: 'Observed total 5000.', outcome: outcome)},
      'PreparationFixtures' => {response: {resultContract: result_contract, fixtures: independent_fixtures}},
      'PreparationGeneration' => {response: {language: 'python', profile: 'python-3.13.15-linux-arm64', source: candidate_source,
        interface: 'json-stdin-result-v1', dependencies: [], capabilities: []}},
      'PreparationReview' => {response: {accepted: true, reason: 'Independent examples match the complete contract.'}},
      'CodeAssessment' => {response: {verdict: 'accepted', reply: 'Exact sales accepted.', reason: 'Actual result matches pinned criteria.',
        evidence: ['Observed complete code result.'], acceptance: outcome,
        activationAudit: {accepted: true, reason: 'Authorized occurrence and checks agree.'}}}}
    host.configure(version: 'codex-cli 0.162.1', mode: 'Exec', model_catalog: [{slug: 'gpt-6-sol', shell_type: 'unified_exec',
      apply_patch_tool_type: 'freeform', experimental_supported_tools: [], tool_mode: 'direct', supports_search_tool: false,
      supported_reasoning_levels: [{effort: 'high', description: 'high'}]}],
      rules: rules.transform_values { |rule| rule.merge(usage: {input_tokens: 19, output_tokens: 9}) })
    host.start
    @owner = host.agent('CanonicalOwner')
    @routine = host.control(@owner, 'routine_create', minutes: 60, prompt: CandidateFixtures::GOAL).fetch('routine').fetch('id')
    first = run('first-observation')
    host.turn(first)
    prepared = await_candidate
    expect(prepared.fetch('contract')).to eq(JSON.parse(JSON.generate(contract)))
    %w[PreparationFixtures PreparationGeneration].each do |node|
      context = JSON.parse(calls(node).fetch(0).fetch('prompt').split("\nContext:\n", 2).fetch(1))
      expect(context.fetch('effectiveInput')).to eq(JSON.parse(JSON.generate(observed_input)))
    end
    generation_context = JSON.parse(calls('PreparationGeneration').fetch(0).fetch('prompt').split("\nContext:\n", 2).fetch(1))
    expect(generation_context.fetch('resultContract')).to eq(JSON.parse(JSON.generate(result_contract)))
    expect(generation_context.keys).not_to include('fixtures', 'examples', 'expected')
    expect(JSON.generate(generation_context)).not_to include(CandidateFixtures::HELDOUT.to_json[1...-1])
    expect(calls('ReadyToWork').length).to eq(1)
    candidate = prepared.fetch('version').fetch('reference')
    host.restart
    trial = run('authorize-trial')
    host.turn(trial)
    expect(routine.dig('activeCode', 'reference')).to eq(candidate)
    fresh = {csv: CandidateFixtures::HELDOUT, start: '2026-12-05', end: '2026-12-06'}
    subsequent = run('fresh-input', fresh)
    host.turn(subsequent)
    detail = host.api('GET', "#{path}/runs/#{subsequent}")
    expect(detail.dig('snapshot', 'contractGeneration')).to eq(1)
    expect(detail.dig('codeExecution', 'runtimeResult', 'actual')).to eq(
      'status' => 'ok', 'value' => {'start' => fresh[:start], 'end' => fresh[:end], 'row_count' => 3,
        'by_category_minor' => {'tools' => 200, 'art' => 0}, 'total_minor' => 200})
    expect(calls('ReadyToWork').length).to eq(1)
    expect(calls('Execution').length).to eq(1)
    expect(calls('PreparationGeneration').length).to eq(1)
    expect(routine.dig('activeCode', 'reference')).to eq(candidate)
    host.api('POST', "#{path}/edit", operation: 'changed-contract', revision: routine.fetch('revision'),
      title: 'Changed sales contract', goal: 'Return only nonnegative category totals.', criteria: ['Return nonnegative category totals.'])
    expect(routine.fetch('activeCode')).to include('status' => 'stale', 'reference' => candidate)
  end
end
