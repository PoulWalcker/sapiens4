require 'rbconfig'
require 'open3'
require_relative 'support/candidate_fixtures'
require_relative 'support/desktop'

RSpec.describe 'Owner preparation of replacement versions for a broken Routine' do
  include CandidateFixtures

  before(:all) do
    skip 'Selected generated-code runtime requires macOS ARM64' unless RUBY_PLATFORM.include?('darwin') && RbConfig::CONFIG.fetch('host_cpu').match?(/arm64|aarch64/)
    @docker = File.expand_path('~/.orbstack/bin/docker')
    @socket = 'unix://' + File.expand_path('~/.orbstack/run/docker.sock')
    output, error, status = Open3.capture3(@docker, '--host', @socket, 'ps', '--all', '--quiet', '--no-trunc')
    raise error unless status.success?
    @containers = output.lines.map(&:strip).sort
  end

  after(:all) do
    next unless @containers
    output, error, status = Open3.capture3(@docker, '--host', @socket, 'ps', '--all', '--quiet', '--no-trunc')
    raise error unless status.success?
    expect(output.lines.map(&:strip).sort).to eq(@containers)
  end

  before do
    repo = ENV.fetch('SAPIENS_TEST_REPOSITORY', Contract::ROOT)
    @host = Host.new(start: false, repo: repo, environment: {'SAPIENS_GENERATED_DOCKER' => @docker, 'SAPIENS_GENERATED_SOCKET' => @socket})
    host.configure(version: 'codex-cli 0.162.1', mode: 'Exec', model_catalog: [{slug: 'gpt-6-sol', shell_type: 'unified_exec', apply_patch_tool_type: 'freeform', experimental_supported_tools: [], tool_mode: 'direct', supports_search_tool: false, supported_reasoning_levels: [{effort: 'high', description: 'high'}]}])
    host.start
    initial = host.state
    expect(initial.fetch('agents').map { |row| row['name'] }).to eq(%w[ChiefSapi])
    @owner = host.agent('RepairOwner')
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", operation: 'create', title: 'Repairable sales', goal: CandidateFixtures::GOAL, criteria: CandidateFixtures::CRITERIA).fetch('routine')
    host.configure(rules: {
      'Execution' => {response: decision('Outcome', reply: 'Observed total5000.', outcome: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}), usage: {input_tokens: 19, output_tokens: 9}},
      'CodeAssessment' => {response: {verdict: 'accepted', reply: 'Actual sums satisfy the criteria.', reason: 'Actual body output matches original criteria.', evidence: ['Actual output.'], acceptance: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}, activationAudit: {accepted: true, reason: 'Independent fixtures and this authorized real result support this version.'}}, usage: {input_tokens: 73, output_tokens: 29}}
    })
    broken = candidate_source.sub('request = json.load(sys.stdin)', "request = json.load(sys.stdin)\nif request.get('force_failure'): raise ValueError('Business-only implementation fault')")
    preparation_rules(broken)
    host.turn(submit('observe'))
    @original = await_candidate
    host.turn(submit('activate'))
    @active = routine.fetch('activeCode')
    host.configure(rules: host.provider.fetch('rules').merge('LocalFallback' => {response: {resolved: true, result: {status: 'ok', value: {total_minor: 5000}}, reason: 'Use retained input to finish safely.'}, usage: {input_tokens: 31, output_tokens: 13}}))
    preparation_rules(candidate_source)
  end

  after { @host&.close }

  def path; "/api/agents/#{@owner}/routines/#{@routine}"; end
  def routine; host.state.fetch('personal_routines').find { |row| row['id'] == @routine }; end
  def repair; routine.fetch('repair'); end
  def submit(operation, input = observed_input)
    host.api('POST', "#{path}/run", operation: operation, revision: routine.fetch('revision'), inputs: JSON.generate(input)).fetch('run')
  end
  def fault(operation = 'fault')
    id = submit(operation, observed_input.merge(force_failure: true))
    host.turn(id)
    id
  end
  def wait_repair(status)
    Contract.wait(timeout: 100, description: "repair #{status}") do
      saved = repair
      raise JSON.pretty_generate(saved) if %w[interrupted exhausted stale declined structure_revision].include?(saved['status']) && saved['status'] != status
      saved if saved['status'] == status
    end
  end
  def preparation_rules(source, **generation)
    host.configure(rules: host.provider.fetch('rules').merge(
      'PreparationFixtures' => {response: {fixtures: independent_fixtures, reason: 'Independent held-out sales examples.'}, usage: {input_tokens: 41, output_tokens: 17}},
      'PreparationGeneration' => {response: {language: 'python', profile: 'python-3.13.15-linux-arm64', source: source, interface: 'json-stdin-result-v1', dependencies: [], capabilities: []}, usage: {input_tokens: 43, output_tokens: 19}}.merge(generation),
      'PreparationReview' => {response: {accepted: true, reason: 'The actual held-out results match all fixtures.'}, usage: {input_tokens: 47, output_tokens: 23}}
    ))
  end

  it 'regenerates directly and activates only its separately checked authorized replacement', :owner_regeneration do
    failed = fault
    ready = wait_repair('awaiting_verification')
    expect(ready.fetch('evidence').map { |entry| entry['run'] }).to include(failed)
    expect(host.api('GET', "#{path}/runs/#{failed}").dig('codeExecution', 'completedViaFallback')).to be(true)
    expect(submit('fault', observed_input.merge(force_failure: true))).to eq(failed)
    expect(ready.fetch('attempts').length).to eq(1)
    expect(ready.fetch('spentMilliseconds')).to be_between(1, 1_799_999)
    candidate = host.api('GET', "#{path}/preparations/#{ready.fetch('candidate')}")
    expect(candidate.fetch('checks')).to all(include('matched' => true))
    expect(candidate.fetch('operations').map { |op| op['purpose'] }).to include('repair_candidate_generation', 'repair_candidate_review')
    expect(routine.dig('activeCode', 'reference')).to eq(@original.dig('version', 'reference'))
    trial = submit('authorized-repair-check')
    host.turn(trial)
    recovered = wait_repair('recovered')
    expect(recovered.fetch('verificationRun')).to eq(trial)
    expect(routine.dig('activeCode', 'reference')).to eq(candidate.dig('version', 'reference'))
    expect(recovered.fetch('recoveredReference')).not_to eq(@original.dig('version', 'reference'))
    expect(routine.fetch('repairCases').length).to eq(1)
  end

  it 'prepares two owners independently and stops one without cancelling the other', :owner_regeneration do
    first_owner, first_routine = @owner, @routine
    @owner = host.agent('ParallelRepairOwner')
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", operation: 'parallel-create', title: 'Parallel sales', goal: CandidateFixtures::GOAL, criteria: CandidateFixtures::CRITERIA).fetch('routine')
    broken = candidate_source.sub('request = json.load(sys.stdin)', "request = json.load(sys.stdin)\nif request.get('force_failure'): raise ValueError('Business-only implementation fault')")
    preparation_rules(broken)
    host.turn(submit('parallel-observe'))
    await_candidate
    host.turn(submit('parallel-activate'))
    second_owner, second_routine = @owner, @routine
    preparation_rules(candidate_source, block: true, gate: 'parallel-regeneration')
    @owner, @routine = first_owner, first_routine
    fault('first-fault')
    @owner, @routine = second_owner, second_routine
    fault('second-fault')
    row_for = ->(identity) { host.state.fetch('personal_routines').find { |row| row['id'] == identity } }
    Contract.wait(timeout: 90) do
      [first_routine, second_routine].all? do |identity|
        saved = row_for.call(identity).fetch('repair', {})
        saved['status'] == 'repairing' && saved.fetch('reservations', []).any? { |receipt| receipt['status'] == 'reserved' }
      end && host.calls.count { |call| call['node'] == 'PreparationGeneration' && call['prompt'].match?(/"previousVersion"\s*:\s*\{/) } == 2
    end
    @owner, @routine = first_owner, first_routine
    first_case = repair
    host.api('POST', "#{path}/stop-repair", operation: 'stop-independent-first', revision: routine.fetch('revision'), case: first_case.fetch('id'), reason: 'Stop only this regeneration.')
    host.release('parallel-regeneration')
    @owner, @routine = second_owner, second_routine
    second_case = wait_repair('awaiting_verification')
    expect(row_for.call(first_routine).dig('repair', 'status')).to eq('stopped')
    expect(row_for.call(first_routine).dig('repair', 'id')).to eq(first_case.fetch('id'))
    host.restart
    expect(row_for.call(first_routine).dig('repair', 'status')).to eq('stopped')
    expect(repair.fetch('id')).to eq(second_case.fetch('id'))
  end

end
