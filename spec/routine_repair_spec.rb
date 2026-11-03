require 'rbconfig'
require 'open3'
require 'sqlite3'
require_relative 'support/candidate_fixtures'
require_relative 'support/desktop'

RSpec.describe 'One durable adaptive Doctor case for a broken personal Routine' do
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
    @host = Host.new(start: false, environment: {'SAPIENS_GENERATED_DOCKER' => @docker, 'SAPIENS_GENERATED_SOCKET' => @socket})
    host.configure(version: 'codex-cli 0.162.1')
    host.start
    initial = host.state
    @doctor = initial.fetch('corpora').fetch('doctor_id')
    expect(initial.fetch('agents').map { |row| row['name'] }).to eq(%w[ChiefSapi Doctor])
    expect(initial.fetch('agents').find { |row| row['id'] == @doctor }).to include('role' => 'System diagnostics and authorized recovery')
    expect(@doctor).not_to eq(initial.fetch('main_agent_id'))
    host.control(chief, 'corpora_update', revision: initial.fetch('corpora').fetch('revision'), doctor_id: nil)
    @owner = host.agent('RepairOwner')
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", operation: 'create', title: 'Repairable sales', goal: CandidateFixtures::GOAL, criteria: CandidateFixtures::CRITERIA).fetch('routine')
    host.configure(rules: {
      'Execution' => {response: decision('Outcome', reply: 'Observed total5000.', outcome: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []})},
      'AutomationAnalysis' => {response: decision('AutomationAnalyzed', decision: {status: 'Ready', automatablePart: 'Compute supplied sales.', parameters: [{name: 'csv', description: 'Sales rows'}]})},
      'CodeAssessment' => {response: {verdict: 'accepted', reply: 'Actual sums satisfy the criteria.', reason: 'Actual body output matches original criteria.', evidence: ['Actual output.'], acceptance: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}, activationAudit: {accepted: true, reason: 'Independent fixtures and this authorized real result support this version.'}}, usage: {input_tokens: 73, output_tokens: 29}}
    })
    host.turn(submit('observe'))
    analysis = Contract.wait { routine['analysis'] if routine.dig('analysis', 'status') == 'Ready' }
    broken = candidate_source.sub('request = json.load(sys.stdin)', "request = json.load(sys.stdin)\nif request.get('force_failure'): raise ValueError('Business-only implementation fault')")
    preparation_rules(broken)
    id = host.api('POST', "#{path}/prepare", operation: 'prepare', revision: 1, analysis: analysis.fetch('id')).fetch('preparation')
    @original = Contract.wait(timeout: 90) do
      row = host.api('GET', "#{path}/preparations/#{id}")
      raise JSON.pretty_generate(row) if %w[failed interrupted stopped].include?(row['status'])
      row if row['status'] == 'awaiting_occurrence'
    end
    host.turn(submit('activate'))
    @active = routine.fetch('activeCode')
    host.configure(rules: host.provider.fetch('rules').merge('LocalFallback' => {response: {resolved: true, result: {status: 'ok', value: {total_minor: 5000}}, reason: 'Use retained input to finish safely.'}, usage: {input_tokens: 31, output_tokens: 13}}))
    preparation_rules(candidate_source)
    diagnosis_rule
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
      saved if saved['status'] == status && (status != 'waiting_doctor' || saved['waitingReason'])
    end
  end
  def preparation_rules(source, **generation)
    host.configure(rules: host.provider.fetch('rules').merge(
      'PreparationFixtures' => {response: {fixtures: independent_fixtures, reason: 'Independent held-out sales examples.'}, usage: {input_tokens: 41, output_tokens: 17}},
      'PreparationGeneration' => {response: {language: 'python', profile: 'python-3.13.15-linux-arm64', source: source, interface: 'json-stdin-result-v1', dependencies: [], capabilities: []}, usage: {input_tokens: 43, output_tokens: 19}}.merge(generation),
      'PreparationReview' => {response: {accepted: true, reason: 'The actual held-out results match all fixtures.'}, usage: {input_tokens: 47, output_tokens: 23}}
    ))
  end
  def diagnosis_rule(response = {}, **options)
    base = {verdict: 'implementation_defect', diagnosis: 'The input-specific branch throws before aggregation.', recommendation: 'Remove that branch and check the unchanged contract.', hypothesis: 'Remove the force_failure branch while preserving complete CSV validation.'}
    host.configure(rules: host.provider.fetch('rules').merge('DoctorDiagnosis' => {response: base.merge(response), usage: {input_tokens: 59, output_tokens: 31}}.merge(options)))
  end
  def assign_doctor
    host.control(chief, 'corpora_update', revision: host.state.fetch('corpora').fetch('revision'), doctor_id: @doctor)
  end
  def saved_definition
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      rowid, value = db.get_first_row('SELECT sequence,value FROM scheduled_routines WHERE json_extract(value,\'$.id\')=?', @routine)
      definition = JSON.parse(value)
      yield definition
      db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?', [JSON.generate(definition), rowid])
    end
  end

  it 'creates the case before Doctor availability, keeps fallback independent, and activates a separately checked and audited replacement' do
    failed = fault
    saved = wait_repair('waiting_doctor')
    expect(saved.fetch('evidence').map { |entry| entry['run'] }).to include(failed)
    expect(saved.fetch('evidence').last.fetch('code')).to have_key('runtimeResult')
    expect(saved.fetch('spentMilliseconds')).to eq(0)
    expect(saved.fetch('waitingReason')).to include('No Doctor')
    expect(host.calls.none? { |call| call['node'] == 'DoctorDiagnosis' }).to be(true)
    expect(host.api('GET', "#{path}/runs/#{failed}").dig('codeExecution', 'completedViaFallback')).to be(true)
    expect(submit('fault', observed_input.merge(force_failure: true))).to eq(failed)
    expect(repair.fetch('evidence').length).to eq(saved.fetch('evidence').length)
    assign_doctor
    ready = wait_repair('awaiting_verification')
    expect(ready.fetch('id')).to eq(saved.fetch('id'))
    expect(ready.fetch('attempts').length).to eq(1)
    expect(ready.fetch('spentMilliseconds')).to be_between(1, 1_799_999)
    expect(ready.fetch('operations').last.dig('evidence', 'usage')).to include('inputTokens' => 59, 'outputTokens' => 31)
    candidate = host.api('GET', "#{path}/preparations/#{ready.fetch('candidate')}")
    expect(candidate.fetch('checks')).to all(include('matched' => true))
    expect(candidate.fetch('operations').map { |op| op['purpose'] }).to include('repair_candidate_generation', 'repair_candidate_review')
    trial = submit('authorized-repair-check')
    host.turn(trial)
    recovered = wait_repair('recovered')
    expect(recovered.fetch('verificationRun')).to eq(trial)
    expect(routine.dig('activeCode', 'reference')).to eq(candidate.dig('version', 'reference'))
    expect(recovered.fetch('recoveredReference')).not_to eq(@original.dig('version', 'reference'))
    expect(routine.fetch('repairCases').length).to eq(1)
  end

  it 'retains a crash reservation and unknown spend across restart, new runs and restoration of the exhausted old version' do
    fault
    diagnosis_rule({}, block: true, gate: 'held-diagnosis')
    assign_doctor
    Contract.wait { host.calls.find { |call| call['node'] == 'DoctorDiagnosis' } }
    before = repair
    expect(before.fetch('spentMilliseconds')).to eq(1_800_000)
    host.crash
    host.release('held-diagnosis')
    host.start
    interrupted = wait_repair('interrupted')
    expect(interrupted.fetch('usageUnknown')).to be(true)
    expect(interrupted.fetch('spentMilliseconds')).to eq(1_800_000)
    host.turn(submit('ordinary-after-crash'))
    host.stop
    saved_definition { |definition| definition['activeCode'] = @active }
    host.start
    fault('restored-version-fault')
    expect(repair.fetch('id')).to eq(before.fetch('id'))
    expect(repair.fetch('spentMilliseconds')).to eq(1_800_000)
    expect(routine.fetch('repairCases').length).to eq(1)
    expect(host.calls.count { |call| call['node'] == 'DoctorDiagnosis' }).to eq(1)
  end

  it 'requires a changed adaptive hypothesis after failed checks and preserves Stop and retained candidates without replay' do
    fault
    corrected = {language: 'python', profile: 'python-3.13.15-linux-arm64', source: candidate_source, interface: 'json-stdin-result-v1', dependencies: [], capabilities: []}
    preparation_rules('print("bad fixture result")', variants: [{contains: 'Use the actual validated aggregation result envelope', response: corrected, block: true, gate: 'held-second-generation'}])
    diagnosis_rule({}, variants: [{contains: '"lastFailure":{', response: {verdict: 'implementation_defect', diagnosis: 'The previous candidate printed a non-result.', recommendation: 'Retain the contract and parse the result envelope.', hypothesis: 'Use the actual validated aggregation result envelope instead of arbitrary output.'}}])
    assign_doctor
    Contract.wait(timeout: 100) { repair if repair.fetch('attempts').length >= 1 && host.calls.count { |call| call['node'] == 'DoctorDiagnosis' } >= 2 }
    Contract.wait(timeout: 100) { host.calls.find { |call| call['node'] == 'PreparationGeneration' && call['prompt'].include?('Use the actual validated aggregation') } }
    current = repair
    expect(current.fetch('attempts').length).to eq(2)
    expect(current.fetch('attempts').map { |attempt| attempt['hypothesis'] }.uniq.length).to eq(2)
    expect(current.fetch('id')).to eq(routine.fetch('repairCases').first.fetch('id'))
    candidate = current.fetch('candidate')
    host.api('POST', "#{path}/stop-preparation", operation: 'stop-repair-generation', revision: routine.fetch('revision'), preparation: candidate)
    host.release('held-second-generation')
    stopped = wait_repair('interrupted')
    expect(stopped.fetch('attempts').length).to eq(2)
    expect(stopped.fetch('spentMilliseconds')).to be_between(1, 1_799_999)
    expect(routine.dig('activeCode', 'status')).to eq('suspect')
    host.restart
    expect(repair.fetch('id')).to eq(stopped.fetch('id'))
    expect(repair.fetch('attempts').length).to eq(2)
  end

  it 'retains a structural diagnosis and stops unchanged-structure generation while safe ordinary work continues' do
    fault
    diagnosis_rule({verdict: 'structure_revision', diagnosis: 'The desired work needs a different workflow structure.', recommendation: 'Revise the structure before generating another implementation.', hypothesis: ''})
    assign_doctor
    saved = wait_repair('structure_revision')
    expect(saved.dig('diagnosis', 'diagnosis')).to include('different workflow structure')
    expect(saved.fetch('terminalReason')).to include('Revise the structure')
    count = host.calls.count { |call| call['node'] == 'PreparationGeneration' }
    host.turn(submit('ordinary-after-structural-handoff'))
    expect(host.calls.count { |call| call['node'] == 'PreparationGeneration' }).to eq(count)
    expect(repair.fetch('id')).to eq(saved.fetch('id'))
  end

  it 'shows the missing Doctor, retained diagnosis, attempts and terminal reason in the native routine view', :desktop do
    fault
    wait_repair('waiting_doctor')
    start_desktop
    desktop.click('Open RepairOwner')
    desktop.click('Work menu')
    desktop.click('Automation menu')
    desktop.click('Routines')
    desktop.click('Repairable sales')
    desktop.see('No Doctor is assigned')
    desktop.see('Attempts: 0')
    diagnosis_rule({verdict: 'structure_revision', diagnosis: 'The saved structure needs revision.', recommendation: 'Revise the workflow structure before repair.', hypothesis: ''})
    assign_doctor
    wait_repair('structure_revision')
    desktop.see('The saved structure needs revision.')
    desktop.see('Revise the workflow structure before repair.')
  end
end
