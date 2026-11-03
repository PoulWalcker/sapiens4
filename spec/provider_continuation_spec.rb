require 'open3'
require 'sqlite3'
require 'digest'
require 'time'
require_relative 'support/candidate_fixtures'
require_relative 'support/desktop'

RSpec.describe 'Bounded maintenance provider continuation' do
  include CandidateFixtures
  TEMPORARY = 'unexpected status 503 Service Unavailable: controlled peer rejection, url: http://127.0.0.1/private'.freeze
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
    host.configure(version: 'codex-cli 0.162.1')
    host.start
    @doctor = host.state.fetch('corpora').fetch('doctor_id')
    assign_doctor(nil)
    @owner = host.agent('ContinuationOwner')
    create_routine('continuation')
    rules('Execution' => {response: decision('Outcome', reply: 'Observed total5000.', outcome: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}), usage: USAGE},
      'AutomationAnalysis' => {response: decision('AutomationAnalyzed', decision: {status: 'Ready', automatablePart: 'Compute supplied sales.', parameters: [{name: 'csv', description: 'Sales rows'}]}), usage: USAGE})
    host.turn(submit('observe'))
    @analysis = Contract.wait { routine['analysis'] if routine.dig('analysis', 'status') == 'Ready' }
    preparation_rules(candidate_source)
  end
  after { @desktop&.close; @host&.close }

  def path; "/api/agents/#{@owner}/routines/#{@routine}"; end
  def routine; host.state.fetch('personal_routines').find { |row| row['id'] == @routine }; end
  def repair; routine.fetch('repair'); end
  def create_routine(operation)
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", operation: operation, title: 'Durable preparation', goal: CandidateFixtures::GOAL, criteria: CandidateFixtures::CRITERIA).fetch('routine')
  end
  def submit(operation, input = observed_input)
    host.api('POST', "#{path}/run", operation: operation, revision: routine.fetch('revision'), inputs: JSON.generate(input)).fetch('run')
  end
  def rules(changes); host.configure(rules: host.provider.fetch('rules', {}).merge(changes)); end
  def assign_doctor(identity = @doctor)
    host.control(chief, 'corpora_update', revision: host.state.fetch('corpora').fetch('revision'), doctor_id: identity)
  end
  def preparation_rules(source)
    rules('PreparationFixtures' => {response: {fixtures: independent_fixtures, reason: 'Independent inputs and rejection checks.'}, usage: USAGE},
      'PreparationGeneration' => {response: {language: 'python', profile: 'python-3.13.15-linux-arm64', source: source, interface: 'json-stdin-result-v1', dependencies: [], capabilities: []}, usage: USAGE},
      'PreparationReview' => {response: {accepted: true, reason: 'Independent actual fixture results match.'}, usage: USAGE})
  end
  def prepare(operation)
    host.api('POST', "#{path}/prepare", operation: operation, revision: routine.fetch('revision'), analysis: @analysis.fetch('id')).fetch('preparation')
  end
  def preparation(identity); host.api('GET', "#{path}/preparations/#{identity}"); end
  def await_preparation(identity, status = 'awaiting_occurrence')
    Contract.wait(timeout: 90, description: "preparation #{status}") do
      saved = preparation(identity)
      raise JSON.pretty_generate(saved) if status == 'awaiting_occurrence' && %w[failed interrupted stopped].include?(saved['status'])
      if saved['status'] == status
        host.turn(identity, %w[failed interrupted cancelled]) if status == 'failed'
        saved
      end
    end
  end
  def await_wait(identity)
    Contract.wait(description: 'durable provider wait') do
      saved = preparation(identity)
      saved if saved.dig('providerWait', 'status') == 'retry_wait' && saved.dig('maintenanceBudget', 'reservations')&.none? { |row| row['status'] == 'reserved' } && host.state.fetch('turns').any? { |row| row['id'] == identity && row['status'] == 'queued' }
    end
  end
  def assessment
    {verdict: 'accepted', reply: 'Actual sales satisfy the original criteria.', reason: 'The actual result agrees.', evidence: ['Actual calculation.'], acceptance: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}, activationAudit: {accepted: true, reason: 'Independent fixtures and actual occurrence support the exact version.'}}
  end
  def wait_repair(status)
    Contract.wait(timeout: 90, description: "repair #{status}") { row = repair; row if row['status'] == status }
  end
  def calls(node); host.calls.select { |call| call['node'] == node }; end
  def mutate_work(identity)
    host.stop
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      value = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', identity))
      yield value
      db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(value), identity])
    end
    host.start
  end
  def canonical_hash(value)
    ordered = lambda { |item| item.is_a?(Hash) ? item.sort.to_h.transform_values { |entry| ordered.call(entry) } : item.is_a?(Array) ? item.map { |entry| ordered.call(entry) } : item }
    Digest::SHA256.hexdigest(JSON.generate(ordered.call(value)))
  end

  it 'resumes only the retained review after restart, keeps checks and allowance, and receipts one shape correction' do
    rules('PreparationReview' => {attempts: [{terminal_failure: TEMPORARY}, {response: {accepted: true, reason: ''}, usage: USAGE}]},
      'PreparationReviewCorrection' => {response: {accepted: true, reason: 'The retained actual checks match all independent fixtures.'}, usage: USAGE})
    identity = prepare('retained-review')
    waiting = await_wait(identity)
    operation = waiting.fetch('operations').find { |row| row['node'] == 'PreparationReview' }
    checks = waiting.fetch('checks')
    spent = waiting.dig('maintenanceBudget', 'spentMilliseconds')
    expect(operation.fetch('attempts').length).to eq(1)
    expect(waiting.dig('providerWait', 'hostAttemptLimit')).to eq(3)
    expect(operation.fetch('reason')).not_to include('private')
    host.crash
    rules('PreparationReview' => {response: {accepted: true, reason: ''}, usage: USAGE})
    host.start
    # The exact due/identity survive; the second process may start only after due.
    completed = await_preparation(identity)
    retained = completed.fetch('operations').find { |row| row['id'] == operation['id'] }
    expect(retained.fetch('attempts').length).to eq(2)
    expect(retained.fetch('deadline')).to eq(operation.fetch('deadline'))
    expect(retained.dig('correction', 'deadline')).to eq(operation.fetch('deadline'))
    expect(retained.fetch('status')).to eq('completed')
    expect(retained.fetch('rawResponse')).to include('"accepted":true')
    expect(completed.fetch('checks')).to eq(checks)
    expect(completed.dig('maintenanceBudget', 'spentMilliseconds')).to be >= spent
    expect(completed.dig('maintenanceBudget', 'limitMilliseconds')).to eq(1_800_000)
    expect(calls('PreparationFixtures').length).to eq(1)
    expect(calls('PreparationGeneration').length).to eq(1)
    expect(calls('PreparationReview').map { |row| row['prompt'] }.uniq.length).to eq(1)
    expect(calls('PreparationReview')[1].fetch('started')).to be >= Time.parse(operation.fetch('retryAt')).to_f
    expect(calls('PreparationReviewCorrection').length).to eq(1)
    expect(completed.fetch('operations').count { |row| row['purpose'] == 'fixture_check' }).to eq(independent_fixtures.length)
    create_routine('safe-ordinary-consumer')
    host.turn(submit('safe-ordinary-after-wait'))
    expect((routine['activeCode'] || {})['status']).not_to eq('active')
  end

  it 'bounds refusals, preserves unknown spend and cancellation, and rejects semantic promotion without new resources' do
    rules('PreparationFixtures' => {terminal_failure: TEMPORARY})
    exhausted = prepare('finite-refusals')
    saved = await_preparation(exhausted, 'failed')
    expect(saved.dig('providerWait', 'status')).to eq('exhausted')
    expect(saved.fetch('operations').first.fetch('attempts').length).to eq(3)
    expect(saved.fetch('operations').first.fetch('attempts')).to all(include('usageUnknown' => true))
    expect(calls('PreparationGeneration')).to be_empty
    %w[401 429 lost].each do |kind|
      rule = case kind
        when '401' then {terminal_failure: 'unexpected status 401 Unauthorized: controlled refusal'}
        when '429' then {terminal_failure: 'exceeded retry limit, last status: 429 Too Many Requests'}
        else {fail: true}
      end
      rules('PreparationFixtures' => rule)
      identity = prepare("terminal-#{kind}")
      terminal = await_preparation(identity, 'failed')
      expect(terminal.fetch('operations').first.fetch('attempts').length).to eq(1)
      expect(terminal.dig('providerWait', 'status')).to eq(kind == '401' ? 'permanent' : 'unknown')
    end
    rules('PreparationFixtures' => {terminal_failure: TEMPORARY})
    stopped = prepare('stop-wait')
    await_wait(stopped)
    host.api('POST', "/api/agents/#{@owner}/turns/#{stopped}/cancel")
    count = calls('PreparationFixtures').length
    host.restart
    host.turn(stopped, 'cancelled')
    expect(preparation(stopped).dig('providerWait', 'status')).to eq('cancelled')
    expect(calls('PreparationFixtures').length).to eq(count)
    # No malformed unknown verdict is sent to a correction provider for approval.
    preparation_rules(candidate_source)
    rules('PreparationReview' => {response: {}, usage: USAGE}, 'PreparationReviewCorrection' => {response: {accepted: true, reason: 'Invented approval.'}, usage: USAGE})
    ambiguous = prepare('ambiguous-review')
    await_preparation(ambiguous, 'failed')
    expect(calls('PreparationReviewCorrection')).to be_empty
    expect((routine['repair'] || {})['attempts'] || []).to be_empty
  end

  it 'continues the Doctor cursor after a confirmed file write and refuses a stale saved-audit source before its retry' do
    broken = candidate_source.sub('request = json.load(sys.stdin)', "request = json.load(sys.stdin)\nif request.get('force_failure'): raise ValueError('Business-only fault')")
    preparation_rules(broken)
    prepared = prepare('doctor-original')
    await_preparation(prepared)
    rules('CodeAssessment' => {response: assessment, usage: USAGE})
    host.turn(submit('activate'))
    rules('LocalFallback' => {response: {resolved: true, result: {status: 'ok', value: {total_minor: 5000}}, reason: 'Finish safely from retained input.'}, usage: USAGE})
    host.turn(submit('fault', observed_input.merge(force_failure: true)))
    wait_repair('waiting_doctor')
    selected = File.join(File.dirname(host.root), 'selected.json')
    File.write(selected, '{"before":true}')
    granted = host.api('POST', "#{path}/doctor-access", operation: 'grant-selected', revision: routine.fetch('revision'), case: repair.fetch('id'), resources: [{path: selected, access: 'write'}])
    resource = granted.dig('access', 'resources').first.fetch('id')
    content = '{"after":true}'
    stage = canonical_hash(name: 'repair.json', content: content)
    final = {verdict: 'request_access', diagnosis: 'The selected repair was confirmed.', recommendation: 'Wait for independent review.'}
    rules('DoctorDiagnosis' => {responses: [{action: {tool: 'stage_file', name: 'repair.json', content: content}}, {action: {tool: 'apply_file', resource: resource, stage: stage, expectedHash: Digest::SHA256.hexdigest(File.read(selected))}}, final], usage: USAGE,
      variants: [{contains: '"confirmed":true', terminal_failure: TEMPORARY}]})
    assign_doctor
    waiting = Contract.wait { row = repair; row if row.dig('providerWait', 'status') == 'retry_wait' && row.fetch('reservations').none? { |r| r['status'] == 'reserved' } }
    host.turn(waiting.fetch('diagnosisRun'), 'queued')
    operation = waiting.fetch('operations').find { |row| row['status'] == 'retry_wait' }
    expect(File.read(selected)).to eq(content)
    host.crash
    rules('DoctorDiagnosis' => {response: final, usage: USAGE})
    host.start
    resumed = wait_repair('waiting_access')
    expect(resumed.fetch('id')).to eq(waiting.fetch('id'))
    expect(resumed.fetch('operations').count { |row| row['tool'] == 'apply_file' }).to eq(1)
    continued = resumed.fetch('operations').find { |row| row['id'] == operation['id'] }
    expect(continued.fetch('attempts').length).to eq(2)
    expect(calls('DoctorDiagnosis').last(2).map { |row| row['prompt'] }.uniq.length).to eq(1)
    # A separate observed healthy consumer establishes the adjacent audit-source guard.
    assign_doctor(nil)
    create_routine('audit-consumer')
    host.turn(submit('audit-observe'))
    @analysis = Contract.wait { routine['analysis'] if routine.dig('analysis', 'status') == 'Ready' }
    preparation_rules(candidate_source)
    await_preparation(prepare('audit-version'))
    rules('CodeAssessment' => {response: assessment, usage: USAGE})
    host.turn(submit('audit-activate'))
    rules('CodeAssessment' => {response: {verdict: 'accepted'}, usage: USAGE})
    source = submit('retained-audit-source')
    host.turn(source, 'failed')
    wait_repair('waiting_doctor')
    rules('CodeAssessment' => {terminal_failure: TEMPORARY}, 'DoctorDiagnosis' => {response: {verdict: 'implementation_defect', diagnosis: 'Only independent assessment is missing.', recommendation: 'Audit the retained exact result.', hypothesis: 'The existing code may already be correct; reassess its retained result.'}, usage: USAGE})
    assign_doctor
    audit_wait = Contract.wait do
      row = repair
      raise JSON.pretty_generate(row) if %w[interrupted exhausted stale].include?(row['status'])
      row if row.dig('providerWait', 'status') == 'retry_wait' && row.fetch('reservations').none? { |r| r['status'] == 'reserved' }
    end
    host.turn(audit_wait.fetch('auditRun'), 'queued')
    count = calls('CodeAssessment').length
    mutate_work(source) { |work| work['executionGate'] = 'stopped' }
    wait_repair('interrupted')
    expect(calls('CodeAssessment').length).to eq(count)
    expect(audit_wait.fetch('kind')).to eq('saved_result_audit')
    source_record = host.api('GET', "#{path}/runs/#{source}")
    expect(source_record.dig('codeExecution', 'operations').count { |row| row['purpose'] == 'code_execution' }).to eq(1)
    expect(calls('PreparationGeneration').length).to eq(2)
  end

  it 'shows the retained refusal and finite host-attempt exhaustion in the native UI', :desktop do
    rules('PreparationFixtures' => {terminal_failure: TEMPORARY})
    @desktop = Desktop.new(host.url)
    @desktop.click('Open ContinuationOwner')
    @desktop.click('Work menu')
    @desktop.click('Automation menu')
    @desktop.click('Routines')
    @desktop.click('Durable preparation')
    identity = prepare('visible-provider-wait')
    @desktop.see('The provider rejected this request with 503')
    @desktop.see('Resume after ')
    await_preparation(identity, 'failed')
    @desktop.see('Provider: exhausted · host attempts 3/3')
    @desktop.see('The original maintenance allowance is retained.')
    expect(calls('PreparationFixtures').length).to eq(3)
  end
end
