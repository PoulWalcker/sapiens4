require 'open3'
require 'sqlite3'
require_relative 'support/candidate_fixtures'
require_relative 'support/desktop'

RSpec.describe 'Explicit finite continuation of the same repair case' do
  include CandidateFixtures
  USAGE = {input_tokens: 17, output_tokens: 7}.freeze
  REFUSAL = 'unexpected status 503 Service Unavailable: controlled provider refusal'.freeze

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
    @owner = host.agent('GrantOwner')
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", operation: 'grant-routine', title: 'Finite sales repair', goal: CandidateFixtures::GOAL, criteria: CandidateFixtures::CRITERIA).fetch('routine')
    rules('Execution' => {response: decision('Outcome', reply: 'Observed5000.', outcome: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}), usage: USAGE},
      'AutomationAnalysis' => {response: decision('AutomationAnalyzed', decision: {status: 'Ready', automatablePart: 'Compute supplied sales.', parameters: [{name: 'csv', description: 'Sales rows'}]}), usage: USAGE})
    host.turn(submit('observe'))
    analysis = Contract.wait { routine['analysis'] if routine.dig('analysis', 'status') == 'Ready' }
    broken = candidate_source.sub('request = json.load(sys.stdin)', "request = json.load(sys.stdin)\nif request.get('force_failure'): raise ValueError('Business-only fault')")
    rules('PreparationFixtures' => {response: {fixtures: independent_fixtures, reason: 'Independent sales and rejection examples.'}, usage: USAGE},
      'PreparationGeneration' => {response: {language: 'python', profile: 'python-3.13.15-linux-arm64', source: broken, interface: 'json-stdin-result-v1', dependencies: [], capabilities: []}, usage: USAGE},
      'PreparationReview' => {response: {accepted: true, reason: 'Actual independent fixtures match.'}, usage: USAGE},
      'CodeAssessment' => {response: {verdict: 'accepted', reply: 'Original criteria are satisfied.', reason: 'Actual result agrees.', evidence: ['Actual output.'], acceptance: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}, activationAudit: {accepted: true, reason: 'Fixtures and this real occurrence support the exact version.'}}, usage: USAGE},
      'LocalFallback' => {response: {resolved: true, result: {status: 'ok', value: {total_minor: 5000}}, reason: 'Finish safely from retained input.'}, usage: USAGE})
    prepared = host.api('POST', "#{path}/prepare", operation: 'prepare-original', revision: routine.fetch('revision'), analysis: analysis.fetch('id')).fetch('preparation')
    Contract.wait(timeout: 90) { host.api('GET', "#{path}/preparations/#{prepared}")['status'] == 'awaiting_occurrence' }
    host.turn(submit('activate'))
    @fault = submit('fault', observed_input.merge(force_failure: true))
    host.turn(@fault)
    wait_case('waiting_doctor')
    rules('DoctorDiagnosis' => {terminal_failure: REFUSAL})
    assign_doctor
    wait_case('interrupted')
    @transport_case = repair
    expect(@transport_case.fetch('spentMilliseconds')).to be < 1_800_000
    expect(@transport_case.fetch('operations').last.fetch('attempts').length).to eq(3)
  end
  after { @desktop&.close; @host&.close }

  def path; "/api/agents/#{@owner}/routines/#{@routine}"; end
  def routine; host.state.fetch('personal_routines').find { |row| row['id'] == @routine }; end
  def repair; routine.fetch('repair'); end
  def rules(changes); host.configure(rules: host.provider.fetch('rules', {}).merge(changes)); end
  def assign_doctor(identity = @doctor)
    host.control(chief, 'corpora_update', revision: host.state.fetch('corpora').fetch('revision'), doctor_id: identity)
  end
  def submit(operation, input = observed_input)
    host.api('POST', "#{path}/run", operation: operation, revision: routine.fetch('revision'), inputs: JSON.generate(input)).fetch('run')
  end
  def wait_case(status)
    Contract.wait(timeout: 90, description: "repair #{status}") { row = repair; row if row['status'] == status && row.fetch('reservations').none? { |r| r['status'] == 'reserved' } }
  end
  def grant_body(operation = 'finite-grant', minutes = 1)
    {operation: operation, revision: routine.fetch('revision'), case: repair.fetch('id'), minutes: minutes, reason: 'Inspect the retained failure with one explicit finite continuation.'}
  end
  def grant(body, status: 200); host.api('POST', "#{path}/repair-grant", body, status: status); end
  def mutate_fixture
    host.stop
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      sequence, value = db.get_first_row('SELECT sequence,value FROM scheduled_routines WHERE json_extract(value,\'$.id\')=?', @routine)
      definition = JSON.parse(value)
      yield definition, db
      db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?', [JSON.generate(definition), sequence])
    end
    host.start
  end
  def exhaust_active_resource
    # A persisted public-file fixture avoids burning30minutes. Real fault,
    # provider attempts, pins, immutable source and completed receipts stay intact.
    mutate_fixture do |definition, _|
      saved = definition.fetch('repairCases').find { |row| row['id'] == definition['repairCase'] }
      saved['status'], saved['terminalReason'] = 'exhausted', 'Fixture: aggregate active repair allowance exhausted'
      saved['spentMilliseconds'] = saved.fetch('limitMilliseconds')
      saved.fetch('reservations').last['usedMilliseconds'] = saved.fetch('limitMilliseconds')
    end
    expect(repair.fetch('canGrant')).to be(true)
  end
  def diagnosis_rule(**extra)
    rules('DoctorDiagnosis' => {response: {verdict: 'request_access', diagnosis: 'The retained evidence was inspected under the explicit grant.', recommendation: 'Wait for independent confirmation before more repair.'}, usage: USAGE}.merge(extra))
  end
  def doctor_calls; host.calls.select { |row| row['node'] == 'DoctorDiagnosis' }; end

  it 'grants once atomically, starts one fresh diagnosis and retains original operations, spending and history across restart' do
    # Provider-attempt exhaustion with resource remaining is not active-budget exhaustion.
    grant(grant_body('transport-is-not-resource'), status: 409)
    expect(repair['grants'] || []).to be_empty
    exhaust_active_resource
    old = repair
    old_operations = old.fetch('operations')
    diagnosis_rule
    body = grant_body
    responses = 2.times.map { Thread.new { grant(body) } }.map(&:value)
    expect(responses[0]).to eq(responses[1])
    expect(grant(body)).to eq(responses[0])
    grant(body.merge(minutes: 2), status: 409)
    grant(body.merge(operation: 'stale-competing-grant'), status: 409)
    resumed = wait_case('waiting_access')
    expect(resumed.fetch('id')).to eq(old.fetch('id'))
    expect(resumed.fetch('rootReference')).to eq(old.fetch('rootReference'))
    expect(resumed.fetch('limitMilliseconds')).to eq(1_860_000)
    expect(resumed.fetch('baseLimitMilliseconds')).to eq(1_800_000)
    expect(resumed.fetch('spentMilliseconds')).to be_between(1_800_001, 1_859_999)
    expect(resumed.fetch('grants').length).to eq(1)
    expect(resumed.fetch('grants').first).to include('authority' => 'admin', 'additionalMilliseconds' => 60_000, 'previousSpentMilliseconds' => 1_800_000)
    expect(resumed.fetch('operations').take(old_operations.length)).to eq(old_operations)
    expect(resumed.fetch('operations').last.fetch('id')).not_to eq(old_operations.last.fetch('id'))
    expect(resumed.fetch('operations').last.fetch('attempts').length).to eq(1)
    expect(doctor_calls.length).to eq(4)
    expect(resumed.fetch('reservations').count { |row| row['status'] == 'reserved' }).to eq(0)
    host.restart
    expect(repair.fetch('grants')).to eq(resumed.fetch('grants'))
    expect(repair.fetch('spentMilliseconds')).to eq(resumed.fetch('spentMilliseconds'))
    expect(grant(body)).to eq(responses[0])
    grant(grant_body('new-id-is-not-a-refill'), status: 409)
    host.turn(submit('safe-business-is-not-a-grant'))
    expect(repair.fetch('limitMilliseconds')).to eq(resumed.fetch('limitMilliseconds'))
    expect(repair.fetch('spentMilliseconds')).to eq(resumed.fetch('spentMilliseconds'))
    expect(repair.fetch('grants').length).to eq(1)
  end

  it 'preserves uncertain and unknown fences, then stops a granted live diagnosis and refuses late publication or new admission after restart' do
    exhaust_active_resource
    # Explicit negative persistence fixtures are not application reconciliation.
    mutate_fixture do |definition, _|
      saved = definition.fetch('repairCases').last
      saved['status'] = 'interrupted'
      saved.fetch('reservations').last['status'] = 'uncertain'
    end
    grant(grant_body('uncertain-is-not-exhausted'), status: 409)
    mutate_fixture do |definition, db|
      saved = definition.fetch('repairCases').last
      saved['status'] = 'exhausted'
      saved.fetch('reservations').last['status'] = 'settled'
      source = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', @fault))
      source['routineEffectUnknown'] = {decisionId: 'original-unknown-action', reason: 'Independent confirmation is required.'}
      db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(source), @fault])
    end
    grant(grant_body('resource-is-not-effect-clearance'), status: 409)
    expect(repair['grants'] || []).to be_empty
    mutate_fixture do |_, db|
      source = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', @fault))
      source.delete('routineEffectUnknown')
      db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(source), @fault])
    end
    diagnosis_rule(block: true, gate: 'grant-diagnosis', child: true)
    grant(grant_body('stoppable-grant', 5))
    running = Contract.wait { row = repair; row if row['status'] == 'diagnosing' && doctor_calls.length == 4 && row.fetch('reservations').any? { |receipt| receipt['status'] == 'reserved' } }
    stopped = host.api('POST', "#{path}/stop-repair", operation: 'stop-granted-case', revision: routine.fetch('revision'), case: running.fetch('id'), reason: 'Revoke future work for this case.')
    expect(stopped.fetch('status')).to eq('stopped')
    host.release('grant-diagnosis')
    host.turn(running.fetch('diagnosisRun'), %w[interrupted cancelled failed])
    settled = wait_case('stopped')
    expect(settled.fetch('grants').length).to eq(1)
    expect(settled.fetch('limitMilliseconds')).to eq(2_100_000)
    expect(settled.fetch('spentMilliseconds')).to be >= 1_800_000
    expect(settled['diagnosis']).not_to include('diagnosis' => 'The retained evidence was inspected under the explicit grant.') if settled['diagnosis']
    count = doctor_calls.length
    host.restart
    expect(repair.fetch('status')).to eq('stopped')
    expect(repair.fetch('userStop')).to include('reason' => 'Revoke future work for this case.')
    expect(repair.fetch('grants')).to eq(settled.fetch('grants'))
    expect(repair.fetch('spentMilliseconds')).to eq(settled.fetch('spentMilliseconds'))
    grant(grant_body('stop-is-not-new-permission'), status: 409)
    expect(doctor_calls.length).to eq(count)
  end

  it 'shows editable5minute terms, reason, retained history and a case-wide Stop in the native UI', :desktop do
    exhaust_active_resource
    diagnosis_rule
    @desktop = Desktop.new(host.url)
    @desktop.click('Open GrantOwner')
    @desktop.click('Work menu')
    @desktop.click('Automation menu')
    @desktop.click('Routines')
    @desktop.click('Finite sales repair')
    @desktop.click('Continue repair…')
    @desktop.see('Add 5 minutes and continue')
    @desktop.fill('Additional active minutes', '2')
    @desktop.fill('Reason for continuing repair', 'Review the saved failure with two finite minutes.')
    @desktop.click('Add 2 minutes and continue')
    resumed = wait_case('waiting_access')
    expect(resumed.fetch('limitMilliseconds')).to eq(1_920_000)
    @desktop.click('Repair continuation grants')
    @desktop.see('Added 2.0 active minutes')
    @desktop.see('Review the saved failure with two finite minutes.')
    @desktop.click('Stop repair…')
    @desktop.click('Stop repair')
    wait_case('stopped')
    @desktop.see('Repair: stopped')
    expect(repair.fetch('grants').length).to eq(1)
  end
end
