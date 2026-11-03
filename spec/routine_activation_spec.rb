require 'digest'
require 'rbconfig'
require 'open3'
require 'timeout'
require 'sqlite3'
require_relative 'support/candidate_fixtures'
require_relative 'support/candidate_docker'

RSpec.describe 'Authorized routine candidate activation with real execution' do
  include CandidateFixtures
  include CandidateDocker

  before(:all) do
    unless RbConfig::CONFIG.fetch('host_os').include?('darwin') && RbConfig::CONFIG.fetch('host_cpu').match?(/\A(?:arm64|aarch64)\z/)
      skip 'Selected generated-code runtime currently supports macOS ARM64 only'
    end
    @docker = File.expand_path('~/.orbstack/bin/docker')
    @socket = 'unix://' + File.expand_path('~/.orbstack/run/docker.sock')
    output, error, status = Open3.capture3(@docker, '--host', @socket, 'ps', '--all', '--quiet', '--no-trunc')
    raise "Selected runtime unavailable: #{error}" unless status.success?
    @existing_containers = output.lines.map(&:strip).sort
  end

  after(:all) do
    next unless @existing_containers
    output, error, status = Open3.capture3(@docker, '--host', @socket, 'ps', '--all', '--quiet', '--no-trunc')
    raise "Cannot verify runtime noninterference: #{error}" unless status.success?
    expect(output.lines.map(&:strip).sort).to eq(@existing_containers)
  end

  before do
    @host = Host.new(start: false, environment: {'SAPIENS_GENERATED_DOCKER' => File.expand_path('~/.orbstack/bin/docker'), 'SAPIENS_GENERATED_SOCKET' => 'unix://' + File.expand_path('~/.orbstack/run/docker.sock')})
    host.configure(version: 'codex-cli 0.162.1')
    host.start
    @owner = host.agent('CandidateOwner')
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", operation: 'create', title: 'Sales report', goal: CandidateFixtures::GOAL, criteria: CandidateFixtures::CRITERIA).fetch('routine')
    host.configure(rules: {
      'Execution' => {response: decision('Outcome', reply: 'Observed sales total: 5000 minor units.', outcome: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []})},
      'AutomationAnalysis' => {response: decision('AutomationAnalyzed', decision: {status: 'Ready', automatablePart: 'Validate and aggregate supplied CSV sales.', parameters: [{name: 'csv', description: 'Sales records'}, {name: 'start', description: 'Inclusive date'}, {name: 'end', description: 'Exclusive date'}]})}
    })
    @business = host.api('POST', "#{routine_path}/run", operation: 'ordinary', revision: 1, inputs: JSON.generate(observed_input)).fetch('run')
    host.turn(@business)
    @analysis = Contract.wait(description: 'Ready analysis for pure preparation') do
      row = routine['analysis']
      row if row && row['status'] == 'Ready'
    end
    configure_preparation
    configure_assessment
  end

  after do
    File.write(@docker_gate, '') if @docker_gate && File.directory?(File.dirname(@docker_gate))
    close_error = nil
    begin
      @host&.close
    rescue => error
      close_error = error
    end
    if @owned_container
      output, error, status = docker('inspect', @owned_container.fetch('id'))
      unless !status.success? && error.match?(/No such (object|container)/i)
        object = JSON.parse(output).fetch(0)
        labels = object.fetch('Config').fetch('Labels')
        if labels['sapiens.owner'] == @owner && (labels['sapiens.preparation'] == @owned_container['preparation'] && labels['sapiens.kind'] != 'occurrence' || labels['sapiens.kind'] == 'occurrence' && labels['sapiens.workload'] == @owned_container['workload']) && labels['sapiens.nonce'] == @owned_container['nonce']
          docker('rm', '--force', @owned_container.fetch('id'))
        end
        raise 'Product did not remove its owned fixture container; scoped emergency cleanup attempted'
      end
    end
    raise close_error if close_error
  end

  def docker(*arguments)
    Timeout.timeout(15) { Open3.capture3(@docker, '--host', @socket, *arguments) }
  end

  def routine_path
    "/api/agents/#{@owner}/routines/#{@routine}"
  end

  def routine
    host.state.fetch('personal_routines').find { |row| row['id'] == @routine }
  end

  def configure_preparation(source: candidate_source, fixtures: independent_fixtures, generation: {}, review: {}, fixture_rule: {}, generation_rule: {}, review_rule: {})
    @candidate_source = source
    generated = {language: 'python', profile: 'python-3.13.15-linux-arm64', source: source, interface: 'json-stdin-result-v1', dependencies: [], capabilities: []}.merge(generation)
    host.configure(rules: host.provider.fetch('rules').merge(
      'PreparationFixtures' => {response: {fixtures: fixtures, reason: 'Independent examples exercise new periods, held-out categories and rejected inputs.'}}.merge(fixture_rule),
      'PreparationGeneration' => {response: generated}.merge(generation_rule),
      'PreparationReview' => {response: {accepted: true, reason: 'Actual held-out results satisfy the stated criteria.'}.merge(review)}.merge(review_rule)
    ))
  end

  def prepare(operation = 'prepare')
    host.api('POST', "#{routine_path}/prepare", operation: operation, revision: 1, analysis: @analysis.fetch('id')).fetch('preparation')
  end

  def preparation(id)
    host.api('GET', "#{routine_path}/preparations/#{id}")
  end

  def completed(id, status = 'awaiting_occurrence')
    Contract.wait(timeout: 90, description: "preparation #{status}") do
      row = preparation(id)
      if %w[failed interrupted stopped stale cleanup_pending awaiting_occurrence].include?(row['status']) && row['status'] != status
        raise "Expected #{status}, received #{row['status']}: #{row['error']}"
      end
      row if row['status'] == status
    end
  end

  def stage_calls(node)
    host.calls.select { |call| call['node'] == node }
  end

  def configure_assessment(audit: true, **rule)
    response = {verdict: 'accepted', reply: 'Computed sales satisfy the accepted criteria.', reason: 'The retained code result matches the requested interval and sums.',
      evidence: ['Observed actual code output and accepted task criteria.'],
      acceptance: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}}
    response[:activationAudit] = {accepted: true, reason: 'The actual result and independent preparation checks support this exact version.'} if audit
    host.configure(rules: host.provider.fetch('rules').merge('CodeAssessment' => {response: response, usage: {input_tokens: 73, cached_input_tokens: 5, output_tokens: 29}}.merge(rule)))
  end

  def run_detail(id)
    host.api('GET', "#{routine_path}/runs/#{id}")
  end

  def run_code(operation, input)
    id = host.api('POST', "#{routine_path}/run", operation: operation, revision: 1, inputs: JSON.generate(input)).fetch('run')
    host.turn(id)
    run_detail(id)
  end

  it 'activates only after an authorized actual result and uses that exact version on a later occurrence' do
    prepared = completed(prepare)
    reference = prepared.fetch('version').fetch('reference')
    expect(routine.fetch('runs').map { |run| run['id'] }).to eq([@business])
    initial_dispatches = stage_calls('Execution').length
    input = {csv: "date,category,amount_minor\n2027-01-04,music,700\n2027-01-04,music,-50\n2027-01-05,food,900\n", start: '2027-01-04', end: '2027-01-05'}
    expected = {'status' => 'ok', 'value' => {'start' => '2027-01-04', 'end' => '2027-01-05', 'row_count' => 2, 'by_category_minor' => {'music' => 650}, 'total_minor' => 650}}
    trial = run_code('trial', input)
    code = trial.fetch('codeExecution')
    expect(code).to include('reference' => reference, 'role' => 'trial', 'input' => JSON.parse(JSON.generate(input)))
    expect(code.fetch('runtimeResult').fetch('actual')).to eq(expected)
    expect(trial.dig('executionBinding', 'binding')).to include('mode' => 'script', 'role' => 'trial', 'reference' => reference)
    expect(code.fetch('acceptance')).to include('verdict' => 'accepted', 'outcome' => include('status' => 'Completed'))
    verification = code.fetch('verification')
    expect(verification.fetch('purpose')).to eq('code_acceptance_and_activation_audit')
    expect(verification.fetch('evidence')).to include('policy' => 'response_only', 'usage' => include('inputTokens' => 73, 'cachedInputTokens' => 5, 'outputTokens' => 29))
    expect(code.dig('acceptance', 'invocation')).to eq(verification.fetch('id'))
    expect(code.dig('activationAudit', 'invocation')).to eq(verification.fetch('id'))
    expect(code.fetch('operations').count { |operation| operation['purpose'] == 'code_acceptance_and_activation_audit' }).to eq(1)
    expect(code.fetch('activationAudit')).to include('accepted' => true)
    expect(routine.fetch('activeCode')).to include('reference' => reference, 'status' => 'active')
    expect(stage_calls('CodeAssessment').length).to eq(1)
    expect(stage_calls('Execution').length).to eq(initial_dispatches)
    expect(routine.fetch('runs').length).to eq(2)
    configure_assessment(audit: false)
    next_input = input.merge(end: '2027-01-06')
    active = run_code('active', next_input).fetch('codeExecution')
    expect(active).to include('reference' => reference, 'role' => 'active', 'input' => JSON.parse(JSON.generate(next_input)))
    expect(active.fetch('runtimeResult').fetch('actual')).to eq({'status' => 'ok', 'value' => {'start' => '2027-01-04', 'end' => '2027-01-06', 'row_count' => 3, 'by_category_minor' => {'music' => 650, 'food' => 900}, 'total_minor' => 1550}})
    expect(active.fetch('verification').fetch('purpose')).to eq('code_acceptance')
    expect(active['activationAudit']).to be_nil
    expect(stage_calls('CodeAssessment').length).to eq(2)
    expect(stage_calls('Execution').length).to eq(initial_dispatches)
    expect(routine.fetch('runs').length).to eq(3)
    expect(stage_calls('PreparationGeneration').length).to eq(1)
  end

  def submit_code(operation, input = observed_input)
    host.api('POST', "#{routine_path}/run", operation: operation, revision: 1, inputs: JSON.generate(input)).fetch('run')
  end

  def settle_code(id)
    host.turn(id, %w[done failed interrupted cancelled])
    run_detail(id)
  end

  def assessment_response(**fields)
    base = host.provider.fetch('rules').fetch('CodeAssessment').fetch(:response)
    configure_assessment(response: base.merge(fields))
  end

  it 'delivers accepted output without activation when the separate audit rejects it' do
    completed(prepare)
    assessment_response(activationAudit: {accepted: false, reason: 'This output is correct but generalization is not established.'})
    id = submit_code('audit-rejected')
    code = settle_code(id).fetch('codeExecution')
    expect(code.dig('acceptance', 'outcome', 'status')).to eq('Completed')
    expect(code.dig('activationAudit', 'accepted')).to be(false)
    expect(code.dig('activation', 'status')).to eq('not_activated')
    expect(routine['activeCode']).to be_nil
    expect(host.state.fetch('turns').find { |turn| turn['id'] == id }.fetch('output')).to include('Computed sales')
    expect(stage_calls('CodeAssessment').length).to eq(1)
  end

  {'missing criterion' => {verdict: 'accepted', acceptance: {status: 'Completed', satisfiedCriteria: [CandidateFixtures::CRITERIA.first], artifacts: []}},
   'unknown assessment' => {verdict: 'unknown', acceptance: {status: 'Unresolved', satisfiedCriteria: [], artifacts: []}},
   'malformed assessment' => {verdict: 'invented'}}.each do |label, response|
    it "retains real output without activating after #{label}" do
      completed(prepare)
      assessment_response(**response)
      code = settle_code(submit_code('uncertain')).fetch('codeExecution')
      expect(code.dig('runtimeResult', 'actual', 'value', 'total_minor')).to eq(5000)
      expect(routine['activeCode']).to be_nil
      expect(stage_calls('CodeAssessment').length).to eq(1)
      expect(routine.fetch('runs').length).to eq(2)
    end
  end

  it 'keeps a newer preparation queued during a pending trial and trials it only on a later occurrence' do
    first = completed(prepare('first'))
    configure_assessment(block: true, gate: 'held-trial')
    trial = submit_code('first-trial')
    Contract.wait { stage_calls('CodeAssessment').length == 1 }
    configure_preparation(source: candidate_source + "\n# New checked candidate.\n")
    pending = prepare('newer')
    expect(preparation(pending).fetch('status')).to eq('queued')
    second = submit_code('while-trial-pending')
    expect(run_detail(second).dig('executionBinding', 'binding', 'mode')).to eq('ordinary')
    host.release('held-trial')
    host.turn(trial)
    host.turn(second)
    newer = completed(pending)
    expect(newer.dig('version', 'reference')).not_to eq(first.dig('version', 'reference'))
    configure_assessment
    later = settle_code(submit_code('newer-trial')).fetch('codeExecution')
    expect(later).to include('role' => 'trial', 'reference' => newer.dig('version', 'reference'))
    expect(preparation(first.fetch('id')).dig('trial', 'run')).to eq(trial)
    expect(stage_calls('CodeAssessment').length).to eq(2)
  end

  it 'retains one claim across duplicate response replay and restart without replaying code or assessment' do
    prepared = completed(prepare)
    configure_assessment(block: true, gate: 'deduplicated-trial')
    requests = 2.times.map { Thread.new { submit_code('same-operation') } }
    ids = requests.map(&:value)
    expect(ids.uniq.length).to eq(1)
    Contract.wait { stage_calls('CodeAssessment').length == 1 }
    host.release('deduplicated-trial')
    host.turn(ids.first)
    saved = run_detail(ids.first).fetch('codeExecution')
    host.restart
    expect(submit_code('same-operation')).to eq(ids.first)
    expect(run_detail(ids.first).fetch('codeExecution')).to eq(saved)
    expect(preparation(prepared.fetch('id')).dig('trial', 'run')).to eq(ids.first)
    expect(stage_calls('CodeAssessment').length).to eq(1)
    expect(routine.fetch('runs').length).to eq(2)
    expect(host.request('POST', "/api/agents/#{@owner}/turns/#{ids.first}/retry", {}).first).to eq(409)
    file = File.join(host.root, 'agentpy/agents', @owner, 'state.json')
    state = JSON.parse(File.read(file))
    turn = state.fetch('turns').find { |row| row['id'] == ids.first }
    turn['managed_command'] = {'id' => 'file-retry-code', 'action' => 'retry', 'revision' => turn.fetch('managed').fetch('revision')}
    Contract.atomic_json(file, state)
    expect(host.request('GET', '/api/state').first).to eq(409)
    turn.delete('managed_command')
    Contract.atomic_json(file, state)
    expect(stage_calls('CodeAssessment').length).to eq(1)

  end

  it 'never rolls back to an older untried candidate after the newest produces a wrong actual answer' do
    older = completed(prepare('older'))
    poisoned = candidate_source.sub('request = json.load(sys.stdin)', "request = json.load(sys.stdin)\nif request.get('start') == '2027-02-01':\n    print(json.dumps({'status':'ok','value':{'total_minor':0}}))\n    sys.exit(0)")
    configure_preparation(source: poisoned)
    newest = completed(prepare('newest'))
    assessment_response(verdict: 'rejected', acceptance: {status: 'Unresolved', satisfiedCriteria: [], artifacts: []}, activationAudit: {accepted: false, reason: 'The observed answer is wrong.'})
    input = {csv: "date,category,amount_minor\n2027-02-01,books,123\n", start: '2027-02-01', end: '2027-02-02'}
    code = settle_code(submit_code('wrong-new-input', input)).fetch('codeExecution')
    expect(code.fetch('reference')).to eq(newest.dig('version', 'reference'))
    expect(code.dig('runtimeResult', 'actual')).to eq({'status' => 'ok', 'value' => {'total_minor' => 0}})
    expect(code.dig('acceptance', 'verdict')).to eq('rejected')
    expect(routine['activeCode']).to be_nil
    next_id = submit_code('after-rejection')
    host.turn(next_id)
    expect(run_detail(next_id).dig('executionBinding', 'binding', 'mode')).to eq('ordinary')
    expect(preparation(older.fetch('id'))['trial']).to be_nil
    expect(stage_calls('CodeAssessment').length).to eq(1)
  end

  %w[Stop Delete goal_roundtrip source_feedback].each do |event|
    it "preserves the actual result but refuses late activation after #{event}" do
      completed(prepare)
      configure_assessment(block: true, gate: 'late-assessment')
      id = submit_code('late-trial')
      Contract.wait { stage_calls('CodeAssessment').length == 1 }
      actual = run_detail(id).dig('codeExecution', 'runtimeResult', 'actual')
      expect(actual.dig('value', 'total_minor')).to eq(5000)
      case event
      when 'Stop'
        host.api('POST', "#{routine_path}/stop", operation: 'stop-trial', revision: 1, run: id)
      when 'Delete'
        host.api('POST', "#{routine_path}/delete", operation: 'delete-trial', revision: 1)
      when 'goal_roundtrip'
        fields = {title: 'Sales report', criteria: CandidateFixtures::CRITERIA}
        host.api('POST', "#{routine_path}/edit", fields.merge(operation: 'change-goal', revision: 1, goal: 'Changed goal'))
        host.api('POST', "#{routine_path}/edit", fields.merge(operation: 'restore-goal', revision: 2, goal: CandidateFixtures::GOAL))
      when 'source_feedback'
        host.api('POST', "#{routine_path}/reject", operation: 'reject-source', revision: 1, run: @business, reason: 'The ordinary source was wrong.')
      end
      host.release('late-assessment')
      settle_code(id)
      expect(routine.dig('activeCode', 'status')).not_to eq('active')
      host.restart
      expect(run_detail(id).dig('codeExecution', 'runtimeResult', 'actual')).to eq(actual)
      expect(stage_calls('CodeAssessment').length).to eq(1)
      expect(routine.fetch('runs').length).to eq(2)
    end
  end

  it 'preserves an active version after an unknown assessment and unavailable runtime' do
    completed(prepare)
    settle_code(submit_code('activate'))
    reference = routine.fetch('activeCode').fetch('reference')
    assessment_response(verdict: 'unknown', acceptance: {status: 'Unresolved', satisfiedCriteria: [], artifacts: []})
    settle_code(submit_code('unknown-active'))
    expect(routine.fetch('activeCode')).to include('status' => 'active', 'reference' => reference)
    host.environment['SAPIENS_GENERATED_SOCKET'] = 'unix://' + File.join(host.root, 'missing-docker.sock')
    host.restart
    settle_code(submit_code('unavailable-active'))
    expect(routine.fetch('activeCode')).to include('status' => 'active', 'reference' => reference)
    expect(stage_calls('CodeAssessment').length).to eq(2)
    expect(routine.fetch('runs').length).to eq(4)
  end

  it 'cleans an occurrence-owned real container after host SIGKILL without replaying business code' do
    looping = candidate_source.sub('request = json.load(sys.stdin)', "request = json.load(sys.stdin)\nif request.get('start') == '2028-01-01':\n    while True: pass")
    configure_preparation(source: looping)
    prepared = completed(prepare)
    id = submit_code('crash-trial', observed_input.merge(start: '2028-01-01', end: '2028-01-02'))
    container = Contract.wait(timeout: 20, description: 'real business container running') do
      value = run_detail(id).dig('codeExecution', 'container')
      next unless value && value['id']
      output, _, status = docker('inspect', value['id'])
      object = JSON.parse(output).first if status.success?
      value if object && object.dig('State', 'Running')
    end
    @owned_container = container
    output, _, status = docker('inspect', container.fetch('id'))
    expect(status.success?).to be(true)
    expect(JSON.parse(output).first.dig('Config', 'Labels')).to include('sapiens.kind' => 'occurrence', 'sapiens.workload' => id, 'sapiens.owner' => @owner)
    host.crash
    host.start
    Contract.wait(description: 'business container removed by startup recovery') do
      _, error, status = docker('inspect', container.fetch('id'))
      !status.success? && error.match?(/No such (object|container)/i)
    end
    expect(run_detail(id).dig('codeExecution', 'container', 'status')).to eq('removed')
    expect(stage_calls('CodeAssessment')).to be_empty
    expect(routine.fetch('runs').length).to eq(2)
    expect(preparation(prepared.fetch('id')).dig('container', 'status')).to eq('removed')
    expect(host.request('POST', "/api/agents/#{@owner}/turns/#{id}/retry", {}).first).to eq(409)
    host.restart
    expect(stage_calls('CodeAssessment')).to be_empty
  end

  it 'does not disable a newer active version when an older pinned run reports a proven late fault' do
    old_source = candidate_source.sub('request = json.load(sys.stdin)', "request = json.load(sys.stdin)\nif request.get('start') == '2027-02-01':\n    print(json.dumps({'status':'ok','value':{'total_minor':0}}))\n    sys.exit(0)")
    configure_preparation(source: old_source)
    old = completed(prepare('old'))
    settle_code(submit_code('activate-old'))
    configure_preparation
    newer = completed(prepare('replacement'))
    configure_assessment(block: true, gate: 'new-version-audit')
    trial = submit_code('activate-replacement')
    Contract.wait { stage_calls('CodeAssessment').length == 2 }
    input = {csv: "date,category,amount_minor\n2027-02-01,books,123\n", start: '2027-02-01', end: '2027-02-02'}
    stale = submit_code('old-active-fault', input)
    expect(run_detail(stale).dig('executionBinding', 'binding')).to include('role' => 'active', 'reference' => old.dig('version', 'reference'))
    assessment_response(verdict: 'rejected', acceptance: {status: 'Unresolved', satisfiedCriteria: [], artifacts: []}, activationAudit: nil)
    host.release('new-version-audit')
    host.turn(trial)
    late = settle_code(stale).fetch('codeExecution')
    expect(late).to include('reference' => old.dig('version', 'reference'))
    expect(late.dig('runtimeResult', 'actual')).to eq('status' => 'ok', 'value' => {'total_minor' => 0})
    expect(late.dig('acceptance', 'verdict')).to eq('rejected')
    expect(routine.fetch('activeCode')).to include('status' => 'active', 'reference' => newer.dig('version', 'reference'))
    configure_assessment(audit: false)
    next_run = settle_code(submit_code('after-old-fault', input)).fetch('codeExecution')
    expect(next_run).to include('role' => 'active', 'reference' => newer.dig('version', 'reference'))
  end

  it 'preserves scheduled null inputs instead of substituting mutable defaults' do
    goal = 'Return the product of the embedded integers six and seven without external input.'
    criteria = ['Return the exact integer 42.']
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", operation: 'null-create', title: 'Embedded arithmetic', goal: goal, criteria: criteria, inputs: 'Do not pass this mutable default').fetch('routine')
    host.configure(rules: {'Execution' => {response: decision('Outcome', reply: '42', outcome: {status: 'Completed', satisfiedCriteria: criteria, artifacts: []})},
      'AutomationAnalysis' => {response: decision('AutomationAnalyzed', decision: {status: 'Ready', automatablePart: 'Compute embedded arithmetic.', parameters: []})}})
    @business = host.api('POST', "#{routine_path}/run", operation: 'null-observed', revision: 1).fetch('run')
    host.turn(@business)
    @analysis = Contract.wait { row = routine['analysis']; row if row && row['status'] == 'Ready' }
    source = "import json,sys\nvalue=json.load(sys.stdin)\nprint(json.dumps({'status':'ok','value':42} if value is None else {'status':'rejected','error':'unexpected_input'}))\n"
    configure_preparation(source: source, fixtures: [{input: nil, expected: {status: 'ok', value: 42}}, {input: 'unexpected', expected: {status: 'rejected', error: 'unexpected_input'}}])
    configure_assessment(response: {verdict: 'accepted', reply: '42', reason: 'The observed integer is exactly 42.', evidence: ['Actual result is 42.'], acceptance: {status: 'Completed', satisfiedCriteria: criteria, artifacts: []}, activationAudit: {accepted: true, reason: 'The embedded operation and actual result agree.'}})
    prepared = completed(prepare)
    host.api('POST', "#{routine_path}/schedule", operation: 'null-schedule', revision: 1, minutes: 60)
    host.stop
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      sequence = @routine.split('_').last.to_i
      row = JSON.parse(db.get_first_value('SELECT value FROM scheduled_routines WHERE sequence=?', sequence))
      row['nextDue'] = '2020-01-01T00:00:00.000000+00:00'
      db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?', [JSON.generate(row), sequence])
    end
    host.start
    scheduled = Contract.wait(timeout: 90, description: 'real scheduled code occurrence') { routine.fetch('runs').find { |run| run['trigger'] == 'interval' && run['status'] == 'done' } }
    detail = run_detail(scheduled.fetch('id'))
    expect(detail.dig('executionBinding', 'inputs')).to eq([])
    expect(detail.dig('codeExecution', 'input')).to be_nil
    expect(detail.dig('codeExecution', 'runtimeResult', 'actual')).to eq('status' => 'ok', 'value' => 42)
    expect(routine.fetch('activeCode')).to include('reference' => prepared.dig('version', 'reference'), 'status' => 'active')
  end

  it 'keeps an ordinary admission queued before preparation on its original binding' do
    host.configure(rules: host.provider.fetch('rules').merge('Assessing' => {block: true, gate: 'occupied-before-prepare'}))
    blocker = host.submit(@owner, 'Occupy ordinary execution')
    Contract.wait { stage_calls('Assessing').any? }
    queued = submit_code('before-preparation')
    expect(run_detail(queued).dig('executionBinding', 'binding', 'mode')).to eq('ordinary')
    pending = prepare
    host.release('occupied-before-prepare')
    host.turn(blocker)
    host.turn(queued)
    prepared = completed(pending)
    settle_code(submit_code('activate-after-queue'))
    expect(run_detail(queued).dig('executionBinding', 'binding', 'mode')).to eq('ordinary')
    expect(run_detail(queued)['codeExecution']).to be_nil
    expect(routine.fetch('activeCode')).to include('reference' => prepared.dig('version', 'reference'))
  end

  it 'fails closed when a saved candidate source changes after authorized admission' do
    prepared = completed(prepare)
    install_create_ack_gate
    id = submit_code('tampered-trial')
    Contract.wait(timeout: 20) { File.exist?(@docker_created) }
    modify_preparation(prepared.fetch('id')) { |row| row.fetch('version')['source'] += "\nprint('tampered')\n" }
    File.write(@docker_gate, '')
    code = settle_code(id).fetch('codeExecution')
    expect(code.fetch('status')).not_to eq('completed')
    expect(File.exist?(@docker_started)).to be(false)
    expect(stage_calls('CodeAssessment')).to be_empty
    expect(routine['activeCode']).to be_nil
    expect(host.request('GET', "/api/agents/#{host.agent('OtherOwner')}/routines/#{@routine}/runs/#{id}").first).to eq(404)
  end

  it 'refuses an external profile without replacing a genuinely checked or active code version' do
    prepared = completed(prepare('checked-before-profile'))
    version = prepared.fetch('version')
    profile = {kind: 'receipt_feed_v1', taskBoundary: 'content_result', executionPolicy: 'response_only',
      source: {url: 'http://127.0.0.1:9/feed', identity: 'sales-feed', initialCursor: 'c0'},
      delivery: {url: 'http://127.0.0.1:9/deliveries', receiptUrl: 'http://127.0.0.1:9/receipts', identity: 'daily-inbox'}}
    fields = {revision: 1, title: 'Sales report', goal: CandidateFixtures::GOAL, criteria: CandidateFixtures::CRITERIA, externalProfile: profile}
    before = routine
    expect(host.request('POST', "#{routine_path}/edit", fields.merge(operation: 'profile-over-checked')).first).to eq(409)
    expect(routine).to eq(before)
    expect(preparation(prepared.fetch('id')).fetch('version')).to eq(version)
    trial = run_code('activate-unchanged', observed_input)
    expect(trial.dig('codeExecution', 'reference')).to eq(version.fetch('reference'))
    expect(routine.fetch('activeCode')).to include('status' => 'active', 'reference' => version.fetch('reference'))
    active = routine
    expect(host.request('POST', "#{routine_path}/edit", fields.merge(operation: 'profile-over-active')).first).to eq(409)
    expect(routine).to eq(active)
    expect(preparation(prepared.fetch('id')).fetch('version')).to eq(version)
  end

end
