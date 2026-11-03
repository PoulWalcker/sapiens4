require 'rbconfig'
require 'open3'
require 'sqlite3'
require 'digest'
require_relative 'support/candidate_fixtures'
require_relative 'support/feed_code_fixtures'
require_relative 'support/effect_peer'
require_relative 'support/desktop'

RSpec.describe 'Doctor tools preserve the observed contract repair and interrupted-action cases' do
  include CandidateFixtures
  include FeedCodeFixtures

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

  def canonical_hash(value)
    ordered = lambda do |item|
      case item
      when Hash then item.sort.to_h.transform_values { |entry| ordered.call(entry) }
      when Array then item.map { |entry| ordered.call(entry) }
      else item
      end
    end
    Digest::SHA256.hexdigest(JSON.generate(ordered.call(value)))
  end

  def configure_access(resources, operation = 'select-doctor-files')
    Contract.wait { repair if repair.fetch('reservations').none? { |entry| entry['status'] == 'reserved' } }
    host.api('POST', "#{path}/doctor-access", operation: operation, revision: routine.fetch('revision'), case: repair.fetch('id'), resources: resources)
  end

  def tool_responses(responses, **options)
    host.configure(rules: host.provider.fetch('rules').merge('DoctorDiagnosis' => {responses: responses, usage: {input_tokens: 59, output_tokens: 31}}.merge(options)))
  end

  def final_diagnosis(verdict = 'implementation_defect')
    {verdict: verdict, diagnosis: 'The selected observation contract names a file absent from the implementation.', recommendation: 'Keep the original criteria and verify the corrected contract through the owner.', hypothesis: 'Remove the absent observation entry and preserve the accepted aggregation contract.'}
  end

  def fixture_work(identity)
    host.stop
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      value = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', identity))
      yield value
      db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(value), identity])
    end
    host.start
  end

  before do
    @selected_files = File.realpath(Dir.mktmpdir('doctor-selected-files-'))
    @contract_file = File.join(@selected_files, 'observation.json')
    @bad_contract = JSON.generate(files: ['ram_report.py'], readOnlyFiles: ['ram_report.py', 'absent.py'])
    File.write(@contract_file, @bad_contract)
  end

  after { FileUtils.remove_entry(@selected_files) if @selected_files && File.directory?(@selected_files) }

  it 'reads the selected contract, reproduces its mismatch in isolation, publishes only the authorized correction and still waits for owner verification' do
    fault
    wait_repair('waiting_doctor')
    granted = configure_access([{path: @contract_file, access: 'write'}])
    resource = granted.dig('access', 'resources').first.fetch('id')
    corrected = JSON.generate(files: ['ram_report.py'], readOnlyFiles: ['ram_report.py'])
    # A self-contained reproduction replaces the prototype's live application import.
    repro = "import json,sys\njson.load(sys.stdin)\ncontract=#{@bad_contract}\nprint(json.dumps({'missing':sorted(set(contract['readOnlyFiles'])-set(contract['files']))}))\n"
    repro_id = canonical_hash(name: 'contract-repro.py', content: repro)
    corrected_id = canonical_hash(name: 'observation.json', content: corrected)
    fixture = @original.fetch('fixtures').first
    tool_responses([
      {action: {tool: 'read_file', resource: resource}},
      {action: {tool: 'stage_file', name: 'contract-repro.py', content: repro}},
      {action: {tool: 'check_stage', stage: repro_id, fixture: canonical_hash(fixture)}},
      {action: {tool: 'stage_file', name: 'observation.json', content: corrected}},
      {action: {tool: 'apply_file', resource: resource, stage: corrected_id, expectedHash: Digest::SHA256.hexdigest(@bad_contract)}},
      {action: {tool: 'read_file', resource: resource}},
      final_diagnosis
    ])
    assign_doctor
    saved = wait_repair('awaiting_verification')
    expect(File.read(@contract_file)).to eq(corrected)
    actions = saved.fetch('operations').select { |entry| entry['purpose'] == 'repair_tool' }
    expect(actions.map { |entry| entry['tool'] }).to eq(%w[read_file stage_file check_stage stage_file apply_file read_file])
    expect(actions).to all(include('status' => 'completed'))
    check = actions.find { |entry| entry['tool'] == 'check_stage' }
    expect(JSON.parse(check.dig('result', 'runtimeResult', 'output'))).to eq('missing' => ['absent.py'])
    expect(check.dig('runtime', 'status')).to eq('removed')
    expect(actions.last.dig('result', 'hash')).to eq(Digest::SHA256.hexdigest(corrected))
    expect(saved.fetch('spentMilliseconds')).to be_between(1, 1_799_999)
    expect(saved.fetch('operations').count { |entry| entry['purpose'] == 'repair_diagnosis' }).to eq(7)
    expect(routine.dig('activeCode', 'reference')).to eq(@active.fetch('reference'))
    expect(saved['verificationRun']).to be_nil
    expect(host.calls.select { |call| call['node'] == 'DoctorDiagnosis' }).to all(satisfy { |call| call.fetch('args').include?('--ignore-user-config') && !call.fetch('args').include?('--dangerously-bypass-approvals-and-sandbox') && call['agency_run'].nil? })
  end

  it 'keeps selected observations available under an unknown original effect while refusing live repair and unsafe resource identities' do
    failed = fault
    wait_repair('waiting_doctor')
    fixture_work(failed) { |work| work['routineEffectUnknown'] = {decisionId: 'retained-original-action', reason: 'Original outcome requires independent confirmation.'} }
    linked = File.join(@selected_files, 'linked.json')
    File.symlink(@contract_file, linked)
    host.api('POST', "#{path}/doctor-access", operation: 'deny-symlink', revision: routine.fetch('revision'), case: repair.fetch('id'), resources: [{path: linked, access: 'write'}], status: 409)
    File.unlink(linked)
    File.link(@contract_file, linked)
    host.api('POST', "#{path}/doctor-access", operation: 'deny-hardlink', revision: routine.fetch('revision'), case: repair.fetch('id'), resources: [{path: @contract_file, access: 'write'}], status: 403)
    File.unlink(linked)
    granted = configure_access([{path: @contract_file, access: 'write'}])
    resource = granted.dig('access', 'resources').first.fetch('id')
    replacement = '{"fixed":true}'
    stage = canonical_hash(name: 'repair.json', content: replacement)
    tool_responses([
      {action: {tool: 'read_file', resource: resource}},
      {action: {tool: 'stage_file', name: 'repair.json', content: replacement}},
      {action: {tool: 'apply_file', resource: resource, stage: stage, expectedHash: Digest::SHA256.hexdigest(@bad_contract)}},
      final_diagnosis('request_access')
    ])
    assign_doctor
    saved = wait_repair('waiting_access')
    actions = saved.fetch('operations').select { |entry| entry['purpose'] == 'repair_tool' }
    expect(actions.first.dig('result', 'content')).to eq(@bad_contract)
    expect(actions.last).to include('status' => 'denied', 'tool' => 'apply_file')
    expect(actions.last.fetch('error')).to include('Original outcome requires independent confirmation')
    expect(File.read(@contract_file)).to eq(@bad_contract)
    expect(saved.fetch('attempts')).to be_empty
    expect(saved.fetch('limitMilliseconds')).to eq(1_800_000)
  end

  it 'retains a stopped Doctor action trace across restart and reassignment, fences its original consumer and never replays the action' do
    fault
    wait_repair('waiting_doctor')
    granted = configure_access([{path: @contract_file, access: 'write'}])
    resource = granted.dig('access', 'resources').first.fetch('id')
    changed = 'changed before stream loss'
    stage = canonical_hash(name: 'changed.txt', content: changed)
    tool_responses([
      {action: {tool: 'stage_file', name: 'changed.txt', content: changed}},
      {action: {tool: 'apply_file', resource: resource, stage: stage, expectedHash: Digest::SHA256.hexdigest(@bad_contract)}},
      final_diagnosis
    ], variants: [{contains: '"confirmed":true', event_delay: 0.35, events: [{type: 'item.started', item: {id: 'uncertain-doctor-action', type: 'file_change'}}], after_events_gate: 'doctor-stream'}])
    assign_doctor
    Contract.wait { File.exist?(File.join(host.root, 'doctor-stream-started')) }
    identity = repair.fetch('diagnosisRun')
    host.api('POST', "/api/agents/#{@doctor}/turns/#{identity}/cancel")
    saved = wait_repair('interrupted')
    expect(File.read(@contract_file)).to eq('changed before stream loss')
    expect(saved.fetch('operations').last.dig('evidence', 'events')).not_to be_empty
    host.restart
    replacement = host.agent('ReplacementDoctor')
    host.control(chief, 'corpora_update', revision: host.state.fetch('corpora').fetch('revision'), doctor_id: replacement)
    host.api('POST', "#{path}/doctor-access", operation: 'no-revive', revision: routine.fetch('revision'), case: repair.fetch('id'), resources: [], status: 409)
    blocked = host.api('POST', "#{path}/run", operation: 'same-consumer-after-unknown', revision: routine.fetch('revision'), inputs: JSON.generate(observed_input), status: 409)
    expect(blocked.fetch('error')).to include('Doctor provider trace')
    expect(host.calls.count { |call| call['node'] == 'DoctorDiagnosis' }).to eq(3)
    writes = repair.fetch('operations').select { |op| op['tool'] == 'apply_file' }
    expect(writes.length).to eq(1)
    expect(writes.first).to include('status' => 'completed')
    expect(writes.first.dig('result', 'confirmed')).to be(true)
    expect(File.read(@contract_file)).to eq(changed)
    other = host.api('POST', "/api/agents/#{@owner}/routines", operation: 'other-routine', title: 'Other consumer', goal: CandidateFixtures::GOAL, criteria: CandidateFixtures::CRITERIA).fetch('routine')
    other_run = host.api('POST', "/api/agents/#{@owner}/routines/#{other}/run", operation: 'other-run', revision: 1, inputs: JSON.generate(observed_input)).fetch('run')
    host.turn(other_run)
    expect(repair.fetch('id')).to eq(saved.fetch('id'))
  end

  it 'audits the exact retained result without a new calculation or candidate and refuses a later malformed negative verdict' do
    accepted = host.provider.fetch('rules').fetch('CodeAssessment')
    host.configure(rules: host.provider.fetch('rules').merge('CodeAssessment' => {response: {verdict: 'accepted'}, usage: {input_tokens: 13, output_tokens: 7}}))
    original = submit('retained-result-needs-audit')
    host.turn(original, 'failed')
    waiting = wait_repair('waiting_doctor')
    expect(waiting.fetch('kind')).to eq('saved_result_audit')
    expect(routine.dig('activeCode', 'status')).to eq('active')
    expect(host.api('GET', "#{path}/runs/#{original}").dig('codeExecution', 'faults') || []).to be_empty
    generation_count = host.calls.count { |call| call['node'] == 'PreparationGeneration' }
    occurrences = routine.fetch('runs').map { |run| run['id'] }
    host.configure(rules: host.provider.fetch('rules').merge('CodeAssessment' => accepted))
    assign_doctor
    recovered = wait_repair('recovered')
    expect(recovered.dig('verification', 'kind')).to eq('saved_result_audit')
    expect(recovered.dig('verification', 'source')).to eq(original)
    expect(recovered.dig('verification', 'auditRun')).not_to eq(original)
    expect(routine.fetch('runs').map { |run| run['id'] }).to eq(occurrences)
    expect(host.calls.count { |call| call['node'] == 'PreparationGeneration' }).to eq(generation_count)
    source = host.api('GET', "#{path}/runs/#{original}")
    expect(source.dig('codeExecution', 'operations').count { |op| op['purpose'] == 'code_execution' }).to eq(1)
    expect(source.dig('codeExecution', 'acceptance', 'verdict')).to eq('accepted')
    audit_usage = recovered.fetch('usage').select { |op| op['purpose'] == 'repair_saved_result_audit' }
    expect(audit_usage.length).to eq(1)
    expect(audit_usage.first.dig('evidence', 'usage')).to include('inputTokens' => 73, 'outputTokens' => 29)
    # A newer malformed negative assessment remains authoritative; it cannot be
    # erased by selecting this older positive audit or another format retry.
    host.control(chief, 'corpora_update', revision: host.state.fetch('corpora').fetch('revision'), doctor_id: nil)
    host.configure(rules: host.provider.fetch('rules').merge('CodeAssessment' => {response: {verdict: 'rejected', acceptance: {status: 'Unresolved'}}, usage: {input_tokens: 11, output_tokens: 5}}))
    negative = submit('newer-negative-result')
    host.turn(negative, 'failed')
    newer = wait_repair('waiting_doctor')
    expect(newer.fetch('auditSource')).to eq(negative)
    count = host.calls.count { |call| call['node'] == 'CodeAssessment' }
    assign_doctor
    refused = wait_repair('interrupted')
    expect(refused.fetch('terminalReason')).to include('rejected')
    expect(host.calls.count { |call| call['node'] == 'CodeAssessment' }).to eq(count)
    expect(host.calls.count { |call| call['node'] == 'PreparationGeneration' }).to eq(generation_count)
    expect(host.api('GET', "#{path}/runs/#{negative}").dig('codeExecution', 'operations').count { |op| op['purpose'] == 'code_execution' }).to eq(1)
    # A real receipt-feed observation with successful calculation but missing
    # assessment must not consume its cursor while required publication is absent.
    host.control(chief, 'corpora_update', revision: host.state.fetch('corpora').fetch('revision'), doctor_id: nil)
    peer = EffectPeer.new(File.join(host.root, 'saved-audit-peer'))
    begin
      peer.page('c0', records: [{id: 'r1', value: 10}, {id: 'r2', value: 20}], next_cursor: 'c2')
      peer.page('c2', records: [{id: 'r2', value: 20}, {id: 'r3', value: 30}], next_cursor: 'page:next-z9')
      profile = {kind: 'receipt_feed_v1', taskBoundary: 'content_result', executionPolicy: 'response_only', source: {url: peer.url + '/feed', identity: 'sales-feed', initialCursor: 'c0'}, delivery: {url: peer.url + '/deliveries', receiptUrl: peer.url + '/receipts', identity: 'daily-inbox'}}
      feed = host.api('POST', "/api/agents/#{@owner}/routines", operation: 'audit-feed', title: 'Retained feed audit', goal: FeedCodeFixtures::GOAL, criteria: FeedCodeFixtures::CRITERIA, externalProfile: profile).fetch('routine')
      feed_path = "/api/agents/#{@owner}/routines/#{feed}"
      read_feed = -> { host.state.fetch('personal_routines').find { |row| row['id'] == feed } }
      host.configure(rules: host.provider.fetch('rules').merge(
        'Execution' => {response: decision('Outcome', reply: FeedCodeFixtures::OBSERVED_TEXT, outcome: {status: 'Completed', satisfiedCriteria: FeedCodeFixtures::CRITERIA, artifacts: []}), usage: {input_tokens: 17, output_tokens: 7}},
        'AutomationAnalysis' => {response: decision('AutomationAnalyzed', decision: {status: 'Ready', automatablePart: 'Render only newly observed feed records.', parameters: [{name: 'prefix', description: 'Content prefix'}]}), usage: {input_tokens: 17, output_tokens: 7}}
      ))
      observed = host.api('POST', "#{feed_path}/run", operation: 'observe-feed-audit', revision: 1, inputs: JSON.generate(FeedCodeFixtures::PARAMETERS)).fetch('run')
      host.turn(observed)
      analysis = Contract.wait { read_feed.call['analysis'] if read_feed.call.dig('analysis', 'status') == 'Ready' }
      host.configure(rules: host.provider.fetch('rules').merge(
        'PreparationFixtures' => {response: {contentInterface: 'script-text-v1', fixtures: independent_feed_cases, reason: 'Independent source and overlap observations.'}, usage: {input_tokens: 17, output_tokens: 7}},
        'PreparationGeneration' => {response: {language: 'python', profile: 'python-3.13.15-linux-arm64', source: feed_code_source, interface: 'json-stdin-result-v1', dependencies: [], capabilities: []}, usage: {input_tokens: 17, output_tokens: 7}},
        'CodeAssessment' => {response: {verdict: 'accepted'}, usage: {input_tokens: 13, output_tokens: 7}}
      ))
      prepared = host.api('POST', "#{feed_path}/prepare", operation: 'prepare-feed-audit', revision: 1, analysis: analysis.fetch('id')).fetch('preparation')
      Contract.wait(timeout: 90) { host.api('GET', "#{feed_path}/preparations/#{prepared}")['status'] == 'awaiting_occurrence' }
      before_cursor = read_feed.call.fetch('sourceState')
      before_deliveries = peer.deliveries.length
      original_feed = host.api('POST', "#{feed_path}/run", operation: 'feed-assessment-failure', revision: 1, inputs: JSON.generate(FeedCodeFixtures::PARAMETERS)).fetch('run')
      host.turn(original_feed, 'failed')
      Contract.wait { read_feed.call.dig('repair', 'status') == 'waiting_doctor' }
      before_audits = host.calls.count { |call| call['node'] == 'CodeAssessment' }
      assign_doctor
      refused_feed = Contract.wait { row = read_feed.call['repair']; row if row && row['status'] == 'interrupted' }
      expect(refused_feed.fetch('terminalReason')).to include('cannot advance its cursor or send it')
      expect(read_feed.call.fetch('sourceState')).to eq(before_cursor)
      expect(peer.deliveries.length).to eq(before_deliveries)
      expect(host.calls.count { |call| call['node'] == 'CodeAssessment' }).to eq(before_audits)
      retained_feed = host.api('GET', "#{feed_path}/runs/#{original_feed}")
      expect(retained_feed.dig('externalExecution', 'contentAcceptance')).to be_nil
      expect(retained_feed.dig('externalExecution', 'cursorCommit')).to be_nil
    ensure
      peer.close
    end
  end

  it 'shows explicit selected access, actual tool work and the wait for independent confirmation in the native UI', :desktop do
    fault
    wait_repair('waiting_doctor')
    start_desktop
    desktop.click('Open RepairOwner')
    desktop.click('Work menu')
    desktop.click('Automation menu')
    desktop.click('Routines')
    desktop.click('Repairable sales')
    desktop.click('Doctor file access…')
    desktop.see('Read-only files (absolute paths, one per line)')
    desktop.fill('Read-only files (absolute paths, one per line)', File.join(@selected_files, 'missing.json'))
    desktop.click('Save Doctor access')
    desktop.see('Doctor resource path is unavailable or changed')
    desktop.fill('Read-only files (absolute paths, one per line)', @contract_file)
    desktop.click('Save Doctor access')
    Contract.wait { repair.dig('access', 'resources')&.length == 1 }
    desktop.click('Doctor file access…')
    desktop.see(@contract_file)
    desktop.click('Close dialog')
    resource = repair.dig('access', 'resources').first.fetch('id')
    tool_responses([{action: {tool: 'read_file', resource: resource}}, final_diagnosis('request_access')])
    assign_doctor
    wait_repair('waiting_access')
    desktop.see('Waiting for file access or independent confirmation.')
    desktop.see('The selected observation contract names a file absent from the implementation.')
    desktop.click('Doctor access and actions')
    desktop.see(@contract_file)
    desktop.see('read_file · completed')
  end
end
