require 'open3'
require 'sqlite3'
require_relative 'support/candidate_fixtures'
require_relative 'support/desktop'

RSpec.describe 'Consumer-specific suspect inspection and Kit upgrade' do
  include CandidateFixtures
  USAGE = {input_tokens: 17, output_tokens: 7}.freeze
  SECRET = 'FIRST_CONSUMER_PRIVATE_MARKER'.freeze

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
    @owner = host.agent('KitOwner')
    rules('Execution' => {response: decision('Outcome', reply: 'Observed sales under the original criteria.', outcome: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}), usage: USAGE},
      'AutomationAnalysis' => {response: decision('AutomationAnalyzed', decision: {status: 'Ready', automatablePart: 'Compute supplied sales.', parameters: [{name: 'csv', description: 'Sales rows'}]}), usage: USAGE},
      'PreparationFixtures' => {response: {fixtures: independent_fixtures, reason: 'Independent held-out values and invalid inputs.'}, usage: USAGE},
      'PreparationGeneration' => {response: {language: 'python', profile: 'python-3.13.15-linux-arm64', source: broken_source, interface: 'json-stdin-result-v1', dependencies: [], capabilities: []}, usage: USAGE},
      'PreparationReview' => {response: {accepted: true, reason: 'Actual independent fixture results match.'}, usage: USAGE},
      'CodeAssessment' => {response: {verdict: 'accepted', reply: 'Actual sales satisfy the original task.', reason: 'The actual result agrees with the criteria.', evidence: ['Actual isolated calculation.'], acceptance: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}, activationAudit: {accepted: true, reason: 'Own independent fixtures and this authorized occurrence support the exact version.'}}, usage: USAGE})
    @first = create_routine('first', 'Original sales')
    observe(@owner, @first, observed_input.merge(private_marker: SECRET))
    @first_preparation = prepare(@owner, @first, 'prepare-first')
    @source = await_preparation(@owner, @first, @first_preparation)
    @first_trial = run_routine(@owner, @first, 'activate-first', observed_input.merge(private_marker: SECRET))
    @source_ref = @source.dig('version', 'reference')
    @first_binding = routine(@first).fetch('activeCode')
    @first_history = routine(@first).fetch('runs')
  end
  after { @desktop&.close; @host&.close }

  def rules(changes); host.configure(rules: host.provider.fetch('rules', {}).merge(changes)); end
  def path(owner, id); "/api/agents/#{owner}/routines/#{id}"; end
  def routine(id); host.state.fetch('personal_routines').find { |row| row['id'] == id }; end
  def create_routine(operation, title, owner = @owner)
    host.api('POST', "/api/agents/#{owner}/routines", operation: operation, title: title, goal: CandidateFixtures::GOAL, criteria: CandidateFixtures::CRITERIA).fetch('routine')
  end
  def run_routine(owner, id, operation, input)
    identity = host.api('POST', "#{path(owner, id)}/run", operation: operation, revision: routine(id).fetch('revision'), inputs: JSON.generate(input)).fetch('run')
    host.turn(identity)
    identity
  end
  def observe(owner, id, input)
    run_routine(owner, id, "observe-#{id}", input)
    Contract.wait { row = routine(id)['analysis']; row if row && row['status'] == 'Ready' }
  end
  def prepare(owner, id, operation)
    host.api('POST', "#{path(owner, id)}/prepare", operation: operation, revision: routine(id).fetch('revision'), analysis: routine(id).fetch('analysis').fetch('id')).fetch('preparation')
  end
  def preparation(owner, id, identity); host.api('GET', "#{path(owner, id)}/preparations/#{identity}"); end
  def await_preparation(owner, id, identity, status = 'awaiting_occurrence')
    Contract.wait(timeout: 90, description: "preparation #{status}") do
      saved = preparation(owner, id, identity)
      raise JSON.pretty_generate(saved) if status == 'awaiting_occurrence' && %w[failed stopped interrupted].include?(saved['status'])
      if saved['status'] == status
        host.turn(identity, %w[failed interrupted cancelled]) if status == 'failed'
        saved
      end
    end
  end
  def second_input; {csv: CandidateFixtures::HELDOUT, start: '2026-12-05', end: '2026-12-06', private_marker: 'SECOND_CONSUMER_PRIVATE_MARKER'}; end
  def select_reuse(reference = @source_ref, **extra)
    rules('PreparationGeneration' => {response: {reuse: reference}, usage: USAGE}.merge(extra))
  end
  def second_consumer(operation = 'second')
    id = create_routine(operation, 'Reused sales')
    observe(@owner, id, second_input)
    entry = routine(id).fetch('kit').fetch('entries').find { |row| row['compatible'] && row['sourceHash'] == @source.dig('version', 'sourceHash') }
    entry ||= routine(id).fetch('kit').fetch('entries').find { |row| row['compatible'] }
    select_reuse(entry.fetch('reference'))
    [id, prepare(@owner, id, "prepare-#{operation}")]
  end
  def work(identity)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) { |db| JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', identity)) }
  end
  def edit_work(identity)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      value = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', identity))
      yield value
      db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(value), identity])
    end
  end
  def provider_calls(node); host.calls.select { |call| call['node'] == node }; end


  def broken_source
    candidate_source.sub('request = json.load(sys.stdin)', "request = json.load(sys.stdin)\nif request.get('force_failure'): raise ValueError('Shared calculation fault')")
  end
  def wait_case(id, status)
    Contract.wait(timeout: 90, description: "#{id} repair #{status}") do
      saved = routine(id)['repair']
      saved if saved && saved['status'] == status && saved.fetch('reservations').none? { |row| row['status'] == 'reserved' }
    end
  end
  def activate_second
    second, preparation_id = second_consumer
    prepared = await_preparation(@owner, second, preparation_id)
    run_routine(@owner, second, 'activate-second', second_input)
    [second, prepared]
  end
  def fault_source(inspection)
    rules('KitInspection' => inspection,
      'LocalFallback' => {response: {resolved: true, result: {status: 'ok', value: {total_minor: 5000}}, reason: 'Use retained input safely.'}, usage: USAGE},
      'WholeFallback' => {response: {resolved: true, result: {status: 'ok', value: {total_minor: 200}}, text: 'Safe ordinary calculation from retained input.', reason: 'Shared bytes are held.'}, usage: USAGE})
    run_routine(@owner, @first, 'real-shared-fault', observed_input.merge(force_failure: true))
  end
  def repair_source_and_offer(second)
    rules('PreparationGeneration' => {response: {language: 'python', profile: 'python-3.13.15-linux-arm64', source: candidate_source, interface: 'json-stdin-result-v1', dependencies: [], capabilities: []}, usage: USAGE})
    saved = wait_case(@first, 'awaiting_verification')
    run_routine(@owner, @first, 'verify-source-repair', observed_input)
    recovered = wait_case(@first, 'recovered')
    offer = Contract.wait { row = routine(second).dig('kitConsumer', 'offers')&.last; row if row }
    expect(offer.fetch('reference')).to eq(recovered.fetch('recoveredReference'))
    [offer, saved]
  end

  it 'holds the old pin for ordinary admission, inspects once and upgrades only after this consumer own checks and authorized audit' do
    second, original = activate_second
    old_pin = routine(second).dig('activeCode', 'reference')
    fault_source(response: {affected: false, reason: 'This consumer accepts normal sales inputs; its own examples do not request the internal fault flag.'}, usage: USAGE, block: true, gate: 'local-inspection')
    pending = Contract.wait do
      row = routine(second)['repair']
      row if row && row['status'] == 'inspecting_kit' && provider_calls('KitInspection').length == 1
    end
    expect(routine(second).dig('activeCode', 'reference')).to eq(old_pin)
    expect(routine(second).dig('activeCode', 'status')).to eq('kit_pending')
    queued = host.api('POST', "#{path(@owner, second)}/run", operation: 'ordinary-while-pending', revision: routine(second).fetch('revision'), inputs: JSON.generate(second_input)).fetch('run')
    expect(host.api('GET', "#{path(@owner, second)}/runs/#{queued}").dig('executionBinding', 'binding', 'mode')).to eq('ordinary')
    host.release('local-inspection')
    inspected = wait_case(second, 'inspection_complete')
    host.turn(queued)
    expect(inspected.fetch('kitInspections').last.dig('verdict', 'affected')).to be(false)
    expect(inspected.fetch('spentMilliseconds')).to be_between(1, 1_799_999)
    expect(inspected.fetch('limitMilliseconds')).to eq(1_800_000)
    expect(routine(second).dig('activeCode', 'reference')).to eq(old_pin)
    offer, = repair_source_and_offer(second)
    expect(routine(second).dig('activeCode', 'reference')).to eq(old_pin)
    select_reuse(offer.fetch('reference'))
    Contract.wait { routine(second)['analysis'] if routine(second).dig('analysis', 'status') == 'Ready' }
    candidate = prepare(@owner, second, 'own-upgrade-preparation')
    prepared = await_preparation(@owner, second, candidate)
    expect(prepared.dig('version', 'source')).to eq(candidate_source)
    expect(prepared.dig('version', 'expectedBinding', 'reference')).to eq(old_pin)
    expect(routine(second).dig('activeCode', 'reference')).to eq(old_pin)
    expect(prepared.fetch('checks')).to all(include('matched' => true))
    trial = run_routine(@owner, second, 'own-upgrade-trial', second_input)
    detail = host.api('GET', "#{path(@owner, second)}/runs/#{trial}")
    expect(detail.dig('codeExecution', 'runtimeResult', 'actual', 'value', 'total_minor')).to eq(200)
    expect(detail.dig('codeExecution', 'activation', 'status')).to eq('activated')
    expect(routine(second).dig('activeCode', 'reference')).to eq(prepared.dig('version', 'reference'))
    expect(routine(second).dig('activeCode', 'reference')).not_to eq(offer.fetch('reference'))
    expect(provider_calls('KitInspection').length).to eq(1)
    expect(routine(second).fetch('repair').fetch('id')).to eq(pending.fetch('id'))
    expect(routine(second).fetch('repair').fetch('limitMilliseconds')).to eq(1_800_000)
  end

  def activate_named(operation)
    id, preparation_id = second_consumer(operation)
    await_preparation(@owner, id, preparation_id)
    run_routine(@owner, id, "activate-#{operation}", second_input)
    id
  end
  def edit_definition(id)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.busy_timeout = 5000
      sequence, raw = db.get_first_row('SELECT sequence,value FROM scheduled_routines WHERE json_extract(value,\'$.id\')=?', id)
      value = JSON.parse(raw)
      yield value
      db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?', [JSON.generate(value), sequence])
    end
  end

  it 'holds unknown inspection without refill, stops live inspection and preserves a concurrent binding against late upgrade acceptance' do
    second, = activate_second
    third = activate_named('third')
    old_second = routine(second).dig('activeCode', 'reference')
    old_third = routine(third).dig('activeCode', 'reference')
    fault_source(response: {affected: nil, reason: 'Applicability remains unknown.'}, usage: USAGE,
      variants: [{contains: third, response: {affected: false, reason: 'Late verdict must not publish after Stop.'}, block: true, gate: 'stop-inspection'}])
    unknown = wait_case(second, 'kit_inspection_unknown')
    live = Contract.wait do
      row = routine(third)['repair']
      row if row && row['status'] == 'inspecting_kit' && provider_calls('KitInspection').length == 2
    end
    run = live.fetch('kitInspections').last.fetch('run')
    host.api('POST', "#{path(@owner, third)}/stop-repair", operation: 'stop-live-inspection', revision: routine(third).fetch('revision'), case: live.fetch('id'), reason: 'Stop this local inspection.')
    host.release('stop-inspection')
    host.turn(run, %w[interrupted cancelled failed])
    stopped = wait_case(third, 'stopped')
    expect(stopped.fetch('userStop')).to include('reason' => 'Stop this local inspection.')
    expect(routine(third).dig('activeCode', 'reference')).to eq(old_third)
    host.restart
    expect(routine(second).fetch('repair').fetch('id')).to eq(unknown.fetch('id'))
    expect(routine(second).fetch('repair').fetch('spentMilliseconds')).to eq(unknown.fetch('spentMilliseconds'))
    expect(routine(second).fetch('repair').fetch('limitMilliseconds')).to eq(1_800_000)
    expect(routine(second).dig('activeCode', 'reference')).to eq(old_second)
    expect(provider_calls('KitInspection').length).to eq(2)
    expect(routine(third).fetch('repair').fetch('status')).to eq('stopped')
    offer, = repair_source_and_offer(second)
    # Attribution fixture: an explanation-only failure cannot poison the new calculation bytes.
    source_run = routine(@first).dig('activeCode', 'run')
    edit_work(source_run) { |value| value.fetch('codeExecution')['faults'] = [{scope: 'explanation', reason: 'Explanation-only attribution fixture'}] }
    entry = routine(second).fetch('kit').fetch('entries').find { |row| row['reference'] == offer.fetch('reference') }
    expect(entry.fetch('compatible')).to be(true)
    select_reuse(offer.fetch('reference'))
    Contract.wait { routine(second)['analysis'] if routine(second).dig('analysis', 'status') == 'Ready' }
    prepared = await_preparation(@owner, second, prepare(@owner, second, 'independent-upgrade-cas'))
    accepted = host.provider.fetch('rules').fetch('CodeAssessment')
    rules('CodeAssessment' => accepted.merge(block: true, gate: 'late-upgrade-audit'))
    before = provider_calls('CodeAssessment').length
    trial = host.api('POST', "#{path(@owner, second)}/run", operation: 'late-upgrade-trial', revision: routine(second).fetch('revision'), inputs: JSON.generate(second_input)).fetch('run')
    Contract.wait { provider_calls('CodeAssessment').length > before }
    concurrent = nil
    # A controlled public-file identity fixture models another committed binding.
    # It is not claimed as a second real concurrent model audit.
    edit_definition(second) do |definition|
      concurrent = definition.fetch('activeCode').merge('reference' => prepared.dig('version', 'reference'), 'reason' => 'Controlled concurrent binding fixture')
      definition['activeCode'] = concurrent
    end
    host.release('late-upgrade-audit')
    host.turn(trial)
    result = host.api('GET', "#{path(@owner, second)}/runs/#{trial}")
    expect(result.dig('codeExecution', 'activation', 'status')).to eq('not_activated')
    expect(routine(second).fetch('activeCode').reject { |key, _| key == 'kitArtifact' }).to eq(concurrent.reject { |key, _| key == 'kitArtifact' })
    expect(provider_calls('KitInspection').length).to eq(2)
  end

  it 'shows suspect consumers, local basis, a proposed repair and own upgrade progress with Stop in the native UI', :desktop do
    second, = activate_second
    old = routine(second).dig('activeCode', 'reference')
    fault_source(response: {affected: false, reason: 'Local normal sales examples are unaffected; the old pin remains locally authorized.'}, usage: USAGE)
    wait_case(second, 'inspection_complete')
    offer, = repair_source_and_offer(second)
    select_reuse(offer.fetch('reference'))
    Contract.wait { routine(second)['analysis'] if routine(second).dig('analysis', 'status') == 'Ready' }
    await_preparation(@owner, second, prepare(@owner, second, 'visible-own-upgrade'))
    @desktop = Desktop.new(host.url)
    @desktop.click('Open KitOwner')
    @desktop.click('Work menu')
    @desktop.click('Automation menu')
    @desktop.click('Routines')
    @desktop.click('Reused sales')
    @desktop.see('Upgrade basis · expected prior pin')
    @desktop.control('scroll-to', '--path', @desktop.find('Kit · personal scripts').fetch('path'))
    @desktop.click('Kit · personal scripts')
    @desktop.see('Shared suspect bytes')
    @desktop.see('Local inspection: completed')
    @desktop.see('Local normal sales examples are unaffected')
    @desktop.see('Proposed version ·')
    @desktop.see(offer.fetch('reference').fetch('digest')[0, 12])
    @desktop.control('scroll-to', '--path', @desktop.find('Checked consumers').fetch('path'))
    @desktop.click('Checked consumers')
    @desktop.see(second)
    @desktop.control('scroll-to', '--path', @desktop.find('Stop repair…').fetch('path'))
    @desktop.click('Stop repair…')
    @desktop.click('Stop repair')
    wait_case(second, 'stopped')
    @desktop.see('Repair: stopped')
    expect(routine(second).dig('activeCode', 'reference')).to eq(old)
  end

end
