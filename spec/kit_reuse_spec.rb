require 'open3'
require 'sqlite3'
require_relative 'support/candidate_fixtures'
require_relative 'support/desktop'

RSpec.describe 'Owner-personal immutable Kit reuse' do
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
      'PreparationGeneration' => {response: {language: 'python', profile: 'python-3.13.15-linux-arm64', source: candidate_source, interface: 'json-stdin-result-v1', dependencies: [], capabilities: []}, usage: USAGE},
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
    select_reuse
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

  it 'automatically reuses exact Python bytes for a second independent consumer and survives deletion of the first Routine' do
    second, identity = second_consumer
    reused = await_preparation(@owner, second, identity)
    artifact = reused.dig('version', 'kitArtifact')
    expect(artifact).to include('owner' => @owner, 'scope' => 'personal')
    expect(artifact.dig('provenance', 'preparation')).to eq(@source_ref)
    expect(reused.dig('version', 'source')).to eq(@source.dig('version', 'source'))
    expect(reused.dig('version', 'sourceHash')).to eq(@source.dig('version', 'sourceHash'))
    expect(reused.dig('version', 'reference')).not_to eq(@source_ref)
    expect(reused.fetch('checks').length).to eq(independent_fixtures.length)
    expect(reused.fetch('checks')).to all(include('matched' => true, 'version' => reused.dig('version', 'reference')))
    expect(routine(second).fetch('runs').length).to eq(1)
    expect(routine(second)['activeCode']).to be_nil
    generated = provider_calls('PreparationGeneration').last
    expect(generated.fetch('prompt')).to include('availableKit')
    expect(generated.fetch('prompt')).not_to include(SECRET)
    expect(routine(second).fetch('kit').to_s).not_to include(SECRET)
    expect(routine(second).fetch('kit')).to include('owner' => @owner, 'scope' => 'personal')
    trial = run_routine(@owner, second, 'independent-target-trial', second_input)
    detail = host.api('GET', "#{path(@owner, second)}/runs/#{trial}")
    expect(detail.dig('codeExecution', 'runtimeResult', 'actual', 'value', 'total_minor')).to eq(200)
    expect(detail.dig('executionBinding', 'binding')).to include('role' => 'trial', 'reference' => reused.dig('version', 'reference'))
    expect(detail.dig('codeExecution', 'activationAudit', 'accepted')).to be(true)
    expect(routine(second).dig('activeCode', 'reference')).to eq(reused.dig('version', 'reference'))
    expect(routine(@first).fetch('activeCode')).to eq(@first_binding)
    expect(routine(@first).fetch('runs')).to eq(@first_history)
    expect(provider_calls('PreparationGeneration').length).to eq(2)
    host.api('POST', "#{path(@owner, @first)}/delete", operation: 'delete-original-consumer', revision: routine(@first).fetch('revision'))
    host.restart
    later = run_routine(@owner, second, 'local-copy-after-source-deletion', second_input)
    expect(host.api('GET', "#{path(@owner, second)}/runs/#{later}").dig('executionBinding', 'binding', 'role')).to eq('active')
    expect(routine(second).dig('activeCode', 'reference')).to eq(reused.dig('version', 'reference'))
  end

  it 'refuses outside scope, a disappearing source, altered local bytes and known-bad aliases before execution' do
    outsider = host.agent('OtherKitOwner')
    other = create_routine('other-owner', 'Other owner sales', outsider)
    observe(outsider, other, second_input)
    expect(routine(other).dig('kit', 'entries')).to be_empty
    select_reuse
    invalid = prepare(outsider, other, 'cross-owner-selection')
    refused = await_preparation(outsider, other, invalid, 'failed')
    expect(refused.fetch('checks')).to be_empty
    expect(refused.fetch('error')).to include('not an applicable entry')
    target = create_routine('target', 'Reused sales')
    observe(@owner, target, second_input)
    select_reuse(block: true, gate: 'kit-selection')
    pending = prepare(@owner, target, 'selection-source-loss')
    Contract.wait { provider_calls('PreparationGeneration').last if provider_calls('PreparationGeneration').length == 3 }
    original = work(@first_preparation)
    # Simulate loss of the selected immutable file while the model is paused;
    # production source publication must still revalidate under its SQL guard.
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) { |db| db.execute('DELETE FROM workloads WHERE id=?', @first_preparation) }
    host.release('kit-selection')
    missing = await_preparation(@owner, target, pending, 'failed')
    expect(missing.fetch('checks')).to be_empty
    expect(missing.fetch('error')).to include('Selected Kit artifact is missing')
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) { |db| db.execute('INSERT INTO workloads(id,value) VALUES (?,?)', [@first_preparation, JSON.generate(original)]) }
    select_reuse
    attached_id = prepare(@owner, target, 'attach-restored-artifact')
    attached = await_preparation(@owner, target, attached_id)
    stored = work(attached_id)
    edit_work(attached_id) { |value| value.fetch('preparation').fetch('version')['source'] += "\n# changed after checks\n" }
    calls = host.calls.length
    runs = routine(target).fetch('runs').map { |row| row['id'] }
    host.api('POST', "#{path(@owner, target)}/run", operation: 'tampered-local-artifact', revision: routine(target).fetch('revision'), inputs: JSON.generate(second_input), status: 409)
    expect(host.calls.length).to eq(calls)
    expect(routine(target).fetch('runs').map { |row| row['id'] }).to eq(runs)
    edit_work(attached_id) { |value| value.replace(stored) }
    edit_work(@first_trial) { |value| value.fetch('codeExecution')['faults'] = [{reason: 'Persisted known artifact fault fixture'}] }
    catalog = routine(target).fetch('kit')
    expect(catalog.fetch('entries')).to all(include('compatible' => false))
    expect(catalog.fetch('entries').map { |row| row['reason'] }.join).to include('Known faulty artifact')
    # The second checked wrapper cannot launder the same bytes as a new source.
    select_reuse(attached.dig('version', 'reference'))
    alias_id = prepare(@owner, target, 'known-bad-alias')
    alias_refusal = await_preparation(@owner, target, alias_id, 'failed')
    expect(alias_refusal.fetch('checks')).to be_empty
    expect(alias_refusal.fetch('error')).to include('not an applicable entry')
    expect(routine(@first).fetch('activeCode')).to eq(@first_binding)
  end

  it 'shows personal catalog requirements, selected provenance and a new-reuse refusal in the native UI', :desktop do
    second, identity = second_consumer
    attached = await_preparation(@owner, second, identity)
    edit_work(@first_trial) { |value| value.fetch('codeExecution')['faults'] = [{reason: 'Persisted known artifact fault fixture'}] }
    @desktop = Desktop.new(host.url)
    @desktop.click('Open KitOwner')
    @desktop.click('Work menu')
    @desktop.click('Automation menu')
    @desktop.click('Routines')
    @desktop.click('Reused sales')
    @desktop.see('Kit source ·')
    @desktop.see(attached.dig('version', 'kitArtifact', 'id')[0, 12])
    @desktop.click('Kit · personal scripts')
    @desktop.see('personal scope only')
    @desktop.see('Required runtime: python-3.13.15-linux-arm64')
    @desktop.see('Known faulty artifact cannot be selected through another alias')
    expect(routine(second)['activeCode']).to be_nil
  end
end
