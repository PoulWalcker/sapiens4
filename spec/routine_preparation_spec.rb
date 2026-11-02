require 'digest'
require 'rbconfig'
require 'open3'
require 'timeout'
require 'sqlite3'
require_relative 'support/candidate_fixtures'
require_relative 'support/candidate_docker'

RSpec.describe 'Preparing pure routine candidates with real bounded execution' do
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
        if labels['sapiens.owner'] == @owner && labels['sapiens.preparation'] == @owned_container['preparation'] && labels['sapiens.nonce'] == @owned_container['nonce']
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

  it 'executes genuine CSV candidate bytes on independent inputs before separate review without business execution' do
    id = prepare
    saved = completed(id)
    version = saved.fetch('version')
    expect(version.fetch('source')).to eq(candidate_source)
    expect(version.fetch('sourceHash')).to eq(Digest::SHA256.hexdigest(candidate_source))
    expect(saved.fetch('checks').length).to eq(independent_fixtures.length)
    expect(saved.fetch('checks')).to all(include('matched' => true, 'exitCode' => 0))
    expect(saved.fetch('checks').map { |check| check['actual'] }).to eq(JSON.parse(JSON.generate(independent_fixtures.map { |sample| sample[:expected] })))
    generator = stage_calls('PreparationGeneration').fetch(0).fetch('prompt')
    expect(generator).not_to include('14999', '2026-12-05', 'tools,250')
    operations = saved.fetch('operations')
    expect(operations.map { |operation| operation['purpose'] }).to include('fixture_design', 'candidate_generation', 'environment', 'fixture_check', 'candidate_review')
    expect(operations.select { |operation| operation['purpose'] == 'environment' }).to all(include('status' => 'completed', 'durationMs' => a_value >= 0))
    expect(operations.select { |operation| %w[fixture_design candidate_generation candidate_review].include?(operation['purpose']) }.map { |operation| operation.dig('evidence', 'policy') }).to all(eq('response_only'))
    expect(stage_calls('PreparationFixtures').length).to eq(1)
    expect(stage_calls('PreparationReview').length).to eq(1)
    expect(routine.fetch('runs').map { |run| run['id'] }).to eq([@business])
    host.restart
    expect(preparation(id)).to eq(saved)
    expect(prepare).to eq(id)
    conflict, = host.request('POST', "#{routine_path}/prepare", operation: 'prepare', revision: 1, analysis: @business)
    expect(conflict).to eq(409)
    expect(stage_calls('PreparationGeneration').length).to eq(1)
  end
  it 'rejects a known-example constant answer even with exit zero and a positive reviewer' do
    observed = {status: 'ok', value: {start: '2026-11-01', end: '2026-11-02', row_count: 4, by_category_minor: {books: 2000, games: 3000}, total_minor: 5000}}
    configure_preparation(source: "print(#{JSON.generate(observed).inspect})\n")
    row = completed(prepare, 'failed')
    expect(row.fetch('checks').first).to include('matched' => false, 'exitCode' => 0)
    expect(stage_calls('PreparationReview')).to be_empty
    expect(routine.fetch('runs').map { |run| run['id'] }).to eq([@business])
  end

  {'syntax error' => 'def broken(', 'runtime exception' => 'raise RuntimeError("fixture failure")', 'non JSON output' => 'print("not a result envelope")'}.each do |kind, source|
    it "rejects #{kind} from the actual candidate body" do
      configure_preparation(source: source)
      row = completed(prepare, 'failed')
      expect(row.fetch('checks')).not_to be_empty
      expect(row.fetch('checks').none? { |check| check['matched'] }).to be(true)
      expect(stage_calls('PreparationReview')).to be_empty
    end
  end

  it 'requires an independent expected result instead of treating its absence as a passing check' do
    configure_preparation(fixtures: [{input: independent_fixtures.first.fetch(:input)}])
    row = completed(prepare, 'failed')
    expect(row.fetch('checks', [])).to be_empty
    expect(stage_calls('PreparationGeneration')).to be_empty
  end

  it 'preserves successful real checks when independent review is explicitly stopped' do
    configure_preparation(review_rule: {block: true, gate: 'held-review'})
    id = prepare
    Contract.wait(timeout: 90, description: 'independent review starts after checks') { stage_calls('PreparationReview').last }
    before = preparation(id).fetch('checks')
    expect(before).to all(include('matched' => true))
    host.api('POST', "#{routine_path}/stop-preparation", operation: 'stop-review', revision: 1, preparation: id)
    host.release('held-review')
    stopped = completed(id, 'stopped')
    expect(stopped.fetch('checks')).to eq(before)
    host.restart
    expect(preparation(id).fetch('checks')).to eq(before)
    expect(preparation(id).fetch('status')).to eq('stopped')
    expect(stage_calls('PreparationGeneration').length).to eq(1)
    expect(stage_calls('PreparationReview').length).to eq(1)
  end

  it 'rejects an unavailable selected language without substituting a native runtime' do
    configure_preparation(generation: {language: 'unavailable-language'})
    row = completed(prepare, 'failed')
    expect(row.fetch('checks', [])).to be_empty
    expect(stage_calls('PreparationReview')).to be_empty
  end

  it 'reconciles a real running owned container after whole-host SIGKILL without replaying preparation' do
    configure_preparation(source: 'while True: pass')
    id = prepare
    @owned_container = Contract.wait(timeout: 90, description: 'actual owned container running') do
      container = preparation(id)['container']
      container if container && container['id'] && container['status'] == 'running'
    end
    inspected, _, inspect_status = docker('inspect', @owned_container.fetch('id'))
    expect(inspect_status.success?).to be(true)
    config = JSON.parse(inspected).fetch(0)
    expect(config.fetch('HostConfig')).to include('NetworkMode' => 'none', 'ReadonlyRootfs' => true, 'Privileged' => false, 'Memory' => 134217728, 'MemorySwap' => 134217728, 'PidsLimit' => 16)
    expect(config.fetch('Config').fetch('Labels')).to include('sapiens.owner' => @owner, 'sapiens.preparation' => id, 'sapiens.nonce' => @owned_container.fetch('nonce'))
    expect(config.fetch('Mounts').none? { |mount| %w[bind volume].include?(mount['Type']) }).to be(true)
    host.crash
    _, _, before = docker('inspect', @owned_container.fetch('id'))
    expect(before.success?).to be(true)
    host.start
    Contract.wait(timeout: 30, description: 'restart removes owned runtime') do
      _, error, status = docker('inspect', @owned_container.fetch('id'))
      !status.success? && error.match?(/No such (object|container)/i)
    end
    row = preparation(id)
    expect(row.fetch('status')).to eq('interrupted')
    expect(stage_calls('PreparationGeneration').length).to eq(1)
    expect(stage_calls('PreparationReview')).to be_empty
    expect(routine.fetch('runs').map { |run| run['id'] }).to eq([@business])
  end

  it 'prevents late publication when the routine goal changes and then changes back' do
    configure_preparation(generation_rule: {block: true, gate: 'held-generation'})
    id = prepare
    Contract.wait { stage_calls('PreparationGeneration').last }
    fields = {title: 'Sales report', criteria: CandidateFixtures::CRITERIA}
    host.api('POST', "#{routine_path}/edit", fields.merge(operation: 'change-goal', revision: 1, goal: 'A different accounting task'))
    host.api('POST', "#{routine_path}/edit", fields.merge(operation: 'restore-goal', revision: 2, goal: CandidateFixtures::GOAL))
    host.release('held-generation')
    row = completed(id, 'stale')
    expect(row.fetch('checks', [])).to be_empty
    expect(stage_calls('PreparationReview')).to be_empty
    host.restart
    expect(preparation(id).fetch('status')).to eq('stale')
    generation_calls = stage_calls('PreparationGeneration').length
    host.api('POST', "#{routine_path}/analyze", operation: 'inspect-historical-source', revision: 3)
    historical = Contract.wait(description: 'current analysis of historical source') do
      current = routine['analysis']
      current if current && current['id'] != @analysis.fetch('id') && current['status'] == 'Ready'
    end
    code, = host.request('POST', "#{routine_path}/prepare", operation: 'reject-historical-preparation', revision: 3, analysis: historical.fetch('id'))
    expect(code).to eq(409)
    expect(stage_calls('PreparationGeneration').length).to eq(generation_calls)
  end

  it 'rejects effect capabilities and dependencies before any candidate check' do
    configure_preparation(generation: {capabilities: ['network'], dependencies: ['unavailable-package']})
    row = completed(prepare, 'failed')
    expect(row.fetch('checks', [])).to be_empty
    expect(stage_calls('PreparationReview')).to be_empty
    expect(host.turn(host.submit(@owner, 'Still available')).fetch('output')).to eq('Contract test reply')
  end

  it 'rejects another owner inspecting a prepared candidate reference' do
    id = prepare
    completed(id)
    other = host.agent('OtherCandidateOwner')
    code, body = host.request('GET', "/api/agents/#{other}/routines/#{@routine}/preparations/#{id}")
    expect(code).to eq(404)
    expect(body.to_json).not_to include(candidate_source)
  end

  it 'fails visibly with an unavailable declared daemon before generation or body execution' do
    host.environment['SAPIENS_GENERATED_SOCKET'] = 'unix://' + File.join(host.root, 'absent-runtime.sock')
    host.restart
    row = completed(prepare, 'failed')
    expect(row.fetch('checks', [])).to be_empty
    expect(row['container']).to be_nil
    expect(stage_calls('PreparationGeneration')).to be_empty
    expect(host.turn(host.submit(@owner, 'Still available')).fetch('output')).to eq('Contract test reply')
  end

  it 'stops an actual candidate that never reads its near-limit input and removes its container' do
    sample = {input: {csv: 'x' * 60_000, start: '2026-11-01', end: '2026-11-02'}, expected: {status: 'rejected', error: 'missing_required_column:amount_minor'}}
    configure_preparation(source: 'while True: pass', fixtures: [sample])
    id = prepare
    @owned_container = Contract.wait(timeout: 90, description: 'non-reading candidate starts') do
      container = preparation(id)['container']
      container if container && container['id'] && container['status'] == 'running'
    end
    started = Process.clock_gettime(Process::CLOCK_MONOTONIC)
    host.api('POST', "#{routine_path}/stop-preparation", operation: 'stop-blocked-input', revision: 1, preparation: id)
    completed(id, 'stopped')
    Contract.wait(timeout: 15, description: 'Stop removes the real non-reading process') do
      _, error, status = docker('inspect', @owned_container.fetch('id'))
      !status.success? && error.match?(/No such (object|container)/i)
    end
    expect(Process.clock_gettime(Process::CLOCK_MONOTONIC) - started).to be < 15
    expect(stage_calls('PreparationReview')).to be_empty
  end

  it 'rechecks immutable source after container creation and rejects tampering before body start' do
    install_create_ack_gate
    id = prepare
    Contract.wait(timeout: 90) { File.exist?(@docker_created) }
    reserved = preparation(id).fetch('container')
    @owned_container = reserved.merge('id' => File.read(@docker_created).strip)
    modify_preparation(id) { |record| record.fetch('version')['source'] += "\n# Changed after publication\n" }
    File.write(@docker_gate, '')
    row = Contract.wait(timeout: 30, description: 'tampered version rejected') do
      saved = preparation(id)
      saved if %w[failed cleanup_pending].include?(saved['status'])
    end
    expect(row.fetch('error')).to match(/hash|digest|changed|version/i)
    expect(File.exist?(@docker_started)).to be(false)
    expect(stage_calls('PreparationReview')).to be_empty
    host.restart
    Contract.wait(timeout: 30) do
      _, error, status = docker('inspect', @owned_container.fetch('id'))
      !status.success? && error.match?(/No such (object|container)/i)
    end
  end

  it 'recovers an actually created container when the host dies before its creation acknowledgement is saved' do
    install_create_ack_gate
    id = prepare
    Contract.wait(timeout: 90) { File.exist?(@docker_created) }
    reserved = preparation(id).fetch('container')
    expect(reserved.fetch('status')).to eq('reserved')
    expect(reserved['id']).to be_nil
    @owned_container = reserved.merge('id' => File.read(@docker_created).strip)
    host.crash
    File.write(@docker_gate, '')
    nonce = reserved.fetch('nonce')
    modify_preparation(id) { |record| record.fetch('container')['nonce'] = 'wrong-owner-nonce' }
    host.start
    expect(preparation(id).fetch('status')).to eq('cleanup_pending')
    _, _, preserved = docker('inspect', @owned_container.fetch('id'))
    expect(preserved.success?).to be(true)
    blocked, = host.request('POST', "#{routine_path}/prepare", operation: 'blocked-unresolved-cleanup', revision: 1, analysis: @analysis.fetch('id'))
    expect(blocked).to eq(503)
    host.stop
    modify_preparation(id) { |record| record.fetch('container')['nonce'] = nonce }
    host.start
    Contract.wait(timeout: 30) do
      _, error, status = docker('inspect', @owned_container.fetch('id'))
      !status.success? && error.match?(/No such (object|container)/i)
    end
    expect(preparation(id).fetch('status')).to eq('interrupted')
    expect(File.exist?(@docker_started)).to be(false)
    expect(stage_calls('PreparationGeneration').length).to eq(1)
  end

  it 'bounds actual output flooding and retains the runtime failure rather than accepting partial JSON' do
    configure_preparation(source: "import os\nwhile True: os.write(1, b'x' * 4096)\n")
    row = completed(prepare, 'failed')
    check = row.fetch('checks').first
    expect(check).to include('matched' => false, 'status' => 'output_limit', 'cleanupConfirmed' => true)
    expect(check.fetch('retainedBytes')).to be <= 65_536
    expect(check.fetch('output').bytesize).to be <= 65_536
    expect(stage_calls('PreparationReview')).to be_empty
  end

  it 'enforces the integrated workload memory limit on an actual allocating candidate' do
    source = "data = bytearray(160 * 1024 * 1024)\nfor i in range(0, len(data), 4096): data[i] = 1\nprint(len(data))\n"
    configure_preparation(source: source)
    row = completed(prepare, 'failed')
    check = row.fetch('checks').first
    expect(check).to include('matched' => false, 'oomKilled' => true, 'cleanupConfirmed' => true)
    expect(check.fetch('exitCode')).not_to eq(0)
    expect(stage_calls('PreparationReview')).to be_empty
  end

  it 'retains completed fixture evidence but prevents promotion after deleting the routine during review' do
    configure_preparation(review_rule: {block: true, gate: 'delete-review'})
    id = prepare
    Contract.wait(timeout: 90) { stage_calls('PreparationReview').last }
    checks = preparation(id).fetch('checks')
    expect(checks).to all(include('matched' => true))
    host.api('POST', "#{routine_path}/delete", operation: 'delete-during-review', revision: 1)
    host.release('delete-review')
    row = completed(id, 'stale')
    expect(row.fetch('checks')).to eq(checks)
    host.restart
    expect(preparation(id).fetch('status')).to eq('stale')
    expect(stage_calls('PreparationGeneration').length).to eq(1)
  end

end
