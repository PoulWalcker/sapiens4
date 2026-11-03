require 'sqlite3'
require 'yaml'
require_relative 'support/effect_peer'

RSpec.describe 'Ordinary Routine uncertainty and legacy admission evidence' do
  before do
    @host = Host.new(start: false)
    provider = File.join(__dir__, 'support/feed_code_legacy_provider.rb')
    if Gem.win_platform?
      host.environment['SAPIENS_CONTRACT_PROVIDER'] = provider
    else
      executable = File.join(host.root, 'legacy-effect-provider')
      File.write(executable, "#!#{RbConfig.ruby}\nload #{provider.inspect}\n")
      File.chmod(0755, executable)
      host.environment['SAPIENS_CODEX_BINARY'] = executable
    end
    @peer = EffectPeer.new(File.join(host.root, 'legacy-independent-peer'))
    @peer.delivery_mode = :drop_ack
    host.configure(feed_legacy_mode: 'clean', effect_url: @peer.url + '/deliveries')
    host.start
    @owner = host.state.fetch('main_agent_id')
    @fields = {title: 'Ordinary result', goal: 'Perform the requested work once and retain its result.', criteria: ['Return the retained result.']}
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", @fields.merge(operation: 'create')).fetch('routine')
  end

  after do
    @peer&.release
    begin
      if @admitted_provider_pid
        host.release
        Contract.wait(description: 'controlled orphan provider exits after host crash') do
          begin
            Process.kill(0, @admitted_provider_pid)
            false
          rescue Errno::ESRCH
            true
          end
        end
      end
      @host&.stop
    ensure
      @peer&.close
      @host&.close
    end
  end

  def path
    "/api/agents/#{@owner}/routines/#{@routine}"
  end

  def submit(operation, revision: 1)
    host.api('POST', "#{path}/run", operation: operation, revision: revision).fetch('run')
  end

  def detail(id)
    host.api('GET', "#{path}/runs/#{id}")
  end

  def settled(id)
    Contract.wait(description: 'ordinary routine occurrence settles') do
      row = detail(id)
      row if %w[done failed unresolved interrupted stopped cancelled].include?(row['status'])
    end
  end

  def mutate_saved_work(id)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      row = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', id))
      yield row
      db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(row), id])
    end
  end

  def public_work(id)
    YAML.safe_load(host.api('GET', "/api/agents/#{@owner}/tasks")).fetch('tasks').find { |row| row['workloadId'] == id }
  end

  it 'keeps an actual unregistered tool delivery blocked across restart, Retry, saved command and definition edit' do
    host.configure(feed_legacy_mode: 'tool_loss')
    id = submit('actual-tool')
    settled(id)
    expect(@peer.deliveries.length).to eq(1)
    expect(@peer.deliveries.first.dig('request', 'content')).to eq('text' => 'Report 42')
    attempt = public_work(id).fetch('decisions').find { |record| record['node'] == 'Execution' }.fetch('routineAttempt')
    expect(attempt).to include('schema' => 1, 'state' => 'admitted', 'policy' => 'unsafe_bypass')
    calls = host.calls.length
    host.restart
    expect(host.request('POST', "/api/agents/#{@owner}/turns/#{id}/retry", {}).first).to eq(409)
    expect(submit('actual-tool')).to eq(id)
    file = File.join(host.root, 'agentpy/agents', @owner, 'state.json')
    saved = JSON.parse(File.read(file))
    turn = saved.fetch('turns').find { |row| row['id'] == id }
    turn['managed_command'] = {'id' => 'file-unknown-retry', 'action' => 'retry', 'revision' => turn.fetch('managed').fetch('revision')}
    Contract.atomic_json(file, saved)
    response, = host.request('GET', '/api/state')
    expect([200, 409]).to include(response)
    host.idle
    expect(host.calls.length).to eq(calls)
    host.api('POST', "#{path}/edit", @fields.merge(operation: 'edit-unknown', revision: 1, goal: 'Perform the changed work once and retain its result.'))
    expect(host.request('POST', "#{path}/run", operation: 'new-key-after-edit', revision: 2).first).to eq(409)
    expect(@peer.deliveries.length).to eq(1)
    host.configure(feed_legacy_mode: 'clean')
    unrelated = host.api('POST', "/api/agents/#{@owner}/routines", @fields.merge(operation: 'unrelated')).fetch('routine')
    other = host.api('POST', "/api/agents/#{@owner}/routines/#{unrelated}/run", operation: 'other-run', revision: 1).fetch('run')
    host.turn(other)
    expect(host.calls.count { |call| call["agency_run"] == id }).to eq(calls)
    expect(@peer.deliveries.length).to eq(1)
  end

  it 'retains admitted uncertainty after real host death before the provider emits any event' do
    host.configure(feed_legacy_mode: 'admitted_block')
    id = submit('admitted-no-output')
    admitted_call = Contract.wait { host.calls.find { |call| call['agency_run'] == id && call['node'] == 'Execution' } }
    @admitted_provider_pid = admitted_call.fetch('pid')
    attempt = public_work(id).fetch('decisions').find { |record| record['node'] == 'Execution' }.fetch('routineAttempt')
    expect(attempt).to include('schema' => 1, 'state' => 'admitted', 'policy' => 'unsafe_bypass')
    host.crash
    host.start
    expect(host.request('POST', "/api/agents/#{@owner}/turns/#{id}/retry", {}).first).to eq(409)
    expect(host.request('POST', "#{path}/run", operation: 'new-after-admission', revision: 1).first).to eq(409)
    expect(@peer.deliveries).to be_empty
    expect(host.calls.count { |call| call['node'] == 'Execution' }).to eq(1)

  end

  it 'allows explicit criteria-failure Retry only with a complete clean no-tool invocation' do
    host.configure(feed_legacy_mode: 'clean_rejected')
    id = submit('clean-rejection')
    expect(settled(id).fetch('assessment')).to include('status' => 'Rejected')
    expect(public_work(id).fetch('decisions').find { |record| record['node'] == 'Execution' }.fetch('routineAttempt')).to include('schema' => 1, 'state' => 'admitted', 'policy' => 'unsafe_bypass')
    evidence = detail(id).fetch('invocations').first.fetch('evidence')
    expect(evidence.fetch('completeness')).to include('finalSeen' => true, 'turnEndSeen' => true, 'streamEndSeen' => true, 'pendingTools' => 0)
    host.configure(feed_legacy_mode: 'clean')
    host.api('POST', "/api/agents/#{@owner}/turns/#{id}/retry")
    Contract.wait { row = detail(id); row if row.fetch('invocations').length == 2 && row.dig('outcome', 'status') == 'Completed' }
    expect(@peer.deliveries).to be_empty
  end

  it 'contrasts a freshly admitted never-started prompt failure with an explicitly saved legacy failure lacking start provenance' do
    directory = File.join(host.root, 'decision-prompts')
    FileUtils.mkdir_p(directory)
    override = File.join(directory, 'decision-execution.md')
    File.binwrite(override, [255].pack('C'))
    fresh = submit('fresh-prompt-failure')
    settled(fresh)
    expect(host.calls).to be_empty
    expect(public_work(fresh).fetch('decisions', [])).to be_empty
    File.delete(override)
    host.api('POST', "/api/agents/#{@owner}/turns/#{fresh}/retry")
    Contract.wait { detail(fresh).dig('outcome', 'status') == 'Completed' }
    legacy_routine = host.api('POST', "/api/agents/#{@owner}/routines", @fields.merge(operation: 'legacy-fixture')).fetch('routine')
    @routine = legacy_routine
    File.binwrite(override, [255].pack('C'))
    legacy = submit('legacy-prompt-failure')
    settled(legacy)
    host.stop
    # Explicit saved-installation legacy shape: absence is not proof of never-started work.
    mutate_saved_work(legacy) { |row| row.delete('routineAttemptSchema') }
    File.delete(override)
    host.start
    expect(host.request('POST', "/api/agents/#{@owner}/turns/#{legacy}/retry", {}).first).to eq(409)
    next_run = submit('new-authorized-not-global-poison')
    expect(settled(next_run).dig('outcome', 'status')).to eq('Completed')
    expect(@peer.deliveries).to be_empty
  end

  it 'preserves historical Completed work without old evidence instead of retroactively poisoning its routine' do
    id = submit('historical-completed')
    expect(settled(id).dig('outcome', 'status')).to eq('Completed')
    host.stop
    mutate_saved_work(id) do |row|
      row.delete('routineAttemptSchema')
      row.fetch('decisions').each { |record| record.delete('routineAttempt'); record.delete('evidence') }
    end
    host.start
    expect(detail(id).dig('outcome', 'status')).to eq('Completed')
    expect(settled(submit('new-after-history')).dig('outcome', 'status')).to eq('Completed')
    expect(@peer.deliveries).to be_empty
  end

  it 'resolves an origin-only legacy consumer from its root owner when a real delegated child produced an unknown tool effect' do
    worker = host.agent('DelegatedEffectWorker')
    host.configure(feed_legacy_mode: 'tool_loss', delegate_target: worker)
    id = submit('delegated-effect')
    settled(id)
    expect(@peer.deliveries.length).to eq(1)
    work = public_work(id)
    expect(work.fetch('calls').map { |call| call['owner'] || call['addressedTo'] }).to include(worker)
    host.stop
    mutate_saved_work(id) do |row|
      row.delete('routineSnapshot')
      row.delete('routineAttemptSchema')
      row.fetch('decisions').each { |record| record.delete('routineAttempt') }
    end
    host.start
    response, failure = host.request('POST', "#{path}/run", operation: 'origin-only-new-key', revision: 1)
    expect(response).to eq(409)
    expect(failure.fetch('error')).to match(/unknown|uncertain|external/i)
    expect(@peer.deliveries.length).to eq(1)
  end
end
