require 'socket'

RSpec.describe 'Ordinary routine evidence through public boundaries' do
  before do |example|
    options = {start: false}
    if example.metadata[:previous_release]
      previous = ENV['SAPIENS_TEST_PRE_EVIDENCE_HOST']
      skip 'Set SAPIENS_TEST_PRE_EVIDENCE_HOST to exercise an actual pre-evidence installation upgrade' unless previous
      @binary_pointer = File.join(Dir.mktmpdir('sapiens-evidence-upgrade-'), 'host-target')
      File.write(@binary_pointer, previous)
      @legacy_host = true
      options[:command] = [RbConfig.ruby, '-e', 'exec File.read(ENV.fetch("SAPIENS_EVIDENCE_HOST_TARGET")).strip, *ARGV', '--']
      options[:environment] = {'SAPIENS_EVIDENCE_HOST_TARGET' => @binary_pointer}
    end
    @host = Host.new(**options)
    provider = File.join(__dir__, 'support/evidence_provider.rb')
    if Gem.win_platform?
      host.environment['SAPIENS_CONTRACT_PROVIDER'] = provider
    else
      executable = File.join(host.root, 'evidence-provider')
      File.write(executable, "#!#{RbConfig.ruby}\nload #{provider.inspect}\n")
      File.chmod(0755, executable)
      host.environment['SAPIENS_CODEX_BINARY'] = executable
    end
    host.start
    @owner = host.agent
    @definition = {title: 'Evidence fixture', goal: 'Produce a comparison.', criteria: ['Return a comparison.', 'Include sources.']}
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", @definition.merge(operation: 'create-evidence')).fetch('routine')
  end

  after do
    @host.close if @host
    FileUtils.remove_entry(File.dirname(@binary_pointer)) if @binary_pointer && File.exist?(@binary_pointer)
  end

  def command(action, operation, **fields)
    host.api('POST', "/api/agents/#{@owner}/routines/#{@routine}/#{action}", {operation: operation, revision: 1}.merge(fields))
  end

  def row(identity)
    return summary(identity) if @legacy_host
    host.api('GET', detail_path(identity))
  end

  def detail_path(identity, owner = @owner, routine = @routine)
    "/api/agents/#{owner}/routines/#{routine}/runs/#{identity}"
  end

  def summary(identity)
    host.state.fetch('personal_routines').find { |routine| routine['id'] == @routine }.fetch('runs').find { |run| run['id'] == identity }
  end

  def settle(identity)
    Contract.wait(description: "evidence occurrence #{identity} settles") do
      current = summary(identity)
      row(identity) if current && %w[done failed unresolved interrupted stopped cancelled].include?(current['status'])
    end
  end

  def run_mode(mode = 'normal', operation: mode, **fields)
    host.configure(evidence_mode: mode)
    identity = command('run', "run-#{operation}", inputs: 'Synthetic input', **fields).fetch('run')
    [identity, settle(identity)]
  end

  def invocation(run)
    run.fetch('invocations').find { |entry| entry['node'] == 'Execution' }.fetch('evidence')
  end

  def retried(identity)
    Contract.wait(description: "second invocation of #{identity} finishes") do
      current = row(identity)
      calls = current.fetch('invocations')
      current if calls.length == 2 && calls.last.fetch('evidence')['finished'] &&
        %w[done failed unresolved interrupted stopped cancelled].include?(current['status'])
    end
  end

  def expect_unknown_usage(evidence)
    expect(evidence.fetch('usage')).to include('inputTokens' => nil, 'outputTokens' => nil, 'price' => nil)
  end

  it 'S1 retains the admitted contract, model assessment, receipt provenance and usage after restart' do
    identity, current = run_mode
    expect(current.fetch('assessment')).to include('status' => 'Accepted', 'source' => 'model_contract_assessment', 'independent' => false)
    expect(current.fetch('snapshot')).to include('revision' => 1, 'inputs' => 'Synthetic input', 'goal' => @definition[:goal], 'criteria' => @definition[:criteria])
    expect(current.fetch('observations')).to include('source' => 'model_statement', 'independent' => false)
    calls = current.fetch('invocations')
    expect(calls.map { |call| call.fetch('decisionId') }.uniq.length).to eq(calls.length)
    expect(calls.map { |call| call.fetch('node') }).to include('Execution')
    evidence = invocation(current)
    expect(evidence).to include('provider' => 'codex', 'model' => 'gpt-6-sol', 'processExit' => 0, 'finished' => true)
    expect(evidence.fetch('completeness')['externalEffectsVerified']).to eq(false)
    expect(evidence.fetch('durationMs')).to be >= 0
    expect(evidence.fetch('usage')).to include('inputTokens' => 101, 'cachedInputTokens' => 7, 'outputTokens' => 13, 'price' => nil, 'source' => 'provider_report')
    expect(evidence.fetch('events')).not_to be_empty
    expect(evidence.fetch('events')).to all(include('source' => 'provider_receipt'))
    expect(evidence.fetch('events').map { |event| event.fetch('sequence') }).to eq(evidence.fetch('events').map { |event| event.fetch('sequence') }.sort.uniq)
    host.restart
    expect(row(identity)).to eq(current)
  end

  it 'S2 distinguishes rejected criteria, unresolved claims and malformed final from accepted transport success' do
    accepted_id, accepted = run_mode
    _, rejected = run_mode('clean_invalid_completed')
    expect(rejected.fetch('assessment')).to include('status' => 'Rejected', 'source' => 'application_validation')
    expect(rejected.fetch('assessment').fetch('reason')).not_to be_empty
    _, unresolved = run_mode('clean_unresolved')
    expect(unresolved.fetch('assessment').fetch('status')).to eq('Unresolved')
    _, malformed = run_mode('clean_malformed_final')
    expect(malformed.fetch('assessment').fetch('status')).to eq('Unresolved')
    expect(invocation(rejected).fetch('processExit')).to eq(0)
    expect(row(accepted_id)).to eq(accepted)
  end

  it 'S2 keeps an unresolved reply and its exact unmet criteria readable in detail' do
    identity, current = run_mode('unresolved')
    expect(current.fetch('result')).to eq('Fixture result')
    expect(current.fetch('outcome')).to include('status' => 'Unresolved', 'satisfiedCriteria' => [])
    criteria = current.fetch('criteriaAssessment')
    expect(criteria.map { |item| item.fetch('criterion') }).to eq(@definition[:criteria])
    expect(criteria).to all(include('status' => 'Unresolved', 'source' => 'model_statement'))
    expect(current.fetch('assessment')['status']).to eq('Unresolved')
    host.restart
    expect(row(identity)).to eq(current)
  end

  %w[loss failure_after_final].each do |mode|
    it "S3 preserves uncertainty and avoids replay after #{mode}" do
      identity, current = run_mode(mode)
      expect(current.fetch('assessment').fetch('status')).to eq('Unresolved')
      evidence = invocation(current)
      expect_unknown_usage(evidence)
      expect(evidence.fetch('completeness')).to include('turnEndSeen' => false, 'finalSeen' => mode != 'loss')
      expect(evidence.fetch('completeness').fetch('externalEffectsVerified')).to eq(false)
      expect(File.readlines(File.join(host.root, 'external-effect.txt'))).to eq(["effect\n"])
      count = host.calls.length
      host.restart
      expect(row(identity)).to eq(current)
      expect(host.calls.length).to eq(count)
      expect(File.readlines(File.join(host.root, 'external-effect.txt'))).to eq(["effect\n"])
    end
  end

  it 'S4 counts duplicate usage once, marks conflicting receipts and preserves absent counters' do
    _, duplicate = run_mode('duplicate_usage')
    expect(invocation(duplicate).fetch('usage')).to include('inputTokens' => 101, 'outputTokens' => 13, 'duplicates' => 1, 'conflict' => false, 'price' => nil)
    _, conflicting = run_mode('conflicting_usage')
    expect(invocation(conflicting).fetch('usage')).to include('conflict' => true, 'price' => nil)
    expect(invocation(conflicting).fetch('usage')['inputTokens']).not_to eq(1201)
    _, missing = run_mode('missing_usage')
    expect_unknown_usage(invocation(missing))
  end

  it 'S4 preserves known token counters when cached usage is absent' do
    _, current = run_mode('missing_cached')
    expect(invocation(current).fetch('usage')).to include('inputTokens' => 101, 'outputTokens' => 13, 'cachedInputTokens' => nil, 'price' => nil)
  end

  it 'S4 preserves separate invocation identities and partial usage across an explicit retry' do
    identity, rejected = run_mode('clean_invalid_completed')
    first = rejected.fetch('invocations').last
    expect(first.fetch('evidence').fetch('usage')['inputTokens']).to eq(101)
    host.configure(evidence_mode: 'clean_missing_usage')
    host.api('POST', "/api/agents/#{@owner}/turns/#{identity}/retry")
    current = retried(identity)
    expect(current.fetch('assessment')['status']).to eq('Accepted')
    calls = current.fetch('invocations')
    expect(calls.length).to eq(2)
    expect(calls.first).to eq(first)
    expect(calls.map { |call| call.fetch('decisionId') }.uniq.length).to eq(2)
    expect(calls.map { |call| call.fetch('callId') }.uniq).to eq([identity])
    expect_unknown_usage(calls.last.fetch('evidence'))
    host.restart
    expect(row(identity)['invocations']).to eq(calls)
  end

  it 'S4 records malformed and overflowing counters without losing valid usage or hanging' do
    _, first = run_mode('invalid_usage_first')
    expect(invocation(first).fetch('usage')).to include('inputTokens' => 101, 'cachedInputTokens' => 7, 'outputTokens' => 13, 'invalidReports' => 1, 'price' => nil)
    _, after = run_mode('invalid_usage_after')
    expect(invocation(after).fetch('usage')).to include('inputTokens' => 101, 'cachedInputTokens' => 7, 'outputTokens' => 13, 'invalidReports' => 1, 'conflict' => true, 'price' => nil)
    _, only = run_mode('invalid_usage_only')
    expect_unknown_usage(invocation(only))
    expect(invocation(only).fetch('usage')['invalidReports']).to eq(1)
  end

  it 'S2 does not reuse an earlier rejected assessment after an explicit retry loses its final response' do
    identity, rejected = run_mode('clean_invalid_completed')
    expect(rejected.fetch('assessment')['status']).to eq('Rejected')
    host.configure(evidence_mode: 'loss')
    host.api('POST', "/api/agents/#{@owner}/turns/#{identity}/retry")
    current = retried(identity)
    expect(current.fetch('assessment')['status']).to eq('Unresolved')
    expect(current.fetch('invocations').length).to eq(2)
    expect(current.fetch('invocations').first).to eq(rejected.fetch('invocations').first)
    expect(host.request('POST', "/api/agents/#{@owner}/turns/#{identity}/retry", {}).first).to eq(409)
    expect(File.readlines(File.join(host.root, 'external-effect.txt'))).to eq(["effect\n"])
  end

  it 'S5 omits tool and stderr credentials from durable evidence and public failure output' do
    identity, current = run_mode('secret_failure')
    canary = 'sk-fixture-private-canary-1234567890'
    expect(JSON.generate(host.state)).not_to include(canary)
    expect(invocation(current).fetch('completeness')['contentOmitted']).to eq(true)
    host.restart
    expect(JSON.generate(row(identity))).not_to include(canary)
    files = Dir[File.join(host.root, '**', '*')].select { |path| File.file?(path) }
    leaks = files.select { |path| File.binread(path).include?(canary) }
    expect(leaks).to be_empty
  end

  it 'S1 retains bounded semantic provider receipts and model artifact references in isolated detail' do
    identity, current = run_mode('semantic')
    evidence = invocation(current)
    payload = JSON.generate(evidence.fetch('events'))
    ['synthetic observation', 'Fixture output', 'fixture-documents', 'read_document', 'comparison.txt',
     'Document has two sources.', 'synthetic comparison sources', 'Long-safe-output-'].each do |fact|
      expect(payload).to include(fact)
    end
    expect(payload).not_to include('private-fixture-field-value')
    fields = evidence.fetch('events').flat_map { |event| event['summary'] ? event.fetch('summary').fetch('fields') : [] }
    expect(fields).to include(include('name' => 'output', 'truncated' => true))
    expect(evidence.fetch('completeness')['truncatedFields']).to be > 0
    expect(evidence.fetch('completeness')['redactedFields']).to be > 0
    artifact = current.fetch('outcome').fetch('artifacts').fetch(0)
    expect(File.read(artifact)).to eq('Comparison with two synthetic sources.')
    expect(current.fetch('observations')).to include('source' => 'model_statement', 'independent' => false)
    brief = summary(identity)
    %w[invocations observations criteriaAssessment feedback outcome].each { |key| expect(brief).not_to have_key(key) }
    expect(brief).to include('invocationCount' => current.fetch('invocations').length)
    expect(brief.fetch('assessment')['status']).to eq('Accepted')
    stranger = host.agent('DetailStranger')
    expect(host.request('GET', detail_path(identity, stranger)).first).to eq(404)
    host.restart
    expect(row(identity)).to eq(current)
    files = Dir[File.join(host.root, '**', '*')].select { |path| File.file?(path) }
    expect(files.select { |path| File.binread(path).include?('private-fixture-field-value') }).to be_empty
  end

  it 'S6 bounds retained receipts while reporting malformed, unknown, oversized and unfinished events' do
    _, current = run_mode('bounded')
    expect(current.fetch('assessment')['status']).to eq('Unresolved')
    evidence = invocation(current)
    expect(evidence.fetch('events').length).to be <= 256
    expect(JSON.generate(evidence.fetch('events')).bytesize).to be <= 128 * 1024
    completeness = evidence.fetch('completeness')
    expect(completeness.fetch('malformedEvents')).to be >= 1
    expect(completeness.fetch('unknownEvents')).to be >= 1
    expect(completeness.fetch('outOfOrderEvents')).to be >= 1
    expect(completeness.fetch('droppedEvents')).to be > 0
    expect(completeness.fetch('droppedBytes')).to be > 0
    # Overflow can make the exact number unknowable; it must not claim no pending work.
    expect(completeness.fetch('pendingTools')).not_to eq(0)
    expect(completeness).to include('finalSeen' => true, 'turnEndSeen' => true, 'streamEndSeen' => true, 'contentOmitted' => true)
    expect(evidence.fetch('usage')['inputTokens']).to eq(101)
  end

  it 'S7 keeps a late completed assessment on its admitted revision' do
    host.configure(evidence_mode: 'blocked')
    identity = command('run', 'late-run', inputs: 'Old input').fetch('run')
    Contract.wait { File.exist?(File.join(host.root, 'provider-events-emitted')) }
    command('edit', 'new-goal', **@definition.merge(goal: 'Produce a different result.', criteria: ['Return a different result.']))
    host.release
    current = settle(identity)
    expect(current).to include('historical' => true, 'currentRevision' => 2)
    expect(current.fetch('snapshot')).to include('revision' => 1, 'goal' => @definition[:goal], 'criteria' => @definition[:criteria])
    expect(current.fetch('assessment')['status']).to eq('Accepted')
    newer, result = run_mode('normal', operation: 'new-revision', revision: 2)
    expect(newer).not_to eq(identity)
    expect(result).to include('historical' => false, 'currentRevision' => 2)
    expect(result.fetch('snapshot')['revision']).to eq(2)
    expect(row(identity)).to eq(current)
  end

  it 'S7 does not promote a stopped occurrence to accepted after delayed provider completion' do
    host.configure(evidence_mode: 'blocked')
    identity = command('run', 'stopped-run').fetch('run')
    Contract.wait { File.exist?(File.join(host.root, 'provider-events-emitted')) }
    command('stop', 'stop-run', run: identity)
    host.release
    host.idle
    current = settle(identity)
    expect(current).to include('status' => 'stopped', 'result' => nil)
    expect(current.fetch('assessment')['status']).to eq('Unresolved')
    host.restart
    expect(row(identity).fetch('assessment')['status']).to eq('Unresolved')
  end

  it 'S6 keeps public reads and Stop responsive while provider events continuously arrive' do
    host.configure(evidence_mode: 'flood')
    identity = command('run', 'flood-run').fetch('run')
    Contract.wait(description: 'a continuous provider flood reaches a public checkpoint') do
      detail = row(identity)
      entry = detail.fetch('invocations').find { |call| call['node'] == 'Execution' }
      entry && entry.fetch('evidence').fetch('completeness').fetch('observedEvents') >= 50
    end
    # Each HTTP read uses the ordinary ten-second request timeout while the writer remains live.
    3.times do
      expect(summary(identity).fetch('status')).to eq('running')
      expect(row(identity).fetch('invocations')).not_to be_empty
    end
    command('stop', 'stop-flood', run: identity)
    host.idle
    current = settle(identity)
    expect(current.fetch('status')).to eq('stopped')
    expect(current.fetch('assessment')['status']).to eq('Unresolved')
    expect(invocation(current).fetch('events').length).to be <= 256
    expect(host.calls.length).to eq(1)
  end

  it 'S8 preserves original acceptance and idempotent negative feedback on the historical revision' do
    identity, current = run_mode
    command('edit', 'edit-before-feedback', **@definition.merge(goal: 'Revised goal.'))
    receipt = command('reject', 'feedback-one', run: identity, reason: 'The comparison omits an important source.')
    expect(command('reject', 'feedback-one', run: identity, reason: 'The comparison omits an important source.')).to eq(receipt)
    path = "/api/agents/#{@owner}/routines/#{@routine}/reject"
    expect(host.request('POST', path, operation: 'feedback-one', revision: 1, run: identity, reason: 'Different correction.').first).to eq(409)
    host.restart
    updated = row(identity)
    expect(updated.fetch('assessment')).to include('status' => 'Rejected', 'source' => 'user_feedback', 'independent' => false)
    expect(updated.fetch('originalAssessment')).to eq(current.fetch('assessment'))
    expect(updated.fetch('feedback').length).to eq(1)
    expect(updated.fetch('feedback').first).to include('operation' => 'feedback-one', 'revision' => 1, 'run' => identity, 'reason' => 'The comparison omits an important source.')
    other = host.api('POST', "/api/agents/#{@owner}/routines", @definition.merge(operation: 'create-other')).fetch('routine')
    expect(host.request('POST', "/api/agents/#{@owner}/routines/#{other}/reject", operation: 'wrong-routine-feedback', revision: 1, run: identity, reason: 'Wrong routine.').first).to be_between(400, 499)
    stranger = host.agent('OtherOwner')
    expect(host.request('POST', "/api/agents/#{stranger}/routines/#{@routine}/reject", operation: 'foreign-feedback', revision: 1, run: identity, reason: 'Foreign correction.').first).to be_between(400, 499)
    expect(row(identity)['feedback']).to eq(updated['feedback'])
    command('delete', 'delete-after-feedback', revision: 2)
    expect(command('reject', 'feedback-one', run: identity, reason: 'The comparison omits an important source.')).to eq(receipt)
    expect(row(identity)['feedback']).to eq(updated['feedback'])
  end

  it 'S8 retains the first 4000-character rejection cause and audits later feedback without duplicate rejection activity' do
    identity, original = run_mode
    reason = 'R' * 4000
    command('reject', 'first-cause', run: identity, reason: reason)
    path = "/api/agents/#{@owner}/routines/#{@routine}/reject"
    expect(host.request('POST', path, operation: 'too-long-cause', revision: 1, run: identity, reason: 'R' * 4001).first).to eq(400)
    second = command('reject', 'later-note', run: identity, reason: 'Additional source correction.')
    expect(command('reject', 'later-note', run: identity, reason: 'Additional source correction.')).to eq(second)
    current = row(identity)
    expect(current.fetch('assessment')).to include('status' => 'Rejected', 'reason' => reason)
    expect(current.fetch('rejection')).to include('operation' => 'first-cause', 'reason' => reason)
    expect(current.fetch('feedback').map { |entry| entry.fetch('operation') }).to eq(%w[first-cause later-note])
    expect(current.fetch('originalAssessment')).to eq(original.fetch('assessment'))
    expect(current.fetch('result')).to eq(original.fetch('result'))
    expect(summary(identity).fetch('assessment')['status']).to eq('Rejected')
    events = host.state.fetch('events').fetch(@owner).select { |event| event['event'] == 'task.result.rejected' }
    expect(events.length).to eq(1)
    host.restart
    expect(row(identity)).to eq(current)
    expect(host.state.fetch('events').fetch(@owner).count { |event| event['event'] == 'task.result.rejected' }).to eq(1)
  end

  it 'S8 replays negative feedback after its HTTP acknowledgement is actually discarded' do
    identity, = run_mode
    path = "/api/agents/#{@owner}/routines/#{@routine}/reject"
    body = {operation: 'lost-feedback-ack', revision: 1, run: identity, reason: 'The delivered result was incorrect.'}
    address = URI(host.url)
    socket = TCPSocket.new(address.host, address.port)
    payload = JSON.generate(body)
    socket.write("POST #{path} HTTP/1.1\r\nHost: #{address.host}:#{address.port}\r\nContent-Type: application/json\r\nX-Sapiens-Local: 1\r\nContent-Length: #{payload.bytesize}\r\nConnection: close\r\n\r\n#{payload}")
    Contract.wait { row(identity).fetch('feedback').length == 1 }
    # Confirm admission independently, then discard every response byte.
    socket.setsockopt(Socket::SOL_SOCKET, Socket::SO_LINGER, [1, 0].pack('ii'))
    socket.close
    count = host.calls.length
    host.restart
    receipt = host.api('POST', path, body)
    expect(host.api('POST', path, body)).to eq(receipt)
    expect(row(identity).fetch('feedback')).to contain_exactly(include('operation' => 'lost-feedback-ack'))
    expect(host.calls.length).to eq(count)
    expect(host.state.fetch('events').fetch(@owner).count { |event| event['event'] == 'task.result.rejected' }).to eq(1)
  ensure
    socket.close if socket && !socket.closed?
  end

  it 'S9 retains an incomplete invocation after host crash without replaying its admitted run' do
    host.configure(evidence_mode: 'crash')
    identity = command('run', 'crash-run').fetch('run')
    Contract.wait { File.exist?(File.join(host.root, 'provider-events-emitted')) }
    Contract.wait do
      entry = row(identity).fetch('invocations').find { |call| call['node'] == 'Execution' }
      entry && !entry.fetch('evidence').fetch('events').empty?
    end
    host.crash
    host.release
    host.start
    current = settle(identity)
    count = host.calls.length
    expect(current.fetch('assessment')['status']).to eq('Unresolved')
    expect(invocation(current)['finished']).to eq(false)
    expect(invocation(current).fetch('completeness')).to include('tailUnknown' => true, 'streamEndSeen' => false)
    expect(invocation(current).fetch('events')).not_to be_empty
    expect_unknown_usage(invocation(current))
    expect(command('run', 'crash-run')['run']).to eq(identity)
    expect(host.calls.length).to eq(count)
    expect(row(identity)['invocations']).to eq(current['invocations'])
  end

  it 'S9 preserves a previous-release result without inventing evidence or zero usage', :previous_release do
    identity, before = run_mode
    expect(before.fetch('status')).to eq('done')
    expect(before).not_to have_key('invocations')
    host.stop
    File.write(@binary_pointer, Contract::BINARY)
    @legacy_host = false
    host.start
    current = row(identity)
    expect(current).to include('result' => before.fetch('result'), 'snapshot' => before.fetch('snapshot'))
    expect(current.fetch('invocations')).to eq([])
    expect(current.fetch('assessment')).to include('status' => 'Unresolved', 'reason' => 'No recorded contract assessment')
    host.restart
    expect(row(identity)).to eq(current)
  end
end
