require 'socket'

RSpec.describe 'Owner analysis of accepted routine results' do
  before do |example|
    options = {start: false}
    if example.metadata[:previous_release]
      previous = ENV['SAPIENS_TEST_PRE_ANALYSIS_HOST']
      skip 'Set SAPIENS_TEST_PRE_ANALYSIS_HOST to verify an actual pre-analysis upgrade' unless previous
      @binary_directory = Dir.mktmpdir('sapiens-analysis-upgrade-')
      @binary_pointer = File.join(@binary_directory, 'host-target')
      File.write(@binary_pointer, previous)
      options[:command] = [RbConfig.ruby, '-e', 'exec File.read(ENV.fetch("SAPIENS_ANALYSIS_HOST_TARGET")).strip, *ARGV', '--']
      options[:environment] = {'SAPIENS_ANALYSIS_HOST_TARGET' => @binary_pointer}
    end
    @host = Host.new(**options)
    host.configure(version: 'codex-cli 0.162.1')
    host.start
    @owner = host.agent('AnalysisOwner')
    @fields = {title: 'Comparison', goal: 'Compare the products.', criteria: ['Return a sourced comparison.']}
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", @fields.merge(operation: 'create-analysis')).fetch('routine')
  end

  after do
    @host.close if @host
    FileUtils.remove_entry(@binary_directory) if @binary_directory && File.directory?(@binary_directory)
  end

  def path(owner = @owner)
    "/api/agents/#{owner}/routines/#{@routine}"
  end

  def command(action, operation, **fields)
    host.api('POST', "#{path}/#{action}", {operation: operation, revision: 1}.merge(fields))
  end

  def routine
    host.state.fetch('personal_routines').find { |row| row['id'] == @routine }
  end

  def analysis
    routine['analysis']
  end

  def analysis_calls
    host.calls.select { |call| call['node'] == 'AutomationAnalysis' }
  end

  def configured_analysis(status = 'Ready', **rule)
    answer = case status
    when 'Ready'
      {status: status, automatablePart: 'Compare the supplied product records.', parameters: [{name: 'products', description: 'Current records'}]}
    when 'NeedMoreEvidence'
      {status: status, missingObservations: ['An example with an unavailable source.']}
    when 'Decline'
      {status: status, reconsiderCondition: 'A stable source format becomes available.'}
    end
    host.configure(rules: {'AutomationAnalysis' => {response: decision('AutomationAnalyzed', decision: answer), usage: {input_tokens: 71, cached_input_tokens: 7, output_tokens: 19}}.merge(rule)})
  end

  def run(operation = 'business', **fields)
    id = command('run', operation, **fields).fetch('run')
    Contract.wait(description: "business run #{id} completes") do
      row = routine.fetch('runs').find { |entry| entry['id'] == id }
      row if row && %w[done failed unresolved stopped interrupted cancelled].include?(row['status'])
    end
    id
  end

  def settled_analysis(status = 'Ready')
    Contract.wait(description: "analysis #{status}; calls #{analysis_calls.length}; current #{analysis.inspect}") do
      current = analysis
      current if current && current['status'] == status
    end
  end

  def started_analysis
    Contract.wait(description: 'owner analysis provider starts') { analysis_calls.last }
    Contract.wait(description: 'analysis identity published') { analysis if analysis && analysis['id'] }
  end

  def detail(id)
    host.api('GET', "#{path}/analysis/#{id}")
  end

  %w[Ready NeedMoreEvidence Decline].each do |status|
    it "S1 saves #{status} after the first accepted business run without creating more business work" do
      configured_analysis(status)
      business = run
      saved = settled_analysis(status)
      expect(analysis_calls.length).to eq(1)
      expect(saved['reason']).not_to be_empty
      expect(saved['sourceRuns']).to include(business)
      expect(routine.fetch('runs').map { |row| row['id'] }).to eq([business])
      case status
      when 'Ready'
        expect(saved['automatablePart']).to include('product records')
        expect(saved['parameters']).to include(include('name' => 'products'))
      when 'NeedMoreEvidence'
        expect(saved['missingObservations']).to include('An example with an unavailable source.')
      when 'Decline'
        expect(saved['reconsiderCondition']).to include('stable source format')
      end
      captured = detail(saved.fetch('id'))
      expect(captured.fetch('invocations').last.fetch('evidence')).to include('policy' => 'response_only', 'providerVersion' => 'codex-cli 0.162.1')
      3.times { host.state; detail(saved.fetch('id')) }
      host.restart
      expect(analysis).to eq(saved)
      expect(detail(saved.fetch('id'))).to eq(captured)
      expect(analysis_calls.length).to eq(1)
      expect(routine.fetch('runs').map { |row| row['id'] }).to eq([business])
    end
  end

  it 'S2 waits for accepted evidence and retains unsuccessful history as unsuccessful' do
    configured_analysis
    rules = host.provider.fetch('rules').merge('Execution' => {response: decision('Outcome', reply: 'Still missing sources', outcome: {status: 'Unresolved', satisfiedCriteria: [], artifacts: []}), usage: {input_tokens: 41, cached_input_tokens: 0, output_tokens: 17}})
    host.configure(rules: rules)
    failed = run('unresolved-source')
    host.restart
    expect(analysis_calls).to be_empty
    configured_analysis
    accepted = run('accepted-source')
    settled_analysis
    context = JSON.parse(analysis_calls.last.fetch('prompt').split("\nContext:\n", 2).last).fetch('analysisEvidence')
    expect(JSON.generate(context.fetch('positiveRuns'))).to include(accepted)
    expect(JSON.generate(context.fetch('positiveRuns'))).not_to include(failed)
    expect(JSON.generate(context.fetch('history'))).to include(failed)
  end

  it 'S3 replays the original command after success and restart without another analysis' do
    configured_analysis('Decline')
    business = run
    saved = settled_analysis('Decline')
    host.restart
    expect(command('run', 'business').fetch('run')).to eq(business)
    host.idle
    expect(analysis.fetch('id')).to eq(saved.fetch('id'))
    expect(analysis_calls.length).to eq(1)
    expect(routine.fetch('runs').length).to eq(1)
  end

  it 'S4 admits only one concurrent analysis when explicit reconsider commands arrive together' do
    configured_analysis
    run
    settled_analysis
    configured_analysis('Ready', block: true, gate: 'analysis-race')
    request = {operation: 'concurrent-reconsider', revision: 1}
    replies = 2.times.map { Thread.new { host.request('POST', "#{path}/analyze", request) } }.map(&:value)
    expect(replies.map(&:first)).to eq([200, 200])
    expect(replies.map { |reply| reply[1] }.uniq.length).to eq(1)
    Contract.wait { analysis_calls.length == 2 }
    host.release('analysis-race')
    settled_analysis
    host.restart
    expect(analysis_calls.length).to eq(2)
    expect(routine.fetch('runs').length).to eq(1)
  end

  %w[stop delete goal_roundtrip].each do |mutation|
    it "S5 prevents late readiness after #{mutation}" do
      configured_analysis('Ready', block: true, gate: 'late-analysis')
      business = run
      active = started_analysis
      case mutation
      when 'stop'
        command('stop-analysis', 'stop-analysis', analysis: active.fetch('id'))
      when 'delete'
        command('delete', 'delete-routine')
      when 'goal_roundtrip'
        command('edit', 'change-goal', **@fields.merge(goal: 'Compare different products.'))
        command('edit', 'restore-text', revision: 2, **@fields)
      end
      host.release('late-analysis')
      host.idle
      host.restart
      expect(analysis && analysis['status']).not_to eq('Ready')
      expect(analysis_calls.length).to eq(1)
      expect(routine.fetch('runs').map { |row| row['id'] }).to eq([business])
      expect(routine['deleted']).to eq(true) if mutation == 'delete'
    end
  end

  it 'S5 preserves analysis eligibility across a title-only edit' do
    configured_analysis('Ready', block: true, gate: 'renamed-analysis')
    run
    active = started_analysis
    command('edit', 'rename', **@fields.merge(title: 'Renamed comparison'))
    host.release('renamed-analysis')
    expect(settled_analysis.fetch('id')).to eq(active.fetch('id'))
    expect(analysis_calls.length).to eq(1)
  end

  it 'S6 reconsiders negative feedback after NeedMoreEvidence without retaining rejected positive sources' do
    configured_analysis('NeedMoreEvidence')
    business = run
    saved = settled_analysis('NeedMoreEvidence')
    configured_analysis('NeedMoreEvidence')
    first = command('reject', 'reject-source', run: business, reason: 'The comparison used an outdated source.')
    expect(command('reject', 'reject-source', run: business, reason: 'The comparison used an outdated source.')).to eq(first)
    Contract.wait { analysis && analysis['id'] != saved.fetch('id') }
    updated = settled_analysis('NeedMoreEvidence')
    expect(updated.fetch('id')).not_to eq(saved.fetch('id'))
    context = JSON.parse(analysis_calls.last.fetch('prompt').split("\nContext:\n", 2).last).fetch('analysisEvidence')
    expect(JSON.generate(context.fetch('positiveRuns'))).not_to include(business)
    expect(JSON.generate(context.fetch('negativeFeedback'))).to include('outdated source')
    host.restart
    expect(analysis_calls.length).to eq(2)
    expect(analysis['status']).to eq('NeedMoreEvidence')
  end

  it 'S7 retains an unknown-start analysis without replaying it after a host crash' do
    configured_analysis('Ready', block: true, gate: 'crashed-analysis')
    business = run
    active = started_analysis
    host.crash
    host.release('crashed-analysis')
    host.start
    host.idle
    expect(analysis.fetch('id')).to eq(active.fetch('id'))
    expect(analysis['status']).not_to eq('Ready')
    expect(analysis_calls.length).to eq(1)
    retained = detail(active.fetch('id'))
    expect(retained.to_json).to include('purpose')
    call_id = retained.fetch('invocations').last.fetch('callId')
    expect(call_id).not_to be_nil
    expect(host.request('POST', "/api/agents/#{@owner}/turns/#{call_id}/retry", {}).first).to eq(409)
    saved_path = File.join(host.root, 'agentpy/agents', @owner, 'state.json')
    saved = JSON.parse(File.read(saved_path))
    edited = JSON.parse(JSON.generate(saved))
    turn = edited.fetch('turns').find { |row| row['id'] == call_id }
    turn['managed_command'] = {id: 'forbidden-analysis-retry', action: 'retry', revision: turn.fetch('managed').fetch('revision')}
    Contract.atomic_json(saved_path, edited)
    expect(host.request('GET', '/api/state').first).to eq(409)
    Contract.atomic_json(saved_path, saved)
    host.restart
    expect(analysis_calls.length).to eq(1)
    expect(routine.fetch('runs').map { |row| row['id'] }).to eq([business])
  end

  it 'S8 reconsiders a new accepted business run without recursively treating analysis as evidence' do
    configured_analysis('NeedMoreEvidence')
    first = run('first-observation')
    initial = settled_analysis('NeedMoreEvidence')
    configured_analysis
    second = run('new-observation')
    final = settled_analysis
    expect(final.fetch('id')).not_to eq(initial.fetch('id'))
    expect(analysis_calls.length).to eq(2)
    expect(final['sourceRuns']).to include(second)
    context = JSON.parse(analysis_calls.last.fetch('prompt').split("\nContext:\n", 2).last).fetch('analysisEvidence')
    expect(context.fetch('positiveRuns')).to include(first, second)
    expect(context.fetch('source').fetch('id')).to eq(second)
    host.restart
    host.idle
    expect(analysis_calls.length).to eq(2)
    expect(routine.fetch('runs').map { |row| row['id'] }).to match_array([first, second])
  end

  it 'S10 rejects a malformed Ready and does not automatically retry the analysis' do
    configured_analysis('Ready', response: decision('AutomationAnalyzed', decision: {status: 'Ready'}))
    run
    Contract.wait { analysis && %w[failed unresolved].include?(analysis['status']) }
    host.restart
    expect(analysis['status']).not_to eq('Ready')
    expect(analysis_calls.length).to eq(1)
  end

  it 'S10 keeps analysis details owner-scoped and preserves unknown usage' do
    configured_analysis('Ready', usage: nil)
    run
    saved = settled_analysis
    stranger = host.agent('OtherOwner')
    expect(host.request('GET', "#{path(stranger)}/analysis/#{saved.fetch('id')}").first).to eq(404)
    expect(host.request('POST', "#{path(stranger)}/analyze", operation: 'foreign-analysis', revision: 1).first).to eq(404)
    invocations = detail(saved.fetch('id')).fetch('invocations')
    expect(invocations).not_to be_empty
    expect(invocations.last.fetch('evidence').fetch('usage')).to include('inputTokens' => nil, 'outputTokens' => nil)
    expect(analysis_calls.length).to eq(1)
  end
  it 'S6 invalidates a pending answer when its accepted business source is rejected' do
    configured_analysis('Ready', block: true, gate: 'feedback-analysis')
    business = run
    active = started_analysis
    configured_analysis('NeedMoreEvidence')
    command('reject', 'feedback-during-analysis', run: business, reason: 'The source was stale.')
    host.release('feedback-analysis')
    host.idle
    expect(analysis['status']).not_to eq('Ready')
    expect(analysis_calls.length).to eq(1)
    command('analyze', 'reconsider-after-pending-feedback')
    settled_analysis('NeedMoreEvidence')
    expect(analysis.fetch('id')).not_to eq(active.fetch('id'))
    expect(analysis_calls.length).to eq(2)
    expect(routine.fetch('runs').length).to eq(1)
  end

  it 'S7 does not retry a failed provider call on restart or explicit detail reads' do
    configured_analysis('Ready', fail: true)
    run
    failed = settled_analysis('failed')
    3.times { detail(failed.fetch('id')) }
    host.restart
    expect(analysis.fetch('id')).to eq(failed.fetch('id'))
    expect(analysis_calls.length).to eq(1)
  end

  it 'S9 analyzes an interval occurrence without stealing a neighboring owner task', :slow do
    neighbor = host.agent('BusyNeighbor')
    configured_analysis
    rules = host.provider.fetch('rules').merge("#{neighbor}:Assessing" => {block: true, gate: 'neighbor-work'})
    host.configure(rules: rules)
    task = host.submit(neighbor, 'Keep my separate task intact')
    Contract.wait { host.calls.any? { |call| call['owner'] == neighbor } }
    command('schedule', 'every-minute', minutes: 1)
    Contract.wait(timeout: 150, description: 'real interval occurrence accepted and analyzed') do
      analysis && analysis['status'] == 'Ready'
    end
    business = routine.fetch('runs')
    expect(business.length).to eq(1)
    expect(business.first['trigger']).to eq('interval')
    expect(analysis_calls.length).to eq(1)
    expect(analysis_calls.first['owner']).to eq(@owner)
    command('pause', 'pause-interval', revision: 2)
    host.release('neighbor-work')
    host.turn(task)
    host.restart
    expect(analysis_calls.length).to eq(1)
    expect(routine.fetch('runs').length).to eq(1)
  end

  it 'S9 stops queued analysis without interrupting the owner conversation' do
    configured_analysis
    run
    settled_analysis
    rules = host.provider.fetch('rules').merge("#{@owner}:Assessing" => {block: true, gate: 'owner-conversation'})
    host.configure(rules: rules)
    conversation = host.submit(@owner, 'Keep this conversation separate')
    Contract.wait { host.calls.any? { |call| call['node'] == 'Assessing' && call['owner'] == @owner } }
    command('analyze', 'queued-analysis')
    queued = settled_analysis('queued')
    command('stop-analysis', 'cancel-queued-analysis', analysis: queued.fetch('id'))
    host.release('owner-conversation')
    host.turn(conversation)
    host.restart
    expect(analysis_calls.length).to eq(1)
    expect(analysis['status']).to eq('stopped')
    expect(routine.fetch('runs').length).to eq(1)
  end

  it 'S5 preserves a pending analysis when the schedule changes' do
    configured_analysis('Ready', block: true, gate: 'rescheduled-analysis')
    run
    active = started_analysis
    command('schedule', 'schedule-edit', minutes: 60)
    host.release('rescheduled-analysis')
    expect(settled_analysis.fetch('id')).to eq(active.fetch('id'))
    host.restart
    expect(analysis_calls.length).to eq(1)
  end

  it 'S6 makes rejected Ready stale until an explicit reconsider command' do
    configured_analysis
    business = run
    saved = settled_analysis
    configured_analysis('NeedMoreEvidence')
    command('reject', 'reject-ready-source', run: business, reason: 'The source changed.')
    host.idle
    expect(analysis['status']).to eq('stale')
    expect(analysis_calls.length).to eq(1)
    host.restart
    expect(analysis_calls.length).to eq(1)
    command('analyze', 'reconsider-stale-ready')
    expect(settled_analysis('NeedMoreEvidence').fetch('id')).not_to eq(saved.fetch('id'))
    expect(analysis_calls.length).to eq(2)
  end

  {
    'rejected criteria' => {response: {event: 'Outcome', reason: 'A source is missing.', evidence: ['Partial comparison'], reply: 'Incomplete comparison', outcome: {status: 'Completed', satisfiedCriteria: [], artifacts: []}}},
    'malformed response' => {response: '{invalid final'},
    'failed provider' => {fail: true}
  }.each do |name, rule|
    it "S2 does not analyze an ordinary run with #{name}" do
      configured_analysis
      host.configure(rules: host.provider.fetch('rules').merge('Execution' => rule))
      run('unaccepted-business')
      host.restart
      host.idle
      expect(analysis_calls).to be_empty
    end
  end

  it 'S2 does not analyze a stopped ordinary run' do
    configured_analysis
    host.configure(rules: host.provider.fetch('rules').merge('Execution' => {block: true, gate: 'stopped-business'}))
    business = command('run', 'business-to-stop').fetch('run')
    Contract.wait { host.calls.any? { |call| call['node'] == 'Execution' } }
    command('stop', 'stop-business', run: business)
    host.release('stopped-business')
    host.restart
    host.idle
    expect(analysis_calls).to be_empty
  end

  it 'S2 gives the owner semantic evidence with explicit truncation provenance' do
    configured_analysis
    events = [{type: 'item.completed', item: {id: 'large-read', type: 'command_execution',
      command: 'read comparison sources', aggregated_output: 'source observation ' * 400, exit_code: 0, status: 'completed'}}]
    host.configure(rules: host.provider.fetch('rules').merge('Execution' => {events: events}))
    run
    settled_analysis
    context = JSON.parse(analysis_calls.last.fetch('prompt').split("\nContext:\n", 2).last).fetch('analysisEvidence')
    text = JSON.generate(context)
    expect(text).to include('read comparison sources', 'source observation', 'truncatedFields')
    expect(text).to match(/"truncatedFields":[1-9]/)
    expect(text).to include('"externalEffectsVerified":false')
  end

  it 'S10 blocks oversized mandatory UTF-8 context before invoking analysis' do
    configured_analysis
    run('small-business')
    settled_analysis
    criteria = 30.times.map { |index| "#{index}:" + '界' * 1995 }
    fields = @fields.merge(goal: '界' * 16000, criteria: criteria)
    command('edit', 'large-contract', **fields)
    configured_analysis
    result = decision('Outcome', reply: 'Comparison supplied', outcome: {status: 'Completed', satisfiedCriteria: criteria, artifacts: []})
    host.configure(rules: host.provider.fetch('rules').merge('Execution' => {response: result}))
    run('large-business', revision: 2, inputs: '界' * 16000)
    Contract.wait { analysis && analysis['status'] == 'blocked' }
    expect(analysis_calls.length).to eq(1)
    expect(analysis['reason']).not_to be_empty
    host.restart
    expect(analysis['status']).to eq('blocked')
    expect(analysis_calls.length).to eq(1)
  end

  it 'S2 does not automatically spend on pre-upgrade history and preserves unknown applicability on explicit analysis', :previous_release do
    configured_analysis
    business = run('pre-analysis-business')
    expect(analysis_calls).to be_empty
    host.stop
    File.write(@binary_pointer, File.join(Contract::ROOT, '.build/bin', Gem.win_platform? ? 'sapiens4.exe' : 'sapiens4'))
    host.start
    host.idle
    expect(analysis_calls).to be_empty
    command('analyze', 'inspect-old-history')
    settled_analysis
    context = JSON.parse(analysis_calls.last.fetch('prompt').split("\nContext:\n", 2).last).fetch('analysisEvidence')
    expect(JSON.generate(context.fetch('positiveRuns'))).not_to include(business)
    expect(context.fetch('source')).to include('id' => business, 'positiveCurrent' => false)
    expect(context.fetch('source').fetch('applicability')).to include('unknown')
    command('reject', 'reject-legacy-source', run: business, reason: 'This old source was incorrect.')
    expect(analysis['status']).to eq('stale')
    host.restart
    expect(analysis['status']).to eq('stale')
    expect(analysis_calls.length).to eq(1)
  end

  it 'S5 preserves a submitted goal edit after unsupported-provider preflight releases' do
    configured_analysis
    host.configure(model_catalog_gate: 'catalog-ready', model_catalog: [])
    business = command('run', 'preflight-business').fetch('run')
    Contract.wait(description: 'model catalog preflight starts') { File.exist?(File.join(host.root, 'catalog-ready-started')) }
    changed = @fields.merge(goal: 'Compare a different product source.', title: 'Changed while checking provider')
    address = URI(host.url)
    payload = JSON.generate(changed.merge(operation: 'edit-during-preflight', revision: 1))
    socket = TCPSocket.new(address.host, address.port)
    socket.write("POST #{path}/edit HTTP/1.1\r\nHost: #{address.host}:#{address.port}\r\nContent-Type: application/json\r\nX-Sapiens-Local: 1\r\nContent-Length: #{payload.bytesize}\r\nConnection: close\r\n\r\n#{payload}")
    socket.close
    host.release('catalog-ready')
    # Preflight holds the service lock. The submitted edit can finish after release;
    # replay its lost acknowledgement rather than claiming concurrent admission.
    command('edit', 'edit-during-preflight', **changed)
    expect(routine).to include('goal' => changed[:goal], 'title' => changed[:title], 'revision' => 2)
    expect(analysis_calls).to be_empty
    expect(routine.fetch('runs').map { |row| row['id'] }).to eq([business])
    host.restart
    expect(routine).to include('goal' => changed[:goal], 'title' => changed[:title], 'revision' => 2)
    expect(analysis_calls).to be_empty
  ensure
    host.release('catalog-ready') if @host
  end

  %w[http managed_file].each do |transport|
  it "S7 does not replace an analysis cancelled via #{transport} after another ordinary run finishes" do
    configured_analysis('NeedMoreEvidence')
    run('initial-observation')
    first = settled_analysis('NeedMoreEvidence')
    configured_analysis('NeedMoreEvidence', block: true, gate: 'cancelled-analysis')
    run('second-observation')
    Contract.wait { analysis_calls.length == 2 }
    active = started_analysis
    expect(active.fetch('id')).not_to eq(first.fetch('id'))
    third = command('run', 'third-observation').fetch('run')
    call_id = detail(active.fetch('id')).fetch('invocations').last.fetch('callId')
    if transport == 'http'
      host.api('POST', "/api/agents/#{@owner}/turns/#{call_id}/cancel", {})
    else
      saved_path = File.join(host.root, 'agentpy/agents', @owner, 'state.json')
      saved = JSON.parse(File.read(saved_path))
      turn = saved.fetch('turns').find { |row| row['id'] == call_id }
      turn['managed_command'] = {id: 'cancel-analysis-file', action: 'cancel', revision: turn.fetch('managed').fetch('revision')}
      Contract.atomic_json(saved_path, saved)
      Contract.wait { analysis && analysis['status'] == 'stopped' }
    end
    host.release('cancelled-analysis')
    host.idle
    expect(routine.fetch('runs').find { |row| row['id'] == third }['status']).to eq('done')
    expect(analysis_calls.length).to eq(2)
    host.restart
    host.idle
    expect(analysis_calls.length).to eq(2)
  end

  end

  it 'S6 still reconsiders new feedback after invalidating a pending NeedMoreEvidence follow-up' do
    configured_analysis('NeedMoreEvidence')
    initial_business = run('feedback-initial')
    initial = settled_analysis('NeedMoreEvidence')
    configured_analysis('NeedMoreEvidence', block: true, gate: 'feedback-follow-up')
    rejected_business = run('feedback-second')
    Contract.wait { analysis_calls.length == 2 }
    active = started_analysis
    prior_evidence = detail(active.fetch('id')).fetch('evidence')
    configured_analysis('NeedMoreEvidence')
    command('reject', 'reject-follow-up-source', run: rejected_business, reason: 'The second observation was incorrect.')
    host.release('feedback-follow-up')
    begin
      Contract.wait(description: 'new feedback reconsiders the prior NeedMoreEvidence decision') do
        analysis_calls.length == 3 && analysis && analysis['id'] != active.fetch('id')
      end
    rescue RuntimeError => error
      raise "#{error.message}; calls=#{analysis_calls.length}; visible analysis=#{analysis.inspect}"
    end
    final = settled_analysis('NeedMoreEvidence')
    expect(final.fetch('id')).not_to eq(initial.fetch('id'))
    expect(detail(final.fetch('id')).fetch('evidence')).not_to eq(prior_evidence)
    context = JSON.parse(analysis_calls.last.fetch('prompt').split("\nContext:\n", 2).last).fetch('analysisEvidence')
    expect(context.fetch('positiveRuns')).to include(initial_business)
    expect(context.fetch('positiveRuns')).not_to include(rejected_business)
    expect(JSON.generate(context.fetch('negativeFeedback'))).to include('second observation was incorrect')
    host.restart
    expect(analysis_calls.length).to eq(3)
  end

end
