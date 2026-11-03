require 'digest'
require 'rbconfig'
require 'open3'
require 'sqlite3'
require_relative 'support/effect_peer'
require_relative 'support/desktop'
require_relative 'support/feed_code_fixtures'

RSpec.describe 'Checked code consumes retained routine feed observations' do
  include FeedCodeFixtures

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

  before do |example|
    @host = Host.new(start: false, environment: {'SAPIENS_GENERATED_DOCKER' => @docker, 'SAPIENS_GENERATED_SOCKET' => @socket})
    @peer = EffectPeer.new(File.join(host.root, 'feed-code-peer'))
    @peer.page('c0', records: [{id: 'r1', value: 10}, {id: 'r2', value: 20}], next_cursor: 'c2')
    @peer.page('c2', records: [{id: 'r2', value: 20}, {id: 'r3', value: 30}], next_cursor: 'page:next-z9')
    @peer.page('page:next-z9', records: [{id: 'r3', value: 30}, {id: 'r4', value: -5}], next_cursor: 'opaque:final')
    host.configure(version: 'codex-cli 0.162.1', model_catalog: [{slug: 'gpt-6-sol', shell_type: 'unified_exec', apply_patch_tool_type: 'freeform', experimental_supported_tools: [], tool_mode: 'direct', supports_search_tool: false, supported_reasoning_levels: %w[low medium high xhigh max ultra].map { |effort| {effort: effort, description: effort} }}])
    host.start
    @owner = host.agent('FeedCodeOwner')
    @profile = {kind: 'receipt_feed_v1', taskBoundary: 'content_result', executionPolicy: 'response_only', source: {url: @peer.url + '/feed', identity: 'sales-feed', initialCursor: 'c0'}, delivery: {url: @peer.url + '/deliveries', receiptUrl: @peer.url + '/receipts', identity: 'daily-inbox'}}
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", operation: 'create-feed', title: 'Checked feed summary', goal: FeedCodeFixtures::GOAL, criteria: FeedCodeFixtures::CRITERIA, externalProfile: @profile).fetch('routine')
    host.configure(rules: {
      'Execution' => {response: decision('Outcome', reply: FeedCodeFixtures::OBSERVED_TEXT, outcome: {status: 'Completed', satisfiedCriteria: FeedCodeFixtures::CRITERIA, artifacts: []}), usage: {input_tokens: 43, cached_input_tokens: 3, output_tokens: 17}},
      'AutomationAnalysis' => {response: decision('AutomationAnalyzed', decision: {status: 'Ready', automatablePart: 'Render newly observed integer-valued records using the supplied prefix.', parameters: [{name: 'prefix', description: 'Prefix for the content text'}]}), usage: {input_tokens: 47, cached_input_tokens: 0, output_tokens: 19}}
    })
  end

  after do
    @peer&.release
    begin
      @host&.stop
    ensure
      @peer&.close
      @host&.close
    end
  end

  def path
    "/api/agents/#{@owner}/routines/#{@routine}"
  end

  def routine
    host.state.fetch('personal_routines').find { |row| row['id'] == @routine }
  end

  def detail(id)
    host.api('GET', "#{path}/runs/#{id}")
  end

  def submit(operation, input = FeedCodeFixtures::PARAMETERS)
    host.api('POST', "#{path}/run", operation: operation, revision: 1, inputs: JSON.generate(input)).fetch('run')
  end

  def delivered(id)
    Contract.wait(timeout: 45, description: 'accepted content and confirmed actual delivery') do
      row = detail(id)
      raise JSON.pretty_generate(row) if %w[failed interrupted stopped].include?(row['status'])
      row if row.dig('externalExecution', 'delivery', 'status') == 'confirmed'
    end
  end

  def prepare(operation: 'prepare-feed', fixtures: independent_feed_cases, fixture_fields: {}, source: feed_code_source, generation: {}, assessment: {})
    @analysis = Contract.wait(description: 'accepted observed source analysis') do
      row = routine['analysis']
      row if row && row['status'] == 'Ready'
    end
    host.configure(rules: host.provider.fetch('rules').merge(
      'PreparationFixtures' => {response: {contentInterface: 'script-text-v1', fixtures: fixtures, reason: 'Independent initial, overlapping and empty complete pages.'}.merge(fixture_fields)},
      'PreparationGeneration' => {response: {language: 'python', profile: 'python-3.13.15-linux-arm64', source: source, interface: 'json-stdin-result-v1', dependencies: [], capabilities: []}.merge(generation)},
      'PreparationReview' => {response: {accepted: true, reason: 'Actual independent fixture texts match the original content criteria.'}},
      'CodeAssessment' => {response: {verdict: 'accepted', reply: 'Verifier replacement text must never be delivered.', reason: 'Actual script text names only the new records and exact values with the accepted prefix.', evidence: ['Retained source page, normalized input and actual script-produced text.'], acceptance: {status: 'Completed', satisfiedCriteria: FeedCodeFixtures::CRITERIA, artifacts: []}, activationAudit: {accepted: true, reason: 'Exact checked implementation satisfied the authorized observed input.'}}.merge(assessment), usage: {input_tokens: 61, cached_input_tokens: 1, output_tokens: 23}}
    ))
    id = host.api('POST', "#{path}/prepare", operation: operation, revision: 1, analysis: @analysis.fetch('id')).fetch('preparation')
    Contract.wait(timeout: 90, description: 'checked feed candidate') do
      row = host.api('GET', "#{path}/preparations/#{id}")
      raise JSON.pretty_generate(row) if %w[failed stopped stale interrupted cleanup_pending].include?(row['status'])
      row if row['status'] == 'awaiting_occurrence'
    end
  end

  def rule(node, fields)
    rules = host.provider.fetch('rules')
    host.configure(rules: rules.merge(node => rules.fetch(node, {}).merge(fields)))
  end

  def settled(id)
    host.turn(id, %w[done failed interrupted cancelled])
    detail(id)
  end

  def stored_work(id)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      row = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', id))
      yield row
      db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(row), id])
    end
  end

  def hybrid_verdicts(first: 'pass')
    [{id: 'criterion-1', status: first, evidence: first == 'pass' ? 'Every observed new ID and integer matches the useful explanation.' : 'The useful explanation contradicts the observed integer.'},
     {id: 'criterion-2', status: 'pass', evidence: 'The explanation uses the supplied prefix and handles empty input.'}]
  end

  def prepare_hybrid
    examples = independent_feed_cases
    facts = [
      {prefix: 'Feed', records: [{id: 'r1', value: 10}, {id: 'r2', value: 20}], total: 30},
      {prefix: 'Feed', records: [{id: 'r3', value: 30}], total: 30},
      {prefix: 'Quiet', records: [], total: 0}
    ]
    examples = examples.each_with_index.map { |sample, index| sample.merge(expected: {status: 'ok', value: facts[index]}) }
    explanation = {response: {text: 'Feed: r1 = 10; r2 = 20. Total: 30.'}, variants: [
      {contains: 'page:next-z9', response: {text: 'Feed: r3 = 30. Total: 30.'}},
      {contains: 'opaque:empty', response: {text: 'Quiet: no new records. Total: 0.'}}
    ], usage: {input_tokens: 53, cached_input_tokens: 3, output_tokens: 17}}
    host.configure(rules: host.provider.fetch('rules').merge(
      'PreparationExplanation' => explanation,
      'PreparationRubricReview' => {response: {criteria: hybrid_verdicts}},
      'HybridExplanation' => explanation,
      'HybridAssessment' => {response: {verdict: 'accepted', reason: 'Retained facts and useful explanation satisfy original criteria.', evidence: ['Actual records and useful text.'], criteria: hybrid_verdicts, activationAudit: {accepted: true, reason: 'The authorized observed hybrid result is correct.'}}, usage: {input_tokens: 61, cached_input_tokens: 1, output_tokens: 23}}
    ))
    prepare(fixtures: examples, fixture_fields: {contentInterface: 'hybrid-explanation-v1', rubric: [{id: 'criterion-1', kind: 'numeric'}, {id: 'criterion-2', kind: 'semantic'}]}, source: File.read(File.join(__dir__, 'support/feed_code_hybrid.py')), generation: {explanation: {instruction: 'Explain every new record and its exact integer value with the supplied prefix. Include the calculated total without inventing records or repeating previous records.'}})
  end

  def canonical(value)
    case value
    when Hash then value.keys.sort.to_h { |key| [key, canonical(value.fetch(key))] }
    when Array then value.map { |item| canonical(item) }
    else value
    end
  end

  it 'prepares from a retained accepted v13 observation without inventing prior history' do
    ordinary = delivered(submit('retained-v13'))
    Contract.wait { routine.dig('analysis', 'status') == 'Ready' }
    host.stop
    stored_work(ordinary.fetch('id')) { |work| work.fetch('externalExecution').delete('inputBinding') }
    host.start
    candidate = prepare
    expect(candidate.fetch('status')).to eq('awaiting_occurrence')
    expect(@peer.deliveries.length).to eq(1)
  end

  %w[missing changed].each do |fault|
    it "refuses preparation from a retained v13 observation with #{fault} delivery reservation" do
      ordinary = delivered(submit("retained-v13-#{fault}"))
      Contract.wait { routine.dig('analysis', 'status') == 'Ready' }
      analysis = routine.fetch('analysis').fetch('id')
      host.stop
      stored_work(ordinary.fetch('id')) do |work|
        external = work.fetch('externalExecution')
        external.delete('inputBinding')
        if fault == 'missing'
          external.delete('delivery')
        else
          external.fetch('delivery').fetch('request')['recordIds'] = ['fabricated']
        end
      end
      host.start
      before_calls = host.calls.length
      code, response = host.request('POST', "#{path}/prepare", operation: "invalid-v13-#{fault}", revision: 1, analysis: analysis)
      expect(code).to eq(409)
      expect(response.to_s).to include('delivery')
      expect(host.calls.length).to eq(before_calls)
      expect(@peer.deliveries.length).to eq(1)
    end
  end

  it 'rejects unsupported business parameters without moving cursor or blaming the active script' do
    delivered(submit('observed'))
    prepare
    delivered(submit('activate'))
    reference = routine.fetch('activeCode').fetch('reference')
    result = settled(submit('invalid-parameters', {unsupported: true}))
    expect(result.dig('outcome', 'status')).not_to eq('Completed')
    expect(routine.fetch('sourceState')).to include('cursor' => 'page:next-z9', 'seenIds' => %w[r1 r2 r3])
    expect(routine.fetch('activeCode')).to include('reference' => reference, 'status' => 'active')
    expect(@peer.deliveries.length).to eq(2)
  end

  it 'shows the retained historical version and ordinary future execution after a source profile edit', :desktop do
    delivered(submit('observed'))
    prepared = prepare
    delivered(submit('activate'))
    fields = {title: 'Checked feed summary', goal: FeedCodeFixtures::GOAL, criteria: FeedCodeFixtures::CRITERIA}
    host.api('POST', "#{path}/edit", fields.merge(operation: 'remove-profile-ui', revision: 1, externalProfile: nil))
    expect(routine.fetch('activeCode')).to include('status' => 'stale', 'reference' => prepared.dig('version', 'reference'))
    calls = host.calls.length
    start_desktop
    desktop.click('Open FeedCodeOwner')
    desktop.click('Work menu')
    desktop.click('Automation menu')
    desktop.click('Routines')
    desktop.click('Checked feed summary')
    desktop.see('This historical version no longer applies to the current task or source profile. Its history is retained.')
    expect(host.calls.length).to eq(calls)
  end

  it 'prepares from an actual accepted observation and atomically retains overlap cursor plus active code before one host delivery' do
    ordinary = delivered(submit('ordinary-observation'))
    expect(ordinary.dig('outcome', 'status')).to eq('Completed')
    expect(@peer.deliveries.length).to eq(1)
    prepared = prepare
    version = prepared.fetch('version')
    generation = host.calls.find { |call| call['node'] == 'PreparationGeneration' }
    expect(generation.fetch('prompt')).not_to include(FeedCodeFixtures::OVERLAP_TEXT, 'Quiet: no new records.')
    expect(version.fetch('inputBinding')).to include('kind' => 'receipt_feed_v1', 'interface' => 'receipt-feed-input-v1', 'contentInterface' => 'script-text-v1')
    expect(@peer.deliveries.length).to eq(1)
    @peer.delivery_mode = :hold_before_commit
    trial_id = submit('overlap-trial')
    Contract.wait { @peer.requests.count { |request| request[:method] == 'POST' } == 2 }
    expect(@peer.deliveries.length).to eq(1)
    expect(routine.fetch('sourceState')).to include('cursor' => 'page:next-z9', 'seenIds' => %w[r1 r2 r3])
    expect(routine.fetch('activeCode')).to include('status' => 'active', 'reference' => version.fetch('reference'))
    @peer.delivery_mode = :confirm
    @peer.release
    trial = delivered(trial_id)
    binding = trial.dig('externalExecution', 'inputBinding')
    expected_input = {'schema' => 1, 'interface' => 'receipt-feed-input-v1', 'taskInputs' => {'prefix' => 'Feed'}, 'observation' => JSON.parse(JSON.generate(feed_page('c2', 'page:next-z9', [{id: 'r2', value: 20}, {id: 'r3', value: 30}]))), 'newRecords' => [{'id' => 'r3', 'value' => 30}], 'previouslySeenIds' => ['r2']}
    expect(binding.fetch('input')).to eq(expected_input)
    expect(binding.fetch('inputHash')).to eq(Digest::SHA256.hexdigest(JSON.generate(canonical(expected_input))))
    expect(trial.dig('codeExecution', 'input')).to eq(expected_input)
    expect(trial.dig('codeExecution', 'reference')).to eq(version.fetch('reference'))
    expect(trial.dig('codeExecution', 'runtimeResult', 'actual')).to eq('status' => 'ok', 'value' => {'text' => FeedCodeFixtures::OVERLAP_TEXT})
    expect(trial.fetch('result')).to eq(FeedCodeFixtures::OVERLAP_TEXT)
    expect(trial.dig('executionBinding', 'contract', 'completionCriteria')).to eq(FeedCodeFixtures::CRITERIA)
    expect(routine.fetch('sourceState')).to include('cursor' => 'page:next-z9', 'seenIds' => %w[r1 r2 r3])
    expect(routine.fetch('activeCode')).to include('status' => 'active', 'reference' => version.fetch('reference'))
    expect(@peer.deliveries.length).to eq(2)
    expect(@peer.deliveries.last.fetch('request')).to include('recordIds' => ['r3'], 'content' => {'text' => FeedCodeFixtures::OVERLAP_TEXT})
    host.restart
    expect(routine.fetch('sourceState')).to include('cursor' => 'page:next-z9', 'seenIds' => %w[r1 r2 r3])
    expect(routine.fetch('activeCode')).to include('reference' => version.fetch('reference'))
    active = delivered(submit('following-page'))
    expect(active.dig('codeExecution', 'role')).to eq('active')
    expect(active.dig('codeExecution', 'reference')).to eq(version.fetch('reference'))
    expect(active.dig('externalExecution', 'inputBinding', 'input', 'previouslySeenIds')).to eq(['r3'])
    expect(@peer.deliveries.length).to eq(3)
    expect(@peer.deliveries.last.fetch('request')).to include('recordIds' => ['r4'], 'content' => {'text' => FeedCodeFixtures::NEXT_TEXT})
    expect(host.calls.count { |call| call['node'] == 'Execution' }).to eq(1)
    expect(@peer.requests.select { |request| request[:path] == '/feed' }.map { |request| request[:query]['cursor'] }).to eq(['c0', 'c2', 'page:next-z9'])
  end

  it 'keeps accepted code active after lost ACK while blocking all new consumer execution until exact receipt lookup' do
    delivered(submit('observed'))
    prepared = prepare
    @peer.delivery_mode = :drop_ack
    id = submit('lost-code-ack')
    result = settled(id)
    expect(result.dig('externalExecution', 'delivery', 'status')).to eq('unknown')
    expect(result.dig('outcome', 'status')).to eq('Completed')
    expect(routine.fetch('activeCode')).to include('status' => 'active', 'reference' => prepared.dig('version', 'reference'))
    expect(routine.fetch('sourceState')).to include('cursor' => 'page:next-z9', 'seenIds' => %w[r1 r2 r3])
    host.restart
    expect(host.request('POST', "#{path}/run", operation: 'new-unknown-key', revision: 1).first).to eq(409)
    expect(host.request('POST', "/api/agents/#{@owner}/turns/#{id}/retry", {}).first).to eq(409)
    expect(submit('lost-code-ack')).to eq(id)
    before = @peer.requests.count { |request| request[:method] == 'POST' }
    host.api('POST', "#{path}/runs/#{id}/reconcile-delivery", operation: 'exact-code-receipt')
    expect(detail(id).dig('externalExecution', 'delivery', 'status')).to eq('confirmed')
    expect(@peer.requests.count { |request| request[:method] == 'POST' }).to eq(before)
    expect(@peer.deliveries.length).to eq(2)
  end

  %w[unavailable incomplete].each do |fault|
    it "does not invoke code or advance source state for a #{fault} actual source page" do
      delivered(submit('observed'))
      prepare
      @peer.page('c2', records: [{id: 'r3', value: 30}], next_cursor: 'wrong-next', complete: fault != 'incomplete', status: fault == 'unavailable' ? 503 : 200)
      result = settled(submit('bad-page'))
      expect(result.dig('outcome', 'status')).not_to eq('Completed')
      expect(result.dig('codeExecution', 'container')).to be_nil
      expect(host.calls.none? { |call| call['node'] == 'CodeAssessment' }).to be(true)
      expect(routine.fetch('sourceState')).to include('cursor' => 'c2', 'seenIds' => %w[r1 r2])
      expect(@peer.deliveries.length).to eq(1)
      expect(routine['activeCode']).to be_nil
    end
  end

  it 'rejects malformed stored seen IDs rather than deriving a new unseen input from an empty default' do
    delivered(submit('observed'))
    prepare
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      sequence = @routine.split('_').last.to_i
      row = JSON.parse(db.get_first_value('SELECT value FROM scheduled_routines WHERE sequence=?', sequence))
      row.fetch('sourceState')['seenIds'] = 'not-an-array'
      db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?', [JSON.generate(row), sequence])
    end
    response, admitted = host.request('POST', "#{path}/run", operation: 'malformed-authority', revision: 1)
    if response == 200
      result = settled(admitted.fetch('run'))
      expect(result.dig('externalExecution', 'inputBinding')).to be_nil
      expect(result.dig('codeExecution', 'container')).to be_nil
      expect(result.dig('outcome', 'status')).not_to eq('Completed')
    else
      expect([409, 500]).to include(response)
    end
    expect(@peer.deliveries.length).to eq(1)
    expect(@peer.requests.count { |request| request[:path] == '/feed' }).to eq(1)
    expect(host.calls.none? { |call| call['node'] == 'CodeAssessment' }).to be(true)
  end

  it 'rejects a normalized envelope over the existing runtime input bound before executing a body' do
    delivered(submit('observed'))
    prepare
    @peer.page('c2', records: [{id: 'large', value: 'x' * 36_000}], next_cursor: 'must-not-commit')
    result = settled(submit('oversized-envelope'))
    expect(result.dig('outcome', 'status')).not_to eq('Completed')
    expect(result.dig('codeExecution', 'container')).to be_nil
    expect(host.calls.none? { |call| call['node'] == 'CodeAssessment' }).to be(true)
    expect(routine.fetch('sourceState')).to include('cursor' => 'c2', 'seenIds' => %w[r1 r2])
    expect(@peer.deliveries.length).to eq(1)
  end

  it 'retains the actual code result but does not activate or deliver after an explicitly advanced saved cursor conflicts' do
    delivered(submit('observed'))
    prepare
    rule('CodeAssessment', block: true, gate: 'code-cursor-cas')
    id = submit('stale-code-cursor')
    Contract.wait { host.calls.any? { |call| call['node'] == 'CodeAssessment' } }
    before = detail(id).dig('externalExecution', 'inputBinding')
    # External saved-state intervention, not simultaneous same-owner scheduler execution.
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      sequence = @routine.split('_').last.to_i
      row = JSON.parse(db.get_first_value('SELECT value FROM scheduled_routines WHERE sequence=?', sequence))
      state = row.fetch('sourceState')
      state['revision'] += 1
      state['cursor'] = 'opaque:newer'
      state['seenIds'] = %w[r1 r2 r3 newer]
      db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?', [JSON.generate(row), sequence])
    end
    host.release('code-cursor-cas')
    result = settled(id)
    expect(result.dig('outcome', 'status')).to eq('Unresolved')
    expect(result.dig('externalExecution', 'cursorCommit', 'status')).to eq('conflict')
    expect(result.dig('externalExecution', 'inputBinding')).to eq(before)
    expect(result.dig('codeExecution', 'runtimeResult', 'actual')).to eq('status' => 'ok', 'value' => {'text' => FeedCodeFixtures::OVERLAP_TEXT})
    expect(routine.fetch('sourceState')).to include('cursor' => 'opaque:newer', 'seenIds' => %w[r1 r2 r3 newer])
    expect(routine['activeCode']).to be_nil
    expect(@peer.deliveries.length).to eq(1)
  end

  it 'revalidates the exact saved input before committing an otherwise accepted code assessment' do
    delivered(submit('observed'))
    prepare
    rule('CodeAssessment', block: true, gate: 'input-tamper')
    id = submit('retained-input')
    Contract.wait { host.calls.any? { |call| call['node'] == 'CodeAssessment' } }
    stored_work(id) { |row| row.fetch('externalExecution').fetch('inputBinding').fetch('input').fetch('newRecords').first['value'] = 999 }
    host.release('input-tamper')
    result = settled(id)
    expect(result.dig('outcome', 'status')).not_to eq('Completed')
    expect(routine.fetch('sourceState')).to include('cursor' => 'c2', 'seenIds' => %w[r1 r2])
    expect(routine['activeCode']).to be_nil
    expect(@peer.deliveries.length).to eq(1)
  end


  it 'delivers a useful real hybrid explanation of exact observed facts through the same cursor acceptance boundary' do
    delivered(submit('observed'))
    prepared = prepare_hybrid
    expect(prepared.dig('version', 'inputBinding', 'contentInterface')).to eq('hybrid-explanation-v1')
    result = delivered(submit('hybrid-overlap'))
    expect(result.dig('codeExecution', 'hybridResult')).to eq('facts' => {'prefix' => 'Feed', 'records' => [{'id' => 'r3', 'value' => 30}], 'total' => 30}, 'explanation' => 'Feed: r3 = 30. Total: 30.')
    expect(@peer.deliveries.last.dig('request', 'content')).to eq('text' => 'Feed: r3 = 30. Total: 30.')
    expect(routine.fetch('sourceState')).to include('cursor' => 'page:next-z9', 'seenIds' => %w[r1 r2 r3])
    expect(routine.fetch('activeCode')).to include('reference' => prepared.dig('version', 'reference'), 'status' => 'active')
    expect(host.calls.count { |call| call['node'] == 'HybridExplanation' }).to eq(1)
    expect(host.calls.count { |call| call['node'] == 'HybridAssessment' }).to eq(1)
  end

  it 'does not commit or deliver contradictory hybrid prose merely because retained numeric facts are right' do
    delivered(submit('observed'))
    prepare_hybrid
    rule('HybridExplanation', response: {text: 'Feed: r3 = 999. Total: 999.'}, variants: [])
    rule('HybridAssessment', response: {verdict: 'rejected', reason: 'The text contradicts r3=30.', evidence: ['Actual input and retained calculation both say30.'], criteria: hybrid_verdicts(first: 'fail'), activationAudit: {accepted: false, reason: 'The useful content fails original criteria.'}})
    result = settled(submit('contradictory-hybrid'))
    expect(result.dig('codeExecution', 'hybridResult', 'facts', 'total')).to eq(30)
    expect(result.dig('outcome', 'status')).to eq('Unresolved')
    expect(routine.fetch('sourceState')).to include('cursor' => 'c2', 'seenIds' => %w[r1 r2])
    expect(routine['activeCode']).to be_nil
    expect(@peer.deliveries.length).to eq(1)
  end

  it 'retains calculated facts after Stop during explanation without cursor acceptance or delivery' do
    delivered(submit('observed'))
    prepare_hybrid
    rule('HybridExplanation', block: true, gate: 'stop-feed-explanation')
    id = submit('stopped-hybrid')
    Contract.wait { host.calls.any? { |call| call['node'] == 'HybridExplanation' } }
    actual = detail(id).dig('codeExecution', 'runtimeResult')
    expect(actual.dig('actual', 'value', 'total')).to eq(30)
    host.api('POST', "#{path}/stop", operation: 'stop-explanation', revision: 1, run: id)
    host.release('stop-feed-explanation')
    result = settled(id)
    expect(result.dig('codeExecution', 'runtimeResult')).to eq(actual)
    expect(host.calls.none? { |call| call['node'] == 'HybridAssessment' }).to be(true)
    expect(routine.fetch('sourceState')).to include('cursor' => 'c2', 'seenIds' => %w[r1 r2])
    expect(@peer.deliveries.length).to eq(1)
  end

  it 'revokes an old profile epoch while preserving the accepted code input and never resurrects its candidate after restoration' do
    delivered(submit('observed'))
    prepared = prepare
    rule('CodeAssessment', block: true, gate: 'feed-profile-aba')
    id = submit('before-profile-edit')
    Contract.wait { host.calls.any? { |call| call['node'] == 'CodeAssessment' } }
    original = detail(id).dig('externalExecution', 'inputBinding')
    fields = {title: 'Checked feed summary', goal: FeedCodeFixtures::GOAL, criteria: FeedCodeFixtures::CRITERIA}
    host.api('POST', "#{path}/edit", fields.merge(operation: 'remove-profile', revision: 1, externalProfile: nil))
    host.api('POST', "#{path}/edit", fields.merge(operation: 'restore-profile', revision: 2, externalProfile: @profile))
    host.release('feed-profile-aba')
    result = settled(id)
    expect(result.dig('outcome', 'status')).not_to eq('Completed')
    expect(result.dig('externalExecution', 'inputBinding')).to eq(original)
    expect(result.dig('codeExecution', 'reference')).to eq(prepared.dig('version', 'reference'))
    expect(routine['activeCode']).to be_nil
    expect(@peer.deliveries.length).to eq(1)
    rule('Execution', response: decision('Outcome', reply: FeedCodeFixtures::OVERLAP_TEXT, outcome: {status: 'Completed', satisfiedCriteria: FeedCodeFixtures::CRITERIA, artifacts: []}))
    next_id = host.api('POST', "#{path}/run", operation: 'ordinary-after-restore', revision: 3, inputs: JSON.generate(FeedCodeFixtures::PARAMETERS)).fetch('run')
    expect(detail(next_id).dig('executionBinding', 'binding', 'mode')).to eq('ordinary')
    delivered(next_id)
    expect(detail(next_id)['codeExecution']).to be_nil
  end

end
