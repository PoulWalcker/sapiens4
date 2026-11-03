require 'digest'
require 'rbconfig'
require 'open3'
require 'sqlite3'
require_relative 'support/effect_peer'
require_relative 'support/desktop'
require_relative 'support/feed_code_fixtures'

RSpec.describe 'Bounded fallback preserves accepted routine work' do
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

  def prepare_hybrid(source: fallback_source)
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
    prepare(fixtures: examples, fixture_fields: {contentInterface: 'hybrid-explanation-v1', rubric: [{id: 'criterion-1', kind: 'numeric'}, {id: 'criterion-2', kind: 'semantic'}]}, source: source, generation: {explanation: {instruction: 'Explain every new record and its exact integer value with the supplied prefix. Include the calculated total without inventing records or repeating previous records.'}})
  end

  def canonical(value)
    case value
    when Hash then value.keys.sort.to_h { |key| [key, canonical(value.fetch(key))] }
    when Array then value.map { |item| canonical(item) }
    else value
    end
  end

  def fallback_source
    source = File.read(File.join(__dir__, 'support/feed_code_hybrid.py'))
    source = source.sub('records = value["newRecords"]', 'if value["taskInputs"].get("force_failure"): raise ValueError("Business-only calculation fault")' + "\n" + 'records = value["newRecords"]')
    source.sub('sum(record["value"] for record in records)', '(999 if value["taskInputs"].get("force_wrong") else sum(record["value"] for record in records))')
  end

  def response_for(records, text)
    {resolved: true, result: {status: 'ok', value: {prefix: 'Feed', records: records, total: records.sum { |record| record.fetch(:value) }}}, text: text, reason: 'Independent expected records and original arithmetic criteria.'}
  end

  def configure_fallback(node, response, **options)
    rule(node, {response: response, usage: {input_tokens: 83, cached_input_tokens: 2, output_tokens: 29}}.merge(options))
  end

  def fallback_calls(node)
    host.calls.select { |call| call['node'] == node }
  end

  def context(call)
    JSON.parse(call.fetch('prompt').split("\nContext:\n", 2).last)
  end

  def fallback_setup
    delivered(submit('ordinary'))
    prepare_hybrid
  end

  it 'replaces a failed real calculation locally while retaining acceptance, delivery, fault and attempt expense' do
    prepared = fallback_setup
    delivered(submit('activate'))
    text = 'Feed: r4 = -5. Total: -5.'
    configure_fallback('LocalFallback', response_for([{id: 'r4', value: -5}], text))
    rule('HybridExplanation', response: {text: text}, variants: [])
    id = submit('local-replacement', {prefix: 'Feed', force_failure: true})
    result = delivered(id)
    code = result.fetch('codeExecution')
    expect(code.fetch('completedViaFallback')).to be(true)
    expect(code.fetch('faults')).to include(include('scope' => 'calculation', 'reference' => prepared.dig('version', 'reference')))
    expect(routine.fetch('activeCode')).to include('status' => 'suspect', 'reference' => prepared.dig('version', 'reference'))
    expect(code.dig('activation', 'status')).not_to eq('activated')
    expect(code.fetch('operations').count { |op| op['purpose'] == 'code_execution' }).to eq(1)
    local = code.fetch('operations').find { |op| op['purpose'] == 'code_local_fallback' }
    expect(local.dig('evidence', 'usage')).to include('inputTokens' => 83, 'outputTokens' => 29)
    expect(context(fallback_calls('LocalFallback').last).fetch('input')).to eq(code.fetch('input'))
    expect(result.fetch('invocations')).to include(include('decisionId' => local.fetch('id'), 'purpose' => 'code_local_fallback'))
    expect(result.fetch('invocations').find { |entry| entry['node'] == 'HybridAssessment' }).to have_key('assessment')
    expect(@peer.deliveries.length).to eq(3)
    expect(@peer.deliveries.last.dig('request', 'content', 'text')).to eq(text)
    expect(fallback_calls('WholeFallback')).to be_empty
  end

  it 'assesses corrected whole facts against original criteria without replaying the wrong calculation or activating it' do
    fallback_setup
    corrected = response_for([{id: 'r3', value: 30}], 'Feed: r3 = 30. Total: 30.')
    configure_fallback('WholeFallback', corrected)
    rejected = {verdict: 'rejected', reason: 'Actual total999 contradicts supplied record30.', evidence: ['Independent original input has r3=30.'], criteria: hybrid_verdicts(first: 'fail'), activationAudit: {accepted: false, reason: 'Incorrect actual business result.'}}
    accepted = {verdict: 'accepted', reason: 'Replacement exact facts and text satisfy original criteria.', evidence: ['Exact corrected30.'], criteria: hybrid_verdicts, activationAudit: {accepted: true, reason: 'Must not activate after fallback.'}}
    rule('HybridAssessment', response: rejected, variants: [{contains: '"resultProvenance":"fallback"', response: accepted}])
    id = submit('whole-correction', {prefix: 'Feed', force_wrong: true})
    result = delivered(id)
    code = result.fetch('codeExecution')
    expect(code.dig('runtimeResult', 'actual', 'value', 'total')).to eq(999)
    expect(code.dig('hybridResult', 'facts', 'total')).to eq(30)
    expect(code.fetch('operations').count { |op| op['purpose'] == 'code_execution' }).to eq(1)
    expect(code.fetch('operations').count { |op| op['purpose'] == 'hybrid_explanation' }).to eq(1)
    expect(code.dig('activation', 'status')).to eq('not_activated')
    expect(routine['activeCode']).to be_nil
    whole = context(fallback_calls('WholeFallback').last)
    expect(whole.fetch('contract').fetch('completionCriteria')).to eq(FeedCodeFixtures::CRITERIA)
    expect(whole.fetch('externalExecution').fetch('read').fetch('newIds')).to eq(['r3'])
    expect(whole.fetch('remainingMilliseconds')).to be_between(1, 1_799_999)
    assessment = context(host.calls.select { |call| call['node'] == 'HybridAssessment' }.last)
    expect(assessment.fetch('resultText')).to eq(corrected.fetch(:text))
    expect(assessment.dig('hybridResult', 'facts', 'total')).to eq(30)
    expect(fallback_calls('WholeFallback').length).to eq(1)
    expect(@peer.deliveries.length).to eq(2)
  end

  it 'retains an unsuccessful fallback expense and refuses escalation or replay after restart' do
    fallback_setup
    configure_fallback('LocalFallback', {resolved: false, reason: 'The retained evidence is insufficient.'})
    id = submit('failed-fallback', {prefix: 'Feed', force_failure: true})
    result = settled(id)
    expect(result.dig('codeExecution', 'fallbacks', 'calculate', 'status')).to eq('failed')
    operation = result.fetch('codeExecution').fetch('operations').find { |op| op['purpose'] == 'code_local_fallback' }
    expect(operation.dig('evidence', 'usage')).to include('inputTokens' => 83, 'outputTokens' => 29)
    expect(result.dig('outcome', 'status')).not_to eq('Completed')
    expect(fallback_calls('WholeFallback')).to be_empty
    count = host.calls.length
    host.restart
    expect(host.request('POST', "/api/agents/#{@owner}/turns/#{id}/retry", {}).first).to eq(409)
    expect(submit('failed-fallback', {prefix: 'Feed', force_failure: true})).to eq(id)
    expect(host.calls.length).to eq(count)
    expect(@peer.deliveries.length).to eq(1)
    expect(routine.fetch('sourceState').fetch('cursor')).to eq('c2')
  end

  it 'uses only the remaining occurrence budget and preserves Stop without accepting a late fallback' do
    fallback_setup
    configure_fallback('LocalFallback', response_for([{id: 'r3', value: 30}], 'Feed: r3 = 30. Total: 30.'), block: true, gate: 'held-fallback')
    id = submit('stopped-fallback', {prefix: 'Feed', force_failure: true})
    call = Contract.wait { fallback_calls('LocalFallback').first }
    expect(context(call).fetch('remainingMilliseconds')).to be_between(1, 1_799_999)
    host.api('POST', "#{path}/stop", operation: 'stop-fallback', revision: 1, run: id)
    host.release('held-fallback')
    result = settled(id)
    expect(result.fetch('status')).to eq('stopped')
    expect(result.dig('codeExecution', 'fallbacks', 'calculate', 'status')).to eq('interrupted')
    expect(result.dig('codeExecution', 'completedViaFallback')).not_to be(true)
    expect(fallback_calls('WholeFallback')).to be_empty
    expect(@peer.deliveries.length).to eq(1)
    expect(routine['activeCode']).to be_nil
    expect(routine.fetch('sourceState').fetch('cursor')).to eq('c2')
  end

  it 'shows fallback completion separately from failed automation while retaining healthy calculation', :desktop do
    fallback_setup
    rule('HybridExplanation', response: {unexpected: 'malformed useful text'}, variants: [])
    configure_fallback('WholeFallback', response_for([{id: 'r3', value: 30}], 'Feed: r3 = 30. Total: 30.'))
    id = submit('explanation-replacement')
    result = delivered(id)
    expect(result.dig('codeExecution', 'runtimeResult', 'actual', 'value', 'total')).to eq(30)
    expect(result.fetch('codeExecution').fetch('faults')).to include(include('scope' => 'explanation'))
    expect(result.fetch('codeExecution').fetch('operations').count { |op| op['purpose'] == 'code_execution' }).to eq(1)
    start_desktop
    desktop.click('Open FeedCodeOwner')
    desktop.click('Work menu')
    desktop.click('Automation menu')
    desktop.click('Routines')
    desktop.click('Checked feed summary')
    desktop.see('Completed through fallback')
    desktop.see('Automation needs repair; successful fallback does not repair its implementation.')
  end
end
