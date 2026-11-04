require 'yaml'
require_relative 'support/host'

RSpec.describe 'Explicit external Workflow results' do
  CRITERIA = ['Return the confirmed result.'].freeze
  RESULT = {status: 'ok', value: 42}.freeze
  before do
    @host = Host.new(start: false, environment: {
      'SAPIENS_GENERATED_DOCKER' => File.expand_path('~/.orbstack/bin/docker'),
      'SAPIENS_GENERATED_SOCKET' => 'unix://' + File.expand_path('~/.orbstack/run/docker.sock')})
    host.configure(version: 'codex-cli 0.162.1', mode: 'Exec', model_catalog: [{slug: 'gpt-6-sol', shell_type: 'unified_exec', apply_patch_tool_type: 'freeform', experimental_supported_tools: [], tool_mode: 'direct', supports_search_tool: false, supported_reasoning_levels: [{effort: 'high', description: 'high'}]}])
    host.start
    @owner = host.agent('ResultOwner')
    rules('Execution' => {response: decision('Outcome', reply: 'Confirmed result.', outcome: {status: 'Completed', satisfiedCriteria: CRITERIA, artifacts: []})},
      'PreparationReview' => {response: {accepted: true, reason: 'Independent complete results match.'}},
      'CodeAssessment' => {response: {verdict: 'accepted', reply: 'Confirmed result.', reason: 'Actual complete result matches.', evidence: ['Retained result.'], acceptance: {status: 'Completed', satisfiedCriteria: CRITERIA, artifacts: []}, activationAudit: {accepted: true, reason: 'Independent checks and actual occurrence agree.'}}},
      'WorkflowStep' => {response: RESULT})
  end
  after do
    if RSpec.current_example.exception && @host
      FileUtils.cp(File.join(host.root, 'corpora.sqlite3'), '/tmp/sapiens22b-failure.sqlite3') rescue nil
      FileUtils.cp_r(File.join(host.root, 'agentpy', 'agents'), '/tmp/sapiens22b-failure-agents') rescue nil
      File.write('/tmp/sapiens22b-failure-stderr.txt', host.process.stderr) rescue nil
      File.write('/tmp/sapiens22b-failure-state.json', JSON.pretty_generate(host.state)) rescue nil
      File.write('/tmp/sapiens22b-failure-calls.json', JSON.pretty_generate(host.calls)) rescue nil
    end
    @host&.close
  end
  def rules(changes); host.configure(rules: host.provider.fetch('rules', {}).merge(changes.transform_values { |rule| {usage: {input_tokens: 19, output_tokens: 9}}.merge(rule) })); end
  def path(routine); "/api/agents/#{@owner}/routines/#{routine}"; end
  def definition(id); host.state.fetch('personal_routines').find { |row| row['id'] == id }; end
  def detail(routine, run); host.api('GET', "#{path(routine)}/runs/#{run}"); end
  def run(routine, operation, input)
    host.api('POST', "#{path(routine)}/run", operation: routine + ":" + operation, revision: definition(routine).fetch('revision'), inputs: JSON.generate(input)).fetch('run')
  end
  def prepare(title, graph, input)
    fixtures = [1, 2, 3].map { |n| {input: input.merge(example: n), expected: RESULT, modelStubs: {await: {source: input[:source], outcome: {status: 'Completed'}, value: nil}, consume: RESULT}} }
    rules('PreparationFixtures' => {response: {fixtures: fixtures}}, 'PreparationGeneration' => {response: {workflow: graph}})
    @sequence = (@sequence || 0) + 1
    id = host.api('POST', "/api/agents/#{@owner}/routines", operation: title + @sequence.to_s, title: title, goal: 'Return the confirmed result.', criteria: CRITERIA).fetch('routine')
    host.turn(run(id, 'observe', input))
    Contract.wait(timeout: 90) do
      p = definition(id)['preparation']
      raise p.inspect if p && %w[failed stopped unsuitable interrupted].include?(p['status'])
      p if p && p['status'] == 'awaiting_occurrence'
    end
    id
  end
  def source(delay)
    graph = {entry: 'delay', nodes: {delay: {kind: 'defer', input: {object: {afterMilliseconds: {input: true, pointer: '/delay'}, progress: {literal: 'Source awaiting its permitted time'}, value: {literal: RESULT}}}, next: 'finish'}, finish: {kind: 'finish', input: {output: 'delay'}}}}
    routine = prepare('Source', graph, {delay: 0})
    identity = run(routine, 'source-execution', {delay: delay})
    wait_record(routine, identity)
    Contract.wait(timeout: 5, description: 'source releases slot after durable wait') { !host.state.fetch('run_activity').key?(identity) }
    [routine, identity, {workload: identity, call: identity}]
  end
  def consumer(reference, input_reference: reference)
    graph = {entry: 'await', nodes: {await: {kind: 'awaitResult', input: {object: {source: (input_reference == reference ? {input: true, pointer: '/source'} : {literal: reference}), progress: {literal: 'Need the confirmed source result'}}}, next: 'consume'}, consume: {kind: 'model', instruction: 'Return the confirmed result envelope.', input: {output: 'await'}, next: 'finish'}, finish: {kind: 'finish', input: {output: 'consume'}}}}
    prepare('Consumer', graph, {source: input_reference})
  end
  def wait_record(routine, identity, status = 'waiting')
    Contract.wait(timeout: 60) do
      d = detail(routine, identity)
      row = d.fetch('workflowWaits').find { |w| w['status'] == status }
      raise d.inspect if !row && %w[failed interrupted stopped].include?(d['status'])
      row
    end
  end
  def successor_calls; host.calls.select { |call| call['node'] == 'WorkflowStep' }; end

  it 'restarts while both executions wait, retains original pins and consumes the accepted exact source once' do
    source_routine, source_run, reference = source(30000)
    consumer_routine = consumer(reference)
    identity = run(consumer_routine, 'wait-for-source', {source: reference})
    record = wait_record(consumer_routine, identity)
    before = detail(consumer_routine, identity)
    expect(record.fetch('source')).to eq(reference.transform_keys(&:to_s))
    expect(detail(source_routine, source_run).fetch('workflowWaits').first['status']).to eq('waiting')
    expect(host.state.fetch('run_activity').keys).not_to include(identity, source_run)
    expect(successor_calls).to be_empty
    host.restart
    restored = detail(consumer_routine, identity)
    expect(restored.fetch('workflowWaits')).to eq(before.fetch('workflowWaits'))
    expect(restored.dig('codeExecution', 'workflowDeadline')).to eq(before.dig('codeExecution', 'workflowDeadline'))
    # Idle resource retention is also checked through the saved reservation projection.
    expect(before.dig('workflowParallel', 'resourceStatus')).to eq('reserved')
    expect(before.dig('workflowParallel', 'activeMilliseconds')).to be_a(Integer)
    expect(restored.dig('workflowParallel', 'activeMilliseconds')).to eq(before.dig('workflowParallel', 'activeMilliseconds'))
    Contract.wait(timeout: 65) { detail(consumer_routine, identity)['status'] == 'done' }
    expect(detail(source_routine, source_run).dig('outcome', 'status')).to eq('Completed')
    completed = detail(consumer_routine, identity)
    expect(completed.fetch('workflowWaits').first.fetch('status')).to eq('completed')
    expect(completed.dig('codeExecution', 'workflowRun', 'nodes', 'await', 'output', 'outcome', 'status')).to eq('Completed')
    expect(successor_calls.length).to eq(1)
    expect(completed.dig('codeExecution', 'reference')).to eq(before.dig('codeExecution', 'reference'))
    host.restart
    pulse = host.state.dig('pulse', 'numbers', 'bpm60')
    Contract.wait { host.state.dig('pulse', 'numbers', 'bpm60') > pulse + 2 }
    expect(successor_calls.length).to eq(1)
    expect(detail(consumer_routine, identity).fetch('workflowWaits')).to eq(completed.fetch('workflowWaits'))
  end

  it 'Stops the consumer independently and refuses a stopped source across repeated wakes and restart' do
    source_routine, source_run, reference = source(600000)
    consumer_routine = consumer(reference)
    stopped = run(consumer_routine, 'stop-consumer', {source: reference})
    wait_record(consumer_routine, stopped)
    host.api('POST', "#{path(consumer_routine)}/stop", operation: 'stop', revision: definition(consumer_routine)['revision'], run: stopped)
    expect(wait_record(consumer_routine, stopped, 'cancelled')['reason']).to include('Stopped')
    expect(detail(source_routine, source_run).fetch('workflowWaits').first['status']).to eq('waiting')
    # A separate consumer has its own original resource and explicitly accepts the same reference.
    other = consumer(reference)
    identity = run(other, 'refuse-source', {source: reference})
    wait_record(other, identity)
    host.api('POST', "#{path(source_routine)}/stop", operation: 'stop-source', revision: definition(source_routine)['revision'], run: source_run)
    blocked = wait_record(other, identity, 'blocked')
    expect(blocked['reason']).to include('stopped')
    initial_routine = consumer(reference)
    initial = run(initial_routine, 'already-stopped-source', {source: reference})
    expect(wait_record(initial_routine, initial, 'blocked')['reason']).to include('stopped')
    wrong_call = reference.merge(call: 'unrelated-call')
    invalid_routine = consumer(wrong_call)
    invalid = run(invalid_routine, 'unrelated-call', {source: wrong_call})
    expect(wait_record(invalid_routine, invalid, 'blocked')['reason']).to include('authorized personal root')
    host.restart
    pulse = host.state.dig('pulse', 'numbers', 'bpm60')
    Contract.wait { host.state.dig('pulse', 'numbers', 'bpm60') > pulse + 2 }
    expect(successor_calls).to be_empty
    expect(detail(other, identity).fetch('workflowWaits').first).to eq(blocked)
    expect(detail(consumer_routine, stopped).fetch('workflowWaits').first['status']).to eq('cancelled')
  end
  it 'accepts a separately completed ordinary Task but refuses generated authority, foreign ownership and unresolved results' do
    spec = {objective: 'Return the confirmed result.', inputs: [], expectedOutputs: ['The confirmed result.'], completionCriteria: CRITERIA}
    rules('ReadyToWork' => {response: decision('TaskPrepared', taskType: 'work', specification: spec)})
    ordinary = host.submit(@owner, 'Return the confirmed result.')
    host.turn(ordinary)
    reference = {workload: ordinary, call: ordinary}
    routine = consumer(reference)
    completed = run(routine, 'ordinary-source', {source: reference})
    Contract.wait(timeout: 45) { detail(routine, completed)['status'] == 'done' }
    output = detail(routine, completed).dig('codeExecution', 'workflowRun', 'nodes', 'await', 'output')
    expect(output.dig('outcome', 'status')).to eq('Completed')
    expect(output['value']).to be_nil
    expect(successor_calls.length).to eq(1)

    absent = {workload: 'not-the-authorized-source', call: 'not-the-authorized-call'}
    generated = consumer(reference, input_reference: absent)
    refused = run(generated, 'literal-grants-no-access', {source: absent})
    expect(wait_record(generated, refused, 'blocked')['reason']).to include('explicitly authorized')

    @owner = host.agent('ForeignConsumer')
    foreign = consumer(reference)
    refused = run(foreign, 'foreign-source', {source: reference})
    expect(wait_record(foreign, refused, 'blocked')['reason']).to include('authorized personal root')
    unknown_routine = consumer({workload: 'placeholder', call: 'placeholder'})
    rules('Execution' => {response: decision('Outcome', reply: 'Still unknown.', outcome: {status: 'Unresolved', satisfiedCriteria: [], artifacts: []})})
    unresolved = host.submit(@owner, 'Return the confirmed result.')
    host.turn(unresolved)
    ref = {workload: unresolved, call: unresolved}
    refused = run(unknown_routine, 'unresolved-source', {source: ref})
    expect(wait_record(unknown_routine, refused, 'blocked')['reason']).to include('did not complete successfully')
    count = host.calls.length
    host.restart
    pulse = host.state.dig('pulse', 'numbers', 'bpm60')
    Contract.wait { host.state.dig('pulse', 'numbers', 'bpm60') > pulse + 2 }
    expect(host.calls.length).to eq(count)
    expect(successor_calls.length).to eq(1)
  end

end
