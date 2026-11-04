require_relative 'support/host'

RSpec.describe 'Authorized personal Workflow agent steps' do
  ROOT_CRITERIA = ['Return the combined confirmed result.'].freeze
  STEP_CRITERIA = ['Save the requested intermediate result.'].freeze
  RESULT = {status: 'ok', value: 'confirmed'}.freeze
  before do
    @host = Host.new(start: false, environment: {
      'SAPIENS_GENERATED_DOCKER' => File.expand_path('~/.orbstack/bin/docker'),
      'SAPIENS_GENERATED_SOCKET' => 'unix://' + File.expand_path('~/.orbstack/run/docker.sock')})
    host.configure(version: 'codex-cli 0.162.1', mode: 'Exec', model_catalog: [{slug: 'gpt-6-sol', shell_type: 'unified_exec', apply_patch_tool_type: 'freeform', experimental_supported_tools: [], tool_mode: 'direct', supports_search_tool: false, supported_reasoning_levels: [{effort: 'high', description: 'high'}]}])
    host.start
    @owner = chief
    @sequence = 0
    rules('Execution' => {response: outcome(ROOT_CRITERIA, 'Original observation.'), variants: [agent_rule]},
      'PreparationReview' => {response: {accepted: true, reason: 'Independent complete results agree.'}},
      'CodeAssessment' => {response: {verdict: 'accepted', reply: 'Combined confirmed result.', reason: 'The complete actual result satisfies the original criteria.', evidence: ['Whole retained result.'], acceptance: {status: 'Completed', satisfiedCriteria: ROOT_CRITERIA, artifacts: []}, activationAudit: {accepted: true, reason: 'Own independent checks and authorized actual occurrence agree.'}}},
      'WorkflowStep' => {response: {value: true, reason: 'Retained local outcome was accepted.'}})
  end
  after do
    if RSpec.current_example.exception && @host
      prefix = '/tmp/sapiens20b-failure-' + RSpec.current_example.metadata[:line_number].to_s
      File.write(prefix + '-state.json', JSON.pretty_generate(host.state)) rescue nil
      File.write(prefix + '-calls.json', JSON.pretty_generate(host.calls)) rescue nil
      FileUtils.cp(File.join(host.root, 'corpora.sqlite3'), prefix + '.sqlite3') rescue nil
      File.write(prefix + '-stderr.txt', host.process.stderr) rescue nil
    end
    @host&.close
  end
  def rules(changes); host.configure(rules: host.provider.fetch('rules', {}).merge(changes.transform_values { |rule| {usage: {input_tokens: 19, output_tokens: 9}}.merge(rule) })); end
  def outcome(criteria, reply); decision('Outcome', reply: reply, outcome: {status: 'Completed', satisfiedCriteria: criteria, artifacts: []}); end
  def tool_events(pending: false)
    item = {id: 'local-tool', type: 'command_execution', command: 'write the requested local intermediate artifact'}
    [{type: 'item.started', item: item.merge(status: 'in_progress')}] + (pending ? [] : [{type: 'item.completed', item: item.merge(status: 'completed', exit_code: 0)}])
  end
  def agent_rule(**extra)
    {contains: '"workflowAgent":', response: outcome(STEP_CRITERIA, 'Saved intermediate result.'), actions: [{write: 'agent-result.txt', content: 'confirmed intermediate artifact'}], events: tool_events}.merge(extra)
  end
  def agent_node(next_node, recipient: nil)
    node = {kind: 'agent', instruction: 'Save the requested intermediate result within the original assignment.', criteria: STEP_CRITERIA, input: {input: true}, next: next_node}
    node[:recipient] = {literal: recipient} if recipient
    node
  end
  def sequential_graph
    {entry: 'act', nodes: {
      act: agent_node('judge'),
      judge: {kind: 'model', instruction: 'Return a strict Boolean with a reason based on the retained local result.', input: {output: 'act'}, next: 'choice'},
      choice: {kind: 'branch', input: {output: 'judge'}, true: 'checkpoint', false: 'reject'},
      checkpoint: {kind: 'defer', input: {object: {afterMilliseconds: {input: true, pointer: '/delay'}, progress: {literal: 'Tool result retained before final completion'}, value: {literal: RESULT}}}, next: 'finish'},
      finish: {kind: 'finish', input: {output: 'checkpoint'}},
      reject: {kind: 'finish', input: {literal: {status: 'rejected', error: 'Unconfirmed local result'}}}}}
  end
  def parallel_graph(left, right)
    {entry: 'split', nodes: {
      split: {kind: 'fork', children: ['left', 'right'], join: 'joined'},
      left: agent_node('joined', recipient: left), right: agent_node('joined', recipient: right),
      joined: {kind: 'join', fork: 'split', input: {array: [{output: 'left'}, {output: 'right'}]}, next: 'finish'},
      finish: {kind: 'finish', input: {literal: RESULT}}}}
  end
  def path(routine); "/api/agents/#{@owner}/routines/#{routine}"; end
  def definition(id); host.state.fetch('personal_routines').find { |r| r['id'] == id }; end
  def detail(routine, run); host.api('GET', "#{path(routine)}/runs/#{run}"); end
  def run(routine, operation, **input)
    host.api('POST', "#{path(routine)}/run", operation: routine + ':' + operation, revision: definition(routine)['revision'], inputs: JSON.generate(input)).fetch('run')
  end
  def prepare(graph)
    stub = {reply: 'Saved intermediate result.', outcome: {status: 'Completed', satisfiedCriteria: STEP_CRITERIA, artifacts: []}, author: 'fixture-only'}
    fixtures = [1, 2, 3].map { |n| {input: {example: n, delay: 0}, expected: RESULT, modelStubs: {act: stub, left: stub, right: stub, judge: {value: true, reason: 'Independent accepted intermediate result.'}}} }
    rules('PreparationFixtures' => {response: {fixtures: fixtures}}, 'PreparationGeneration' => {response: {workflow: graph}})
    @sequence += 1
    routine = host.api('POST', "/api/agents/#{@owner}/routines", operation: 'create-' + @owner + @sequence.to_s, title: 'Agent result', goal: 'Return the combined confirmed result within the accepted assignment.', criteria: ROOT_CRITERIA).fetch('routine')
    host.turn(run(routine, 'observe', example: 0, delay: 0))
    candidate = Contract.wait(timeout: 90) do
      p = definition(routine)['preparation']
      raise p.inspect if p && %w[failed interrupted stopped unsuitable].include?(p['status'])
      p if p && p['status'] == 'awaiting_occurrence'
    end
    expect(agent_calls).to be_empty if @sequence == 1
    [routine, candidate]
  end
  def agent_calls; host.calls.select { |c| c['node'] == 'Execution' && c['prompt'].include?('"workflowAgent":') }; end
  def until_detail(routine, identity, &block)
    Contract.wait(timeout: 65) do
      value = detail(routine, identity)
      block.call(value) ? value : nil
    end
  end

  it 'uses ordinary tools once, branches on the local result, restarts after completion and activates only the whole verified result' do
    routine, candidate = prepare(sequential_graph)
    expect(candidate.dig('trial')).to be_nil
    identity = run(routine, 'actual', delay: 8000)
    waiting = until_detail(routine, identity) { |d| d['workflowWaits'].any? { |w| w['status'] == 'waiting' } }
    expect(waiting['outcome']).to be_nil
    expect(waiting.dig('codeExecution', 'workflowRun', 'nodes', 'act', 'output', 'outcome', 'status')).to eq('Completed')
    expect(waiting.dig('codeExecution', 'workflowRun', 'nodes', 'choice', 'selected')).to eq('checkpoint')
    expect(waiting.fetch('routineEffectUnknown')).to be_nil
    expect(agent_calls.length).to eq(1)
    actor = agent_calls.first.fetch('owner')
    expect(File.read(File.join(host.workspace(actor), 'agent-result.txt'))).to eq('confirmed intermediate artifact')
    expect(agent_calls.first.fetch('args')).to include('--dangerously-bypass-approvals-and-sandbox')
    expect(host.state.fetch('run_activity').keys).not_to include(identity)
    host.restart
    restored = detail(routine, identity)
    expect(restored.dig('codeExecution', 'reference')).to eq(waiting.dig('codeExecution', 'reference'))
    expect(restored.dig('codeExecution', 'workflowDeadline')).to eq(waiting.dig('codeExecution', 'workflowDeadline'))
    expect(restored.dig('workflowParallel', 'activeMilliseconds')).to eq(waiting.dig('workflowParallel', 'activeMilliseconds'))
    final = until_detail(routine, identity) { |d| d['status'] == 'done' }
    expect(final.dig('outcome', 'satisfiedCriteria')).to eq(ROOT_CRITERIA)
    expect(final.dig('codeExecution', 'activation', 'status')).to eq('activated')
    expect(agent_calls.length).to eq(1)
    expect(File.read(File.join(host.workspace(actor), 'agent-result.txt'))).to eq('confirmed intermediate artifact')
    host.restart
    expect(agent_calls.length).to eq(1)
  end

  it 'joins different authorized Sapis under one resource, then Stops a later child while retaining its completed sibling' do
    left, right = host.agent('Left'), host.agent('Right')
    rules("#{left}:Execution" => agent_rule(after_events_gate: 'left-has-effect', child: true), "#{right}:Execution" => agent_rule)
    routine, = prepare(parallel_graph(left, right))
    identity = run(routine, 'parallel', delay: 0)
    partial = until_detail(routine, identity) { |d| d.dig('codeExecution', 'workflowRun', 'nodes', 'right', 'status') == 'completed' }
    expect(partial.dig('codeExecution', 'workflowRun', 'nodes')).not_to have_key('joined')
    expect(partial['outcome']).to be_nil
    expect(partial.dig('workflowParallel', 'resourceStatus')).to eq('reserved')
    expect(agent_calls.map { |c| c['owner'] }).to match_array([left, right])
    Contract.wait { File.exist?(File.join(host.root, 'left-has-effect-started')) }
    host.release('left-has-effect')
    completed = until_detail(routine, identity) { |d| d['status'] == 'done' }
    expect(completed.dig('codeExecution', 'workflowRun', 'nodes', 'joined', 'status')).to eq('completed')
    outputs = completed.dig('codeExecution', 'workflowRun', 'nodes', 'joined', 'output')
    expect(outputs.map { |o| o['author'] }).to eq([left, right])
    expect(completed.dig('outcome', 'satisfiedCriteria')).to eq(ROOT_CRITERIA)
    rules("#{left}:Execution" => agent_rule(after_events_gate: 'left-stop-effect', child: true))
    identity = run(routine, 'parallel-stop', delay: 0, example: 4)
    partial = until_detail(routine, identity) { |d| d.dig('codeExecution', 'workflowRun', 'nodes', 'right', 'status') == 'completed' }
    Contract.wait { File.exist?(File.join(host.root, 'left-stop-effect-started')) }
    host.api('POST', "#{path(routine)}/stop", operation: 'stop-parallel', revision: definition(routine)['revision'], run: identity)
    host.release('left-stop-effect')
    stopped = until_detail(routine, identity) { |d| d['status'] == 'stopped' }
    expect(stopped.dig('codeExecution', 'workflowRun', 'nodes', 'right')).to eq(partial.dig('codeExecution', 'workflowRun', 'nodes', 'right'))
    host.restart
    expect(agent_calls.length).to eq(4)
    expect(detail(routine, identity).dig('codeExecution', 'workflowRun', 'nodes')).not_to have_key('joined')
  end

  it 'refuses unauthorized recipients and holds unknown tool outcomes across restart without retrying the source' do
    other = host.agent('Unrelated')
    @owner = host.agent('Requester')
    routine, = prepare(parallel_graph(other, @owner))
    refused = run(routine, 'forbidden-route', delay: 0)
    until_detail(routine, refused) { |d| %w[failed stopped interrupted].include?(d['status']) }
    expect(agent_calls.none? { |c| c['owner'] == other }).to be(true)
    # A fresh independent candidate exercises uncertain capture, without granting retry to a failed trial.
    @owner = chief
    routine, = prepare(sequential_graph)
    rules('Execution' => {response: outcome(ROOT_CRITERIA, 'Original observation.'), variants: [agent_rule(events: tool_events(pending: true))]})
    uncertain = run(routine, 'unknown-tool', delay: 0)
    failed = until_detail(routine, uncertain) { |d| %w[failed stopped interrupted].include?(d['status']) }
    expect(failed['outcome']).to be_nil
    expect(failed['routineEffectUnknown']).not_to be_nil
    expect(failed.dig('codeExecution', 'workflowRun', 'nodes')).not_to have_key('judge')
    count = agent_calls.length
    host.restart
    pulse = host.state.dig('pulse', 'numbers', 'bpm60')
    Contract.wait { host.state.dig('pulse', 'numbers', 'bpm60') > pulse + 2 }
    expect(agent_calls.length).to eq(count)
    expect(detail(routine, uncertain)['outcome']).to be_nil
  end
end
