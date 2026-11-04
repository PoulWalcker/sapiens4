require 'sqlite3'
require 'yaml'
require_relative 'support/candidate_fixtures'
require_relative 'support/desktop'

RSpec.describe 'Owner-chosen standalone prepared execution' do
  include CandidateFixtures
  USAGE = {input_tokens: 19, output_tokens: 9}.freeze
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
    host.configure(version: 'codex-cli 0.162.1', mode: 'Exec', model_catalog: [{slug: 'gpt-6-sol', shell_type: 'unified_exec', apply_patch_tool_type: 'freeform', experimental_supported_tools: [], tool_mode: 'direct', supports_search_tool: false, supported_reasoning_levels: [{effort: 'high', description: 'high'}]}])
    host.start
    host.control(chief, 'corpora_update', revision: host.state.fetch('corpora').fetch('revision'), doctor_id: nil)
    @owner = host.agent('PreparedOwner')
    rules('ReadyToWork' => {response: prepared_task(observed_input), usage: USAGE},
      'Execution' => {response: decision('ComposeWorkflow', operation: 'initial-version'), usage: USAGE},
      'PreparationFixtures' => {response: {fixtures: [independent_fixtures[0], independent_fixtures[2], independent_fixtures[4]]}, usage: USAGE},
      'PreparationGeneration' => {response: {workflow: graph}, usage: USAGE},
      'PreparationReview' => {response: {accepted: true, reason: 'Actual isolated results match independent examples.'}, usage: USAGE},
      'CodeAssessment' => {response: accepted_result, usage: USAGE})
  end
  after do
    if RSpec.current_example.exception && @host
      directory = ENV.fetch('SAPIENS_STANDALONE_EVIDENCE', File.join(Contract::ROOT, 'tmp'))
      FileUtils.mkdir_p(directory)
      stem = RSpec.current_example.metadata.fetch(:line_number).to_s
      File.write(File.join(directory, "standalone-#{stem}-state.json"), JSON.pretty_generate(host.state))
      File.write(File.join(directory, "standalone-#{stem}-calls.json"), JSON.pretty_generate(host.calls))
      File.write(File.join(directory, "standalone-#{stem}-tasks.json"), JSON.pretty_generate(tasks))
      File.write(File.join(directory, "standalone-#{stem}-accessibility.json"), JSON.pretty_generate(@desktop.tree)) if @desktop
    end
    @desktop&.close
    @host&.close
  end
  def rules(changes); host.configure(rules: host.provider.fetch('rules', {}).merge(changes)); end
  def contract(input)
    {objective: CandidateFixtures::GOAL, inputs: [JSON.generate(input)], expectedOutputs: ['An exact validated sales result.'], completionCriteria: CandidateFixtures::CRITERIA}
  end
  def prepared_task(input); decision('TaskPrepared', taskType: 'work', specification: contract(input)); end
  def accepted_result
    {verdict: 'accepted', reply: 'Sales computed against all original criteria.', reason: 'The actual complete sales result matches the accepted input.', evidence: ['Retained actual calculation and original criteria.'], acceptance: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}, activationAudit: {accepted: true, reason: 'Own independent examples and actual authorized result support this exact version.'}}
  end
  def graph(suffix = '')
    {entry: 'calculate', nodes: {calculate: {kind: 'script', source: candidate_source + suffix, input: {input: true}, next: 'finish'}, finish: {kind: 'finish', input: {output: 'calculate'}}}}
  end
  def versions; host.api('GET', "/api/agents/#{@owner}/workflows"); end
  def tasks; YAML.safe_load(host.api('GET', "/api/agents/#{@owner}/tasks")).fetch('tasks'); end
  def task(identity); tasks.find { |row| row['workloadId'] == identity }; end
  def provider_calls(node); host.calls.select { |call| call['node'] == node }; end
  def context(call); JSON.parse(call.fetch('prompt').split("\nContext:\n", 2).last); end
  def second_input; {csv: CandidateFixtures::HELDOUT, start: '2026-12-05', end: '2026-12-06'}; end
  def wait_version(parent, status = 'awaiting_occurrence')
    Contract.wait(timeout: 90, description: "standalone candidate #{status}") do
      value = versions.find { |row| row.dig('parent', 'workload') == parent }
      raise value.inspect if value && %w[failed stopped stale interrupted].include?(value['status']) && status == 'awaiting_occurrence'
      value if value && value['status'] == status
    end
  end
  def edit_work(identity)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.busy_timeout = 5000
      value = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', identity))
      yield value
      db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(value), identity])
    end
  end

  it 'autonomously checks and runs versions, preserves in-flight pins and settles each original ordinary Task without user actions' do
    rules('CodeAssessment' => {response: accepted_result, usage: USAGE, block: true, gate: 'actual-audit'})
    original = host.submit(@owner, 'Compute the supplied sales with all requested validation.')
    first = wait_version(original)
    Contract.wait { provider_calls('CodeAssessment').length == 1 }
    expect(task(original)['outcome']).to be_nil
    expect(task(original).dig('execution', 'state')).to eq('Queued')
    expect(first.fetch('checks')).to all(include('matched' => true))
    expect(first.dig('trial', 'status')).to eq('claimed')
    first_child = first.dig('trial', 'run')
    pin = task(first_child).dig('executionBinding', 'binding')
    expect(pin.dig('source', 'kind')).to eq('task_decision')
    expect(pin.fetch('reference')).to eq(first.fetch('reference'))
    expect(task(first_child).fetch('standaloneRun')).not_to have_key('routine')
    expect(host.state.fetch('personal_routines')).to be_empty

    rules('Execution' => {response: decision('RunWorkflow', operation: 'conflicting-first-trial', reference: first.fetch('reference')), usage: USAGE})
    conflict = host.submit(@owner, 'Compute another compatible input without stealing an existing verification.')
    host.turn(conflict, 'failed', timeout: 40)
    expect(versions.find { |row| row['id'] == first['id'] }.dig('trial', 'run')).to eq(first_child)
    expect(provider_calls('CodeAssessment').length).to eq(1)

    rules('ReadyToWork' => {response: prepared_task(second_input), usage: USAGE},
      'Execution' => {response: decision('ComposeWorkflow', operation: 'improve-version', previous: first.fetch('reference')), usage: USAGE},
      'PreparationGeneration' => {response: {workflow: graph("\n# Independently checked next version.\n")}, usage: USAGE})
    next_original = host.submit(@owner, 'Compute the next supplied sales, preserving every original requirement.')
    second = wait_version(next_original)
    Contract.wait { provider_calls('CodeAssessment').length == 2 }
    expect(second.fetch('reference')).not_to eq(first.fetch('reference'))
    expect(second.dig('source', 'workload')).to eq(original)
    expect(task(first_child).dig('executionBinding', 'binding')).to eq(pin)
    expect(task(original)['outcome']).to be_nil
    expect(second.fetch('checks')).to all(include('matched' => true))
    expect(first.fetch('maintenanceBudget').fetch('limitMilliseconds')).to eq(1_800_000)

    host.release('actual-audit')
    host.turn(original, 'done', timeout: 60)
    host.turn(next_original, 'done', timeout: 60)
    [original, next_original].each do |identity|
      expect(task(identity).dig('outcome', 'status')).to eq('Completed')
      expect(task(identity).dig('task', 'specification', 'completionCriteria')).to eq(CandidateFixtures::CRITERIA)
      expect(task(identity).dig('executionBinding', 'binding', 'mode')).to eq('ordinary')
      expect(task(identity).fetch('standaloneResult').fetch('child')).not_to eq(identity)
    end
    expect(versions.map { |row| row.dig('trial', 'status') }).to eq(%w[activated activated])
    expect(context(provider_calls('CodeAssessment')[0]).fetch('input')).to eq(JSON.parse(JSON.generate(observed_input)))
    expect(context(provider_calls('CodeAssessment')[1]).fetch('input')).to eq(JSON.parse(JSON.generate(second_input)))

    rules('Execution' => {response: decision('RunWorkflow', operation: 'reuse-exact-version', reference: first.fetch('reference')), usage: USAGE})
    reused = host.submit(@owner, 'Compute these sales using an already verified compatible execution.')
    host.turn(reused, 'done', timeout: 60)
    expect(task(reused).dig('standaloneResult', 'reference')).to eq(first.fetch('reference'))
    expect(provider_calls('PreparationGeneration').length).to eq(2)
    expect(context(provider_calls('CodeAssessment').last).fetch('initialTrial')).to be(false)
    ordinary = host.agent('OrdinaryReply')
    rules("#{ordinary}:Assessing" => {response: decision('FitsSpecialization', mode: 'Repl', reply: 'An ordinary answer.'), usage: USAGE})
    host.turn(host.submit(ordinary, 'Give an ordinary short answer.'))
    calls = host.calls.length
    before = versions
    host.restart
    expect(versions).to eq(before)
    expect(host.calls.length).to eq(calls)
    expect(host.api('GET', "/api/agents/#{@owner}/tasks?format=json").fetch('tasks').map { |row| row['id'] }).not_to include(first_child)
  end

  it 'retains business effects, propagates Stop and reports exhausted preparation or mismatched child criteria without replay' do
    tool = {id: 'saved-effect', type: 'command_execution', command: 'write retained fixture artifact'}
    events = [{type: 'item.started', item: tool.merge(status: 'in_progress')}, {type: 'item.completed', item: tool.merge(status: 'completed', exit_code: 0)}]
    unresolved = decision('Outcome', reply: 'Retained work needs further evidence.', outcome: {status: 'Unresolved', satisfiedCriteria: [], artifacts: []})
    rules('Execution' => {response: decision('ComposeWorkflow', operation: 'unsafe-handoff'), usage: USAGE, events: events, actions: [{write: 'retained-effect.txt', content: 'Keep this completed action.'}], variants: [{contains: '"workflowRefused":', response: unresolved, events: [], actions: []}]})
    effect = host.submit(@owner, 'Preserve completed actions while handling the supplied sales.')
    host.turn(effect, 'done', timeout: 40)
    expect(task(effect).dig('outcome', 'status')).to eq('Unresolved')
    expect(provider_calls('Execution').length).to eq(2)
    expect(provider_calls('PreparationFixtures')).to be_empty
    expect(File.read(File.join(host.workspace(@owner), 'retained-effect.txt'))).to eq('Keep this completed action.')
    expect(task(effect).fetch('decisions').any? { |row| row['error'].to_s.include?('handoff refused') }).to be(true)

    rules('Execution' => {response: decision('ComposeWorkflow', operation: 'stop-original'), usage: USAGE},
      'PreparationReview' => {response: {accepted: true, reason: 'A late review cannot override Stop.'}, usage: USAGE, block: true, gate: 'stopped-review'})
    stopped = host.submit(@owner, 'Complete the supplied sales, unless this original task is stopped.')
    Contract.wait(timeout: 90) { provider_calls('PreparationReview').length == 1 }
    expect(task(stopped)['outcome']).to be_nil
    host.api('POST', "/api/agents/#{@owner}/turns/#{stopped}/cancel")
    host.turn(stopped, %w[cancelled interrupted])
    Contract.wait(timeout: 30) { versions.find { |row| row.dig('parent', 'workload') == stopped && %w[stale stopped].include?(row['status']) } }
    expect(provider_calls('CodeAssessment')).to be_empty
    host.release('stopped-review')
    count = host.calls.length
    host.restart
    expect(host.calls.length).to eq(count)
    expect(task(stopped)['outcome']).to be_nil

    rules('PreparationReview' => {response: {accepted: true, reason: 'Own checked candidate.'}, usage: USAGE},
      'Execution' => {response: decision('ComposeWorkflow', operation: 'no-refill'), usage: USAGE, block: true, gate: 'before-budget', variants: [{contains: '"workflowResult":', response: unresolved, block: false}]})
    exhausted = host.submit(@owner, 'Respect the finite preparation resource for this original task.')
    Contract.wait { provider_calls('Execution').any? { |call| call['agency_run'] == exhausted } }
    # Finite saved-resource fixture; no thirty-minute burn and no new allowance.
    edit_work(exhausted) { |work| work['maintenanceBudget'] = {limitMilliseconds: 1_800_000, spentMilliseconds: 1_800_000, reservations: []} }
    host.release('before-budget')
    host.turn(exhausted, 'done', timeout: 40)
    expect(task(exhausted).dig('outcome', 'status')).to eq('Unresolved')
    expect(provider_calls('PreparationFixtures').length).to eq(1)
    exhausted_version = versions.find { |row| row.dig('parent', 'workload') == exhausted }
    expect(exhausted_version.fetch('status')).to eq('failed')
    expect(exhausted_version.fetch('error')).to include('allowance is exhausted')
    budget = exhausted_version.fetch('maintenanceBudget')
    expect(budget).to include('limitMilliseconds' => 1_800_000, 'spentMilliseconds' => 1_800_000, 'reservations' => [])

    wrong = contract(observed_input).merge(completionCriteria: ['An unrelated easier criterion.'])
    rules('Execution' => {response: decision('ComposeWorkflow', operation: 'criteria-mismatch'), usage: USAGE, variants: [{contains: '"workflowResult":', response: unresolved}]},
      'ReadyToWork' => {response: prepared_task(observed_input), usage: USAGE, variants: [{contains: 'internally authorized execution of the ORIGINAL accepted Task', response: decision('TaskPrepared', taskType: 'work', specification: wrong)}]})
    mismatch = host.submit(@owner, 'Keep every original criterion while preparing this computation.')
    host.turn(mismatch, 'done', timeout: 90)
    expect(task(mismatch).dig('outcome', 'status')).to eq('Unresolved')
    expect(task(mismatch).dig('task', 'specification', 'completionCriteria')).to eq(CandidateFixtures::CRITERIA)
    expect(provider_calls('CodeAssessment')).to be_empty
    failed = versions.find { |row| row.dig('parent', 'workload') == mismatch }
    expect(failed.dig('trial', 'status')).to eq('claimed')
    expect(task(failed.dig('trial', 'run')).dig('execution', 'state')).to eq('Failed')
    before = host.calls.length
    host.restart
    expect(host.calls.length).to eq(before)
  end

  def click_control(label)
    roles = %w[AXButton AXCheckBox AXRadioButton AXLink AXDisclosureTriangle AXPopUpButton]
    @desktop.control('scroll-to', '--path', @desktop.find(label, role: roles).fetch('path'))
    @desktop.click_node(@desktop.find(label, role: roles))
  end

  def expand_task
    label = Regexp.new(Regexp.escape(CandidateFixtures::GOAL))
    @desktop.control('scroll-to', '--path', @desktop.find(label, role: 'AXDisclosureTriangle').fetch('path'))
    @desktop.click_node(@desktop.find(label, role: 'AXDisclosureTriangle'))
  end

  it 'shows autonomous preparation, actual verification and original-task Stop in the native app without Compose or Run clicks', :desktop do
    rules('PreparationReview' => {response: {accepted: true, reason: 'Own actual fixture checks.'}, usage: USAGE, block: true, gate: 'visible-preparation'})
    original = host.submit(@owner, 'Complete the supplied sales and preserve all validation requirements.')
    Contract.wait(timeout: 90) { provider_calls('PreparationReview').length == 1 }
    @desktop = Desktop.new(host.url)
    @desktop.click('Open PreparedOwner')
    click_control('Work menu')
    click_control('Tasks')
    @desktop.see('Task history')
    expand_task
    @desktop.see('Prepared execution')
    @desktop.see('Preparation: review')
    @desktop.see('Checks: 3/3')
    @desktop.see('Stop this task')
    host.release('visible-preparation')
    host.turn(original, 'done', timeout: 60)
    click_control('Task history')
    click_control('Executed')
    @desktop.see('Verification: activated')
    @desktop.see('Result: Completed')

    rules('CodeAssessment' => {response: accepted_result, usage: USAGE, block: true, gate: 'stop-native'})
    stopped = host.submit(@owner, 'Complete another supplied sales task, unless it is stopped.')
    Contract.wait(timeout: 90) { provider_calls('CodeAssessment').length == 2 }
    click_control('Task history')
    click_control('Planned')
    expand_task
    @desktop.see('Prepared execution')
    click_control('Stop this task')
    host.turn(stopped, %w[cancelled interrupted])
    expect(task(stopped)['outcome']).to be_nil
    click_control('Task history')
    click_control('Executed')
    @desktop.see('Preparation: stale')
  end
end
