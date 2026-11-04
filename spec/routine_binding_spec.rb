require 'yaml'
require 'sqlite3'

RSpec.describe 'Pinned ordinary execution contracts' do
  before do |example|
    options = {}
    if example.metadata[:previous_release]
      previous = ENV['SAPIENS_TEST_PRE_BINDING_HOST']
      skip 'Set SAPIENS_TEST_PRE_BINDING_HOST for actual pre-binding upgrade coverage' unless previous
      @binary_directory = Dir.mktmpdir('sapiens-binding-upgrade-')
      @binary_pointer = File.join(@binary_directory, 'host-target')
      File.write(@binary_pointer, previous)
      options[:command] = [RbConfig.ruby, '-e', 'exec File.read(ENV.fetch("SAPIENS_BINDING_HOST_TARGET")).strip, *ARGV', '--']
      options[:environment] = {'SAPIENS_BINDING_HOST_TARGET' => @binary_pointer}
    end
    @host = Host.new(**options)
    @owner = host.agent('BindingOwner')
    @fields = {title: 'Comparison', goal: 'Compare the products.', criteria: ['Return a sourced comparison.']}
  end

  after do
    @host&.close
    FileUtils.remove_entry(@binary_directory) if @binary_directory && File.directory?(@binary_directory)
  end

  def create_routine
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", @fields.merge(operation: 'create')).fetch('routine')
  end

  def path
    "/api/agents/#{@owner}/routines/#{@routine}"
  end

  def command(action, operation, revision: 1, **fields)
    host.api('POST', "#{path}/#{action}", {operation: operation, revision: revision}.merge(fields))
  end

  def detail(run)
    host.api('GET', "#{path}/runs/#{run}")
  end

  def tasks(owner = @owner)
    YAML.safe_load(host.api('GET', "/api/agents/#{owner}/tasks")).fetch('tasks')
  end

  def context(call)
    JSON.parse(call.fetch('prompt').split("\nContext:\n", 2).last)
  end

  def executions
    host.calls.select { |call| call['node'] == 'Execution' }
  end

  def wait_execution
    Contract.wait(description: 'ordinary execution provider admission') { executions.last }
  end
  it 'keeps the accepted routine contract and inputs through future edits and restart' do
    create_routine
    host.configure(rules: {'Execution' => {block: true, gate: 'first-execution'}})
    run = command('run', 'first', inputs: 'Products A and B').fetch('run')
    first = wait_execution
    saved = detail(run)
    expect(saved.fetch('executionBinding')).to include('status' => 'pinned', 'inputs' => ['Products A and B'])
    expect(saved.dig('executionBinding', 'binding')).to include('schema' => 1, 'mode' => 'ordinary', 'owner' => @owner, 'source' => {'kind' => 'routine_snapshot'})
    command('edit', 'future-goal', **@fields.merge(goal: 'Compare different products.', criteria: ['Return a different comparison.']))
    expect(detail(run).fetch('snapshot')).to eq(saved.fetch('snapshot'))
    expect(detail(run).fetch('executionBinding')).to eq(saved.fetch('executionBinding'))
    expect(context(first).fetch('task').to_json).to include('Compare the products.', 'Return a sourced comparison.')
    expect(first.fetch('prompt')).to include('Products A and B')
    host.release('first-execution')
    host.turn(run)
    completed = detail(run)
    host.restart
    expect(detail(run)).to eq(completed)
    expect(executions.length).to eq(1)
    expect(completed.fetch('snapshot')).to include('goal' => 'Compare the products.', 'inputs' => 'Products A and B')
  end

  it 'keeps ordinary conversational replies separate from accepted task execution' do
    response = host.submit(@owner, 'Hello there')
    expect(host.turn(response).fetch('output')).to eq('Contract test reply')
    expect(executions).to be_empty
    expect(tasks).to be_empty
  end

  it 'retains queued admission through a goal roundtrip and restart instead of resolving the latest definition' do
    create_routine
    host.configure(rules: {'Assessing' => {block: true, gate: 'occupied'}})
    occupied = host.submit(@owner, 'Occupy the owner')
    Contract.wait { host.calls.any? { |call| call['node'] == 'Assessing' } }
    run = command('run', 'queued', inputs: 'Pinned queue input').fetch('run')
    saved = detail(run).fetch('executionBinding')
    command('edit', 'goal-b', **@fields.merge(goal: 'Different goal'))
    command('edit', 'goal-a-again', revision: 2, **@fields)
    expect(detail(run).fetch('executionBinding')).to eq(saved)
    host.api('POST', "/api/agents/#{@owner}/turns/#{occupied}/cancel")
    host.configure(rules: {})
    host.restart
    host.turn(run)
    expect(detail(run).fetch('executionBinding')).to eq(saved)
    expect(detail(run).dig('snapshot', 'contractGeneration')).to eq(1)
    expect(executions.length).to eq(1)
    expect(executions.last.fetch('prompt')).to include('Pinned queue input')
  end

  it 'deduplicates an admitted request across restart and rejects changed inputs' do
    create_routine
    run = command('run', 'stable-operation', inputs: 'Original input').fetch('run')
    host.turn(run)
    saved = detail(run).fetch('executionBinding')
    host.restart
    expect(command('run', 'stable-operation', inputs: 'Original input').fetch('run')).to eq(run)
    code, = host.request('POST', "#{path}/run", operation: 'stable-operation', revision: 1, inputs: 'Changed input')
    expect(code).to eq(409)
    expect(detail(run).fetch('executionBinding')).to eq(saved)
    expect(executions.length).to eq(1)
  end

  it 'preserves stopped queued bindings without executing them after restart' do
    create_routine
    host.configure(rules: {'Assessing' => {block: true, gate: 'occupied'}})
    occupied = host.submit(@owner, 'Occupy the owner')
    Contract.wait { host.calls.any? { |call| call['node'] == 'Assessing' } }
    run = command('run', 'queued-stop', inputs: 'Never execute').fetch('run')
    saved = detail(run).fetch('executionBinding')
    command('stop', 'stop-queued', run: run)
    host.api('POST', "/api/agents/#{@owner}/turns/#{occupied}/cancel")
    host.configure(rules: {})
    host.restart
    expect(detail(run)).to include('status' => 'stopped', 'executionBinding' => saved)
    expect(executions).to be_empty
  end

  it 'pins an accepted ordinary task decision and exposes its actual execution contract' do
    host.configure(mode: 'Exec', rules: {'Execution' => {block: true, gate: 'accepted-task'}})
    run = host.submit(@owner)
    execution = wait_execution
    task = tasks.find { |row| row['workloadId'] == run }
    pin = task.fetch('executionBinding')
    expect(pin).to include('status' => 'pinned')
    expect(pin.dig('binding', 'source', 'kind')).to eq('task_decision')
    decision_id = pin.dig('binding', 'source', 'decisionId')
    accepted = task.fetch('decisions').find { |row| row['decisionId'] == decision_id }
    expect(accepted).to include('accepted' => true, 'node' => 'ReadyToWork')
    expect(pin.fetch('contract')).to include('objective' => 'Compare the products.', 'completionCriteria' => ['Return a sourced comparison.'])
    expect(context(execution).fetch('task').to_json).to include('Return a sourced comparison.')
    host.release('accepted-task')
    host.turn(run)
    host.restart
    expect(tasks.find { |row| row['workloadId'] == run }.fetch('executionBinding')).to eq(pin)
  end

  it 'rejects another owner reading a routine run binding' do
    create_routine
    run = command('run', 'owned').fetch('run')
    host.turn(run)
    other = host.agent('OtherBindingOwner')
    code, body = host.request('GET', "/api/agents/#{other}/routines/#{@routine}/runs/#{run}")
    expect(code).to eq(404)
    expect(body.to_json).not_to include('routine_snapshot', 'Return a sourced comparison.')
  end

  it 'keeps actual old history unknown while new admissions receive truthful bindings', :previous_release do
    create_routine
    old = command('run', 'old-release', inputs: 'Historical input').fetch('run')
    host.turn(old)
    history = detail(old)
    host.stop
    File.write(@binary_pointer, File.join(Contract::ROOT, '.build/bin/sapiens4'))
    host.start
    restored = detail(old)
    expect(restored.fetch('executionBinding')).to include('status' => 'legacy_unknown')
    expect(restored.slice('result', 'snapshot', 'originalAssessment')).to eq(history.slice('result', 'snapshot', 'originalAssessment'))
    newer = command('run', 'new-release', inputs: 'Current input').fetch('run')
    host.turn(newer)
    expect(detail(newer).fetch('executionBinding')).to include('status' => 'pinned', 'inputs' => ['Current input'])
    expect(detail(old).fetch('executionBinding')).to include('status' => 'legacy_unknown')
    expect(executions.length).to eq(2)
  end

  it 'uses the same ordinary binding contract for a real scheduled occurrence and legacy tool edits' do
    @fields[:inputs] = 'Saved default input'
    create_routine
    manual = command('run', 'manual-before-schedule', inputs: 'Explicit manual input').fetch('run')
    host.turn(manual)
    expect(detail(manual).dig('executionBinding', 'inputs')).to eq(['Explicit manual input'])
    command('schedule', 'interval', minutes: 60)
    changed = host.control(@owner, 'routine_update', id: @routine, minutes: 60, prompt: 'Compare the products.').fetch('routine')
    expect(changed.fetch('id')).to eq(@routine)
    host.stop
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      sequence = @routine.split('_').last.to_i
      saved = JSON.parse(db.get_first_value('SELECT value FROM scheduled_routines WHERE sequence=?', sequence))
      saved['nextDue'] = '2020-01-01T00:00:00.000000+00:00'
      db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?', [JSON.generate(saved), sequence])
    end
    host.start
    run = Contract.wait(timeout: 90, description: 'real minute ordinary binding') do
      rows = host.state.fetch('personal_routines').find { |row| row['id'] == @routine }.fetch('runs')
      rows.find { |row| row['trigger'] == 'interval' && row['status'] == 'done' }
    end
    pinned = detail(run.fetch('id')).fetch('executionBinding')
    expect(pinned).to include('status' => 'pinned', 'inputs' => [])
    expect(pinned.dig('binding', 'source')).to eq('kind' => 'routine_snapshot')
    expect(pinned.fetch('contract')).to include('objective' => 'Compare the products.', 'completionCriteria' => ['Return a sourced comparison.'])
  end

  it 'fails closed on a saved new binding with another owner without losing the visible run' do
    create_routine
    host.configure(rules: {'Assessing' => {block: true, gate: 'occupied'}})
    host.submit(@owner, 'Occupy the owner')
    Contract.wait { host.calls.any? { |call| call['node'] == 'Assessing' } }
    run = command('run', 'corrupt-binding', inputs: 'Protected input').fetch('run')
    host.stop
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      saved = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', run))
      saved.fetch('calls').first.fetch('executionBinding')['owner'] = 'different-owner'
      db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(saved), run])
    end
    host.configure(rules: {})
    host.start
    row = Contract.wait(description: 'invalid binding fails visibly') do
      candidate = detail(run)
      candidate if %w[failed unresolved stopped interrupted].include?(candidate['status'])
    end
    expect(row.dig('executionBinding', 'status')).to eq('invalid')
    expect(executions).to be_empty
    expect(host.state.fetch('personal_routines').map { |item| item['id'] }).to include(@routine)
  end

  it 'retains separate caller and recipient bindings through an unchanged accepted assignment' do
    child = host.agent('BindingSpecialist')
    accepted = {objective: 'Compare the products.', inputs: [], expectedOutputs: ['A comparison'], completionCriteria: ['Return a sourced comparison.'], conditionalCriteria: {}}
    host.configure(mode: 'Exec', rules: {
      "#{@owner}:Execution" => {response: decision('OutsideSpecialization')},
      'ChiefTriage' => {response: decision('SpecialistSelected', target: child)},
      "#{child}:ReadyToWork" => {response: decision('TaskPrepared', taskType: 'research', specification: accepted)},
      "#{child}:Execution" => {response: decision('Outcome', reply: 'Checked comparison', outcome: {status: 'Completed', satisfiedCriteria: accepted[:completionCriteria], artifacts: []})}
    })
    run = host.submit(@owner)
    host.idle
    work = tasks.find { |row| row['workloadId'] == run }
    expect(work.fetch('outcome')).to include('author' => child, 'reply' => 'Checked comparison')
    original = work.fetch('calls').find { |call| call['addressedTo'] == @owner }
    recipient = work.fetch('calls').find { |call| call['addressedTo'] == child }
    expect(original.fetch('executionBinding').fetch('owner')).to eq(@owner)
    expect(recipient.fetch('executionBinding').fetch('owner')).to eq(child)
    expect(recipient.dig('executionBinding', 'source', 'decisionId')).not_to eq(original.dig('executionBinding', 'source', 'decisionId'))
    expect(work.dig('executionBinding', 'contract', 'completionCriteria')).to eq(accepted[:completionCriteria])
    parent_execution = executions.find { |call| call['owner'] == @owner }
    child_execution = executions.find { |call| call['owner'] == child }
    expect(parent_execution.fetch('prompt')).not_to include('Check source freshness.')
    expect(context(child_execution).fetch('executionSpecification')).to eq(JSON.parse(JSON.generate(accepted)))
    host.restart
    expect(tasks.find { |row| row['workloadId'] == run }).to eq(work)
  end

  it 'refuses a child adding criteria while retaining the original caller binding' do
    child = host.agent('BindingSpecialist')
    expanded = {objective: 'Compare the products with a source check.', inputs: ['Additional source'],
      expectedOutputs: ['Comparison with checked source'], completionCriteria: ['Return a sourced comparison.', 'Check source freshness.'], conditionalCriteria: {}}
    host.configure(mode: 'Exec', rules: {
      "#{@owner}:Execution" => {response: decision('OutsideSpecialization')},
      'ChiefTriage' => {response: decision('SpecialistSelected', target: child)},
      "#{child}:ReadyToWork" => {response: decision('TaskPrepared', taskType: 'research', specification: expanded)}
    })
    run = host.submit(@owner)
    host.idle
    work = tasks.find { |row| row['workloadId'] == run }
    expect(work['outcome']).to be_nil
    expect(executions.none? { |call| call['owner'] == child }).to be(true)
    original = work.fetch('calls').find { |call| call['addressedTo'] == @owner }
    recipient = work.fetch('calls').find { |call| call['addressedTo'] == child }
    expect(original.dig('executionBinding', 'owner')).to eq(@owner)
    expect(original.dig('specification', 'completionCriteria')).to eq(['Return a sourced comparison.'])
    expect(recipient.fetch('state')).to eq('Failed')
    failure = host.state.fetch('turns').find { |turn| turn['id'] == recipient.fetch('callId') }
    expect(failure.fetch('error')).to include('Assignment must preserve every accepted criterion and condition')
    host.restart
    expect(tasks.find { |row| row['workloadId'] == run }).to eq(work)
  end

end
