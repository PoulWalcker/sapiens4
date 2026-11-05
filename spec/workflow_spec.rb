require 'yaml'
require 'sqlite3'

RSpec.describe 'Delegation and completion contracts' do
  before { host.configure(mode: 'Exec') }

  def tasks(owner = chief)
    YAML.safe_load(host.api('GET', "/api/agents/#{owner}/tasks")).fetch('tasks')
  end

  def route_to(target, rules = {})
    host.configure(rules: {
      "#{chief}:Assessing" => {response: decision('OutsideSpecialization')},
      'ChiefTriage' => {response: decision('SpecialistSelected', target: target)}
    }.merge(rules))
  end

  def required(id, requirement)
    {id: id, requirement: requirement, applicability: {kind: 'Required'}}
  end

  def conditional(id, condition, requirement)
    {id: id, requirement: requirement, applicability: {kind: 'Conditional', condition: condition}}
  end

  def result(id, kind, evidence)
    {id: id, result: kind, evidence: evidence}
  end

  it 'delegates once, attributes the result, and preserves the complete audit after restart' do
    researcher = host.agent
    route_to(researcher, 'Delegation' => {response: decision('HandoffPrepared', request: 'Compare the products.', reason: '  Preserve recorded reasoning.  ')})
    call = host.submit(chief)
    host.idle
    task = tasks.first
    aggregate_failures do
      expect(task['calls'].map { |c| c['addressedTo'] }).to eq([chief, researcher])
      expect(task['calls'].last).to include('causedBy' => call, 'state' => 'Completed')
      expect(task['calls'].last['handoffReason']).to eq('  Preserve recorded reasoning.  ')
      expect(task['outcome']).to include('author' => researcher, 'reply' => 'Verified comparison')
      expect(task['decisions']).to all(include('accepted' => true))
      expect(task['decisions'].first['renderedPrompt']).to include('Compare the products.')
    end
    host.restart
    expect(tasks.first).to eq(task)
    expect(tasks.first['outcome']).to include('author' => researcher, 'reply' => 'Verified comparison')
    listing = host.api('GET', "/api/agents/#{chief}/tasks?format=json")['tasks'].first
    expect(listing).to include('state' => 'Completed', 'owner' => researcher, 'title' => 'Compare the products.')
    expect(YAML.safe_load(listing['body'])).to eq(task['task']['specification'])
  end

  it 'returns a specialist referral through the chief without routing in a loop' do
    researcher = host.agent
    host.configure(rules: {
      "#{researcher}:Assessing" => {response: decision('OutsideSpecialization')},
      'ChiefTriage' => {response: decision('SpecialistSelected', target: researcher)}})
    host.submit(researcher)
    host.idle
    task = tasks(researcher).first
    expect(task['calls'].map { |c| c['addressedTo'] }).to eq([researcher, chief])
    expect(task['calls'].last).to include('state' => 'Failed', 'error' => a_string_including('already assessed'))
  end

  it 'creates a needed specialist and completes the original request without extra permission' do
    host.configure(rules: {
      "#{chief}:Assessing" => {response: decision('OutsideSpecialization')},
      'ChiefTriage' => {response: decision('NewSpecialistNeeded')}})
    original = host.submit(chief)
    host.idle
    specialist = host.state['agents'].find { |a| a['name'] == 'NewResearcher' }
    task = tasks.first
    expect(task['workloadId']).to eq(original)
    expect(task['execution']).to include('state' => 'Completed', 'owner' => specialist['id'])
    expect(host.state['agents'].size).to eq(2)
    expect(host.state['orchestration'][specialist['id']]['manager']).to eq(chief)
  end

  it 'keeps workload identity and accepts exactly one clarification for a waiting chief' do
    researcher = host.agent
    route_to(researcher, 'ChiefTriage' => {response: decision('RequestUnclear', question: 'Which products?')})
    original = host.submit(chief)
    host.idle
    expect(tasks.first['execution']['state']).to eq('WaitingForAdmin')
    expect(host.request('POST', "/api/agents/#{researcher}/messages", {text: 'Hijack', workload: original}).first).to eq(409)
    route_to(researcher)
    clarification = host.submit(chief, 'Products A and B', workload: original)
    host.idle
    task = tasks.first
    expect(tasks.size).to eq(1)
    expect(task['workloadId']).to eq(original)
    expect(task['calls'][1]).to include('callId' => clarification, 'activation' => 'RequestClarified')
    expect(task['execution']['state']).to eq('Completed')
    expect(host.request('POST', "/api/agents/#{chief}/messages", {text: 'Duplicate', workload: original}).first).to eq(409)
  end

  it 'rejects an unrecognized provider decision without dispatching work' do
    host.configure(rules: {'Assessing' => {response: decision('StartCron', minutes: 5)}})
    id = host.submit(chief)
    expect(host.turn(id, 'failed')['error']).to include('Unknown decision event')
    expect(host.calls.size).to eq(1)
    expect(host.state['agents'].size).to eq(1)
  end

  {
    'missing reply' => {event: 'FitsSpecialization', mode: 'Repl'},
    'event from another node' => {event: 'SapiSpecified', name: 'Worker', role: 'Research'},
    'unknown response field' => {event: 'FitsSpecialization', mode: 'Exec', typo: true}
  }.each do |behavior, response|
    it "rejects a decision with #{behavior} before any following step" do
      host.configure(rules: {'Assessing' => {response: {reason: 'Fixture', evidence: []}.merge(response)}})
      host.turn(host.submit(chief), 'failed')
      expect(host.calls.map { |call| call['node'] }).to eq(['Assessing'])
      expect(host.state['agents'].size).to eq(1)
    end
  end

  it 'refuses to execute a task with no completion criteria' do
    host.configure(rules: {'ReadyToWork' => {response: decision('TaskPrepared', taskType: 'research', specification: {
      objective: 'Compare products', inputs: [], expectedOutputs: ['Report'], completionCriteria: []})}})
    host.turn(host.submit(chief), 'failed')
    expect(host.calls.map { |call| call['node'] }).to eq(%w[Assessing ReadyToWork])
  end

  it 'rejects duplicate criterion IDs before execution' do
    criteria = [required('same', 'Read the source.'), conditional('same', 'The source exists.', 'Report it.')]
    host.configure(rules: {'ReadyToWork' => {response: decision('TaskPrepared', taskType: 'research', specification: {
      objective: 'Read the source', inputs: [], expectedOutputs: ['Report'], completionCriteria: criteria})}})
    host.turn(host.submit(chief), 'failed')
    expect(host.calls.map { |call| call['node'] }).to eq(%w[Assessing ReadyToWork])
    expect(tasks.first['execution']['state']).to eq('Failed')
  end

  it 'preserves a criterion and its applicability across a handoff' do
    researcher = host.agent
    accepted = [conditional('review', 'A source is available.', 'Review that source.')]
    weakened = [required('review', 'Review that source.')]
    host.configure(rules: {
      "#{chief}:ReadyToWork" => {response: decision('TaskPrepared', taskType: 'research', specification: {
        objective: 'Review a source', inputs: [], expectedOutputs: ['Report'], completionCriteria: accepted})},
      "#{chief}:Execution" => {response: decision('OutsideSpecialization')},
      'ChiefTriage' => {response: decision('SpecialistSelected', target: researcher)},
      "#{researcher}:ReadyToWork" => {response: decision('TaskPrepared', taskType: 'research', specification: {
        objective: 'Review a source', inputs: [], expectedOutputs: ['Report'], completionCriteria: weakened})}})
    host.submit(chief)
    host.idle
    task = tasks.first
    expect(task['calls'].last).to include('addressedTo' => researcher, 'state' => 'Failed')
    expect(task['calls'].last['error']).to include('cannot remove accepted completion criteria')
    expect(task['task']['specification']['completionCriteria']).to eq(JSON.parse(JSON.generate(accepted)))
    expect(task['decisions'].map { |d| d['node'] }).to include('Delegation', 'ReadyToWork')
  end

  it 'rejects an unfinished legacy task on retry without changing its saved specification or audit' do
    host.configure(rules: {'Execution' => {response: decision('Outcome', reply: 'Incomplete', outcome: {
      status: 'Completed', criterionResults: [], artifacts: []})}})
    id = host.submit(chief)
    host.turn(id, 'failed')
    before = tasks.first['decisions'].map { |entry| entry['decisionId'] }
    host.stop
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      saved = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', [id]))
      saved['task']['specification']['completionCriteria'] = ['If a source is available, review it.']
      db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(saved), id])
    end
    host.start
    calls_before_retry = host.calls.size
    host.api('POST', "/api/agents/#{chief}/turns/#{id}/retry", {})
    failed = host.turn(id, 'failed')
    task = tasks.first
    expect(failed['error']).to include('Saved Task specification must use structured completion criteria')
    expect(task['task']['specification']['completionCriteria']).to eq(['If a source is available, review it.'])
    expect(task['decisions'].map { |entry| entry['decisionId'] }).to eq(before)
    expect(host.calls.size).to eq(calls_before_retry)
  end

  it 'reports malformed current task data as corruption rather than an unsupported old format' do
    host.configure(rules: {'Execution' => {response: decision('Outcome', reply: 'Incomplete', outcome: {
      status: 'Completed', criterionResults: [], artifacts: []})}})
    id = host.submit(chief)
    host.turn(id, 'failed')
    decisions = tasks.first.fetch('decisions').map { |entry| entry.fetch('decisionId') }
    host.stop
    saved = SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', [id]))
    end
    malformed_spec = saved.fetch('task')
    malformed_spec.fetch('specification').fetch('completionCriteria').first.delete('applicability')
    {'structured specification' => malformed_spec, 'string task' => 'broken',
      'numeric task' => 17, 'boolean task' => false, 'array task' => []}.each do |label, raw_task|
      SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
        db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(saved.merge('task' => raw_task)), id])
      end
      host.start
      calls_before_retry = host.calls.size
      host.api('POST', "/api/agents/#{chief}/turns/#{id}/retry", {})
      failed = host.turn(id, 'failed')
      history = tasks.first
      aggregate_failures(label) do
        expect(failed['error']).to include('Saved Task specification is malformed')
        expect(failed['error']).not_to include('create a new Task')
        expect(host.calls.size).to eq(calls_before_retry)
        expect(history.fetch('task')).to eq(raw_task)
        expect(history.fetch('decisions').map { |entry| entry.fetch('decisionId') }).to eq(decisions)
      end
      host.stop
      SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
        retained = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', [id]))
        expect(retained.fetch('task')).to eq(raw_task), label
      end
    end
  end

  it 'keeps completed legacy task history readable after restart' do
    id = host.submit(chief)
    host.turn(id, 'done')
    before = tasks.first['decisions'].map { |entry| entry['decisionId'] }
    host.stop
    legacy = 'If a source exists, inspect it.'
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      saved = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', [id]))
      saved['task']['specification']['completionCriteria'] = [legacy]
      db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(saved), id])
    end
    host.start
    task = tasks.first
    expect(task['execution']['state']).to eq('Completed')
    expect(task['task']['specification']['completionCriteria']).to eq([legacy])
    expect(task['decisions'].map { |entry| entry['decisionId'] }).to eq(before)
    expect(task['outcome']).to include('reply' => 'Verified comparison')
  end

  it 'prevents a delegated recipient from reporting completion without an accepted task' do
    researcher = host.agent
    route_to(researcher, "#{researcher}:Assessing" => {response: decision('FitsSpecialization', mode: 'Repl', reply: 'Done')})
    host.submit(chief)
    host.idle
    expect(tasks.first['execution']['state']).to eq('Failed')
    expect(tasks.first['outcome']).to be_nil
    expect(host.calls.none? { |call| call['node'] == 'Execution' }).to eq(true)
  end

  it 'refuses a retired specialist without transferring responsibility' do
    researcher = host.agent
    host.control(chief, 'retire_agent', target: researcher)
    route_to(researcher)
    id = host.submit(chief)
    host.turn(id, 'failed')
    expect(host.calls.none? { |call| call['owner'] == researcher }).to eq(true)
    expect(tasks).to be_empty
    expect(host.state['turns'].map { |turn| turn['id'] }).to eq([id])
  end

  it 'cancels between decision steps without admitting a handoff' do
    route_to(host.agent, 'Delegation' => {block: true})
    id = host.submit(chief)
    Contract.wait { host.calls.any? { |call| call['node'] == 'Delegation' } }
    host.api('POST', "/api/agents/#{chief}/turns/#{id}/cancel", {})
    host.turn(id, %w[interrupted cancelled])
    host.idle
    expect(tasks).to be_empty
    expect(host.state['turns'].map { |turn| turn['id'] }).to eq([id])
    expect(host.calls.all? { |call| call['owner'] == chief }).to eq(true)
  end

  it 'preserves interrupted recipient identity across restart and runs it only on explicit retry' do
    researcher = host.agent
    route_to(researcher, "#{researcher}:Execution" => {block: true})
    host.submit(chief)
    Contract.wait { host.calls.any? { |c| c['owner'] == researcher && c['node'] == 'Execution' } }
    task = tasks.first
    recipient_call = task['execution']['currentCall']
    route_to(researcher)
    host.stop
    host.start
    host.turn(recipient_call, 'interrupted')
    expect(tasks.first['calls'].size).to eq(2)
    host.api('POST', "/api/agents/#{researcher}/turns/#{recipient_call}/retry", {})
    host.idle
    expect(tasks.first['execution']).to include('state' => 'Completed', 'attempt' => 2, 'currentCall' => recipient_call)
    expect(tasks.first['calls'].size).to eq(2)
  end

  criteria = [
    {id: 'read', requirement: 'Read the collection.', applicability: {kind: 'Required'}},
    {id: 'save', requirement: 'Save the joke.', applicability: {kind: 'Conditional', condition: 'A joke is found.'}},
    {id: 'report', requirement: 'Report that no joke was found.', applicability: {kind: 'Conditional', condition: 'No joke is found.'}},
    {id: 'memo', requirement: 'Leave Memo unchanged.', applicability: {kind: 'Required'}}
  ]
  criterion_result = ->(id, kind, evidence) { {id: id, result: kind, evidence: evidence} }
  {
    'verified positive branch' => [[criterion_result.call('read', 'Satisfied', 'Read the collection.'), criterion_result.call('save', 'Satisfied', 'Saved the joke.'), criterion_result.call('report', 'NotApplicable', 'A joke was found.'), criterion_result.call('memo', 'Satisfied', 'Memo was unchanged.')], ['Saved artifact'], 'done'],
    'verified fallback branch' => [[criterion_result.call('read', 'Satisfied', 'Read the collection.'), criterion_result.call('save', 'NotApplicable', 'Nothing found.'), criterion_result.call('report', 'Satisfied', 'Reported the empty search.'), criterion_result.call('memo', 'Satisfied', 'Memo was unchanged.')], ['Search record'], 'done'],
    'missing required result' => [[criterion_result.call('save', 'Satisfied', 'Saved the joke.'), criterion_result.call('report', 'NotApplicable', 'A joke was found.'), criterion_result.call('memo', 'Satisfied', 'Memo was unchanged.')], ['Saved artifact'], 'failed'],
    'missing overall evidence' => [[criterion_result.call('read', 'Satisfied', 'Read the collection.'), criterion_result.call('save', 'Satisfied', 'Saved the joke.'), criterion_result.call('report', 'NotApplicable', 'A joke was found.'), criterion_result.call('memo', 'Satisfied', 'Memo was unchanged.')], [], 'failed'],
    'skipping a required result' => [[criterion_result.call('read', 'NotApplicable', 'Not needed.'), criterion_result.call('save', 'Satisfied', 'Saved the joke.'), criterion_result.call('report', 'NotApplicable', 'A joke was found.'), criterion_result.call('memo', 'Satisfied', 'Memo was unchanged.')], ['Evidence'], 'failed'],
    'unknown result ID' => [[criterion_result.call('read', 'Satisfied', 'Read the collection.'), criterion_result.call('save', 'Satisfied', 'Saved the joke.'), criterion_result.call('report', 'NotApplicable', 'A joke was found.'), criterion_result.call('memo', 'Satisfied', 'Memo was unchanged.'), criterion_result.call('unknown', 'NotApplicable', 'Not needed.')], ['Evidence'], 'failed'],
    'empty result evidence' => [[criterion_result.call('read', 'Satisfied', 'Read the collection.'), criterion_result.call('save', 'Satisfied', 'Saved the joke.'), criterion_result.call('report', 'NotApplicable', ''), criterion_result.call('memo', 'Satisfied', 'Memo was unchanged.')], ['Evidence'], 'failed'],
    'duplicate result ID' => [[criterion_result.call('read', 'Satisfied', 'Read the collection.'), criterion_result.call('read', 'NotApplicable', 'Not needed.'), criterion_result.call('save', 'Satisfied', 'Saved the joke.'), criterion_result.call('report', 'NotApplicable', 'A joke was found.'), criterion_result.call('memo', 'Satisfied', 'Memo was unchanged.')], ['Evidence'], 'failed']
  }.each do |behavior, (results, evidence, status)|
    it "validates completion through the service: #{behavior}" do
      host.configure(rules: {
        'ReadyToWork' => {response: decision('TaskPrepared', taskType: 'research', specification: {
          objective: 'Find a joke 😎\n"quoted"', inputs: [], expectedOutputs: ['Report'], completionCriteria: criteria})},
        'Execution' => {response: decision('Outcome', evidence: evidence, reply: 'Finished', outcome: {
          status: 'Completed', criterionResults: results, artifacts: []})}})
      id = host.submit(chief)
      host.turn(id, status)
      task = tasks.first
      expect(task['execution']['state']).to eq(status == 'done' ? 'Completed' : 'Failed')
      expect(host.calls.count { |call| call['node'] == 'Execution' }).to eq(1)
      expect(task['outcome']).to be_nil if status == 'failed'
    end
  end

  it 'applies prompt edits to future work while retaining the original audit' do
    host.turn(host.submit(chief, 'First'))
    original = tasks.first
    host.api('PUT', '/api/decision-prompts/Assessing', {content: 'New steering: assess the request.'})
    host.turn(host.submit(chief, 'Second'))
    expect(tasks.first).to eq(original)
    expect(tasks.last['decisions'].first['renderedPrompt']).to include('New steering')
    expect(tasks.last['decisions'].first['promptHash']).not_to eq(original['decisions'].first['promptHash'])
    expect(host.request('PUT', '/api/decision-prompts/Unknown', {content: 'No'}).first).to eq(400)
    expect(host.request('PUT', '/api/decision-prompts/Execution', {content: 'No'}, {'Origin' => 'https://evil.example'}).first).to eq(403)
  end
end
