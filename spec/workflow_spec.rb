require 'yaml'

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

  it 'resumes the waiting chief task from a normal chat reply after restart' do
    researcher = host.agent
    route_to(researcher, 'ChiefTriage' => {response: decision('RequestUnclear', question: 'Which products?')})
    original = host.submit(chief)
    host.idle
    expect(tasks.first['execution']['state']).to eq('WaitingForAdmin')
    host.restart
    route_to(researcher)
    clarification = host.submit(chief, 'Products A and B')
    host.idle
    task = tasks.first
    expect(tasks.size).to eq(1)
    expect(task['workloadId']).to eq(original)
    expect(task['calls'][1]).to include('callId' => clarification, 'causedBy' => original,
      'activation' => 'RequestClarified', 'request' => 'Products A and B')
    expect(task['execution']['state']).to eq('Completed')
    following = host.submit(chief, 'A separate comparison')
    host.idle
    expect(tasks.map { |work| work['workloadId'] }).to contain_exactly(original, following)
  end

  it 'keeps a reply to another Sapi separate from the waiting chief task' do
    researcher = host.agent
    route_to(researcher, 'ChiefTriage' => {response: decision('RequestUnclear', question: 'Which products?')})
    original = host.submit(chief)
    host.idle
    waiting = tasks.first
    expect(waiting['execution']['state']).to eq('WaitingForAdmin')
    separate = host.submit(researcher, 'A different request')
    host.idle
    expect(tasks).to eq([waiting])
    expect(tasks(researcher).map { |work| work['workloadId'] }).to eq([separate])
    expect(tasks.first['workloadId']).to eq(original)
  end

  it 'does not guess between waiting chief tasks and retains explicit clarification' do
    researcher = host.agent
    route_to(researcher, 'ChiefTriage' => {block: true,
      response: decision('RequestUnclear', question: 'Which products?')})
    first = host.submit(chief, 'First comparison')
    Contract.wait { host.calls.count { |call| call['node'] == 'ChiefTriage' } == 1 }
    second = host.submit(chief, 'Second comparison')
    Contract.wait { host.calls.count { |call| call['node'] == 'ChiefTriage' } == 2 }
    host.release
    host.idle
    waiting = tasks
    expect(waiting.map { |work| work['workloadId'] }).to contain_exactly(first, second)
    expect(waiting.map { |work| work['execution']['state'] }).to eq(['WaitingForAdmin', 'WaitingForAdmin'])
    turns = host.state['turns'].map { |turn| turn['id'] }
    status, body = host.request('POST', "/api/agents/#{chief}/messages", {text: 'Ambiguous answer'})
    expect(status).to eq(409)
    expect(body['error']).to include('several tasks waiting')
    expect(tasks).to eq(waiting)
    expect(host.state['turns'].map { |turn| turn['id'] }).to eq(turns)
    route_to(researcher)
    explicit = host.submit(chief, 'Answer the first comparison', workload: first)
    host.idle
    automatic = host.submit(chief, 'Answer the remaining comparison')
    host.idle
    completed = tasks.to_h { |work| [work['workloadId'], work] }
    expect(completed.keys).to contain_exactly(first, second)
    expect(completed[first]['calls'][1]['callId']).to eq(explicit)
    expect(completed[second]['calls'][1]['callId']).to eq(automatic)
    expect(completed.values.map { |work| work['execution']['state'] }).to eq(['Completed', 'Completed'])
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

  criteria = ['Read the collection.', 'If a joke is found, save it.', 'If no joke is found, report that.', 'Leave Memo unchanged.']
  {
    'verified positive branch' => [[criteria[0], criteria[1], criteria[3]], {criteria[2] => 'Found a joke'}, ['Saved artifact'], 'done'],
    'verified fallback branch' => [[criteria[0], criteria[2], criteria[3]], {criteria[1] => 'Nothing found'}, ['Search record'], 'done'],
    'missing unconditional requirement' => [[criteria[1], criteria[3]], {criteria[2] => 'Found a joke'}, ['Saved artifact'], 'failed'],
    'missing evidence' => [criteria, {}, [], 'failed'],
    'skipping unconditional requirement' => [[criteria[1], criteria[2], criteria[3]], {criteria[0] => 'Not needed'}, ['Evidence'], 'failed'],
    'unknown condition' => [criteria, {'If unknown, skip' => 'Not needed'}, ['Evidence'], 'failed'],
    'empty skip reason' => [[criteria[0], criteria[1], criteria[3]], {criteria[2] => ''}, ['Evidence'], 'failed'],
    'both satisfied and skipped' => [criteria, {criteria[2] => 'Not needed'}, ['Evidence'], 'failed']
  }.each do |behavior, (satisfied, skipped, evidence, status)|
    it "validates completion through the service: #{behavior}" do
      host.configure(rules: {
        'ReadyToWork' => {response: decision('TaskPrepared', taskType: 'research', specification: {
          objective: 'Find a joke 😎\n"quoted"', inputs: [], expectedOutputs: ['Report'], completionCriteria: criteria})},
        'Execution' => {response: decision('Outcome', evidence: evidence, reply: 'Finished', outcome: {
          status: 'Completed', satisfiedCriteria: satisfied, notApplicableCriteria: skipped, artifacts: []})}})
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
