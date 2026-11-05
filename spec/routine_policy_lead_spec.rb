require 'sqlite3'
require 'digest/sha1'

RSpec.describe 'Routine policy setup after Group leadership changes', :host do
  before do
    @old_lead, @new_lead = %w[OriginalLead NewLead].map { |name| host.agent(name) }
    @group = host.api('POST', '/api/groups', {name: 'Policy Group', lead: @old_lead,
      members: [@old_lead, @new_lead]}, status: 201)
    @routine = host.control(@old_lead, 'routine_create', minutes: 30,
      prompt: 'Prepare a periodic comparison.', missedPolicy: 'run_latest').fetch('routine')
    host.stop
    @routine.merge!('owner' => @group.fetch('id'), 'policySelection' => {'kind' => 'awaiting_selection'})
    namespace = ['6ba7b8119dad11d180b400c04fd430c8'].pack('H*')
    bytes = Digest::SHA1.digest(namespace + "routine-policy:#{@routine['id']}:#{@routine['prompt']}").bytes.first(16)
    bytes[6], bytes[8] = (bytes[6] & 15) | 80, (bytes[8] & 63) | 128
    @identity = bytes.pack('C*').unpack1('H*')
    @origin = {'policyRoutine' => @routine.fetch('id'), 'author' => 'system', 'group' => @group.fetch('id')}
    @message = {'id' => @identity, 'author' => 'system', 'text' => 'Select the saved routine policy.',
      'created' => @routine.fetch('created')}
  end

  def path; "/api/groups/#{@group.fetch('id')}"; end
  def group; host.api('GET', path); end

  def policy_operation
    {op: 'routine_policy', id: @routine.fetch('id'), expectedPrompt: @routine.fetch('prompt'),
      expectedSelection: {kind: 'awaiting_selection'}, missedPolicy: 'skip_missed'}
  end

  # Saved setup accepted before a leadership transfer.
  def install_setup(status:, materialized:, lead: @new_lead)
    raise 'Fixture requires a stopped host' if host.process
    @original = {'id' => @identity, 'target' => @old_lead, 'status' => status,
      'settled' => status == 'failed', 'message' => @identity, 'root' => @identity,
      'created' => @routine.fetch('created'), 'origin' => @origin}
    @original['error'] = 'Saved setup failure' if status == 'failed'
    @group.merge!('lead' => lead, 'requests' => [@original],
      'messages' => @group.fetch('messages') + [@message])
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.transaction do
        db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?',
          [JSON.generate(@routine), @routine.fetch('id').split('_').last.to_i])
        db.execute('UPDATE groups SET value=? WHERE id=?', [JSON.generate(@group), @group.fetch('id')])
      end
    end
    return unless materialized
    state_path = File.join(host.root, 'agentpy', 'agents', @old_lead, 'state.json')
    state = JSON.parse(File.read(state_path))
    turn = {'id' => @identity, 'flow' => 'chat', 'input' => @message.fetch('text'), 'status' => status,
      'attempt' => 1, 'created' => @routine.fetch('created'), 'origin' => @origin, 'batchable' => false}
    turn['error'] = 'Saved setup failure' if status == 'failed'
    state.fetch('turns') << turn
    Contract.atomic_json(state_path, state)
  end

  def selected
    Contract.wait(description: 'current Group Lead saves the policy and settles setup') do
      snapshot = host.state
      saved = snapshot.fetch('routines').find { |routine| routine['id'] == @routine['id'] }
      requests = snapshot.fetch('groups').find { |row| row['id'] == @group['id'] }.fetch('requests')
      saved if saved['policySelection'] == {'kind' => 'selected', 'policy' => 'skip_missed'} &&
        requests.last['settled'] && requests.last['status'] == 'done'
    end
  end

  {'unmaterialized' => false, 'materialized' => true}.each do |name, materialized|
    it "reassigns #{name} queued setup once without executing the former Lead" do
      install_setup(status: 'queued', materialized: materialized)
      host.configure(rules: {"#{@new_lead}:group" => {actions: [{control: policy_operation}]}})
      host.start
      expect(selected).to include('runCount' => 0)
      saved = group
      expect(saved.fetch('requests').size).to eq(2)
      expect(saved.fetch('requests').first).to include('id' => @identity, 'target' => @old_lead,
        'status' => 'cancelled', 'settled' => true)
      expect(saved.fetch('requests').last).to include('target' => @new_lead, 'root' => @identity,
        'message' => @identity, 'status' => 'done')
      expect(saved.fetch('messages').count { |row| row['id'] == @identity }).to eq(1)
      expect(host.calls.map { |call| call['owner'] }).to eq([@new_lead])
      if materialized
        expect(host.state.fetch('turns').find { |turn| turn['id'] == @identity }).to include('status' => 'cancelled')
      end
      before = saved.fetch('requests')
      host.restart
      expect(group.fetch('requests')).to eq(before)
      expect(host.calls.size).to eq(1)
    end
  end

  it 'rejects queued setup on a former Lead before calling the model after an in-session transfer' do
    install_setup(status: 'queued', materialized: true, lead: @old_lead)
    Contract.atomic_json(File.join(host.root, 'corpora-settings.json'),
      {device: 'This computer', scope: '', status: 'paused', revision: 1, operations: {}})
    host.configure(rules: {"#{@new_lead}:group" => {actions: [{control: policy_operation}]}})
    host.start
    host.api('PUT', path, {revision: group.fetch('revision'), lead: @new_lead})
    host.api('PUT', '/api/corpora/lifecycle', {action: 'activate', operation: 'activate-policy-fixture', revision: 1})
    expect(host.turn(@identity, 'failed')).to include('error' => a_string_including('current Group Lead'))
    expect(host.calls).to be_empty
    host.restart
    selected
    expect(group.fetch('requests').map { |request| request['target'] }).to eq([@old_lead, @new_lead])
    expect(host.calls.map { |call| call['owner'] }).to eq([@new_lead])
  end

  it 'preserves failed history and retries only the current Lead setup without an automatic retry loop' do
    install_setup(status: 'failed', materialized: true)
    host.configure(rules: {"#{@new_lead}:group" => {fail: true}})
    host.start
    replacement = group.fetch('requests').last.fetch('id')
    host.turn(replacement, 'failed')
    host.idle
    before = group.fetch('requests')
    expect(before.first).to eq(@original)
    host.restart
    expect(group.fetch('requests')).to eq(before)
    expect(host.calls.size).to eq(1)
    expect(host.request('POST', path + "/calls/#{@identity}/retry", {}).first).to eq(409)
    expect(group.fetch('requests')).to eq(before)
    host.configure(rules: {"#{@new_lead}:group" => {actions: [{control: policy_operation}]}})
    host.api('POST', path + "/calls/#{replacement}/retry", {})
    selected
    expect(group.fetch('requests').map { |request| request['id'] }).to eq([@identity, replacement])
    expect(group.fetch('requests').first).to eq(@original)
    expect(host.calls.map { |call| call['owner'] }).to eq([@new_lead, @new_lead])
  end

  it 'waits for running setup to stop and creates a fresh successor when leadership returns to an earlier Lead' do
    install_setup(status: 'failed', materialized: true)
    host.configure(rules: {
      "#{@new_lead}:group" => {block: true, gate: 'finish-old-policy', actions: [{control: policy_operation}]},
      "#{@old_lead}:group" => {actions: [{control: policy_operation}]}})
    host.start
    running = Contract.wait(description: 'replacement setup is actually running on the new Lead') do
      request = group.fetch('requests').last
      request if request['status'] == 'running' && host.calls.size == 1
    end
    host.api('PUT', path, {revision: group.fetch('revision'), lead: @old_lead})
    host.api('POST', path + '/messages', {text: "Edit @#{@routine['id']} ( #{@routine['title']} )\n" \
      "Edit Frequency (prev every 30 minutes): new every 30 minutes\n" \
      "Edit Execution Prompt as: #{@routine['prompt']}"}, status: 202)
    expect(group.fetch('requests').map { |request| request['id'] }).to eq([@identity, running.fetch('id')])
    expect(host.request('POST', path + "/calls/#{@identity}/retry", {}).first).to eq(409)
    expect(host.request('POST', "/api/agents/#{@old_lead}/turns/#{@identity}/retry", {}).first).to eq(409)
    expect(host.calls.size).to eq(1)
    host.release('finish-old-policy')
    host.turn(running.fetch('id'), 'failed')
    host.idle
    host.restart
    selected
    requests = group.fetch('requests')
    expect(requests.size).to eq(3)
    expect(requests.map { |request| request['id'] }.uniq.size).to eq(3)
    expect(requests.first).to eq(@original)
    expect(requests[1]).to include('id' => running.fetch('id'), 'target' => @new_lead, 'status' => 'failed')
    expect(requests.last).to include('target' => @old_lead, 'root' => @identity, 'status' => 'done')
    expect(host.calls.map { |call| call['owner'] }).to eq([@new_lead, @old_lead])
  end
end
