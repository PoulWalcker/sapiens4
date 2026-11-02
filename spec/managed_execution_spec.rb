require 'sqlite3'

RSpec.describe 'Opt-in managed execution and saved-file compatibility', :host do
  before { @owner = host.agent }
  def file; File.join(host.root, 'agentpy/agents', @owner, 'state.json'); end
  def saved; JSON.parse(File.read(file)); end
  def edit_turn(id)
    state = saved
    yield state.fetch('turns').find { |t| t['id'] == id }
    Contract.atomic_json(file, state)
  end
  def command(id, action, token = 'external-operation')
    turn = saved.fetch('turns').find { |t| t['id'] == id }
    value = {'id' => token, 'action' => action, 'revision' => turn.fetch('managed').fetch('revision')}
    edit_turn(id) { |t| t['managed_command'] = value }
    value
  end
  def failed
    host.configure(fail: true)
    id = host.submit(@owner, 'Managed request', managed: true)
    host.turn(id, 'failed')
    id
  end
  def database
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) { |db| yield db }
  end

  it 'runs an ordinary request with durable pins, actual attempt count and outcome receipts' do
    id = host.submit(@owner, 'Managed request', managed: true)
    row = host.turn(id)
    expect(row['managed']).to include('pin' => {'input' => 'Managed request', 'flow' => 'chat'}, 'attempts_started' => 1)
    expect(row['managed']['receipts']).to contain_exactly(include('attempt' => 1, 'status' => 'done', 'output' => 'Contract test reply'))
    host.restart
    expect(host.turn(id)['managed']).to eq(row['managed'])
    expect(host.calls.length).to eq(1)
  end

  it 'keeps ordinary legacy submission outside managed authority' do
    id = host.submit(@owner)
    expect(host.turn(id)).not_to have_key('managed')
    edit_turn(id) { |t| t['status'] = 'cancelled' }
    expect(host.turn(id, 'cancelled')).not_to have_key('managed')
  end

  [
    {managed: 'yes'}, {managed: true, flow: 'computer'},
    {managed: true, workload: 'unknown'},
    {managed: true, text: "Add New Scheduled Routine:\nFrequency: every 1 minute\nExecute: hello"}
  ].each do |fields|
    it "rejects unsupported managed submission #{fields.inspect} before creating work" do
      expect(host.request('POST', "/api/agents/#{@owner}/messages", {text: 'request'}.merge(fields)).first).to eq(400)
      expect(host.state['turns']).to be_empty
      expect(host.state['routines']).to be_empty
      expect(host.calls).to be_empty
    end
  end

  %w[pin receipts attempts_started].each do |protected|
    it "rejects external changes to protected #{protected} and recovers after repairing the file" do
      id = failed
      original = saved
      edit_turn(id) { |t| t['managed'].delete(protected) }
      expect(host.request('GET', '/api/state').first).to eq(409)
      expect(host.request('GET', '/api/health').first).to eq(200)
      Contract.atomic_json(file, original)
      expect(host.turn(id, 'failed')['managed']).to have_key(protected)
    end
  end

  it 'rejects externally promoted, deleted and duplicate managed records' do
    id = failed
    original = saved
    [[], original['turns'] * 2].each do |turns|
      Contract.atomic_json(file, original.merge('turns' => turns))
      expect(host.request('GET', '/api/state').first).to eq(409)
    end
    invented = original['turns'].first.merge('id' => 'invented-managed')
    Contract.atomic_json(file, original.merge('turns' => [invented]))
    expect(host.request('GET', '/api/state').first).to eq(409)
    Contract.atomic_json(file, original)
  end

  it 'executes explicit external retry once, retains earlier receipts and rejects stale new commands' do
    id = failed
    host.configure(fail: false)
    cmd = command(id, 'retry')
    row = Contract.wait { value = host.state['turns'].find { |t| t['id'] == id }; value if value['status'] == 'done' }
    expect(row['managed']['attempts_started']).to eq(2)
    expect(row['managed']['receipts'].map { |r| r['status'] }).to eq(%w[failed done])
    edit_turn(id) { |t| t['managed_command'] = cmd }
    host.restart
    expect(host.turn(id)['managed']['attempts_started']).to eq(2)
    expect(host.calls.length).to eq(2)
    edit_turn(id) { |t| t['managed_command'] = cmd.merge('revision' => t['managed']['revision']) }
    expect(host.request('GET', '/api/state').first).to eq(409)
    edit_turn(id) { |t| t['managed_command'] = cmd.merge('id' => 'new-stale-command') }
    expect(host.request('GET', '/api/state').first).to eq(409)
    edit_turn(id) { |t| t.delete('managed_command') }
  end

  it 'does not execute a file command or write the database while reading a paused installation' do
    id = failed
    identity = host.api('GET', '/api/corpora')
    host.api('PUT', '/api/corpora/lifecycle', action: 'pause', operation: 'managed-pause', revision: identity['revision'])
    command(id, 'retry')
    database do |db|
      version = db.get_first_value('PRAGMA data_version')
      3.times { expect(host.turn(id, 'failed')['managed']['attempts_started']).to eq(1) }
      expect(db.get_first_value('PRAGMA data_version')).to eq(version)
    end
    expect(host.calls.length).to eq(1)
  end

  it 'uses the same durable cancellation path for a running external command and HTTP retry' do
    host.configure(block: true)
    id = host.submit(@owner, 'Managed request', managed: true)
    host.turn(id, 'running')
    command(id, 'cancel')
    row = host.turn(id, 'interrupted')
    expect(row['managed']['last_command']['action']).to eq('cancel')
    expect(row['managed']['receipts'].last['status']).to eq('interrupted')
    host.configure(block: false)
    host.api('POST', "/api/agents/#{@owner}/turns/#{id}/retry")
    expect(host.turn(id)['managed']['attempts_started']).to eq(2)
  end

  it 'rolls back a rejected managed transition without consuming its command or creating fresh work' do
    id = failed
    original = host.turn(id, 'failed')['managed']
    database { |db| db.execute("CREATE TRIGGER reject_managed BEFORE UPDATE ON managed_turns BEGIN SELECT RAISE(ABORT, 'fixture transaction failure'); END") }
    expect(host.request('POST', "/api/agents/#{@owner}/turns/#{id}/retry", {}).first).to eq(500)
    expect(host.turn(id, 'failed')['managed']).to eq(original)
    database { |db| db.execute('DROP TRIGGER reject_managed') }
    expect(host.calls.length).to eq(1)
  end

  it 'recovers a committed retry after its file projection failed without duplicating the attempt', unless: Gem.win_platform? do
    id = failed
    host.configure(fail: false)
    directory = File.dirname(file)
    File.chmod(0500, directory)
    expect(host.request('POST', "/api/agents/#{@owner}/turns/#{id}/retry", {}).first).to eq(500)
    host.crash
    File.chmod(0700, directory)
    host.start
    row = host.turn(id)
    expect(row['managed']['attempts_started']).to eq(2)
    expect(row['managed']['receipts'].length).to eq(2)
    host.restart
    expect(host.calls.length).to eq(2)
  ensure
    File.chmod(0700, directory) if directory && File.directory?(directory)
  end
  it 'retains the submitted message when enrollment commits before the first file write fails', unless: Gem.win_platform? do
    directory = File.dirname(file)
    File.chmod(0500, directory)
    expect(host.request('POST', "/api/agents/#{@owner}/messages", {text: 'Durable initial message', managed: true}).first).to eq(500)
    row = host.state['turns'].find { |t| t['managed'] }
    expect(row).to include('input' => 'Durable initial message', 'status' => 'queued')
    host.crash
    File.chmod(0700, directory)
    host.start
    host.turn(row['id'])
    expect(saved['chat']).to include(include('role' => 'user', 'content' => 'Durable initial message'))
    expect(host.calls.length).to eq(1)
  ensure
    File.chmod(0700, directory) if directory && File.directory?(directory)
  end

  it 'recovers a committed reply and chat after the final file projection fails', unless: Gem.win_platform? do
    host.configure(block: true)
    id = host.submit(@owner, 'Durable reply', managed: true)
    host.turn(id, 'running')
    directory = File.dirname(file)
    File.chmod(0500, directory)
    host.release
    row = host.turn(id)
    expect(row['managed']['receipts'].last['output']).to eq('Contract test reply')
    expect(saved['turns'].find { |t| t['id'] == id }['status']).to eq('running')
    host.crash
    File.chmod(0700, directory)
    host.start
    expect(host.turn(id)['output']).to eq('Contract test reply')
    expect(saved['chat']).to include(include('role' => 'agent', 'content' => 'Contract test reply', 'turn' => id))
    expect(host.calls.length).to eq(1)
  ensure
    File.chmod(0700, directory) if directory && File.directory?(directory)
  end

  it 'recovers a consumed external retry whose command envelope remained in the old file', unless: Gem.win_platform? do
    id = failed
    host.configure(fail: false)
    cmd = command(id, 'retry', 'durable-external-retry')
    directory = File.dirname(file)
    File.chmod(0500, directory)
    Contract.wait do
      row = host.state['turns'].find { |t| t['id'] == id }
      row if row['managed']['commands'].include?(cmd)
    end
    host.crash
    File.chmod(0700, directory)
    host.start
    expect(host.turn(id)['managed']['attempts_started']).to eq(2)
    expect(host.calls.length).to eq(2)
  ensure
    File.chmod(0700, directory) if directory && File.directory?(directory)
  end

  it 'leaves unknown managed-command metadata on legacy turns inert' do
    host.configure(fail: true)
    id = host.submit(@owner, 'Legacy request')
    host.turn(id, 'failed')
    edit_turn(id) { |t| t['managed_command'] = {'id' => 'legacy-field', 'action' => 'retry', 'revision' => 0} }
    host.restart
    # Observe a later actual worker pulse, rather than assume a sleep ran it.
    previous = host.state['pulse']['numbers']['bpm60']
    Contract.wait { host.state['pulse']['numbers']['bpm60'] > previous + 2 }
    expect(host.turn(id, 'failed')).not_to have_key('managed')
    expect(host.calls.length).to eq(1)
  end

  it 'still stops the active process when its committed cancel cannot be projected to a file', unless: Gem.win_platform? do
    host.configure(block: true)
    id = host.submit(@owner, 'Cancel despite file failure', managed: true)
    host.turn(id, 'running')
    directory = File.dirname(file)
    File.chmod(0500, directory)
    expect(host.request('POST', "/api/agents/#{@owner}/turns/#{id}/cancel", {}).first).to eq(500)
    row = host.turn(id, 'interrupted')
    expect(row['managed']['last_command']['action']).to eq('cancel')
    File.chmod(0700, directory)
    host.configure(block: false)
    host.api('POST', "/api/agents/#{@owner}/turns/#{id}/retry")
    expect(host.turn(id)['managed']['attempts_started']).to eq(2)
  ensure
    File.chmod(0700, directory) if directory && File.directory?(directory)
  end

  it 'upgrades a saved schema-eight installation without changing legacy history and remains restartable' do
    legacy = host.submit(@owner, 'Existing installation')
    expected = host.turn(legacy)
    host.stop
    database do |db|
      db.execute('DROP TABLE managed_turns')
      db.execute('PRAGMA user_version=8')
    end
    host.start
    expect(host.turn(legacy)).to eq(expected)
    managed = host.submit(@owner, 'New managed request', managed: true)
    result = host.turn(managed)
    host.restart
    expect(host.turn(legacy)).to eq(expected)
    expect(host.turn(managed)['managed']).to eq(result['managed'])
    expect(host.calls.length).to eq(2)
  end

end
