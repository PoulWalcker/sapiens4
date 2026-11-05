require 'sqlite3'
require 'time'
require 'digest/sha1'

RSpec.describe 'Scheduled work and pulse contracts' do
  def routine(owner = chief, minutes = 1, prompt = 'Return a sourced comparison.')
    host.control(owner, 'routine_create', minutes: minutes, prompt: prompt, missedPolicy: 'run_latest').fetch('routine')
  end

  [0, 525601, '2', 1.5].each do |minutes|
    it "rejects invalid API interval #{minutes.inspect} without saving a schedule" do
      expect(host.request('POST', "/api/agents/#{chief}/control", {op: 'routine_create', minutes: minutes, prompt: 'Compare products', missedPolicy: 'run_latest'}).first).to eq(400)
      expect(host.state['routines']).to be_empty
      expect(host.calls).to be_empty
    end
  end

  it 'preserves the saved due-time spelling when pausing a routine' do
    owner = chief
    item = routine
    item['nextDue'] = '2030-01-01T12:00:00.000000+00:00'
    host.stop
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?', [JSON.generate(item), item['id'].split('_').last.to_i])
    end
    host.start
    paused = host.control(owner, 'routine_pause', id: item['id'])['routine']
    expect(paused).to include('paused' => true, 'nextDue' => item['nextDue'])
    expect(host.calls).to be_empty
  end

  it 'preserves legacy deadline identities through recovery, pause, edit and restart' do
    owner = chief
    spellings = {
      'fractional UTC' => '01-01T12:00:00.000000+00:00',
      'positive offset' => '01-01T16:00:00.125000+04:00',
      'negative offset' => '01-01T07:00:00.123456-05:00'
    }
    originals = spellings.map do |label, suffix|
      item = routine(owner, 60, "Keep the original #{label} instructions.")
      item.merge('nextDue' => "2030-#{suffix}", 'extension' => {'label' => label})
    end
    host.stop
    # Saved installation admissions use the released UUID-v5 input verbatim.
    # Pausing the host keeps recovery observable without executing the queued work.
    Contract.atomic_json(File.join(host.root, 'corpora-settings.json'),
      {device: 'This computer', scope: '', status: 'paused', revision: 1, operations: {}})
    namespace = ['6ba7b8119dad11d180b400c04fd430c8'].pack('H*')
    admissions = originals.map do |item|
      snapshot = item.merge('nextDue' => item.fetch('nextDue').sub('2030-', '2020-'))
      bytes = Digest::SHA1.digest(namespace + "#{snapshot.fetch('id')}:#{snapshot.fetch('nextDue')}").bytes.first(16)
      bytes[6], bytes[8] = (bytes[6] & 15) | 80, (bytes[8] & 63) | 128
      {id: bytes.pack('C*').unpack1('H*'), routine: snapshot,
        target: {kind: 'personal', agent: owner}, created: '2020-01-02T00:00:00+00:00'}
    end
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.transaction do
        originals.each do |item|
          db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?',
            [JSON.generate(item), item.fetch('id').split('_').last.to_i])
        end
        admissions.each do |admission|
          db.execute("INSERT INTO routine_occurrences(id,value,status) VALUES (?,?,'admitted_pending')",
            [admission.fetch(:id), JSON.generate(admission)])
        end
      end
    end
    host.start
    admissions.zip(originals).each do |admission, item|
      turn = host.turn(admission.fetch(:id), 'queued')
      expect(turn).to include('input' => item.fetch('prompt'))
      expect(turn.fetch('origin')).to include('routine' => item.fetch('id'),
        'due' => admission.fetch(:routine).fetch('nextDue'))
      paused = host.control(owner, 'routine_pause', id: item.fetch('id')).fetch('routine')
      expect(paused).to include('paused' => true, 'nextDue' => item.fetch('nextDue'),
        'extension' => item.fetch('extension'))
      edited = host.control(owner, 'routine_update', id: item.fetch('id'), minutes: 120,
        prompt: "Updated #{item.fetch('prompt')}").fetch('routine')
      expect(edited).to include('id' => item.fetch('id'), 'paused' => true,
        'extension' => item.fetch('extension'))
      expect(edited.fetch('nextDue')).to end_with('+00:00')
    end
    before = host.state
    host.restart
    expect(host.state.fetch('routines')).to eq(before.fetch('routines'))
    expect(host.state.fetch('turns').map { |turn| turn.fetch('id') }.sort).to eq(admissions.map { |item| item.fetch(:id) }.sort)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      admissions.each do |admission|
        value, status = db.get_first_row('SELECT value,status FROM routine_occurrences WHERE id=?', admission.fetch(:id))
        expect(JSON.parse(value)).to eq(JSON.parse(JSON.generate(admission)))
        expect(status).to eq('admitted_ready')
      end
    end
    expect(host.calls).to be_empty
  end

  {'missing' => ->(item) { item.delete('paused') },
   'null' => ->(item) { item['paused'] = nil }}.each do |state, corrupt|
    it "rejects a #{state} lifecycle flag in a current-schema routine without repairing or activating it" do
      owner = chief
      item = routine
      item['nextDue'] = '2030-01-01T12:00:00.000000+00:00'
      corrupt.call(item)
      host.stop
      SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
        expect(db.get_first_value('PRAGMA user_version')).to eq(9)
        db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?', [JSON.generate(item), item['id'].split('_').last.to_i])
      end
      host.start
      code, error = host.request('POST', "/api/agents/#{owner}/control", {op: 'routine_resume', id: item['id']})
      expect(code).to eq(500)
      expect(error['error']).to include('paused')
      expect(host.state['routines']).to eq([item])
      expect(host.calls).to be_empty
    end
  end

  it 'migrates schema 8 lifecycle defaults once while preserving explicit flags, instructions and history' do
    owner = chief
    originals = 4.times.map { |index| routine(owner, 60, "Keep migration instructions #{index}.") }
    audit = host.state['events'][owner].select { |entry| entry['event'].start_with?('routine.') }
    legacy = originals.map(&:dup)
    legacy.each do |item|
      item.delete('policySelection')
      item['nextDue'] = '2030-01-01T12:00:00.000000+00:00'
    end
    legacy[0].delete('paused')
    legacy[1]['paused'] = nil
    legacy[2]['paused'] = true
    legacy[3]['paused'] = false
    host.stop
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.execute('DROP TABLE routine_occurrences')
      db.execute('PRAGMA user_version=8')
      legacy.each do |item|
        db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?', [JSON.generate(item), item['id'].split('_').last.to_i])
      end
    end
    host.start
    expected = legacy.each_with_index.map do |item, index|
      item.merge('paused' => index == 2, 'policySelection' => {'kind' => 'awaiting_selection'})
    end
    expect(host.state['routines']).to eq(expected)
    expect(host.state['events'][owner].select { |entry| entry['event'].start_with?('routine.') }).to eq(audit)
    host.restart
    expect(host.state['routines']).to eq(expected)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      expect(db.get_first_value('PRAGMA user_version')).to eq(9)
    end
  end

  it 'creates, edits, pauses and resumes a persistent routine without calling the model' do
    owner = chief
    item = routine
    host.submit(owner, "Pause Scheduled Routine\n@#{item['id']}")
    updated = host.control(owner, 'routine_update', id: item['id'], minutes: 2, prompt: 'Updated instructions')['routine']
    expect(updated).to include('id' => item['id'], 'paused' => true, 'runCount' => 0)
    other = host.agent
    expect(host.request('POST', "/api/agents/#{other}/control", {op: 'routine_resume', id: item['id']}).first).to eq(404)
    host.restart
    expect(host.state['routines'].first).to eq(updated)
    resumed = host.control(owner, 'routine_resume', id: item['id'])['routine']
    expect(resumed).to include('paused' => false, 'runCount' => 0)
    expect(Time.iso8601(resumed['nextDue']) - Time.now).to be_between(115, 121)
    expect(host.calls).to be_empty
    events = host.state['events'][owner].select { |e| e['event'].start_with?('routine.') }
    expect(events.map { |e| e['event'] }).to include('routine.created', 'routine.paused', 'routine.updated', 'routine.resumed')
    before = events
    3.times { host.state }
    expect(host.state['events'][owner].select { |e| e['event'].start_with?('routine.') }).to eq(before)
  end

  [['XXX', 'PROMPT'], ['0', 'Hello'], ['1.5', 'Hello'], ['1', 'PROMPT']].each do |minutes, prompt|
    it "rejects invalid routine template #{minutes.inspect}/#{prompt.inspect} without executing work" do
      text = "Add New Scheduled Routine:\nFrequency: every #{minutes} minutes\nExecute: #{prompt}"
      expect(host.request('POST', "/api/agents/#{chief}/messages", {text: text}).first).to eq(400)
      expect(host.state['routines']).to be_empty
      expect(host.calls).to be_empty
    end
  end

  it 'rejects a stale frequency edit without changing the current routine' do
    item = routine
    text = "Edit @#{item['id']} ( #{item['title']} )\nEdit Frequency (prev every 1 minutes): new every 2 minutes\nEdit Execution Prompt as: New comparison."
    host.submit(chief, text)
    updated = host.state['routines'].first
    expect(host.request('POST', "/api/agents/#{chief}/messages", {text: text}).first).to eq(409)
    expect(host.state['routines'].first).to eq(updated)
  end

  it 'admits one occurrence from an overdue fixture, enforces the run limit, and excludes paused or retired owners', :slow do
    owner, retired = chief, host.agent
    ordinary, capped, paused, unavailable = routine, routine, routine, routine(retired)
    host.control(owner, 'routine_pause', id: paused['id'])
    host.control(owner, 'retire_agent', target: retired)
    initial = host.state['routines']
    host.stop
    # Persisted installation input: the service owns scheduling and all outcomes.
    # An overdue installation avoids a thousand-minute wait without replacing its clock.
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      initial.each do |item|
        item['nextDue'] = (Time.now.utc - 600).iso8601(6).sub('Z', '+00:00')
        item['runCount'] = 999 if item['id'] == capped['id']
        db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?',
          [JSON.generate(item), item['id'].split('_').last.to_i])
      end
    end
    host.start
    Contract.wait(timeout: 85, description: 'real minute pulse to admit overdue routines') do
      host.state['routines'].select { |r| [ordinary['id'], capped['id']].include?(r['id']) }.all? { |r| r['runCount'] >= 1 && Time.iso8601(r['nextDue']) > Time.now }
    end
    current = host.state['routines'].to_h { |r| [r['id'], r] }
    expect(current[ordinary['id']]['runCount']).to eq(1)
    expect(current[capped['id']]).to include('runCount' => 1000, 'paused' => true)
    expect(current[paused['id']]['runCount']).to eq(0)
    expect(current[unavailable['id']]['runCount']).to eq(0)
    host.control(owner, 'routine_pause', id: ordinary['id'])
    host.control(owner, 'routine_update', id: capped['id'], minutes: 2, prompt: 'Edited at the limit')
    host.restart
    capped_after_restart = host.state['routines'].find { |r| r['id'] == capped['id'] }
    expect(capped_after_restart).to include('paused' => true, 'runCount' => 1000)
    resumed = host.control(owner, 'routine_resume', id: capped['id'])['routine']
    expect(resumed).to include('paused' => false, 'runCount' => 0)
    expect(host.state['turns'].count { |t| (t['origin'] || {})['routine'] == ordinary['id'] }).to eq(1)
  end

  it 'runs a real minute schedule as an automated task and publishes its result without changing the memo', :slow do
    owner = chief
    memo = host.api('GET', "/api/agents/#{owner}/notes")['content']
    item = routine
    turn = Contract.wait(timeout: 150, description: 'real minute pulse to admit a new schedule') do
      host.state['turns'].find { |t| (t['origin'] || {})['routine'] == item['id'] }
    end
    host.control(owner, 'routine_pause', id: item['id'])
    host.turn(turn['id'], 'done', timeout: 85)
    work = host.api('GET', "/api/agents/#{owner}/tasks?format=json")['tasks'].first
    expect(work).to include('taskType' => 'automated', 'state' => 'Completed', 'result' => 'Verified comparison')
    expect(host.api('GET', "/api/agents/#{owner}/notes")['content']).to eq(memo)
    expect(host.calls.map { |c| c['prompt'] }.join).to include('Memo is read-only by default')
    before = host.state['routines'].first['runCount']
    host.restart
    expect(host.state['routines'].first['runCount']).to eq(before)
    expect(host.calls.size).to eq(2)
  end
end
