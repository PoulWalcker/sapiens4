require 'sqlite3'
require 'time'

RSpec.describe 'Daily routine schedules', :host do
  before do
    @owner = host.agent('CalendarClock')
    @fields = {title: 'Daily comparison', goal: 'Compare the products.', criteria: ['Return a sourced comparison.']}
  end

  def create(operation)
    host.api('POST', "/api/agents/#{@owner}/routines", @fields.merge(operation: operation)).fetch('routine')
  end

  def definition(id)
    host.state.fetch('personal_routines').find { |row| row['id'] == id }
  end

  def schedule(id, operation, **rule)
    host.api('POST', "/api/agents/#{@owner}/routines/#{id}/schedule",
      {operation: operation, revision: definition(id).fetch('revision')}.merge(rule))
  end

  def daily(time = '09:00', zone = 'Asia/Dubai')
    {kind: 'daily', time: time, timezone: zone}
  end

  def save_installation(routines)
    host.stop
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      routines.each do |routine|
        db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?',
          [JSON.generate(routine), routine.fetch('id').split('_').last.to_i])
      end
    end
    host.start
  end

  it 'previews DST without effects and preserves contracts through daily, interval and manual edits' do
    id = create('create')
    before = host.state.slice('personal_routines', 'routines', 'events')
    cases = {
      'spring gap shifts by the gap' => [daily('02:30', 'Europe/Berlin'), '2027-03-28T00:00:00+00:00', '2027-03-28T01:30:00+00:00'],
      'fall fold chooses the first time' => [daily('02:30', 'Europe/Berlin'), '2027-10-30T22:00:00+00:00', '2027-10-31T00:30:00+00:00'],
      'between fold occurrences waits for tomorrow' => [daily('02:30', 'Europe/Berlin'), '2027-10-31T00:45:00+00:00', '2027-11-01T01:30:00+00:00'],
      'Dubai keeps the selected local time' => [daily, '2027-01-01T04:59:00+00:00', '2027-01-01T05:00:00+00:00']
    }
    cases.each do |name, (rule, after, expected)|
      result = host.control(@owner, 'routine_schedule_preview', schedule: rule, after: after)
      expect(result.fetch('nextDue')).to eq(expected), name
      expect(Time.iso8601(result.fetch('nextDue'))).to be > Time.iso8601(after)
    end
    expect(host.control(@owner, 'routine_schedule_preview', trigger: 'manual')['nextDue']).to be_nil
    expect(host.state.slice('personal_routines', 'routines', 'events')).to eq(before)
    expect(host.calls).to be_empty

    invalid = {
      'unknown timezone' => {schedule: daily('12:00', 'Invalid/Zone')},
      'missing timezone' => {schedule: {kind: 'daily', time: '12:00'}},
      'invalid local time' => {schedule: daily('24:00')},
      'mixed interval and daily rule' => {minutes: 60, schedule: daily},
      'mixed manual and interval rule' => {trigger: 'manual', minutes: 60}
    }
    invalid.each do |name, rule|
      code, = host.request('POST', "/api/agents/#{@owner}/routines/#{id}/schedule",
        {operation: name, revision: 1}.merge(rule))
      expect(code).to eq(400), name
      expect(definition(id)).to include('trigger' => 'manual', 'revision' => 1, 'contractGeneration' => 1)
    end
    expect(host.request('POST', "/api/agents/#{@owner}/control",
      op: 'routine_schedule_preview', schedule: daily, after: '2027-01-01T12:00:00').first).to eq(400)
    expect(host.state.fetch('personal_routines').size).to eq(1)

    run = host.api('POST', "/api/agents/#{@owner}/routines/#{id}/run", operation: 'ordinary', revision: 1).fetch('run')
    host.turn(run)
    path = "/api/agents/#{@owner}/routines/#{id}/runs/#{run}"
    binding = host.api('GET', path).fetch('executionBinding')
    original_snapshot = host.api('GET', path).fetch('snapshot')
    expect(binding.fetch('status')).to eq('pinned')
    [daily, {minutes: 60}, {trigger: 'manual'}].each_with_index do |rule, index|
      payload = index.zero? ? {schedule: rule} : rule
      schedule(id, "switch-#{index}", **payload)
      saved = definition(id)
      expect(saved).to include('id' => id, 'contractGeneration' => 1)
      if index.zero?
        expect(saved.fetch('schedule')).to eq(JSON.parse(JSON.generate(daily)))
        expect(saved).not_to have_key('minutes')
      elsif index == 1
        expect(saved.fetch('minutes')).to eq(60)
        expect(saved).not_to have_key('schedule')
      else
        expect(saved.fetch('trigger')).to eq('manual')
        %w[minutes schedule nextDue paused].each { |key| expect(saved).not_to have_key(key) }
      end
      if index.zero?
        held_due = '2020-01-01T12:00:00.000000+00:00'
        save_installation([saved.merge('nextDue' => held_due, 'paused' => true, 'extension' => {'kept' => true})])
        host.control(@owner, 'routine_update', id: id, schedule: daily, prompt: @fields.fetch(:goal))
        saved = definition(id)
        expect(saved).to include('nextDue' => held_due, 'paused' => true, 'extension' => {'kept' => true})
      end
      host.restart
      expect(definition(id)).to eq(saved)
      expect(host.api('GET', path).fetch('executionBinding')).to eq(binding)
      expect(host.api('GET', path).fetch('snapshot')).to eq(original_snapshot)
    end
    expect(host.request('POST', "/api/agents/#{@owner}/routines/#{id}/schedule",
      operation: 'stale', revision: 1, minutes: 5).first).to eq(409)
    legacy = definition(id).merge('minutes' => 17, 'nextDue' => '2030-01-01T12:00:00.000000+00:00', 'paused' => true)
    save_installation([legacy])
    expect(definition(id)).to include('trigger' => 'manual', 'contractGeneration' => 1)
    expect(host.state.fetch('routines').none? { |row| row['id'] == id }).to be(true)
  end

  it 'admits overdue daily work once through the real pulse and retains its next due after restart', :slow do
    daily_id, interval_id, paused_id = create('daily'), create('interval'), create('paused')
    schedule(daily_id, 'enable-daily', schedule: daily('09:00', 'Etc/UTC'))
    schedule(interval_id, 'enable-interval', minutes: 60)
    schedule(paused_id, 'enable-paused', schedule: daily)
    host.control(@owner, 'routine_pause', id: paused_id)
    all = host.state.fetch('routines')
    overdue = (Time.now.utc - 864_000).iso8601(6).sub('Z', '+00:00')
    all.each { |routine| routine['nextDue'] = overdue }
    save_installation(all)
    Contract.wait(timeout: 85, description: 'real minute pulse for overdue daily and interval work') do
      [daily_id, interval_id].all? do |id|
        routine = definition(id)
        routine.fetch('runCount') == 1 && Time.iso8601(routine.fetch('nextDue')) > Time.now
      end
    end
    Contract.wait(description: 'accepted scheduled routine results') do
      [daily_id, interval_id].all? do |id|
        run = definition(id).fetch('runs').last
        run && run['status'] == 'done' && run['result'] == 'Verified comparison'
      end
    end
    rows = host.state.fetch('routines').to_h { |row| [row.fetch('id'), row] }
    expect(rows.fetch(paused_id)).to include('paused' => true, 'runCount' => 0, 'nextDue' => overdue)
    expect(rows.fetch(daily_id).fetch('schedule')).to eq(JSON.parse(JSON.generate(daily('09:00', 'Etc/UTC'))))
    daily_due = Time.iso8601(rows.fetch(daily_id).fetch('nextDue'))
    expect(daily_due.utc.strftime('%H:%M')).to eq('09:00')
    runs = [daily_id, interval_id].to_h { |id| [id, definition(id).fetch('runs').map { |run| run.fetch('id') }] }
    runs.each_value { |ids| expect(ids.size).to eq(1) }
    host.restart
    after = host.state.fetch('routines').to_h { |row| [row.fetch('id'), row] }
    [daily_id, interval_id, paused_id].each { |id| expect(after.fetch(id)).to eq(rows.fetch(id)) }
    runs.each { |id, ids| expect(definition(id).fetch('runs').map { |run| run.fetch('id') }).to eq(ids) }
  end
end
