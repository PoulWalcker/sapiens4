require_relative 'desktop'
require 'sqlite3'
require 'digest/sha1'
require 'time'

module RoutineGroupRecovery
  def group_record
    host.api('GET', "/api/groups/#{@group_id}")
  end

  def group_routine
    host.state.fetch('routines').find { |row| row['id'] == @routine_id }
  end

  def group_message(text)
    desktop.fill('Message Recovery Group', text)
    desktop.click('Send message')
    desktop.value('Message Recovery Group', '')
  end

  def chief_request(text, actions)
    host.configure(messages: [{contains: text, node: 'Execution', rule: {actions: actions}}])
    desktop.fill('Message ChiefSapi', text)
    desktop.click('Send message')
    desktop.value('Message ChiefSapi', '')
    host.idle
  end

  def group_occurrences
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3'), readonly: true) do |db|
      db.execute('SELECT id,status,value FROM routine_occurrences').filter_map do |id, status, value|
        row = JSON.parse(value)
        {id: id, status: status, value: row} if row.dig('routine', 'id') == @routine_id
      end
    end
  end

  # UUID v5 is the saved-installation identity format, not a scheduler oracle.
  def saved_due_id(routine, due)
    namespace = ['6ba7b8119dad11d180b400c04fd430c8'].pack('H*')
    bytes = Digest::SHA1.digest(namespace + "#{routine}:#{due}").bytes.first(16)
    bytes[6] = (bytes[6] & 15) | 80
    bytes[8] = (bytes[8] & 63) | 128
    bytes.pack('C*').unpack1('H*')
  end

  def install_group_recovery_fixture(routine, old_lead)
    raise 'Saved installation fixture requires a stopped host' if host.process
    observed = Time.now.utc
    stamp = ->(time) { time.iso8601(6).sub('Z', '+00:00') }
    admitted_due = stamp.call(observed - 12 * 60)
    first_deferred = stamp.call(observed - 10 * 60)
    last_deferred = stamp.call(observed - 6 * 60)
    @admitted_id = saved_due_id(routine.fetch('id'), admitted_due)
    @deferred_id = saved_due_id(routine.fetch('id'), first_deferred)
    target = {kind: 'group', group: @group_id, agent: old_lead}
    snapshot = routine.merge('nextDue' => admitted_due, 'runCount' => 0, 'paused' => false)
    admitted = {id: @admitted_id, routine: snapshot, target: target, created: stamp.call(observed - 11 * 60)}
    deferred = {id: @deferred_id, routine: snapshot.merge('nextDue' => first_deferred), target: target,
      created: stamp.call(observed - 5 * 60), through: last_deferred, reason: {kind: 'active_run', run: @admitted_id}}
    current = routine.merge('nextDue' => stamp.call(observed + 5 * 60), 'runCount' => 1)
    # Labelled persisted installation input, not an actual crash. Admission is
    # durable but Group materialization has not begun: no half-written Group
    # message/task/request transaction is invented. The compact deferred row
    # represents exactly three two-minute historical deadlines.
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      raise 'Only current schema 9 is supported' unless db.get_first_value('PRAGMA user_version') == 9
      db.transaction do
        db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?',
          [JSON.generate(current), routine.fetch('id').split('_').last.to_i])
        db.execute("INSERT INTO routine_occurrences VALUES (?,?,'admitted_pending')", [@admitted_id, JSON.generate(admitted)])
        db.execute("INSERT INTO routine_occurrences VALUES (?,?,'deferred')", [@deferred_id, JSON.generate(deferred)])
      end
    end
    {admitted_due: admitted_due, first_deferred: first_deferred, last_deferred: last_deferred, admitted: admitted}
  end

  def effect_entries
    File.exist?(@journal) ? File.readlines(@journal).map { |line| JSON.parse(line) } : []
  end

  def await_group_result(identity, description:)
    Contract.wait(timeout: 150, description: description) do
      group = group_record
      request = group.fetch('requests').find { |row| row['id'] == identity }
      raise "Group execution failed: #{request.inspect}" if request && %w[failed interrupted cancelled].include?(request['status'])
      group if request && request['settled'] && request['status'] == 'done' &&
        group.fetch('messages').any? { |row| row['turn'] == identity }
    end
  end
end
