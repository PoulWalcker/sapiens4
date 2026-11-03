require 'digest'
require 'sqlite3'
require 'yaml'
require_relative 'support/effect_peer'

RSpec.describe 'Receipt-aware routine feed and delivery' do
  GOAL = 'Summarize newly observed feed records and their values without inventing records.'.freeze
  CRITERIA = ['Name every new record ID and its supplied value; do not repeat previously accepted records.'].freeze

  before do
    @host = Host.new(start: false)
    @peer = EffectPeer.new(File.join(host.root, 'independent-peer'))
    @peer.page('c0', records: [{id: 'r1', value: 10}, {id: 'r2', value: 20}], next_cursor: 'c2')
    @peer.page('c2', records: [{id: 'r2', value: 20}, {id: 'r3', value: 30}], next_cursor: 'page:next-z9')
    @peer.page('page:next-z9', records: [], next_cursor: 'page:next-z9')
    host.configure(version: 'codex-cli 0.162.1')
    host.start
    @owner = host.agent('FeedOwner')
    @profile = {kind: 'receipt_feed_v1', taskBoundary: 'content_result', executionPolicy: 'response_only', source: {url: @peer.url + '/feed', identity: 'sales-feed', initialCursor: 'c0'}, delivery: {url: @peer.url + '/deliveries', receiptUrl: @peer.url + '/receipts', identity: 'daily-inbox'}}
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", operation: 'create', title: 'Feed summary', goal: GOAL, criteria: CRITERIA, externalProfile: @profile).fetch('routine')
    configure_summary('New records: r1 = 10; r2 = 20.')
  end

  after do
    @peer&.release
    begin
      @host&.stop
    ensure
      @peer&.close
      @host&.close
    end
  end

  def path
    "/api/agents/#{@owner}/routines/#{@routine}"
  end

  def routine
    host.state.fetch('personal_routines').find { |row| row['id'] == @routine }
  end

  def configure_summary(reply, **rule)
    host.configure(rules: {'Execution' => {response: decision('Outcome', reply: reply, outcome: {status: 'Completed', satisfiedCriteria: CRITERIA, artifacts: []}), usage: {input_tokens: 43, cached_input_tokens: 3, output_tokens: 17}}.merge(rule)})
  end

  def submit(operation)
    host.api('POST', "#{path}/run", operation: operation, revision: 1).fetch('run')
  end

  def detail(run)
    host.api('GET', "#{path}/runs/#{run}")
  end

  def settled(run)
    host.turn(run, %w[done failed interrupted cancelled])
    detail(run)
  end

  def reconcile(run, operation = 'reconcile')
    host.api('POST', "#{path}/runs/#{run}/reconcile-delivery", operation: operation)
  end

  def canonical(value)
    case value
    when Hash then value.keys.sort.to_h { |key| [key, canonical(value.fetch(key))] }
    when Array then value.map { |item| canonical(item) }
    else value
    end
  end

  it 'retains one actual delivery across lost ACK and restart, reconciles independently and preserves opaque cursor progress' do
    first = submit('initial')
    first_detail = settled(first)
    expect(first_detail.dig('outcome', 'status')).to eq('Completed'), JSON.pretty_generate(first_detail)
    expect(first_detail.dig('externalExecution', 'read')).to include('status' => 'received', 'newIds' => %w[r1 r2])
    expect(first_detail.dig('externalExecution', 'contentAcceptance', 'status')).to eq('accepted')
    expect(first_detail.dig('externalExecution', 'cursorCommit', 'status')).to eq('committed')
    expect(first_detail.dig('externalExecution', 'delivery', 'status')).to eq('confirmed')
    expect(routine.fetch('sourceState')).to include('cursor' => 'c2', 'seenIds' => %w[r1 r2])
    expect(@peer.deliveries.length).to eq(1)
    first_request = @peer.deliveries.first.fetch('request')
    expect(first_request.fetch('recordIds')).to eq(%w[r1 r2])
    expect(first_request.fetch('content')).to eq('text' => 'New records: r1 = 10; r2 = 20.')
    expect(first_request.fetch('requestHash')).to eq(Digest::SHA256.hexdigest(JSON.generate(canonical(first_request.reject { |key, _| key == 'requestHash' }))))
    @peer.delivery_mode = :drop_ack
    configure_summary('New record: r3 = 30.')
    second = submit('overlap-lost-ack')
    lost = settled(second)
    expect(lost.dig('outcome', 'status')).to eq('Completed')
    expect(lost.dig('externalExecution', 'delivery', 'status')).to eq('unknown')
    expect(routine.fetch('sourceState')).to include('cursor' => 'page:next-z9', 'seenIds' => %w[r1 r2 r3])
    expect(@peer.deliveries.length).to eq(2)
    second_request = @peer.deliveries.last.fetch('request')
    expect(second_request.fetch('recordIds')).to eq(['r3'])
    expect(lost.dig('externalExecution', 'read', 'newIds')).to eq(['r3'])
    expect(second_request.fetch('content')).to eq('text' => 'New record: r3 = 30.')
    expect(second_request.fetch('operationKey')).not_to eq(first_request.fetch('operationKey'))
    expect(host.request('POST', "#{path}/run", operation: 'blocked-new-key', revision: 1).first).to eq(409)
    expect(host.request('POST', "/api/agents/#{@owner}/turns/#{second}/retry", {}).first).to eq(409)
    host.restart
    expect(submit('overlap-lost-ack')).to eq(second)
    expect(detail(second).dig('externalExecution', 'delivery', 'status')).to eq('unknown')
    expect(@peer.deliveries.length).to eq(2)
    posts_before = @peer.requests.count { |request| request[:method] == 'POST' }
    reconcile(second)
    expect(detail(second).dig('externalExecution', 'delivery', 'status')).to eq('confirmed')
    expect(@peer.requests.count { |request| request[:method] == 'POST' }).to eq(posts_before)
    expect(@peer.requests.last).to include(method: 'GET', path: '/receipts', query: {'operationKey' => second_request.fetch('operationKey')})
    reconcile(second)
    expect(@peer.deliveries.length).to eq(2)
    configure_summary('No new records were observed in the complete source page.')
    empty = settled(submit('empty'))
    expect(empty.dig('outcome', 'status')).to eq('Completed')
    expect(routine.fetch('sourceState')).to include('cursor' => 'page:next-z9', 'seenIds' => %w[r1 r2 r3])
    expect(@peer.deliveries.length).to eq(2)
    calls = host.calls.select { |call| call['node'] == 'Execution' }
    expect(calls.length).to eq(3)
    expect(calls).to all(include('args' => include('--strict-config', '--sandbox', 'read-only')))
    expect(first_detail.dig('executionBinding', 'contract', 'completionCriteria')).to eq(CRITERIA)
  end

  it 'does not treat missing, unavailable or mismatched receipt evidence as proof and preserves the first confirmed receipt' do
    @peer.delivery_mode = :drop_ack
    id = submit('lost')
    expect(settled(id).dig('externalExecution', 'delivery', 'status')).to eq('unknown')
    original = @peer.deliveries.first.fetch('receipt')
    [:not_found, :unavailable].each do |mode|
      @peer.receipt_mode = mode
      reconcile(id, "lookup-#{mode}")
      expect(detail(id).dig('externalExecution', 'delivery', 'status')).to eq('unknown')
    end
    @peer.receipt_mode = :confirm
    %w[operationKey scope profileHash requestHash receiver].each do |field|
      @peer.receipt_override = original.merge(field => 'foreign')
      reconcile(id, "wrong-#{field}")
      expect(detail(id).dig('externalExecution', 'delivery', 'status')).to eq('unknown')
    end
    @peer.receipt_override = nil
    reconcile(id, 'positive-proof')
    expect(detail(id).dig('externalExecution', 'delivery', 'receipt')).to eq(original)
    @peer.receipt_override = original.merge('receiptId' => 'conflicting-second-receipt')
    reconcile(id, 'conflicting-proof')
    expect(detail(id).dig('externalExecution', 'delivery')).to include('status' => 'confirmed', 'receipt' => original)
    expect(@peer.deliveries.length).to eq(1)
    expect(@peer.requests.count { |request| request[:method] == 'POST' }).to eq(1)
  end

  it 'never follows a delivery redirect or automatically retries its reserved write' do
    @peer.delivery_mode = :redirect
    id = submit('redirect')
    expect(settled(id).dig('externalExecution', 'delivery', 'status')).to eq('unknown')
    expect(@peer.deliveries).to be_empty
    expect(@peer.requests.count { |request| request[:method] == 'POST' }).to eq(1)
    expect(@peer.requests.none? { |request| request[:path] == '/redirected-delivery' }).to be(true)
    host.restart
    expect(detail(id).dig('externalExecution', 'delivery', 'status')).to eq('unknown')
    expect(@peer.requests.count { |request| request[:method] == 'POST' }).to eq(1)
  end

  %w[Stop host_crash].each do |event|
    it "preserves a real peer commit after #{event} while its acknowledgement is held" do
      @peer.delivery_mode = :hold_after_commit
      id = submit('held-ack')
      Contract.wait { @peer.deliveries.length == 1 }
      reservation = detail(id).dig('externalExecution', 'delivery')
      expect(reservation).to include('status' => 'started')
      expect(reservation.fetch('request')).to eq(@peer.deliveries.first.fetch('request'))
      if event == 'Stop'
        host.api('POST', "#{path}/stop", operation: 'stop-started', revision: 1, run: id)
        Contract.wait { detail(id).dig('externalExecution', 'delivery', 'status') == 'unknown' }
        @peer.release
        settled(id)
      else
        host.crash
        @peer.release
        host.start
      end
      expect(detail(id).dig('externalExecution', 'delivery', 'status')).to eq('unknown')
      expect(@peer.deliveries.length).to eq(1)
      reconcile(id)
      expect(detail(id).dig('externalExecution', 'delivery', 'status')).to eq('confirmed')
      expect(@peer.deliveries.length).to eq(1)
      expect(@peer.requests.count { |request| request[:method] == 'POST' }).to eq(1)
    end
  end

  %w[unavailable incomplete oversized].each do |fault|
    it "retains the original cursor and does not deliver after a #{fault} source page" do
      @peer.page('c0', records: [{id: 'r1', value: fault == 'oversized' ? 'x' * 70_000 : 10}], next_cursor: 'must-not-commit', complete: fault != 'incomplete', status: fault == 'unavailable' ? 503 : 200)
      id = submit('bad-source')
      result = settled(id)
      expect(result.dig('externalExecution', 'cursorCommit', 'status')).not_to eq('committed')
      expect(routine.dig('sourceState', 'cursor')).to eq('c0')
      expect(routine.dig('sourceState', 'seenIds')).to eq([])
      expect(@peer.deliveries).to be_empty
      expect(host.calls.none? { |call| call['node'] == 'Execution' }).to be(true)
    end
  end

  it 'does not advance source progress when the content Task is unresolved' do
    host.configure(rules: {'Execution' => {response: decision('Outcome', reply: 'Unable to verify the supplied record values.', outcome: {status: 'Unresolved', satisfiedCriteria: [], artifacts: []}), usage: {input_tokens: 43, cached_input_tokens: 3, output_tokens: 17}}})
    id = submit('unresolved-content')
    result = settled(id)
    expect(result.dig('outcome', 'status')).to eq('Unresolved')
    expect(result.dig('externalExecution', 'read', 'status')).to eq('received')
    expect(result.dig('externalExecution', 'cursorCommit', 'status')).not_to eq('committed')
    expect(routine.dig('sourceState', 'cursor')).to eq('c0')
    expect(@peer.deliveries).to be_empty
  end

  it 'distinguishes incomplete normal termination and Stop from positive opaque-effect evidence' do
    configure_summary('New records: r1 = 10; r2 = 20.', usage: nil)
    incomplete = settled(submit('missing-turn-end'))
    expect(incomplete.dig('outcome', 'status')).not_to eq('Completed')
    expect(incomplete.dig('externalExecution', 'opaqueEffectUnknown')).to be_nil
    expect(incomplete.fetch('error')).to include('complete terminal evidence')
    expect(@peer.deliveries).to be_empty
    expect(routine.dig('sourceState', 'cursor')).to eq('c0')
    configure_summary('New records: r1 = 10; r2 = 20.', block: true, gate: 'response-only-stop')
    id = submit('before-content')
    Contract.wait { host.calls.count { |call| call['node'] == 'Execution' } == 2 }
    host.api('POST', "#{path}/stop", operation: 'stop-content', revision: 1, run: id)
    host.release('response-only-stop')
    result = settled(id)
    expect(result.dig('externalExecution', 'opaqueEffectUnknown')).to be_nil
    expect(@peer.deliveries).to be_empty
    expect(routine.dig('sourceState', 'cursor')).to eq('c0')
    configure_summary('New records: r1 = 10; r2 = 20.')
    valid = settled(submit('valid-after-incomplete-and-stop'))
    expect(valid.dig('externalExecution', 'delivery', 'status')).to eq('confirmed')
    expect(@peer.deliveries.length).to eq(1)
  end


  it 'fails closed on positive tool lifecycle evidence even when the model proposes successful content' do
    events = [{type: 'item.started', item: {id: 'unexpected-tool', type: 'command_execution', command: 'unexpected tool proposal'}},
              {type: 'item.completed', item: {id: 'unexpected-tool', type: 'command_execution', exit_code: 0}}]
    configure_summary('New records: r1 = 10; r2 = 20.', events: events)
    id = submit('opaque-protocol')
    result = settled(id)
    expect(result.dig('externalExecution', 'opaqueEffectUnknown')).not_to be_nil
    expect(@peer.deliveries).to be_empty
    expect(routine.dig('sourceState', 'cursor')).to eq('c0')
    expect(host.request('POST', "#{path}/run", operation: 'opaque-new-key', revision: 1).first).to eq(409)
    host.restart
    expect(detail(id).dig('externalExecution', 'opaqueEffectUnknown')).not_to be_nil
    expect(host.request('POST', "#{path}/run", operation: 'opaque-after-restart', revision: 1).first).to eq(409)
  end

  it 'retains the same-consumer fence after profile removal even for Retry of an older ordinary run' do
    fields = {title: 'Feed summary', goal: GOAL, criteria: CRITERIA}
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", fields.merge(operation: 'plain-create')).fetch('routine')
    configure_summary('Never delivered', fail: true)
    old = submit('old-ordinary')
    host.turn(old, 'failed')
    host.api('POST', "#{path}/edit", fields.merge(operation: 'enable-profile', revision: 1, externalProfile: @profile))
    configure_summary('New records: r1 = 10; r2 = 20.')
    @peer.delivery_mode = :drop_ack
    id = host.api('POST', "#{path}/run", operation: 'new-profile', revision: 2).fetch('run')
    expect(settled(id).dig('externalExecution', 'delivery', 'status')).to eq('unknown')
    host.api('POST', "#{path}/edit", fields.merge(operation: 'remove-profile', revision: 2, externalProfile: nil))
    expect(host.request('POST', "#{path}/run", operation: 'bypass-profile', revision: 3).first).to eq(409)
    expect(host.request('POST', "/api/agents/#{@owner}/turns/#{old}/retry", {}).first).to eq(409)
    other = host.api('POST', "/api/agents/#{@owner}/routines", fields.merge(operation: 'unrelated-create')).fetch('routine')
    other_run = host.api('POST', "/api/agents/#{@owner}/routines/#{other}/run", operation: 'unrelated-run', revision: 1).fetch('run')
    host.turn(other_run)
    expect(@peer.deliveries.length).to eq(1)
    reconcile(id, 'original-pinned-endpoint')
    expect(detail(id).dig('externalExecution', 'delivery', 'status')).to eq('confirmed')
  end

  it 'recovers an explicitly saved prepared-but-not-started installation as not sent without replay' do
    # Saved installation boundary, NOT a claimed timed process kill before socket send.
    id = submit('saved-boundary-source')
    expect(settled(id).dig('externalExecution', 'delivery', 'status')).to eq('confirmed')
    baseline = @peer.deliveries.length
    posts = @peer.requests.count { |request| request[:method] == 'POST' }
    host.stop
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      row = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', id))
      delivery = row.fetch('externalExecution').fetch('delivery')
      delivery['status'] = 'prepared'
      delivery.delete('receipt')
      db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(row), id])
    end
    host.start
    expect(detail(id).dig('externalExecution', 'delivery', 'status')).to eq('not_sent')
    expect(@peer.deliveries.length).to eq(baseline)
    expect(@peer.requests.count { |request| request[:method] == 'POST' }).to eq(posts)
    expect(host.request('POST', "#{path}/runs/#{id}/reconcile-delivery", operation: 'not-redelivery').first).to eq(409)
  end


  it 'replays one admitted operation and rejects changed inputs under the same operation identity' do
    configure_summary('New records: r1 = 10; r2 = 20.', block: true, gate: 'identity')
    id = submit('stable-operation')
    Contract.wait { host.calls.any? { |call| call['node'] == 'Execution' } }
    expect(submit('stable-operation')).to eq(id)
    expect(host.request('POST', "#{path}/run", operation: 'stable-operation', revision: 1, inputs: 'changed payload').first).to eq(409)
    host.release('identity')
    expect(settled(id).dig('externalExecution', 'delivery', 'status')).to eq('confirmed')
    expect(@peer.deliveries.length).to eq(1)
    expect(host.calls.count { |call| call['node'] == 'Execution' }).to eq(1)
  end

  it 'reconciles the original pinned endpoint after deletion without undoing a real external commit' do
    @peer.delivery_mode = :hold_after_commit
    id = submit('delete-after-start')
    Contract.wait { @peer.deliveries.length == 1 }
    host.api('POST', "#{path}/delete", operation: 'delete-started', revision: 1)
    Contract.wait { detail(id).dig('externalExecution', 'delivery', 'status') == 'unknown' }
    @peer.release
    settled(id)
    expect(detail(id).dig('externalExecution', 'delivery', 'status')).to eq('unknown')
    reconcile(id, 'historical-receipt')
    expect(detail(id).dig('externalExecution', 'delivery', 'status')).to eq('confirmed')
    expect(@peer.deliveries.length).to eq(1)
  end

  it 'does not accept a stale source observation after a profile is removed and restored during content generation' do
    configure_summary('New records: r1 = 10; r2 = 20.', block: true, gate: 'profile-aba')
    id = submit('old-profile-observation')
    Contract.wait { host.calls.any? { |call| call['node'] == 'Execution' } }
    fields = {title: 'Feed summary', goal: GOAL, criteria: CRITERIA}
    host.api('POST', "#{path}/edit", fields.merge(operation: 'remove-aba', revision: 1, externalProfile: nil))
    host.api('POST', "#{path}/edit", fields.merge(operation: 'restore-aba', revision: 2, externalProfile: @profile))
    host.release('profile-aba')
    result = settled(id)
    expect(result.dig('outcome', 'status')).not_to eq('Completed')
    expect(result.dig('externalExecution', 'cursorCommit', 'status')).not_to eq('committed')
    expect(result.dig('externalExecution', 'read', 'newIds')).to eq(%w[r1 r2])
    expect(@peer.deliveries).to be_empty
    expect(routine.dig('sourceState', 'cursor')).to eq('c0')
  end


  it 'rejects a retained real observation against explicitly advanced saved source state without rewinding progress' do
    # Saved-state conflict injection, not simultaneous same-owner scheduler execution.
    configure_summary('New records: r1 = 10; r2 = 20.', block: true, gate: 'saved-cursor-conflict')
    id = submit('stale-saved-observation')
    Contract.wait { host.calls.any? { |call| call['node'] == 'Execution' } }
    observed = detail(id).dig('externalExecution', 'read')
    expect(observed.fetch('newIds')).to eq(%w[r1 r2])
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      sequence = @routine.split('_').last.to_i
      row = JSON.parse(db.get_first_value('SELECT value FROM scheduled_routines WHERE sequence=?', sequence))
      state = row.fetch('sourceState')
      state['revision'] += 1
      state['cursor'] = 'opaque:newer-progress'
      state['seenIds'] = %w[r1 r2 already-newer]
      db.execute('UPDATE scheduled_routines SET value=? WHERE sequence=?', [JSON.generate(row), sequence])
    end
    host.release('saved-cursor-conflict')
    result = settled(id)
    expect(result.dig('outcome', 'status')).to eq('Unresolved')
    expect(result.dig('externalExecution', 'cursorCommit', 'status')).to eq('conflict')
    expect(result.dig('externalExecution', 'read')).to eq(observed)
    task = YAML.safe_load(host.api('GET', "/api/agents/#{@owner}/tasks")).fetch('tasks').find { |row| row['workloadId'] == id }
    expect(task.fetch('decisions').last.dig('response', 'outcome', 'status')).to eq('Completed')
    expect(task.dig('outcome', 'status')).to eq('Unresolved')
    expect(routine.fetch('sourceState')).to include('cursor' => 'opaque:newer-progress', 'seenIds' => %w[r1 r2 already-newer])
    expect(@peer.deliveries).to be_empty
  end

end
