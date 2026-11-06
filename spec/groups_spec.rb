require 'sqlite3'

RSpec.describe 'Group collaboration contracts' do
  before do
    @lead, @member, @outsider = %w[Lead Researcher Outside].map { |name| host.agent(name) }
    @group = host.api('POST', '/api/groups', {name: 'Research Group', description: 'Compare products',
      lead: @lead, members: [@lead, @member]}, status: 201)
  end

  def path; "/api/groups/#{@group['id']}"; end
  def group; host.api('GET', path); end
  def edit(**fields); host.api('PUT', path, {revision: group['revision']}.merge(fields)); end
  def message(text, **fields); host.api('POST', path + '/messages', {text: text}.merge(fields), status: 202); end
  def task(**fields); host.api('POST', path + '/tasks', {title: 'Compare products', assignee: @member}.merge(fields), status: 201); end

  {'nonboolean deletion' => {deleted: 'false'}, 'fractional revision' => {revision: 1.5}, 'unknown task state' => {state: 'dnoe'}}.each do |behavior, fields|
    it "rejects #{behavior} without changing the task" do
      work = task
      expect(host.request('PUT', path + "/tasks/#{work['id']}", {revision: work['revision']}.merge(fields)).first).to eq(400)
      expect(group['tasks']).to eq([work])
    end
  end

  it 'keeps the established whitespace semantics for task creation and edits' do
    work = task(title: '  Initial title  ')
    expect(work['title']).to eq('Initial title')
    updated = host.api('PUT', path + "/tasks/#{work['id']}", {revision: work['revision'], title: '  Edited title  '})
    expect(updated['title']).to eq('  Edited title  ')
  end

  it 'requires a creation title and applies the established task defaults' do
    expect(host.request('POST', path + '/tasks', {}).first).to eq(400)
    expect(group['tasks']).to be_empty
    created = host.api('POST', path + '/tasks', {title: 'New task', state: 'done', deleted: true, revision: 99}, status: 201)
    expect(created).to include('title' => 'New task', 'body' => '', 'assignee' => @lead,
      'state' => 'backlog', 'deleted' => false, 'revision' => 1)
  end

  it 'requires the latest revision and distinguishes an omitted state from an explicit state' do
    work = task(state: 'done')
    expect(host.request('PUT', path + "/tasks/#{work['id']}", {body: 'No revision'}).first).to eq(409)
    expect(group['tasks']).to eq([work])
    done = host.api('PUT', path + "/tasks/#{work['id']}", {revision: work['revision'], title: 'Finished objective', state: 'done'})
    expect(done['state']).to eq('done')
    revised = host.api('PUT', path + "/tasks/#{work['id']}", {revision: done['revision'], body: 'Changed objective'})
    expect(revised).to include('state' => 'backlog', 'title' => 'Finished objective', 'body' => 'Changed objective')
    expect(revised['history'].last).to include('state' => 'done', 'revision' => done['revision'])
  end

  it 'restores a deleted task only after assigning a current group member' do
    work = task
    deleted = host.api('PUT', path + "/tasks/#{work['id']}", {revision: work['revision'], deleted: true})
    expect(host.request('PUT', path + "/tasks/#{work['id']}", {revision: deleted['revision'], title: 'Edit while deleted'}).first).to eq(409)
    edit(members: [@lead, @outsider])
    expect(host.request('PUT', path + "/tasks/#{work['id']}", {revision: deleted['revision'], deleted: false}).first).to eq(409)
    expect(host.request('PUT', path + "/tasks/#{work['id']}", {revision: deleted['revision'], deleted: false, assignee: @member}).first).to eq(400)
    restored = host.api('PUT', path + "/tasks/#{work['id']}", {revision: deleted['revision'], deleted: false, assignee: @outsider})
    expect(restored).to include('deleted' => false, 'state' => 'backlog', 'assignee' => @outsider)
  end

  it 'preserves saved task extensions through editing, execution and restart' do
    work = task
    saved = group
    saved['tasks'].first['extension'] = {'kept' => ['legacy', 7]}
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.execute('UPDATE groups SET value = ? WHERE id = ?', [JSON.generate(saved), @group['id']])
    end
    edited = host.api('PUT', path + "/tasks/#{work['id']}", {revision: work['revision'], body: 'Updated body'})
    host.api('POST', path + "/tasks/#{work['id']}/run", {revision: edited['revision']}, status: 202)
    host.idle
    host.restart
    completed = group['tasks'].first
    expect(completed).to include('state' => 'done', 'extension' => {'kept' => ['legacy', 7]})
    expect(completed['history'].first['extension']).to eq('kept' => ['legacy', 7])
    expect(completed['results'].first).to include('stale' => false, 'author' => @member)
  end

  it 'does not start a second execution while a task request is pending' do
    work = task
    host.configure(block: true)
    running = host.api('POST', path + "/tasks/#{work['id']}/run", {revision: work['revision']}, status: 202)
    expect(host.request('POST', path + "/tasks/#{work['id']}/run", {revision: running['revision']}).first).to eq(409)
    expect(group['requests'].count { |r| r['task'] == work['id'] }).to eq(1)
    host.release
    host.idle
  end

  it 'returns failed task work to the backlog and persists the failure result once' do
    work = task
    host.configure(fail: true)
    running = host.api('POST', path + "/tasks/#{work['id']}/run", {revision: work['revision']}, status: 202)
    host.idle
    saved = group['tasks'].first
    expect(saved).to include('state' => 'backlog', 'revision' => running['revision'] + 1)
    expect(saved['results'].size).to eq(1)
    expect(saved['results'].first).to include('stale' => false, 'text' => nil, 'author' => @member)
    expect(saved['results'].first['error']).not_to be_empty
    host.restart
    expect(group['tasks']).to eq([saved])
  end

  it 'introduces the purpose and roster without model calls and persists stable identity after lead transfer' do
    expect(group['messages'].map { |m| m['text'] }.join).to include('Compare products', 'Lead', 'Researcher')
    expect(host.calls).to be_empty
    initial = group
    edit(lead: @member)
    host.restart
    expect(group).to include('id' => initial['id'], 'color' => initial['color'], 'lead' => @member)
    expect(group['strip_order'].first).to eq(@member)
  end

  it 'routes mentions once, does not ping-pong, and keeps private and group contexts separate' do
    host.turn(host.submit(@lead, 'PRIVATE PERSONAL CONTEXT'))
    host.configure(rules: {"#{@lead}:group" => {response: '@Researcher please contribute.'},
      "#{@member}:group" => {response: '@Lead here is the finding.'}})
    message('GROUP ONLY REQUEST')
    host.idle
    requests = group['requests'].reject { |r| r['memo'] }
    expect(requests.map { |r| r['target'] }).to eq([@lead, @member])
    expect(group['messages'].last['author']).to eq(@member)
    expect(host.calls.select { |c| c['node'] == 'group' }.map { |c| c['prompt'] }.join).not_to include('PRIVATE PERSONAL CONTEXT')
    host.configure(rules: {})
    host.turn(host.submit(@lead, 'Personal again'))
    expect(host.calls.last['prompt']).not_to include('GROUP ONLY REQUEST')
    before = group['requests']
    host.restart
    expect(group['requests']).to eq(before)
  end

  it 'updates the shared memo only after member replies and leaves personal notes and chat untouched' do
    shared = File.join(host.state['groups'].find { |g| g['id'] == @group['id'] }['workspace'], 'Notes.html')
    original = '<section id="about">Our group</section><section id="map"></section><section id="content">Existing knowledge</section>'
    File.write(shared, original)
    personal = [@lead, @member].to_h { |id| [id, host.api('GET', "/api/agents/#{id}/notes")['content']] }
    host.configure(rules: {
      "#{@lead}:group" => {response: '@Researcher investigate'},
      "#{@member}:group" => {response: 'Verified member finding'},
      'group-memo' => {response: 'Memo saved.', actions: [{write: shared, content: original.sub('Existing knowledge', 'Existing knowledge; Verified member finding')}]}})
    message('Investigate together')
    host.idle
    expect(host.calls.last['prompt']).to include('Verified member finding', 'Existing knowledge')
    expect(File.read(shared)).to include('Verified member finding')
    expect(group['messages'].map { |m| m['text'] }).not_to include('Memo saved.')
    personal.each do |id, content|
      expect(host.api('GET', "/api/agents/#{id}/notes")['content']).to eq(content)
      expect(host.api('GET', "/api/agents/#{id}/tasks?format=json")['tasks']).to be_empty
    end
    host.restart
    expect(host.calls.size).to eq(3)
    expect(group['requests'].size).to eq(3)
  end

  it 'retries a failed group request without duplicating the admin message or accepted call' do
    host.configure(fail: true)
    message('Try once')
    Contract.wait { group['requests'].first['status'] == 'failed' }
    host.idle
    request = group['requests'].first
    host.configure(fail: false)
    host.api('POST', path + "/calls/#{request['id']}/retry", {})
    host.idle
    expect(group['messages'].count { |m| m['author'] == 'admin' && m['text'] == 'Try once' }).to eq(1)
    expect(group['requests'].reject { |r| r['memo'] }.size).to eq(1)
    expect(group['messages'].last).to include('author' => @lead, 'text' => 'Contract test reply')
  end

  it 'preserves the original author during lead transfer and routes future messages to the new lead' do
    host.configure(block: true)
    message('Already accepted')
    Contract.wait { host.calls.size == 1 }
    edit(lead: @member)
    host.release
    host.idle
    expect(group['messages'].last['author']).to eq(@lead)
    message('For the new lead')
    host.idle
    expect(group['messages'].last['author']).to eq(@member)

    # A memo already queued at the previous Lead must not execute as the new recipient.
    saved = group
    entry = saved.fetch('messages').find { |row| row['text'] == 'Already accepted' }
    memo_id = 'queued-memo-before-lead-transfer'
    origin = {group: saved.fetch('id'), author: 'system', memo: true}
    saved.fetch('requests') << {id: memo_id, target: @lead, status: 'queued', settled: false,
      message: entry.fetch('id'), root: entry.fetch('id'), created: entry.fetch('created'),
      origin: origin, memo: true}
    host.stop
    state_path = File.join(host.root, 'agentpy', 'agents', @lead, 'state.json')
    state = JSON.parse(File.read(state_path))
    state.fetch('turns') << {id: memo_id, flow: 'chat', input: entry.fetch('text'), status: 'queued',
      attempt: 1, created: entry.fetch('created'), batchable: false, origin: origin}
    Contract.atomic_json(state_path, state)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.execute('UPDATE groups SET value = ? WHERE id = ?', [JSON.generate(saved), saved.fetch('id')])
    end
    host.start
    finished = host.turn(memo_id, %w[done failed])
    aggregate_failures('queued memo recipient changed') do
      expect(finished).to include('status' => 'failed',
        'error' => 'Group is archived or membership changed before execution')
      expect(group.fetch('requests').find { |row| row['id'] == memo_id }).to include('target' => @member)
      expect(host.calls.select { |call| call['agency_run'] == memo_id }
        .map { |call| call.slice('owner', 'node', 'agency_run') }).to be_empty
    end
  end

  it 'rejects stale edits, preserves deleted work against late results, and retains task history' do
    work = task
    updated = host.api('PUT', path + "/tasks/#{work['id']}", {revision: work['revision'], title: 'Revised objective'})
    expect(host.request('PUT', path + "/tasks/#{work['id']}", {revision: work['revision'], title: 'Stale'}).first).to eq(409)
    host.configure(block: true)
    running = host.api('POST', path + "/tasks/#{work['id']}/run", {revision: updated['revision']}, status: 202)
    Contract.wait { host.calls.size == 1 }
    deleted = host.api('PUT', path + "/tasks/#{work['id']}", {revision: running['revision'], deleted: true})
    host.release
    host.idle
    saved = group['tasks'].first
    expect(saved).to include('deleted' => true, 'revision' => deleted['revision'])
    expect(saved['results'].first['stale']).to eq(true)
    expect(group['messages'].last['stale']).to eq(true)
    expect(saved['history'].size).to be >= 2
  end

  it 'does not replay completed tasks after archive, restore and process restart' do
    work = task
    host.api('POST', path + "/tasks/#{work['id']}/run", {revision: work['revision']}, status: 202)
    host.idle
    expect(group['tasks'].first['state']).to eq('done')
    edit(archived: true)
    edit(archived: false)
    count = host.calls.size
    host.restart
    expect(group['tasks'].first['state']).to eq('done')
    expect(group['tasks'].first['results'].size).to eq(1)
    expect(host.calls.size).to eq(count)
  end

  it 'allows overlapping membership while enforcing the eleven-active-group limit on create and restore' do
    10.times { |n| host.api('POST', '/api/groups', {name: "Group #{n}", lead: @lead, members: [@lead, @member]}, status: 201) }
    body = {name: 'Twelfth', lead: @lead, members: [@lead, @outsider]}
    expect(host.request('POST', '/api/groups', body).first).to eq(409)
    edit(archived: true)
    host.api('POST', '/api/groups', body, status: 201)
    expect(host.request('PUT', path, {revision: group['revision'], archived: false}).first).to eq(409)
    expect(host.state['groups'].count { |g| !g['archived'] }).to eq(11)
  end

  {
    'only one member' => -> { [@lead] },
    'duplicate member' => -> { [@lead, @lead] },
    'missing lead' => -> { [@member, @outsider] },
    'unknown member' => -> { [@lead, 'missing'] }
  }.each do |behavior, members|
    it "rejects membership with #{behavior}" do
      body = {name: 'Invalid', lead: @lead, members: instance_exec(&members)}
      expect(host.request('POST', '/api/groups', body).first).to eq(behavior == 'unknown member' ? 404 : 400)
      expect(host.state['groups'].size).to eq(1)
    end
  end

  it 'enforces chief, lead and member permissions through the provider control endpoint' do
    gid = @group['id']
    [
      [@member, {op: 'group_create', name: 'Forbidden', lead: @lead, members: [@lead, @member]}],
      [@lead, {op: 'group_update', group: gid, revision: group['revision'], archived: true}],
      [@outsider, {op: 'group_task_create', group: gid, title: 'Intrusion'}],
      [@outsider, {op: 'workspace_open', group: gid, url: 'https://example.com'}]
    ].each do |owner, data|
      expect(host.request('POST', "/api/agents/#{owner}/control", data).first).to eq(403)
    end
    created = host.control(@member, 'group_task_create', group: gid, title: 'Member initiative', assignee: @lead)
    expect(created['author']).to eq(@member)
    expect(host.request('POST', "/api/agents/#{chief}/control", {op: 'retire_agent', target: @lead}).first).to eq(409)
  end

  it 'scopes shared attachments and browser tabs to their group' do
    attachment = host.api('POST', path + '/attachments', {kind: 'link', value: 'https://example.com/reference'}, status: 201)
    other = host.api('POST', '/api/groups', {name: 'Other', lead: @lead, members: [@lead, @member]}, status: 201)
    expect(host.request('POST', "/api/groups/#{other['id']}/messages", {text: 'Steal', attachments: [attachment['id']]}).first).to eq(400)
    message('', attachments: [attachment['id']])
    host.idle
    expect(host.calls.first['prompt']).to include(attachment['value'])
    host.control(@member, 'workspace_open', group: @group['id'], url: 'https://example.com/shared')
    expect(host.control(@member, 'workspace')['tabs']).to be_empty
    expect(host.control(@member, 'workspace', group: @group['id'])['tabs'].first['url']).to eq('https://example.com/shared')
  end
  it 'rejects removal of the designated Lead without changing persisted membership' do
    before = group
    expect(host.request('PUT', path, {revision: before['revision'], members: [@member, @outsider]}).first).to eq(400)
    expect(group).to eq(before)
    host.restart
    expect(group).to eq(before)
  end

  it 'persists leadership and membership together while retaining unknown group metadata' do
    saved = group.merge('extension' => {'keep' => [1, 'two']})
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.execute('UPDATE groups SET value=? WHERE id=?', [JSON.generate(saved), @group['id']])
    end
    edit(lead: @outsider, members: [@member, @outsider])
    updated = group
    expect(updated).to include('lead' => @outsider, 'members' => [@member, @outsider], 'extension' => saved['extension'])
    expect(updated['strip_order'].first).to eq(@outsider)
    host.restart
    expect(group).to eq(updated)
  end

end
