require 'sqlite3'
require 'digest'

RSpec.describe 'Workspace and memory contracts' do
  def memo(content)
    '<section id="about">Research assistant</section><section id="map">Saved facts</section><section id="content">' + content + '</section>'
  end

  it 'preserves tabs, bookmarks, zoom and file contents through restart and close' do
    owner = chief
    file = File.join(host.workspace(owner), 'notes with spaces 😎 #.md')
    File.write(file, "# Notes\n<script>literal</script>")
    tab = host.control(owner, 'workspace_open', path: file)['workspace']['tabs'].first
    host.control(owner, 'workspace_zoom', factor: 1.5)
    2.times { host.control(owner, 'workspace_bookmark') }
    expect(host.control(owner, 'workspace')['bookmarks'].size).to eq(1)
    host.control(owner, 'workspace_open', url: 'https://example.com')
    host.control(owner, 'workspace_focus', id: tab['id'])
    host.control(owner, 'workspace_reload')
    host.restart
    workspace = host.control(owner, 'workspace')
    expect(workspace['active_tab']).to eq(tab['id'])
    expect(workspace['tabs'].first).to include('zoom' => 1.5)
    expect(File.identical?(workspace['tabs'].first['path'], file)).to eq(true)
    expect(File.identical?(workspace['bookmarks'].first['path'], file)).to eq(true)
    host.control(owner, 'workspace_close', id: tab['id'])
    expect(File.read(file)).to eq("# Notes\n<script>literal</script>")
    expect(host.control(owner, 'workspace')['tabs'].size).to eq(1)
  end

  it 'rejects invalid edits atomically and selects the last remaining tab on close' do
    owner = chief
    first = host.control(owner, 'workspace_open', url: 'https://example.com')['workspace']['tabs'].first
    second = host.control(owner, 'workspace_open', url: 'about:blank')['workspace']['tabs'].last
    before = host.control(owner, 'workspace')
    expect(host.request('POST', "/api/agents/#{owner}/control", {op: 'workspace_zoom', id: second['id'], factor: 0.1}).first).to eq(400)
    expect(host.request('POST', "/api/agents/#{owner}/control", {op: 'workspace_open', id: 'absent', url: 'https://example.org'}).first).to eq(404)
    expect(host.control(owner, 'workspace')).to eq(before)
    host.control(owner, 'workspace_close', id: second['id'])
    expect(host.control(owner, 'workspace')['active_tab']).to eq(first['id'])
    host.control(owner, 'workspace_close', id: first['id'])
    host.restart
    expect(host.control(owner, 'workspace')).to include('active_tab' => nil, 'tabs' => [])
  end

  it 'accepts current browser observations but ignores stale navigation and closed tabs' do
    owner = chief
    tab = host.control(owner, 'workspace_open', url: 'https://example.com')['workspace']['tabs'].first
    report = {id: tab['id'], seq: tab['command']['seq'], url: 'https://example.com/redirected', title: 'Actual title', can_back: true}
    path = "/api/agents/#{owner}/browser"
    expect(host.api('POST', path, report)['saved']).to eq(true)
    host.control(owner, 'workspace_open', id: tab['id'], url: 'https://example.org')
    expect(host.api('POST', path, report)['saved']).to eq(false)
    expect(host.control(owner, 'workspace')['tabs'].first['url']).to eq('https://example.org')
    host.control(owner, 'workspace_close', id: tab['id'])
    expect(host.api('POST', path, report)['saved']).to eq(false)
  end

  it 'reads only registered file tabs, enforces owner boundaries, and limits binary and oversized previews' do
    owner, other = chief, host.agent
    file = File.join(host.workspace(owner), 'jokes.txt')
    File.write(file, 'Joke 😎 <script>literal</script>')
    tab = host.control(owner, 'workspace_open', path: file)['workspace']['tabs'].first
    path = "/api/agents/#{owner}/browser/#{tab['id']}/content"
    preview = host.api('GET', path)
    expect(preview).to include('content' => File.read(file))
    expect(File.identical?(preview['path'], file)).to eq(true)
    File.write(file, 'Updated')
    expect(host.api('GET', path)['content']).to eq('Updated')
    expect(host.request('GET', path.sub(owner, other)).first).to eq(404)
    expect(host.request('GET', path.sub(tab['id'], 'missing')).first).to eq(404)
    File.binwrite(file, "\xff\x00")
    expect(host.request('GET', path).first).to eq(415)
    File.write(file, 'x' * 2_000_000)
    expect(host.api('GET', path)['content'].bytesize).to eq(2_000_000)
    File.write(file, 'x' * 2_000_001)
    expect(host.request('GET', path).first).to eq(413)
    File.delete(file)
    expect(host.request('GET', path).first).to eq(404)
  end

  it 'rejects oversized previews within a bounded heap and keeps the service usable' do
    @host.close
    @host = Host.new(command: [Contract::BINARY, '+RTS', '-M32m', '-RTS'])
    owner = chief
    file = File.join(host.workspace(owner), 'large.txt')
    File.open(file, 'wb') { |stream| stream.truncate(64 * 1024 * 1024) }
    tab = host.control(owner, 'workspace_open', path: file)['workspace']['tabs'].first
    path = "/api/agents/#{owner}/browser/#{tab['id']}/content"
    expect(host.request('GET', path).first).to eq(413)
    expect(host.api('GET', '/api/health')['status']).to eq('ok')
    File.write(file, "\uFEFFStill usable 😎")
    expect(host.api('GET', path)['content']).to eq('Still usable 😎')
  end

  [
    {op: 'workspace_open', url: 'javascript:alert(1)'},
    {op: 'workspace_open', url: 'https://user:secret@example.com'},
    {op: 'workspace_open', path: 'missing.md'},
    {op: 'workspace_open', url: 'https://example.com', path: 'a.md'},
    {op: 'workspace_zoom', factor: 0}, {op: 'workspace_zoom', factor: true},
    {op: 'artifact_save', name: 'old.md', content: 'Removed'}
  ].each do |data|
    it "rejects unsafe workspace operation #{data.inspect}" do
      host.control(chief, 'workspace_open', url: 'https://example.com') if data[:op] == 'workspace_zoom'
      before = host.control(chief, 'workspace')
      expect(host.request('POST', "/api/agents/#{chief}/control", data).first).to eq(data[:path] == 'missing.md' ? 404 : 400)
      expect(host.control(chief, 'workspace')).to eq(before)
    end
  end

  it 'lets the external provider edit Notes and supplies the full saved memo to subsequent work' do
    owner = chief
    content = memo('UniqueSavedName ' + 'x' * 65000 + ' TAIL')
    host.configure(mode: 'Exec', rules: {'Execution' => {actions: [{write: 'Notes.html', content: content}]}})
    host.turn(host.submit(owner, 'Save my name'))
    notes = host.api('GET', "/api/agents/#{owner}/notes")
    expect(notes['content']).to eq(content)
    host.configure(rules: {}, mode: 'Repl')
    host.turn(host.submit(owner, 'What is my name?'))
    expect(host.calls.last['prompt']).to include('UniqueSavedName', 'TAIL')
    expect(host.api('GET', "/api/agents/#{owner}/notes")['content']).to eq(content)
    other = host.agent
    host.turn(host.submit(other))
    expect(host.calls.last['prompt']).not_to include('UniqueSavedName')
    host.restart
    expect(host.api('GET', "/api/agents/#{owner}/notes")['content']).to eq(content)
  end

  it 'migrates legacy Notes without overwriting original files or interpreting saved markup' do
    owner = chief
    folder = host.workspace(owner)
    host.stop
    File.delete(File.join(folder, 'Notes.html'))
    legacy = "facts:\n  name: Aleksi\n  markup: <script>literal 😎</script>\n"
    File.write(File.join(folder, 'Notes.yaml'), legacy)
    host.start
    content = host.api('GET', "/api/agents/#{owner}/notes")['content']
    expect(content).to include('Aleksi', '&lt;script&gt;')
    expect(content).not_to include('<script>')
    expect(File.read(File.join(folder, 'Notes.yaml'))).to eq(legacy)
    File.write(File.join(folder, 'Notes.html'), memo('New authoritative notes'))
    host.restart
    expect(host.api('GET', "/api/agents/#{owner}/notes")['content']).to eq(memo('New authoritative notes'))
  end

  it 'rejects traversal and symlink escapes without preventing ordinary chat' do
    owner = chief
    path = "/api/agents/#{owner}/notes"
    expect(host.request('GET', path + '?image=../private.png').first).to eq(409)
    file = File.join(host.workspace(owner), 'Notes.html')
    File.binwrite(file, "\xff\x00")
    host.turn(host.submit(owner, 'Continue despite invalid notes'))
    expect(host.request('GET', path).first).to eq(409)
    unless Gem.win_platform?
      outside = File.join(host.root, 'private.txt')
      File.write(outside, 'DO NOT EXPOSE')
      File.delete(file)
      File.symlink(outside, file)
      expect(host.request('GET', path).first).to eq(409)
      host.turn(host.submit(owner, 'Continue despite unsafe notes'))
      expect(host.calls.last['prompt']).not_to include('DO NOT EXPOSE')
    end
  end
  it 'migrates saved inline browser tabs once and verifies memo provenance against the saved content' do
    owner = chief
    folder = host.workspace(owner)
    content = memo('Preserved notes')
    file = File.join(folder, 'Notes.html')
    history = File.join(folder, 'Notes.history.json')
    File.write(file, content)
    File.write(history, JSON.generate(created_by: 'Admin', modified_by: 'Verified editor', sha256: Digest::SHA256.hexdigest(content)))
    expect(host.api('GET', "/api/agents/#{owner}/notes")['metadata']['modified_by']).to eq('Verified editor')
    host.stop
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      preferences = JSON.parse(db.get_first_value('SELECT value FROM preferences WHERE id=1'))
      preferences.delete('browser_version')
      preferences['workspaces'] = {owner => {activeTab: 'legacy', tabs: [{id: 'legacy', type: 'html', html: '<p>Saved 😎</p>'}]}}
      db.execute('UPDATE preferences SET value=? WHERE id=1', [JSON.generate(preferences)])
    end
    host.start
    workspace = host.control(owner, 'workspace')
    tab = workspace['tabs'].first
    expect(workspace['active_tab']).to eq('legacy')
    expect(tab).to include('zoom' => 1, 'url' => a_string_starting_with('file://'))
    expect(tab.keys).not_to include('type', 'html')
    expect(File.read(tab['path'])).to eq('<p>Saved 😎</p>')
    File.write(file, memo('Changed outside the recorded revision'))
    expect(host.api('GET', "/api/agents/#{owner}/notes")['metadata']).to include('created_by' => 'Admin', 'modified_by' => nil)
    host.restart
    expect(host.control(owner, 'workspace')['tabs'].first).to eq(tab)
    expect(Dir.children(folder).grep(/\Asaved-tab-.*\.html\z/).size).to eq(1)
    unless Gem.win_platform?
      outside = File.join(host.root, 'private-history.json')
      File.write(outside, JSON.generate(created_by: 'DO NOT EXPOSE'))
      File.delete(history)
      File.symlink(outside, history)
      expect(host.api('GET', "/api/agents/#{owner}/notes")['metadata']['created_by']).to be_nil
    end
  end

  it 'parses memo HTML sections across quoting and attribute order while preserving their required sequence' do
    owner = chief
    file = File.join(host.workspace(owner), 'Notes.html')
    about = "<SECTION class='memo' id='about'>Research &amp; review</SECTION>"
    navigation = "<section data-kind='navigation' id='map'><a href='#saved'>Saved</a></section>"
    content = "<section id='content'>Saved facts</section>"
    File.write(file, about + navigation + content)
    host.restart
    host.turn(host.submit(owner))
    context = JSON.parse(host.calls.last['prompt'].split("\nContext:\n", 2).last)
    notes = JSON.parse(context['conversation']['manifests']['host-facts'])['notes']
    expect(notes).not_to have_key('error')
    expect(notes['about']).to include('Research &amp; review')
    expect(notes['map']).to include('Saved')
    File.write(file, navigation + about + content)
    host.restart
    host.turn(host.submit(owner))
    context = JSON.parse(host.calls.last['prompt'].split("\nContext:\n", 2).last)
    expect(JSON.parse(context['conversation']['manifests']['host-facts'])['notes']['error']).to include('requires sections')
  end

end
