require_relative 'support/desktop'
require_relative 'support/http_peer'

RSpec.describe 'Linux native browser behavior' do
  before do
    skip 'Linux desktop session required' unless ENV['SAPIENS_TEST_LINUX_DESKTOP'] == '1'
    @fixture = Host.new(start: false)
    start_browser
  end

  after do
    @desktop&.signal('TERM')
    @desktop&.close
    @pages&.close
    @fixture&.close
  end

  def start_browser
    config = File.join(@fixture.fixture_dir, 'config')
    FileUtils.mkdir_p(config)
    File.write(File.join(config, 'user-dirs.dirs'), "XDG_DOWNLOAD_DIR=\"#{@fixture.root}\"\n")
    @desktop = Contract::Child.new(File.join(Contract::ROOT, '.build/bin/sapiens-linux'),
      '--data-dir', @fixture.root, env: @fixture.environment.merge('XDG_CONFIG_HOME' => config))
    Contract.wait(description: 'native browser readiness') { @url = @desktop.stdout[%r{READY (http://127.0.0.1:\d+)}, 1] }
    @owner = Contract.http(@url + '/api/state', 'GET')[1].fetch('main_agent_id')
  rescue StandardError => error
    raise "#{error.message}\n#{@desktop&.stderr}"
  end

  def control(operation, owner: @owner, **fields)
    status, result = Contract.http(@url + "/api/agents/#{owner}/control", 'POST', {op: operation}.merge(fields))
    expect(status).to eq(200), result.inspect
    result
  end

  def tab_title(title, owner: @owner)
    Contract.wait(description: "native title #{title}") do
      control('workspace', owner: owner).fetch('tabs').find { |tab| tab['title'] == title && !tab['loading'] }
    end
  end

  def web_processes
    pending, found = [@desktop.pid], []
    pending.each do |pid|
      begin
        pending.concat(File.read("/proc/#{pid}/task/#{pid}/children").split.map(&:to_i))
        found << pid if File.read("/proc/#{pid}/comm").start_with?('WebKitWebProces')
      rescue Errno::ENOENT
      end
    end
    found
  end

  def native_control(label = nil, role: nil, value: nil, focused: false)
    driver = Desktop.driver
    arguments = ['find', '--pid', @desktop.pid.to_s, '--depth', '40']
    arguments.concat(['--title', label]) if label
    arguments.concat(['--role', role]) if role
    Contract.wait(description: "native control #{label}") do
      output, _, status = Contract.capture(driver, *arguments)
      if status == 0
        JSON.parse(output).fetch('matches').find do |node|
          (!label || node['title'] == label) && (value.nil? || node['value'] == value) && (!focused || node['focused'])
        end
      end
    end
  end

  def native_action(operation, node, *arguments)
    driver = Desktop.driver
    output, error, status = Contract.capture(driver, operation, '--pid', @desktop.pid.to_s,
      '--path', node.fetch('path'), *arguments)
    expect(status).to eq(0), output + error
  end

  it 'asks for a download destination and cancels without writing a file' do
    skip 'Foreground verification required' unless ENV['XDG_SESSION_TYPE'] == 'x11'
    @pages = HTTPPeer.new { |_| [200, 'Download fixture', {'Content-Type' => 'application/octet-stream', 'Content-Disposition' => 'attachment; filename="fixture.txt"'}] }
    control('workspace_open', url: @pages.url)
    native_control('Save download')
    native_action('press', native_control('Cancel', role: 'Button'))
    expect(Dir.glob(File.join(@fixture.root, '**/fixture.txt'))).to be_empty
    expect(Contract.http(@url + '/api/health', 'GET').first).to eq(200)
  end

  it 'saves a download only to the explicitly selected destination' do
    skip 'Foreground verification required' unless ENV['XDG_SESSION_TYPE'] == 'x11'
    @pages = HTTPPeer.new { |_| [200, 'Download fixture', {'Content-Type' => 'application/octet-stream', 'Content-Disposition' => 'attachment; filename="fixture.txt"'}] }
    control('workspace_open', url: @pages.url)
    native_control('Save download')
    destination = File.join(@fixture.root, 'selected-download.txt')
    native_action('set-value', native_control(role: 'TextField', value: 'fixture.txt'), '--value', destination)
    expect(File).not_to exist(destination)
    native_action('press', native_control('Save', role: 'Button'))
    Contract.wait(description: 'saved download') { File.file?(destination) && File.read(destination) == 'Download fixture' }
  end

  it 'refuses to overwrite an existing download destination' do
    skip 'Foreground verification required' unless ENV['XDG_SESSION_TYPE'] == 'x11'
    destination = File.join(@fixture.root, 'existing.txt')
    File.write(destination, 'Original content')
    @pages = HTTPPeer.new { |_| [200, 'Replacement content', {'Content-Type' => 'application/octet-stream', 'Content-Disposition' => 'attachment; filename="fixture.txt"'}] }
    control('workspace_open', url: @pages.url)
    native_control('Save download')
    native_action('set-value', native_control(role: 'TextField', value: 'fixture.txt'), '--value', destination)
    native_action('press', native_control('Save', role: 'Button'))
    Contract.wait(description: 'download destination rejection') do
      control('workspace').fetch('tabs').any? { |tab| tab['error'].to_s.include?('already exists') }
    end
    expect(File.read(destination)).to eq('Original content')
  end

  it 'preserves a destination created while a download is in progress' do
    skip 'Foreground verification required' unless ENV['XDG_SESSION_TYPE'] == 'x11'
    release = Queue.new
    body = proc do |stream|
      stream.write('x' * 16384)
      Timeout.timeout(45) { release.pop }
      stream.write('Last chunk')
    end
    @pages = HTTPPeer.new { |_| [200, body, {'Content-Type' => 'application/octet-stream', 'Content-Disposition' => 'attachment; filename="fixture.txt"'}] }
    control('workspace_open', url: @pages.url)
    native_control('Save download')
    folder = Dir.mktmpdir('download destination ', @fixture.root)
    destination = File.join(folder, 'concurrent.txt')
    native_action('set-value', native_control(role: 'TextField', value: 'fixture.txt'), '--value', destination)
    native_action('press', native_control('Save', role: 'Button'))
    # The network response is still held. Wait until the download has
    # started writing in the chosen directory before introducing the collision.
    Contract.wait(description: 'download started in the selected directory') { !Dir.empty?(folder) }
    File.open(destination, 'wx') { |file| file.write('Keep this file') }
    release << true
    Contract.wait(description: 'download publication refused') do
      control('workspace').fetch('tabs').any? { |tab| tab['error'].to_s.downcase.include?('exists') }
    end
    expect(File.read(destination)).to eq('Keep this file')
    expect(Dir.children(folder)).to contain_exactly('concurrent.txt')
  ensure
    release << true if release
  end

  it 'cancels a pending download when its tab is closed' do
    skip 'Foreground verification required' unless ENV['XDG_SESSION_TYPE'] == 'x11'
    @pages = HTTPPeer.new { |_| [200, 'Download fixture', {'Content-Type' => 'application/octet-stream', 'Content-Disposition' => 'attachment; filename="fixture.txt"'}] }
    control('workspace_open', url: @pages.url)
    native_control('Save download')
    tab = control('workspace').fetch('tabs').first
    control('workspace_close', id: tab.fetch('id'))
    control('workspace_open', url: 'about:blank')
    Contract.wait(description: 'desktop remains responsive after download cancellation') do
      control('workspace').fetch('tabs').any? { |item| item['url'] == 'about:blank' && !item['loading'] }
    end
    expect(File).not_to exist(File.join(@fixture.root, 'fixture.txt'))
  end

  it 'opens a Unicode file path selected through the native chooser' do
    skip 'Foreground verification required' unless ENV['XDG_SESSION_TYPE'] == 'x11'
    driver = Desktop.driver
    path = File.join(@fixture.root, 'Selected café.html')
    File.write(path, '<title>Selected local file</title><h1>Selected local file</h1>')
    control('workspace_open', url: 'about:blank')
    native_action('press', native_control('Workspace options', role: 'Button'))
    native_action('press', native_control('Open file…', role: 'Button'))
    native_control('Open file')
    output, error, status = Contract.capture(driver, 'key', '--pid', @desktop.pid.to_s, '--key', 'ctrl+l')
    expect(status).to eq(0), output + error
    focused = native_control(role: 'TextField', focused: true)
    native_action('set-value', focused, '--value', path)
    native_action('press', native_control('Open', role: 'Button'))
    expect(tab_title('Selected local file')['url']).to start_with('file://')
  end

  it 'reports a guest renderer crash and recovers only after an explicit reload' do
    shell = Contract.wait(description: 'workspace renderer') { pids = web_processes; pids unless pids.empty? }
    revision = 1
    @pages = HTTPPeer.new { |_| [200, "<title>Recovery fixture #{revision}</title><h1>Recovery fixture</h1>", {'Content-Type' => 'text/html', 'Cache-Control' => 'no-store'}] }
    control('workspace_open', url: @pages.url)
    tab_title('Recovery fixture 1')
    guest = Contract.wait(description: 'separate guest renderer') { (web_processes - shell).first }
    before_crash = @pages.requests.length
    Process.kill('KILL', guest)
    Contract.wait(description: 'visible renderer failure') do
      control('workspace').fetch('tabs').any? { |tab| tab['error'].to_s.include?('Reload') && !tab['loading'] }
    end
    expect(@pages.requests.length).to eq(before_crash)
    expect(Contract.http(@url + '/api/health', 'GET').first).to eq(200)
    revision = 2
    control('workspace_reload')
    Contract.wait(description: 'explicit reload request') { @pages.requests.length > before_crash }
    expect(tab_title('Recovery fixture 2')['error'].to_s).to eq('')
    expect(@pages.requests.length).to be > before_crash
  end

  it 'stops the host when the workspace renderer crashes and permits a clean restart' do
    shell = Contract.wait(description: 'workspace renderer') { web_processes.first }
    Process.kill('KILL', shell)
    output, error, status = @desktop.finish(10)
    expect(status).not_to eq(0), output + error
    expect(error).to include('Workspace renderer terminated')
    expect { Contract.http(@url + '/api/health', 'GET') }.to raise_error(Errno::ECONNREFUSED)
    @desktop.close
    start_browser
    expect(Contract.http(@url + '/api/health', 'GET').first).to eq(200)
  end

  it 'navigates back and forward and reloads fresh server content' do
    revision = 1
    @pages = HTTPPeer.new do |request|
      [200, "<title>#{request.path} revision #{revision}</title><h1>#{request.path}</h1>",
        {'Content-Type' => 'text/html', 'Cache-Control' => 'no-store'}]
    end
    control('workspace_open', url: @pages.url + '/first')
    tab = tab_title('/first revision 1')
    control('workspace_open', id: tab['id'], url: @pages.url + '/second')
    expect(tab_title('/second revision 1')['can_back']).to eq(true)
    control('workspace_back')
    expect(tab_title('/first revision 1')['can_forward']).to eq(true)
    control('workspace_forward')
    tab_title('/second revision 1')
    revision = 2
    control('workspace_reload')
    tab_title('/second revision 2')
    expect(@pages.requests.count { |request| request[:path] == '/second' }).to be >= 2
  end

  it 'isolates owner cookies and retains them through a clean desktop restart' do
    @pages = HTTPPeer.new do |request|
      cookie = request['cookie'].to_s.include?('profile=chief') ? 'Chief cookie' : 'No cookie'
      headers = {'Content-Type' => 'text/html', 'Cache-Control' => 'no-store'}
      headers['Set-Cookie'] = 'profile=chief; Path=/; Max-Age=3600; SameSite=Strict' if request.path == '/set'
      [200, "<title>#{cookie}</title><h1>#{cookie}</h1>", headers]
    end
    control('workspace_open', url: @pages.url + '/set')
    tab = tab_title('No cookie')
    control('workspace_open', id: tab['id'], url: @pages.url + '/read')
    tab_title('Chief cookie')
    status, other = Contract.http(@url + '/api/agents', 'POST', {name: 'BrowserGuest', role: 'Research'})
    expect(status).to eq(201), other.inspect
    control('workspace_open', owner: other.fetch('id'), url: @pages.url + '/read')
    native_action('press', native_control('Open BrowserGuest', role: 'Button'))
    tab_title('No cookie', owner: other.fetch('id'))
    @desktop.signal('TERM')
    expect(@desktop.finish.last).to eq(0)
    @desktop.close
    start_browser
    native_action('press', native_control('Open ChiefSapi', role: 'Button'))
    control('workspace_reload')
    tab_title('Chief cookie')
    native_action('press', native_control('Open BrowserGuest', role: 'Button'))
    control('workspace_reload', owner: other.fetch('id'))
    tab_title('No cookie', owner: other.fetch('id'))
  end

  it 'changes the native page viewport when workspace zoom changes' do
    widths = Queue.new
    @pages = HTTPPeer.new do |request|
      widths << request.query['width'].to_i if request.path == '/measure'
      [200, '<title>Zoom fixture</title><h1>Zoom fixture</h1><script>function report(){fetch("/measure?width="+innerWidth)};addEventListener("resize",report);report()</script>',
        {'Content-Type' => 'text/html', 'Cache-Control' => 'no-store'}]
    end
    control('workspace_open', url: @pages.url)
    tab_title('Zoom fixture')
    baseline = nil
    Contract.wait(description: 'native viewport measurement') do
      baseline = widths.pop until widths.empty?
      baseline && baseline > 100
    end
    control('workspace_zoom', factor: 2)
    Contract.wait(description: 'zoomed native viewport') do
      current = widths.pop unless widths.empty?
      current && current.between?(baseline * 0.45, baseline * 0.55)
    end
  end

  it 'blocks guest redirects into the local host' do
    @pages = HTTPPeer.new { |_| [302, '', {'Location' => @url + '/workspace/'}] }
    control('workspace_open', url: @pages.url)
    Contract.wait(description: 'rejected guest destination') do
      control('workspace').fetch('tabs').any? { |tab| !tab['loading'] && tab['error'].to_s.include?('blocked') }
    end
    expect(Contract.http(@url + '/api/health', 'GET').first).to eq(200)
  end

  it 'delivers a physical pointer click to a native guest button' do
    skip 'Xorg coordinates required' unless ENV['XDG_SESSION_TYPE'] == 'x11'
    @pages = HTTPPeer.new do |_request|
      [200, '<title>Pointer fixture</title><button style="width:300px;height:200px" onclick="fetch(\'/clicked\')">Click native guest</button>',
        {'Content-Type' => 'text/html'}]
    end
    control('workspace_open', url: @pages.url)
    tab_title('Pointer fixture')
    driver = Desktop.driver
    button = nil
    Contract.wait(description: 'native guest button coordinates') do
      output, _, status = Contract.capture(driver, 'find', '--pid', @desktop.pid.to_s,
        '--title', 'Click native guest', '--role', 'Button', '--depth', '40')
      button = JSON.parse(output).fetch('matches').find { |node| node['position'] && node['size'] } if status == 0
    end
    position, size = button.values_at('position', 'size')
    output, error, status = Contract.capture(driver, 'click', '--pid', @desktop.pid.to_s,
      '--x', (position.fetch('x') + size.fetch('width') / 2).to_s,
      '--y', (position.fetch('y') + size.fetch('height') / 2).to_s)
    expect(status).to eq(0), output + error + button.inspect
    Contract.wait(description: 'native guest click effect') { @pages.requests.any? { |request| request[:path] == '/clicked' } }
  end
end
