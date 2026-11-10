require_relative 'support/desktop'
require_relative 'support/http_peer'

RSpec.describe 'Linux desktop lifecycle' do
  before do
    skip 'Linux desktop session required' unless RUBY_PLATFORM.include?('linux') && ENV['SAPIENS_TEST_LINUX_DESKTOP'] == '1'
    @fixture = Host.new(start: false)
    @probe = File.join(Contract::ROOT, '.build/linux-window')
    flags, error, code = Contract.capture('pkg-config', '--cflags', '--libs', 'atspi-2', 'gobject-2.0')
    expect(code).to eq(0), error
    _, error, code = Contract.capture('cc', '-Wall', '-Wextra', '-Werror',
      File.join(Contract::ROOT, 'spec/support/LinuxWindow.c'), '-o', @probe, *flags.split)
    expect(code).to eq(0), error
  end

  after { @fixture&.close }

  it 'selects the desktop backend explicitly without requiring a display for help' do
    output, error, status = Contract.capture('sh', File.join(Contract::ROOT, 'environments/linux/start.sh'), '--help',
      env: {'SAPIENS_LINUX_BACKEND' => 'desktop', 'DISPLAY' => nil, 'WAYLAND_DISPLAY' => nil})
    expect(status).to eq(0), output + error
    expect(output).to include('Usage: sapiens-linux')
  end

  it 'provides help without a graphical session' do
    output, error, status = Contract.capture(File.join(Contract::ROOT, '.build/bin/sapiens-linux'), '--help',
      env: {'DISPLAY' => nil, 'WAYLAND_DISPLAY' => nil, 'GDK_BACKEND' => 'x11'})
    expect(status).to eq(0), error
    expect(output).to include('Usage: sapiens-linux')
  end

  it 'refuses to start a host when no graphical session is available' do
    _, error, status = Contract.capture(File.join(Contract::ROOT, '.build/bin/sapiens-linux'),
      '--data-dir', @fixture.root,
      env: @fixture.environment.merge('DISPLAY' => nil, 'WAYLAND_DISPLAY' => nil, 'GDK_BACKEND' => 'x11'))
    expect(status).not_to eq(0)
    expect(error).to include('graphical')
    expect(File).not_to exist(File.join(@fixture.root, 'corpora.sqlite3'))
  end

  it 'opens the UI and stops its host when the desktop closes' do
    desktop = Contract::Child.new('sh', File.join(Contract::ROOT, 'environments/linux/start.sh'),
      '--data-dir', @fixture.root, env: @fixture.environment.merge('SAPIENS_LINUX_BACKEND' => 'desktop',
        'SAPIENS_BLINDLY_BINARY' => '/missing/external-driver'))
    url = nil
    Contract.wait(description: 'Linux desktop readiness') do
      url = desktop.stdout[%r{READY (http://127.0.0.1:\d+)}, 1]
    end
    registry = Contract.http(url + '/api/host', 'GET')[1]
    expect(registry.fetch('local_available')).to eq(true)
    expect(registry.fetch('corpora')).not_to be_empty
    state = Contract.http(url + '/api/state', 'GET')[1]
    chief = state.fetch('agents').find { |agent| agent.fetch('id') == state.fetch('main_agent_id') }
    instructions = Dir.glob(File.join(@fixture.root, '**/manifests/computer-use.md')).map { |path| File.read(path) }.join
    expect(instructions).to include('Blindly4 UI wrapper:')
    observer = Contract::Child.new(@probe, desktop.pid.to_s)
    Contract.wait(description: 'accessible Linux conversation') do
      names = observer.stdout.lines.map(&:strip)
      names.include?('Sapiens4') && names.include?("Message #{chief.fetch('name')}")
    end
    expect(Contract.http(url + '/api/health', 'GET').first).to eq(200)
    status, diagnostics = Contract.http(url + '/api/computer', 'GET')
    expect(status).to eq(200)
    expect(diagnostics).to include('backend' => 'native', 'built' => true, 'trusted' => true)
    expect(Contract.http(url + '/workspace/app.js', 'GET').first).to eq(200)
    desktop.signal('TERM')
    _, error, status = desktop.finish
    expect(status).to eq(0), error
    expect(File).to exist(File.join(@fixture.root, 'corpora.sqlite3'))
    expect { Contract.http(url + '/api/health', 'GET') }.to raise_error(Errno::ECONNREFUSED)
  ensure
    observer&.signal('TERM')
    observer&.close
    desktop&.signal('TERM')
    desktop&.close
  end

  it 'loads an unframeable site in a native tab and reports its title to the workspace' do
    pages = HTTPPeer.new do |_request|
      [200, '<title>Linux native guest</title><h1>Native guest content</h1><p id="bridge"></p><script>document.getElementById("bridge").textContent = window.webkit?.messageHandlers?.browser ? "Bridge exposed" : "Bridge isolated"</script>',
        {'Content-Type' => 'text/html', 'X-Frame-Options' => 'DENY', 'Content-Security-Policy' => "frame-ancestors 'none'"}]
    end
    desktop = Contract::Child.new(File.join(Contract::ROOT, '.build/bin/sapiens-linux'),
      '--data-dir', @fixture.root, env: @fixture.environment)
    url = nil
    Contract.wait(description: 'Linux desktop readiness') { url = desktop.stdout[%r{READY (http://127.0.0.1:\d+)}, 1] }
    chief = Contract.http(url + '/api/state', 'GET')[1].fetch('main_agent_id')
    endpoint = url + "/api/agents/#{chief}/control"
    status, state = Contract.http(endpoint, 'POST', {op: 'workspace_open', url: pages.url})
    expect(status).to eq(200)
    tab = state.fetch('workspace').fetch('tabs').first.fetch('id')
    observer = Contract::Child.new(@probe, desktop.pid.to_s)
    Contract.wait(description: 'accessible native browser content') { observer.stdout.include?('Native guest content') }
    Contract.wait(description: 'guest has no host bridge') { observer.stdout.include?('Bridge isolated') }
    expect(observer.stdout).not_to include('Bridge exposed')
    Contract.wait(description: 'native title reported to host') do
      workspace = Contract.http(endpoint, 'POST', {op: 'status'})[1].fetch('workspace')
      workspace.fetch('tabs').any? { |item| item['id'] == tab && item['title'] == 'Linux native guest' }
    end
    expect(Contract.http(endpoint, 'POST', {op: 'workspace_close', id: tab}).first).to eq(200)
    Contract.wait(description: 'closed native guest') do
      snapshot = observer.stdout.scan(/SNAPSHOT\n(.*?)\nEND/m).last&.first
      snapshot && snapshot.include?('Sapiens4') && !snapshot.include?('Native guest content')
    end
  rescue StandardError
    if desktop
      output, error, status = Contract.capture(Desktop.driver,
        'find', '--pid', desktop.pid.to_s, '--title', 'Native guest content', '--depth', '40')
      warn "Direct accessibility (#{status}): #{output} #{error}"
    end
    warn "Desktop: #{desktop&.stderr}\nAccessibility: #{observer&.stderr}\n#{observer&.stdout&.slice(-12000, 12000) || observer&.stdout}"
    raise
  ensure
    observer&.signal('TERM')
    observer&.close
    desktop&.signal('TERM')
    desktop&.close
    pages&.close
  end

  it 'opens and cancels a native file picker from the workspace menu' do
    skip 'Foreground verification required' unless ENV['XDG_SESSION_TYPE'] == 'x11'
    desktop = Contract::Child.new(File.join(Contract::ROOT, '.build/bin/sapiens-linux'),
      '--data-dir', @fixture.root, env: @fixture.environment)
    url = nil
    Contract.wait(description: 'Linux desktop readiness') { url = desktop.stdout[%r{READY (http://127.0.0.1:\d+)}, 1] }
    chief = Contract.http(url + '/api/state', 'GET')[1].fetch('main_agent_id')
    expect(Contract.http(url + "/api/agents/#{chief}/control", 'POST', {op: 'workspace_open', url: 'about:blank'}).first).to eq(200)
    driver = Desktop.driver
    ['Workspace options', 'Open file…'].each do |label|
      node = nil
      observation = nil
      Contract.wait(description: "native #{label}") do
        output, error, status = Contract.capture(driver, 'find', '--pid', desktop.pid.to_s, '--title', label, '--depth', '40')
        observation = "#{output} #{error}"
        node = JSON.parse(output).fetch('matches').find { |item| item.fetch('actions', []).include?('AXPress') } if status == 0
      end
      output, error, status = Contract.capture(driver, 'press', '--pid', desktop.pid.to_s, '--path', node.fetch('path'))
      expect(status).to eq(0), output + error
    rescue StandardError
      warn "Accessibility for #{label}: #{observation}\nDesktop: #{desktop.stderr}"
      warn Contract.capture(driver, 'tree', '--pid', desktop.pid.to_s, '--depth', '40', '--max-nodes', '250').join("\n")
      raise
    end
    picker_pid = cancel = nil
    Contract.wait(description: 'native file dialog') do
      output, _, status = Contract.capture(driver, 'apps')
      next false unless status == 0
      candidates = JSON.parse(output).fetch('apps').select { |app| app['pid'] == desktop.pid || app['name'].start_with?('xdg-desktop-portal-') }
      candidates.any? do |app|
        picker_pid = app.fetch('pid').to_s
        output, _, status = Contract.capture(driver, 'find', '--pid', picker_pid, '--title', 'Cancel', '--role', 'Button', '--depth', '40')
        cancel = JSON.parse(output).fetch('matches').find { |item| item.fetch('actions', []).include?('AXPress') } if status == 0
      end
    end
    observer = Contract::Child.new(@probe, picker_pid)
    Contract.wait(description: 'file picker title') { observer.stdout.downcase.include?('open file') }
    output, error, status = Contract.capture(driver, 'press', '--pid', picker_pid, '--path', cancel.fetch('path'))
    expect(status).to eq(0), output + error
    expect(Contract.http(url + '/api/health', 'GET').first).to eq(200)
  ensure
    observer&.signal('TERM')
    observer&.close
    desktop&.signal('TERM')
    desktop&.close
  end
end
