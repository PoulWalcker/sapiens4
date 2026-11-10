require_relative 'support/desktop'
require_relative 'support/relay'

RSpec.describe 'Packaged Windows application', :windows do
  it 'keeps chat and shutdown responsive after a guest browser fails without a console', :desktop do
    skip 'Windows package required' unless Gem.win_platform? && ENV['SAPIENS_TEST_WINDOWS_PACKAGE']
    @host = Host.new
    file = File.join(host.workspace(chief), 'saved.txt')
    File.write(file, 'Saved workspace document')
    host.control(chief, 'workspace_open', path: file)
    host.stop
    binary = File.join(ENV.fetch('SAPIENS_TEST_WINDOWS_PACKAGE'), 'Sapiens4.exe')
    quote = ->(value) { "'#{value.gsub("'", "''")}'" }
    # Launch like Explorer, without the test runner's redirected console handles.
    command = "$p = Start-Process -FilePath #{quote.call(binary)} -ArgumentList #{quote.call('--data-dir "' + host.root + '"')} -PassThru; $p.Id"
    output, error, code = Contract.capture('powershell.exe', '-NoProfile', '-Command', command,
      env: host.environment.merge('SAPIENS_PYTHON' => nil, 'SAPIENS_RUNTIME_DIR' => nil))
    raise error unless code == 0
    app = Struct.new(:pid).new(Integer(output.strip))
    # The packaged window has no test API. Discover it through the native process.
    @desktop = Desktop.new(process: app, ready: false)
    desktop.see('Saved workspace document')
    command = "Get-CimInstance Win32_Process -Filter \"Name='msedgewebview2.exe'\" | Where-Object { $_.ParentProcessId -eq #{app.pid} -and $_.CommandLine.Contains('WebView2\\Guests') -and -not $_.CommandLine.Contains('--type=') } | ForEach-Object { Stop-Process -Id $_.ProcessId -Force; $_.ProcessId }"
    killed, error, code = Contract.capture('powershell.exe', '-NoProfile', '-Command', command)
    expect(code).to eq(0), error
    expect(killed.strip).to match(/\A\d+\z/)
    desktop.click('Workspace options')
    desktop.click('URL or file path…')
    Contract.wait(description: 'workspace menu selection') { !desktop.text.include?('Open file…') }
    desktop.fill('Message ChiefSapi', 'Hello from the packaged app')
    desktop.press('Send message')
    expect(desktop.see('Contract test reply')).to include('Hello from the packaged app')
    desktop.key('alt+F4')
    _, error, code = Contract.capture('powershell.exe', '-NoProfile', '-Command',
      "$p = Get-Process -Id #{app.pid} -ErrorAction SilentlyContinue; if ($p -and -not $p.WaitForExit(10000)) { exit 1 }; exit 0")
    expect(code).to eq(0), error
    app = nil
  ensure
    system('taskkill', '/PID', app.pid.to_s, '/T', '/F', out: File::NULL, err: File::NULL) if app
  end

  it 'preserves remote authority across an embedded-runtime restart' do
    skip 'Windows package required' unless Gem.win_platform? && ENV['SAPIENS_TEST_WINDOWS_PACKAGE']
    runtime = File.join(ENV.fetch('SAPIENS_TEST_WINDOWS_PACKAGE'), 'runtime')
    @host = Host.new(repo: runtime, command: [File.join(runtime, '.build/bin/sapiens4.exe')],
      environment: {'PYTHONPATH' => nil, 'PYTHONHOME' => nil})
    relay = Relay.new
    peer = PairedDevice.new
    invitation = host.api('POST', '/api/remote/pair', {relay: relay.url, token: relay.token}, status: 201)['qr']
    peer.pair(invitation)
    pending = Contract.wait { host.api('GET', '/api/remote')['pending'] }
    host.api('POST', '/api/remote/approve', {peer: pending['peer']})
    expect(peer.receive['paired']).to eq(true)
    identity = host.state['corpora']['id']
    expect(peer.request('/api/state')['body']['corpora']['id']).to eq(identity)
    host.restart
    expect(peer.request('/api/state')['body']['corpora']['id']).to eq(identity)
    host.api('POST', '/api/remote/revoke', {})
    expect(host.api('GET', '/api/remote')['paired']).to eq(false)
  ensure
    peer&.close
    @host&.close
    relay&.close
  end
end
