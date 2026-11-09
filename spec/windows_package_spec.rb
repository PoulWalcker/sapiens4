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

require_relative 'support/package'

RSpec.describe 'Windows per-user installer', :windows do
  include PackageExamples

  it 'installs, refuses active changes, reinstalls and uninstalls without changing external state' do
    skip 'Set SAPIENS_TEST_INSTALLER on Windows' unless Gem.win_platform? && ENV['SAPIENS_TEST_INSTALLER']
    work = Dir.mktmpdir('sapiens-install-check-')
    app, state = File.join(work, 'installed app'), File.join(work, 'external state')
    evidence = File.expand_path(ENV.fetch('SAPIENS_TEST_INSTALLER_EVIDENCE', File.join(work, 'evidence')))
    FileUtils.mkdir_p([state, evidence])
    installer = File.realpath(ENV.fetch('SAPIENS_TEST_INSTALLER'))
    registration = 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Uninstall\com.poulwalcker.sapiens4.development_is1'
    env = {'SAPIENS_INSTALL_APP' => app.tr('/', '\\'), 'SAPIENS_INSTALL_REGISTRATION' => registration}
    ps = lambda do |script, extra = {}|
      Contract.capture('powershell.exe', '-NoProfile', '-NonInteractive', '-Command',
        "$ErrorActionPreference='Stop'; " + script, env: env.merge(extra), timeout: 190)
    end
    observe = lambda do
      out, error, code = ps.call(<<~'PS')
        $r=Get-ItemProperty $env:SAPIENS_INSTALL_REGISTRATION -ErrorAction SilentlyContinue
        $processes=@(Get-CimInstance Win32_Process | Where-Object { $_.ExecutablePath -and $_.ExecutablePath.StartsWith($env:SAPIENS_INSTALL_APP+'\',[StringComparison]::OrdinalIgnoreCase) })
        @{registered=($null -ne $r); location=$r.InstallLocation; shortcut=(Test-Path (Join-Path ([Environment]::GetFolderPath('Programs')) 'Sapiens4 Development.lnk')); processes=@($processes | Select-Object ProcessId,ParentProcessId,ExecutablePath,@{Name='Ports';Expression={@(Get-NetTCPConnection -OwningProcess $_.ProcessId -State Listen -ErrorAction SilentlyContinue | Select-Object -ExpandProperty LocalPort)}})} | ConvertTo-Json -Depth 5 -Compress
      PS
      expect(code).to eq(0), error
      JSON.parse(out)
    end
    initial = observe.call
    expect(initial.values_at('registered', 'shortcut')).to eq([false, false]), 'Use a disposable Windows account without an existing development installation'
    sentinels = %w[corpus.txt config.json onboarding.json noncredential-auth-sentinel.txt unknown.bin].to_h do |name|
      path = File.join(state, name)
      File.write(path, 'synthetic-only-' + name)
      [path, Digest::SHA256.file(path).hexdigest]
    end
    setup = lambda do |program, name, success|
      arguments = ['/VERYSILENT','/SUPPRESSMSGBOXES','/NORESTART','/SP-', "/LOG=\"#{File.join(evidence, name + '.log')}\""]
      arguments << "/DIR=\"#{app}\"" if program == installer
      _, error, code = ps.call(<<~'PS', 'SAPIENS_SETUP_PROGRAM' => program, 'SAPIENS_SETUP_ARGUMENTS' => arguments.join(' '))
        $p=Start-Process -FilePath $env:SAPIENS_SETUP_PROGRAM -ArgumentList $env:SAPIENS_SETUP_ARGUMENTS -PassThru
        if (-not $p.WaitForExit(180000)) { $p.Kill(); throw 'Owned installer timed out' }
        exit $p.ExitCode
      PS
      expect(code == 0).to eq(success), "#{name}: #{code}: #{error}"
    end
    setup.call(installer, 'install', true)
    installed = observe.call
    expect(installed.values_at('registered', 'shortcut')).to eq([true, true])
    expect(File.expand_path(installed.fetch('location')).downcase).to eq(File.expand_path(app).downcase)
    expect(installed.fetch('processes')).to be_empty
    runtime = File.join(app, 'runtime')
    _, error, code = Contract.capture('runghc', '-ibuild/package', 'build/Package.hs', 'verify', '--runtime', runtime, timeout: 120)
    expect(code).to eq(0), error
    unknown = File.join(app, 'user-added.txt')
    File.write(unknown, 'unknown installed file must survive uninstall')
    uninstaller = File.join(app, 'unins000.exe')
    if ENV['SAPIENS_WINDOWS_CERT_THUMBPRINT']
      [installer, uninstaller].each do |path|
        out, error, code = ps.call('$s=Get-AuthenticodeSignature -LiteralPath $env:SAPIENS_SIGNED_FILE; @{status=[string]$s.Status; thumbprint=$s.SignerCertificate.Thumbprint} | ConvertTo-Json -Compress', 'SAPIENS_SIGNED_FILE' => path)
        expect(code).to eq(0), error
        expect(JSON.parse(out)).to include('status' => 'Valid', 'thumbprint' => ENV.fetch('SAPIENS_WINDOWS_CERT_THUMBPRINT'))
      end
    end
    clean = ENV.keys.grep(/\A(SAPIENS_|CODEX_|OPENAI_)/).to_h { |key| [key, nil] }.merge(
      'SAPIENS_INSTALL_APP' => app.tr('/', '\\'), 'SAPIENS_CONFIG_DIR' => File.join(state, 'fresh-config'),
      'SAPIENS_DATA_DIR' => File.join(state, 'fresh-data'), 'SAPIENS_CODEX_BINARY' => File.join(state, 'provider-disabled.exe'))
    out, error, code = ps.call('$p=Start-Process -FilePath (Join-Path $env:SAPIENS_INSTALL_APP "Sapiens4.exe") -PassThru; $p.Id', clean)
    expect(code).to eq(0), error
    native = Integer(out.strip)
    backend_ids = []
    Contract.wait(timeout: 60, description: 'installed native owned backend health') do
      snapshot = observe.call
      backends = snapshot.fetch('processes').select { |p| p['ParentProcessId'] == native && p['ExecutablePath'].tr('\\', '/').downcase == File.join(runtime, '.build/bin/sapiens4.exe').tr('\\', '/').downcase }
      backend_ids = backends.map { |p| p['ProcessId'] }
      !backends.empty? && backends.flat_map { |backend| backend.fetch('Ports') }.any? do |port|
        status, body = Contract.http("http://127.0.0.1:#{port}/api/health", 'GET')
        status == 200 && body['status'] == 'ok'
      rescue SystemCallError, Timeout::Error, EOFError
        false
      end
    end
    before = Digest::SHA256.file(File.join(app, 'Sapiens4.exe')).hexdigest
    setup.call(installer, 'active-reinstall-refused', false)
    setup.call(uninstaller, 'active-uninstall-refused', false)
    expect(observe.call.fetch('processes').map { |p| p['ProcessId'] }).to include(native)
    expect(Digest::SHA256.file(File.join(app, 'Sapiens4.exe')).hexdigest).to eq(before)
    _, error, code = ps.call("$p=Get-Process -Id #{native}; if (-not $p.CloseMainWindow() -or -not $p.WaitForExit(20000)) { exit 1 }")
    expect(code).to eq(0), error
    out, error, code = ps.call("@(#{backend_ids.join(',')}) | ForEach-Object { Get-Process -Id $_ -ErrorAction SilentlyContinue } | Select-Object -ExpandProperty Id")
    expect(code).to eq(0), error
    expect(out.strip).to be_empty
    native = nil
    setup.call(installer, 'stopped-reinstall', true)
    verify_native_runtime(runtime)
    setup.call(uninstaller, 'uninstall', true)
    expect(observe.call.values_at('registered', 'shortcut')).to eq([false, false])
    expect(File.exist?(File.join(app, 'Sapiens4.exe'))).to eq(false)
    expect(File.read(unknown)).to eq('unknown installed file must survive uninstall')
    sentinels.each { |path, digest| expect(Digest::SHA256.file(path).hexdigest).to eq(digest) }
  ensure
    ps.call("$p=Get-Process -Id #{native} -ErrorAction SilentlyContinue; if ($p) { [void]$p.CloseMainWindow(); if (-not $p.WaitForExit(10000)) { & taskkill.exe /PID #{native} /T /F | Out-Null } }") if native && ps
    warn "Installer evidence: #{evidence}; owned fixture: #{work}" if work
  end
end
