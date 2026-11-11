require_relative 'desktop'

# Real build/install operations shared by the D1 acceptance scenarios only.
class D1Package
  attr_reader :root

  def initialize
    @root = File.realpath(Dir.mktmpdir('sapiens-d1-'))
    @logs = File.join(Contract::ROOT, '.build', "d1-acceptance-#{File.basename(root)}")
    FileUtils.mkdir_p(@logs)
    @artifact = File.expand_path(ENV.fetch('SAPIENS_TEST_D1_INSTALLER'))
    @commands = 0
    warn "D1 logs: #{@logs}"
  end

  def run(*command, success: true, **options)
    @commands += 1
    out, error, status = Contract.capture(*command, **options)
    log = File.join(@logs, format('%03d.log', @commands))
    File.write(log, "#{command.inspect}\nexit: #{status}\n#{out}\n#{error}")
    raise "Unexpected exit (#{status}): #{command.inspect}; see #{log}\n#{error[-2000..] || error}" unless (status == 0) == success
    out
  end

  def windows?; Gem.win_platform?; end
  def name; 'Sapiens4'; end
  def runtime(app); File.join(app, windows? ? 'runtime' : 'Contents/Resources/runtime'); end
  def binary(app); File.join(app, windows? ? 'Sapiens4.exe' : 'Contents/MacOS/Sapiens4'); end

  def ps(script, env: {}, **options)
    shell = File.join(ENV.fetch('SystemRoot'), 'System32/WindowsPowerShell/v1.0/powershell.exe')
    run(shell, '-NoProfile', '-NonInteractive', '-Command',
      "$ErrorActionPreference='Stop'; [Console]::OutputEncoding=[Text.UTF8Encoding]::new($false); #{script}", env: env, **options)
  end

  def verify_artifact
    if windows?
      raise 'Use a disposable Windows account and set SAPIENS_TEST_D1_WINDOWS_DISPOSABLE=1' unless ENV['SAPIENS_TEST_D1_WINDOWS_DISPOSABLE'] == '1'
      raise 'Existing Sapiens4 registration or shortcut; use a clean account' if registered?
    end
    checksum = File.read(File.join(File.dirname(@artifact), 'SHA256SUMS')).lines.find { |line| line.split.last == File.basename(@artifact) }
    raise 'Missing or invalid installer checksum' unless checksum && checksum.split.first == Digest::SHA256.file(@artifact).hexdigest
    if windows?
      ps('if ((Get-AuthenticodeSignature -LiteralPath $env:D1_ARTIFACT).Status -ne "Valid") { throw "Invalid installer signature" }', env: {'D1_ARTIFACT' => @artifact}) if public_release?
    else
      run('hdiutil', 'verify', @artifact)
      if public_release?
        run('codesign', '--verify', '--strict', @artifact)
        run('xcrun', 'stapler', 'validate', @artifact)
        run('spctl', '--assess', '--type', 'open', '--context', 'context:primary-signature', @artifact)
      end
    end
  end

  def public_release?; ENV['SAPIENS_TEST_D1_PUBLIC'] == '1'; end

  def registration
    "HKCU:\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\com.poulwalcker.sapiens4_is1"
  end

  def registered?
    registration_state.values_at('registered', 'shortcut').any?
  end

  def registration_state
    JSON.parse(ps(<<~'PS', env: {'D1_REGISTRY' => registration, 'D1_NAME' => name}))
      $r=Get-ItemProperty -LiteralPath $env:D1_REGISTRY -ErrorAction SilentlyContinue
      @{registered=($null -ne $r); location=$r.InstallLocation; shortcut=(Test-Path -LiteralPath (Join-Path ([Environment]::GetFolderPath('Programs')) ($env:D1_NAME+'.lnk')))} | ConvertTo-Json -Compress
    PS
  end

  def install(folder)
    FileUtils.mkdir_p(folder)
    app = File.join(folder, windows? ? name : "#{name}.app")
    if windows?
      installer(app)
      if public_release?
        ps('Get-ChildItem -LiteralPath $env:D1_APP -Recurse -File | Where-Object { $_.Extension -in @(".exe",".dll") } | ForEach-Object { if ((Get-AuthenticodeSignature -LiteralPath $_.FullName).Status -ne "Valid") { throw "Invalid installed signature: $($_.FullName)" } }', env: {'D1_APP' => app})
      end
      location = registration_state.fetch('location')
      raise "Installer registered a different directory: #{location}" unless File.expand_path(location).downcase == File.expand_path(app).downcase
    else
      mount = File.join(root, 'mounted-image')
      run('hdiutil', 'attach', '-readonly', '-nobrowse', '-mountpoint', mount, @artifact)
      begin
        FileUtils.rm_rf(app)
        run('ditto', File.join(mount, "#{name}.app"), app)
      ensure
        run('hdiutil', 'detach', mount)
      end
      run('codesign', '--verify', '--deep', '--strict', app)
      if public_release?
        run('xcrun', 'stapler', 'validate', app)
        run('spctl', '--assess', '--type', 'execute', app)
      end
    end
    app
  end

  def installer(app, remove: false, success: true, reason: nil)
    program = remove ? File.join(app, 'unins000.exe') : @artifact
    log = File.join(@logs, "installer-#{@commands + 1}.log")
    arguments = ['/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART', '/SP-', "/LOG=\"#{log}\""]
    arguments << "/DIR=\"#{app}\"" unless remove
    ps(<<~'PS', env: {'D1_INSTALLER' => program, 'D1_ARGUMENTS' => arguments.join(' ')}, success: success, timeout: 150)
      $p=Start-Process -FilePath $env:D1_INSTALLER -ArgumentList $env:D1_ARGUMENTS -PassThru
      if (-not $p.WaitForExit(120000)) { $p.Kill(); throw 'Installer timed out' }
      exit $p.ExitCode
    PS
    unless success
      raise "Installer failed for an unexpected reason; see #{log}" unless File.binread(log).delete("\0").include?(reason || 'Install/uninstall refused: an executable from the installation is running.')
    end
  end

  def uninstall(app)
    windows? ? installer(app, remove: true) : FileUtils.remove_entry(app)
  end

  def environment(home)
    FileUtils.mkdir_p(home)
    clean = ENV.keys.grep(/\A(SAPIENS_|CODEX_|OPENAI_)/i).to_h { |key| [key, nil] }
    clean.merge('HOME' => home, 'USERPROFILE' => home, 'CFFIXED_USER_HOME' => home,
      'LOCALAPPDATA' => File.join(home, 'Local'), 'APPDATA' => File.join(home, 'Roaming'),
      'CODEX_HOME' => File.join(home, 'codex'), 'SAPIENS_CODEX_BINARY' => File.join(home, 'provider-disabled'),
      'PATH' => windows? ? File.join(ENV.fetch('SystemRoot'), 'System32') : '/usr/bin:/bin:/usr/sbin:/sbin')
  end

  def alive?(pid)
    return ps("[bool](Get-Process -Id #{Integer(pid)} -ErrorAction SilentlyContinue)").strip == 'True' if windows?
    Process.kill(0, pid)
    true
  rescue Errno::ESRCH
    false
  end

  def backends(pid, app)
    expected = File.join(runtime(app), '.build/bin', windows? ? 'sapiens4.exe' : 'sapiens4')
    if windows?
      JSON.parse(ps(<<~'PS', env: {'D1_PARENT' => pid.to_s, 'D1_BACKEND' => expected.tr('/', '\\')}))
        $children=@(Get-CimInstance Win32_Process -Filter "ParentProcessId=$env:D1_PARENT" | Where-Object { $_.ExecutablePath -eq $env:D1_BACKEND })
        ConvertTo-Json -Depth 4 -Compress -InputObject @($children | ForEach-Object {
          @{pid=[int]$_.ProcessId; ports=@(Get-NetTCPConnection -OwningProcess $_.ProcessId -State Listen -ErrorAction SilentlyContinue | ForEach-Object { @{address=$_.LocalAddress; port=$_.LocalPort} })}
        })
      PS
    else
      rows = run('ps', '-axo', 'pid=,ppid=,comm=').lines.map { |line| line.strip.split(/\s+/, 3) }
      rows.select { |_, parent, path| parent.to_i == pid && path == expected }.map do |child, _, _|
        out, _, status = Contract.capture('lsof', '-nP', '-a', '-p', child, '-iTCP', '-sTCP:LISTEN', '-Fn', timeout: 5)
        raise "lsof failed: #{status}" unless [0, 1].include?(status)
        ports = out.lines.filter_map { |line| match = line.match(/^n(.+):(\d+)$/); {'address' => match[1], 'port' => match[2].to_i} if match }
        {'pid' => child.to_i, 'ports' => ports}
      end
    end
  end

  def launch_native(app, home, env, args = [])
    child = nil
    if windows?
      pid = Integer(ps(<<~'PS', env: env.merge('D1_BINARY' => binary(app), 'D1_ARGS' => args.map { |arg| '"' + arg + '"' }.join(' '), 'D1_CWD' => home)).strip)
        $options=@{FilePath=$env:D1_BINARY; WorkingDirectory=$env:D1_CWD; PassThru=$true}
        if ($env:D1_ARGS) { $options.ArgumentList=$env:D1_ARGS }
        $p=Start-Process @options; $p.Id
      PS
    else
      child = Contract::Child.new([binary(app), binary(app)], *args, env: env, chdir: home)
      pid = child.pid
    end
    [child, pid]
  end

  def prerequisite_recovery(app, home)
    return unless windows?
    FileUtils.mkdir_p(home)
    loader = File.join(app, 'WebView2Loader.dll')
    File.rename(loader, loader + '.disabled')
    child, pid = launch_native(app, home, environment(home))
    desktop = Desktop.new(process: Struct.new(:pid).new(pid), ready: false)
    desktop.see('WebView2Loader.dll is missing')
    desktop.find('Install/Repair WebView2')
    raise 'Prerequisite failure started a backend' unless backends(pid, app).empty?
    File.rename(loader + '.disabled', loader)
    desktop.click('Retry')
    Contract.wait(timeout: 40, description: 'WebView2 Retry readiness') { backends(pid, app).any? { |b| !b.fetch('ports').empty? } }
    desktop.see('Sapiens')
    ps("$p=Get-Process -Id #{pid}; if (-not $p.CloseMainWindow() -or -not $p.WaitForExit(15000)) { throw 'Recovery quit failed' }")
    raise 'Recovery left a backend' unless backends(pid, app).empty?
  ensure
    File.rename(loader + '.disabled', loader) if loader && File.exist?(loader + '.disabled')
    ps("Stop-Process -Id #{pid} -Force -ErrorAction SilentlyContinue") if pid && alive?(pid)
  end

  def startup_failure(app, home)
    FileUtils.mkdir_p(home)
    child, pid = launch_native(app, home, environment(home))
    desktop = Desktop.new(process: child || Struct.new(:pid).new(pid), ready: false)
    Contract.wait(description: 'native startup failure') { desktop.text.match?(/could not (open|start)/i) }
    raise 'Startup failure left a backend' unless backends(pid, app).empty?
    if windows?
      ps("$p=Get-Process -Id #{pid}; if (-not $p.CloseMainWindow() -or -not $p.WaitForExit(15000)) { throw 'Cancel failed' }")
    else
      desktop.click('OK')
      _, error, code = child.finish
      raise error unless code == 0
    end
  ensure
    child&.close
    ps("Stop-Process -Id #{pid} -Force -ErrorAction SilentlyContinue") if windows? && pid && alive?(pid)
  end

  def with_native(app, home, overrides = {}, args: [], abrupt: false)
    env = environment(home).merge(overrides)
    child, pid = launch_native(app, home, env, args)
    owned = []
    address = Contract.wait(timeout: 40, description: 'installed native backend') do
      raise "Native process exited before readiness: #{child&.stderr}" unless alive?(pid)
      owned = backends(pid, app)
      next false if owned.empty? || owned.first.fetch('ports').empty?
      raise "Expected one owned backend, got #{owned.inspect}" unless owned.length == 1
      ports = owned.first.fetch('ports')
      raise "Backend must listen only on loopback: #{ports.inspect}" unless ports.all? { |port| port['address'] == '127.0.0.1' }
      url = "http://127.0.0.1:#{ports.first.fetch('port')}"
      status, body = Contract.http(url + '/api/health', 'GET')
      url if status == 200 && body['status'] == 'ok'
    rescue Errno::ECONNREFUSED, EOFError
      false
    end
    desktop = Desktop.new(process: child || Struct.new(:pid).new(pid), ready: false)
    Contract.wait(description: 'installed UI render') { desktop.text.include?('Sapiens') }
    yield({pid: pid, backend: owned.first.fetch('pid'), url: address, desktop: desktop})
    if abrupt
      windows? ? ps("Stop-Process -Id #{pid} -Force") : child.signal('KILL')
    elsif windows?
      ps("$p=Get-Process -Id #{pid}; if (-not $p.CloseMainWindow() -or -not $p.WaitForExit(15000)) { throw 'Native quit failed' }")
    else
      desktop = Desktop.new(process: child, ready: false)
      find_quit = lambda do
        result = desktop.control('find', '--title', 'Quit Sapiens4', '--role', 'AXMenuItem', '--depth', '6', '--limit', '10')
        matches = result.fetch('matches').select { |node| node['title'] == 'Quit Sapiens4' }
        raise 'Expected one native Quit menu item' if result['truncated'] || matches.length != 1
        matches.first
      end
      # Open the application menu, then invoke its visible Quit item. This
      # exercises normal termination without racing a global keyboard shortcut.
      desktop.control('press', '--path', find_quit.call.fetch('path').split('.')[0...-2].join('.'))
      quit = Contract.wait(description: 'visible native Quit item') do
        node = find_quit.call
        node if node['enabled'] && node.dig('size', 'height').to_f > 0
      end
      desktop.control('press', '--path', quit.fetch('path'))
      _, error, status = child.finish
      raise "Native quit failed: #{status}: #{error}" unless status == 0
    end
    Contract.wait(description: 'owned backend exit') { owned.none? { |process| alive?(process.fetch('pid')) } }
    uri = URI(address)
    begin
      socket = TCPSocket.new(uri.host, uri.port)
      socket.close
      raise "Backend port still open: #{address}"
    rescue Errno::ECONNREFUSED
    end
  ensure
    child&.close
    if windows? && pid && alive?(pid)
      run('taskkill', '/PID', pid.to_s, '/T', '/F')
    end
    # Only the exact child PIDs observed under this fixture may be cleaned up.
    (owned || []).each do |process|
      backend = process.fetch('pid')
      next unless alive?(backend)
      windows? ? run('taskkill', '/PID', backend.to_s, '/T', '/F') : Process.kill('TERM', backend)
    end
  end

  def with_backend(app, home, overrides = {}, args: [])
    program = File.join(runtime(app), '.build/bin', windows? ? 'sapiens4.exe' : 'sapiens4')
    child = Contract::Child.new(program, '--desktop', '--port', '0', *args, env: environment(home).merge(overrides), chdir: home)
    Contract.wait(description: 'packaged CLI readiness') { child.stdout.include?('Sapiens4: http://127.0.0.1:') }
    yield child
    child.input.close
    _, error, status = child.finish
    raise "Packaged CLI EOF shutdown failed: #{status}: #{error}" unless status == 0
    raise 'Packaged CLI is still alive' if alive?(child.pid)
  ensure
    child&.close
  end

  def close
    # Failed Windows installations are kept for diagnosis rather than deleting
    # registered files behind the uninstaller's back.
    if windows? && registered?
      warn "D1 installation remains in #{root}; remove it in the disposable account."
    else
      FileUtils.remove_entry(root)
    end
  end
end
