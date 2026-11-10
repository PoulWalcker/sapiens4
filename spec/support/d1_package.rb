require_relative 'desktop'

# Real build/install operations shared by the D1 acceptance scenarios only.
class D1Package
  attr_reader :root

  def initialize
    @root = File.realpath(Dir.mktmpdir('sapiens-d1-'))
    @logs = File.join(Contract::ROOT, '.build', "d1-acceptance-#{File.basename(root)}")
    FileUtils.mkdir_p(@logs)
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
  def name(channel); channel == 'stable' ? 'Sapiens4' : 'Sapiens4 Development'; end
  def output(channel); File.join(root, channel); end
  def built_app(channel); File.join(output(channel), windows? ? "#{name(channel).tr(' ', '-')}-win-x64" : "#{name(channel)}.app"); end
  def runtime(app); File.join(app, windows? ? 'runtime' : 'Contents/Resources/runtime'); end
  def binary(app); File.join(app, windows? ? 'Sapiens4.exe' : 'Contents/MacOS/Sapiens4'); end
  def packager; File.join(Contract::ROOT, '.build/package-tool', windows? ? 'sapiens-package.exe' : 'sapiens-package'); end

  def ps(script, env: {}, **options)
    shell = File.join(ENV.fetch('SystemRoot'), 'System32/WindowsPowerShell/v1.0/powershell.exe')
    run(shell, '-NoProfile', '-NonInteractive', '-Command',
      "$ErrorActionPreference='Stop'; [Console]::OutputEncoding=[Text.UTF8Encoding]::new($false); #{script}", env: env, **options)
  end

  def build
    if windows?
      raise 'Use a disposable Windows account and set SAPIENS_TEST_D1_WINDOWS_DISPOSABLE=1' unless ENV['SAPIENS_TEST_D1_WINDOWS_DISPOSABLE'] == '1'
      %w[development stable].each do |channel|
        raise "Existing #{name(channel)} registration or shortcut; use a clean account" if registered?(channel)
      end
    end
    %w[development stable].each_with_index do |channel, index|
      started = Process.clock_gettime(Process::CLOCK_MONOTONIC)
      if windows?
        args = ['powershell.exe', '-NoProfile', '-ExecutionPolicy', 'Bypass', '-File',
          'environments/windows/build.ps1', '-Channel', channel, '-Version', '0.1.0-dev.acceptance', '-OutputDirectory', output(channel)]
        args << '-SkipBuild' unless index.zero?
        run(*args, env: {'SAPIENS_WINDOWS_CERT_THUMBPRINT' => nil}, timeout: 1800)
      else
        args = ['runghc', '-ibuild/package', 'build/Package.hs', 'macos', '--channel', channel,
          '--version', '0.1.0-dev.acceptance', '--output-directory', output(channel), '--sign-identity', '-', '--notary-profile', '']
        args << '--skip-build' unless index.zero?
        run(*args, timeout: 1800)
      end
      warn format('D1 %s build/package: %.1fs', channel, Process.clock_gettime(Process::CLOCK_MONOTONIC) - started)
    end
  end

  def registration(channel)
    suffix = channel == 'stable' ? '' : '.development'
    "HKCU:\\Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\com.poulwalcker.sapiens4#{suffix}_is1"
  end

  def registered?(channel)
    registration_state(channel).values_at('registered', 'shortcut').any?
  end

  def registration_state(channel)
    JSON.parse(ps(<<~'PS', env: {'D1_REGISTRY' => registration(channel), 'D1_NAME' => name(channel)}))
      $r=Get-ItemProperty -LiteralPath $env:D1_REGISTRY -ErrorAction SilentlyContinue
      @{registered=($null -ne $r); location=$r.InstallLocation; shortcut=(Test-Path -LiteralPath (Join-Path ([Environment]::GetFolderPath('Programs')) ($env:D1_NAME+'.lnk')))} | ConvertTo-Json -Compress
    PS
  end

  def install(channel, folder)
    FileUtils.mkdir_p(folder)
    app = File.join(folder, windows? ? name(channel) : "#{name(channel)}.app")
    if windows?
      installer(channel, app)
      location = registration_state(channel).fetch('location')
      raise "Installer registered a different directory: #{location}" unless File.expand_path(location).downcase == File.expand_path(app).downcase
    else
      mount = File.join(root, 'mounted-image')
      run('hdiutil', 'attach', '-readonly', '-nobrowse', '-mountpoint', mount, File.join(output(channel), 'Sapiens.dmg'))
      begin
        FileUtils.rm_rf(app)
        run('ditto', File.join(mount, "#{name(channel)}.app"), app)
      ensure
        run('hdiutil', 'detach', mount)
      end
      run('codesign', '--verify', '--deep', '--strict', app)
    end
    app
  end

  def installer(channel, app, remove: false, success: true)
    program = remove ? File.join(app, 'unins000.exe') : File.join(output(channel), 'Sapiens-Setup.exe')
    log = File.join(@logs, "installer-#{@commands + 1}.log")
    arguments = ['/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART', '/SP-', "/LOG=\"#{log}\""]
    arguments << "/DIR=\"#{app}\"" unless remove
    ps(<<~'PS', env: {'D1_INSTALLER' => program, 'D1_ARGUMENTS' => arguments.join(' ')}, success: success, timeout: 150)
      $p=Start-Process -FilePath $env:D1_INSTALLER -ArgumentList $env:D1_ARGUMENTS -PassThru
      if (-not $p.WaitForExit(120000)) { $p.Kill(); throw 'Installer timed out' }
      exit $p.ExitCode
    PS
    unless success
      raise "Installer failed for an unexpected reason; see #{log}" unless File.binread(log).delete("\0").include?('Install/uninstall refused: an executable from the installation is running.')
    end
  end

  def uninstall(channel, app)
    windows? ? installer(channel, app, remove: true) : FileUtils.remove_entry(app)
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

  def with_native(app, home, overrides = {}, args: [])
    env = environment(home).merge(overrides)
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
    yield({pid: pid, backend: owned.first.fetch('pid'), url: address})
    if windows?
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

  def close
    # Failed Windows installations are kept for diagnosis rather than deleting
    # registered files behind the uninstaller's back.
    if windows? && %w[development stable].any? { |channel| registered?(channel) }
      warn "D1 installation remains in #{root}; remove it in the disposable account."
    else
      FileUtils.remove_entry(root)
    end
  end
end
