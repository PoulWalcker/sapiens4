require_relative 'support/desktop'
require_relative 'support/windows_webview2_cleanup'
require 'digest'
require 'securerandom'

RSpec.describe 'Installed D1 distribution' do
  def command(*args, **options)
    out, err, code = Contract.capture(*args, **options)
    expect(code).to eq(0), "#{args.first}: #{out}#{err}"
    out
  end

  def launch(binary, env, cwd, observation: nil)
    process = Contract::Child.new(binary, env: env, chdir: cwd)
    begin
      observation&.snapshot('launched', process.pid)
      if RUBY_PLATFORM.include?('darwin')
        Contract.wait(description: "Launch Services registration of #{process.pid}") do
          JSON.parse(command(Desktop.driver, 'apps')).fetch('apps').any? { |app| app['pid'] == process.pid }
        end
      end
      yield Desktop.new(process: process, ready: false), process
    ensure
      process.close
    end
  end

  def alive?(pid)
    Process.kill(0, pid)
    true
  rescue Errno::ESRCH
    false
  end

  def send_message(desktop, text)
    desktop.act_on('Message ChiefSapi', role: %w[AXTextArea AXTextField]) do |node|
      desktop.control('paste', '--target-path', node.fetch('path'), '--text', text)
    end
    desktop.value('Message ChiefSapi', text)
    desktop.press('Send message')
  end

  def quit(desktop, process)
    desktop.key(Gem.win_platform? ? 'alt+F4' : 'command+q')
    out, err, code = process.finish(30)
    expect(code).to eq(0), out + err
  end

  it 'reopens completed work after replacing the DMG app that quit during an active request', :macos do
    skip 'Set SAPIENS_TEST_DMG on macOS' unless RUBY_PLATFORM.include?('darwin') && ENV['SAPIENS_TEST_DMG']
    dmg = File.realpath(ENV.fetch('SAPIENS_TEST_DMG'))
    provider = Host.new(start: false)
    Dir.mktmpdir('Sapiens installed ') do |dir|
      mount, app, data, config = %w[volume Sapiens4.app data config].map { |name| File.join(dir, name) }
      FileUtils.mkdir_p([mount, config])
      token = SecureRandom.hex(12)
      marker = File.join(config, 'keep')
      File.write(marker, token)
      binary = provider.environment.fetch('SAPIENS_CODEX_BINARY')
      File.write(binary, "#!#{RbConfig.ruby}\n" + File.read(File.join(__dir__, 'support/provider.rb')))
      FileUtils.cp(File.join(__dir__, 'support/heartbeat.rb'), File.dirname(binary))
      cli_hash = Digest::SHA256.file(binary).hexdigest
      env = provider.environment.merge('HOME' => dir, 'CODEX_HOME' => File.join(dir, '.codex'),
        'SAPIENS_DATA_DIR' => data, 'SAPIENS_CONFIG_DIR' => config, 'SAPIENS_RUNTIME_DIR' => nil,
        'SAPIENS_REPOSITORY' => nil, 'PATH' => '/usr/bin:/bin:/usr/sbin:/sbin')
      host = File.join(app, 'Contents/MacOS/Sapiens4')
      install = lambda do
        command('hdiutil', 'attach', '-readonly', '-nobrowse', '-mountpoint', mount, dmg)
        begin
          command('ditto', File.join(mount, 'Sapiens4.app'), app)
        ensure
          command('hdiutil', 'detach', mount)
        end
      end
      install.call
      launch(host, env, dir) do |desktop, process|
        desktop.press('Start fresh')
        desktop.press('Continue')
        send_message(desktop, token)
        desktop.see('Contract test reply')
        expect(provider.calls.first.fetch('prompt')).to include(token)
        backend = Integer(command('pgrep', '-P', process.pid.to_s, '-x', 'sapiens4').strip)
        path = command('ps', '-p', backend.to_s, '-o', 'comm=').strip
        expect(File.realpath(path)).to eq(File.realpath(File.join(app, 'Contents/Resources/runtime/.build/bin/sapiens4')))
        provider.configure(rules: {'Assessing' => {block: true, child: true}})
        send_message(desktop, "Hold #{token}")
        call = Contract.wait(description: 'active external request and its child') do
          provider.calls.find { |c| c['prompt'].include?("Hold #{token}") && c['heartbeat'] && File.exist?(c['heartbeat']) }
        end
        owned = [backend, call.fetch('pid'), call.fetch('child')]
        owned.each { |pid| Process.kill(0, pid) }
        quit(desktop, process)
        Contract.wait(description: "Quit to stop owned PIDs #{owned.inspect}") do
          owned.none? { |pid| alive?(pid) }
        end
        RSpec.configuration.reporter.message("Quit stopped host #{process.pid}, backend/CLI/child #{owned.inspect}")
      end
      FileUtils.remove_entry(app)
      install.call
      launch(host, env.merge('SAPIENS_DATA_DIR' => nil), dir) do |desktop, process|
        expect(desktop.see('Contract test reply')).to include(token)
        quit(desktop, process)
      end
      db = File.join(data, 'corpora.sqlite3')
      saved = Digest::SHA256.file(db).hexdigest
      FileUtils.remove_entry(app)
      expect(File.exist?(app)).to be(false)
      expect(Digest::SHA256.file(db).hexdigest).to eq(saved)
      expect(File.read(marker)).to eq(token)
      expect(Digest::SHA256.file(binary).hexdigest).to eq(cli_hash)
      RSpec.configuration.reporter.message("Reopened request #{token}; removed app; retained database SHA-256 #{saved}")
    end
  ensure
    provider&.close
  end

  it 'recovers the installed Windows shell and protects running files and user state through Setup', :windows do
    setup = ENV['D1_CLEANUP_SETUP'] || ENV['SAPIENS_TEST_SETUP']
    skip 'Set SAPIENS_TEST_SETUP in a disposable Windows x64 user session' unless Gem.win_platform? && setup
    setup = File.realpath(setup)
    command('powershell.exe', '-NoProfile', '-NonInteractive', '-Command', <<~'PS')
      $ErrorActionPreference = 'Stop'
      $principal = New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
      if ($principal.IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator)) { throw 'Use an unelevated test user' }
      if ((Get-CimInstance Win32_Processor | Select-Object -First 1).Architecture -ne 9) { throw 'Use a native x64 Windows host' }
      foreach ($view in 'Registry32','Registry64') {
        $hkcu = [Microsoft.Win32.RegistryKey]::OpenBaseKey('CurrentUser',$view)
        if ($hkcu.OpenSubKey('Software\Microsoft\Windows\CurrentVersion\Uninstall\com.sapiens4.desktop_is1')) { throw 'Existing installation' }
      }
      if (Test-Path (Join-Path ([Environment]::GetFolderPath('Programs')) 'Sapiens4')) { throw 'Existing Start menu group' }
    PS
    evidence = ENV['D1_CLEANUP_EVIDENCE']
    if evidence
      cases = ENV.fetch('D1_CLEANUP_CASES', 'A B C D E').split
      raise 'Expected cleanup cases A–E' if cases.empty? || !(cases - %w[A B C D E]).empty?
      FileUtils.mkdir_p(evidence)
      File.write(File.join(evidence, 'identity.txt'), "source=#{ENV['D1_CLEANUP_REVISION']}\nsetup=#{setup}\nsha256=#{Digest::SHA256.file(setup).hexdigest}\n")
      survivors = cases.flat_map do |label|
        dir = File.join(File.expand_path(evidence), "case-#{label}")
        Dir.mkdir(dir)
        windows_install(dir, setup, label)
      end
      expect(survivors).to be_empty
    else
      provider = Host.new(start: false)
      Dir.mktmpdir('sapiens-setup-') do |dir|
        expect(windows_install(dir, setup, 'installed', provider)).to be_empty
      end
    end
  ensure
    provider&.close
  end

  def windows_session(host, env, dir, profile, token, scenario, observation)
    if %w[A B C].include?(scenario)
      FileUtils.mkdir_p(File.dirname(profile))
      File.write(profile, token)
    end
    launch(host, env, dir, observation: observation) do |desktop, process|
      sleep 2
      observation.snapshot('two-seconds-after-launch')
      if %w[A B C].include?(scenario)
        recovery = -> { Contract.wait(timeout: 60, description: 'native startup recovery') { desktop.text.include?('Sapiens4 could not start') } }
        recovery.call
        observation.snapshot('first-recovery')
        desktop.find('Retry', role: 'AXButton')
        if scenario == 'B'
          desktop.press('Retry')
          sleep 1
          observation.snapshot('retry-launched')
          recovery.call
          observation.snapshot('second-recovery')
        end
        expect(File.read(profile)).to eq(token)
        File.unlink(profile) if scenario == 'C'
        desktop.press(scenario == 'C' ? 'Retry' : 'Cancel')
      end
      unless %w[A B].include?(scenario)
        desktop.press('Start fresh')
        desktop.press('Continue')
        desktop.see('Message ChiefSapi')
        observation.snapshot(scenario == 'C' ? 'recovered-startup' : 'successful-startup')
        yield desktop
        desktop.key('alt+F4')
      end
      out, err, code = process.finish(35)
      File.write(File.join(dir, 'host.log'), "exit=#{code}\n#{out}\n#{err}")
      expect(code).to eq(0), out + err
    end
    sleep 5
    observation.snapshot('five-seconds-after-host-exit')
  end

  def windows_install(dir, setup, label, provider = nil)
    install, unsafe, data, config = %w[install unrelated data config].map { |name| File.join(dir, name) }
    profile = File.join(config, 'WebView2/Shell')
    observation = WindowsWebViewProcesses.new(dir, label)
    token = SecureRandom.hex(12)
    env = (provider&.environment || {'SAPIENS_CODEX_BINARY' => ENV['SAPIENS_TEST_CODEX']}).merge(
      'SAPIENS_DATA_DIR' => data, 'SAPIENS_CONFIG_DIR' => config, 'SAPIENS_RUNTIME_DIR' => nil,
      'SAPIENS_REPOSITORY' => nil, 'CODEX_HOME' => File.join(dir, '.codex'))
    flags = %w[/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP-]
    run_setup = lambda do |path, log, expected = 0|
      out, err, code = Contract.capture(setup, *flags, "/DIR=#{path.tr('/', '\\')}", "/LOG=#{File.join(dir, log)}", env: env, chdir: dir, timeout: 120)
      expect(code).to eq(expected), out + err
    end
    uninstall = File.join(install, 'unins000.exe')
    host = File.join(install, 'Sapiens4.exe')
    if provider
      FileUtils.mkdir_p([unsafe, config])
      keep = [unsafe, config].map { |path| File.join(path, 'keep.txt') }
      keep.each { |path| File.write(path, token) }
      run_setup.call(unsafe, 'refused.log', 7)
      expect(Dir.children(unsafe)).to eq(['keep.txt'])
    end
    observation.snapshot('before-install')
    run_setup.call(install, 'setup.log')
    (provider ? %w[A C] : [label]).each do |scenario|
      windows_session(host, env, dir, profile, token, scenario, observation) do |desktop|
        next unless provider
        run_setup.call(install, 'running.log', 7)
        desktop.fill('Message ChiefSapi', token)
        desktop.press('Send message')
        expect(desktop.see('Contract test reply')).to include(token)
        expect(provider.calls.first.fetch('prompt')).to include(token)
      end
    end
    if provider
      keep << File.join(install, 'user-document.txt')
      File.write(keep.last, token)
    end
    if provider || label == 'E'
      run_setup.call(install, 'reinstall.log')
      observation.snapshot('reinstalled')
    end
    if provider
      launch(host, env.merge('SAPIENS_DATA_DIR' => nil), dir) do |desktop, process|
        observation.snapshot('reopened', process.pid)
        expect(desktop.see('Contract test reply')).to include(token)
        quit(desktop, process)
      end
      db = File.join(data, 'corpora.sqlite3')
      digest = Digest::SHA256.file(db).hexdigest
    end
    command(uninstall, *flags, "/LOG=#{File.join(dir, 'uninstall.log')}", env: env, chdir: dir, timeout: 120)
    Contract.wait(timeout: 25, description: 'uninstaller self-removal') { !File.exist?(uninstall) }
    survivors = observation.after_uninstall
    if provider
      remaining = Dir.glob('**/*', File::FNM_DOTMATCH, base: install).select { |name| File.file?(File.join(install, name)) }
      expect(remaining).to contain_exactly(File.basename(keep.last))
      expect(Digest::SHA256.file(db).hexdigest).to eq(digest)
      keep.each { |path| expect(File.read(path)).to eq(token) }
    end
    survivors
  rescue Exception => error
    File.write(File.join(dir, 'failure.txt'), error.full_message)
    observation&.snapshot('harness-failure') rescue nil
    raise
  ensure
    [uninstall, File.join(unsafe, 'unins000.exe')].compact.each do |path|
      command(path, *flags, env: env, chdir: dir, timeout: 120) if File.file?(path)
    end
  end
end
