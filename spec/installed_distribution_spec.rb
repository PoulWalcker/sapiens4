require_relative 'support/desktop'
require 'digest'
require 'securerandom'

RSpec.describe 'Installed D1 distribution' do
  def command(*args, **options)
    out, err, code = Contract.capture(*args, **options)
    expect(code).to eq(0), "#{args.first}: #{out}#{err}"
    out
  end

  def launch(binary, env, cwd)
    process = Contract::Child.new(binary, env: env, chdir: cwd)
    begin
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
      # The independently installed test double must work outside the checkout too.
      binary = provider.environment.fetch('SAPIENS_CODEX_BINARY')
      File.write(binary, "#!#{RbConfig.ruby}\n" + File.read(File.join(__dir__, 'support/provider.rb')))
      FileUtils.cp(File.join(__dir__, 'support/heartbeat.rb'), provider.root)
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
      launch(host, env, dir) do |desktop, process|
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
    skip 'Set SAPIENS_TEST_SETUP in a disposable Windows x64 user session' unless Gem.win_platform? && ENV['SAPIENS_TEST_SETUP']
    setup = File.realpath(ENV.fetch('SAPIENS_TEST_SETUP'))
    # Inno uses a fixed per-user identity and Start menu group even with /DIR.
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
    provider = Host.new(start: false)
    Dir.mktmpdir('sapiens-setup-') do |dir|
      install, unsafe, data, config = %w[install unrelated data config].map { |name| File.join(dir, name) }
      FileUtils.mkdir_p([unsafe, config])
      token = SecureRandom.hex(12)
      keep = [unsafe, config].map { |path| File.join(path, 'keep.txt') }
      keep.each { |path| File.write(path, token) }
      env = provider.environment.merge('SAPIENS_DATA_DIR' => data, 'SAPIENS_CONFIG_DIR' => config, 'SAPIENS_RUNTIME_DIR' => nil,
        'SAPIENS_REPOSITORY' => nil, 'CODEX_HOME' => File.join(dir, '.codex'))
      flags = %w[/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP-]
      run_setup = lambda do |path|
        Contract.capture(setup, *flags, "/DIR=#{path.tr('/', '\\')}", env: env, chdir: dir, timeout: 120)
      end
      # The host shows its recovery controls after a 35-second WebView2 startup deadline.
      wait_for_recovery = lambda do |desktop|
        Contract.wait(timeout: 55, description: 'native Windows startup recovery') do
          desktop.text.include?('Sapiens4 could not start')
        end
      end
      uninstall = File.join(install, 'unins000.exe')
      uninstalled = false
      begin
        expect(run_setup.call(unsafe).last).to eq(7) # PrepareToInstall refused the destination.
        expect(Dir.children(unsafe)).to eq(['keep.txt'])
        expect(run_setup.call(install).last).to eq(0)
        host = File.join(install, 'Sapiens4.exe')
        shell_profile = File.join(config, 'WebView2', 'Shell')
        FileUtils.mkdir_p(File.dirname(shell_profile))
        File.write(shell_profile, token)
        launch(host, env, dir) do |desktop, process|
          wait_for_recovery.call(desktop)
          desktop.find('Retry', role: 'AXButton')
          desktop.press('Cancel')
          expect(process.finish(30).last).to eq(0)
        end
        launch(host, env, dir) do |desktop, process|
          wait_for_recovery.call(desktop)
          expect(File.read(shell_profile)).to eq(token)
          File.unlink(shell_profile)
          desktop.press('Retry')
          desktop.press('Start fresh')
          desktop.see('Message ChiefSapi')
          expect(run_setup.call(install).last).to eq(7)
          desktop.fill('Message ChiefSapi', token)
          desktop.press('Send message')
          expect(desktop.see('Contract test reply')).to include(token)
          expect(provider.calls.first.fetch('prompt')).to include(token)
          quit(desktop, process)
        end
        db = File.join(data, 'corpora.sqlite3')
        digest = Digest::SHA256.file(db).hexdigest
        keep << File.join(install, 'user-document.txt')
        File.write(keep.last, token)
        expect(run_setup.call(install).last).to eq(0)
        command(uninstall, *flags, env: env, chdir: dir, timeout: 120)
        uninstalled = true
        # Inno removes its own image asynchronously. Compare names relative to the install
        # root because TEMP may use the short spelling of the Windows user directory.
        Contract.wait(timeout: 20, description: 'uninstaller self-removal') { !File.exist?(uninstall) }
        remaining = Dir.glob('**/*', File::FNM_DOTMATCH, base: install).select { |name| File.file?(File.join(install, name)) }
        expect(remaining).to contain_exactly(File.basename(keep.last))
        expect(Digest::SHA256.file(db).hexdigest).to eq(digest)
        keep.each { |path| expect(File.read(path)).to eq(token) }
      ensure
        [uninstalled ? nil : uninstall, File.join(unsafe, 'unins000.exe')].compact.each do |path|
          command(path, *flags, env: env, chdir: dir, timeout: 120) if File.file?(path)
        end
      end
    end
  ensure
    provider&.close
  end
end
