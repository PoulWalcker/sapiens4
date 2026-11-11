require_relative 'support/desktop'

RSpec.describe 'Native first-run onboarding', :desktop do
  def click_setup(text, role: nil)
    Contract.wait(timeout: 3, description: "native window ready for #{text}") do
      desktop.click_node(desktop.find(text, role: role))
      true
    rescue RuntimeError => error
      # This ownership rejection happens before input; never retry an uncertain click.
      raise unless error.message.match?(/\ABlindly4 (?:click|activate): .*"error":"(?:Click point belongs to another process|Target application is not frontmost; refusing cross-app input)"/)
      false
    end
  end

  def backend_pids
    IO.popen(['ps', '-axo', 'pid=,ppid=,comm='], &:read).lines.filter_map do |line|
      pid, parent, command = line.strip.split(/\s+/, 3)
      pid.to_i if parent.to_i == @launcher.pid && File.basename(command.to_s) == 'sapiens4'
    end
  end

  def launch_setup(binary, environment)
    @desktop = nil
    @launcher = Contract::Child.new(binary, env: environment)
    @desktop = Contract.wait(description: 'native application registration') do
      begin
        Desktop.new(process: @launcher, ready: false)
      rescue RuntimeError => error
        raise unless error.message.match?(/is not a running application|No visible windows for PID/)
        false
      end
    end
  end

  def quit_setup
    desktop.control('activate')
    desktop.key('command+q')
    _, error, status = @launcher.finish
    expect(status).to eq(0), "Native shutdown failed: #{error}"
    @launcher.close
    @launcher = nil
  end

  def choose_storage(path)
    desktop.find('Choose data', role: 'AXButton')
    desktop.control('activate')
    desktop.key('command+shift+g')
    Contract.wait(description: 'folder picker text focus') do
      %w[AXTextField AXComboBox].include?(desktop.control('focused')['role'])
    end
    desktop.control('paste', '--text', path)
    desktop.key('return')
    click_setup('Choose data')
  end

  def with_setup
    skip 'This journey drives the macOS native folder picker' unless RUBY_PLATFORM.include?('darwin')
    @host = Host.new
    @studio = host.api('POST', '/api/host/local', {name: 'Onboarding Studio'}, status: 201)
    host.stop
    Dir.mktmpdir('sapiens-onboarding-') do |directory|
      directory = File.realpath(directory)
      contents = File.join(directory, 'Onboarding.app/Contents')
      FileUtils.mkdir_p(File.join(contents, 'MacOS'))
      binary = File.join(contents, 'MacOS/Sapiens4')
      FileUtils.cp(Desktop.binary, binary)
      File.write(File.join(contents, 'Info.plist'), <<~PLIST)
        <?xml version="1.0" encoding="UTF-8"?>
        <plist version="1.0"><dict>
        <key>CFBundleExecutable</key><string>Sapiens4</string>
        <key>CFBundleIdentifier</key><string>com.sapiens4.onboarding-contract</string>
        <key>CFBundlePackageType</key><string>APPL</string>
        <key>NSHighResolutionCapable</key><true/>
        <key>NSAppTransportSecurity</key><dict><key>NSAllowsLocalNetworking</key><true/>
        <key>NSAllowsArbitraryLoadsInWebContent</key><true/></dict>
        </dict></plist>
      PLIST
      config, fresh = %w[config fresh].map { |name| File.join(directory, name) }
      tool = File.join(directory, 'permission-tool')
      File.write(tool, "#!#{RbConfig.ruby}\nload #{File.join(__dir__, 'support/onboarding_tools.rb').inspect}\n")
      File.chmod(0755, tool)
      environment = host.environment.merge('SAPIENS_CONFIG_DIR' => config, 'SAPIENS_DATA_DIR' => fresh,
        'SAPIENS_BLINDLY_BINARY' => tool, 'SAPIENS_ONBOARDING_FIXTURE' => directory)
      yield directory, binary, environment, config, fresh
    rescue Exception
      warn @launcher.stderr if @launcher
      warn @desktop.text if @desktop
      raise
    ensure
      @launcher&.close
      @launcher = nil
      @desktop = nil
    end
  end

  it 'selects storage before startup, switches safely, resumes permission setup and saves completion before opening the restored workspace' do
    with_setup do |directory, binary, environment, config, fresh|
      invalid = File.join(directory, 'invalid')
      Dir.mkdir(invalid)
      marker = File.join(invalid, 'keep.txt')
      File.write(marker, 'Preserve existing data')
      selection, setup = %w[storage.json onboarding.json].map { |name| File.join(config, name) }
      launch_setup(binary, environment)
      desktop.see('Meet your Sapis.')
      desktop.find('Start fresh')
      expect(backend_pids).to be_empty
      expect(File.exist?(selection)).to be(false)
      click_setup('Continue')
      desktop.see('Computer access')
      original = backend_pids
      expect(original.size).to eq(1)
      identity = File.binread(File.join(fresh, 'corpora-identity.json'))
      saved_selection = File.binread(selection)
      click_setup('Your data')
      click_setup('Start fresh')
      click_setup('Continue')
      desktop.see('Computer access')
      expect(backend_pids).to eq(original)
      expect(File.binread(File.join(fresh, 'corpora-identity.json'))).to eq(identity)
      click_setup('Your data')
      click_setup('Use another folder')
      choose_storage(invalid)
      expect(backend_pids).to eq(original)
      expect(File.binread(selection)).to eq(saved_selection)
      click_setup('Continue')
      desktop.see('This is not a complete Sapiens Data Root.')
      expect(backend_pids).to eq(original)
      expect(File.read(marker)).to eq('Preserve existing data')
      expect(File.exist?(File.join(invalid, 'corpora.sqlite3'))).to be(false)
      click_setup('Choose another folder')
      choose_storage(host.root)
      click_setup('Continue')
      desktop.see('Computer access')
      expect(backend_pids.size).to eq(1)
      expect(backend_pids).not_to eq(original)
      expect(JSON.parse(File.read(selection))['directory']).to eq(host.root)
      click_setup('Recheck access')
      desktop.see('Sapiens needs Accessibility permission.')
      click_setup('Skip for now')
      desktop.see("You're ready")
      click_setup('← Back')
      desktop.see('Computer access')
      expect(JSON.parse(File.read(setup))['step']).to eq('permissions')
      quit_setup
      launch_setup(binary, environment)
      desktop.see('Computer access')
      click_setup('Enable access')
      Contract.wait { File.exist?(File.join(directory, 'permission-requested')) }
      File.write(File.join(directory, 'permission-granted'), '')
      click_setup('Recheck access')
      desktop.see('Computer control enabled.')
      click_setup('Continue')
      desktop.see("You're ready")
      expect(JSON.parse(File.read(setup))['step']).to eq('ready')
      quit_setup
      launch_setup(binary, environment)
      desktop.see("You're ready")
      ready = File.binread(setup)
      File.rename(setup, setup + '.saved')
      Dir.mkdir(setup)
      begin
        click_setup('Open Workspace')
        desktop.see('Could not save setup.')
        desktop.find("You're ready", role: 'AXHeading')
        expect(File.binread(setup + '.saved')).to eq(ready)
      ensure
        Dir.rmdir(setup)
        File.rename(setup + '.saved', setup)
      end
      click_setup('Open Workspace')
      desktop.find('Message ChiefSapi')
      expect(JSON.parse(File.read(setup))).to eq('schema' => 2, 'step' => 'complete')
      desktop.click('Local')
      desktop.click('Onboarding Studio')
      desktop.click('Close corpora menu')
      desktop.find('Onboarding Studio', role: 'AXHeading')
      desktop.fill('Message ChiefSapi', 'Retain this draft')
      quit_setup
      launch_setup(binary, environment)
      desktop.find('Onboarding Studio', role: 'AXHeading')
      desktop.value('Message ChiefSapi', 'Retain this draft')
      expect(desktop.text).not_to include('Meet your Sapis.', 'Connect your AI', "You're ready")
      quit_setup
    end
  end

  it 'rolls back failed replacement startup, retries locked storage, and recovers a missing saved folder without resetting completion' do
    with_setup do |directory, binary, environment, config, _fresh|
      selection = File.join(config, 'storage.json')
      broken = File.join(host.root, 'agentpy/corpora/directory.json')
      saved = File.binread(broken)
      File.write(broken, '{broken')
      launch_setup(binary, environment)
      click_setup('Continue')
      desktop.see('Computer access')
      original = backend_pids
      selected = File.binread(selection)
      click_setup('Your data')
      click_setup('Use another folder')
      choose_storage(host.root)
      click_setup('Continue')
      desktop.see("We couldn't switch storage. Your previous storage has been reopened.")
      click_setup('OK')
      desktop.see('Computer access')
      expect(File.binread(selection)).to eq(selected)
      expect(backend_pids.size).to eq(1)
      expect(backend_pids).not_to eq(original)
      File.binwrite(broken, saved)
      host.start
      click_setup('Your data')
      click_setup('Use another folder')
      choose_storage(host.root)
      click_setup('Continue')
      desktop.see('Another Sapiens4 server')
      expect(File.binread(selection)).to eq(selected)
      host.stop
      click_setup('Continue')
      desktop.see('Computer access')
      click_setup('Skip for now')
      desktop.see("You're ready")
      click_setup('Open Workspace')
      desktop.find('Message ChiefSapi')
      quit_setup
      moved = host.root + '.moved'
      File.rename(host.root, moved)
      begin
        launch_setup(binary, environment)
        desktop.see('This data directory is unavailable.')
        expect(backend_pids).to be_empty
        click_setup('Use another folder')
        choose_storage(moved)
        click_setup('Continue')
        desktop.find('Message ChiefSapi')
        expect(desktop.text).not_to include('Computer access', "You're ready")
        expect(JSON.parse(File.read(selection))['directory']).to eq(moved)
        quit_setup
      ensure
        File.rename(moved, host.root)
      end
    end
  end
end
