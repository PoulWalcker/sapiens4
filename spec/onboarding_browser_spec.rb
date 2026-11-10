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

  def cancel_sign_in(directory)
    desktop.see('Copy sign-in link')
    pid = Integer(File.read(File.join(directory, 'login.pid')))
    click_setup('Cancel')
    desktop.see('Sign-in cancelled.')
    Contract.wait(description: 'cancelled login process exits') do
      Process.kill(0, pid)
      false
    rescue Errno::ESRCH
      true
    end
  end

  it 'recovers storage and sign-in errors, retains setup decisions, and finishes durably in the chosen Corpora' do
    skip 'This journey drives the macOS native storage picker' unless RUBY_PLATFORM.include?('darwin')
    @host = Host.new
    studio = host.api('POST', '/api/host/local', {name: 'Onboarding Studio'}, status: 201)
    host.stop

    Dir.mktmpdir('sapiens-onboarding-') do |directory|
      directory = File.realpath(directory)
      # Use an app bundle for native activation, with the current compiled binary.
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
      config, fresh, invalid = %w[config fresh invalid].map { |name| File.join(directory, name) }
      FileUtils.mkdir_p(invalid)
      marker = File.join(invalid, 'keep.txt')
      File.write(marker, 'Existing data must survive rejected storage')
      tool = File.join(directory, 'onboarding-tool')
      File.write(tool, "#!#{RbConfig.ruby}\nload #{File.join(__dir__, 'support/onboarding_tools.rb').inspect}\n")
      File.chmod(0755, tool)
      File.write(File.join(directory, 'login-mode'), 'fail')
      environment = host.environment.merge('SAPIENS_CONFIG_DIR' => config, 'SAPIENS_DATA_DIR' => fresh,
        'SAPIENS_CODEX_BINARY' => tool, 'SAPIENS_BLINDLY_BINARY' => tool, 'SAPIENS_ONBOARDING_FIXTURE' => directory)
      selection, setup = %w[storage.json onboarding.json].map { |name| File.join(config, name) }
      begin
        # First launch and same-session Start fresh reuse.
        launch_setup(binary, environment)
        desktop.see('Meet your Sapis.')
        click_setup('Continue')
        desktop.see('Where should we start?')
        expect(backend_pids).to be_empty
        expect(File.exist?(selection)).to be(false)
        click_setup('Continue')
        desktop.see('Connect your AI')
        original = backend_pids
        expect(original.size).to eq(1)
        identity = File.binread(File.join(fresh, 'corpora-identity.json'))
        saved_selection = File.binread(selection)
        click_setup('← Back')
        click_setup('Start fresh')
        click_setup('Continue')
        desktop.see('Connect your AI')
        expect(backend_pids).to eq(original)
        expect(File.binread(File.join(fresh, 'corpora-identity.json'))).to eq(identity)

        # A draft does not apply storage; rejection leaves the old host usable.
        click_setup('← Back')
        click_setup('Use another folder')
        choose_storage(invalid)
        expect(backend_pids).to eq(original)
        expect(File.binread(selection)).to eq(saved_selection)
        click_setup('Continue')
        desktop.see('This is not a complete Sapiens Data Root.')
        expect(backend_pids).to eq(original)
        expect(File.binread(selection)).to eq(saved_selection)
        expect(File.read(marker)).to eq('Existing data must survive rejected storage')
        expect(File.exist?(File.join(invalid, 'corpora.sqlite3'))).to be(false)
        click_setup('Choose another folder')
        choose_storage(host.root)
        expect(File.binread(selection)).to eq(saved_selection)
        click_setup('Continue')
        desktop.see('Connect your AI')
        expect(backend_pids.size).to eq(1)
        expect(backend_pids).not_to eq(original)
        expect(JSON.parse(File.read(selection))['directory']).to eq(host.root)

        # Error, retry, cancellation, and optional AI/permission steps.
        click_setup('Connect')
        desktop.see("Sign-in didn't finish.")
        File.write(File.join(directory, 'login-mode'), 'wait')
        click_setup('Try again')
        cancel_sign_in(directory)
        expect(File.exist?(File.join(config, 'codex/auth.json'))).to be(false)
        click_setup('Skip for now')
        desktop.see('Computer access')
        desktop.see('Accessibility permission')
        click_setup('Recheck access')
        desktop.see('To interact with apps on your Mac, Sapiens needs Accessibility permission.')
        expect(JSON.parse(File.read(setup))['step']).to eq('permissions')
        quit_setup
        launch_setup(binary, environment)
        desktop.see('Computer access')
        expect(JSON.parse(File.read(selection))['directory']).to eq(host.root)
        expect(JSON.parse(File.read(setup))['step']).to eq('permissions')

        # Return to AI, use the alternative login, and retain the Corpora choice.
        click_setup('← Back')
        desktop.see('Connect your AI')
        click_setup('Connect')
        cancel_sign_in(directory)
        click_setup('Having trouble signing in?')
        # The expanded help extends below the scrollable content viewport.
        desktop.control('scroll', '--direction', 'down', '--amount', '6')
        click_setup('Use a one-time code')
        desktop.see('ABCD-EFGH')
        File.write(File.join(directory, 'finish-login'), '')
        desktop.see('Connected')
        expect(desktop.text).not_to include('ABCD-EFGH', 'Copy sign-in link')
        click_setup('Continue')
        desktop.see('Computer access')
        desktop.see('Accessibility permission')
        click_setup('Continue')
        desktop.see('Choose your Corpora')
        click_setup('Onboarding Studio', role: 'AXCheckBox')
        click_setup('Continue')
        desktop.see("You're ready")
        desktop.see('Onboarding Studio')
        click_setup('← Back')
        desktop.see('Choose your Corpora')
        expect(desktop.find('Onboarding Studio', role: 'AXCheckBox')['value'].to_f).to eq(1.0)
        click_setup('Continue')
        desktop.see("You're ready")

        # A failed durable save must not dismiss setup; retry uses the same choice.
        ready = File.binread(setup)
        File.rename(setup, setup + '.saved')
        Dir.mkdir(setup)
        begin
          click_setup('Open Sapiens4')
          desktop.see('Could not save setup.')
          desktop.find("You're ready", role: 'AXHeading')
          expect(File.binread(setup + '.saved')).to eq(ready)
        ensure
          Dir.rmdir(setup)
          File.rename(setup + '.saved', setup)
        end
        click_setup('Open Sapiens4')
        desktop.find('Onboarding Studio', role: 'AXHeading')
        desktop.find('Message ChiefSapi')
        expect(JSON.parse(File.read(setup))).to include('step' => 'complete', 'selected_corpora' => studio.fetch('id'))
        quit_setup
        launch_setup(binary, environment)
        desktop.find('Onboarding Studio', role: 'AXHeading')
        desktop.find('Message ChiefSapi')
        expect(desktop.text).not_to include('Meet your Sapis.', 'Connect your AI', "You're ready")
        expect(JSON.parse(File.read(selection))['directory']).to eq(host.root)
        click_setup('Menu')
        click_setup('AI Connections')
        click_setup('Manage Codex')
        desktop.see('Connected')
        quit_setup
      rescue Exception
        if @desktop && @launcher
          warn desktop.text
          File.write(File.join(Contract::ROOT, '.build/desktop-failure-onboarding.json'), JSON.pretty_generate(desktop.tree))
        end
        raise
      ensure
        @launcher&.close
      end
    end
  end
end
