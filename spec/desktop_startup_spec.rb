require_relative 'support/desktop'

RSpec.describe 'Minimal native desktop startup', :desktop do
  before do
    skip 'Cold-start desktop adapter is macOS or Windows' if Desktop.linux?
    @startup_directory = File.realpath(Dir.mktmpdir('sapiens-first-run-'))
    @startup_root = File.join(@startup_directory, 'data')
    @startup_config = File.join(@startup_directory, 'config')
    @startup_env = {'SAPIENS_DATA_DIR' => @startup_root, 'SAPIENS_CONFIG_DIR' => @startup_config,
      'SAPIENS_CODEX_BINARY' => File.join(@startup_directory, 'no-provider')}
  end

  after do
    @startup_process&.close
    if @startup_directory
      Contract.wait(timeout: 10, description: 'startup fixture profile release') do
        begin
          FileUtils.remove_entry(@startup_directory) if File.directory?(@startup_directory)
          true
        rescue Errno::EACCES, Errno::ENOTEMPTY, Errno::ENOENT
          raise unless Gem.win_platform?
          false
        end
      end
    end
  end

  def launch_startup
    binary = File.join(Contract::ROOT, '.build/bin', Gem.win_platform? ? 'sapiens-windows.exe' : 'sapiens-macos')
    arguments = Gem.win_platform? ? ['--runtime-dir', Contract::ROOT] : []
    @startup_process = Contract::Child.new(binary, *arguments, env: @startup_env)
    @desktop = Desktop.new(process: @startup_process, ready: false)
  end

  def quit_startup
    desktop.key(Gem.win_platform? ? 'alt+F4' : 'command+q')
    _, error, code = @startup_process.finish(20)
    expect(code).to eq(0), error
    @startup_process.close
    @startup_process = nil
    @desktop = nil
  end

  it 'goes directly from storage to workspace and restores the saved Corpora, draft and appearance after quitting' do
    launch_startup
    desktop.see('Meet your Sapis.')
    expect(File).not_to exist(File.join(@startup_root, 'corpora.sqlite3'))
    desktop.click('Continue')
    desktop.see('Message ChiefSapi')
    path = File.join(@startup_config, 'storage.json')
    Contract.wait { File.file?(path) }
    expect(JSON.parse(File.read(path))).to include('version' => 1, 'directory' => @startup_root)
    desktop.click('Local')
    desktop.click('＋ Create new local corpora')
    desktop.fill('Corpora name', 'Startup Studio')
    desktop.click('Create')
    desktop.find('Startup Studio', role: 'AXHeading')
    desktop.fill('Message ChiefSapi', 'Draft surviving a complete app quit')
    toggle = desktop.label(desktop.find(/Switch to (dark|light) theme/, role: 'AXButton'))[/Switch to (?:dark|light) theme/]
    desktop.click(toggle)
    changed = toggle.include?('dark') ? 'Switch to light theme' : 'Switch to dark theme'
    desktop.see(changed)
    quit_startup
    identity = File.binread(File.join(@startup_root, 'corpora-identity.json'))
    launch_startup
    desktop.find('Startup Studio', role: 'AXHeading')
    desktop.value('Message ChiefSapi', 'Draft surviving a complete app quit')
    desktop.see(changed)
    expect(desktop.text).not_to include('Computer access', "You're ready", 'Your data')
    expect(File.binread(File.join(@startup_root, 'corpora-identity.json'))).to eq(identity)
    expect(File).not_to exist(File.join(@startup_config, 'onboarding.json'))
    quit_startup
  end

  it 'recovers missing and partial saved storage before starting a backend and allows cancellation without replacement data' do
    FileUtils.mkdir_p(@startup_config)
    selection = File.join(@startup_config, 'storage.json')
    Contract.atomic_json(selection, version: 1, directory: @startup_root)
    original = File.binread(selection)
    launch_startup
    desktop.see('This data directory is unavailable')
    expect(File).not_to exist(@startup_root)
    expect(File.binread(selection)).to eq(original)
    quit_startup
    FileUtils.mkdir_p(@startup_root)
    identity = File.join(@startup_root, 'corpora-identity.json')
    File.write(identity, '{"version":1,"corpora_id":"82a38dfa-8217-458f-9795-31f1c0e9069a"}')
    partial = File.binread(identity)
    launch_startup
    desktop.see('This is not a complete Sapiens Data Root')
    desktop.click('Continue')
    desktop.see('Start fresh requires an empty data directory')
    expect(File).not_to exist(File.join(@startup_root, 'corpora.sqlite3'))
    expect(File.binread(identity)).to eq(partial)
    expect(File.binread(selection)).to eq(original)
    # Attach a real, complete root through the native picker after recovery.
    @host = Host.new(start: false)
    attached_identity = File.binread(File.join(host.root, 'corpora-identity.json'))
    desktop.click('Use another folder')
    desktop.see(Gem.win_platform? ? 'Select Folder' : 'Choose data')
    desktop.key(Gem.win_platform? ? 'alt+d' : 'command+shift+g')
    Contract.wait do
      %w[AXTextField AXComboBox].include?(desktop.control('focused')['role'])
    rescue RuntimeError => error
      raise unless error.message.include?('"code":"focus_unavailable"')
      false
    end
    desktop.control('paste', '--text', host.root)
    desktop.key('return')
    desktop.click(Gem.win_platform? ? 'Select Folder' : 'Choose data')
    desktop.click('Continue')
    desktop.see('Message ChiefSapi')
    Contract.wait { JSON.parse(File.read(selection))['directory'] == host.root }
    expect(File.binread(File.join(host.root, 'corpora-identity.json'))).to eq(attached_identity)
    quit_startup
    host.start
    launch_startup
    desktop.see('Another Sapiens4 server')
    expect(host.state['corpora']).not_to be_empty
    quit_startup
    host.stop
    identity = File.join(host.root, 'corpora-identity.json')
    File.write(identity, 'corrupted identity')
    launch_startup
    desktop.see('Could not read data:')
    expect(File.binread(identity)).to eq('corrupted identity')
    quit_startup
    File.binwrite(identity, attached_identity)
    File.write(selection, 'corrupted selection')
    launch_startup
    desktop.see('Could not read saved storage:')
    expect(File.binread(selection)).to eq('corrupted selection')
    quit_startup
  end
end
