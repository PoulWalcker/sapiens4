require 'digest'
require 'socket'
require_relative 'support/d1_package'

RSpec.describe 'D1 native distribution' do
  before(:context) do
    skip 'Set SAPIENS_TEST_D1=1 and SAPIENS_TEST_D1_INSTALLER to a finished installer' unless ENV['SAPIENS_TEST_D1'] == '1'
    skip 'D1 targets macOS ARM64 and Windows x64' unless RUBY_PLATFORM.include?('darwin') || Gem.win_platform?
    @suite = D1Package.new
    @suite.verify_artifact
  end
  after(:context) { @suite&.close }

  it 'installs and renders without Codex, owns its backend, and cleans up on quit and startup failure' do
    app = @suite.install(File.join(@suite.root, 'lifecycle-install'))
    home = File.join(@suite.root, 'lifecycle-user')
    listener = TCPServer.new('127.0.0.1', 0)
    @suite.with_native(app, home) do |native|
      expect(native.fetch(:desktop).text).to include('Sapiens')
      status, state = Contract.http(native.fetch(:url) + '/api/state', 'GET')
      expect(status).to eq(200)
      status, body = Contract.http(native.fetch(:url) + "/api/agents/#{state.fetch('main_agent_id')}/messages", 'POST', {text: 'Provider absent'})
      expect(status).to eq(503)
      expect(body.to_s).to include('Codex')
    end
    # Controlled external CLI; never use real credentials or paid model calls.
    fixture = Host.new(start: false)
    overrides = fixture.environment.merge('SAPIENS_DATA_DIR' => fixture.root, 'SAPIENS_CONFIG_DIR' => File.join(home, 'config'))
    @suite.with_native(app, home, overrides) do |native|
      url = native.fetch(:url)
      _, state = Contract.http(url + '/api/state', 'GET')
      status, turn = Contract.http(url + "/api/agents/#{state.fetch('main_agent_id')}/messages", 'POST', {text: 'External CLI execution'})
      expect(status).to eq(202)
      Contract.wait(timeout: 40, description: 'external provider reply') do
        _, current = Contract.http(url + '/api/state', 'GET')
        row = current.fetch('turns').find { |item| item['id'] == turn.fetch('id') }
        row && row['output'] == 'Contract test reply'
      end
    end
    selected = File.join(home, 'selected')
    @suite.with_backend(app, home, {'SAPIENS_DATA_DIR' => File.join(home, 'ignored')}, args: ['--data-dir', selected]) do |child|
      expect(File.file?(File.join(selected, 'corpora.sqlite3'))).to eq(true)
      expect(File.exist?(File.join(home, 'ignored'))).to eq(false)
    end
    @suite.with_native(app, home, {}, abrupt: true) { }
    # Remove one required asset only in this disposable installed copy. The
    # owned backend must exit before readiness and native recovery must close.
    asset = File.join(@suite.runtime(app), '.build/web/app.js')
    File.rename(asset, asset + '.disabled')
    begin
      @suite.startup_failure(app, home)
    ensure
      File.rename(asset + '.disabled', asset)
    end

  ensure
    if listener
      socket = TCPSocket.new('127.0.0.1', listener.addr[1]); socket.close
      listener.close
    end
    fixture&.close
    @suite.uninstall(app) if app && File.exist?(@suite.binary(app))
  end

  it 'preserves real state and isolated overrides through locking, replacement, uninstall and reinstall' do
    folder = File.join(@suite.root, 'state-install')
    app = @suite.install(folder)
    home = File.join(@suite.root, 'state-user')
    profile = File.join(home, @suite.windows? ? 'Local' : 'Library/Application Support', 'Sapiens4')
    sentinels = {}
    @suite.with_native(app, home) do |native|
      %w[data config].each do |kind|
        path = File.join(profile, kind, 'keep.txt')
        File.write(path, kind)
        sentinels[path] = kind
      end
      status, agent = Contract.http(native.fetch(:url) + '/api/agents', 'POST', {name: 'Keeper', role: 'Research'})
      expect(status).to eq(201)
      @saved_agent = agent.fetch('id')
      backend = File.join(@suite.runtime(app), '.build/bin', @suite.windows? ? 'sapiens4.exe' : 'sapiens4')
      _, error, code = Contract.capture(backend, '--desktop', '--port', '0', '--data-dir', File.join(profile, 'data'), env: @suite.environment(home), chdir: home)
      expect(code).not_to eq(0)
      expect(error).to match(/lock|already|using/i)
      if @suite.windows?
        before = Digest::SHA256.file(@suite.binary(app)).hexdigest
        @suite.installer(app, success: false)
        @suite.installer(app, remove: true, success: false)
        expect(@suite.alive?(native.fetch(:backend))).to eq(true)
        expect(Digest::SHA256.file(@suite.binary(app)).hexdigest).to eq(before)
      end
    end
    isolated = {'SAPIENS_DATA_DIR' => File.join(home, 'developer-data'), 'SAPIENS_CONFIG_DIR' => File.join(home, 'developer-config')}
    @suite.with_native(app, home, isolated) do |native|
      isolated.each_value { |path| expect(File.directory?(path)).to eq(true) }
      _, state = Contract.http(native.fetch(:url) + '/api/state', 'GET')
      expect(state.fetch('agents').map { |a| a['id'] }).not_to include(@saved_agent)
    end
    2.times do |index|
      @suite.uninstall(app) if index == 1
      app = @suite.install(folder)
      @suite.with_native(app, home) do |native|
        _, state = Contract.http(native.fetch(:url) + '/api/state', 'GET')
        expect(state.fetch('agents').map { |a| a['id'] }).to include(@saved_agent)
      end
      sentinels.each { |path, bytes| expect(File.binread(path)).to eq(bytes) }
    end
    if @suite.windows?
      unowned = File.join(@suite.root, 'unowned')
      FileUtils.mkdir_p(unowned)
      File.write(File.join(unowned, 'keep.txt'), 'Unowned')
      @suite.installer(unowned, success: false, reason: 'Existing data or unowned files')
      expect(File.read(File.join(unowned, 'keep.txt'))).to eq('Unowned')
      protected = File.join(ENV.fetch('LOCALAPPDATA'), 'Sapiens4', 'data')
      @suite.installer(protected, success: false, reason: 'Existing data or unowned files')
      File.write(File.join(app, 'user-added.txt'), 'Unowned')
    end
    @suite.uninstall(app)
    expect(File.exist?(@suite.binary(app))).to eq(false)
    if @suite.windows?
      expect(@suite.registered?).to eq(false)
      expect(File.read(File.join(app, 'user-added.txt'))).to eq('Unowned')
    end
    sentinels.each { |path, bytes| expect(File.binread(path)).to eq(bytes) }
  ensure
    @suite.uninstall(app) if app && File.exist?(@suite.binary(app))
  end
end
