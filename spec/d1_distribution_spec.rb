require 'digest'
require_relative 'support/d1_package'

RSpec.describe 'D1 native distribution' do
  before(:context) do
    skip 'Set SAPIENS_TEST_D1=1 to build and install native packages' unless ENV['SAPIENS_TEST_D1'] == '1'
    skip 'D1 packages target macOS arm64 and Windows x64' unless RUBY_PLATFORM.include?('darwin') || Gem.win_platform?
    @suite = D1Package.new
    @suite.build
  end

  after(:context) { @suite&.close }

  it 'keeps channel profiles separate and applies data and config overrides independently' do
    apps = {}
    %w[development stable].each { |channel| apps[channel] = @suite.install(channel, File.join(@suite.root, 'profile-install')) }
    home = File.join(@suite.root, 'profile-user')
    profile = lambda do |user, channel|
      File.join(user, @suite.windows? ? 'Local' : 'Library/Application Support', @suite.name(channel))
    end
    sentinels = {}
    @suite.with_native(apps.fetch('development'), home) do |development|
      @suite.with_native(apps.fetch('stable'), home) do |stable|
        expect(stable.fetch(:url)).not_to eq(development.fetch(:url))
        %w[development stable].each do |channel|
          %w[data config].each do |kind|
            directory = File.join(profile.call(home, channel), kind)
            expect(File.directory?(directory)).to eq(true), "Missing #{directory}"
            path = File.join(directory, 'keep.txt')
            File.write(path, "#{channel}/#{kind}")
            sentinels[path] = File.binread(path)
          end
        end
      end
      expect(Contract.http(development.fetch(:url) + '/api/health', 'GET').first).to eq(200)
    end
    app = apps.fetch('development')
    %w[data config].each do |kind|
      user = File.join(@suite.root, "#{kind}-override-user")
      target = File.join(user, 'override')
      @suite.with_native(app, user, {"SAPIENS_#{kind.upcase}_DIR" => target}) do
        other = kind == 'data' ? 'config' : 'data'
        expect(File.directory?(target)).to eq(true)
        expect(File.directory?(File.join(profile.call(user, 'development'), other))).to eq(true)
        expect(File.exist?(File.join(profile.call(user, 'development'), kind))).to eq(false)
      end
    end
    # Windows exposes --data-dir on the native launcher; macOS exposes it on
    # the packaged backend CLI. Both must give it precedence over the environment.
    user = File.join(@suite.root, 'cli-user')
    ignored, selected = File.join(user, 'ignored'), File.join(user, 'selected')
    env = {'SAPIENS_DATA_DIR' => ignored, 'SAPIENS_CONFIG_DIR' => File.join(user, 'config')}
    if @suite.windows?
      @suite.with_native(app, user, env, args: ['--data-dir', selected]) { expect(File.directory?(selected)).to eq(true) }
    else
      backend = File.join(@suite.runtime(app), '.build/bin/sapiens4')
      process = Contract::Child.new(backend, '--desktop', '--port', '0', '--data-dir', selected,
        env: @suite.environment(user).merge(env), chdir: user)
      begin
        Contract.wait(description: 'packaged CLI readiness') { process.stdout.include?('Sapiens4: http://127.0.0.1:') }
        expect(File.directory?(selected)).to eq(true)
        process.input.write("stop\n")
        _, error, status = process.finish
        expect(status).to eq(0), error
      ensure
        process.close
      end
    end
    expect(File.exist?(ignored)).to eq(false)
    apps.each { |channel, installed| @suite.uninstall(channel, installed) }
    sentinels.each { |path, bytes| expect(File.binread(path)).to eq(bytes) }
  ensure
    (apps || {}).each { |channel, installed| @suite.uninstall(channel, installed) if File.exist?(@suite.binary(installed)) }
  end

  it 'installs independently, owns its backend and preserves external state through replacement and removal' do
    channel = 'development'
    folder = File.join(@suite.root, 'installation')
    home = File.join(@suite.root, 'installation-user')
    data, config = File.join(home, 'data'), File.join(home, 'config')
    sentinels = [data, config].map do |directory|
      FileUtils.mkdir_p(directory)
      path = File.join(directory, 'keep.txt')
      File.write(path, "Preserve #{directory}")
      [path, File.binread(path)]
    end.to_h
    app = @suite.install(channel, folder)
    if @suite.windows?
      expect(@suite.registration_state(channel)).to include('registered' => true, 'shortcut' => true)
      File.write(File.join(app, 'user-added.txt'), 'Unowned file')
    end
    runtime = @suite.runtime(app)
    manifest = JSON.parse(File.read(File.join(runtime, 'package-manifest.json')))
    provider = File.join(runtime, manifest.fetch('provider').fetch('entrypoint'))
    pinned = JSON.parse(File.read(File.join(Contract::ROOT, 'build/codex-provider-lock.json'))).fetch('version')
    expect(@suite.run(provider, '--version', env: @suite.environment(home), chdir: home).strip).to eq("codex-cli #{pinned}")
    state = {'SAPIENS_DATA_DIR' => data, 'SAPIENS_CONFIG_DIR' => config}
    @suite.with_native(app, home, state) do |native|
      if @suite.windows?
        before = Digest::SHA256.file(@suite.binary(app)).hexdigest
        @suite.installer(channel, app, success: false)
        @suite.installer(channel, app, remove: true, success: false)
        expect(@suite.alive?(native.fetch(:pid))).to eq(true)
        expect(Digest::SHA256.file(@suite.binary(app)).hexdigest).to eq(before)
      end
    end
    @suite.install(channel, folder)
    @suite.with_native(app, home, state) { }
    @suite.uninstall(channel, app)
    expect(File.exist?(@suite.binary(app))).to eq(false)
    if @suite.windows?
      expect(@suite.registered?(channel)).to eq(false)
      expect(File.read(File.join(app, 'user-added.txt'))).to eq('Unowned file')
    end
    sentinels.each { |path, bytes| expect(File.binread(path)).to eq(bytes) }
  ensure
    @suite.uninstall(channel, app) if app && File.exist?(@suite.binary(app))
  end

  it 'binds both channel artifacts to this source and rejects changed runtime bytes' do
    commit = @suite.run('git', 'rev-parse', 'HEAD').strip
    %w[development stable].each do |channel|
      runtime = @suite.runtime(@suite.built_app(channel))
      manifest = JSON.parse(File.read(File.join(runtime, 'package-manifest.json')))
      receipt = JSON.parse(File.read(Dir[File.join(@suite.output(channel), '*.release.json')].fetch(0)))
      expected = {'repository' => 'aleski-green/sapiens4', 'sourceCommit' => commit,
        'channel' => channel, 'packageStatus' => 'provider-complete'}
      expect(manifest).to include(expected)
      expect(receipt).to include(expected)
      expect(receipt.fetch('packageManifestSha256')).to eq(Digest::SHA256.file(File.join(runtime, 'package-manifest.json')).hexdigest)
      expect(manifest.fetch('appId')).to eq(channel == 'stable' ? 'com.poulwalcker.sapiens4' : 'com.poulwalcker.sapiens4.development')
      receipt.fetch('artifacts').each do |artifact|
        file = File.join(@suite.output(channel), artifact.fetch('assetName'))
        expect(File.size(file)).to eq(artifact.fetch('size'))
        expect(Digest::SHA256.file(file).hexdigest).to eq(artifact.fetch('sha256'))
      end
      @suite.run(@suite.packager, 'verify', '--runtime', runtime)
    end
    # Mutate only a disposable copy; keep signed installation artifacts intact.
    copy = File.join(@suite.root, 'damaged-runtime')
    FileUtils.cp_r(@suite.runtime(@suite.built_app('development')), copy)
    File.open(File.join(copy, 'submodules/libs4/web/voice-worklet.js'), 'a') { |file| file.write("\nchanged") }
    out, error, status = Contract.capture(@suite.packager, 'verify', '--runtime', copy)
    expect(status).not_to eq(0)
    expect(out + error).to include('Inventory differs from final package bytes')
  ensure
    FileUtils.rm_rf(copy) if copy
  end
end
