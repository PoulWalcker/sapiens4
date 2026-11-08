RSpec.describe 'Source distribution contract' do
  before(:context) do
    @folder = File.realpath(Dir.mktmpdir('sapiens source release '))
    @release = File.join(@folder, 'source')
    files, error, code = Contract.capture('git', 'ls-files', '--recurse-submodules',
      'build', 'corpora', 'remote', 'environments', 'app',
      'license', 'README.md', 'submodules/blindly4', 'submodules/libs4')
    expect(code).to eq(0), error
    files.lines.map(&:strip).each do |file|
      next if file.end_with?('.swift', '.cs') || (file.end_with?('.py') && !file.start_with?('build/package/'))
      target = File.join(@release, file)
      FileUtils.mkdir_p(File.dirname(target))
      FileUtils.cp(File.join(Contract::ROOT, file), target)
    end
    _, error, code = Contract.capture(Gem.win_platform? ? 'python' : 'python3', 'build/package/package.py', 'provenance',
      '--output', File.join(@release, 'build/source-provenance.json'))
    expect(code).to eq(0), error
    if ENV['SAPIENS_TEST_CABAL_CACHE']
      cache = File.realpath(ENV['SAPIENS_TEST_CABAL_CACHE'])
      destination = File.join(@release, 'submodules/libs4/haskell/.cabal')
      if Gem.win_platform?
        FileUtils.cp_r(cache, destination)
      else
        FileUtils.ln_s(cache, destination)
      end
    end
    @data = File.join(@folder, "existing corpora's é")
    FileUtils.mkdir_p(@data)
    File.write(File.join(@data, 'keep.txt'), 'Existing corpora data')
    @app = File.join(@release, '.build/macos/Sapiens4 Development.app')
    command = RUBY_PLATFORM.include?('darwin') ? ['sh', 'environments/macos/build.sh', '--allow-missing-provider'] : ['runghc', 'build/Build.hs']
    out, error, code = Contract.capture(*command, chdir: @release, timeout: 1200,
      env: {'SAPIENS_DATA_DIR' => @data})
    expect(code).to eq(0), out + error
  end

  after(:context) do
    if @folder && File.directory?(@folder)
      if Gem.win_platform?
        # Ruby's directory walker cannot traverse Cabal's long Windows paths.
        _, error, code = Contract.capture('powershell.exe', '-NoProfile', '-NonInteractive', '-Command',
          'Remove-Item -LiteralPath $env:SAPIENS_SOURCE_RELEASE -Recurse -Force -ErrorAction Stop',
          env: {'SAPIENS_SOURCE_RELEASE' => '\\\\?\\' + @folder.tr('/', '\\')}, timeout: 120)
        raise error unless code == 0
      else
        FileUtils.remove_entry(@folder)
      end
    end
  end

  it 'builds a signed macOS launcher for existing data without changing that data', :macos do
    skip 'macOS desktop build' unless RUBY_PLATFORM.include?('darwin')
    manifest = JSON.parse(File.read(File.join(@app, 'Contents/Resources/runtime/package-manifest.json')))
    expect(manifest).to include('appId' => 'com.poulwalcker.sapiens4.development',
      'channel' => 'development', 'updateDiscovery' => false)
    expect(File.exist?(File.join(@app, 'Contents/Resources/Launcher.plist'))).to eq(false)
    expect(File.executable?(File.join(@app, 'Contents/Resources/runtime/.build/bin/sapiens4'))).to eq(true)
    expect(Dir.children(@data)).to eq(['keep.txt'])
    expect(File.read(File.join(@data, 'keep.txt'))).to eq('Existing corpora data')
    _, error, code = Contract.capture('codesign', '--verify', '--strict', @app)
    expect(code).to eq(0), error
    before = File.binread(File.join(@app, 'Contents/Resources/runtime/package-manifest.json'))
    _, error, code = Contract.capture('sh', 'environments/macos/build.sh', '--skip-build', '--allow-missing-provider',
      chdir: @release, timeout: 120)
    expect(code).not_to eq(0)
    expect(error).to include('Output app already exists')
    expect(File.binread(File.join(@app, 'Contents/Resources/runtime/package-manifest.json'))).to eq(before)
  end

  it 'boots from a source-only release and completes work using its packaged assets' do
    isolated = Host.new(repo: @release)
    begin
      state = isolated.state
      id = isolated.submit(state['main_agent_id'], 'Hello from an installed release')
      expect(isolated.turn(id)['output']).to eq('Contract test reply')
      example = isolated.calls.first['prompt'][/read the worked example at (.*?)\. It demonstrates/m, 1]
      expect(example).not_to be_nil
      expect(File.identical?(example, File.join(@release, 'corpora/prompts/examples/jarvis-notes.html'))).to eq(true)
      %w[/workspace/ /workspace/app.js /workspace/guest.js /sw.js /remote-vendor.js /workspace/styles.css].each do |path|
        status, body = isolated.request('GET', path)
        expect(status).to eq(200)
        expect(body).not_to be_empty
      end
      expect(isolated.request('GET', '/submodules/libs4/package.json').first).to eq(404)
      expect(isolated.request('GET', '/submodules/libs4/.git').first).to eq(404)
      expect(File.exist?(File.join(@release, '.git'))).to eq(false)
    ensure
      isolated.close
    end
  end
end
