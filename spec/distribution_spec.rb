RSpec.describe 'Source distribution contract' do
  before(:context) do
    @folder = File.realpath(Dir.mktmpdir('sapiens source release '))
    @release = File.join(@folder, 'source')
    files, error, code = Contract.capture('git', 'ls-files', '-z', '--recurse-submodules',
      'build', 'corpora', 'remote', 'environments', 'app',
      'license', 'README.md', 'submodules/blindly4', 'submodules/libs4')
    expect(code).to eq(0), error
    additions, error, code = Contract.capture('git', 'ls-files', '-z', '--others', '--exclude-standard',
      'build', 'corpora', 'remote', 'environments', 'app', 'license')
    expect(code).to eq(0), error
    (files.split("\0") + additions.split("\0")).uniq.each do |file|
      next if file.end_with?('.py', '.swift', '.cs')
      target = File.join(@release, file)
      FileUtils.mkdir_p(File.dirname(target))
      FileUtils.cp(File.join(Contract::ROOT, file), target)
    end
    {'SAPIENS_TEST_CABAL_CACHE' => 'submodules/libs4/haskell/.cabal',
     'SAPIENS_TEST_UI_CACHE' => 'submodules/libs4/.cache'}.each do |name, relative|
      next unless ENV[name]
      cache = File.realpath(ENV[name])
      destination = File.join(@release, relative)
      if Gem.win_platform?
        FileUtils.cp_r(cache, destination)
      else
        FileUtils.ln_s(cache, destination)
      end
    end
    @data = File.join(@folder, "existing corpora's é")
    FileUtils.mkdir_p(@data)
    File.write(File.join(@data, 'keep.txt'), 'Existing corpora data')
    @product = File.join(@folder, 'macos release')
    if RUBY_PLATFORM.include?('darwin')
      bin = File.join(@release, '.build/bin')
      FileUtils.mkdir_p(bin)
      File.write(File.join(bin, 'obsolete-helper.py'), 'Old build artifact')
      command = ['sh', 'environments/macos/build.sh', '0.1.0', @product]
    else
      command = ['runghc', 'build/Build.hs']
    end
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

  it 'packages a signed standalone app from source without changing existing user data', :macos do
    skip 'macOS desktop build' unless RUBY_PLATFORM.include?('darwin')
    expect(Dir.children(@product)).to contain_exactly('Sapiens.dmg', 'SHA256SUMS')
    out, error, code = Contract.capture('shasum', '-a', '256', '-c', 'SHA256SUMS', chdir: @product)
    expect(code).to eq(0), out + error
    expect(out).to include('Sapiens.dmg: OK')
    mount = File.join(@folder, 'mounted')
    Dir.mkdir(mount)
    _, error, code = Contract.capture('hdiutil', 'attach', '-readonly', '-nobrowse', '-mountpoint', mount, File.join(@product, 'Sapiens.dmg'))
    expect(code).to eq(0), error
    begin
      app = File.join(mount, 'Sapiens4.app')
      bin = File.join(app, 'Contents/Resources/runtime/.build/bin')
      expect(File.executable?(File.join(bin, 'sapiens4'))).to be(true)
      expect(File.exist?(File.join(bin, 'obsolete-helper.py'))).to be(false)
      expect(File.binread(File.join(app, 'Contents/Resources/runtime/app/shell/AI-LOGOS-LICENSE.txt')))
        .to eq(File.binread(File.join(Contract::ROOT, 'app/shell/AI-LOGOS-LICENSE.txt')))
      _, error, code = Contract.capture('codesign', '--verify', '--deep', '--strict', app)
      expect(code).to eq(0), error
    ensure
      _, error, code = Contract.capture('hdiutil', 'detach', mount)
      raise error unless code == 0
    end
    expect(Dir.children(@data)).to eq(['keep.txt'])
    expect(File.read(File.join(@data, 'keep.txt'))).to eq('Existing corpora data')
  end

  it 'boots from a source-only release and completes work using its packaged assets' do
    command = ['sh', File.join(@release, 'environments/linux/start.sh')] if RUBY_PLATFORM.include?('linux')
    isolated = Host.new(repo: @release, command: command)
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
