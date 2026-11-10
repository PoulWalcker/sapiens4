RSpec.describe 'Haskell command-line contracts', :host do
  {
    'computer' => ['computer-use.md', /\ABlindly4 UI wrapper: (.+)$/, " launch '   '", 'Usage: sapiens4 computer launch'],
    'host control' => ['host-control.md', /\ARun (.+) 'JSON';/, " '{}'", 'Unknown operation or field']
  }.each do |name, (file, pattern, arguments, message)|
    it "runs the published Windows #{name} launcher through PowerShell" do
      skip 'Windows PowerShell command syntax' unless Gem.win_platform?
      owner = host.state['main_agent_id']
      manifest = File.read(File.join(host.root, 'agentpy', 'agents', owner, 'manifests', file))
      launcher = manifest.match(pattern).captures.first
      command = "#{launcher}#{arguments}; exit $LASTEXITCODE"
      encoded = [command.encode('UTF-16LE')].pack('m0')
      out, error, code = Contract.capture('powershell.exe', '-NoProfile', '-NonInteractive',
        '-EncodedCommand', encoded, chdir: host.workspace(owner), env: host.environment)
      expect(code).to eq(1), error
      expect(JSON.parse(out)['error']).to include(message)
      expect(File.exist?(File.join(host.workspace(owner), '.computer-used'))).to eq(false)
      expect(host.state['computer']['owner']).to be_nil
    end
  end

  it 'preserves Unicode control payloads and machine-readable failures' do
    owner = host.state['main_agent_id']
    config = File.join(host.workspace(owner), 'host-control.json')
    message = 'Résumé 中文 😎 — quoted "text"'
    out, error, code = Contract.capture(Contract::BINARY, 'control', config,
      JSON.generate(op: 'routine_create', minutes: 2, prompt: message), env: host.environment)
    expect(code).to eq(0), error
    expect(JSON.parse(out)['routine']['prompt']).to eq(message)
    out, _, code = Contract.capture(Contract::BINARY, 'control', config, '{', env: host.environment)
    expect(code).to eq(1)
    expect(JSON.parse(out)).to include('error' => a_kind_of(String))
    expect(host.calls).to be_empty
  end

  it 'rejects invalid launch arguments before acquiring or marking the computer' do
    workspace = host.workspace(host.state['main_agent_id'])
    out, _, code = Contract.capture(Contract::BINARY, 'computer', 'launch', '   ', chdir: workspace, env: host.environment)
    expect(code).to eq(1)
    expect(JSON.parse(out)['error']).to include('Usage:')
    expect(File.exist?(File.join(workspace, '.computer-used'))).to eq(false)
    expect(host.state['computer']['owner']).to be_nil
  end

  it 'launches a Linux executable with isolated streams without waiting for it to exit' do
    skip 'Linux launch contract' unless RUBY_PLATFORM.include?('linux')
    Dir.mktmpdir do |directory|
      application = File.join(directory, 'app;literal')
      marker = File.join(directory, 'started')
      release, finished = File.join(directory, 'release'), File.join(directory, 'finished')
      File.write(application, <<~RUBY)
        #!#{RbConfig.ruby}
        STDOUT.puts 'application output'
        STDERR.puts 'application diagnostics'
        File.write(#{marker.inspect}, STDIN.read.empty? ? 'launched' : 'unexpected input')
        deadline = Process.clock_gettime(Process::CLOCK_MONOTONIC) + 5
        sleep 0.01 until File.exist?(#{release.inspect}) || Process.clock_gettime(Process::CLOCK_MONOTONIC) >= deadline
        File.write(#{finished.inspect}, '')
      RUBY
      File.chmod(0755, application)
      output, error, status = Contract.capture(Contract::BINARY, 'computer', 'launch', application, chdir: directory)
      expect(status).to eq(0), output + error
      expect(JSON.parse(output)).to include('launched' => true, 'pid' => a_kind_of(Integer))
      expect(error).to eq('')
      Contract.wait(description: 'launched application') { File.exist?(marker) }
      expect(File.read(marker)).to eq('launched')
      expect(File).not_to exist(finished)
    ensure
      File.write(release, '') if release
      Contract.wait(description: 'application exit') { File.exist?(finished) } if finished
    end
  end

  it 'validates read options and preserves unrelated cache files while paginating exact paths' do
    skip 'Executable fixture uses a Unix shebang' if Gem.win_platform?
    Dir.mktmpdir('sapiens-reader-') do |folder|
      binary = File.join(folder, 'blindly-fixture')
      File.write(binary, "#!#{RbConfig.ruby}\nputs #{JSON.generate(tree: {pid: 123, role: 'AXWindow', title: 'Résumé 😎', children: [{role: 'AXButton', title: 'Open'}]}).inspect}\n")
      File.chmod(0755, binary)
      cache = File.join(folder, '.computer-reads')
      FileUtils.mkdir_p(cache)
      sentinel = File.join(cache, 'keep.txt')
      File.write(sentinel, 'Unrelated content')
      6.times { |i| File.write(File.join(cache, "#{i}.json"), '{}') }
      env = host.environment.merge('SAPIENS_BLINDLY_BINARY' => binary)
      out, error, code = Contract.capture(Contract::BINARY, 'computer', 'read', '--pid=123', chdir: folder, env: env)
      expect(code).to eq(0), error
      snapshot = JSON.parse(out)
      expect(snapshot['nodes'].map { |row| row['path'] }).to eq(['', '0'])
      expect(snapshot['nodes'].first['title']).to eq('Résumé 😎')
      expect(File.read(sentinel)).to eq('Unrelated content')
      expect(Dir[File.join(cache, '*.json')].size).to eq(5)
      out, _, code = Contract.capture(Contract::BINARY, 'computer', 'read', '--pid', '123', '--unknown', 'x', chdir: folder, env: env)
      expect(code).to eq(1)
      expect(JSON.parse(out)['error']).to include('Unknown read option')
      File.write(binary, "#!#{RbConfig.ruby}\nputs '{}'\n")
      out, _, code = Contract.capture(Contract::BINARY, 'computer', 'read', '--pid', '123', chdir: folder, env: env)
      expect(code).to eq(1)
      expect(JSON.parse(out)['error']).to include('invalid accessibility tree')
      File.write(binary, "#!#{RbConfig.ruby}\nputs #{JSON.generate(code: 'accessibility_error', error: 'Application returned a null accessibility element').inspect}\nexit 77\n")
      out, _, code = Contract.capture(Contract::BINARY, 'computer', 'read', '--pid', '123', chdir: folder, env: env)
      expect(code).to eq(1)
      expect(JSON.parse(out)['error']).to include('Blindly read failed: Application returned a null accessibility element')
    end
  end
end
