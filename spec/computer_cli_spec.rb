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
    end
  end
end
