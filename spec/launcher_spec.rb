RSpec.describe 'Service launcher validation' do
  {
    'unknown option' => [['--unknown'], 'Unknown option or missing argument: --unknown'],
    'missing port' => [['--port'], 'Unknown option or missing argument: --port'],
    'noninteger port' => [['--port=invalid'], 'Invalid --port'],
    'negative port' => [['--port=-1'], 'Port must be between 0 and 65535'],
    'out-of-range port' => [['--port=65536'], 'Port must be between 0 and 65535']
  }.each do |name, (arguments, message)|
    it "rejects #{name} before creating a database" do
      Dir.mktmpdir('sapiens-launcher-') do |directory|
        _, error, code = Contract.capture(Contract::BINARY, '--data-dir', directory, *arguments)
        expect(code).to eq(1)
        expect(error).to include(message)
        expect(Dir.children(directory)).to be_empty
      end
    end
  end

  it 'reports a missing relay origin without provisioning a relay' do
    Dir.mktmpdir('sapiens-relay-launcher-') do |directory|
      _, error, code = Contract.capture(Contract::BINARY, 'relay', '--db', File.join(directory, 'relay.sqlite3'),
        env: {'SAPIENS_RELAY_ORIGIN' => nil, 'RAILWAY_PUBLIC_DOMAIN' => nil, 'PORT' => '4180'})
      expect(code).to eq(1)
      expect(error).to include('Relay --origin is required')
      expect(Dir.children(directory)).to be_empty
    end
  end

  it 'reports an invalid relay port from the environment as an option error' do
    _, error, code = Contract.capture(Contract::BINARY, 'relay', env: {'PORT' => 'invalid'})
    expect(code).to eq(1)
    expect(error).to include('Invalid --port')
  end

  [[], ['relay']].each do |command|
    it "prints #{command.empty? ? 'host' : 'relay'} help without starting a service" do
      output, error, code = Contract.capture(Contract::BINARY, *command, '--help')
      expect(code).to eq(0), error
      expect(output).to start_with('Usage: sapiens4')
    end
  end
end

RSpec.describe 'Data Root startup validation' do
  it 'initializes once, reopens complete registered data and rejects locked, corrupted and partial roots without rewriting them' do
    require 'digest'
    saved = Host.new
    begin
      saved.submit(saved.state.fetch('main_agent_id'), 'Keep this conversation')
      created = saved.api('POST', '/api/host/local', {name: 'Saved Studio'}, status: 201)
      expect(saved.request('GET', '/api/host/onboarding').first).to eq(404)
      expect(saved.request('POST', '/api/host/onboarding', {action: 'finish'}).first).to eq(404)
      check = ->(root, mode = 'existing') { Contract.capture(Contract::BINARY, 'check-data', root, mode) }
      expect(check.call(saved.root)[1..]).to match([/Another Sapiens4 server/, 1])
      saved.stop
      # SQLite may create transient WAL/SHM metadata in read-only mode.
      # Compare durable data bytes; existing database content must stay intact.
      fingerprint = ->(root) { Dir[File.join(root, '**/*')].select { |f| File.file?(f) && !f.match?(/corpora\.sqlite3-(wal|shm)\z/) }.to_h { |f| [f.delete_prefix(root), Digest::SHA256.file(f).hexdigest] } }
      before = fingerprint.call(saved.root)
      expect(check.call(saved.root).last).to eq(0)
      expect(fingerprint.call(saved.root)).to eq(before)
      File.write(File.join(saved.environment.fetch('SAPIENS_CONFIG_DIR'), 'onboarding.json'), 'legacy invalid setup')
      saved.start
      expect(saved.state.fetch('turns').map { |t| t['input'] }).to include('Keep this conversation')
      expect(saved.api('GET', '/api/host')['corpora'].map { |r| r['id'] }).to include(created.fetch('id'))
      saved.stop
      Dir.mktmpdir('sapiens-invalid-storage-') do |directory|
        missing = File.join(directory, 'missing')
        expect(check.call(missing).last).to eq(1)
        expect(File).not_to exist(missing)
        invalid = File.join(directory, 'invalid')
        FileUtils.mkdir_p(invalid)
        identity = File.join(invalid, 'corpora-identity.json')
        File.write(identity, File.read(File.join(saved.root, 'corpora-identity.json')))
        partial = fingerprint.call(invalid)
        expect(check.call(invalid).last).to eq(1)
        expect(check.call(invalid, 'new').last).to eq(1)
        expect(fingerprint.call(invalid).reject { |key, _| key == '/host.lock' }).to eq(partial)
        FileUtils.cp(File.join(saved.root, 'corpora.sqlite3'), File.join(invalid, 'corpora.sqlite3'))
        File.write(identity, 'corrupt identity')
        corrupt = fingerprint.call(invalid)
        expect(check.call(invalid).last).to eq(1)
        expect(fingerprint.call(invalid)).to eq(corrupt)
        FileUtils.cp(File.join(saved.root, 'corpora-identity.json'), identity)
        File.write(File.join(invalid, 'corpora.sqlite3'), 'corrupt database')
        corrupt = fingerprint.call(invalid)
        expect(check.call(invalid).last).to eq(1)
        expect(fingerprint.call(invalid)).to eq(corrupt)
      end
    ensure
      saved.close
    end
  end
end
