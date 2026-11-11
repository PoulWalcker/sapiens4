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
