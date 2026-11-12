RSpec.describe 'Codex executable discovery' do
  before do
    skip 'Native installation discovery' unless Gem.win_platform? || RUBY_PLATFORM.include?('darwin')
    @host = Host.new(start: false)
    @fixture = host.fixture_dir
    @local, @home, @bin = %w[local home bin].map { |name| File.join(@fixture, name) }
    FileUtils.mkdir_p([@local, @home, @bin])
    @driver = File.join(@fixture, 'discovery-provider.rb')
    File.write(@driver, <<~'RUBY')
      require 'json'
      executable = ENV.fetch('SAPIENS_CONTRACT_EXECUTABLE')
      if ARGV.include?('--version')
        version = JSON.parse(File.read(executable + '.json'))
        puts version.fetch('output')
        exit version.fetch('status', 0)
      end
      File.write(File.join(ENV.fetch('SAPIENS_CONTRACT_ENV'), 'selected-codex'), executable)
      load ENV.fetch('SAPIENS_CONTRACT_ACTUAL_PROVIDER')
    RUBY
    host.environment.merge!('SAPIENS_CODEX_BINARY' => nil, 'LOCALAPPDATA' => @local,
      'APPDATA' => File.join(@fixture, 'roaming'), 'HOME' => @home, 'PATH' => @bin,
      'SAPIENS_CONTRACT_ACTUAL_PROVIDER' => host.environment.fetch('SAPIENS_CONTRACT_PROVIDER'),
      'SAPIENS_CONTRACT_PROVIDER' => @driver)
  end

  after { @host&.close }

  def candidate(location, output, status: 0)
    path = if Gem.win_platform?
      File.join(@local, 'OpenAI/Codex/bin', location, 'codex.exe')
    elsif location == 'path'
      File.join(@bin, 'codex')
    else
      File.join(@home, '.local/bin/codex')
    end
    FileUtils.mkdir_p(File.dirname(path))
    if Gem.win_platform?
      FileUtils.cp(ENV.fetch('SAPIENS_TEST_CODEX'), path)
    else
      File.write(path, "#!#{RbConfig.ruby}\nENV['SAPIENS_CONTRACT_EXECUTABLE'] = __FILE__\nload #{@driver.inspect}\n")
      File.chmod(0755, path)
    end
    Contract.atomic_json(path + '.json', output: output, status: status)
    path
  end

  def selected
    host.start
    turn = host.submit(host.state.fetch('main_agent_id'), 'Choose the working local fixture CLI.')
    expect(host.turn(turn).fetch('output')).to eq('Contract test reply')
    File.realpath(File.read(File.join(@fixture, 'selected-codex')))
  end

  it 'prefers the numerically newest working version among multiple installations' do
    candidate(Gem.win_platform? ? '0.99.0' : 'path', 'codex-cli 998.99.0')
    newest = candidate('0.156.1', 'codex-cli 998.156.1')
    expect(selected).to eq(File.realpath(newest))
  end

  {'failed version command' => ['codex-cli 9999.0.0', 1],
   'invalid version response' => ['unrecognized executable', 0]}.each do |name, (output, status)|
    it "skips a candidate with a #{name}" do
      candidate(Gem.win_platform? ? '9999.0.0' : 'path', output, status: status)
      working = candidate('0.156.1', 'codex-cli 998.156.1')
      expect(selected).to eq(File.realpath(working))
    end
  end

  it 'honors an explicit binary override even when a newer installation exists' do
    explicit = candidate(Gem.win_platform? ? '0.99.0' : 'path', 'codex-cli 998.99.0')
    candidate('0.156.1', 'codex-cli 998.156.1')
    host.environment['SAPIENS_CODEX_BINARY'] = explicit
    expect(selected).to eq(File.realpath(explicit))
  end
end
