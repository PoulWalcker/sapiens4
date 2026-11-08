require_relative 'spec_helper'

RSpec.describe 'Local Codex setup' do
  it 'isolates login and execution, cancels owned sign-in, reports failure and preserves readiness across restart' do
    skip 'The fixture executable uses a POSIX script; native Windows acceptance uses the packaged CLI' if Gem.win_platform?
    host = Host.new(start: false)
    config = File.join(host.root, 'app config')
    external = File.join(host.root, 'external-codex')
    FileUtils.mkdir_p(external)
    File.write(File.join(external, 'auth.json'), 'external synthetic authentication')
    host.environment.merge!('SAPIENS_CONFIG_DIR' => config, 'CODEX_HOME' => external,
      'OPENAI_API_KEY' => 'synthetic-key-must-not-be-used', 'CODEX_API_KEY' => 'synthetic-codex-key-must-not-be-used')
    File.write(host.environment.fetch('SAPIENS_CODEX_BINARY'), <<~RUBY)
      #!#{RbConfig.ruby}
      require 'json'
      root = ENV.fetch('SAPIENS_CONTRACT_ENV')
      home = ENV.fetch('CODEX_HOME')
      File.open(File.join(root, 'auth-invocations.jsonl'), 'a') { |f| f.puts JSON.generate(home: home, args: ARGV, inherited_keys: ENV.keys.grep(/\\A(?:OPENAI_|CODEX_)/) - ['CODEX_HOME']) }
      if ARGV.include?('login')
        warn 'synthetic-private-diagnostic'
        if ARGV.include?('status')
          exit(File.exist?(File.join(home, 'auth.json')) ? 0 : 1)
        end
        exit 1 if File.exist?(File.join(root, 'fail-login'))
        File.write(File.join(root, 'login.pid'), Process.pid.to_s)
        $stdout.sync = true
        puts '2. Enter this one-time code (expires in 15 minutes)'
        puts "\\e[94mABCD-EFGH\\e[0m"
        sleep 0.02 until File.exist?(File.join(root, 'finish-login'))
        File.write(File.join(home, 'auth.json'), 'synthetic app authentication')
        exit
      end
      load #{File.join(__dir__, 'support/provider.rb').inspect}
    RUBY
    host.start
    expect(host.api('GET', '/api/host/onboarding').fetch('provider')).to eq('sign_in_required')
    expect(host.request('GET', '/api/host/onboarding', nil, 'Origin' => 'https://untrusted.example').first).to eq(403)
    expect(host.request('POST', '/api/host/onboarding', {action: 'login'}, 'Origin' => 'https://untrusted.example').first).to eq(403)
    expect(host.request('POST', '/api/host/onboarding', {action: 'login'}, 'X-Sapiens-Local' => '').first).to eq(403)
    host.api('POST', '/api/host/onboarding', action: 'login')
    view = Contract.wait { v = host.api('GET', '/api/host/onboarding'); v if v.dig('login', 'code') == 'ABCD-EFGH' }
    expect(JSON.generate(view)).not_to include('synthetic-private-diagnostic')
    host.api('POST', '/api/host/onboarding', {action: 'login'}, status: 409)
    expect(host.api('POST', '/api/host/onboarding', action: 'cancel').dig('login', 'state')).to eq('cancelled')
    expect { Process.kill(0, File.read(File.join(host.root, 'login.pid')).to_i) }.to raise_error(Errno::ESRCH)
    expect(File.exist?(File.join(config, 'codex/auth.json'))).to eq(false)
    host.api('POST', '/api/host/onboarding', action: 'login')
    Contract.wait { host.api('GET', '/api/host/onboarding').dig('login', 'code') == 'ABCD-EFGH' }
    login_pid = File.read(File.join(host.root, 'login.pid')).to_i
    host.stop
    expect { Process.kill(0, login_pid) }.to raise_error(Errno::ESRCH)
    expect(File.exist?(File.join(config, 'codex/auth.json'))).to eq(false)
    host.start
    File.write(File.join(host.root, 'fail-login'), '')
    host.api('POST', '/api/host/onboarding', action: 'login')
    Contract.wait { host.api('GET', '/api/host/onboarding').dig('login', 'state') == 'failed' }
    File.delete(File.join(host.root, 'fail-login'))
    host.api('POST', '/api/host/onboarding', action: 'login')
    File.write(File.join(host.root, 'finish-login'), '')
    Contract.wait { host.api('GET', '/api/host/onboarding').fetch('ready') }
    host.restart
    expect(host.api('GET', '/api/host/onboarding').fetch('ready')).to eq(true)
    id = host.submit(host.state.fetch('main_agent_id'), 'Hello')
    host.turn(id)
    invocations = File.readlines(File.join(host.root, 'auth-invocations.jsonl')).map { |line| JSON.parse(line) }
    expect(invocations.any? { |v| v['args'].include?('exec') }).to eq(true)
    expect(invocations.map { |v| v['home'] }.uniq).to eq([File.join(config, 'codex')])
    expect(invocations.all? { |v| v['args'].include?('cli_auth_credentials_store="file"') && v['inherited_keys'].empty? }).to eq(true)
    expect(host.process.stderr).not_to include('synthetic-private-diagnostic')
    expect(File.read(File.join(external, 'auth.json'))).to eq('external synthetic authentication')
    expect(File.stat(File.join(config, 'codex')).mode & 0777).to eq(0700)
    # Managed configuration must fail closed instead of selecting Codex on PATH.
    host.environment['SAPIENS_CODEX_BINARY'] = ''
    host.environment['PATH'] = host.root + File::PATH_SEPARATOR + ENV.fetch('PATH')
    count = invocations.length
    host.restart
    expect(host.api('GET', '/api/host/onboarding').fetch('provider')).to eq('missing')
    host.api('POST', '/api/host/onboarding', {action: 'login'}, status: 503)
    expect(File.readlines(File.join(host.root, 'auth-invocations.jsonl')).length).to eq(count)
  ensure
    host&.close
  end
end
