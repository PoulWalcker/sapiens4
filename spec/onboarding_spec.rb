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
      require 'rbconfig'
      root = ENV.fetch('SAPIENS_CONTRACT_ENV')
      home = ENV.fetch('CODEX_HOME')
      File.open(File.join(root, 'auth-invocations.jsonl'), 'a') { |f| f.puts JSON.generate(home: home, args: ARGV, inherited_keys: ENV.keys.grep(/\\A(?:OPENAI_|CODEX_)/) - ['CODEX_HOME']) }
      if ARGV.include?('login')
        warn 'synthetic-private-diagnostic'
        if ARGV.include?('status')
          exit 1 if File.exist?(File.join(root, 'fail-status'))
          if File.exist?(File.join(root, 'slow-status'))
            trap('TERM') {}
            child = spawn(RbConfig.ruby, '-e', 'sleep 60')
            File.write(File.join(root, 'status-pids'), [Process.pid, child].join(' '))
            sleep 60
          end
          if File.exist?(File.join(root, 'orphan-status'))
            child = spawn(RbConfig.ruby, '-e', 'sleep 60', out: File::NULL, err: File::NULL)
            File.write(File.join(root, 'status-pids'), [Process.pid, child].join(' '))
          end
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
    File.write(File.join(host.root, 'slow-status'), '')
    uri = URI(host.url + '/api/host/onboarding')
    http = Net::HTTP.new(uri.host, uri.port, nil)
    http.read_timeout = 20
    expect(JSON.parse(http.get(uri.request_uri).body).fetch('provider')).to eq('unavailable')
    status_pids = File.read(File.join(host.root, 'status-pids')).split.map(&:to_i)
    Contract.wait do
      status_pids.none? { |pid| begin; Process.kill(0, pid); true; rescue Errno::ESRCH; false; end }
    end
    File.delete(File.join(host.root, 'status-pids'))
    File.delete(File.join(host.root, 'slow-status'))
    File.write(File.join(host.root, 'orphan-status'), '')
    expect(host.api('GET', '/api/host/onboarding').fetch('provider')).to eq('sign_in_required')
    status_pids = File.read(File.join(host.root, 'status-pids')).split.map(&:to_i)
    Contract.wait do
      status_pids.none? { |pid| begin; Process.kill(0, pid); true; rescue Errno::ESRCH; false; end }
    end
    File.delete(File.join(host.root, 'status-pids'))
    File.delete(File.join(host.root, 'orphan-status'))
    File.write(File.join(host.root, 'slow-status'), '')
    status_request = Thread.new { http.get(uri.request_uri) rescue nil }
    Contract.wait { File.exist?(File.join(host.root, 'status-pids')) }
    status_pids = File.read(File.join(host.root, 'status-pids')).split.map(&:to_i)
    host.stop
    expect(status_request.join(20)).not_to be_nil
    Contract.wait do
      status_pids.none? { |pid| begin; Process.kill(0, pid); true; rescue Errno::ESRCH; false; end }
    end
    File.delete(File.join(host.root, 'status-pids'))
    File.delete(File.join(host.root, 'slow-status'))
    host.start
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
    auth_file = File.join(config, 'codex/auth.json')
    saved_auth = File.binread(auth_file)
    File.write(File.join(host.root, 'fail-status'), '')
    expect(host.api('GET', '/api/host/onboarding').fetch('provider')).to eq('unavailable')
    host.api('POST', '/api/host/onboarding', {action: 'login'}, status: 503)
    host.api('POST', '/api/host/onboarding', action: 'refresh')
    expect(File.binread(auth_file)).to eq(saved_auth)
    File.delete(File.join(host.root, 'fail-status'))
    host.api('POST', '/api/host/onboarding', {action: 'login'}, status: 409)
    host.restart
    expect(host.api('GET', '/api/host/onboarding').fetch('ready')).to eq(true)
    expect(File.binread(auth_file)).to eq(saved_auth)
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
    if host && File.exist?(File.join(host.root, 'status-pids'))
      File.read(File.join(host.root, 'status-pids')).split.map(&:to_i).each { |pid| Process.kill('KILL', pid) rescue Errno::ESRCH }
    end
    host&.close
  end
end
