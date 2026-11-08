require_relative 'spec_helper'

RSpec.describe 'Local Codex setup' do
  it 'isolates ChatGPT sign-in and safely replaces accounts across restart' do
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
      if ARGV.include?('logout')
        warn 'synthetic-private-logout-diagnostic'
        exit 1 if File.exist?(File.join(root, 'fail-logout'))
        sleep 60 if File.exist?(File.join(root, 'slow-logout'))
        File.delete(File.join(home, 'auth.json')) if File.exist?(File.join(home, 'auth.json'))
        exit
      end
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
        if ARGV.include?('--device-auth')
          puts '2. Enter this one-time code (expires in 15 minutes)'
          puts "\\e[94mABCD-EFGH\\e[0m"
        else
          $stderr.sync = true
          warn 'https://untrusted.example/oauth/authorize?state=must-not-escape'
          warn 'https://auth.openai.com/oauth/authorize?state=synthetic&response_type=code'
        end
        sleep 0.02 until File.exist?(File.join(root, 'finish-login'))
        File.write(File.join(home, 'auth.json'), 'synthetic app authentication')
        exit
      end
      load #{File.join(__dir__, 'support/provider.rb').inspect}
    RUBY
    host.start
    expect(host.api('GET', '/api/host/onboarding')).to include('provider' => 'sign_in_required', 'step' => 'welcome', 'completed' => false)
    host.api('POST', '/api/host/onboarding', {action: 'finish'}, status: 409)
    expect(host.api('POST', '/api/host/onboarding', action: 'begin')).to include('step' => 'sign_in', 'completed' => false)
    host.api('POST', '/api/host/onboarding', {action: 'finish'}, status: 409)
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
    expect(host.request('POST', '/api/host/onboarding', {action: 'device-login'}, 'Origin' => 'https://untrusted.example').first).to eq(403)
    expect(host.request('POST', '/api/host/onboarding', {action: 'device-login'}, 'X-Sapiens-Local' => '').first).to eq(403)
    host.api('POST', '/api/host/onboarding', action: 'device-login')
    view = Contract.wait { v = host.api('GET', '/api/host/onboarding'); v if v.dig('login', 'code') == 'ABCD-EFGH' }
    expect(JSON.generate(view)).not_to include('synthetic-private-diagnostic', 'synthetic-private-logout-diagnostic')
    host.api('POST', '/api/host/onboarding', {action: 'device-login'}, status: 409)
    expect(host.api('POST', '/api/host/onboarding', action: 'cancel').dig('login', 'state')).to eq('cancelled')
    expect(host.api('GET', '/api/host/onboarding')).to include('step' => 'sign_in', 'completed' => false)
    expect { Process.kill(0, File.read(File.join(host.root, 'login.pid')).to_i) }.to raise_error(Errno::ESRCH)
    expect(File.exist?(File.join(config, 'codex/auth.json'))).to eq(false)
    host.api('POST', '/api/host/onboarding', action: 'device-login')
    Contract.wait { host.api('GET', '/api/host/onboarding').dig('login', 'code') == 'ABCD-EFGH' }
    login_pid = File.read(File.join(host.root, 'login.pid')).to_i
    host.stop
    expect { Process.kill(0, login_pid) }.to raise_error(Errno::ESRCH)
    expect(File.exist?(File.join(config, 'codex/auth.json'))).to eq(false)
    host.start
    expect(host.api('GET', '/api/host/onboarding')).to include('step' => 'sign_in', 'completed' => false)
    File.write(File.join(host.root, 'fail-login'), '')
    host.api('POST', '/api/host/onboarding', action: 'device-login')
    Contract.wait { host.api('GET', '/api/host/onboarding').dig('login', 'state') == 'failed' }
    File.delete(File.join(host.root, 'fail-login'))
    host.api('POST', '/api/host/onboarding', action: 'device-login')
    File.write(File.join(host.root, 'finish-login'), '')
    Contract.wait { host.api('GET', '/api/host/onboarding').fetch('ready') }
    expect(host.api('GET', '/api/host/onboarding')).to include('step' => 'permissions', 'completed' => false)
    auth_file = File.join(config, 'codex/auth.json')
    saved_auth = File.binread(auth_file)
    File.write(File.join(host.root, 'fail-status'), '')
    expect(host.api('GET', '/api/host/onboarding').fetch('provider')).to eq('unavailable')
    host.api('POST', '/api/host/onboarding', {action: 'device-login'}, status: 503)
    host.api('POST', '/api/host/onboarding', {action: 'finish'}, status: 409)
    host.api('POST', '/api/host/onboarding', action: 'refresh')
    expect(File.binread(auth_file)).to eq(saved_auth)
    File.delete(File.join(host.root, 'fail-status'))
    host.api('POST', '/api/host/onboarding', {action: 'device-login'}, status: 409)
    # Optional computer permissions/helper availability do not gate basic chat.
    expect(host.api('POST', '/api/host/onboarding', action: 'finish')).to include('step' => 'complete', 'completed' => true)
    setup_file = File.join(config, 'onboarding.json')
    expect(JSON.parse(File.read(setup_file))).to eq('schema' => 1, 'welcomed' => true, 'completed' => true)
    host.restart
    expect(host.api('GET', '/api/host/onboarding')).to include('ready' => true, 'step' => 'complete', 'completed' => true)
    expect(File.binread(auth_file)).to eq(saved_auth)
    # Existing app auth skips redundant sign-in when setup has no prior marker.
    File.rename(setup_file, setup_file + '.preserved')
    host.restart
    expect(host.api('GET', '/api/host/onboarding').fetch('step')).to eq('welcome')
    expect(host.api('POST', '/api/host/onboarding', action: 'begin').fetch('step')).to eq('permissions')
    expect(File.binread(auth_file)).to eq(saved_auth)
    host.api('POST', '/api/host/onboarding', action: 'finish')
    id = host.submit(host.state.fetch('main_agent_id'), 'Hello')
    host.turn(id)
    # Whole runs in any local corpus prevent account mutation. Pending work is
    # kept across replacement and requires explicit resume with the new account.
    chief = host.state.fetch('main_agent_id')
    host.configure(block: true)
    active = host.submit(chief, 'Keep this task running')
    Contract.wait { host.calls.any? { |call| call['prompt'].include?('Keep this task running') } }
    host.api('POST', '/api/host/onboarding', {action: 'change-account'}, status: 409)
    expect(File.binread(auth_file)).to eq(saved_auth)
    host.release
    host.turn(active)
    host.configure(block: false)
    # Another host sharing this managed profile takes the same OS-wide lease.
    other = Host.new(environment: {'SAPIENS_CONFIG_DIR' => config})
    other.configure(block: true)
    other_id = other.submit(other.state.fetch('main_agent_id'), 'Work in another host')
    Contract.wait { other.calls.any? }
    host.api('POST', '/api/host/onboarding', {action: 'sign-out'}, status: 409)
    expect(File.binread(auth_file)).to eq(saved_auth)
    other.release
    other.turn(other_id)
    other.close
    other = nil
    File.delete(File.join(host.root, 'finish-login'))
    # Confirmation is client-side; only the explicit action invokes logout.
    Contract.wait do
      code, value, = host.request('POST', '/api/host/onboarding', action: 'change-account')
      raise value.inspect unless [200, 409].include?(code)
      code == 200
    end
    browser = Contract.wait { v = host.api('GET', '/api/host/onboarding'); v if v.dig('login', 'url').to_s.include?('response_type=code') }
    expect(browser.dig('login', 'code')).to eq('')
    expect(JSON.generate(browser)).not_to include('untrusted.example', 'synthetic-private')
    host.api('POST', '/api/agents/' + chief + '/messages', {text: 'Must not start while switching'}, status: 409)
    expect(host.api('POST', '/api/host/onboarding', action: 'cancel')).to include('ready' => false, 'completed' => true)
    expect(File.exist?(auth_file)).to eq(false)
    host.restart
    expect(host.api('GET', '/api/host/onboarding')).to include('completed' => true, 'ready' => false, 'hold' => 'disconnected')
    occupied = TCPServer.new('127.0.0.1', 1455)
    host.api('POST', '/api/host/onboarding', action: 'login')
    Contract.wait { host.api('GET', '/api/host/onboarding').dig('login', 'state') == 'browser_busy' }
    occupied.close
    occupied = nil
    host.api('POST', '/api/host/onboarding', action: 'login')
    Contract.wait { host.api('GET', '/api/host/onboarding').dig('login', 'url').to_s.include?('response_type=code') }
    File.write(File.join(host.root, 'finish-login'), '')
    Contract.wait { host.api('GET', '/api/host/onboarding').fetch('ready') }
    expect(host.api('GET', '/api/host/onboarding')).to include('completed' => true, 'hold' => 'review')
    host.restart
    expect(host.api('GET', '/api/host/onboarding').fetch('hold')).to eq('review')
    host.api('POST', '/api/agents/' + chief + '/messages', {text: 'Must not start before review'}, status: 409)
    expect(host.api('POST', '/api/host/onboarding', action: 'resume').fetch('hold')).to eq('')
    host.turn(host.submit(chief, 'A task with the reconnected account'))
    # Failed logout preserves the honest connected/uncertain state and blocks AI.
    File.write(File.join(host.root, 'fail-logout'), '')
    host.api('POST', '/api/host/onboarding', action: 'sign-out')
    failed = Contract.wait { v = host.api('GET', '/api/host/onboarding'); v if v.dig('login', 'state') == 'logout_failed' }
    expect(failed).to include('ready' => true, 'hold' => 'uncertain')
    expect(File.binread(auth_file)).to eq(saved_auth)
    host.api('POST', '/api/host/onboarding', {action: 'resume'}, status: 409)
    File.delete(File.join(host.root, 'fail-logout'))
    host.api('POST', '/api/host/onboarding', action: 'sign-out')
    Contract.wait { host.api('GET', '/api/host/onboarding').dig('login', 'state') == 'signed_out' }
    expect(host.api('GET', '/api/host/onboarding')).to include('completed' => true, 'ready' => false)
    host.api('POST', '/api/host/onboarding', action: 'login')
    Contract.wait { host.api('GET', '/api/host/onboarding').fetch('ready') }
    host.api('POST', '/api/host/onboarding', action: 'resume')
    expect(host.turn(id)['output']).to eq('Contract test reply')
    # Shutdown during revocation leaves an uncertainty hold across restart.
    File.write(File.join(host.root, 'slow-logout'), '')
    host.api('POST', '/api/host/onboarding', action: 'sign-out')
    Contract.wait { host.api('GET', '/api/host/onboarding').fetch('provider') == 'signing_out' }
    host.api('POST', '/api/host/onboarding', {action: 'cancel'}, status: 409)
    host.restart
    expect(host.api('GET', '/api/host/onboarding')).to include('ready' => true, 'hold' => 'uncertain')
    host.api('POST', '/api/host/onboarding', {action: 'resume'}, status: 409)
    File.delete(File.join(host.root, 'slow-logout'))
    host.api('POST', '/api/host/onboarding', action: 'change-account')
    Contract.wait { host.api('GET', '/api/host/onboarding').fetch('ready') }
    host.api('POST', '/api/host/onboarding', action: 'resume')

    invocations = File.readlines(File.join(host.root, 'auth-invocations.jsonl')).map { |line| JSON.parse(line) }
    expect(invocations.any? { |v| v['args'].include?('exec') }).to eq(true)
    expect(invocations.map { |v| v['home'] }.uniq).to eq([File.join(config, 'codex')])
    expect(invocations.all? { |v| v['args'].include?('cli_auth_credentials_store="file"') && v['inherited_keys'].empty? }).to eq(true)
    expect(host.process.stderr).not_to include('synthetic-private-diagnostic', 'synthetic-private-logout-diagnostic')
    expect(File.read(File.join(external, 'auth.json'))).to eq('external synthetic authentication')
    expect(File.stat(File.join(config, 'codex')).mode & 0777).to eq(0700)
    # Managed configuration must fail closed instead of selecting Codex on PATH.
    runtime = File.join(host.root, 'missing provider runtime')
    %w[app/shell/index.html .build/web/app.js].each do |relative|
      destination = File.join(runtime, relative)
      FileUtils.mkdir_p(File.dirname(destination))
      FileUtils.cp(File.join(Contract::ROOT, relative), destination)
    end
    FileUtils.cp_r(File.join(Contract::ROOT, 'corpora'), runtime)
    File.write(File.join(runtime, 'package-manifest.json'), '{}')
    host.environment['SAPIENS_REPOSITORY'] = runtime
    host.environment['SAPIENS_CODEX_BINARY'] = ''
    host.environment['PATH'] = host.root + File::PATH_SEPARATOR + ENV.fetch('PATH')
    count = invocations.length
    host.restart
    expect(host.api('GET', '/api/host/onboarding')).to include('provider' => 'missing', 'step' => 'sign_in', 'completed' => true)
    host.api('POST', '/api/host/onboarding', {action: 'device-login'}, status: 503)
    expect(File.readlines(File.join(host.root, 'auth-invocations.jsonl')).length).to eq(count)
  ensure
    if host && File.exist?(File.join(host.root, 'status-pids'))
      File.read(File.join(host.root, 'status-pids')).split.map(&:to_i).each { |pid| Process.kill('KILL', pid) rescue Errno::ESRCH }
    end
    occupied&.close
    other&.close
    host&.close
  end
end
