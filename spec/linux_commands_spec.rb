require_relative 'support/generated_command'

RSpec.describe 'Linux command forwarding' do
  it 'preserves arguments, environment, working directory, streams and exit status without a display' do
    skip 'Linux launcher required' unless RUBY_PLATFORM.include?('linux')
    Dir.mktmpdir("sapiens cli's é ") do |directory|
      launcher = File.join(directory, 'sapiens-linux')
      FileUtils.cp(File.join(ENV.fetch('SAPIENS_TEST_LINUX_RUNTIME', Contract::ROOT), '.build/bin/sapiens-linux'), launcher)
      core = File.join(directory, 'sapiens4')
      File.write(core, "#!#{RbConfig.ruby}\nrequire 'json'\nputs JSON.generate(args: ARGV, cwd: Dir.pwd, run: ENV['SAPIENS_AGENCY_RUN'], repository: ENV['SAPIENS_REPOSITORY'], backend: ENV['SAPIENS_LINUX_BACKEND'], input: STDIN.read)\nwarn 'core stderr'\nexit 37\n")
      File.chmod(0755, core)
      %w[control computer].each do |kind|
        arguments = [kind, '', "Résumé 中文 😎 'quoted'", '; $(false)', '--help']
        output, error, status = Contract.capture(launcher, *arguments, chdir: directory, input: 'core stdin',
          env: {'DISPLAY' => nil, 'WAYLAND_DISPLAY' => nil, 'GDK_BACKEND' => 'x11',
            'SAPIENS_AGENCY_RUN' => 'fixture-run', 'SAPIENS_REPOSITORY' => '/unused', 'SAPIENS_LINUX_BACKEND' => 'fixture'})
        expect(status).to eq(37), output + error
        expect(error).to eq("core stderr\n")
        expect(JSON.parse(output)).to eq('args' => arguments, 'cwd' => directory, 'run' => 'fixture-run',
          'repository' => '/unused', 'backend' => 'fixture', 'input' => 'core stdin')
      end
    end
  end
end

RSpec.describe 'Linux desktop generated agent commands' do
  before do
    skip 'Linux desktop session required' unless RUBY_PLATFORM.include?('linux') && ENV['SAPIENS_TEST_LINUX_DESKTOP'] == '1'
    @runtime = ENV.fetch('SAPIENS_TEST_LINUX_RUNTIME', Contract::ROOT)
    @host = Host.new(start: false)
    @desktop = Contract::Child.new(File.join(@runtime, '.build/bin/sapiens-linux'), '--data-dir', host.root,
      env: host.environment.merge('SAPIENS_REPOSITORY' => nil), chdir: File.dirname(host.root))
    url = Contract.wait(description: 'desktop command fixture readiness') do
      @desktop.stdout[%r{READY (http://127.0.0.1:\d+)}, 1]
    end
    host.instance_variable_set(:@url, url)
  end

  after do
    @desktop&.signal('TERM')
    @desktop&.close
    @host&.close
  end

  def command(owner, kind, *arguments, run: nil)
    GeneratedCommand.capture(host.workspace(owner), kind, arguments, host.environment.merge(
      'DISPLAY' => nil, 'WAYLAND_DISPLAY' => nil, 'GDK_BACKEND' => 'x11', 'SAPIENS_AGENCY_RUN' => run))
  end

  def result(value, status = 0)
    expect(value.fetch('status')).to eq(status), value.inspect
    JSON.parse(value.fetch('stdout'))
  end

  def action(kind, *arguments)
    {generated_command: kind, arguments: arguments}
  end

  def commands(owner, count)
    Contract.wait(description: "#{count} generated commands for #{owner}") do
      host.calls.find { |entry| entry['owner'] == owner && entry.fetch('commands', []).size >= count }
    end
  end

  it 'executes the published control command without a display and rejects idle computer access' do
    owner = chief
    expect(result(command(owner, 'control', JSON.generate(op: 'status'))).fetch('notes').fetch('workspace')).to eq(host.workspace(owner))
    prompt = "Résumé 中文 😎 — 'quoted' \"text\"; $(false)"
    saved = result(command(owner, 'control', JSON.generate(op: 'routine_create', minutes: 2, prompt: prompt)))
    expect(saved.fetch('routine').fetch('prompt')).to eq(prompt)
    expect(result(command(owner, 'control', '{'), 1)).to include('error' => a_kind_of(String))
    rejected = result(command(owner, 'computer', 'blindly', 'apps'), 1)
    expect(rejected.fetch('error')).to include('requires a running Sapi turn')
    expect(File).not_to exist(File.join(host.workspace(owner), '.computer-used'))
    expect(host.state.fetch('computer').fetch('owner')).to be_nil
  end

  it 'runs generated commands in a real turn and rejects that run after completion' do
    owner = chief
    host.configure(rules: {'Assessing' => {actions: [action('control', JSON.generate(op: 'status')),
      action('computer', 'blindly', 'apps'), {gate: 'finish'}]}})
    turn = host.submit(owner)
    call = commands(owner, 2)
    result(call.fetch('commands')[0])
    expect(result(call.fetch('commands')[1]).fetch('apps')).to be_an(Array)
    expect(host.state.fetch('computer').fetch('owner')).to eq(owner)
    expect(File).to exist(File.join(host.workspace(owner), '.computer-used'))
    host.release('finish')
    host.turn(turn)
    Contract.wait { host.state.fetch('computer').fetch('owner').nil? }
    [['control', JSON.generate(op: 'status')], ['computer', 'blindly', 'apps']].each do |kind, *args|
      expect(result(command(owner, kind, *args, run: call.fetch('agency_run')), 1).fetch('error')).to include('no longer active')
    end
  end

  it 'enforces exclusive ownership, cancels its owner, and permits the waiting turn to acquire' do
    first = chief
    second = host.agent('SecondOwner')
    host.configure(rules: {
      "#{first}:Assessing" => {actions: [action('computer', 'blindly', 'apps'), {gate: 'hold-owner'}]},
      "#{second}:Assessing" => {actions: [action('computer', 'blindly', 'apps'), {gate: 'try-after-cancel'},
        action('computer', 'blindly', 'apps'), {gate: 'finish-second'}]}
    })
    a = host.submit(first)
    first_call = commands(first, 1)
    result(first_call.fetch('commands').first)
    b = host.submit(second)
    second_call = commands(second, 1)
    expect(result(second_call.fetch('commands').first, 1).fetch('error')).to include('busy with another Sapi')
    expect(File).not_to exist(File.join(host.workspace(second), '.computer-used'))
    expect(host.state.fetch('computer').fetch('owner')).to eq(first)
    host.api('POST', "/api/agents/#{first}/turns/#{a}/cancel", {})
    host.turn(a, %w[interrupted cancelled])
    Contract.wait { host.state.fetch('computer').fetch('owner').nil? }
    expect(result(command(first, 'computer', 'blindly', 'apps', run: first_call.fetch('agency_run')), 1).fetch('error')).to include('no longer active')
    host.release('try-after-cancel')
    expect(result(commands(second, 2).fetch('commands').last).fetch('apps')).to be_an(Array)
    expect(host.state.fetch('computer').fetch('owner')).to eq(second)
    host.release('finish-second')
    host.turn(b)
    Contract.wait { host.state.fetch('computer').fetch('owner').nil? }
  end

  it 'returns to the originating desktop after a turn uses another application on Xorg' do
    skip 'X11 session required' unless ENV['XDG_SESSION_TYPE'] == 'x11'
    other = Host.new(start: false)
    window = Contract::Child.new(File.join(@runtime, '.build/bin/sapiens-linux'), '--data-dir', other.root,
      env: other.environment.merge('SAPIENS_REPOSITORY' => nil))
    Contract.wait(description: 'second desktop readiness') { window.stdout.include?('READY ') }
    driver = File.join(@runtime, 'submodules/blindly4/.build/release/blindly4')
    output, error, status = Contract.capture(driver, 'activate', '--pid', @desktop.pid.to_s)
    expect(status).to eq(0), output + error
    host.configure(rules: {'Assessing' => {actions: [
      action('computer', 'blindly', 'activate', '--pid', window.pid.to_s), {gate: 'finish-foreground'}
    ]}})
    turn = host.submit(chief)
    result(commands(chief, 1).fetch('commands').first)
    foreground = lambda do
      output, error, status = Contract.capture(driver, 'apps')
      expect(status).to eq(0), output + error
      JSON.parse(output).fetch('apps').find { |app| app['frontmost'] }&.fetch('pid')
    end
    expect(foreground.call).to eq(window.pid)
    host.release('finish-foreground')
    host.turn(turn)
    Contract.wait(description: 'originating desktop restored') { foreground.call == @desktop.pid }
  ensure
    window&.signal('TERM')
    window&.close
    other&.close
  end
end
