require_relative 'support/desktop'
RSpec.describe 'Linux native automation' do
  before do
    skip 'Isolated Linux automation session required' unless ENV['SAPIENS_TEST_LINUX_AUTOMATION'] == '1'
    @binary = Desktop.driver
    executable = File.join(Contract::ROOT, '.build/linux-controls')
    flags, error, status = Contract.capture('pkg-config', '--cflags', '--libs', 'gtk4', 'x11')
    expect(status).to eq(0), error
    _, error, status = Contract.capture('cc', '-Wall', '-Wextra', '-Werror',
      File.join(Contract::ROOT, 'spec/support/LinuxControls.c'), '-o', executable, *flags.split)
    expect(status).to eq(0), error
    @window = Contract::Child.new(executable)
    Contract.wait(description: 'native control discovery') do
      output, _, status = Contract.capture(@binary, 'find', '--pid', @window.pid.to_s, '--title', 'Draft', '--role', 'TextField')
      @draft = status == 0 && JSON.parse(output).fetch('matches').find { |node| node['role'] == 'AXTextField' }
    end
  end

  after do
    if @window
      @window.signal('TERM')
      @window.close
    end
  end

  def cli(*arguments)
    output, error, status = Contract.capture(@binary, *arguments)
    expect(status).to eq(0), output + error
    JSON.parse(output)
  end

  it 'discovers real controls, preserves Unicode edits and redacts passwords' do
    expect(cli('apps').fetch('apps')).to include(a_hash_including('pid' => @window.pid))
    cli('set-value', '--pid', @window.pid.to_s, '--path', @draft.fetch('path'), '--value', "Ubuntu café العربية")
    expect(cli('inspect', '--pid', @window.pid.to_s, '--path', @draft.fetch('path')).fetch('value')).to eq("Ubuntu café العربية")
    tree = cli('tree', '--pid', @window.pid.to_s, '--depth', '8', '--max-nodes', '100')
    expect(JSON.generate(tree)).not_to include('fixture-secret')
    expect(JSON.generate(tree)).to include('AXSecureTextField')
  end

  it 'rejects a mismatched draft and permits an exact targeted action' do
    cli('set-value', '--pid', @window.pid.to_s, '--path', @draft.fetch('path'), '--value', 'Review this fixture')
    button = cli('find', '--pid', @window.pid.to_s, '--title', 'Send', '--role', 'Button').fetch('matches').first
    arguments = ['press', '--pid', @window.pid.to_s, '--path', button.fetch('path'),
      '--require-value-path', @draft.fetch('path'), '--require-value']
    output, _, status = Contract.capture(@binary, *arguments, 'Different text')
    expect(status).to eq(77), output
    expect(@window.stdout).not_to include('SENT ')
    cli(*arguments, 'Review this fixture')
    Contract.wait(description: 'native button effect') { @window.stdout.include?('SENT ') }
    expect(@window.stdout.lines.find { |line| line.start_with?('SENT ') }.split.last.unpack1('m0')).to eq('Review this fixture')
  end

  it 'bounds accessibility traversal without changing the draft' do
    tree = cli('tree', '--pid', @window.pid.to_s, '--depth', '8', '--max-nodes', '1')
    expect(tree.fetch('truncated')).to eq(true)
    expect(cli('inspect', '--pid', @window.pid.to_s, '--path', @draft.fetch('path')).fetch('value')).to eq('')
  end

  it 'reports the current control after focus moves within the application' do
    cli('activate', '--pid', @window.pid.to_s)
    [['focus-send', 'AXButton'], ['focus', 'AXTextField'], ['focus-send', 'AXButton']].each do |command, role|
      @window.input.puts(command)
      focused = nil
      Contract.wait(description: "focus moved to #{role}") do
        focused = cli('focused', '--pid', @window.pid.to_s)
        focused['role'] == role
      end
      expect(focused).to include('pid' => @window.pid, 'focused' => true)
    end
    expect(@window.stdout).not_to include('SENT ')
  end

  it 'uses the requested display scale for native controls' do
    skip 'Explicit scaling run required' unless ENV['SAPIENS_TEST_SCALE']
    @window.input.puts('scale')
    Contract.wait(description: 'native scale observation') { @window.stdout.include?('SCALE ') }
    expect(@window.stdout[/SCALE (\d+)/, 1].to_i).to eq(Integer(ENV.fetch('SAPIENS_TEST_SCALE')))
  end

  it 'replaces and verifies a Unicode draft through the native editable control' do
    cli('set-value', '--pid', @window.pid.to_s, '--path', @draft.fetch('path'), '--value', 'Previous draft')
    result = cli('paste', '--pid', @window.pid.to_s, '--target-path', @draft.fetch('path'), '--text', 'Café العربية 🐧')
    expect(result.fetch('verified')).to eq(true)
    expect(cli('inspect', '--pid', @window.pid.to_s, '--path', @draft.fetch('path')).fetch('value')).to eq('Café العربية 🐧')
    expect(@window.stdout).not_to include('SENT ')
  end

  it 'injects an Xorg key with a verified foreground PID and exact draft' do
    skip 'Xorg only' unless ENV['XDG_SESSION_TYPE'] == 'x11'
    cli('set-value', '--pid', @window.pid.to_s, '--path', @draft.fetch('path'), '--value', 'Xorg input')
    arguments = ['key', '--pid', @window.pid.to_s, '--key', 'enter', '--target-path', @draft.fetch('path'), '--require-value']
    _, _, status = Contract.capture(@binary, *arguments, 'wrong')
    expect(status).to eq(77)
    expect(@window.stdout).not_to include('SENT ')
    cli(*arguments, 'Xorg input')
    Contract.wait(description: 'XTest keyboard effect') { @window.stdout.include?('SENT ') }
    expect(@window.stdout.lines.grep(/^SENT /).length).to eq(1)
  end

  it 'checks Xorg pointer ownership before clicking and scrolling' do
    skip 'Xorg only' unless ENV['XDG_SESSION_TYPE'] == 'x11'
    _, _, status = Contract.capture(@binary, 'click', '--pid', @window.pid.to_s, '--x', '0', '--y', '0')
    expect(status).to eq(77)
    expect(@window.stdout).not_to include('SENT ')
    @window.input.puts('point')
    point = nil
    Contract.wait(description: 'fixture screen coordinates') { point = @window.stdout.match(/BUTTON (\d+) (\d+)/) }
    cli('click', '--pid', @window.pid.to_s, '--x', point[1], '--y', point[2])
    Contract.wait(description: 'XTest button effect') { @window.stdout.include?('SENT ') }
    cli('scroll', '--pid', @window.pid.to_s, '--direction', 'down', '--amount', '1')
    Contract.wait(description: 'XTest scroll effect') { @window.stdout.include?('SCROLLED ') }
  end

end
