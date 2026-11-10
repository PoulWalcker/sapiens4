require_relative 'support/desktop'

RSpec.describe 'Revisiting onboarding storage', :desktop do
  it 'retains the draft through navigation and never restarts the backend for unchanged or rejected storage' do
    skip 'macOS native storage lifecycle' unless RUBY_PLATFORM.include?('darwin')
    @host = Host.new(start: false)
    config = host.environment.fetch('SAPIENS_CONFIG_DIR')
    File.delete(File.join(config, 'onboarding.json'))
    File.write(File.join(config, 'storage.json'), JSON.generate(version: 1, directory: host.root))
    launcher = Contract::Child.new(File.join(Contract::ROOT, '.build/macos/Sapiens4.app/Contents/MacOS/Sapiens4'),
      env: host.environment.merge('SAPIENS_DATA_DIR' => host.root), chdir: Contract::ROOT)
    @desktop = Contract.wait(description: 'native application registration') do
      begin
        Desktop.new(process: launcher, ready: false)
      rescue RuntimeError => error
        raise unless error.message.include?('is not a running application')
        false
      end
    end
    desktop.see('Connect your AI')
    desktop.find('← Back')
    children = -> { IO.popen(['ps', '-axo', 'pid=,ppid=,comm='], &:read).lines.filter_map do |line|
      pid, parent, command = line.strip.split(/\s+/, 3)
      pid.to_i if parent.to_i == launcher.pid && File.basename(command.to_s) == 'sapiens4'
    end }
    original = children.call
    expect(original.length).to eq(1)
    selection = File.read(File.join(config, 'storage.json'))
    6.times do
      desktop.click('← Back')
      desktop.see('Keep current storage')
      desktop.press('Continue')
      desktop.see('Connect your AI')
    end
    desktop.click('← Back')
    desktop.press(/Start fresh/)
    desktop.click('← Back')
    desktop.see('Your AI team starts here.')
    desktop.press('Continue')
    # WebKit exposes the pressed state as AXValue; verify the draft survived.
    expect(desktop.find(/Start fresh/)['value'].to_f).to eq(1.0)
    desktop.press('Continue')
    desktop.see(/Another Sapiens4|empty data directory/)
    expect(children.call).to eq(original)
    expect(File.read(File.join(config, 'storage.json'))).to eq(selection)
    expect(desktop.find(/Start fresh/)['value'].to_f).to eq(1.0)
    desktop.press(/Keep current storage/)
    desktop.press('Continue')
    desktop.see('Connect your AI')
    expect(children.call).to eq(original)
  ensure
    launcher&.close
  end
end
