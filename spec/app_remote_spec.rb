require_relative 'support/file_picker'
require_relative 'support/relay'

RSpec.describe 'Connecting from the shipped remote app', :desktop do
  before do
    skip 'Remote WebKit fixture requires macOS or Linux' if Gem.win_platform?
    @relay = Relay.new
    @host = Host.new
    @invitation = host.api('POST', '/api/remote/pair', {relay: @relay.url, token: @relay.token}, status: 201)['qr']
    start_desktop(@relay.url + (Desktop.linux? ? '/linux' : '/mac'))
  end
  after { @relay&.close }

  def connect
    desktop.press('Connect remote corpora')
    desktop.fill('Or paste an invitation', JSON.generate(@invitation))
    desktop.press('Connect')
    pending = Contract.wait { host.api('GET', '/api/remote')['pending'] }
    expect(desktop.see(pending['confirmation_code'])).to include('Approve this matching code')
    expect(host.state['turns']).to be_empty
    host.api('POST', '/api/remote/approve', {peer: pending['peer']})
    desktop.see('Message ChiefSapi')
  end

  it 'waits for matching-code approval, sends an encrypted message and reconnects after reload' do
    connect
    desktop.fill('Message ChiefSapi', 'A private remote message')
    desktop.press('Send message')
    desktop.see('Contract test reply')
    desktop.fill('Message ChiefSapi', 'Unsent remote draft')
    desktop.reload
    desktop.see('Contract test reply')
    expect(desktop.find('Message ChiefSapi')['value']).to eq('Unsent remote draft')
    expect(host.state['turns'].map { |turn| turn['input'] }).to eq(['A private remote message'])
  end

  it 'renders literal remote file previews and recovers after the file becomes unavailable' do
    file = File.join(host.workspace(chief), 'preview.txt')
    File.write(file, 'Literal <b>remote preview</b>')
    host.control(chief, 'workspace_open', path: file)
    connect
    expect(desktop.see('Literal <b>remote preview</b>')).to include('preview.txt')
    File.unlink(file)
    desktop.see('File not found or unreadable')
    File.write(file, 'Recovered remote preview')
    expect(desktop.see('Recovered remote preview')).not_to include('File not found or unreadable')
  end

  it 'rejects a malformed invitation and still accepts a valid invitation afterward' do
    desktop.press('Connect remote corpora')
    desktop.fill('Or paste an invitation', 's4.A.abc.abc.abc.A.1')
    desktop.press('Connect')
    desktop.see('Use the QR')
    expect(host.api('GET', '/api/remote')['pending']).to be_nil
    desktop.fill('Or paste an invitation', JSON.generate(@invitation))
    desktop.press('Connect')
    pending = Contract.wait { host.api('GET', '/api/remote')['pending'] }
    expect(desktop.see(pending['confirmation_code'])).to include('Approve this matching code')
  end

  it 'preserves an invitation on cancelled QR upload and pairs from a selected QR image' do
    desktop.press('Connect remote corpora')
    desktop.fill('Or paste an invitation', 'Keep this invitation')
    desktop.press('Upload QR')
    desktop.find('Open', role: 'AXButton')
    desktop.press('Cancel')
    desktop.value('Or paste an invitation', 'Keep this invitation')
    expect(host.api('GET', '/api/remote')['pending']).to be_nil
    Dir.mktmpdir('sapiens-qr-upload') do |dir|
      path = File.join(File.realpath(dir), 'invitation.png')
      out, error, status = Contract.capture('node', '-e',
        "require('./submodules/libs4/node_modules/qrcode').toFile(process.argv[1], process.argv[2], {width: 1024})",
        path, JSON.generate(@invitation))
      expect(status).to eq(0), out + error
      desktop.press('Upload QR')
      choose_file(path)
      pending = Contract.wait { host.api('GET', '/api/remote')['pending'] }
      desktop.see(pending['confirmation_code'])
      host.api('POST', '/api/remote/approve', {peer: pending['peer']})
      desktop.see('Message ChiefSapi')
    end
  end
end
