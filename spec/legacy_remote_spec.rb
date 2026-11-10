require_relative 'support/desktop'
require_relative 'support/proxy'
require_relative 'support/relay'

RSpec.describe 'Cached legacy remote app compatibility', :desktop do
  before do
    skip 'Linux WebKit desktop required' unless Desktop.linux?
    skip 'Set SAPIENS_TEST_LEGACY_REMOTE to an archived public relay shell' unless ENV['SAPIENS_TEST_LEGACY_REMOTE']
    @assets = File.realpath(ENV.fetch('SAPIENS_TEST_LEGACY_REMOTE'))
    @relay = Relay.new
    @host = Host.new
  end

  after do
    @desktop&.close
    @desktop = nil
    @relay&.stop
    @proxy&.close
    @relay&.close
  end

  it 'pairs the cached iOS entrypoint and sends a message without downloading its old shell again' do
    available, requested = true, []
    @proxy = HostProxy.new(@relay, updates: true) do |request, response|
      next response if request.path.start_with?('/api/')
      path = request.path == '/' || request.path == '/ios' ? 'index.html' : request.path.delete_prefix('/')
      file = File.expand_path(path, @assets)
      next [404, '', {}] unless file.start_with?(@assets + '/') && File.file?(file)
      requested << request.path
      next [503, 'Shell unavailable', {'Cache-Control' => 'no-store'}] unless available
      mime = WEBrick::HTTPUtils.mime_type(file, WEBrick::HTTPUtils::DefaultMimeTypes)
      [200, File.binread(file), {'Content-Type' => mime, 'Cache-Control' => 'no-store'}]
    end
    invitation = host.api('POST', '/api/remote/pair', {relay: @proxy.url, token: @relay.token}, status: 201)['qr']
    start_desktop(@proxy.url + '/ios')
    Contract.wait(description: 'legacy service-worker shell download') do
      requested.include?('/sw.js') && requested.include?('/app-icon-512.png')
    end
    desktop.reload
    desktop.see('Connect remote corpora')
    available = false
    desktop.reload
    desktop.press('Connect remote corpora')
    desktop.fill(/^Or paste an invitation/, JSON.generate(invitation))
    desktop.press('Connect')
    pending = Contract.wait { host.api('GET', '/api/remote')['pending'] }
    desktop.see('Approve this matching code on the host.')
    host.api('POST', '/api/remote/approve', {peer: pending['peer']})
    desktop.see('Message ChiefSapi')
    desktop.reload
    desktop.fill(/^Message ChiefSapi/, 'Message from the cached legacy app')
    desktop.press('Send message')
    desktop.see('Contract test reply')
    expect(host.state['turns'].map { |turn| turn['input'] }).to eq(['Message from the cached legacy app'])
  end
end
