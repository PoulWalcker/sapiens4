require_relative 'support/desktop'
require_relative 'support/relay'

RSpec.describe 'Choosing and managing corpora', :desktop do
  it 'creates a named corpora through the menu and opens its own conversation' do
    start_desktop
    desktop.click('Local')
    desktop.click('＋ Create new local corpora')
    desktop.fill('Corpora name', 'Studio')
    desktop.fill('Scope of work', 'Design research')
    desktop.click('Create')
    desktop.find('Studio', role: 'AXHeading')
    desktop.click('Paused')
    desktop.click('Activate')
    desktop.find('Active', role: 'AXButton')
    desktop.fill('Message ChiefSapi', 'A message for Studio')
    desktop.click('Send message')
    desktop.see('Contract test reply')
    record = host.api('GET', '/api/host')['corpora'].find { |row| row['name'] == 'Studio' }
    expect(host.api('GET', "/api/host/corpora/#{record['id']}/api/state")['turns'].map { |turn| turn['input'] }).to eq(['A message for Studio'])
    expect(host.state['turns']).to be_empty
  end

  it 'switches between corpora without activating paused work or losing a draft' do
    @host = desktop_host
    other = host.api('POST', '/api/host/local', {name: 'Studio'}, status: 201)
    route = "/api/host/corpora/#{other['id']}"
    info = host.api('GET', route + '/api/corpora')
    host.api('PUT', route + '/api/corpora/lifecycle', action: 'pause', revision: info['revision'], operation: SecureRandom.uuid)
    start_desktop
    desktop.fill('Message ChiefSapi', 'Local draft')
    desktop.click('Local')
    desktop.click('Studio')
    desktop.click('Close corpora menu')
    desktop.find('Studio', role: 'AXHeading')
    expect(desktop.text).to include('Paused')
    desktop.click('Local')
    desktop.click(host.state['corpora']['name'])
    desktop.click('Close corpora menu')
    desktop.find(host.state['corpora']['name'], role: 'AXHeading')
    expect(desktop.find('Message ChiefSapi')['value']).to eq('Local draft')
    expect(host.api('GET', route + '/api/corpora')['status']).to eq('paused')
    expect(host.state['corpora']['status']).to eq('active')
  end

  it 'pauses and resumes execution from the status menu' do
    start_desktop
    desktop.find('Active', role: 'AXButton')
    desktop.click('Active')
    desktop.click('Pause')
    desktop.find('Paused', role: 'AXButton')
    expect(host.state['corpora']['status']).to eq('paused')
    desktop.click('Paused')
    desktop.click('Activate')
    desktop.find('Active', role: 'AXButton')
    expect(host.state['corpora']['status']).to eq('active')
  end

  it 'selects a saved remote corpora, returns to local and retains separate drafts after reload' do
    @host = desktop_host
    relay = Relay.new
    remote = Host.new
    metadata = remote.api('GET', '/api/corpora')
    remote.control(remote.state['main_agent_id'], 'corpora_update', name: 'Remote Studio', revision: metadata['revision'])
    invitation = remote.api('POST', '/api/remote/pair', {relay: relay.url, token: relay.token}, status: 201)['qr']
    pairing = host.api('POST', '/api/host/pair', invitation, status: 202)
    pending = Contract.wait { remote.api('GET', '/api/remote')['pending'] }
    remote.api('POST', '/api/remote/approve', {peer: pending['peer']})
    Contract.wait { host.api('GET', "/api/host/pair/#{pairing['id']}")['state'] == 'approved' }
    start_desktop
    desktop.fill('Message ChiefSapi', 'Local draft')
    desktop.click('Local')
    desktop.click('Remote Studio')
    desktop.click('Close corpora menu')
    desktop.find('Remote Studio', role: 'AXHeading')
    desktop.value('Message ChiefSapi', '')
    desktop.fill('Message ChiefSapi', 'Remote draft')
    desktop.reload
    desktop.find('Remote Studio', role: 'AXHeading')
    desktop.value('Message ChiefSapi', 'Remote draft')
    desktop.click('Remote')
    desktop.click(host.state['corpora']['name'])
    desktop.click('Close corpora menu')
    desktop.find(host.state['corpora']['name'], role: 'AXHeading')
    desktop.value('Message ChiefSapi', 'Local draft')
    expect(host.state['corpora']['status']).to eq('active')
    expect(remote.state['corpora']['status']).to eq('active')
  ensure
    remote&.close
    relay&.close
  end
end
