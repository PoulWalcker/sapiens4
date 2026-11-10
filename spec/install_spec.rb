require_relative 'support/relay'

RSpec.describe 'Public installation entry points' do
  it 'serves the installable app and advertised icons without host credentials' do
    relay = Relay.new
    status, manifest = relay.request('GET', '/manifest.webmanifest', nil, bearer: nil)
    expect(status).to eq(200)
    expect(manifest).to include('display' => 'standalone', 'start_url' => '/', 'id' => '/')
    [manifest['start_url'], *manifest.fetch('icons').map { |icon| icon.fetch('src') }].each do |path|
      code, body = relay.request('GET', path, nil, bearer: nil)
      expect(code).to eq(200), "Missing public installation resource: #{path}"
      expect(body).not_to be_empty
    end
    expect(relay.request('GET', '/api/state', nil, bearer: nil).first).to eq(401)
    code, page = relay.request('GET', '/linux', nil, bearer: nil)
    expect(code).to eq(200)
    expect(page).to include('/manifest.webmanifest?platform=linux')
    code, linux = relay.request('GET', '/manifest.webmanifest?platform=linux', nil, bearer: nil)
    expect(code).to eq(200)
    expect(linux.fetch('start_url')).to eq('/linux')
  ensure
    relay&.close
  end

  it 'redirects an old pairing link into the current workspace', :host do
    status, _, headers = host.request('GET', '/remote/')
    expect(status).to eq(302)
    expect(headers['location']).to eq(['/workspace/?pairing=local'])
    expect(host.request('GET', headers['location'].first).first).to eq(200)
  end
end
