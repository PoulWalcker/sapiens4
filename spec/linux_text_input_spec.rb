require_relative 'support/desktop'
require_relative 'support/http_peer'

RSpec.describe 'Linux browser text verification', :desktop do
  before do
    skip 'Linux X11 desktop required' unless Desktop.linux?
    blocked = RSpec.current_example.metadata[:blocked] ? 'onkeydown="event.preventDefault()"' : ''
    @pages = HTTPPeer.new do |_request|
      [200, '<form method="post"><label>Draft<textarea aria-label="Draft" ' + blocked + '>Original</textarea></label><button>Send</button></form>',
        {'Content-Type' => 'text/html; charset=utf-8'}]
    end
    start_desktop(@pages.url)
  end

  after { @pages&.close }

  it 'verifies a long replacement through the native keyboard transport' do
    text = '{"invitation":"' + ('Ab09_-/' * 50) + '"}'
    desktop.fill('Draft', text)
    expect(desktop.find('Draft')['value']).to eq(text)
    expect(@pages.requests.none? { |request| request[:method] == 'POST' }).to eq(true)
  end

  it 'rejects unconsumed input and a submission guarded by that unverified text', :blocked do
    desktop.click('Draft')
    expect { desktop.control('type', '--text', 'Unconsumed input') }
      .to raise_error(RuntimeError, /Keyboard text failed exact draft verification/)
    draft = desktop.find('Draft')
    expect(draft['value']).to eq('Original')
    send = desktop.find('Send', role: 'AXButton')
    expect do
      desktop.control('press', '--path', send.fetch('path'),
        '--require-value-path', draft.fetch('path'), '--require-value', 'Unconsumed input')
    end.to raise_error(RuntimeError, /draft|value/i)
    expect(@pages.requests.none? { |request| request[:method] == 'POST' }).to eq(true)
  end
end
