require_relative 'support/desktop'
require_relative 'support/effect_peer'

RSpec.describe 'External delivery receipts in the native app', :desktop do
  before do
    @host = Host.new(start: false)
    @peer = EffectPeer.new(File.join(host.root, 'native-independent-peer'))
    @peer.page('c0', records: [{id: 'r1', value: 10}], next_cursor: 'opaque:next')
    @peer.delivery_mode = :drop_ack
    host.configure(version: 'codex-cli 0.162.1')
    host.start
    @owner = host.state.fetch('main_agent_id')
    criteria = ['Name record r1 and its supplied value 10.']
    profile = {kind: 'receipt_feed_v1', taskBoundary: 'content_result', executionPolicy: 'response_only', source: {url: @peer.url + '/feed', identity: 'sales-feed', initialCursor: 'c0'}, delivery: {url: @peer.url + '/deliveries', receiptUrl: @peer.url + '/receipts', identity: 'daily-inbox'}}
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", operation: 'create', title: 'Feed delivery', goal: 'Summarize the newly observed feed record.', criteria: criteria, externalProfile: profile).fetch('routine')
    host.configure(rules: {'Execution' => {response: decision('Outcome', reply: 'New record: r1 = 10.', outcome: {status: 'Completed', satisfiedCriteria: criteria, artifacts: []}), usage: {input_tokens: 43, cached_input_tokens: 3, output_tokens: 17}}})
  end

  after do
    @peer&.release
    begin
      @host&.stop
    ensure
      @peer&.close
      @host&.close
    end
  end

  def path
    "/api/agents/#{@owner}/routines/#{@routine}"
  end

  def click_control(label)
    roles = %w[AXButton AXCheckBox AXRadioButton AXLink AXDisclosureTriangle AXPopUpButton]
    node = desktop.find(label, role: roles)
    desktop.control('scroll-to', '--path', node.fetch('path'))
    desktop.click_node(desktop.find(label, role: roles))
  end

  it 'separates accepted content from unknown delivery and checks a receipt without sending again' do
    id = host.api('POST', "#{path}/run", operation: 'native-lost-ack', revision: 1).fetch('run')
    host.turn(id, %w[done failed interrupted])
    expect(host.api('GET', "#{path}/runs/#{id}").dig('externalExecution', 'delivery', 'status')).to eq('unknown')
    expect(@peer.deliveries.length).to eq(1)
    start_desktop
    click_control('Work menu')
    click_control('Automation menu')
    click_control('Routines')
    click_control('Feed delivery')
    desktop.see('Content and delivery')
    desktop.see('Content · Accepted')
    desktop.see('Delivery · Outcome unknown')
    desktop.see('New record: r1 = 10.')
    expect(@peer.requests.none? { |request| request[:path] == '/receipts' }).to be(true)
    calls_before = host.calls.length
    click_control('Check delivery receipt')
    desktop.see('Delivery · Confirmed')
    desktop.see('Content · Accepted')
    expect(@peer.requests.count { |request| request[:path] == '/receipts' }).to eq(1)
    expect(@peer.requests.count { |request| request[:method] == 'POST' }).to eq(1)
    expect(@peer.deliveries.length).to eq(1)
    expect(host.calls.length).to eq(calls_before)
  end
end
