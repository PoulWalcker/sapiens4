require_relative 'support/desktop'
require_relative 'support/proxy'

RSpec.describe 'Routine list, results and settings in the native app', :desktop do
  def click_button(label)
    desktop.click_node(desktop.find(label, role: %w[AXButton AXCheckBox AXRadioButton AXLink AXDisclosureTriangle AXPopUpButton]))
  end

  def open_routines
    click_button('Work menu')
    click_button('Automation menu')
    click_button('Routines')
  end

  def create_routine(name = 'Daily comparison')
    click_button('+ New routine')
    desktop.fill('Name (up to 60 characters)', name)
    desktop.fill('What to do', 'Compare the products.')
    desktop.fill('Completion criteria (one per line)', 'Return a sourced comparison.')
    click_button('Create routine')
    desktop.see('No results yet')
  end

  it 'uses separate list, latest result, history, run and inline settings screens' do
    @host = desktop_host
    start_desktop
    open_routines
    create_routine
    click_button('Manage Daily comparison')
    click_button('Settings')
    desktop.fill('Name (up to 60 characters)', 'Weekly comparison')
    click_button('Save changes')
    desktop.see('Weekly comparison')
    click_button('Run now')
    desktop.fill('Inputs for this run', 'Today’s products')
    click_button('Run now')
    desktop.see('Verified comparison')
    desktop.see('Definition revision 2')
    click_button('History')
    click_button(/Run 1 ·/)
    desktop.see('Today’s products')
    host.configure(block: true)
    click_button('Run now')
    click_button('Run now')
    desktop.see('Work is in progress.')
    click_button('Stop this run…')
    click_button('Stop run')
    desktop.see('Stopped')
    click_button('Manage Weekly comparison')
    click_button('Delete…')
    click_button('Delete routine and stop runs')
    click_button('Weekly comparison')
    click_button('History')
    click_button(/Run 1 ·/)
    desktop.see('Verified comparison')
    desktop.reload
    click_button('Deleted')
    click_button('Weekly comparison')
    click_button('History')
    click_button(/Run 1 ·/)
    desktop.see('Verified comparison')
  end

  it 'preserves inline edits across polling and Sapi changes, and confirms explicit discard' do
    @host = desktop_host
    host.agent('OtherSapi')
    start_desktop
    open_routines
    create_routine
    click_button('Manage Daily comparison')
    click_button('Settings')
    desktop.fill('What to do', 'An unsaved request')
    click_button('Open OtherSapi')
    click_button('Open ChiefSapi')
    open_routines
    desktop.value('What to do', 'An unsaved request')
    click_button('Cancel')
    desktop.see('Your changes have not been saved.')
    click_button('Discard changes')
    click_button('Manage Daily comparison')
    click_button('Settings')
    desktop.value('What to do', 'Compare the products.')
  end

  it 'keeps same-title legacy intervals visible without merging them with manual definitions' do
    @host = desktop_host
    owner = host.state.fetch('main_agent_id')
    interval = host.control(owner, 'routine_create', minutes: 60, prompt: 'Same title')['routine']
    host.api('POST', "/api/agents/#{owner}/routines", operation: 'manual-fixture', title: 'Same title', goal: 'Compare the products.', criteria: ['Return a sourced comparison.'])
    start_desktop
    open_routines
    desktop.see('On request')
    desktop.see('Every 60 minutes')
    buttons = desktop.tree.select { |node| node['role'] == 'AXButton' && desktop.label(node) == 'Same title' }
    expect(buttons.length).to eq(2)
    desktop.click_node(buttons.last)
    click_button('Manage Same title')
    click_button('Pause schedule through chat')
    desktop.value('Message ChiefSapi', "Pause Scheduled Routine\n@#{interval['id']}")
  end
  it 'recovers the same accepted save after switching corpora while its response is delayed', unless: Gem.win_platform? do
    @host = desktop_host
    original_name = host.state['corpora']['name']
    host.api('POST', '/api/host/local', {name: 'Other Studio'}, status: 201)
    requested, release = Queue.new, Queue.new
    requests = []
    proxy = HostProxy.new(host) do |request, response|
      if request.request_method == 'POST' && request.path.end_with?('/routines')
        requests << JSON.parse(request.body)
        if requests.length == 1
          requested << true
          Timeout.timeout(30) { release.pop }
        end
      end
      response
    end
    start_desktop(proxy.url)
    open_routines
    click_button('+ New routine')
    desktop.fill('Name (up to 60 characters)', 'One saved routine')
    desktop.fill('What to do', 'Compare the products.')
    desktop.fill('Completion criteria (one per line)', 'Return a sourced comparison.')
    click_button('Create routine')
    Timeout.timeout(10) { requested.pop }
    desktop.click('Local')
    desktop.click('Other Studio')
    desktop.click('Close corpora menu')
    desktop.find('Other Studio', role: 'AXHeading')
    release << true
    desktop.click('Local')
    desktop.click(original_name)
    desktop.click('Close corpora menu')
    open_routines
    desktop.see('The connection changed while saving.')
    click_button('Create routine')
    desktop.see('No results yet')
    expect(requests.length).to eq(2)
    expect(requests.first).to eq(requests.last)
    expect(host.state['personal_routines'].map { |row| row['title'] }).to eq(['One saved routine'])
  ensure
    release << true if release
    proxy&.close
  end

  it 'opens the existing chat composer when a run needs the user’s reply' do
    @host = desktop_host
    owner = host.state.fetch('main_agent_id')
    host.configure(rules: {
      "#{owner}:Execution" => {response: decision('OutsideSpecialization')},
      'ChiefTriage' => {response: decision('RequestUnclear', question: 'Which products?')}
    })
    start_desktop
    open_routines
    create_routine
    click_button('Run now')
    click_button('Run now')
    desktop.see('Which products?')
    click_button('Reply to ChiefSapi')
    desktop.fill('Message ChiefSapi', 'Products A and B')
    desktop.value('Message ChiefSapi', 'Products A and B')
  end

end
