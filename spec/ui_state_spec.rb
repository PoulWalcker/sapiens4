require_relative 'support/desktop'
require_relative 'support/proxy'

RSpec.describe 'Navigating work', :desktop do
  def completed_task
    @host = desktop_host
    host.configure(mode: 'Exec')
    host.submit(chief, 'Compare the products.')
    host.idle
  end

  def work_view(name)
    desktop.press('Work menu')
    desktop.press(name)
  end

  it 'retains the chosen appearance and an unsent message after reloading' do
    start_desktop
    label = desktop.label(desktop.find(/Switch to (dark|light) theme/, role: 'AXButton'))
    action = label[/Switch to (?:dark|light) theme/]
    changed = action.include?('dark') ? 'Switch to light theme' : 'Switch to dark theme'
    desktop.fill('Message ChiefSapi', 'Draft with a saved appearance')
    desktop.press(action)
    desktop.see(changed)
    desktop.reload
    desktop.see(changed)
    expect(desktop.find('Message ChiefSapi')['value']).to eq('Draft with a saved appearance')
  end

  it 'retains the conversation and unsent draft across a malformed snapshot and recovers' do
    @host = desktop_host
    malformed = false
    proxy = HostProxy.new(host) do |request, response|
      if malformed && request.path.end_with?('/api/state')
        [response[0], response[1].merge('agents' => 'invalid agent collection'), response[2]]
      else
        response
      end
    end
    start_desktop(proxy.url)
    desktop.fill('Message ChiefSapi', 'Keep this draft during recovery')
    malformed = true
    desktop.see('Invalid snapshot response: agents: expected array')
    expect(desktop.find('Message ChiefSapi')['value']).to eq('Keep this draft during recovery')
    malformed = false
    desktop.see('Active')
    expect(desktop.find('Message ChiefSapi')['value']).to eq('Keep this draft during recovery')
  ensure
    proxy&.close
  end

  it 'lets a person send a message and read the reply' do
    start_desktop
    desktop.fill('Message ChiefSapi', 'Hello from the workspace')
    desktop.press('Send message')
    expect(desktop.see('Contract test reply')).to include('Hello from the workspace')
    expect(host.state['turns'].map { |turn| turn['input'] }).to eq(['Hello from the workspace'])
  end

  it 'restores the chosen task history after reloading the workspace' do
    completed_task
    start_desktop
    desktop.press('Work')
    desktop.see('Compare the products.')
    desktop.reload
    expect(desktop.see('Compare the products.')).to include('Executed')
    desktop.press('Task history')
    desktop.press('Planned')
    expect(desktop.see('No planned tasks')).to include('Planned tasks')
  end

  it 'shows the shared memo and returns to task history through the Work menu' do
    completed_task
    File.write(File.join(host.workspace(chief), 'Notes.html'),
      '<section id="about">Research notebook</section><section id="map"></section><section id="content"><p>Remember the budget.</p></section>')
    start_desktop
    work_view('Memo')
    expect(desktop.see('Remember the budget.')).to include('Research notebook')
    desktop.press('Work')
    expect(desktop.see('Compare the products.')).to include('Executed')
  end

  it 'shows a pending task result after the person leaves and returns' do
    completed_task
    requested, release = Queue.new, Queue.new
    proxy = HostProxy.new(host) do |request, response|
      if request.path.end_with?('/tasks')
        requested << true
        Timeout.timeout(15) { release.pop }
      end
      response
    end
    start_desktop(proxy.url)
    desktop.press('Work')
    Timeout.timeout(10) { requested.pop }
    desktop.see('Loading tasks')
    work_view('Memo')
    desktop.press('Work')
    release << true
    expect(desktop.see('Compare the products.')).to include('Completed')
  ensure
    release << true if release
    proxy&.close
  end

  it 'keeps the new corpora’s task list when the previous corpora replies late' do
    completed_task
    host.api('POST', '/api/host/local', {name: 'Studio'}, status: 201)
    requested, release, returned = Queue.new, Queue.new, Queue.new
    old_route = "/api/host/corpora/#{host.state['corpora']['id']}/"
    proxy = HostProxy.new(host) do |request, response|
      if request.path.start_with?(old_route) && request.path.end_with?('/tasks')
        requested << true
        Timeout.timeout(15) { release.pop }
        returned << true
      end
      response
    end
    start_desktop(proxy.url)
    desktop.press('Work')
    Timeout.timeout(10) { requested.pop }
    desktop.press('Local')
    desktop.press('Studio')
    desktop.press('Close corpora menu')
    work_view('Tasks')
    desktop.see('No planned tasks')
    release << true
    Timeout.timeout(10) { returned.pop }
    desktop.press('Task history')
    desktop.press('Executed')
    expect(desktop.see('No executed tasks')).not_to include('Compare the products.')
  ensure
    release << true if release
    proxy&.close
  end

  [:unavailable, :malformed].each do |failure|
    it "lets the person retry #{failure} task history without reopening the app" do
      completed_task
      fail_next = true
      proxy = HostProxy.new(host) do |request, response|
        if request.path.end_with?('/tasks') && fail_next
          fail_next = false
          [failure == :unavailable ? 503 : 200, failure == :unavailable ? {error: 'Unavailable'} : {tasks: 'invalid'}, {'Content-Type' => 'application/json'}]
        else
          response
        end
      end
      start_desktop(proxy.url)
      desktop.press('Work')
      desktop.see('Could not load tasks.')
      desktop.press('Try again')
      expect(desktop.see('Compare the products.')).to include('Completed')
    ensure
      proxy&.close
    end
  end

  it 'keeps an unsent draft when switching people and narrows the roster by search' do
    @host = desktop_host
    host.agent('Researcher')
    start_desktop
    desktop.fill('Message ChiefSapi', 'Keep my draft')
    desktop.press('Open Researcher')
    desktop.fill('Message Researcher', 'Another draft')
    desktop.press('Open ChiefSapi')
    expect(desktop.find('Message ChiefSapi')['value']).to eq('Keep my draft')
    desktop.fill('Search agents', 'Researcher')
    expect(desktop.find('Open Researcher')).not_to be_nil
    expect(desktop.text).not_to include('Open ChiefSapi')
  end

  it 'keeps the next draft when the previous send is still being confirmed' do
    @host = desktop_host
    host.agent('Researcher')
    requested, release = Queue.new, Queue.new
    proxy = HostProxy.new(host) do |request, response|
      if request.request_method == 'POST' && request.path.end_with?('/messages')
        requested << true
        Timeout.timeout(15) { release.pop }
      end
      response
    end
    start_desktop(proxy.url)
    desktop.fill('Message ChiefSapi', 'First request')
    desktop.press('Send message')
    Timeout.timeout(10) { requested.pop }
    desktop.fill('Message ChiefSapi', 'Keep this next request')
    desktop.press('Open Researcher')
    release << true
    host.idle
    desktop.press('Open ChiefSapi')
    desktop.see('Contract test reply')
    expect(desktop.find('Message ChiefSapi')['value']).to eq('Keep this next request')
    expect(host.state['turns'].map { |turn| turn['input'] }).to eq(['First request'])
  ensure
    release << true if release
    proxy&.close
  end

  it 'keeps task history visible when a Memo finishes loading after navigation' do
    completed_task
    File.write(File.join(host.workspace(chief), 'Notes.html'),
      '<section id="about">Notebook</section><section id="map"></section><section id="content"><p>Late Memo content.</p></section>')
    requested, release, returned = Queue.new, Queue.new, Queue.new
    proxy = HostProxy.new(host) do |request, response|
      if request.path.end_with?('/notes')
        requested << true
        Timeout.timeout(15) { release.pop }
        returned << true
      end
      response
    end
    start_desktop(proxy.url)
    work_view('Memo')
    Timeout.timeout(10) { requested.pop }
    desktop.press('Work')
    desktop.see('Compare the products.')
    release << true
    Timeout.timeout(10) { returned.pop }
    expect(desktop.see('Compare the products.')).not_to include('Late Memo content.')
    work_view('Memo')
    expect(desktop.see('Late Memo content.')).to include('Notebook')
  ensure
    release << true if release
    proxy&.close
  end

  it 'lets the person retry a Memo that was temporarily unavailable' do
    completed_task
    File.write(File.join(host.workspace(chief), 'Notes.html'),
      '<section id="about">Notebook</section><section id="map"></section><section id="content"><p>Recovered Memo.</p></section>')
    fail_next = true
    proxy = HostProxy.new(host) do |request, response|
      if request.path.end_with?('/notes') && fail_next
        fail_next = false
        [503, {error: 'Memo temporarily unavailable'}, {'Content-Type' => 'application/json'}]
      else
        response
      end
    end
    start_desktop(proxy.url)
    work_view('Memo')
    desktop.see('Memo temporarily unavailable')
    desktop.press('Try again')
    expect(desktop.see('Recovered Memo.')).to include('Notebook')
  ensure
    proxy&.close
  end

  it 'keeps a person’s workspace available after visiting somebody with no tabs' do
    @host = desktop_host
    owner = host.agent('Researcher')
    file = File.join(host.workspace(owner), 'research.txt')
    File.write(file, 'Research document remains available')
    host.control(owner, 'workspace_open', path: file)
    start_desktop
    desktop.press('Open Researcher')
    desktop.see('Research document remains available')
    desktop.press('Open ChiefSapi')
    desktop.press('Open Researcher')
    expect(desktop.see('Research document remains available')).to include('research.txt')
  end
end
