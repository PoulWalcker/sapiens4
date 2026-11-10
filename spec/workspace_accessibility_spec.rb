require_relative 'support/desktop'
require_relative 'support/http_peer'

RSpec.describe 'Working with browser tabs and documents', :desktop do
  # WebKitGTK may append an editor's value to its native accessible name.
  let(:first_input) { Desktop.linux? ? /\AFirst input(?: |$)/ : 'First input' }
  before do
    @pages = HTTPPeer.new do |request|
      name = request.path.include?('second') ? 'Second' : 'First'
      playing = request.path.end_with?('/playing')
      [200, "<title>#{name} player</title><h1>#{name} player</h1><a href='/#{name.downcase}/#{playing ? 'stopped' : 'playing'}'>#{playing ? 'Pause' : 'Play'} #{name}</a><label>#{name} input<input aria-label='#{name} input'></label>",
        {'Content-Type' => 'text/html', 'X-Frame-Options' => 'DENY', 'Content-Security-Policy' => "frame-ancestors 'none'"}]
    end
    @host = desktop_host
    host.control(chief, 'workspace_open', url: @pages.url + '/first')
    # Restore two tabs, with the second one in the background.
    if RSpec.current_example.metadata[:lazy_tabs]
      first = host.state.dig('preferences', 'workspaces', chief, 'activeTab')
      host.control(chief, 'workspace_open', url: @pages.url + '/second')
      host.control(chief, 'workspace_focus', id: first)
    end
    start_desktop
    desktop.see('Play First')
  end
  after { @pages&.close }

  it 'loads restored background tabs only on selection and retains visited pages', :lazy_tabs do
    expect(@pages.requests.map { |r| r[:path] }).not_to include('/second')
    desktop.fill(first_input, 'Keep this page')
    second = host.state.dig('preferences', 'workspaces', chief, 'tabs').find { |t| t['url'].end_with?('/second') }
    host.control(chief, 'workspace_focus', id: second.fetch('id'))
    desktop.see('Play Second')
    desktop.press('First player')
    expect(desktop.find(first_input, role: 'AXTextField')['value']).to eq('Keep this page')
    expect(@pages.requests.count { |r| r[:path] == '/first' }).to eq(1)
  end

  it 'opens a site that forbids framing and follows its links' do
    desktop.click('Play First')
    expect(desktop.see('Pause First')).to include('First player')
  end

  it 'restores guest keyboard focus when returning from another Windows window' do
    skip 'Windows activation contract' unless Gem.win_platform?
    desktop.fill(first_input, 'Website draft')
    other = desktop_host
    desktop.control('type', '--text', ' continues')
    expect(desktop.value(first_input, 'Website draft continues')).to eq('Website draft continues')
  ensure
    other&.close
  end

  it 'lets the person add a tab, navigate, and close it to return to the previous page' do
    desktop.press('Add workspace tab')
    desktop.fill('URL or file path', @pages.url + '/second')
    desktop.press('Open address')
    desktop.see('Play Second')
    expect(desktop.text).not_to include('Play First')
    desktop.press('Close Second player')
    expect(desktop.see('Play First')).not_to include('Play Second')
  end

  it 'restores the guest after workspace menus and history-filter selection' do
    desktop.fill(first_input, 'A draft on the website')
    desktop.press('Workspace options')
    desktop.see('Open file…')
    expect(desktop.text).not_to include('Play First')
    desktop.press('Workspace options')
    expect(desktop.find(first_input, role: 'AXTextField')['value']).to eq('A draft on the website')
    identity = host.api('GET', '/api/corpora')
    host.api('PUT', '/api/corpora/lifecycle', action: 'pause', operation: 'history-visibility', revision: identity['revision'])
    desktop.see('Paused')
    desktop.press('Work menu')
    desktop.press('Tasks')
    desktop.press('Task history')
    expect(desktop.text).not_to include('Play First')
    desktop.press('Executed')
    expect(desktop.find(first_input, role: 'AXTextField')['value']).to eq('A draft on the website')
    desktop.click('Play First')
    desktop.see('Pause First')
  end

  it 'opens a new address from the keyboard while the workspace menu is open' do
    desktop.click('Workspace options')
    desktop.see('Open file…')
    desktop.key(Gem.win_platform? || Desktop.linux? ? 'control+l' : 'command+l')
    desktop.fill('URL or file path', @pages.url + '/second')
    desktop.press('Open address')
    expect(desktop.see('Play Second')).not_to include('Play First')
  end

  it 'preserves a bookmark after reload and opens it after closing the tab' do
    desktop.press('Bookmark First player')
    desktop.see('Unbookmark First player')
    desktop.reload
    desktop.see('Unbookmark First player')
    desktop.press('Close First player')
    desktop.press('Show workspace')
    desktop.press('Workspace options')
    desktop.press('★ First player')
    desktop.see('Play First')
    expect(desktop.see('Unbookmark First player')).to include('Play First')
  end

  it 'reads a text file literally and reloads changes from disk' do
    file = File.join(host.workspace(chief), 'research.txt')
    File.write(file, 'A literal <b>research finding</b>')
    desktop.press('Add workspace tab')
    address = Desktop.linux? ? 'file://' + URI::DEFAULT_PARSER.escape(file) : file
    desktop.fill('URL or file path', address)
    desktop.press('Open address')
    desktop.see('A literal <b>research finding</b>')
    File.write(file, 'Revised research finding')
    desktop.press('Workspace options')
    desktop.press('Reload')
    expect(desktop.see('Revised research finding')).not_to include('A literal <b>research finding</b>')
  end

  # Only this example intentionally moves the window immediately before a click.
  def click_after_resize(text)
    node = desktop.find(text, role: 'AXLink')
    previous = nil
    Contract.wait(description: "stable click target #{desktop.label(node).inspect}") do
      snapshot = desktop.tree
      current = snapshot.find { |item| item['path'] == node['path'] }
      unless current && current['role'] == node['role'] && desktop.label(current) == desktop.label(node)
        raise "Click target changed before input: #{desktop.label(node).inspect}"
      end
      windows = snapshot.select { |item| item['role'] == 'AXWindow' }
      geometry = [current.values_at('position', 'size'), windows.map { |w| w.values_at('position', 'size') }]
      # Resizing is asynchronous. Observe stable native bounds before input.
      if geometry != previous
        previous = geometry
        next false
      end
      raise 'Cannot click a disabled or empty control' if current['enabled'] == false ||
        current.fetch('size')['width'] <= 0 || current['size']['height'] <= 0
      x = current.fetch('position')['x'] + current.fetch('size')['width'] / 2
      y = current['position']['y'] + current['size']['height'] / 2
      inside = windows.any? do |window|
        origin, size = window.values_at('position', 'size')
        x >= origin['x'] && x < origin['x'] + size['width'] &&
          y >= origin['y'] && y < origin['y'] + size['height']
      end
      raise "Cannot click #{desktop.label(current).inspect}: target at (#{x}, #{y}) is outside the fixture window #{windows.map { |w| w.slice('position', 'size') }}" unless inside
      begin
        desktop.control('click', '--x', x.to_i.to_s, '--y', y.to_i.to_s)
        true
      rescue RuntimeError => error
        # Verified in blindly4/macos/src/native.rs: this exact rejection precedes
        # event creation and injection. Never retry a successful click or any
        # error whose input outcome is uncertain; retain every driver guard.
        prefix = 'Blindly4 click: '
        refusal = if error.message.start_with?(prefix)
          begin
            JSON.parse(error.message.delete_prefix(prefix))
          rescue JSON::ParserError
            nil
          end
        end
        raise unless RUBY_PLATFORM.include?('darwin') && refusal.is_a?(Hash) &&
          refusal['code'] == 'accessibility_error' && refusal['error'] == 'Click point belongs to another process'
        previous = nil
        false
      end
    end
  end

  it 'keeps guest controls operable after resizing the desktop window' do
    @desktop.process.input.puts(JSON.generate(width: 1100, height: 740))
    click_after_resize('Play First')
    expect(desktop.see('Pause First')).to include('First player')
    @desktop.process.input.puts(JSON.generate(width: 1300, height: 900, top: 44))
    click_after_resize('Pause First')
    expect(desktop.see('Play First')).to include('First player')
  end
end
