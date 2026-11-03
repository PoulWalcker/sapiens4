require_relative 'host'

class Desktop
  attr_reader :process

  def self.available?
    RUBY_PLATFORM.include?('darwin') || Gem.win_platform?
  end

  def self.driver
    File.expand_path(ENV.fetch('SAPIENS_TEST_BLINDLY4') {
      File.join(Contract::ROOT, 'submodules/blindly4/.build',
        Gem.win_platform? ? 'windows/blindly4.exe' : 'release/blindly4')
    })
  end

  def self.binary
    return @binary if @binary
    @binary = File.join(Contract::ROOT, '.build/bin', Gem.win_platform? ? 'sapiens-windows-probe.exe' : 'sapiens-macos')
    unless File.executable?(@binary)
      out, error, code = Contract.capture('runghc', 'build/Build.hs', timeout: 600)
      raise out + error unless code == 0
    end
    @binary
  end

  def self.cleanup
    FileUtils.remove_entry(@build) if @build && File.directory?(@build)
  end

  def initialize(url = nil, process: nil, ready: true)
    @owns_process = process.nil?
    @process = process || Contract::Child.new(self.class.binary, url)
    @navigation_events = Gem.win_platform? && ready
    Contract.wait(description: 'desktop startup') { @process.stdout.include?('READY') || @process.stdout.include?('TITLE:') } if ready
    wait_for_navigation if @navigation_events
    Contract.wait(description: 'packaged desktop window') { !tree(wait_for_window: true).empty? } unless ready
    control('activate')
  end

  def control(command, *args)
    out, error, code = Contract.capture(self.class.driver, command,
      '--pid', process.pid.to_s, *args, timeout: 15, chdir: File.dirname(self.class.driver))
    raise "Blindly4 #{command}: #{out}#{error}" unless code == 0
    JSON.parse(out)
  end

  def tree(wait_for_window: false)
    # WebView2 can invalidate a read while replacing its accessibility tree.
    # Retry only observations, never input, and still fail persistent errors.
    result = Contract.wait(description: 'stable accessibility tree') do
      begin
        control('tree', '--depth', '50', '--max-nodes', '10000')
      rescue RuntimeError => error
        transient = error.message.match?(/\ABlindly4 tree:.*"code":"accessibility_error".*0x(?:80040201|00000000)/m)
        starting = wait_for_window && error.message.include?("No visible windows for PID #{process.pid}")
        raise unless Gem.win_platform? && (transient || starting)
        false
      end
    end
    raise 'Accessibility tree truncated' if result['truncated']
    flatten = lambda do |node, path|
      [node.reject { |key, _| key == 'children' }.merge('path' => path)] + node.fetch('children', []).each_with_index.flat_map do |child, i|
        flatten.call(child, path.empty? ? i.to_s : "#{path}.#{i}")
      end
    end
    result.fetch('tree').fetch('children', []).each_with_index.flat_map do |child, index|
      child['role'] == 'AXWindow' ? flatten.call(child, index.to_s) : []
    end
  end

  def label(node)
    %w[title description value].map { |key| node[key].to_s }.reject(&:empty?).uniq.join(' ')
  end

  def find(text, role: nil)
    roles = Array(role).flat_map { |value| Gem.win_platform? && value == 'AXHeading' ? ['AXStaticText'] : [value] }
    Contract.wait(description: "accessible #{text.inspect}") do
      tree.find { |node| (!role || roles.include?(node['role'])) && (text.is_a?(Regexp) ? label(node).match?(text) :
        %w[title description value].any? { |key| node[key] == text }) }
    end
  end

  def text
    tree.map { |node| label(node) }.join("\n")
  end

  def see(text)
    Contract.wait(description: "visible #{text.inspect}") { value = self.text; value if text.is_a?(Regexp) ? value.match?(text) : value.include?(text) }
  end

  def press(text, role: %w[AXButton AXCheckBox AXRadioButton AXLink AXDisclosureTriangle AXPopUpButton])
    act_on(text, role: role) do |node|
      if Gem.win_platform? && !node.fetch('actions', []).include?('AXPress') && node.fetch('actions', []).include?('AXShowMenu')
        # Expand-only menus need a click to support both opening and closing.
        click_node(node)
      else
        guard = node['description'].to_s
        control('press', '--path', node.fetch('path'), *(!guard.empty? ? ['--expect-description', guard] : []))
      end
    end
  end

  def fill(text, value)
    editable = %w[AXTextField AXTextArea AXComboBox]
    if Gem.win_platform?
      act_on(text, role: editable) { |node| control('set-value', '--path', node.fetch('path'), '--value', value) }
    else
      # Clear through keyboard editing so WebKit updates controlled form state.
      # AXValue writes alone can be reverted by the next render.
      node = find(text, role: editable)
      click_node(node)
      node = find(text, role: editable)
      unless node['value'].to_s.empty?
        previous = node.fetch('value')
        # Observe readiness before destructive input; never retry a rejected key.
        Contract.wait(description: "focused unchanged draft #{text.inspect}") do
          current = find(text, role: editable)
          out, error, code = Contract.capture(self.class.driver, 'focused', timeout: 15,
            chdir: File.dirname(self.class.driver))
          raise "Blindly4 focused: #{out}#{error}" unless code == 0
          focused = JSON.parse(out)
          current['enabled'] != false && current['focused'] == true &&
            current['pid'] == process.pid && current['value'] == previous &&
            focused['pid'] == process.pid && editable.include?(focused['role']) &&
            focused['value'] == previous && %w[title description].any? do |key|
              !current[key].to_s.empty? && focused[key] == current[key]
            end
        end
        act_on(text, role: editable) { |current| control('key', '--key', 'command+a', '--target-path', current.fetch('path'), '--require-value', previous) }
        act_on(text, role: editable) { |current| control('key', '--key', 'backspace', '--target-path', current.fetch('path'), '--require-value', previous) }
        self.value(text, '')
      end
      node = find(text, role: editable)
      control('paste', '--target-path', node.fetch('path'), '--text', value)
    end
    self.value(text, value)
    # UIA SetValue can blur WebView2; establish keyboard focus after it completes.
    target = find(text, role: editable)
    click_node(target)
    Contract.wait(description: "focused #{text.inspect}") do
      node = control('focused')
      editable.include?(node['role']) && %w[title description].any? { |key| !target[key].to_s.empty? && node[key] == target[key] }
    rescue RuntimeError => error
      raise unless error.message.start_with?('Blindly4 focused: ') && error.message.include?('"code":"focus_unavailable"')
      false
    end
    value
  end

  # A loading page may replace an accessible element between discovery and action.
  # Retry only lookup or writable-control validation failures before any input.
  def act_on(text, role: nil)
    Contract.wait(description: "stable accessible #{text}") do
      begin
        yield find(text, role: role)
      rescue RuntimeError => error
        raise unless error.message.match?(/Path .* (does not|no longer) exist|press target does not match --expect-description|\ABlindly4 set-value: Error: Target is not a writable text control\s*\z|\ABlindly4 set-value: \{"code":"usage_error","error":"Target is not a writable UI Automation text control"\}\s*\z/)
        false
      end
    end
  end

  def click(text)
    click_node(find(text))
  end

  def click_node(node)
    raise 'Cannot click a disabled or empty control' if node['enabled'] == false ||
      node.fetch('size')['width'] <= 0 || node['size']['height'] <= 0
    x = node.fetch('position')['x'] + node.fetch('size')['width'] / 2
    y = node['position']['y'] + node['size']['height'] / 2
    windows = tree.select { |item| item['role'] == 'AXWindow' }
    inside = windows.any? do |window|
      origin, size = window.values_at('position', 'size')
      x >= origin['x'] && x < origin['x'] + size['width'] &&
        y >= origin['y'] && y < origin['y'] + size['height']
    end
    raise "Cannot click #{label(node).inspect}: target at (#{x}, #{y}) is outside the fixture window #{windows.map { |w| w.slice('position', 'size') }}" unless inside
    control('click', '--x', x.to_i.to_s, '--y', y.to_i.to_s)
  end

  def key(value)
    control('key', '--key', value)
  end

  def reload
    previous = process.stdout.scan(/^NAVIGATION START (\d+)/).last&.first
    key(Gem.win_platform? ? 'control+r' : 'command+r')
    wait_for_navigation(after: previous) if @navigation_events
  end

  def wait_for_navigation(after: nil)
    Contract.wait(description: 'completed native navigation') do
      current = process.stdout.scan(/^NAVIGATION START (\d+)/).last&.first
      next false unless current && current != after
      result = process.stdout.match(/^NAVIGATION END #{Regexp.escape(current)} (True|False) (\S+)/)
      raise "Native navigation failed: #{result[2]}" if result && result[1] == 'False'
      result
    end
  end

  def value(text, expected)
    Contract.wait(description: "#{text.inspect} value #{expected.inspect}") do
      node = find(text, role: %w[AXTextField AXTextArea AXComboBox])
      node['value'] if node['value'] == expected
    end
  end

  def close
    process.close if @owns_process
  end
end

module DesktopExamples
  def desktop; @desktop; end
  def desktop_host
    return Host.new unless Gem.win_platform?
    host = Host.new(command: [Desktop.binary, '--runtime-dir', Contract::ROOT])
    # The native host's URL is ready before WebView2 has loaded the shell.
    Desktop.new(process: host.process).see('Message ChiefSapi')
    host
  rescue Exception
    warn host.process.stdout if host&.process
    host&.close
    raise
  end
  def start_desktop(url = nil)
    @host ||= desktop_host
    skip 'Network proxy window currently requires macOS WebKit' if Gem.win_platform? && url && url != host.url
    @desktop = Gem.win_platform? ? Desktop.new(process: host.process) : Desktop.new(url || host.url)
    # The Windows host owns an already-loaded window; reload after arranging
    # service fixtures, matching the fresh page opened by the macOS driver.
    if Gem.win_platform?
      @desktop.see('Message ChiefSapi')
      @desktop.reload
    end
  end
end

RSpec.configure do |config|
  config.include DesktopExamples, :desktop
  config.before(:each, :desktop) do
    skip 'Native desktop tests require macOS or Windows' unless Desktop.available?
    raise "Desktop driver missing: #{Desktop.driver}. Build submodules/blindly4 or set SAPIENS_TEST_BLINDLY4." unless File.executable?(Desktop.driver)
  end
  config.after(:each, :desktop) do |example|
    if example.exception && @desktop
      FileUtils.mkdir_p(File.join(Contract::ROOT, 'tmp'))
      File.write(File.join(Contract::ROOT, 'tmp', 'desktop-failure.txt'), @desktop.text) rescue nil
    end
    @desktop&.close
    @host&.close
  end
  config.after(:suite) { Desktop.cleanup }
end
