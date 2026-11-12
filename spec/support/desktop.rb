require_relative 'host'

class Desktop
  attr_reader :process

  def self.available?
    RUBY_PLATFORM.include?('darwin') || Gem.win_platform? || (linux? && ENV['SAPIENS_TEST_LINUX_DESKTOP'] == '1' && ENV['XDG_SESSION_TYPE'] == 'x11')
  end

  def self.linux?
    RUBY_PLATFORM.include?('linux')
  end

  def self.driver
    File.expand_path(ENV.fetch('SAPIENS_TEST_BLINDLY4') {
      File.join(Contract::ROOT, 'submodules/blindly4/.build',
        Gem.win_platform? ? 'windows/blindly4.exe' : 'release/blindly4')
    })
  end

  def self.binary
    return @binary if @binary
    if linux?
      @binary = File.join(Contract::ROOT, '.build/bin/sapiens-linux-browser')
      flags, error, code = Contract.capture('pkg-config', '--cflags', '--libs', 'gtk+-3.0', 'webkit2gtk-4.1', 'json-glib-1.0')
      raise error unless code == 0
      output, error, code = Contract.capture('cc', '-Wall', '-Wextra', '-Werror',
        'spec/support/LinuxBrowser.c', 'environments/linux/Desktop.c', 'environments/linux/Browser.c', '-o', @binary, *flags.split)
      raise output + error unless code == 0
      return @binary
    end
    @binary = File.join(Contract::ROOT, '.build/bin', Gem.win_platform? ? 'sapiens-windows-probe.exe' : 'sapiens-macos')
    unless File.executable?(@binary)
      out, error, code = Contract.capture('runghc', 'build/Build.hs', timeout: 600)
      raise out + error unless code == 0
    end
    @binary
  end

  def initialize(url = nil, process: nil, ready: true)
    @owns_process = process.nil?
    if self.class.linux? && !process
      @data = Dir.mktmpdir('sapiens-linux-browser-')
      address = URI(url)
      address.path = '/workspace/' if address.path.empty? || address.path == '/'
      @process = Contract::Child.new(self.class.binary, address.to_s, @data)
    else
      @process = process || Contract::Child.new(self.class.binary, url)
    end
    @navigation_events = Gem.win_platform? && ready
    Contract.wait(description: 'desktop startup') { @process.stdout.include?('READY') || @process.stdout.include?('TITLE:') } if ready
    wait_for_navigation if @navigation_events
    Contract.wait(description: 'packaged desktop window') { !tree(wait_for_window: true).empty? } unless ready
    control('activate')
  rescue Exception
    # A failed constructor never reaches @desktop, so the example cannot close it.
    # Dispose only a process created here; a borrowed host belongs to its caller.
    @process.close if @owns_process && @process
    raise
  end

  def control(command, *args)
    out, error, code = Contract.capture(self.class.driver, command,
      '--pid', process.pid.to_s, *args, timeout: 15, chdir: File.dirname(self.class.driver),
      env: self.class.linux? ? {'BLINDLY4_LINUX_TEXT_INPUT' => 'keyboard'} : {})
    raise "Blindly4 #{command}: #{out}#{error}" unless code == 0
    JSON.parse(out)
  end

  def tree(wait_for_window: false)
    # Web views can invalidate a read while replacing their accessibility trees.
    # Retry only observations, never input, and still fail persistent errors.
    result = Contract.wait(description: 'stable accessibility tree') do
      begin
        control('tree', '--depth', '50', '--max-nodes', '10000')
      rescue RuntimeError => error
        transient = error.message.match?(/\ABlindly4 tree:.*"code":"accessibility_error".*0x(?:80040201|00000000)/m)
        starting = wait_for_window && error.message.include?("No visible windows for PID #{process.pid}")
        replaced = self.class.linux? && error.message.start_with?('Blindly4 tree: ') &&
          error.message.match?(/org\.freedesktop\.DBus\.Error\.UnknownObject|Object does not exist at path|Accessible element is defunct|No such interface .(?:org\.a11y\.atspi\.Accessible|org\.freedesktop\.DBus\.Properties). on object/)
        raise unless (Gem.win_platform? && (transient || starting)) || replaced
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
      if !node.fetch('actions', []).include?('AXPress') &&
          (self.class.linux? || (Gem.win_platform? && node.fetch('actions', []).include?('AXShowMenu')))
        # WebKit radio/checkbox controls and expand-only menus may need a
        # physical click; use verified bounds and window ownership.
        click_node(node)
      else
        guard = node['description'].to_s
        control('press', '--path', node.fetch('path'), *(!guard.empty? ? ['--expect-description', guard] : []))
      end
    end
  end

  def fill(text, value)
    editable = %w[AXTextField AXTextArea AXComboBox]
    click_node(find(text, role: editable)) if self.class.linux?
    if Gem.win_platform? || self.class.linux?
      act_on(text, role: editable) { |node| control('set-value', '--path', node.fetch('path'), '--value', value) }
    else
      # Clear through keyboard editing so WebKit updates controlled form state.
      # AXValue writes alone can be reverted by the next render.
      node = find(text, role: editable)
      click_node(node)
      node = find(text, role: editable)
      unless node['value'].to_s.empty?
        previous = node.fetch('value')
        act_on(text, role: editable) { |current| control('key', '--key', 'command+a', '--target-path', current.fetch('path'), '--require-value', previous) }
        act_on(text, role: editable) { |current| control('key', '--key', 'backspace', '--target-path', current.fetch('path'), '--require-value', previous) }
        self.value(text, '')
      end
      act_on(text, role: editable) { |node| control('paste', '--target-path', node.fetch('path'), '--text', value) }
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
        raise unless error.message.match?(/Path .* (does not|no longer) exist|press target does not match --expect-description|\ABlindly4 paste: Error: Paste target is not a writable text control\s*\z|\ABlindly4 set-value: Error: Target is not a writable text control\s*\z|\ABlindly4 set-value: \{"code":"usage_error","error":"Target is not a writable UI Automation text control"\}\s*\z/)
        false
      end
    end
  end

  def click(text)
    click_node(find(text))
  end

  def click_node(node)
    raise "No verified screen coordinates: #{node.inspect}" unless node['position'] && node['size']
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
    key(Gem.win_platform? || self.class.linux? ? 'control+r' : 'command+r')
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
      node = find(text, role: %w[AXTextField AXTextArea AXComboBox AXPopUpButton])
      node['value'] if node['value'] == expected
    end
  end

  def close
    process.close if @owns_process
    FileUtils.remove_entry(@data) if @data && File.directory?(@data)
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
    skip 'Network proxy window requires the macOS or Linux WebKit fixture' if Gem.win_platform? && url && url != host.url
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
    skip 'Native desktop tests require macOS, Windows or an isolated Linux Xorg session' unless Desktop.available?
    raise "Desktop driver missing: #{Desktop.driver}. Build submodules/blindly4 or set SAPIENS_TEST_BLINDLY4." unless File.executable?(Desktop.driver)
  end
  config.after(:each, :desktop) do |example|
    if example.exception && @desktop
      warn "#{example.full_description}: #{example.exception.message}"
      FileUtils.mkdir_p(File.join(Contract::ROOT, 'tmp'))
      File.write(File.join(Contract::ROOT, 'tmp', 'desktop-failure.txt'), @desktop.text) rescue nil
      File.write(File.join(Contract::ROOT, '.build', "desktop-failure-#{example.id.gsub(/\W/, '_')}.json"), JSON.pretty_generate(@desktop.tree)) rescue nil
    end
    @desktop&.close
    @host&.close
  end
end
