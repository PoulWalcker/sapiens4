require_relative 'support/desktop'
require_relative 'support/http_peer'

RSpec.describe 'Linux native idle-tab protocol', :desktop do
  before do
    skip 'Linux WebKit contract' unless Desktop.linux?
    @directory = Dir.mktmpdir('native-idle-tabs-')
    @commands, @acks, @events = Queue.new, Queue.new, Queue.new
    @serial = 0
    @shell = HTTPPeer.new do |request|
      case request.path
      when '/command' then [200, @commands.pop(timeout: 0)]
      when '/ack' then @acks << request.body.to_i; [200, {}]
      when '/event' then @events << JSON.parse(request.body); [200, {}]
      else [200, File.read(File.join(__dir__, 'support/native_tab_bridge.html')), {'Content-Type' => 'text/html'}]
      end
    end
    @text = File.join(@directory, 'report café.txt')
    @form = File.join(@directory, 'draft.html')
    File.write(@text, 'Original report')
    File.write(@form, '<title>Draft</title><label>Working draft<input aria-label="Working draft"></label>')
    @tabs = [tab('text', @text), tab('form', @form)]
    @desktop = Desktop.new(@shell.url + '/workspace/')
  end

  after do
    @desktop&.close
    @desktop = nil
    @shell&.close
    FileUtils.remove_entry(@directory) if @directory
  end

  def tab(id, path)
    {id: id, path: path, url: 'file://' + URI::DEFAULT_PARSER.escape(path), zoom: 1,
      command: {seq: id + '-one', action: 'navigate'}}
  end

  def send_message(message)
    @serial += 1
    @commands << {serial: @serial, message: message}
    expect(Timeout.timeout(5) { @acks.pop }).to eq(@serial)
  end

  def show(id)
    send_message(workspaces: {chief: {tabs: @tabs}}, owner: 'chief', active: id, visible: true,
      rect: {x: 0, y: 0, width: 1000, height: 700})
  end

  def expire(id, **fields)
    spec = @tabs.find { |t| t[:id] == id }
    send_message(unloadIdle: {owner: 'chief', id: id, path: spec[:path], seq: spec[:command][:seq]}.merge(fields))
  end

  def draft
    desktop.find(/\AWorking draft(?: |$)/, role: 'AXTextField')['value']
  end

  it 'reloads only expired hidden text and preserves the active unsaved form' do
    show('text')
    desktop.see('Original report')
    show('form')
    desktop.fill(/\AWorking draft(?: |$)/, 'Keep my unsaved work')
    File.write(@text, 'Revised report')
    expire('text')
    expect(draft).to eq('Keep my unsaved work')
    show('text')
    desktop.see('Revised report')
    expire('form') # HTML must retain unsaved state even while hidden.
    show('form')
    expect(draft).to eq('Keep my unsaved work')
  end

  it 'ignores stale deadlines, visible-tab expiry, and unknown messages' do
    show('text')
    desktop.see('Original report')
    File.write(@text, 'Revised report')
    expire('text')
    send_message(unknown: true)
    show('form')
    desktop.see('Working draft')
    expire('text', seq: 'obsolete')
    expire('text', path: @form)
    show('text')
    desktop.see('Original report')
    expect(desktop.text).not_to include('Revised report')
  end

  it 'loads a restored background tab only when selected, including a saved reload command' do
    @tabs[1][:command][:action] = 'reload'
    show('text')
    desktop.see('Original report')
    events = []
    events << @events.pop until @events.empty?
    expect(events.none? { |event| event['id'] == 'form' }).to eq(true)
    show('form')
    desktop.see('Working draft')
  end
end
