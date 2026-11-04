require 'tmpdir'
require_relative 'support/file_picker'

RSpec.describe 'Attaching files from the desktop picker', :desktop do
  PIXEL_PNG = 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=='.unpack1('m0')

  around do |example|
    Dir.mktmpdir('sapiens-attach') { |dir| @files = File.realpath(dir); example.run }
  end

  before do
    skip 'Native open panel contract is macOS-only' unless RUBY_PLATFORM.include?('darwin')
    @host = desktop_host
    start_desktop
  end

  def open_picker(kind)
    desktop.press('Add attachment')
    desktop.press(kind)
    desktop.find('Open', role: 'AXButton')
  end

  def draft(name)
    Contract.wait(description: "saved draft #{name}") do
      host.state.fetch('attachment_drafts', {}).fetch(chief, []).find { |item| item['name'] == name }
    end
  end

  {
    'an image' => {kind: 'Image', name: 'pixel.png', bytes: PIXEL_PNG, stored: 'image'},
    'a document' => {kind: 'Document', name: 'notes.txt', bytes: "Total: 42\n", stored: 'document'}
  }.each do |label, file|
    it "attaches #{label} chosen in the native open panel" do
      path = File.join(@files, file[:name])
      File.binwrite(path, file[:bytes])
      open_picker(file[:kind])
      choose_file(path)
      desktop.see(file[:name])
      expect(draft(file[:name])).to include('kind' => file[:stored], 'size' => file[:bytes].bytesize)
    end
  end

  it 'keeps the composer usable after the open panel is cancelled' do
    open_picker('Image')
    desktop.press('Cancel')
    desktop.press('Close dialog')
    desktop.fill('Message ChiefSapi', 'Still here')
    desktop.press('Send message')
    desktop.see('Contract test reply')
    expect(host.state.fetch('attachment_drafts', {}).fetch(chief, [])).to be_empty
  end

  it 'rejects an oversized file and allows a subsequent attachment' do
    large = File.join(@files, 'large.txt')
    File.binwrite(large, 'x' * (10 * 1024 * 1024 + 1))
    open_picker('Document')
    choose_file(large)
    desktop.see('File is too large.')
    expect(host.state.fetch('attachment_drafts', {}).fetch(chief, [])).to be_empty
    small = File.join(@files, 'small.txt')
    File.write(small, 'Retry')
    desktop.press('Document')
    choose_file(small)
    desktop.see('small.txt')
    expect(draft('small.txt')).to include('size' => 5)
  end
end
