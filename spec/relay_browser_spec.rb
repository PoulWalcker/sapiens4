require_relative 'support/file_picker'

RSpec.describe 'Managing relay connections', :desktop do
  def catalogue; host.api('GET', '/api/host/relays'); end

  it 'preserves connection text when import is cancelled and imports a selected JSON file' do
    skip 'Native open panel requires macOS or Linux' unless RUBY_PLATFORM.include?('darwin') || Desktop.linux?
    start_desktop
    desktop.press('Local')
    desktop.press('Remote Control Relay Servers')
    desktop.press(/Add your own relay/)
    desktop.fill('Name', 'Imported')
    desktop.fill('Connection JSON', 'Keep this draft')
    desktop.press('Import JSON')
    desktop.find('Open', role: 'AXButton')
    desktop.press('Cancel')
    desktop.value('Connection JSON', 'Keep this draft')
    Dir.mktmpdir('sapiens-relay-import') do |dir|
      path = File.join(File.realpath(dir), 'relay.json')
      File.write(path, 'x' * 4097)
      desktop.press('Import JSON')
      choose_file(path)
      desktop.see('File is too large.')
      desktop.value('Connection JSON', 'Keep this draft')
      raw = JSON.generate(format: 'sapiens-relay-connection-v1', relay: 'https://relay.example', token: 't' * 32)
      File.write(path, raw)
      desktop.press('Import JSON')
      choose_file(path)
      desktop.value('Connection JSON', raw)
      desktop.press('Save')
      desktop.see('Imported')
      expect(catalogue['servers'].map { |row| row['name'] }).to include('Imported')
    end
  end

  it 'keeps an invalid connection editable and saves a valid relay without switching to it' do
    start_desktop
    desktop.press('Local')
    desktop.press('Remote Control Relay Servers')
    desktop.press(/Add your own relay/)
    desktop.fill('Name', 'Office')
    desktop.fill('Connection JSON', 'not json')
    desktop.press('Save')
    desktop.see('valid relay connection JSON')
    expect(catalogue['servers'].map { |row| row['name'] }).not_to include('Office')
    desktop.fill('Connection JSON', JSON.generate(format: 'sapiens-relay-connection-v1', relay: 'https://relay.example', token: 't' * 32))
    desktop.press('Save')
    desktop.see('Office')
    expect(catalogue['selected']).to eq('sapiens-beta')
    desktop.press(/Office/, role: 'AXRadioButton')
    desktop.see('Switch server?')
    desktop.press('Cancel')
    expect(catalogue['selected']).to eq('sapiens-beta')
    desktop.press(/Office/, role: 'AXRadioButton')
    desktop.press('Switch & revoke all')
    selected = Contract.wait do
      state = catalogue
      state if state['servers'].find { |row| row['id'] == state['selected'] }['name'] == 'Office'
    end
    expect(selected['selected']).not_to eq('sapiens-beta')
  end
end
