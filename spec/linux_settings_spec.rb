require_relative 'support/desktop'

RSpec.describe 'Linux computer settings', :desktop do
  before { skip 'Linux settings' unless Desktop.linux? }

  it 'shows native accessibility diagnostics and Linux permission guidance' do
    start_desktop
    desktop.click('Open shared computer')
    desktop.see('AT-SPI accessibility is available')
    expect(desktop.text).to include('Desktop control requires an X11 session', 'Wayland is not supported')
    expect(desktop.text).not_to include('macOS Accessibility', 'Windows UI Automation')
  end
end
