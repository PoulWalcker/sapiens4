require_relative 'desktop'

module FilePickerExamples
  def choose_file(path)
    desktop.find('Open', role: 'AXButton')
    desktop.control('activate')
    desktop.key('command+shift+g')
    Contract.wait(description: 'Go to Folder field focus') do
      %w[AXTextField AXComboBox].include?(desktop.control('focused')['role'])
    rescue RuntimeError => error
      raise unless error.message.include?('"code":"focus_unavailable"')
      false
    end
    desktop.control('paste', '--text', path)
    desktop.key('return')
    desktop.press('Open')
  end
end

RSpec.configure { |config| config.include FilePickerExamples, :desktop }
