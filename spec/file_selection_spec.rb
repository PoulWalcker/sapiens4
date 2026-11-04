RSpec.describe 'Typed browser file selection' do
  def pick(scenario)
    out, error, status = Contract.capture('node', 'spec/support/file_selection.mjs', scenario)
    expect(status).to eq(0), error
    result = JSON.parse(out)
    expect(result).to include('clickedWhileAttached' => true, 'attached' => false, 'removals' => 1, 'accept' => '.txt')
    result
  end

  it 'returns the selected file with a known MIME type and retains the input through reading' do
    expect(pick('selected')).to include('selected' => true, 'retainedWhileReading' => true,
      'file' => {'name' => 'résumé.txt', 'mime' => 'text/plain', 'size' => 5, 'data' => 'aGVsbG8='})
  end

  it 'represents an unknown MIME type as Nothing' do
    expect(pick('unknown-mime').fetch('file')).to include('mime' => nil, 'data' => 'aGVsbG8=')
  end

  it 'keeps an empty file distinct from cancellation' do
    expect(pick('empty')).to include('selected' => true,
      'file' => {'name' => 'résumé.txt', 'mime' => 'text/plain', 'size' => 0, 'data' => ''})
  end

  %w[cancel no-file].each do |scenario|
    it "returns Nothing for #{scenario}" do
      expect(pick(scenario)).to include('selected' => false)
    end
  end

  {
    'too-large' => 'File is too large.',
    'read-error' => 'Could not read this file.',
    'read-abort' => 'File reading was cancelled.',
    'invalid-result' => 'Could not read this file.',
    'read-throws' => 'Reader unavailable.',
    'picker-error' => 'Picker unavailable.'
  }.each do |scenario, message|
    it "reports #{scenario} as an error and removes the input" do
      expect(pick(scenario)).to include('error' => message)
    end
  end
end
