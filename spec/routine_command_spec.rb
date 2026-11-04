RSpec.describe 'Routine command executable' do
  def interpret(text, attached = false)
    binary = File.join(Contract::ROOT, '.build/domain', Gem.win_platform? ? 'sapiens-domain.exe' : 'sapiens-domain')
    out, error, code = Contract.capture(binary, 'routine', attached ? '1' : '0', input: text)
    expect(code).to eq(0), error
    JSON.parse(out)
  end

  {
    'ordinary message' => ['Discuss a routine', nil],
    'case-sensitive prefix' => ["add New Scheduled Routine:\nFrequency: every 1 minute\nExecute: Work", nil],
    'multiline Unicode prompt' => ["Add New Scheduled Routine:\nFrequency: every 02 minutes\nExecute:  Résumé 中文 🧭\nSecond line  ", {'minutes' => 2, 'execution' => "Résumé 中文 🧭\nSecond line"}],
    'Unicode whitespace and CRLF' => ["Add New Scheduled Routine:\u0085\r\nFrequency: every 1 minute\r\nExecute: Work\r\nMore", {'minutes' => 1, 'execution' => "Work\r\nMore"}],
    'parentheses in edit title' => ["Edit @routine_sch_00001 ( Title ) more )\nEdit Frequency (prev every 1 minute): new every 3 minutes\nEdit Execution Prompt as: Work", {'routine_id' => 'routine_sch_00001', 'previous_minutes' => 1, 'minutes' => 3, 'execution' => 'Work'}],
    'legacy pause spelling' => ["Pause Schedule Riutine:\n@routine_sch_00001", {'routine_id' => 'routine_sch_00001', 'paused' => true}],
    'resume command' => ["Resume Scheduled Routine\n@routine_sch_00001", {'routine_id' => 'routine_sch_00001', 'paused' => false}]
  }.each do |name, (text, expected)|
    it("interprets #{name}") { expect(interpret(text)).to eq(expected) }
  end

  ["Pause Scheduled Routine\n@routine_sch_1234", "Pause Scheduled Routine\n@routine_sch_00001 extra"].each do |text|
    it("rejects malformed routine identity #{text.inspect}") do
      expect(interpret(text)).to eq('error' => 'Put the Scheduled Routine ID on the next line')
    end
  end

  ["Add New Scheduled Routine:\nFrequency: every ١ minute\nExecute: Work",
   "Add New Scheduled Routine:\nFrequency: every 1 minute\nExecute: ",
   "Add New Scheduled Routine:\n Frequency: every 1 minute\nExecute: Work"].each do |text|
    it("rejects malformed template #{text.inspect}") do
      expect(interpret(text)).to eq('error' => 'Use the Scheduled Routine template with a whole number of minutes and an execution prompt')
    end
  end

  it 'rejects attachments before interpreting a recognized template' do
    expect(interpret('Add New Scheduled Routine: broken', true)).to eq('error' => 'Put the execution instructions in the routine prompt')
    expect(interpret('Ordinary attachment message', true)).to be_nil
  end

  it 'rejects invalid invocation before reading a routine' do
    binary = File.join(Contract::ROOT, '.build/domain', Gem.win_platform? ? 'sapiens-domain.exe' : 'sapiens-domain')
    output, error, code = Contract.capture(binary, 'routine', '2', input: 'Discuss a routine')
    expect(code).to eq(1)
    expect(output).to be_empty
    expect(error).to include('Usage: sapiens-domain routine 0|1')
  end
end
