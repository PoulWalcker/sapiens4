RSpec.describe 'Initial storage selection' do
  it 'opens the picker when selected and applies the retained path only on Continue' do
    output, error, status = Contract.capture('node', 'spec/support/bootstrap_storage.mjs')
    expect(status).to eq(0), output + error
  end
end
