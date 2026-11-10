RSpec.describe 'Setup rendering before account status arrives' do
  it 'shows onboarding only after setup is known to be incomplete' do
    output, error, status = Contract.capture('node', 'spec/support/setup_render.mjs')
    expect(status).to eq(0), output + error
  end
end
