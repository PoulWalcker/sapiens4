require_relative 'support/desktop'

RSpec.describe 'Workspace tab expiry clock' do
  it 'expires after one uninterrupted background hour and cancels obsolete deadlines' do
    _, error, status = Contract.capture('node', 'spec/support/idle_tab_clock.mjs')
    expect(status).to eq(0), error
  end
end

RSpec.describe 'Inactive native Workspace tabs', :desktop do
  it 'reloads expired text views across Sapis and preserves an inactive form' do
    # Opt in to the wall-clock expiry test with the timeout of the build under test.
    interval = ENV['SAPIENS_TEST_TAB_IDLE_SECONDS']
    skip 'Set SAPIENS_TEST_TAB_IDLE_SECONDS=3600 for the production build' unless interval
    @host = desktop_host
    other = host.agent('Researcher')
    file = File.join(host.workspace(chief), 'report.txt')
    form = File.join(host.workspace(other), 'draft.html')
    File.write(file, 'Original report')
    File.write(form, '<label>Working draft<input></label>')
    workspace = host.control(chief, 'workspace_open', path: file).fetch('workspace')
    tab = workspace.fetch('active_tab')
    host.control(chief, 'workspace_bookmark', id: tab)
    host.control(other, 'workspace_open', path: form)
    start_desktop
    desktop.see('Original report')
    desktop.press('Open Researcher')
    File.write(file, 'Revised report')
    desktop.press('Open ChiefSapi')
    desktop.see('Original report')  # A short visit to another Sapi keeps the same page.
    desktop.press('Open Researcher')
    desktop.fill('Working draft', 'Keep my unsaved work')
    sleep Float(interval) + 1
    saved = host.control(chief, 'workspace')
    expect(saved['active_tab']).to eq(tab)
    expect(saved['tabs'].map { |t| t['id'] }).to eq([tab])
    expect(saved['bookmarks'].map { |b| b['path'] }).to include(file)
    desktop.press('Open ChiefSapi')
    desktop.see('Revised report')
    sleep Float(interval) + 1
    desktop.press('Open Researcher')
    expect(desktop.find('Working draft', role: 'AXTextField')['value']).to eq('Keep my unsaved work')
  end
end
