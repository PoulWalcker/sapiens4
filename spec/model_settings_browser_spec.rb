require_relative 'support/desktop'

RSpec.describe 'Model settings', :desktop do
  def open_models
    desktop.click('Menu')
    desktop.press('Workspace & settings')
    desktop.press('Models')
    desktop.see('Models for this corpora and its Sapis.')
  end

  def choose(field, moves, expected)
    desktop.press(field)
    moves.abs.times { desktop.key(moves.positive? ? 'down' : 'up') }
    desktop.key('return')
    desktop.value(field, expected)
  end

  def expect_saved_and_closed
    desktop.see('Model settings saved.')
    desktop.find('Message ChiefSapi')
    expect(desktop.text).not_to include('Save and apply')
  end

  it 'sets a Sapi override on one page, runs it, then returns the Sapi to the default' do
    start_desktop
    desktop.fill('Message ChiefSapi', 'Keep my unsent message')
    open_models
    desktop.see('Changes apply to new runs. Running work is unchanged.')
    desktop.value('Default provider', 'Codex')
    desktop.value('Default model', 'GPT-6-Astra')
    desktop.value('Default effort', 'medium')
    desktop.value('ChiefSapi provider', 'Use default')
    choose('ChiefSapi provider', 2, 'Claude Code')
    desktop.value('ChiefSapi model', 'Opus 5.5')
    desktop.value('ChiefSapi effort', 'medium')
    choose('ChiefSapi effort', 1, 'high')
    desktop.press('Manage AI connections')
    desktop.see('Your AI lineup')
    desktop.press('Back to model settings')
    desktop.value('ChiefSapi effort', 'high')
    desktop.click('Save and apply')
    expect_saved_and_closed
    expect(desktop.find('Message ChiefSapi')['value']).to eq('Keep my unsent message')
    saved = host.api('GET', '/api/model-settings')
    expect(saved.fetch('overrides').fetch(chief)).to eq('provider' => 'claude', 'model' => 'claude-opus-5-5', 'effort' => 'high')
    host.turn(host.submit(chief, 'Use the Sapi override'))
    expect(host.calls).not_to be_empty
    host.calls.each { |call| expect(call.fetch('args')).to include('--model', 'claude-opus-5-5', '--effort', 'high') }

    open_models
    choose('ChiefSapi provider', -2, 'Use default')
    desktop.click('Save and apply')
    expect_saved_and_closed
    expect(host.api('GET', '/api/model-settings').fetch('overrides')).to be_empty
  end

  it 'shows CLI default for a model without efforts and keeps edits on a save conflict' do
    start_desktop
    host.api('PUT', '/api/model-settings', {revision: 0, defaults: {provider: 'claude', model: 'claude-haiku-4-5-20251001', effort: ''}, overrides: {}})
    open_models
    desktop.value('Default model', 'Haiku 4.5')
    expect(desktop.find('Default effort', role: 'AXPopUpButton')['enabled']).to eq(false)
    desktop.press('Close dialog')
    host.api('PUT', '/api/model-settings', {revision: 1, defaults: {provider: 'codex', model: 'gpt-6-astra', effort: 'medium'}, overrides: {}})
    open_models
    choose('Default effort', 1, 'high')
    original = host.api('GET', '/api/model-settings')
    host.api('PUT', '/api/model-settings', {revision: original.fetch('revision'), defaults: {provider: 'codex', model: 'gpt-6-sol', effort: 'low'}, overrides: {}})
    desktop.click('Save and apply')
    desktop.see('Model settings changed elsewhere. Close and reopen Models before saving.')
    desktop.value('Default effort', 'high')
    expect(host.api('GET', '/api/model-settings').fetch('revision')).to eq(original.fetch('revision') + 1)
    desktop.press('Close dialog')
  end
end
