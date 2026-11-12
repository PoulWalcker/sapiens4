require 'yaml'

RSpec.describe 'Model settings and providers', :host do
  def codex(model = 'gpt-6-astra', effort = 'medium')
    {provider: 'codex', model: model, effort: effort}
  end

  def claude(model = 'claude-opus-5-5', effort = 'medium')
    {provider: 'claude', model: model, effort: effort}
  end

  def save(revision, defaults, overrides = {})
    host.api('PUT', '/api/model-settings', {revision: revision, defaults: defaults, overrides: overrides})
  end

  def runs(calls, provider, model, effort)
    expect(calls).not_to be_empty
    calls.each do |call|
      args = call.fetch('args')
      expect(call.fetch('provider')).to eq(provider)
      if provider == 'codex'
        expect(args).to include("model=\"#{model}\"", "model_reasoning_effort=\"#{effort}\"", '--dangerously-bypass-approvals-and-sandbox')
      else
        expect(args[args.index('--model') + 1]).to eq(model)
        expect(args).to include('--dangerously-skip-permissions', '--no-session-persistence')
        if effort.empty?
          expect(args).not_to include('--effort')
        else
          expect(args[args.index('--effort') + 1]).to eq(effort)
        end
      end
    end
  end

  it 'runs the default and each Sapi override with its own provider, keeps them across a restart and returns a Sapi to the default' do
    agent = host.agent
    saved = save(0, claude, {agent => codex('gpt-6-luna', 'low')})
    host.restart
    expect(host.api('GET', '/api/model-settings')).to eq(saved)
    host.turn(host.submit(chief, 'Use the corpora default'))
    host.turn(host.submit(agent, 'Use my override'))
    runs(host.calls.select { |call| call['owner'] == chief }, 'claude', 'claude-opus-5-5', 'medium')
    runs(host.calls.select { |call| call['owner'] == agent }, 'codex', 'gpt-6-luna', 'low')
    count = host.calls.size
    save(1, claude('claude-haiku-4-5-20251001', ''))
    host.turn(host.submit(agent, 'Inherit the corpora default'))
    runs(host.calls.drop(count), 'claude', 'claude-haiku-4-5-20251001', '')
  end

  it 'keeps the settings a task started with for every step and audit record while new runs use the update' do
    save(0, claude)
    host.configure(mode: 'Exec', rules: {'Assessing' => {block: true}})
    first = host.submit(chief, 'Keep this run consistent')
    Contract.wait(description: 'first model invocation') { host.calls.any? }
    run = host.calls.first.fetch('agency_run')
    save(1, codex)
    host.release
    host.turn(first)
    host.idle
    calls = host.calls.select { |call| call['agency_run'] == run }
    expect(calls.map { |call| call['node'] }).to include('Assessing', 'ReadyToWork', 'Execution')
    runs(calls, 'claude', 'claude-opus-5-5', 'medium')
    records = YAML.safe_load(host.api('GET', "/api/agents/#{chief}/tasks")).fetch('tasks').flat_map { |task| task.fetch('decisions') }
    expect(records).not_to be_empty
    expect(records).to all(include('provider' => 'claude', 'model' => 'claude-opus-5-5', 'effort' => 'medium'))
    count = host.calls.size
    host.turn(host.submit(chief, 'Use the new settings'))
    runs(host.calls.drop(count), 'codex', 'gpt-6-astra', 'medium')
  end

  it 'rejects bad saves and keeps the active settings' do
    saved = save(0, codex)
    [[{revision: 0}, 409],
     [{defaults: codex('gpt-6-luna', 'ultra')}, 400],
     [{defaults: {provider: 'unknown', model: 'unsupported', effort: 'medium'}}, 400],
     [{defaults: claude('claude-haiku-4-5-20251001', 'medium')}, 400],
     [{overrides: {'missing-sapi' => codex}}, 404]].each do |changes, status|
      expect(host.request('PUT', '/api/model-settings', {revision: 1, defaults: codex, overrides: {}}.merge(changes)).first).to eq(status)
      expect(host.api('GET', '/api/model-settings')).to eq(saved)
    end
  end

  it 'fails the turn on a provider failure without falling back to another model' do
    save(0, codex('gpt-6-luna', 'low'))
    host.configure(fail: true)
    host.turn(host.submit(chief), 'failed')
    runs(host.calls, 'codex', 'gpt-6-luna', 'low')
    save(1, claude)
    {'Analyst' => {fail: false, claude_result_error: true}, 'Writer' => {claude_result_error: false, claude_missing_result: true}}.each do |name, configuration|
      count = host.calls.size
      host.configure(**configuration)
      host.turn(host.submit(host.agent(name)), 'failed')
      runs(host.calls.drop(count), 'claude', 'claude-opus-5-5', 'medium')
    end
  end

  it 'lets Claude write in the Sapi workspace and create a specialist through the decision flow' do
    save(0, claude)
    host.configure(mode: 'Exec', rules: {
      "#{chief}:Assessing" => {response: decision('OutsideSpecialization'), actions: [{write: 'Notes.html', content: '<h1>Notes updated by the provider fixture</h1>'}]},
      'ChiefTriage' => {response: decision('NewSpecialistNeeded')}})
    host.submit(chief)
    host.idle
    expect(File.read(File.join(host.workspace(chief), 'Notes.html'))).to eq('<h1>Notes updated by the provider fixture</h1>')
    expect(host.state.fetch('agents').map { |agent| agent['name'] }).to include('NewResearcher')
    expect(host.calls.first.fetch('agency_run')).not_to be_empty
    runs(host.calls, 'claude', 'claude-opus-5-5', 'medium')
  end

  it 'stops the running provider process when its turn is cancelled' do
    save(0, claude)
    host.configure(block: true)
    id = host.submit(chief)
    Contract.wait(description: 'blocked provider process') { host.calls.any? }
    pid = host.calls.first.fetch('pid')
    host.api('POST', "/api/agents/#{chief}/turns/#{id}/cancel", {})
    host.turn(id, %w[interrupted cancelled])
    Contract.wait(description: 'provider process exit') do
      Process.kill(0, pid)
      false
    rescue Errno::ESRCH
      true
    end
  end
end
