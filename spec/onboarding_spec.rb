RSpec.describe 'One-time local setup and external Codex execution' do
  after { @host&.close }

  def setup(action = nil, status: 200)
    action ? host.api('POST', '/api/host/onboarding', {action: action}, status: status) : host.api('GET', '/api/host/onboarding')
  end

  it 'runs external Codex with inherited credentials before setup, ignores old account holds, and restores active Corpora' do
    @host = Host.new(start: false)
    config = host.environment.fetch('SAPIENS_CONFIG_DIR')
    File.delete(File.join(config, 'onboarding.json'))
    Contract.atomic_json(File.join(config, 'account-state.json'), hold: 'uncertain')
    external = File.join(config, 'external-codex')
    FileUtils.mkdir_p(external)
    environment = {'CODEX_HOME' => external, 'OPENAI_API_KEY' => 'fixture-openai', 'CODEX_API_KEY' => 'fixture-codex'}
    host.environment.merge!(environment)
    # Exercise installation discovery as well as the explicit override contract.
    unless Gem.win_platform?
      host.environment['SAPIENS_CODEX_BINARY'] = nil
      host.environment['PATH'] = host.root + File::PATH_SEPARATOR + ENV.fetch('PATH')
    end
    host.configure(capture_environment: true, version: 'codex-cli 999.0.0')
    host.start
    expect(setup).to include('step' => 'permissions', 'completed' => false)
    turn = host.submit(chief, 'Run while setup is incomplete')
    expect(host.turn(turn)['output']).to eq('Contract test reply')
    expect(host.calls.last.fetch('environment')).to eq(environment)
    expect(host.calls.last['args']).not_to include('forced_login_method="chatgpt"', 'cli_auth_credentials_store="file"')
    other = host.api('POST', '/api/host/local', {name: 'Restored Studio'}, status: 201)
    route = "/api/host/corpora/#{other.fetch('id')}"
    host.api('PUT', route + '/api/corpora/lifecycle', action: 'activate', revision: other['revision'], operation: SecureRandom.uuid)
    host.restart
    expect(setup['completed']).to be(false)
    expect(host.api('GET', '/api/host')['corpora']).to include(hash_including('id' => other['id'], 'status' => 'active'))
    state = host.api('GET', route + '/api/state')
    expect(state['pulse']['running']).to be(true)
    id = host.api('POST', route + "/api/agents/#{state['main_agent_id']}/messages", {text: 'Work in restored Corpora'}, status: 202)['id']
    Contract.wait(description: 'restored Corpora executes before setup completion') do
      host.api('GET', route + '/api/state')['turns'].any? { |row| row['id'] == id && row['status'] == 'done' }
    end
    expect(File.exist?(File.join(config, 'codex'))).to be(false)
  end

  it 'resumes committed stages, rejects obsolete actions, and commits completion only after a successful save' do
    @host = Host.new
    path = File.join(host.environment.fetch('SAPIENS_CONFIG_DIR'), 'onboarding.json')
    # Completed earlier D3 installations keep completion without a Corpora choice.
    expect(setup).to include('step' => 'complete', 'completed' => true)
    host.stop
    Contract.atomic_json(path, schema: 2, step: 'permissions')
    host.start
    expect(setup('finish', status: 409)['error']).to include('Computer access')
    expect(setup('login', status: 400)['error']).to include('Unknown setup action')
    expect(setup('permissions-done')).to include('step' => 'ready', 'completed' => false)
    host.restart
    expect(setup['step']).to eq('ready')
    expect(setup('back')['step']).to eq('permissions')
    host.restart
    expect(setup['step']).to eq('permissions')
    setup('permissions-done')
    saved = File.binread(path)
    File.rename(path, path + '.saved')
    Dir.mkdir(path)
    begin
      expect(setup('finish', status: 503)['error']).to include('Could not save setup')
      expect(setup['step']).to eq('ready')
      expect(File.binread(path + '.saved')).to eq(saved)
    ensure
      Dir.rmdir(path)
      File.rename(path + '.saved', path)
    end
    expect(setup('finish')).to include('step' => 'complete', 'completed' => true)
    expect(JSON.parse(File.read(path))).to eq('schema' => 2, 'step' => 'complete')
    host.restart
    expect(setup('finish')['completed']).to be(true)
    expect(setup('back', status: 409)['error']).to include('cannot move back')
  end
end
