require 'sqlite3'

RSpec.describe 'Default Doctor population initialization' do
  it 'creates a separate Doctor once, resumes interrupted fresh initialization, and preserves existing Corpora with no assignment' do
    binary = ENV.fetch('SAPIENS_TEST_DOCTOR_BOOTSTRAP_HOST', Contract::BINARY)
    @host = Host.new(command: [binary])
    initial = host.state
    initial_agents = initial.fetch('agents')
    doctor = initial.fetch('corpora').fetch('doctor_id')
    expect(initial_agents.map { |row| row['name'] }).to eq(%w[ChiefSapi Doctor])
    expect(initial_agents.last).to include('id' => doctor, 'role' => 'System diagnostics and authorized recovery')
    expect(initial.fetch('main_agent_id')).to eq(initial_agents.first.fetch('id'))
    expect(initial.dig('orchestration', doctor, 'manager')).to eq(initial.fetch('main_agent_id'))
    expect(host.calls).to be_empty
    host.restart
    expect(host.state.fetch('agents')).to eq(initial_agents)
    expect(host.state.dig('corpora', 'doctor_id')).to eq(doctor)

    identity_path = File.join(host.root, 'corpora-identity.json')
    settings_path = File.join(host.root, 'corpora-settings.json')
    host.stop
    identity = JSON.parse(File.read(identity_path)).merge('initialPopulation' => 'chief_doctor')
    File.write(identity_path, JSON.generate(identity))
    settings = JSON.parse(File.read(settings_path)).merge('doctor_id' => nil)
    File.write(settings_path, JSON.generate(settings))
    host.start
    expect(host.state.fetch('agents')).to eq(initial_agents)
    expect(host.state.dig('corpora', 'doctor_id')).to eq(doctor)
    expect(JSON.parse(File.read(identity_path))).not_to have_key('initialPopulation')

    host.stop
    File.write(identity_path, JSON.generate(identity))
    File.write(settings_path, JSON.generate(settings))
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) { |db| db.execute('DELETE FROM agents WHERE id=?', doctor) }
    host.start
    resumed = host.state
    replacement = resumed.dig('corpora', 'doctor_id')
    expect(resumed.fetch('main_agent_id')).to eq(initial.fetch('main_agent_id'))
    expect(resumed.fetch('agents').map { |row| row['name'] }).to eq(%w[ChiefSapi Doctor])
    expect(replacement).not_to eq(doctor)
    expect(resumed.dig('orchestration', replacement, 'manager')).to eq(initial.fetch('main_agent_id'))
    expect(JSON.parse(File.read(identity_path))).not_to have_key('initialPopulation')
    host.restart
    expect(host.state.fetch('agents')).to eq(resumed.fetch('agents'))

    host.control(chief, 'corpora_update', revision: host.state.dig('corpora', 'revision'), doctor_id: nil)
    host.restart
    expect(host.state.dig('corpora', 'doctor_id')).to be_nil
    expect(host.state.fetch('agents')).to eq(resumed.fetch('agents'))
    host.stop
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) { |db| db.execute('DELETE FROM agents WHERE id=?', replacement) }
    host.start
    expect(host.state.fetch('agents')).to eq([initial_agents.first])
    expect(host.state.dig('corpora', 'doctor_id')).to be_nil
    expect(host.calls).to be_empty
  ensure
    @host&.close
  end
end
