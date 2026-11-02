require 'rbconfig'
require 'open3'
require_relative 'support/desktop'
require_relative 'support/candidate_fixtures'

RSpec.describe 'Routine script activation basis in the native app', :desktop do
  include CandidateFixtures

  def click_control(label)
    roles = %w[AXButton AXCheckBox AXRadioButton AXLink AXDisclosureTriangle AXPopUpButton]
    node = desktop.find(label, role: roles)
    desktop.control('scroll-to', '--path', node.fetch('path'))
    desktop.click_node(desktop.find(label, role: roles))
  end

  it 'shows the prepared version and then its actual authorized activation basis without adding work' do
    unless RbConfig::CONFIG.fetch('host_os').include?('darwin') && RbConfig::CONFIG.fetch('host_cpu').match?(/\A(?:arm64|aarch64)\z/)
      skip 'Selected generated-code runtime currently supports macOS ARM64 only'
    end
    docker = File.expand_path('~/.orbstack/bin/docker')
    socket = 'unix://' + File.expand_path('~/.orbstack/run/docker.sock')
    output, error, status = Open3.capture3(docker, '--host', socket, 'ps', '--all', '--quiet', '--no-trunc')
    raise "Selected runtime unavailable: #{error}" unless status.success?
    baseline = output.lines.map(&:strip).sort
    @host = Host.new(start: false, environment: {'SAPIENS_GENERATED_DOCKER' => docker, 'SAPIENS_GENERATED_SOCKET' => socket})
    host.configure(version: 'codex-cli 0.162.1', rules: {
      'Execution' => {response: decision('Outcome', reply: '5000', outcome: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []})},
      'AutomationAnalysis' => {response: decision('AutomationAnalyzed', decision: {status: 'Ready', automatablePart: 'Aggregate the supplied CSV records.', parameters: [{name: 'csv', description: 'Sales records'}]})},
      'PreparationFixtures' => {response: {fixtures: independent_fixtures, reason: 'Independent expected values.'}},
      'PreparationGeneration' => {response: {language: 'python', profile: 'python-3.13.15-linux-arm64', source: candidate_source, interface: 'json-stdin-result-v1', dependencies: [], capabilities: []}},
      'PreparationReview' => {response: {accepted: true, reason: 'Independent real outputs agree.'}},
      'CodeAssessment' => {response: {verdict: 'accepted', reply: '5000', reason: 'Exact observed sum.', evidence: ['Observed actual output.'], acceptance: {status: 'Completed', satisfiedCriteria: CandidateFixtures::CRITERIA, artifacts: []}, activationAudit: {accepted: true, reason: 'Activated after the checked sales result.'}}}
    })
    host.start
    owner = host.state.fetch('main_agent_id')
    routine = host.api('POST', "/api/agents/#{owner}/routines", operation: 'create', title: 'Checked sales', goal: CandidateFixtures::GOAL, criteria: CandidateFixtures::CRITERIA).fetch('routine')
    path = "/api/agents/#{owner}/routines/#{routine}"
    original = host.api('POST', "#{path}/run", operation: 'ordinary', revision: 1, inputs: JSON.generate(observed_input)).fetch('run')
    host.turn(original)
    snapshot = lambda { host.state.fetch('personal_routines').find { |row| row['id'] == routine } }
    analysis = Contract.wait { row = snapshot.call['analysis']; row if row && row['status'] == 'Ready' }
    preparation = host.api('POST', "#{path}/prepare", operation: 'prepare', revision: 1, analysis: analysis.fetch('id')).fetch('preparation')
    prepared = Contract.wait(timeout: 90) { row = host.api('GET', "#{path}/preparations/#{preparation}"); row if row['status'] == 'awaiting_occurrence' }
    reference = prepared.fetch('version').fetch('reference')
    start_desktop
    click_control('Work menu')
    click_control('Automation menu')
    click_control('Routines')
    click_control('Checked sales')
    desktop.see('Script automation')
    desktop.see('Prepared · awaiting an authorized run')
    run = host.api('POST', "#{path}/run", operation: 'trial', revision: 1, inputs: JSON.generate(observed_input)).fetch('run')
    host.turn(run)
    desktop.see('Script active · ' + reference.fetch('digest')[0, 8])
    desktop.see('Activated by run ' + run)
    desktop.see('Activated after the checked sales result.')
    calls = host.calls.length
    desktop.see('Run now')
    expect(snapshot.call.fetch('activeCode')).to include('reference' => reference, 'run' => run)
    expect(snapshot.call.fetch('runs').length).to eq(2)
    expect(host.calls.length).to eq(calls)
    output, error, status = Open3.capture3(docker, '--host', socket, 'ps', '--all', '--quiet', '--no-trunc')
    raise "Cannot verify runtime noninterference: #{error}" unless status.success?
    expect(output.lines.map(&:strip).sort).to eq(baseline)
  end
end
