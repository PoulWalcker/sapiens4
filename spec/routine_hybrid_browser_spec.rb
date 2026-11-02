require 'rbconfig'
require 'digest'
require 'open3'
require 'sqlite3'
require_relative 'support/desktop'
require_relative 'support/hybrid_fixtures'
require_relative 'support/candidate_docker'

RSpec.describe 'Hybrid execution steps in the native app', :desktop do
  include HybridFixtures
  include CandidateDocker

  before(:all) do
    unless RbConfig::CONFIG.fetch('host_os').include?('darwin') && RbConfig::CONFIG.fetch('host_cpu').match?(/\A(?:arm64|aarch64)\z/)
      skip 'Selected generated-code runtime currently supports macOS ARM64 only'
    end
    @docker = File.expand_path('~/.orbstack/bin/docker')
    @socket = 'unix://' + File.expand_path('~/.orbstack/run/docker.sock')
    output, error, status = Open3.capture3(@docker, '--host', @socket, 'ps', '--all', '--quiet', '--no-trunc')
    raise "Selected runtime unavailable: #{error}" unless status.success?
    @existing_containers = output.lines.map(&:strip).sort
  end

  after(:all) do
    next unless @existing_containers
    output, error, status = Open3.capture3(@docker, '--host', @socket, 'ps', '--all', '--quiet', '--no-trunc')
    raise "Cannot verify runtime noninterference: #{error}" unless status.success?
    expect(output.lines.map(&:strip).sort).to eq(@existing_containers)
  end

  before do
    @host = Host.new(start: false, environment: {'SAPIENS_GENERATED_DOCKER' => @docker, 'SAPIENS_GENERATED_SOCKET' => @socket})
    host.configure(version: 'codex-cli 0.162.1', model_catalog: [{slug: 'gpt-6-sol', shell_type: 'unified_exec', apply_patch_tool_type: 'freeform', experimental_supported_tools: [], tool_mode: 'direct', supports_search_tool: false, supported_reasoning_levels: %w[low medium high xhigh max ultra].map { |effort| {effort: effort, description: effort} }}])
    host.start
    @owner = host.state.fetch('main_agent_id')
    @routine = host.api('POST', "/api/agents/#{@owner}/routines", operation: 'create', title: 'Sales explanation', goal: HybridFixtures::GOAL, criteria: HybridFixtures::CRITERIA).fetch('routine')
    host.configure(rules: {
      'Execution' => {response: decision('Outcome', reply: HybridFixtures::OBSERVED_TEXT, outcome: {status: 'Completed', satisfiedCriteria: HybridFixtures::CRITERIA, artifacts: []})},
      'AutomationAnalysis' => {response: decision('AutomationAnalyzed', decision: {status: 'Ready', automatablePart: 'Calculate exact sales deltas and explain them.', parameters: [{name: 'periods', description: 'Current and previous sales records'}]})}
    })
    @business = submit_run('ordinary', HybridFixtures::OBSERVED)
    host.turn(@business)
    @analysis = Contract.wait { row = routine['analysis']; row if row && row['status'] == 'Ready' }
    configure_hybrid
  end

  after do
    File.write(@docker_gate, '') if @docker_gate && File.directory?(File.dirname(@docker_gate))
    @host&.close
  end

  def path
    "/api/agents/#{@owner}/routines/#{@routine}"
  end

  def routine
    host.state.fetch('personal_routines').find { |row| row['id'] == @routine }
  end

  def criteria_verdicts
    HybridFixtures::CRITERIA.each_index.map { |index| {id: "criterion-#{index + 1}", status: 'pass', evidence: index.zero? ? 'Exact independently expected arithmetic agrees with the actual calculated facts.' : 'The narrative supports this original criterion with the supplied periods and retained facts.'} }
  end

  def configure_hybrid(source: hybrid_source, instruction: 'Explain the two periods, every category delta and net total using only supplied records and calculated facts. State that percentage comparisons with zero previous amounts are undefined.')
    explanation = {response: {text: HybridFixtures::OBSERVED_TEXT}, variants: [{contains: '2026-12-05', response: {text: HybridFixtures::HELDOUT_TEXT}}], usage: {input_tokens: 53, cached_input_tokens: 3, output_tokens: 17}}
    host.configure(rules: host.provider.fetch('rules').merge(
      'PreparationFixtures' => {response: {fixtures: [{input: HybridFixtures::OBSERVED, expected: {status: 'ok', value: HybridFixtures::OBSERVED_FACTS}}, {input: HybridFixtures::HELDOUT, expected: {status: 'ok', value: HybridFixtures::HELDOUT_FACTS}}], rubric: HybridFixtures::CRITERIA.each_index.map { |index| {id: "criterion-#{index + 1}", kind: index.zero? ? 'numeric' : 'semantic'} }, reason: 'Independent exact facts and original narrative criteria.'}},
      'PreparationGeneration' => {response: {language: 'python', profile: 'python-3.13.15-linux-arm64', source: source, interface: 'json-stdin-result-v1', dependencies: [], capabilities: [], explanation: {instruction: instruction}}},
      'PreparationExplanation' => explanation,
      'PreparationRubricReview' => {response: {criteria: criteria_verdicts}},
      'PreparationReview' => {response: {accepted: true, reason: 'Actual calculations and separate narrative rubric checks agree.'}},
      'HybridExplanation' => explanation,
      'HybridAssessment' => {response: {verdict: 'accepted', reason: 'Separate numerical and semantic criteria are satisfied.', evidence: ['Actual retained facts and generated narrative.'], criteria: criteria_verdicts, activationAudit: {accepted: true, reason: 'The actual hybrid result supports this exact implementation.'}}, usage: {input_tokens: 71, cached_input_tokens: 7, output_tokens: 23}}
    ))
  end

  def prepare(operation = 'prepare', status: 'awaiting_occurrence')
    id = host.api('POST', "#{path}/prepare", operation: operation, revision: 1, analysis: @analysis.fetch('id')).fetch('preparation')
    Contract.wait(timeout: 90, description: 'checked hybrid candidate') do
      row = host.api('GET', "#{path}/preparations/#{id}")
      raise "Hybrid preparation failed: #{row['error']}" if %w[failed stopped stale interrupted cleanup_pending].include?(row['status']) && row['status'] != status
      row if row['status'] == status
    end
  end

  def submit_run(operation, input)
    host.api('POST', "#{path}/run", operation: operation, revision: 1, inputs: JSON.generate(input)).fetch('run')
  end

  def run_detail(id)
    host.api('GET', "#{path}/runs/#{id}")
  end

  def stage_calls(node)
    host.calls.select { |call| call['node'] == node }
  end

  def click_control(label)
    roles = %w[AXButton AXCheckBox AXRadioButton AXLink AXDisclosureTriangle AXPopUpButton]
    node = desktop.find(label, role: roles)
    desktop.control('scroll-to', '--path', node.fetch('path'))
    desktop.click_node(desktop.find(label, role: roles))
  end

  it 'shows the two completed stages and retains calculation when explanation is stopped' do
    prepare
    first = submit_run('successful-hybrid', HybridFixtures::OBSERVED)
    host.turn(first)
    start_desktop
    click_control('Work menu')
    click_control('Automation menu')
    click_control('Routines')
    click_control('Sales explanation')
    desktop.see('Execution steps')
    desktop.see('Calculation · Completed')
    desktop.see('Explanation · Completed')
    desktop.see(HybridFixtures::OBSERVED_TEXT)
    rules = host.provider.fetch('rules')
    host.configure(rules: rules.merge('HybridExplanation' => rules.fetch('HybridExplanation').merge(block: true, gate: 'native-held-explanation')))
    click_control('Run now')
    desktop.fill('Inputs for this run', JSON.generate(HybridFixtures::HELDOUT))
    click_control('Run now')
    Contract.wait { stage_calls('HybridExplanation').length == 2 }
    desktop.see('Calculation · Completed')
    desktop.see('Explanation · In progress')
    click_control('Stop this run…')
    click_control('Stop run')
    host.release('native-held-explanation')
    desktop.see('Stopped')
    desktop.see('Calculation · Completed')
    desktop.see(/Explanation · (Interrupted|Failed)/)
    expect(stage_calls('HybridAssessment').length).to eq(1)
    expect(routine.fetch('runs').length).to eq(3)
  end
end
