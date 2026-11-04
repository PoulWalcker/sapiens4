require 'yaml'
require_relative 'support/host'

RSpec.describe 'Scoped completion and explicit conditional criteria' do
  # Original criterion text and outcome accounting from the observed RAM failure.
  # Live artifact paths are removed; this controlled check performs no host work.
  RAM_SPEC = JSON.parse(<<~'JSON').freeze
{
  "completionCriteria": [
    "В Execution выполнено немедленное измерение занятой и общей RAM этого Mac, а результат подготовлен для доставки Админу в формате <used> of <total>.",
    "После немедленного измерения RamLive вызвал routine_create с minutes=3 и промптом, сохраняющим все требования к каждому повторному отчёту.",
    "Получено и сохранено подтверждение host-control с ID активной Scheduled Routine, принадлежащей RamLive; только после этого подготовлено сообщение Админу о её создании.",
    "Если измерение или создание рутины не удалось, подготовлено сообщение Админу с точной ошибкой и фактическим статусом каждого действия."
  ],
  "expectedOutputs": [
    "Сообщение Админу с результатом немедленного измерения в формате <used> of <total>.",
    "Сохранённая Scheduled Routine, принадлежащая RamLive, с интервалом 3 минуты и промптом каждого запуска: измерить актуальную занятую и общую RAM этого Mac и отправить Админу одно сообщение в формате <used> of <total>.",
    "Подтверждение создания с ID рутины и сообщение Админу о создании после получения подтверждения.",
    "Если измерение или создание не удастся, сообщение Админу с точной ошибкой и фактическим статусом."
  ],
  "inputs": [
    "Исходное намерение Админа: создать нового Sapi RamLive для немедленного измерения RAM и отчётов каждые 3 минуты.",
    "Исполнитель: RamLive, sapi_35b3f79f3e61; ранее созданный RamMonitor не исполняет это поручение.",
    "Операция host-control: routine_create(minutes,prompt), где minutes=3; подтверждение содержит сохранённый ID рутины."
  ],
  "objective": "RamLive должен сразу измерить занятую и общую RAM этого Mac, сообщить Админу результат в формате <used> of <total>, затем в Execution создать собственную Scheduled Routine для такого отчёта каждые 3 минуты."
}
JSON
  RAM_OUTCOME = JSON.parse(<<~'JSON').freeze
{
  "event": "Outcome",
  "evidence": [
    "Команда top показала PhysMem: 23G used, 113M unused; sysctl показала общий объём 25769803776 байт (24 GiB).",
    "После измерения routine_create с minutes=3 вернула saved=true и ID routine_sch_00004.",
    "Повторная проверка routines подтвердила owner=sapi_35b3f79f3e61, minutes=3, paused=false и промпт каждого запуска с измерением RAM и одним сообщением Админу.",
    "Notes.html обновлён атомарно."
  ],
  "outcome": {
    "artifacts": [],
    "notApplicableCriteria": {
      "Если измерение или создание рутины не удалось, подготовлено сообщение Админу с точной ошибкой и фактическим статусом каждого действия.": "Измерение успешно; routine_create вернула saved=true, а routines подтвердил активную рутину."
    },
    "satisfiedCriteria": [
      "В Execution выполнено немедленное измерение занятой и общей RAM этого Mac, а результат подготовлен для доставки Админу в формате <used> of <total>.",
      "После немедленного измерения RamLive вызвал routine_create с minutes=3 и промптом, сохраняющим все требования к каждому повторному отчёту.",
      "Получено и сохранено подтверждение host-control с ID активной Scheduled Routine, принадлежащей RamLive; только после этого подготовлено сообщение Админу о её создании."
    ],
    "status": "Completed"
  },
  "reason": "Немедленное измерение выполнено; создание и параметры рутины подтверждены host-control.",
  "reply": "Админ, 23G of 24G. Создана активная рутина RamLive каждые 3 минуты; подтверждённый ID: routine_sch_00004."
}
JSON

  before do
    @host = Host.new(start: false, repo: ENV.fetch('SAPIENS_TEST_REPOSITORY', Contract::ROOT))
    host.configure(version: 'codex-cli 0.162.1', mode: 'Exec', model_catalog: [{slug: 'gpt-6-sol', shell_type: 'unified_exec', apply_patch_tool_type: 'freeform', experimental_supported_tools: [], tool_mode: 'direct', supports_search_tool: false, supported_reasoning_levels: [{effort: 'high', description: 'high'}]}])
    host.start
  end
  after do
    if RSpec.current_example.exception && @host
      prefix = "/tmp/sapiens-scoped-completion-#{RSpec.current_example.metadata[:line_number]}"
      File.write(prefix + '-calls.json', JSON.pretty_generate(host.calls))
      File.write(prefix + '-state.json', JSON.pretty_generate(host.state))
      File.write(prefix + '-tasks.json', JSON.pretty_generate(tasks))
    end
    @host&.close
  end
  def rules(changes)
    host.configure(rules: host.provider.fetch('rules', {}).merge(changes.transform_values { |rule| {usage: {input_tokens: 19, output_tokens: 9}}.merge(rule) }))
  end
  def tasks
    YAML.safe_load(host.api('GET', "/api/agents/#{chief}/tasks")).fetch('tasks')
  end
  def context(call); JSON.parse(call.fetch('prompt').split("\nContext:\n", 2).last); end

  it 'accepts the captured Russian conditional only when explicitly declared in the pinned contract' do
    conditional = RAM_SPEC.fetch('completionCriteria').last
    rules('ReadyToWork' => {response: decision('TaskPrepared', taskType: 'work', specification: RAM_SPEC.merge('conditionalCriteria' => {conditional => 'Измерение или создание рутины не удалось.'}))},
      'Execution' => {response: RAM_OUTCOME})
    accepted = host.submit(chief, 'Verify the recorded RAM completion accounting.')
    host.turn(accepted)
    expect(tasks.find { |t| t['workloadId'] == accepted }.dig('outcome', 'status')).to eq('Completed')
    rules('ReadyToWork' => {response: decision('TaskPrepared', taskType: 'work', specification: RAM_SPEC)},
      'Execution' => {response: RAM_OUTCOME})
    undeclared = host.submit(chief, 'Verify that an undeclared criterion cannot be skipped.')
    rejected = host.turn(undeclared, 'failed')
    expect(rejected['error']).to include('pinned completion criteria')
    expect(tasks.find { |t| t['workloadId'] == undeclared }['outcome']).to be_nil
  end
  def scoped_setup(second_block: false, continuation_block: false)
    first, second = host.agent('SocialCounts'), host.agent('OrderSummary')
    @first, @second = first, second
    @criteria = ['Report the follower count.', 'Report the order summary.']
    @root_spec = {objective: 'Provide both requested reports.', inputs: [], expectedOutputs: ['Both reports'], completionCriteria: @criteria, conditionalCriteria: {}}
    @first_spec = @root_spec.merge(objective: 'Provide the follower report.', expectedOutputs: ['Follower report'], completionCriteria: [@criteria[0]])
    @second_spec = @root_spec.merge(objective: 'Provide the order report.', expectedOutputs: ['Order report'], completionCriteria: [@criteria[1]])
    completed = ->(criteria, reply) { decision('Outcome', reply: reply, outcome: {status: 'Completed', satisfiedCriteria: criteria, artifacts: []}) }
    rules(
      "#{chief}:Assessing" => {response: decision('OutsideSpecialization'), variants: [
        {contains: '"author":"' + second + '"', response: decision('FitsSpecialization', mode: 'Exec')},
        * (continuation_block ? [{contains: '"author":"' + first + '"', response: decision('OutsideSpecialization'), block: true, gate: 'owner-continuation'}] : [])]},
      'ChiefTriage' => {response: decision('SpecialistSelected', target: first), variants: [{contains: '"author":"' + first + '"', response: decision('SpecialistSelected', target: second)}]},
      "#{chief}:ReadyToWork" => {response: decision('TaskPrepared', taskType: 'work', specification: @root_spec)},
      'Delegation' => {response: decision('HandoffPrepared', request: 'Provide the follower report.', scope: 'part', specification: @first_spec), variants: [{contains: '"target":"' + second + '"', response: decision('HandoffPrepared', request: 'Provide the order report.', scope: 'part', specification: @second_spec)}]},
      "#{first}:ReadyToWork" => {response: decision('TaskPrepared', taskType: 'work', specification: @first_spec)},
      "#{second}:ReadyToWork" => {response: decision('TaskPrepared', taskType: 'work', specification: @second_spec)},
      "#{first}:Execution" => {response: completed.call([@criteria[0]], 'Follower receipt retained.')},
      "#{second}:Execution" => {response: completed.call([@criteria[1]], 'Orders receipt retained.'), block: second_block, gate: 'second-result'},
      "#{chief}:Execution" => {response: completed.call(@criteria, 'Both retained reports verified.')})
  end
  def owner_calls(owner, node)
    host.calls.select { |c| c['owner'] == owner && c['node'] == node }
  end

  it 'retains scoped receipts, returns to the responsible owner and completes only after both specialists' do
    scoped_setup(second_block: true)
    original = host.submit(chief, 'Report followers and summarize orders.')
    Contract.wait { owner_calls(@second, 'Execution').length == 1 }
    pending = tasks.find { |t| t['workloadId'] == original }
    expect(pending.dig('outcome', 'status')).to eq('Unresolved')
    expect(owner_calls(@first, 'Execution').length).to eq(1)
    second_context = context(owner_calls(@second, 'Execution').first)
    expect(second_context.dig('currentCall', 'specification', 'completionCriteria')).to eq([@criteria[1]])
    expect(second_context.fetch('partialResults').map { |r| r['author'] }).to include(@first)
    expect(second_context['remainingCriteria']).to eq([@criteria[1]])
    host.release('second-result')
    finished = Contract.wait { row = tasks.find { |t| t['workloadId'] == original }; row if row.dig('outcome', 'status') == 'Completed' }
    expect(finished.dig('task', 'specification', 'completionCriteria')).to eq(@criteria)
    expect(owner_calls(@first, 'Execution').length).to eq(1)
    expect(owner_calls(@second, 'Execution').length).to eq(1)
    expect(owner_calls(chief, 'Execution').length).to eq(1)
  end

  it 'credits evidence-backed success in an unresolved partial result without replaying it' do
    scoped_setup(second_block: true)
    rules("#{@first}:Execution" => {response: decision('Outcome', reply: 'Follower receipt saved; the complete request still needs orders.', outcome: {status: 'Unresolved', satisfiedCriteria: [@criteria[0]], artifacts: []})})
    original = host.submit(chief, 'Preserve the completed follower read while finishing the order report.')
    Contract.wait { owner_calls(@second, 'Execution').length == 1 }
    second_context = context(owner_calls(@second, 'Execution').first)
    first_result = second_context.fetch('partialResults').find { |r| r['author'] == @first }
    expect(first_result.dig('outcome', 'status')).to eq('Unresolved')
    expect(first_result.dig('outcome', 'satisfiedCriteria')).to eq([@criteria[0]])
    expect(second_context['remainingCriteria']).to eq([@criteria[1]])
    expect(second_context.dig('currentCall', 'specification', 'completionCriteria')).to eq([@criteria[1]])
    host.release('second-result')
    Contract.wait { row = tasks.find { |t| t['workloadId'] == original }; row.dig('outcome', 'status') == 'Completed' }
    expect(owner_calls(@first, 'Execution').length).to eq(1)
    expect(owner_calls(@second, 'Execution').length).to eq(1)
  end
end
