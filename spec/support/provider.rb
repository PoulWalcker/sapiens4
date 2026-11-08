require 'json'
require 'securerandom'
require 'net/http'
require 'rbconfig'

root = ENV.fetch('SAPIENS_CONTRACT_ENV')
config = JSON.parse(File.read(File.join(root, 'provider.json')))
if ARGV.include?('--version')
  puts config.fetch('version', 'codex-cli 0.158.0')
  exit
end

text = STDIN.read
context = if text.include?("\nContext:\n")
  JSON.parse(text.split("\nContext:\n", 2).last)
elsif text.include?("\nCurrent Group context:\n")
  JSON.parse(text.split("\nCurrent Group context:\n", 2).last.lines.first)
else
  {}
end
node = text[/decision at node (\w+)/, 1] || (text.include?('You are acting as yourself inside') ? 'group' :
  text.start_with?('You are the current Lead updating') ? 'group-memo' : 'other')
owner = context['selfId'] || context['self_id'] || File.basename(Dir.pwd)
key = "#{owner}:#{node}"
rule = config.fetch('rules', {}).fetch(key, config.fetch('rules', {}).fetch(node, {}))
matching = config.fetch('messages', []).find do |item|
  intent = context.fetch('originalIntent') { text.split("\nCurrent request:\n", 2).last }
  (!item['node'] || item['node'] == node) && intent.include?(item.fetch('contains'))
end
rule = rule.merge(matching.fetch('rule')) if matching
record = {owner: owner, node: node, prompt: text, args: ARGV, cwd: Dir.pwd,
          pid: Process.pid, started: Time.now.to_f, agency_run: ENV['SAPIENS_AGENCY_RUN']}
path = File.join(root, 'calls', SecureRandom.hex(12) + '.json')
if rule['child']
  record[:heartbeat] = path + '.heartbeat'
  record[:child] = Process.spawn(RbConfig.ruby, File.join(__dir__, 'heartbeat.rb'), record[:heartbeat],
    **(Gem.win_platform? ? {} : {pgroup: true}), out: File::NULL, err: File::NULL)
end
File.write(path + '.new', JSON.generate(record))
File.rename(path + '.new', path)
if rule.fetch('block', config.fetch('block', false))
  sleep 0.02 until File.exist?(File.join(root, rule.fetch('gate', 'released'))) || File.exist?(File.join(root, 'released'))
end
if rule.fetch('fail', config.fetch('fail', false))
  warn 'Scripted provider failure'
  exit 1
end
Array(rule['actions']).each do |action|
  if action['write'] || action['append']
    content = action['contentFrom'] ? File.read(File.expand_path(action['contentFrom'], Dir.pwd)) : action.fetch('content')
    target = File.expand_path(action['write'] || action['append'], Dir.pwd)
    File.open(target, action['append'] ? 'a' : 'w') do |file|
      file.flock(File::LOCK_EX)
      file.write(action.fetch('prefix', '') + content)
    end
  elsif action['appendContext']
    entry = context.select { |name, _| action.fetch('fields').include?(name) }
    File.open(File.expand_path(action['appendContext'], Dir.pwd), 'a') do |file|
      file.flock(File::LOCK_EX)
      file.puts(JSON.generate(entry))
    end
  elsif action['control'] || action['controlFrom']
    operation = action['control'] || JSON.parse(File.read(File.expand_path(action['controlFrom'], Dir.pwd)))
    uri = URI(JSON.parse(File.read('host-control.json')).fetch('url'))
    request = Net::HTTP::Post.new(uri)
    request['Content-Type'], request['X-Sapiens-Local'] = 'application/json', '1'
    request.body = JSON.generate(operation.merge('agencyRun' => ENV['SAPIENS_AGENCY_RUN']).reject { |_, v| v.nil? })
    http = Net::HTTP.new(uri.host, uri.port, nil)
    http.read_timeout = 5
    response = http.request(request)
    raise "Host control failed: #{response.code}: #{response.body}" unless response.code.to_i == 200
    File.write(File.expand_path(action['receipt'], Dir.pwd), response.body) if action['receipt']
  elsif action['gate']
    sleep 0.02 until File.exist?(File.join(root, action['gate'])) || File.exist?(File.join(root, 'released'))
  end
end
defaults = {
  'Assessing' => {event: 'FitsSpecialization', mode: config.fetch('mode', 'Repl'), reply: 'Contract test reply'},
  'ReadyToWork' => {event: 'TaskPrepared', taskType: 'research', specification: {
    objective: 'Compare the products.', inputs: [], expectedOutputs: ['A comparison'], completionCriteria: [
      {id: 'comparison', requirement: 'Return a sourced comparison.', applicability: {kind: 'Required'}}]}},
  'Execution' => {event: 'Outcome', reply: 'Verified comparison', outcome: {status: 'Completed', criterionResults: [
    {id: 'comparison', result: 'Satisfied', evidence: 'The sourced comparison was prepared.'}], artifacts: []}},
  'Delegation' => {event: 'HandoffPrepared', request: 'Compare the products.'},
  'CreatingSapi' => {event: 'SapiSpecified', name: 'NewResearcher', role: 'Research'}
}
reply = rule.fetch('response') do
  if defaults[node]
    defaults[node].merge(reason: 'Based on the supplied request.', evidence: ['Fixture evidence.'])
  else
    'Contract test reply'
  end
end
reply = JSON.generate(reply) unless reply.is_a?(String)
record[:finished] = Time.now.to_f
File.write(path + '.new', JSON.generate(record))
File.rename(path + '.new', path)
puts JSON.generate(type: 'thread.started', thread_id: 'fixture-session')
puts JSON.generate(type: 'turn.started')
Array(rule.fetch('events', [])).each { |event| puts(event.is_a?(String) ? event : JSON.generate(event)) }
puts JSON.generate(type: 'item.completed', item: {type: 'agent_message', text: reply})
Array(rule.fetch('completion', [{type: 'turn.completed', usage: {input_tokens: 100, cached_input_tokens: 20, output_tokens: 30}}])).each do |event|
  puts(event.is_a?(String) ? event : JSON.generate(event))
end
