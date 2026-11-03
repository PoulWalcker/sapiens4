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

if ARGV.take(3) == %w[debug models --bundled]
  if (gate = config['model_catalog_gate'])
    File.write(File.join(root, gate + '-started'), '')
    sleep 0.02 until File.exist?(File.join(root, gate))
  end
  models = config.fetch('model_catalog', [{slug: ENV.fetch('SAPIENS_CODEX_MODEL', 'gpt-6-sol'),
    shell_type: 'unified_exec', apply_patch_tool_type: 'freeform',
    experimental_supported_tools: [], tool_mode: 'direct', supports_search_tool: false}])
  puts JSON.generate(models: models)
  exit
end

text = STDIN.read
context = text.include?("\nContext:\n") ? JSON.parse(text.split("\nContext:\n", 2).last) : {}
node = text[/decision at node (\w+)/, 1] || (text.start_with?('You are acting as yourself inside') ? 'group' :
  text.start_with?('You are the current Lead updating') ? 'group-memo' : 'other')
owner = context['selfId'] || File.basename(Dir.pwd)
key = "#{owner}:#{node}"
rule = config.fetch('rules', {}).fetch(key, config.fetch('rules', {}).fetch(node, {}))
variants = rule.fetch('variants', []).select { |variant| text.include?(variant.fetch('contains')) }
raise 'Ambiguous controlled provider variants' if variants.length > 1
rule = rule.merge(variants.first.reject { |name, _| name == 'contains' }) unless variants.empty?
matching = config.fetch('messages', []).find { |item| context.fetch('originalIntent', text).include?(item.fetch('contains')) }
rule = rule.merge(matching.fetch('rule')) if matching
record = {owner: owner, node: node, prompt: text, args: ARGV, cwd: Dir.pwd,
          pid: Process.pid, started: Time.now.to_f, agency_run: ENV['SAPIENS_AGENCY_RUN']}
if (catalog_argument = ARGV.find { |argument| argument.start_with?('model_catalog_json=') })
  catalog_path = JSON.parse(catalog_argument.split('=', 2).last)
  record[:invocation_catalog] = JSON.parse(File.read(catalog_path)).fetch('models').map do |model|
    model.slice('slug', 'shell_type', 'apply_patch_tool_type', 'experimental_supported_tools', 'tool_mode', 'supports_search_tool')
  end
end
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
  if action['write']
    File.write(File.expand_path(action['write'], Dir.pwd), action.fetch('content'))
  elsif action['control']
    uri = URI(JSON.parse(File.read('host-control.json')).fetch('url'))
    request = Net::HTTP::Post.new(uri)
    request['Content-Type'], request['X-Sapiens-Local'] = 'application/json', '1'
    request.body = JSON.generate(action['control'].merge('agencyRun' => ENV['SAPIENS_AGENCY_RUN']).reject { |_, v| v.nil? })
    http = Net::HTTP.new(uri.host, uri.port, nil)
    http.read_timeout = 5
    response = http.request(request)
    raise "Host control failed: #{response.code}: #{response.body}" unless response.code.to_i == 200
  end
end
defaults = {
  'Assessing' => {event: 'FitsSpecialization', mode: config.fetch('mode', 'Repl'), reply: 'Contract test reply'},
  'ReadyToWork' => {event: 'TaskPrepared', taskType: 'research', specification: {
    objective: 'Compare the products.', inputs: [], expectedOutputs: ['A comparison'], completionCriteria: ['Return a sourced comparison.']}},
  'Execution' => {event: 'Outcome', reply: 'Verified comparison', outcome: {status: 'Completed', satisfiedCriteria: ['Return a sourced comparison.'], artifacts: []}},
  'AutomationAnalysis' => {event: 'AutomationAnalyzed', decision: {status: 'Ready',
    automatablePart: 'Compare the supplied product records.', parameters: [{name: 'products', description: 'Current records'}]}},
  'Delegation' => {event: 'HandoffPrepared', request: 'Compare the products.'},
  'CreatingSapi' => {event: 'SapiSpecified', name: 'NewResearcher', role: 'Research'}
}
reply = if rule['responses']
  rule.fetch('responses').fetch(context.fetch('observations', []).length)
else
 rule.fetch('response') do
  if defaults[node]
    defaults[node].merge(reason: 'Based on the supplied request.', evidence: ['Fixture evidence.'])
  else
    'Contract test reply'
  end
end
end
reply = JSON.generate(reply) unless reply.is_a?(String)
puts JSON.generate(type: 'thread.started', thread_id: 'fixture-session')
sleep rule.fetch('event_delay', 0)
Array(rule['events']).each { |event| puts JSON.generate(event) }
STDOUT.flush
if (gate = rule['after_events_gate'])
  File.write(File.join(root, gate + '-started'), '')
  sleep 0.02 until File.exist?(File.join(root, gate))
end
puts JSON.generate(type: 'item.completed', item: {type: 'agent_message', text: reply})

puts JSON.generate(type: 'turn.completed', usage: rule['usage']) if rule['usage']
