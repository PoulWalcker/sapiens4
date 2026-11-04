require 'json'
require 'securerandom'
require 'net/http'

# Controlled external provider boundary with an actual independent peer effect.
root = ENV.fetch('SAPIENS_CONTRACT_ENV')
if ARGV.include?('--version')
  puts 'codex-cli 0.162.1'
  exit
end
if ARGV.take(3) == %w[debug models --bundled]
  puts JSON.generate(models: [{slug: ENV.fetch('SAPIENS_CODEX_MODEL', 'gpt-6-sol'), shell_type: 'unified_exec', apply_patch_tool_type: 'freeform', experimental_supported_tools: [], tool_mode: 'direct', supports_search_tool: false}])
  exit
end
$stdout.sync = true
text = STDIN.read
context = JSON.parse(text.split("\nContext:\n", 2).last)
node = text[/decision at node (\w+)/, 1]
owner = context.fetch('selfId')
config = JSON.parse(File.read(File.join(root, 'provider.json')))
mode = config.fetch('feed_legacy_mode', 'clean')
target = config['delegate_target']
path = File.join(root, 'calls', SecureRandom.hex(12) + '.json')
File.write(path + '.new', JSON.generate(owner: owner, node: node, mode: mode, pid: Process.pid, agency_run: ENV.fetch('SAPIENS_AGENCY_RUN')))
File.rename(path + '.new', path)
if mode == 'admitted_block' && node == 'Execution'
  sleep 0.02 until File.exist?(File.join(root, 'released'))
end
emit = ->(value) { puts JSON.generate(value) }
emit.call(type: 'thread.started', thread_id: 'controlled-ordinary-routine')
if mode == 'tool_loss' && node == 'Execution' && (!target || owner == target)
  emit.call(type: 'item.started', item: {id: 'actual-peer-post', type: 'command_execution', command: 'POST an isolated receipt peer once'})
  uri = URI(config.fetch('effect_url'))
  client = Net::HTTP.new(uri.host, uri.port, nil)
  client.max_retries = 0
  client.read_timeout = 5
  request = Net::HTTP::Post.new(uri)
  request['Content-Type'] = 'application/json'
  request.body = JSON.generate(schema: 1, operationKey: 'legacy-report-42', scope: 'independent-legacy-fixture', profileHash: 'unregistered-profile', requestHash: 'unregistered-request', receiver: 'daily-inbox', content: {text: 'Report 42'})
  begin
    client.request(request)
  rescue EOFError, Errno::ECONNRESET, Net::ReadTimeout
    # The independent peer, not this statement, proves the actual committed write.
  end
  emit.call(type: 'item.completed', item: {id: 'actual-peer-post', type: 'command_execution', exit_code: 0, aggregated_output: 'Transport acknowledgement was not retained.'})
  exit 1
end
specification = context.fetch('task').fetch('specification') || config['ordinary_specification']
response = case node
when 'Execution'
  if target && owner != target
    {event: 'OutsideSpecialization'}
  else
    {event: 'Outcome', reply: 'Done.', outcome: {status: 'Completed', satisfiedCriteria: mode == 'clean_rejected' ? [] : specification.fetch('completionCriteria'), artifacts: []}}
  end
when 'ChiefTriage' then {event: 'SpecialistSelected', target: target}
when 'Delegation' then {event: 'HandoffPrepared', request: specification.fetch('objective')}
when 'Assessing' then {event: 'FitsSpecialization', mode: 'Exec'}
when 'ReadyToWork' then {event: 'TaskPrepared', taskType: 'automated', specification: specification}
when 'AutomationAnalysis' then {event: 'AutomationAnalyzed', decision: {status: 'Decline', reason: 'Fixture has no preparation requirement.'}}
else raise "Unexpected controlled node #{node}"
end
response.merge!(reason: 'Controlled public protocol response.', evidence: ['Independent fixture context.'])
emit.call(type: 'item.completed', item: {id: 'final', type: 'agent_message', text: JSON.generate(response)})
emit.call(type: 'turn.completed', usage: {input_tokens: 37, cached_input_tokens: 0, output_tokens: 13})
