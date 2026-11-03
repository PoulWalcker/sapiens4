require 'json'
require 'securerandom'

# This executable is an external provider fixture, not an application decoder.
root = ENV.fetch('SAPIENS_CONTRACT_ENV')
if ARGV.include?('--version')
  puts 'codex-cli 0.162.1'
  exit
end
$stdout.sync = true
input = STDIN.read
context = JSON.parse(input.split("\nContext:\n", 2).last)
node = input[/decision at node (\w+)/, 1]
configuration = JSON.parse(File.read(File.join(root, 'provider.json')))
mode = configuration.fetch('evidence_mode', 'normal')
mode = 'normal' unless node == 'Execution'
clean = %w[clean_invalid_completed clean_unresolved clean_malformed_final clean_missing_usage].include?(mode)
mode = mode.delete_prefix('clean_') if clean
record = {owner: context.fetch('selfId'), node: node, started: Time.now.to_f,
          pid: Process.pid, agency_run: ENV.fetch('SAPIENS_AGENCY_RUN'), mode: mode, clean_no_tools: clean}
receipt = File.join(root, 'calls', SecureRandom.hex(12) + '.json')
File.write(receipt + '.new', JSON.generate(record))
File.rename(receipt + '.new', receipt)
emit = ->(event) { puts JSON.generate(event) }
emit.call(type: 'thread.started', thread_id: 'fixture-session-shared-across-invocations')
emit.call(type: 'item.started', item: {id: 'tool-1', type: 'command_execution', command: 'synthetic observation'}) unless clean

if mode == 'flood'
  until File.exist?(File.join(root, 'released'))
    5.times { emit.call(type: 'item.updated', item: {id: 'tool-1', type: 'command_execution', command: 'Continuous synthetic progress', aggregated_output: 'x' * 1024}) }
    sleep 0.01
  end
  exit 0
end

if %w[loss failure_after_final].include?(mode)
  File.open(File.join(root, 'external-effect.txt'), 'a') { |file| file.puts('effect') }
end
if mode == 'secret_failure'
  canary = 'sk-fixture-private-canary-1234567890'
  emit.call(type: 'item.updated', item: {id: canary, type: 'command_execution', command: "curl https://example.invalid/?token=#{canary}", aggregated_output: canary})
  emit.call(type: 'item.completed', item: {id: canary, type: 'mcp_tool_call', tool: canary, result: {secret: canary}, error: canary})
  emit.call(type: 'error', message: canary)
  warn "Authorization: Bearer #{canary}"
  exit 1
end
if mode == 'bounded'
  puts 'not-json'
  emit.call(type: 'provider.future', content: 'omitted future payload')
  emit.call(type: 'item.completed', item: {id: 'out-of-order', type: 'command_execution', exit_code: 0})
  emit.call(type: 'item.updated', item: {id: 'tool-1', type: 'command_execution', command: 'updated fixture'})
  400.times { |index| emit.call(type: 'item.started', item: {id: "pending-#{index}", type: 'command_execution', command: 'é' * 1024}) }
  puts JSON.generate(type: 'provider.future', content: 'é' * (2 * 1024 * 1024))
end
if %w[blocked crash].include?(mode)
  if mode == 'crash'
    sleep 0.3
    emit.call(type: 'item.updated', item: {id: 'tool-1', type: 'command_execution', command: 'still running'})
  end
  File.write(File.join(root, 'provider-events-emitted'), '')
  sleep 0.02 until File.exist?(File.join(root, 'released'))
end
exit 0 if mode == 'loss'
emit.call(type: 'item.completed', item: {id: 'tool-1', type: 'command_execution', command: 'synthetic observation', aggregated_output: 'Fixture output', exit_code: 0}) unless clean

if mode == 'semantic'
  emit.call(type: 'item.completed', item: {id: 'mcp-1', type: 'mcp_tool_call', server: 'fixture-documents', tool: 'read_document', arguments: {path: 'comparison.txt', api_key: 'private-fixture-field-value'}, result: {content: [{type: 'text', text: 'Document has two sources.'}]}, status: 'completed'})
  emit.call(type: 'item.completed', item: {id: 'files-1', type: 'file_change', changes: [{path: 'comparison.txt', kind: 'add'}], status: 'completed'})
  emit.call(type: 'item.completed', item: {id: 'web-1', type: 'web_search', query: 'synthetic comparison sources', status: 'completed'})
  emit.call(type: 'item.completed', item: {id: 'long-1', type: 'command_execution', command: 'Inspect a long synthetic result', aggregated_output: 'Long-safe-output-' + 'é' * 5000, exit_code: 0})
end

specification = context.fetch('task').fetch('specification')
response = if node == 'ReadyToWork'
  {event: 'TaskPrepared', taskType: 'automated', specification: specification}
else
  criteria = specification.fetch('completionCriteria')
  {event: 'Outcome', reply: 'Fixture result', outcome: {
    status: mode == 'unresolved' ? 'Unresolved' : 'Completed',
    satisfiedCriteria: %w[invalid_completed unresolved].include?(mode) ? [] : criteria,
    artifacts: []}}
end
if mode == 'semantic'
  artifact = File.join(Dir.pwd, 'comparison.txt')
  File.write(artifact, 'Comparison with two synthetic sources.')
  response[:outcome][:artifacts] = [artifact]
end
response.merge!(reason: 'Fixture contract assessment.', evidence: ['The model states the requested result exists.'])
final = mode == 'malformed_final' ? '{invalid final' : JSON.generate(response)
emit.call(type: 'item.completed', item: {id: 'final', type: 'agent_message', text: final})
exit 1 if mode == 'failure_after_final'
unless mode == 'missing_usage'
  usage = {input_tokens: 101, cached_input_tokens: 7, output_tokens: 13}
  usage.delete(:cached_input_tokens) if mode == 'missing_cached'
  invalid_usage = '{"type":"turn.completed","usage":{"input_tokens":1e1000000000,"cached_input_tokens":-1,"output_tokens":"invalid"}}'
  puts invalid_usage if %w[invalid_usage_first invalid_usage_only].include?(mode)
  emit.call(type: 'turn.completed', usage: usage) unless mode == 'invalid_usage_only'
  emit.call(type: 'turn.completed', usage: usage) if %w[duplicate_usage conflicting_usage].include?(mode)
  emit.call(type: 'turn.completed', usage: usage.merge(input_tokens: 999)) if mode == 'conflicting_usage'
  puts invalid_usage if mode == 'invalid_usage_after'
end

emit.call(type: 'turn.completed') if clean && mode == 'missing_usage'
