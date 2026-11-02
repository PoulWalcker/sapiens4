require 'open3'
require 'socket'

RSpec.describe 'Response-only Routine analysis with the actual provider CLI' do
  it 'uses the shared host flow with no registered tools and refuses unsolicited actions' do
    binary = ENV.fetch('SAPIENS_TEST_REAL_CODEX', '/opt/homebrew/bin/codex')
    skip 'Verified Codex CLI is not installed on this platform' unless File.executable?(binary)
    expect(Open3.capture2(binary, '--version').first.strip).to eq('codex-cli 0.162.1')
    catalog = Open3.capture2(binary, 'debug', 'models', '--bundled').first
    directory = Dir.mktmpdir('sapiens-analysis-provider-')
    marker = File.join(directory, 'unexpected-action')
    inherited = File.join(directory, 'inherited-mcp')
    peer = TCPServer.new('127.0.0.1', 0)
    requests, errors = [], []
    server = Thread.new do
      loop do
        client = peer.accept
        request_line = client.gets
        headers = {}
        while (line = client.gets) && line != "\r\n"
          key, value = line.split(':', 2)
          headers[key.downcase] = value.strip
        end
        if request_line.start_with?('GET /v1/models')
          client.write("HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: #{catalog.bytesize}\r\nConnection: close\r\n\r\n#{catalog}")
          client.close
          next
        end
        length = headers.fetch('content-length', '0').to_i
        raise 'Expected a plain local provider request body' if length.zero?
        requests << JSON.parse(client.read(length))
        index = requests.length
        item = case index
        when 1
          {type: 'function_call', id: 'fc1', call_id: 'c1', name: 'exec_command', arguments: JSON.generate(cmd: "touch #{marker}")}
        when 2
          {type: 'custom_tool_call', id: 'fc2', call_id: 'c2', name: 'apply_patch', input: "*** Begin Patch\n*** Add File: #{marker}\n+Unexpected mutation\n*** End Patch"}
        when 3
          {type: 'function_call', id: 'fc3', call_id: 'c3', name: 'mcp__inherited__send', arguments: '{}'}
        else
          answer = {event: 'AutomationAnalyzed', reason: 'Saved records support a deterministic comparison.', evidence: ['Supplied result'],
            decision: {status: 'Ready', automatablePart: 'Compare supplied records.', parameters: []}}
          {type: 'message', id: 'msg', role: 'assistant', status: 'completed', content: [{type: 'output_text', text: JSON.generate(answer), annotations: []}]}
        end
        response = {id: "resp#{index}", object: 'response', status: 'completed', output: [item], usage: {input_tokens: 11, output_tokens: 7, total_tokens: 18}}
        events = [{type: 'response.created', response: response.merge(status: 'in_progress', output: [])},
          {type: 'response.output_item.added', output_index: 0, item: item},
          {type: 'response.output_item.done', output_index: 0, item: item}, {type: 'response.completed', response: response}]
        stream = events.map { |e| "event: #{e[:type]}\ndata: #{JSON.generate(e)}\n\n" }.join
        client.write("HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nContent-Length: #{stream.bytesize}\r\nConnection: close\r\n\r\n#{stream}")
        client.close
      end
    rescue IOError, Errno::EBADF
    rescue => error
      errors << error.message
      client&.close
    end
    wrapper = File.join(directory, 'provider')
    fixture = File.join(__dir__, 'support/provider.rb')
    File.write(wrapper, <<~RUBY)
      #!#{RbConfig.ruby}
      require 'json'
      if ARGV.first != 'exec'
        exec #{binary.inspect}, *ARGV
      end
      unless ARGV.include?('--ignore-user-config')
        load #{fixture.inspect}
        exit
      end
      # Transport is the external test seam; the host supplies the actual policy.
      settings = {'model_provider'=>'proof', 'model_providers.proof.name'=>'Local proof',
        'model_providers.proof.base_url'=>#{"http://127.0.0.1:#{peer.addr[1]}/v1".inspect},
        'model_providers.proof.wire_api'=>'responses', 'model_providers.proof.requires_openai_auth'=>false,
        'model_providers.proof.supports_websockets'=>false, 'model_providers.proof.request_max_retries'=>0,
        'model_providers.proof.stream_max_retries'=>0,
        'features.shell_tool'=>true, 'features.hooks'=>true,
        'mcp_servers.inherited.command'=>'/usr/bin/touch', 'mcp_servers.inherited.args'=>[#{inherited.inspect}]}
      options = settings.flat_map { |key,value| ['-c', key+'='+JSON.generate(value)] }
      exec #{binary.inspect}, 'exec', *options, *ARGV.drop(1)
    RUBY
    File.chmod(0755, wrapper)
    host = Host.new(environment: {'SAPIENS_CODEX_BINARY' => wrapper})
    owner = host.agent('AnalysisProviderOwner')
    routine = host.api('POST', "/api/agents/#{owner}/routines", operation: 'create', title: 'Comparison', goal: 'Compare the products.', criteria: ['Return a sourced comparison.']).fetch('routine')
    path = "/api/agents/#{owner}/routines/#{routine}"
    business = host.api('POST', "#{path}/run", operation: 'ordinary', revision: 1).fetch('run')
    saved = Contract.wait(timeout: 40, description: 'actual CLI response-only analysis') do
      row = host.state.fetch('personal_routines').find { |value| value['id'] == routine }
      analysis = row['analysis']
      analysis if analysis && analysis['status'] == 'Ready'
    end
    expect(errors).to be_empty
    expect(requests.size).to eq(4)
    expect(requests.map { |request| request['tools'] || [] }).to all(eq([]))
    outputs = requests.last.fetch('input').filter_map { |item| item['output'] if item['type'].end_with?('_output') }.join
    expect(outputs).to include('unsupported call: exec_command', 'unsupported custom tool call: apply_patch', 'unsupported call: mcp__inherited__send')
    expect(File.exist?(marker)).to be(false)
    expect(File.exist?(inherited)).to be(false)
    detail = host.api('GET', "#{path}/analysis/#{saved.fetch('id')}")
    expect(detail['sourceRuns']).to eq([business])
    expect(detail.fetch('invocations').size).to eq(1)
    receipt = detail.fetch('invocations').first
    expect(receipt).to include('purpose' => 'automation_analysis')
    expect(receipt.fetch('evidence')).to include('policy' => 'response_only', 'providerVersion' => 'codex-cli 0.162.1', 'model' => 'gpt-6-sol')
    expect(host.state.fetch('personal_routines').find { |row| row['id'] == routine }.fetch('runs').map { |run| run['id'] }).to eq([business])
    host.restart
    expect(host.api('GET', "#{path}/analysis/#{saved.fetch('id')}")['status']).to eq('Ready')
    expect(requests.size).to eq(4)
  ensure
    host&.close
    peer&.close
    server&.join
    FileUtils.remove_entry(directory) if directory && File.directory?(directory)
  end
end
