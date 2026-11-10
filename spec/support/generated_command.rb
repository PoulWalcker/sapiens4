require_relative 'process'

# Execute the shell syntax published to the agent, including its quoted paths.
module GeneratedCommand
  def self.capture(workspace, kind, arguments, environment = {})
    file, pattern = case kind
    when 'control' then ['host-control.md', /\ARun (.+) 'JSON';/]
    when 'computer' then ['computer-use.md', /\A(?:Blindly4|Linux) UI wrapper: (.+)$/]
    else raise "Unknown generated command: #{kind}"
    end
    manifest = File.read(File.join(workspace, '../..', 'agentpy/agents', File.basename(workspace), 'manifests', file))
    launcher = manifest[pattern, 1]
    raise "Missing #{kind} launcher in #{file}" unless launcher
    output, error, status = Contract.capture('sh', '-c', "exec #{launcher} \"$@\"",
      'agent-command', *arguments, chdir: workspace, env: environment)
    {'stdout' => output, 'stderr' => error, 'status' => status}
  end
end
