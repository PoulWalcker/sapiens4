require_relative 'process'

class Host
  attr_reader :root, :url, :environment, :provider, :process

  def initialize(start: true, environment: {}, repo: Contract::ROOT, command: nil)
    @directory = File.realpath(Dir.mktmpdir('sapiens-contract-'))
    @root, @repo = File.join(@directory, "workspace's é"), repo
    @command = command
    FileUtils.mkdir_p(File.join(root, 'calls'))
    @provider = {}
    configure
    if Gem.win_platform?
      binary = ENV.fetch('SAPIENS_TEST_CODEX')
    else
      binary = File.join(root, 'codex')
      File.write(binary, "#!#{RbConfig.ruby}\nload #{File.join(__dir__, 'provider.rb').inspect}\n")
      File.chmod(0755, binary)
    end
    @environment = {'SAPIENS_CODEX_BINARY' => binary, 'SAPIENS_CONTRACT_ENV' => root,
      'SAPIENS_CONTRACT_RUBY' => RbConfig.ruby, 'SAPIENS_CONTRACT_PROVIDER' => File.join(__dir__, 'provider.rb'),
      'NO_COLOR' => '1', 'SAPIENS_WHISPER_URL' => 'http://127.0.0.1:9', 'SAPIENS_CODEX_MODEL' => 'gpt-6-sol',
      'SAPIENS_CODEX_REASONING_EFFORT' => 'high'}.merge(environment)
    self.start if start
  rescue Exception
    close
    raise
  end

  def configure(**options)
    @provider.merge!(options.transform_keys(&:to_s))
    Contract.atomic_json(File.join(root, 'provider.json'), @provider)
  end

  def start
    @process = Contract::Child.new(*(@command || [File.join(@repo, '.build/bin', Gem.win_platform? ? 'sapiens4.exe' : 'sapiens4')]), '--port', '0',
      '--data-dir', root, '--desktop', env: environment, chdir: @repo)
    Contract.wait(description: 'host startup: ' + root) do
      match = @process.stdout.match(%r{Sapiens4: (http://127.0.0.1:\d+)})
      @url = match[1] if match
    end
    self
  rescue Exception => error
    raise "#{error.message}\n#{@process && @process.stderr}"
  end

  def stop
    return unless @process
    @process.input.close unless @process.input.closed?
    _, error, code = @process.finish
    raise "Host shutdown failed (#{code}): #{error}" unless code == 0
  ensure
    @process.close if @process
    @process = nil
  end

  def crash
    @process.signal('KILL')
    @process.close
    @process = nil
  end

  def restart; stop; start; end

  def close
    release
    stop
  ensure
    if @directory
      # WebView2 may release its temporary profile shortly after the window exits.
      Contract.wait(timeout: 10, description: "fixture profile release: #{@directory}") do
        begin
          FileUtils.remove_entry(@directory) if File.exist?(@directory)
          true
        rescue Errno::EACCES, Errno::ENOTEMPTY, Errno::ENOENT
          raise unless Gem.win_platform?
          false
        end
      end
    end
  end

  def request(method, path, body = nil, headers = {})
    Contract.http(url + path, method, body, headers)
  end

  def api(method, path, body = nil, status: 200, **fields)
    body = fields unless fields.empty?
    body = {} if body.nil? && %w[POST PUT].include?(method)
    code, value = request(method, path, body)
    raise "#{method} #{path}: expected #{status}, received #{code}: #{value.inspect}" unless code == status
    value
  end

  def state; api('GET', '/api/state'); end
  def agent(name = 'Researcher', **fields)
    api('POST', '/api/agents', {name: name, role: 'Research'}.merge(fields), status: 201).fetch('id')
  end
  def control(owner, op, **fields)
    api('POST', "/api/agents/#{owner}/control", {op: op}.merge(fields))
  end
  def submit(owner, text = 'Compare the products.', **fields)
    api('POST', "/api/agents/#{owner}/messages", {text: text}.merge(fields), status: 202).fetch('id')
  end
  def turn(id, status = 'done', timeout: 30)
    Contract.wait(timeout: timeout, description: "turn #{id} -> #{status}; calls: #{calls.map { |c| c['node'] }}") do
      row = state.fetch('turns').find { |item| item['id'] == id }
      raise "Turn failed unexpectedly: #{row.inspect}" if status == 'done' && row && %w[failed interrupted cancelled].include?(row['status'])
      row if row && Array(status).include?(row['status'])
    end
  end
  def idle
    Contract.wait(description: 'all work to settle') do
      snapshot = state
      snapshot if snapshot['turns'].none? { |t| %w[queued running output_pending].include?(t['status']) } &&
        snapshot.fetch('workloads', []).none? { |work| %w[Queued Running].include?(work['state']) } &&
        snapshot['groups'].all? { |g| g['requests'].all? { |r| r['settled'] } }
    end
  end
  def calls
    Dir[File.join(root, 'calls', '*.json')].map { |file| JSON.parse(File.read(file)) }.sort_by { |c| c['started'] }
  end
  def release(gate = 'released'); File.write(File.join(root, gate), '') if root && File.directory?(root); end
  def workspace(owner); control(owner, 'status').fetch('notes').fetch('workspace'); end
end
