require 'json'
require 'net/http'
require 'open3'
require 'tmpdir'
require 'fileutils'
require 'timeout'
require 'rbconfig'

module Contract
  ROOT = File.expand_path('../..', __dir__)
  BINARY = File.join(ROOT, '.build/bin', Gem.win_platform? ? 'sapiens4.exe' : 'sapiens4')

  def self.wait(timeout: 20, description: 'condition')
    deadline = Process.clock_gettime(Process::CLOCK_MONOTONIC) + timeout
    loop do
      result = yield
      return result if result
      raise "Timed out waiting for #{description}" if Process.clock_gettime(Process::CLOCK_MONOTONIC) >= deadline
      sleep 0.05
    end
  end

  def self.http(url, method, body = nil, headers = {})
    uri = URI(url)
    request = Net::HTTP.const_get(method.capitalize).new(uri)
    unless body.nil?
      request['Content-Type'] = 'application/json'
      request['X-Sapiens-Local'] = '1'
      request.body = body.is_a?(String) ? body : JSON.generate(body)
    end
    headers.each { |key, value| request[key] = value }
    http = Net::HTTP.new(uri.host, uri.port, nil)
    http.open_timeout = http.read_timeout = 10
    response = http.request(request)
    payload = response['Content-Type'].to_s.include?('json') ? JSON.parse(response.body) : response.body
    [response.code.to_i, payload, response.to_hash]
  end

  def self.capture(*command, env: {}, input: '', timeout: 30, chdir: ROOT)
    process = Child.new(*command, env: env, chdir: chdir)
    process.input.write(input)
    process.input.close
    process.finish(timeout)
  ensure
    process.close if process
  end

  def self.atomic_json(path, data)
    File.write(path + '.new', JSON.generate(data))
    File.rename(path + '.new', path)
  end

  class Child
    attr_reader :input, :pid, :stdout, :stderr
    def initialize(*command, env: {}, chdir: ROOT)
      @input, out, err, @thread = Open3.popen3(env, *command, chdir: chdir)
      @pid, @stdout, @stderr = @thread.pid, +'', +''
      @readers = [[out, @stdout], [err, @stderr]].map do |io, buffer|
        Thread.new do
          begin
            loop { buffer << io.readpartial(8192) }
          rescue EOFError, IOError
          ensure
            io.close
          end
        end
      end
    end

    def finish(timeout = 15)
      raise "Process #{@pid} timed out: #{@stderr}" unless @thread.join(timeout)
      @readers.each(&:join)
      [@stdout.force_encoding('UTF-8'), @stderr.force_encoding('UTF-8'), @thread.value.exitstatus]
    end

    def signal(signal)
      Process.kill(signal, @pid) if @thread.alive?
    end

    def close
      @input.close unless @input.closed?
      unless @thread.join(10)
        if Gem.win_platform?
          system('taskkill', '/PID', @pid.to_s, '/T', '/F', out: File::NULL, err: File::NULL)
        else
          owned = [@pid]
          loop do
            owned.each { |pid| Process.kill('STOP', pid) rescue Errno::ESRCH }
            rows = IO.popen(['ps', '-axo', 'pid=,ppid='], &:read).lines.map { |line| line.split.map(&:to_i) }
            children = rows.select { |_, parent| owned.include?(parent) }.map(&:first) - owned
            break if children.empty?
            owned.concat(children)
          end
          owned.reverse_each { |pid| Process.kill('KILL', pid) rescue Errno::ESRCH }
        end
        @thread.join
      end
      @readers.each(&:join)
    end
  end
end
