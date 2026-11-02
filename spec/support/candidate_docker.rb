require 'fileutils'
require 'json'
require 'open3'

module CandidateDocker
  def install_create_ack_gate
    @docker_gate = File.join(host.root, 'create-ack-release')
    @docker_created = File.join(host.root, 'created-container')
    @docker_started = File.join(host.root, 'unexpected-container-start')
    wrapper = File.join(host.root, 'controlled-docker')
    File.write(wrapper, <<~RUBY)
      #!#{RbConfig.ruby}
      require 'open3'
      if ARGV.include?('create')
        output, error, status = Open3.capture3(#{@docker.inspect}, *ARGV)
        if status.success?
          File.write(#{@docker_created.inspect}, output.strip)
          sleep 0.02 until File.exist?(#{@docker_gate.inspect})
        end
        STDOUT.write(output)
        STDERR.write(error)
        exit(status.exitstatus)
      end
      File.write(#{@docker_started.inspect}, '') if ARGV.include?('start')
      exec #{@docker.inspect}, *ARGV
    RUBY
    File.chmod(0755, wrapper)
    host.environment['SAPIENS_GENERATED_DOCKER'] = wrapper
    host.restart
  end

  def modify_preparation(id)
    SQLite3::Database.open(File.join(host.root, 'corpora.sqlite3')) do |db|
      db.transaction do
        value = JSON.parse(db.get_first_value('SELECT value FROM workloads WHERE id=?', id))
        yield value.fetch('preparation')
        db.execute('UPDATE workloads SET value=? WHERE id=?', [JSON.generate(value), id])
      end
    end
  end
end
