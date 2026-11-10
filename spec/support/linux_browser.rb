require_relative 'process'

# Launch the installed browser in its own profile so its PID identifies the
# window used by the native accessibility driver.
class LinuxBrowser
  attr_reader :pid

  def initialize(engine, url)
    @profile = Dir.mktmpdir("sapiens-#{engine}-")
    binary = ENV.fetch("SAPIENS_TEST_#{engine.upcase}_EXECUTABLE", engine)
    args = case engine
    when 'chromium'
      ["--user-data-dir=#{@profile}", "--ozone-platform=x11",
       '--no-first-run', '--no-default-browser-check', '--force-renderer-accessibility', '--disable-gpu']
    when 'firefox'
      File.write(File.join(@profile, 'user.js'), "user_pref(\"accessibility.force_disabled\", -1);\n")
      ['--no-remote', '--profile', @profile]
    else
      raise ArgumentError, "Unknown browser #{engine}"
    end
    @process = Contract::Child.new(binary, *args, url, env: {
      'MOZ_ENABLE_WAYLAND' => '0', 'ACCESSIBILITY_ENABLED' => '1', 'LIBGL_ALWAYS_SOFTWARE' => '1'
    })
    @pid = @process.pid
  rescue StandardError
    close
    raise
  end

  def close
    @process&.signal('TERM')
    @process&.close
  ensure
    FileUtils.remove_entry(@profile) if @profile && File.directory?(@profile)
  end
end
