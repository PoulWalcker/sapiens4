require_relative 'support/desktop'

RSpec.describe 'Windows desktop startup recovery', :windows, :desktop do
  {
    'missing runtime' => 'Haskell runtime is missing',
    'server exiting before readiness' => 'Sapiens4 server stopped'
  }.each do |scenario, message|
    it "shows #{scenario} and closes without leaving a running launcher" do
      skip 'Windows desktop startup contract' unless Gem.win_platform?
      directory = Dir.mktmpdir('sapiens-desktop-startup-')
      begin
        runtime = File.join(directory, 'runtime')
        FileUtils.mkdir_p(File.join(runtime, '.build/bin'))
        if scenario == 'server exiting before readiness'
          # This real executable rejects the host arguments and exits immediately.
          FileUtils.cp(File.join(Contract::ROOT, '.build/domain/sapiens-domain.exe'),
            File.join(runtime, '.build/bin/sapiens4.exe'))
        end
        process = Contract::Child.new(File.join(Contract::ROOT, '.build/bin/sapiens-windows.exe'),
          '--runtime-dir', runtime, '--data-dir', File.join(directory, 'data'))
        begin
          @desktop = Desktop.new(process: process, ready: false)
          expect(desktop.see(message)).to include('Sapiens4 could not start')
          desktop.key('alt+F4')
          _, error, code = process.finish(10)
          expect(code).to eq(0), error
        ensure
          process.close
        end
      ensure
        # Match Host's bounded cleanup: Windows may release image files after exit.
        Contract.wait(timeout: 10, description: "startup fixture release: #{directory}") do
          begin
            FileUtils.remove_entry(directory) if File.exist?(directory)
            true
          rescue Errno::EACCES, Errno::ENOTEMPTY, Errno::ENOENT
            false
          end
        end
      end
    end
  end
end
