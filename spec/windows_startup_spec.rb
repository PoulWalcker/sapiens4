require_relative 'support/desktop'

RSpec.describe 'Windows desktop startup recovery', :windows, :desktop do
  {
    'missing runtime' => 'Some Sapiens4 application files are missing',
    'server exiting before readiness' => 'app.js'
  }.each do |scenario, message|
    it "shows #{scenario} and closes without leaving a running launcher" do
      skip 'Windows desktop startup contract' unless Gem.win_platform?
      directory = Dir.mktmpdir('sapiens-desktop-startup-')
      begin
        runtime = File.join(directory, 'runtime')
        FileUtils.mkdir_p(File.join(runtime, '.build/bin'))
        FileUtils.mkdir_p(File.join(runtime, '.build/web'))
        FileUtils.mkdir_p(File.join(runtime, 'app'))
        FileUtils.cp_r(File.join(Contract::ROOT, 'app/shell'), File.join(runtime, 'app'))
        FileUtils.cp_r(File.join(Contract::ROOT, 'app/theme'), File.join(runtime, 'app'))
        FileUtils.cp(File.join(Contract::ROOT, '.build/web/guest.js'), File.join(runtime, '.build/web'))
        if scenario == 'server exiting before readiness'
          # The backend must report the missing workspace asset to the recovery screen.
          FileUtils.cp(File.join(Contract::ROOT, '.build/bin/sapiens4.exe'),
            File.join(runtime, '.build/bin/sapiens4.exe'))
        end
        process = Contract::Child.new(File.join(Contract::ROOT, '.build/bin/sapiens-windows.exe'),
          '--runtime-dir', runtime, '--data-dir', File.join(directory, 'data'),
          env: {'SAPIENS_CONFIG_DIR' => File.join(directory, 'config')})
        begin
          @desktop = Desktop.new(process: process, ready: false)
          desktop.see(message)
          expect(File).not_to exist(File.join(directory, 'config/storage.json'))
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
