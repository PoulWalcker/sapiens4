require 'open3'
require 'json'
RSpec.describe 'Linux Chromium navigation and accessibility regression' do
  it 'survives repeated guarded navigation and immediate accessibility reads' do
    skip 'Set SAPIENS_TEST_CHROME_PID to an isolated disposable Chrome profile' unless ENV['SAPIENS_TEST_CHROME_PID']
    pid = Integer(ENV.fetch('SAPIENS_TEST_CHROME_PID'))
    driver = File.expand_path('../submodules/blindly4/.build/release/blindly4', __dir__)
    run = lambda do |*args|
      out, err, status = Open3.capture3({'BLINDLY4_LINUX_TEXT_INPUT' => 'keyboard'}, driver, *args.map(&:to_s))
      expect(status.success?).to be(true), "#{args.inspect}: #{out}\n#{err}"
      out
    end
    3.times do |cycle|
      ['https://example.com/', 'https://web.whatsapp.com/'].each do |url|
        run.call('activate', '--pid', pid)
        fields = JSON.parse(run.call('find', '--pid', pid, '--role', 'AXTextField', '--depth', 14)).fetch('matches')
        field = fields.find { |row| row['description'] == 'Address and search bar' }
        expect(field).not_to be_nil
        path = field.fetch('path')
        run.call('focus', '--pid', pid, '--path', path)
        run.call('set-value', '--pid', pid, '--path', path, '--value', url)
        expect(JSON.parse(run.call('inspect', '--pid', pid, '--path', path))['value']).to eq(url)
        run.call('key', '--pid', pid, '--key', 'return', '--target-path', path, '--require-value', url)
        expect(run.call('show', '--pid', pid, '--depth', 12)).to include('AXWindow')
        sleep 2
        expect(run.call('show', '--pid', pid, '--depth', 18)).to include('AXWebArea')
        expect(JSON.parse(run.call('apps')).fetch('apps').map { |app| app['pid'] }).to include(pid)
        warn "Navigation cycle #{cycle + 1}: #{url} passed"
      end
    end
  end
end
