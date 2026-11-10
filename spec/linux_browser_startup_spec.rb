require_relative 'support/http_peer'
require_relative 'support/linux_browser'

RSpec.describe 'Linux browser startup stability' do
  %w[chromium firefox].each do |engine|
    it "starts and closes #{engine} repeatedly without a retry" do
      skip 'Isolated browser stress session required' unless ENV['SAPIENS_TEST_BROWSER_STRESS'] == '1'
      runs = Integer(ENV.fetch('SAPIENS_TEST_BROWSER_RUNS', '20'))
      expect(runs).to be_between(1, 100)
      pages = HTTPPeer.new { |_| [200, '<title>Startup fixture</title>', {'Content-Type' => 'text/html'}] }
      runs.times do |index|
        browser = LinuxBrowser.new(engine, pages.url)
        Contract.wait(description: "#{engine} launch #{index + 1}") do
          pages.requests.count { |request| request[:path] == '/' } == index + 1
        end
      ensure
        browser&.close
      end
      expect(pages.requests.count { |request| request[:path] == '/' }).to eq(runs)
    ensure
      pages&.close
    end
  end
end
