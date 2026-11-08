RSpec.describe 'Installed native package' do
  it 'verifies final bytes and serves its assets across a clean restart with stable identity' do
    skip 'Set SAPIENS_TEST_PACKAGE_RUNTIME to a packaged runtime' unless ENV['SAPIENS_TEST_PACKAGE_RUNTIME']
    runtime = File.realpath(ENV.fetch('SAPIENS_TEST_PACKAGE_RUNTIME'))
    manifest = JSON.parse(File.read(File.join(runtime, 'package-manifest.json')))
    expect(manifest).to include('appId' => 'com.poulwalcker.sapiens4.development',
      'channel' => 'development', 'repository' => 'PoulWalcker/sapiens4',
      'updateDiscovery' => false, 'packageStatus' => 'provider-complete')
    python = Gem.win_platform? ? 'python' : 'python3'
    _, error, code = Contract.capture(python, 'build/package/package.py', 'verify', '--runtime', runtime, timeout: 120)
    expect(code).to eq(0), error
    version, error, code = Contract.capture(File.join(runtime, manifest.fetch('provider').fetch('entrypoint')), '--version',
      env: {'PATH' => Gem.win_platform? ? File.join(ENV.fetch('SystemRoot'), 'System32') : '/usr/bin:/bin:/usr/sbin:/sbin'})
    expect(code).to eq(0), error
    expect(version.strip).to eq('codex-cli ' + manifest.fetch('provider').fetch('version'))
    output, error, code = Contract.capture(python, 'build/package/smoke.py', '--runtime', runtime, timeout: 120)
    expect(code).to eq(0), error
    launches = JSON.parse(output)
    expect(launches.map { |launch| launch.fetch('iteration') }).to eq([1, 2])
    expect(launches.map { |launch| launch.fetch('identitySha256') }.uniq.length).to eq(1)
    launches.each do |launch|
      expect(launch.fetch('exitCode')).to eq(0), launch.fetch('stderr')
      expect(launch.fetch('routes').map { |route| route.fetch('path') }).to eq(
        %w[/api/health /workspace/ /workspace/app.js /workspace/guest.js /remote-vendor.js /workspace/styles.css])
      launch.fetch('routes').each do |route|
        expect(route.fetch('status')).to eq(200), route.fetch('path')
        expect(route.fetch('bytes')).to be > 0
      end
    end
  end
end
