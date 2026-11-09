require 'digest'

module PackageExamples
  def verify_native_runtime(runtime)
    manifest = JSON.parse(File.read(File.join(runtime, 'package-manifest.json')))
    expect(manifest).to include('appId' => 'com.poulwalcker.sapiens4.development',
      'channel' => 'development', 'repository' => 'PoulWalcker/sapiens4',
      'updateDiscovery' => false, 'packageStatus' => 'provider-complete')
    if manifest['platform'] == 'macos'
      expect(manifest.fetch('minimumOS')).to eq('13.0')
      compatibility = manifest.fetch('macOSCompatibility')
      expect(Gem::Version.new(compatibility.fetch('requiredMinimumOS'))).to be <= Gem::Version.new('13.0')
      expect(compatibility.fetch('targetOSAcceptance')).to eq('not-run')
    end
    _, error, code = Contract.capture('runghc', '-ibuild/package', 'build/Package.hs', 'verify', '--runtime', runtime, timeout: 120)
    expect(code).to eq(0), error
    restricted_path = Gem.win_platform? ? File.join(ENV.fetch('SystemRoot'), 'System32') : '/usr/bin:/bin:/usr/sbin:/sbin'
    version, error, code = Contract.capture(File.join(runtime, manifest.fetch('provider').fetch('entrypoint')), '--version', env: {'PATH' => restricted_path})
    expect(code).to eq(0), error
    expect(version.strip).to eq('codex-cli ' + manifest.fetch('provider').fetch('version'))
    Dir.mktmpdir('sapiens-package-smoke-') do |folder|
      data = File.join(folder, 'synthetic-data')
      env = ENV.keys.grep(/\A(SAPIENS_|CODEX_|OPENAI_)/).to_h { |key| [key, nil] }.merge(
        'SAPIENS_REPOSITORY' => runtime, 'SAPIENS_CONFIG_DIR' => File.join(folder, 'config'),
        'SAPIENS_DATA_DIR' => data, 'SAPIENS_CODEX_BINARY' => File.join(folder, 'provider-disabled'), 'PATH' => restricted_path)
      identity = nil
      2.times do
        process = Contract::Child.new(File.join(runtime, '.build/bin', Gem.win_platform? ? 'sapiens4.exe' : 'sapiens4'),
          '--port', '0', '--data-dir', data, '--desktop', env: env, chdir: folder)
        begin
          address = Contract.wait(timeout: 40, description: 'installed runtime readiness') do
            raise "Installed runtime exited before readiness: #{process.status.inspect}" if process.status
            process.stdout[/Sapiens4: (http:\/\/127\.0\.0\.1:\d+)/, 1]
          end
          %w[/api/health /workspace/ /workspace/app.js /workspace/guest.js /remote-vendor.js /workspace/styles.css].each do |route|
            status, body = Contract.http(address + route, 'GET')
            expect(status).to eq(200), route
            expect(body).not_to be_empty
          end
          current = Digest::SHA256.file(File.join(data, 'corpora-identity.json')).hexdigest
          identity ||= current
          expect(current).to eq(identity)
          process.input.write("stop\n")
          _, error, code = process.finish
          expect(code).to eq(0), error
        ensure
          failure = $!
          process.close
          if failure
            warn "Packaged runtime stdout:\n#{process.stdout}\nPackaged runtime stderr:\n#{process.stderr}"
          end
        end
      end
    end
  end
end
