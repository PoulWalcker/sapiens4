require 'securerandom'

RSpec.describe 'Ubuntu development package' do
  it 'installs, reinstalls, upgrades and rolls back supplied versions without losing user data' do
    administration = {'PATH' => '/usr/sbin:/usr/bin:/sbin:/bin'}
    skip 'Disposable package-test filesystem required' unless ENV['SAPIENS_TEST_LINUX_PACKAGE_LIFECYCLE'] == '1' && File.file?('/run/sapiens-package-test')
    package = ENV.fetch('SAPIENS_TEST_PACKAGE')
    expect(Contract.capture('dpkg-query', '-W', 'sapiens4').last).not_to eq(0)
    data = File.join(Dir.home, '.local/share/sapiens4')
    FileUtils.mkdir_p(data)
    marker = File.join(data, 'qualification-marker')
    File.write(marker, 'Preserve user data')
    previous = ENV['SAPIENS_TEST_PREVIOUS_PACKAGE']
    versions = [previous, package].compact.to_h do |file|
      output, error, status = Contract.capture('dpkg-deb', '-f', file, 'Version')
      expect(status).to eq(0), output + error
      [file, output.strip]
    end
    if previous
      expect(Contract.capture('dpkg', '--compare-versions', versions.fetch(previous), 'lt', versions.fetch(package)).last).to eq(0)
    end
    sequence = previous ? [previous, package, package, previous, package] : [package, package]
    fixture = backup = identity = owner = turn = artifact = nil
    sequence.each do |candidate|
      output, error, status = Contract.capture('apt-get', '-o', 'APT::Sandbox::User=root', '--yes', '--no-install-recommends',
        '--allow-downgrades', '--reinstall', 'install', candidate, timeout: 300, env: administration)
      expect(status).to eq(0), output + error
      output, error, status = Contract.capture('dpkg-query', '-W', '-f=${Version}', 'sapiens4')
      expect(status).to eq(0), output + error
      expect(output).to eq(versions.fetch(candidate))
      output, error, status = Contract.capture('/usr/bin/sapiens4', '--help')
      expect(status).to eq(0), error
      expect(output).to include('Usage: sapiens-linux')
      if fixture
        fixture.start
      else
        fixture = Host.new(repo: '/opt/sapiens4', command: ['/opt/sapiens4/.build/bin/sapiens4'])
        owner = fixture.agent('RetainedResearcher')
        turn = fixture.submit(owner, 'Retain this completed work through package changes.')
        fixture.turn(turn)
        artifact = File.join(fixture.workspace(owner), 'retained.txt')
        File.write(artifact, 'Saved research')
        identity = fixture.api('GET', '/api/corpora')
        fixture.api('PUT', '/api/corpora/lifecycle', action: 'pause', revision: identity.fetch('revision'), operation: SecureRandom.uuid)
      end
      expect(fixture.api('GET', '/api/corpora')).to include('id' => identity.fetch('id'), 'status' => 'paused')
      expect(fixture.state.fetch('agents')).to include(hash_including('id' => owner, 'name' => 'RetainedResearcher'))
      expect(fixture.turn(turn).fetch('output')).to eq('Contract test reply')
      expect(fixture.calls.length).to eq(1)
      expect(File.read(artifact)).to eq('Saved research')
      fixture.stop
      unless backup
        backup = Dir.mktmpdir('sapiens-offline-backup-')
        FileUtils.cp_r(fixture.root, File.join(backup, 'corpus'))
      end
      expect(File.read(marker)).to eq('Preserve user data')
    end
    FileUtils.remove_entry(fixture.root)
    FileUtils.cp_r(File.join(backup, 'corpus'), fixture.root)
    fixture.start
    expect(fixture.api('GET', '/api/corpora')).to include('id' => identity.fetch('id'), 'status' => 'paused')
    expect(fixture.turn(turn).fetch('output')).to eq('Contract test reply')
    expect(File.read(artifact)).to eq('Saved research')
    expect(fixture.calls.length).to eq(1)
    fixture.stop
    output, error, status = Contract.capture('dpkg', '--purge', 'sapiens4', timeout: 120, env: administration)
    expect(status).to eq(0), output + error
    expect(File).not_to exist('/usr/bin/sapiens4')
    expect(File.read(marker)).to eq('Preserve user data')
  ensure
    fixture&.close
    FileUtils.remove_entry(backup) if backup && File.directory?(backup)
    if marker
      Contract.capture('dpkg', '--purge', 'sapiens4', timeout: 120, env: administration)
      FileUtils.rm_f(marker)
    end
  end

  it 'contains a relocatable runtime without installation hooks' do
    skip 'Linux package validation required' unless ENV['SAPIENS_TEST_LINUX_PACKAGE'] == '1'
    Dir.mktmpdir('sapiens package ') do |directory|
      package = ENV['SAPIENS_TEST_PACKAGE'] || File.join(directory, 'sapiens4.deb')
      unless ENV['SAPIENS_TEST_PACKAGE']
        output, error, status = Contract.capture('sh', 'environments/linux/package.sh', package, timeout: 120)
        expect(status).to eq(0), output + error
      end
      contents = File.join(directory, 'unpacked')
      _, error, status = Contract.capture('dpkg-deb', '--raw-extract', package, contents)
      expect(status).to eq(0), error
      expect(File.stat(contents).mode & 0777).to eq(0755)
      expect(Dir.children(File.join(contents, 'DEBIAN'))).to contain_exactly('control')
      runtime = File.join(contents, 'opt/sapiens4')
      expect(File.executable?(File.join(runtime, '.build/domain/sapiens-domain'))).to eq(true)
      output, error, status = Contract.capture(File.join(runtime, '.build/bin/sapiens-linux'), '--help')
      expect(status).to eq(0), error
      expect(output).to include('Usage: sapiens-linux')
      output, error, status = Contract.capture(File.join(runtime, 'submodules/blindly4/.build/release/blindly4'), 'schema')
      expect(status).to eq(0), error
      expect(JSON.parse(output).fetch('platform')).to eq('linux')
      _, error, status = Contract.capture('desktop-file-validate', File.join(contents, 'usr/share/applications/org.sapiens.Sapiens4.desktop'))
      expect(status).to eq(0), error
      if ENV['SAPIENS_TEST_LINUX_DESKTOP'] == '1'
        fixture = Host.new(start: false)
        desktop = Contract::Child.new(File.join(runtime, '.build/bin/sapiens-linux'), '--data-dir', fixture.root,
          env: fixture.environment.merge('SAPIENS_REPOSITORY' => nil), chdir: directory)
        url = nil
        Contract.wait(description: 'unpacked desktop readiness') { url = desktop.stdout[%r{READY (http://127.0.0.1:\d+)}, 1] }
        %w[/workspace/ /workspace/app.js /workspace/guest.js /workspace/styles.css].each do |path|
          expect(Contract.http(url + path, 'GET').first).to eq(200)
        end
      end
    ensure
      desktop&.signal('TERM')
      desktop&.close
      fixture&.close
    end
  end
end
