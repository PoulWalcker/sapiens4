require 'digest'

RSpec.describe 'Storage validation and backend initialization' do
  after { @host&.close }

  def check(path, mode = 'existing')
    Contract.capture(Contract::BINARY, 'check-data', path, mode)
  end

  def fingerprint(root)
    Dir.glob(File.join(root, '**', '*'), File::FNM_DOTMATCH).select { |path| File.file?(path) && !path.end_with?('host.lock', 'corpora.sqlite3-wal', 'corpora.sqlite3-shm') }
      .to_h { |path| [path.delete_prefix(root), Digest::SHA256.file(path).hexdigest] }
  end

  it 'rejects missing, partial, locked, incompatible and corrupt stores without rewriting existing data, then retries safely' do
    @host = Host.new(start: false)
    Dir.mktmpdir('sapiens-storage-') do |directory|
      missing = File.join(directory, 'missing')
      expect(check(missing).last).to eq(1)
      expect(File.exist?(missing)).to be(false)
      invalid = File.join(directory, 'invalid')
      Dir.mkdir(invalid)
      File.write(File.join(invalid, 'keep.txt'), 'preserve me')
      before = fingerprint(invalid)
      expect(check(invalid).last).to eq(1)
      expect(check(invalid, 'new').last).to eq(1)
      expect(fingerprint(invalid)).to eq(before)
      identity = File.binread(File.join(host.root, 'corpora-identity.json'))
      database = File.join(host.root, 'corpora.sqlite3')
      bytes = File.binread(database)
      # Version is in SQLite's big-endian file header; no SQLite test gem needed.
      unsupported = bytes.dup
      unsupported[60, 4] = [999].pack('N')
      File.binwrite(database, unsupported)
      before = fingerprint(host.root)
      expect(check(host.root)[1]).to include('Unsupported Corpora schema')
      expect(fingerprint(host.root)).to eq(before)
      File.binwrite(database, 'partial SQLite file')
      expect(check(host.root)[1]).to include('database is invalid')
      expect(File.binread(database)).to eq('partial SQLite file')
      File.binwrite(database, bytes)
      host.start
      expect(check(host.root)[1]).to include('Another Sapiens4 server')
      host.stop
      expect(check(host.root).last).to eq(0)
      expect(File.binread(File.join(host.root, 'corpora-identity.json'))).to eq(identity)
      host.start
      expect(host.state['agents']).not_to be_empty
    end
  end

  it 'attaches every supported schema and uses the existing initializer and legacy jobs import without changing Corpora identity' do
    @host = Host.new
    turn = host.submit(chief, 'Preserve this completed turn through migration')
    host.turn(turn)
    original = host.state
    host.stop
    database = File.join(host.root, 'corpora.sqlite3')
    # The fixture uses the same SQLite binding declared in libs4's test bundle.
    require 'sqlite3'
    (0..7).each do |version|
      SQLite3::Database.new(database) do |db|
        if [1, 2].include?(version)
          db.execute('DROP TABLE IF EXISTS jobs')
          db.execute('CREATE TABLE jobs AS SELECT * FROM turns')
          db.execute('DELETE FROM turns')
        end
        db.execute("PRAGMA user_version=#{version}")
      end
      before = fingerprint(host.root)
      expect(check(host.root).last).to eq(0)
      expect(fingerprint(host.root)).to eq(before)
      host.start
      snapshot = host.state
      expect(snapshot['corpora']['id']).to eq(original['corpora']['id'])
      expect(snapshot['agents'].map { |a| a['id'] }).to eq(original['agents'].map { |a| a['id'] })
      expect(snapshot['turns']).to include(hash_including('id' => turn, 'status' => 'done'))
      host.stop
      expect(File.binread(database)[60, 4].unpack1('N')).to eq(8)
    end
  end
end
