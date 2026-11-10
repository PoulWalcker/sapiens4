require_relative 'spec_helper'
require 'digest'

RSpec.describe 'Explicit storage selection' do
  it 'rejects StartNew on populated data and attaches without altering rejected data' do
    host = Host.new(start: false)
    snapshot = -> { Dir[File.join(host.root, '**', '*')].select { |p| File.file?(p) }.to_h { |p| [p, Digest::SHA256.file(p).hexdigest] } }
    original = snapshot.call
    _, error, code = Contract.capture(Contract::BINARY, 'check-data', host.root, 'new')
    expect(code).not_to eq(0)
    expect(error).to include('empty')
    expect(snapshot.call).to eq(original)
    _, error, code = Contract.capture(Contract::BINARY, 'check-data', host.root, 'existing')
    expect([code, error]).to eq([0, ''])
    # SQLite may create transient WAL readers' sidecars even in read-only mode.
    expect(snapshot.call.slice(*original.keys)).to eq(original)
    expect((snapshot.call.keys - original.keys).all? { |p| p.end_with?('-shm', '-wal') }).to eq(true)
    original = snapshot.call
    # The actual startup repeats validation under its own lease.
    _, error, code = Contract.capture(Contract::BINARY, '--port', '0', '--data-dir', host.root, '--desktop',
      env: host.environment.merge('SAPIENS_STORAGE_MODE' => 'new'), input: "\n")
    expect(code).not_to eq(0)
    expect(error).to include('empty')
    expect(snapshot.call).to eq(original)
    host.start
    _, error, code = Contract.capture(Contract::BINARY, 'check-data', host.root, 'existing')
    expect(code).not_to eq(0)
    expect(error).to include('Another Sapiens4')
    host.stop
    _, _, code = Contract.capture(Contract::BINARY, 'check-data', host.root, 'existing')
    expect(code).to eq(0)
  ensure
    host&.close
  end

  it 'never initializes a missing or empty AttachExisting directory' do
    Dir.mktmpdir('sapiens-storage-') do |root|
      missing = File.join(root, 'missing')
      _, _, code = Contract.capture(Contract::BINARY, 'check-data', missing, 'existing')
      expect(code).not_to eq(0)
      expect(File.exist?(missing)).to eq(false)
      Dir.mkdir(missing)
      _, _, code = Contract.capture(Contract::BINARY, 'check-data', missing, 'existing')
      expect(code).not_to eq(0)
      expect(Dir.children(missing) - ['host.lock']).to be_empty
    end
  end
end
