require_relative 'support/package'

RSpec.describe 'Installed native package' do
  include PackageExamples

  it 'verifies final bytes and serves its assets across a clean restart with stable identity' do
    skip 'Set SAPIENS_TEST_PACKAGE_RUNTIME to a packaged runtime' unless ENV['SAPIENS_TEST_PACKAGE_RUNTIME']
    verify_native_runtime(File.realpath(ENV.fetch('SAPIENS_TEST_PACKAGE_RUNTIME')))
  end
end
