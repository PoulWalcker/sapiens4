# Permission boundary for the native journey; never invokes a real OS prompt.
require 'json'
root = ENV.fetch('SAPIENS_ONBOARDING_FIXTURE')
if ARGV.include?('focused')
  puts JSON.generate(code: File.exist?(File.join(root, 'permission-granted')) ? 'focus_unavailable' : 'accessibility_permission_denied')
  exit 77
end
if ARGV == ['request-permission']
  File.write(File.join(root, 'permission-requested'), '')
  exit
end
abort "Unexpected onboarding subprocess: #{ARGV.inspect}"
