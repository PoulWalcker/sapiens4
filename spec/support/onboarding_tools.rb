# External CLI boundary for the native onboarding journey; never opens a browser.
require 'json'

if ARGV.include?('focused')
  puts JSON.generate(code: 'accessibility_permission_denied')
  exit 77
end
if ARGV.include?('--version')
  puts 'codex-cli 0.158.0'
  exit
end

root = ENV.fetch('SAPIENS_ONBOARDING_FIXTURE')
auth = File.join(ENV.fetch('CODEX_HOME'), 'auth.json')
exit(File.exist?(auth) ? 0 : 1) if ARGV.include?('login') && ARGV.include?('status')
abort 'Unexpected external command in onboarding' unless ARGV.include?('login')
exit 1 if File.read(File.join(root, 'login-mode')) == 'fail'

File.write(File.join(root, 'login.pid'), Process.pid.to_s)
if ARGV.include?('--device-auth')
  $stdout.sync = true
  puts "Enter this one-time code\nABCD-EFGH"
else
  $stderr.sync = true
  warn 'https://auth.openai.com/oauth/authorize?response_type=code&state=onboarding-fixture'
end
sleep 0.02 until File.exist?(File.join(root, 'finish-login'))
File.write(auth, JSON.generate(account: 'onboarding-fixture'))
