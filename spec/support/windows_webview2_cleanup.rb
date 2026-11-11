require 'time'
require 'digest'
require_relative 'desktop'
raise 'Windows required' unless Gem.win_platform?
$stdout.sync = true
ROOT = File.expand_path(ENV.fetch('D1_CLEANUP_EVIDENCE'))
SETUP = File.expand_path(ENV.fetch('D1_CLEANUP_SETUP'))
FLAGS = %w[/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP-]
FileUtils.mkdir_p(ROOT)
def ps(script)
  out, err, code = Contract.capture('powershell.exe', '-NoProfile', '-NonInteractive', '-Command', script, timeout: 30)
  raise "PowerShell #{code}: #{err}" unless code == 0
  out.delete_prefix("\uFEFF").strip
end
ps(<<~'PS')
  $ErrorActionPreference='Stop'
  $p=New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
  if($p.IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator)){throw 'Use unelevated session'}
  foreach($v in 'Registry32','Registry64'){
    $k=[Microsoft.Win32.RegistryKey]::OpenBaseKey('CurrentUser',$v)
    if($k.OpenSubKey('Software\Microsoft\Windows\CurrentVersion\Uninstall\com.sapiens4.desktop_is1')){throw 'Existing installation'}
  }
PS
File.write(File.join(ROOT,'identity.txt'), "source=#{ENV['D1_CLEANUP_REVISION']}\nsetup=#{SETUP}\nsha256=#{Digest::SHA256.file(SETUP).hexdigest}\n") if defined?(Digest)
failed = false
ENV.fetch('D1_CLEANUP_CASES', 'A B C D E').split.each do |label|
  raise 'Unknown case' unless %w[A B C D E].include?(label)
  dir = File.join(ROOT, "case-#{label}")
  raise "Existing evidence: #{dir}" if File.exist?(dir)
  FileUtils.mkdir_p(dir)
  data, install, config = %w[data install config].map { |n| File.join(dir,n) }
  profile = File.join(data,'WebView2','Shell')
  env = {'SAPIENS_DATA_DIR'=>data, 'SAPIENS_CONFIG_DIR'=>config, 'CODEX_HOME'=>File.join(dir,'codex'),
    'SAPIENS_RUNTIME_DIR'=>nil,'SAPIENS_REPOSITORY'=>nil,'SAPIENS_CODEX_BINARY'=>ENV['SAPIENS_TEST_CODEX']}
  host = nil
  known = []
  snapshots = []
  snapshot = lambda do |stage|
    rows = JSON.parse(ps(<<~PS))
      $ErrorActionPreference='Stop'
      $rows=@(Get-CimInstance Win32_Process | Where-Object { $_.Name -in @('Sapiens4.exe','sapiens4.exe','msedgewebview2.exe') } | ForEach-Object {
        $p=Get-Process -Id $_.ProcessId -ErrorAction SilentlyContinue
        [pscustomobject]@{pid=$_.ProcessId;ppid=$_.ParentProcessId;created=$_.CreationDate.ToUniversalTime().ToString('o');name=$_.Name;exe=$_.ExecutablePath;command=$_.CommandLine;title=$p.MainWindowTitle}
      })
      ConvertTo-Json -Depth 4 -Compress -InputObject $rows
    PS
    owned = rows.select { |r| r['pid']==host&.pid || r['command'].to_s.tr('\\','/').include?(dir) || r['exe'].to_s.tr('\\','/').downcase.start_with?(install.downcase+'/') || known.include?([r['pid'],r['created']]) }
    known |= owned.map { |r| [r['pid'],r['created']] }
    entry = {at:Time.now.utc.iso8601(3),stage:stage,host_pid:host&.pid,profile:profile,processes:owned}
    snapshots << entry
    File.write(File.join(dir,'processes.json'),JSON.pretty_generate(snapshots))
    puts "#{label} #{stage}: #{owned.map { |r| "#{r['name']}:#{r['pid']}<-#{r['ppid']} #{r['title']}" }.join('; ')}"
    owned
  end
  run = lambda do |exe,*args|
    out,err,code=Contract.capture(exe,*args,env:env,chdir:dir,timeout:120)
    File.open(File.join(dir,'installer.log'),'a'){|f| f.puts([exe,*args].inspect,code,out,err)}
    raise "Installer failed: #{code}" unless code==0
  end
  begin
    snapshot.call('before-install')
    run.call(SETUP,*FLAGS,"/DIR=#{install.tr('/','\\')}","/LOG=#{File.join(dir,'setup.log')}")
    if %w[A B C].include?(label)
      FileUtils.mkdir_p(File.dirname(profile))
      File.write(profile,"D1-#{label}")
    end
    host=Contract::Child.new(File.join(install,'Sapiens4.exe'),env:env,chdir:dir)
    snapshot.call('launched')
    desktop=Desktop.new(process:host,ready:false)
    if %w[A B C].include?(label)
      Contract.wait(timeout:60,description:'native recovery'){desktop.text.include?('Sapiens4 could not start')}
      snapshot.call('first-recovery')
      if label=='B'
        puts 'B select Retry (still obstructed)'
        desktop.press('Retry')
        sleep 1
        Contract.wait(timeout:60,description:'second native recovery'){desktop.text.include?('Sapiens4 could not start')}
        snapshot.call('second-recovery')
      end
      if label=='C'
        File.unlink(profile)
        puts 'C remove obstruction; select Retry'
        desktop.press('Retry')
        desktop.see('Message ChiefSapi')
        snapshot.call('recovered-startup')
        desktop.key('alt+F4')
      else
        puts "#{label} select Cancel"
        desktop.press('Cancel')
      end
    else
      desktop.see('Message ChiefSapi')
      snapshot.call('successful-startup')
      desktop.key('alt+F4')
    end
    out,err,code=host.finish(35)
    File.write(File.join(dir,'host.log'),"exit=#{code}\n#{out}\n#{err}")
    raise "Host exit #{code}" unless code==0
    sleep 5
    after=snapshot.call('five-seconds-after-host-exit')
    if label=='E'
      run.call(SETUP,*FLAGS,"/DIR=#{install.tr('/','\\')}","/LOG=#{File.join(dir,'reinstall.log')}")
      snapshot.call('reinstalled')
    end
    run.call(File.join(install,'unins000.exe'),*FLAGS,"/LOG=#{File.join(dir,'uninstall.log')}")
    Contract.wait(timeout:25,description:'uninstaller self-removal'){!File.exist?(File.join(install,'unins000.exe'))}
    sleep 5
    survivors=snapshot.call('five-seconds-after-uninstall')
    failed ||= survivors.any?
    File.write(File.join(dir,'result.json'),JSON.pretty_generate({case:label,host_pid:host.pid,host_exited:true,backend_survived:survivors.any?{|r|r['name'].downcase=='sapiens4.exe'},webview_survivors:survivors.select{|r|r['name']=='msedgewebview2.exe'},uninstall_completed:true}))
    survivors.select{|r|r['name']=='msedgewebview2.exe' && r['command'].to_s.tr('\\','/').include?(profile)}.each do |r|
      out,err,code=Contract.capture(Desktop.driver,'tree','--pid',r['pid'].to_s,'--depth','20',chdir:File.dirname(Desktop.driver),timeout:20)
      File.write(File.join(dir,"survivor-#{r['pid']}-window.json"),out+err)
      # Close only the observed error window, after preserving the failure.
      ps("$p=Get-Process -Id #{r['pid']} -ErrorAction SilentlyContinue; if($p -and $p.StartTime.ToUniversalTime().ToString('o') -eq '#{r['created']}'){$p.CloseMainWindow() | Out-Null}")
    end
    sleep 2
    snapshot.call('after-owned-error-window-close')
  rescue Exception=>e
    File.write(File.join(dir,'failure.txt'),"#{e.full_message}")
    snapshot.call('harness-failure') rescue nil
    raise
  end
end
puts 'D1 MATRIX COMPLETE (see per-case process verdicts)'
exit(failed ? 1 : 0)
