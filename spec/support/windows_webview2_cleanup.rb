require 'time'
require_relative 'desktop'

# Correlate PIDs with creation times so PID reuse cannot identify a survivor.
class WindowsWebViewProcesses
  def initialize(dir, label)
    @dir, @label = dir, label
    @profile = File.join(dir, 'config/WebView2/Shell')
    @known, @snapshots = [], []
  end

  def powershell(script)
    out, err, code = Contract.capture('powershell.exe', '-NoProfile', '-NonInteractive', '-Command', script)
    raise "PowerShell #{code}: #{err}" unless code == 0
    out.delete_prefix("\uFEFF").strip
  end

  def snapshot(stage, host_pid = nil)
    @host_pid = host_pid if host_pid
    rows = JSON.parse(powershell(<<~'PS'))
      $ErrorActionPreference='Stop'
      $rows=@(Get-CimInstance Win32_Process | Where-Object { $_.Name -in @('Sapiens4.exe','sapiens4.exe','msedgewebview2.exe') } | ForEach-Object {
        $p=Get-Process -Id $_.ProcessId -ErrorAction SilentlyContinue
        [pscustomobject]@{pid=$_.ProcessId;ppid=$_.ParentProcessId;created=$_.CreationDate.ToUniversalTime().ToString('o');name=$_.Name;exe=$_.ExecutablePath;command=$_.CommandLine;title=$p.MainWindowTitle}
      })
      ConvertTo-Json -Depth 4 -Compress -InputObject $rows
    PS
    owned = rows.select do |row|
      row['pid'] == host_pid || @known.include?([row['pid'], row['created']]) ||
        row['command'].to_s.tr('\\', '/').downcase.include?(@dir.downcase + '/') ||
        row['exe'].to_s.tr('\\', '/').downcase.start_with?(@dir.downcase + '/install/')
    end
    @known |= owned.map { |row| [row['pid'], row['created']] }
    @snapshots << {at: Time.now.utc.iso8601(3), stage: stage, host_pid: @host_pid, profile: @profile, processes: owned}
    File.write(File.join(@dir, 'processes.json'), JSON.pretty_generate(@snapshots))
    owned
  end

  def after_uninstall
    sleep 5
    survivors = snapshot('five-seconds-after-uninstall')
    webviews = survivors.select { |row| row['name'].casecmp?('msedgewebview2.exe') }
    result = {case: @label, host_pid: @host_pid, host_exited: true, uninstall_completed: true,
      backend_survived: survivors.any? { |row| row['name'].casecmp?('sapiens4.exe') }, webview_survivors: webviews}
    File.write(File.join(@dir, 'result.json'), JSON.pretty_generate(result))
    webviews.each do |row|
      next unless row['command'].to_s.tr('\\', '/').downcase.include?(@profile.downcase)
      pid = Integer(row.fetch('pid'))
      out, err, = Contract.capture(Desktop.driver, 'tree', '--pid', pid.to_s, '--depth', '20', chdir: File.dirname(Desktop.driver))
      File.write(File.join(@dir, "survivor-#{pid}-window.json"), out + err)
      powershell("$c=Get-CimInstance Win32_Process -Filter 'ProcessId=#{pid}'; if($c -and $c.CreationDate.ToUniversalTime().ToString('o') -eq '#{row.fetch('created')}'){ (Get-Process -Id #{pid} -ErrorAction SilentlyContinue).CloseMainWindow() | Out-Null }")
    end
    unless survivors.empty?
      sleep 2
      raise "Fixture processes remain in #{@dir}" unless snapshot('after-owned-error-window-close').empty?
    end
    survivors
  end
end
