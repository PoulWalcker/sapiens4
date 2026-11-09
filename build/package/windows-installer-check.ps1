param([Parameter(Mandatory)][string]$Installer, [Parameter(Mandatory)][string]$EvidenceDirectory)
$ErrorActionPreference = 'Stop'
$installerPath = (Resolve-Path -LiteralPath $Installer).Path
$evidence = [IO.Path]::GetFullPath($EvidenceDirectory)
New-Item -ItemType Directory -Path $evidence -Force | Out-Null
$work = Join-Path $env:TEMP ('sapiens-install-check-'+[Guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $work | Out-Null
$app = Join-Path $work 'installed app'
$state = Join-Path $work 'external state'
$registration = 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Uninstall\com.poulwalcker.sapiens4.development_is1'
$shortcut = Join-Path ([Environment]::GetFolderPath('Programs')) 'Sapiens4 Development.lnk'
$native = $null; $backendIds = @(); $result = [ordered]@{status='FAIL'; installer=$installerPath; checks=@(); statePreserved=$false}
function Run-Setup([string]$Program,[string[]]$Arguments,[bool]$Success,[string]$Name) {
    $log = Join-Path $evidence ($Name+'.log')
    $process = Start-Process -FilePath $Program -ArgumentList ($Arguments+('/LOG="'+$log+'"')) -PassThru
    if (-not $process.WaitForExit(180000)) { $process.Kill(); throw "$Name timed out; only owned installer terminated" }
    if (($process.ExitCode -eq 0) -ne $Success) { throw "$Name unexpected exit $($process.ExitCode); see $log" }
    $script:result.checks += @{name=$Name;exitCode=$process.ExitCode}
}
function Check-Inventory {
    & runghc -ibuild/package build/Package.hs verify --runtime (Join-Path $app 'runtime')
    if ($LASTEXITCODE -ne 0) { throw 'Installed runtime inventory failed' }
}
try {
    if (Test-Path $registration) { throw 'Existing development installation registration; use a disposable Windows test account' }
    if (Test-Path -LiteralPath $shortcut) { throw 'Existing development shortcut; refusing to replace it in this probe' }
    New-Item -ItemType Directory -Path $state | Out-Null
    $sentinels = @{}
    foreach ($name in @('corpus.txt','config.json','onboarding.json','noncredential-auth-sentinel.txt','unknown.bin')) {
        $path=Join-Path $state $name; [IO.File]::WriteAllText($path,'synthetic-only-'+$name)
        $sentinels[$path]=(Get-FileHash -LiteralPath $path).Hash
    }
    $installArgs=@('/VERYSILENT','/SUPPRESSMSGBOXES','/NORESTART','/SP-',('/DIR="'+$app+'"'))
    Run-Setup $installerPath $installArgs $true 'install'
    if (-not (Test-Path $registration) -or -not (Test-Path -LiteralPath $shortcut)) { throw 'Per-user registration/shortcut missing' }
    if ((Get-ItemProperty $registration).InstallLocation.TrimEnd('\') -ne $app.TrimEnd('\')) { throw 'Unexpected registered installation root' }
    Check-Inventory
    if (Get-CimInstance Win32_Process | Where-Object { $_.ExecutablePath -and $_.ExecutablePath.StartsWith($app+'\',[StringComparison]::OrdinalIgnoreCase) }) { throw 'Installer auto-started application/auth' }
    $unknown=Join-Path $app 'user-added.txt'; [IO.File]::WriteAllText($unknown,'unknown installed file must survive uninstall')
    $uninstaller=Join-Path $app 'unins000.exe'
    if ($env:SAPIENS_WINDOWS_CERT_THUMBPRINT) {
        foreach ($path in @($installerPath,$uninstaller)) {
            $signature=Get-AuthenticodeSignature -LiteralPath $path
            if ($signature.Status -ne 'Valid' -or $signature.SignerCertificate.Thumbprint -ne $env:SAPIENS_WINDOWS_CERT_THUMBPRINT) { throw 'Installer/uninstaller publisher mismatch' }
        }
    }
    $info=[Diagnostics.ProcessStartInfo]::new();$info.FileName=Join-Path $app 'Sapiens4.exe';$info.UseShellExecute=$false
    foreach ($key in @($info.Environment.Keys)) { if ($key -match '^(SAPIENS_|CODEX_|OPENAI_)') { $info.Environment.Remove($key) | Out-Null } }
    $info.Environment['SAPIENS_CONFIG_DIR']=Join-Path $state 'fresh-config'
    $info.Environment['SAPIENS_DATA_DIR']=Join-Path $state 'fresh-data'
    $info.Environment['SAPIENS_CODEX_BINARY']=Join-Path $state 'provider-disabled.exe'
    $native=[Diagnostics.Process]::Start($info)
    $deadline=[DateTime]::UtcNow.AddSeconds(60);$healthy=$false
    while ([DateTime]::UtcNow -lt $deadline) {
        if ($native.HasExited) { throw "Installed native app exited early: $($native.ExitCode)" }
        $backends=@(Get-CimInstance Win32_Process | Where-Object { $_.ParentProcessId -eq $native.Id -and $_.ExecutablePath -eq (Join-Path $app 'runtime/.build/bin/sapiens4.exe') })
        foreach ($backend in $backends) {
            $backendIds=@($backends.ProcessId)
            foreach ($port in @(Get-NetTCPConnection -OwningProcess $backend.ProcessId -State Listen -ErrorAction SilentlyContinue)) {
                try { $health=Invoke-RestMethod -Uri ("http://127.0.0.1:"+$port.LocalPort+'/api/health') -TimeoutSec 2; if ($health.status -eq 'ok') { $healthy=$true } } catch {}
            }
        }
        if ($healthy) { break }; Start-Sleep -Milliseconds 250
    }
    if (-not $healthy) { throw 'Native application did not reach its owned backend; WebView2/native startup requires investigation' }
    $result.checks += @{name='native-launch-owned-backend-health';status='PASS'}
    $before=(Get-FileHash -LiteralPath (Join-Path $app 'Sapiens4.exe')).Hash
    Run-Setup $installerPath $installArgs $false 'active-reinstall-refused'
    Run-Setup $uninstaller @('/VERYSILENT','/SUPPRESSMSGBOXES','/NORESTART') $false 'active-uninstall-refused'
    if ($native.HasExited -or (Get-FileHash -LiteralPath (Join-Path $app 'Sapiens4.exe')).Hash -ne $before) { throw 'Active installation modified or app stopped' }
    if (-not $native.CloseMainWindow() -or -not $native.WaitForExit(20000)) { throw 'Installed native window did not close gracefully' }
    foreach ($id in $backendIds) { if (Get-Process -Id $id -ErrorAction SilentlyContinue) { throw 'Owned backend survived native window close' } }
    $native=$null
    Run-Setup $installerPath $installArgs $true 'stopped-reinstall'
    Check-Inventory
    $prior=$env:SAPIENS_TEST_PACKAGE_RUNTIME
    try {
        $env:SAPIENS_TEST_PACKAGE_RUNTIME=Join-Path $app 'runtime'
        & ruby submodules/libs4/ruby/bundle.rb exec rspec spec/native_package_spec.rb
        if ($LASTEXITCODE -ne 0) { throw 'Installed-runtime RSpec scenario failed' }
    } finally { $env:SAPIENS_TEST_PACKAGE_RUNTIME=$prior }
    Run-Setup $uninstaller @('/VERYSILENT','/SUPPRESSMSGBOXES','/NORESTART') $true 'uninstall'
    if (Test-Path $registration) { throw 'Uninstall registration survived' }
    if (Test-Path -LiteralPath $shortcut) { throw 'Uninstall shortcut survived' }
    if (Test-Path -LiteralPath (Join-Path $app 'Sapiens4.exe')) { throw 'Installed launcher survived uninstall' }
    if (-not (Test-Path -LiteralPath $unknown)) { throw 'Uninstall deleted an unknown user file' }
    foreach ($path in $sentinels.Keys) { if ((Get-FileHash -LiteralPath $path).Hash -ne $sentinels[$path]) { throw 'External state changed' } }
    $result.statePreserved=$true; $result.status='PASS'
} catch { $result.error=$_.Exception.Message; throw }
finally {
    if ($native -and -not $native.HasExited) { [void]$native.CloseMainWindow(); if (-not $native.WaitForExit(10000)) { $native.Kill($true) } }
    $result.ownedWorkRoot=$work
    $result | ConvertTo-Json -Depth 6 | Set-Content -LiteralPath (Join-Path $evidence 'result.json')
}
