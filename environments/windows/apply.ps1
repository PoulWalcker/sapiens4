param([switch]$Repair, [switch]$Previous)
$ErrorActionPreference = 'Stop'
if ($Repair -and $Previous) { throw 'Choose either Repair or Previous.' }
$savedAttempt = Get-Content -LiteralPath (Join-Path $PSScriptRoot 'request.json') -Raw | ConvertFrom-Json
$App = $savedAttempt.app; $State = $savedAttempt.state; $Version = $savedAttempt.version
[uri]$Asset = $savedAttempt.asset; $Root = $savedAttempt.root; $Config = $savedAttempt.config; $Digest = $savedAttempt.digest
[int]$ShellPid = if ($Repair -or $Previous) { 0 } else { $savedAttempt.shellPid }
$pending = Join-Path $State 'pending'
$attempt = Split-Path $PSScriptRoot -Leaf
$outcome = Join-Path $State 'outcome.txt'
$exe = Join-Path $App 'Sapiens4.exe'
$manifest = Join-Path $App 'runtime/desktop-update.json'
$package = Join-Path $PSScriptRoot 'update.exe'
$previousInstaller = Join-Path $PSScriptRoot 'previous.exe'
$identity = 'com.sapiens4.desktop'
$armed = $false
$old = $null
$originalShell = $null
$leases = @()
$ownsGate = $false

Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
public static class UpdateFiles {
    [DllImport("kernel32.dll", CharSet=CharSet.Unicode, SetLastError=true)]
    public static extern bool MoveFileEx(string source, string target, uint flags);
}
'@
function Write-Record([string]$Path, $Value, [switch]$Plain) {
    $next = "$Path.new"
    $text = if ($Plain) { [string]$Value } else { $Value | ConvertTo-Json -Depth 5 -Compress }
    [IO.File]::WriteAllText($next,$text)
    $file = [IO.File]::Open($next,'Open','ReadWrite','None')
    try { $file.Flush($true) } finally { $file.Dispose() }
    if (-not [UpdateFiles]::MoveFileEx($next,$Path,9)) { throw 'Cannot publish the update recovery record.' }
}
function Lock-Installation([string]$Name) {
    $file = [IO.File]::Open((Join-Path $State $Name),'OpenOrCreate','ReadWrite','ReadWrite')
    try {
        for ($n=0; $n -lt 50; $n++) {
            try { $file.Lock(0,1); return $file }
            catch [IO.IOException] { if ($n -eq 49) { throw }; Start-Sleep -Milliseconds 100 }
        }
    } catch { $file.Dispose(); throw 'Another process is using this installation. Close it before recovery.' }
}
function Acquire-Maintenance {
    $script:leases += Lock-Installation 'admission.lock'
    $script:leases += Lock-Installation 'use.lock'
    if (Test-Path -LiteralPath $pending) {
        if ([IO.File]::ReadAllText($pending) -cne $attempt) { throw 'A different update owns this installation.' }
    } else {
        $marker = Join-Path $State 'update.json'
        if (($Repair -or $Previous) -and (Test-Path -LiteralPath $marker)) {
            $current = Get-Content -LiteralPath $marker -Raw | ConvertFrom-Json
            if ($current.attempt -cne $attempt) { throw 'A newer update owns this installation. Use its recovery command.' }
        }
        Write-Record $pending $attempt -Plain
    }
    $script:ownsGate = $true
}

function Release-Reservation {
    if ([IO.File]::Exists($pending) -and [IO.File]::ReadAllText($pending) -ceq $attempt) {
        [IO.File]::Delete($pending)
    }
}
function Installed-Release {
    $local = Get-Content -LiteralPath $manifest -Raw | ConvertFrom-Json
    if ($local.platform -notin @('win-x64','win-arm64') -or $local.version -notmatch '^\d+\.\d+\.\d+$') {
        throw 'The installed release could not be verified.'
    }
    if ([IO.File]::ReadAllText((Join-Path $App 'sapiens-installation.id')) -cne $identity) { throw 'Wrong installed application.' }
    $entry = Get-ItemProperty -LiteralPath "HKCU:\Software\Microsoft\Windows\CurrentVersion\Uninstall\${identity}_is1"
    if ([IO.Path]::GetFullPath($entry.InstallLocation).TrimEnd('\') -ne $App.TrimEnd('\')) { throw 'The registered installation location changed.' }
    return $local
}
function Verify-Publisher([string]$Path, [string]$Thumbprint) {
    $signature = Get-AuthenticodeSignature -LiteralPath $Path
    if ($signature.Status -ne 'Valid' -or $signature.SignerCertificate.Thumbprint -ne $Thumbprint) {
        throw "The publisher signature could not be verified on $([IO.Path]::GetFileName($Path))."
    }
}
function Verify-Installer([string]$Path, [string]$Expected) {
    Verify-Publisher $Path $old.publisher
    $info = [Diagnostics.FileVersionInfo]::GetVersionInfo($Path)
    if ($info.ProductName -cne $identity -or $info.FileDescription -cne "Sapiens4 Setup $($old.platform)" -or $info.ProductVersion -cne $Expected) {
        throw 'Wrong signed installer product, version or platform.'
    }
}
function Close-Shell {
    if ($originalShell -and -not $originalShell.HasExited) {
        if (-not $originalShell.CloseMainWindow() -and -not $originalShell.HasExited) { throw 'Close Sapiens4 to finish the update.' }
        if (-not $originalShell.WaitForExit(30000)) { throw 'The application window is still running.' }
    }
}
function Open-Shell {
    $start = [Diagnostics.ProcessStartInfo]::new($exe)
    $start.UseShellExecute = $false
    if ($Root) { $start.EnvironmentVariables['SAPIENS_DATA_DIR'] = $Root }
    else { $start.EnvironmentVariables.Remove('SAPIENS_DATA_DIR') }
    $start.EnvironmentVariables['SAPIENS_CONFIG_DIR'] = $Config
    [Diagnostics.Process]::Start($start).Dispose()
}

try {
    $App = [IO.Path]::GetFullPath($App)
    $State = [IO.Path]::GetFullPath($State)
    $expected = Join-Path (Split-Path $App -Parent) ('.' + (Split-Path $App -Leaf) + '.update')
    if ($State -ne $expected -or (Split-Path $PSScriptRoot -Parent) -ne $State) { exit 1 }
    if (-not $Asset.IsAbsoluteUri -or $Asset.Scheme -ne 'https' -or $Asset.UserInfo -or $Asset.Fragment -or
        $Version -notmatch '^\d+\.\d+\.\d+$' -or $Digest -notmatch '^[a-fA-F0-9]{64}$') { throw 'Invalid selected release.' }
    if ($ShellPid -gt 0) { $originalShell = Get-Process -Id $ShellPid -ErrorAction SilentlyContinue }
    if ($originalShell -and $originalShell.Path -ne $exe) { $originalShell = $null }
    if ($Repair -or $Previous) {
        $old = $savedAttempt.old
    } else {
    $installed = Installed-Release
    if ([version]$installed.version -ge [version]$Version -or $installed.publisher -notmatch '^[a-fA-F0-9]{40}$') { throw 'Invalid installed publisher or version.' }
    Verify-Publisher $exe $installed.publisher
    $old = $installed
    Add-Type -AssemblyName System.Net.Http
    $handler = [Net.Http.HttpClientHandler]::new()
    $handler.AllowAutoRedirect = $false
    $client = [Net.Http.HttpClient]::new($handler)
    $client.Timeout = [TimeSpan]::FromMinutes(60)
    try {
        $response = $client.GetAsync($Asset, [Net.Http.HttpCompletionOption]::ResponseHeadersRead).GetAwaiter().GetResult()
        try {
            if ([int]$response.StatusCode -ne 200) { throw "Update download failed (HTTP $([int]$response.StatusCode))." }
            $stream = [IO.File]::Open($package,'CreateNew','Write','None')
            $timeout = [Threading.CancellationTokenSource]::new([TimeSpan]::FromMinutes(60))
            try { $response.Content.CopyToAsync($stream).Wait($timeout.Token) }
            finally { $timeout.Dispose(); $stream.Dispose() }
        } finally { $response.Dispose() }
    } finally { $client.Dispose() }
    if ((Get-FileHash -LiteralPath $package -Algorithm SHA256).Hash -ne $Digest) { throw 'The update checksum did not match the selected release.' }
    Verify-Installer $package $Version
    $cacheRecord = Join-Path $State 'current-installer'
    if (-not [IO.File]::Exists($cacheRecord)) { throw 'Reinstall the current version once to preserve its signed recovery installer.' }
    $cached = [IO.File]::ReadAllText($cacheRecord)
    if ($cached -notmatch '^[a-fA-F0-9]{64}$') { throw 'Reinstall the current version once to preserve its signed recovery installer.' }
    $original = Join-Path $State "installers\$cached.exe"
    if ((Get-FileHash -LiteralPath $original -Algorithm SHA256).Hash -ne $cached) { throw 'The previous installer cache is incomplete.' }
    Verify-Installer $original $old.version
    [IO.File]::Copy($original,$previousInstaller,$false)
    Verify-Installer $previousInstaller $old.version
    foreach ($path in @($package,$previousInstaller)) {
        $file = [IO.File]::Open($path,'Open','ReadWrite','None')
        try { $file.Flush($true) } finally { $file.Dispose() }
    }
    $savedAttempt | Add-Member -NotePropertyName old -NotePropertyValue $old
    Write-Record (Join-Path $PSScriptRoot 'request.json') $savedAttempt
    [IO.File]::WriteAllText((Join-Path $PSScriptRoot 'Repair.cmd'), "@echo off`r`npowershell.exe -NoProfile -File `"%~dp0apply.ps1`" -Repair`r`npause`r`n")
    [Console]::Out.WriteLine('downloaded')
    [Console]::Out.Flush()
    if ([Console]::ReadLine() -cne 'install') { throw 'The update was cancelled before installation. The previous version is available.' }
    if (-not [IO.File]::Exists($pending) -or [IO.File]::ReadAllText($pending) -cne $attempt) { throw 'Installation ownership was not granted.' }
    }
    if ($Previous) { $package = $previousInstaller; $Version = $old.version }
    elseif ((Get-FileHash -LiteralPath $package -Algorithm SHA256).Hash -ne $Digest) { throw 'The retained update checksum does not match.' }
    Verify-Installer $package $Version
    Acquire-Maintenance
    Close-Shell
    $armed = $true
    $setup = Start-Process -FilePath $package -ArgumentList @('/CURRENTUSER','/VERYSILENT','/SUPPRESSMSGBOXES','/NORESTART','/RESTARTEXITCODE=3010',("/DIR=`"$App`"")) -PassThru -Wait
    if ($setup.ExitCode -eq 3010) { throw 'Restart Windows and repair this installation before reopening.' }
    if ($setup.ExitCode -ne 0) { throw "Setup exited with code $($setup.ExitCode). Repair this installation before reopening." }
    $new = Installed-Release
    if ($new.version -cne $Version -or $new.platform -cne $old.platform -or $new.publisher -ne $old.publisher) { throw 'Could not confirm the installed release.' }
    Verify-Publisher $exe $old.publisher
    Verify-Publisher (Join-Path $App 'runtime/.build/bin/sapiens4.exe') $old.publisher
    $marker = Join-Path $State 'update.json'
    if ($Previous) {
        if ([IO.File]::Exists($marker) -and -not [UpdateFiles]::MoveFileEx($marker,(Join-Path $PSScriptRoot 'restored-update.json'),9)) { throw 'Cannot finish application recovery.' }
    } else {
        Write-Record $marker @{attempt=(Split-Path $PSScriptRoot -Leaf);version=$Version;status='installed'}
    }
    Release-Reservation
    if ($Previous) { Write-Output 'Previous application restored. Open it only after explicitly restoring the corresponding data; newer data must be preserved first.' }
    else { Open-Shell }
} catch {
    [IO.File]::WriteAllText($outcome,($_.Exception.Message + " Recovery: powershell.exe -NoProfile -File `"$PSCommandPath`" -Repair. The retained installers have not been deleted."))
    if ($ownsGate -and -not $armed -and -not $Repair -and -not $Previous) { Release-Reservation }
    exit 1
} finally {
    foreach ($file in $leases) { $file.Dispose() }
}
