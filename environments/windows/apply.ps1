param(
    [string]$App, [string]$State, [string]$Version, [uri]$Asset,
    [AllowEmptyString()][string]$Root, [string]$Config, [int]$ShellPid, [string]$Digest
)
$ErrorActionPreference = 'Stop'
$pending = Join-Path $State 'pending'
$reservation = Join-Path $pending (Split-Path $PSScriptRoot -Leaf)
$outcome = Join-Path $State 'outcome.txt'
$exe = Join-Path $App 'Sapiens4.exe'
$manifest = Join-Path $App 'runtime/desktop-update.json'
$package = Join-Path $PSScriptRoot 'update.exe'
$identity = 'com.sapiens4.desktop'
$armed = $false
$old = $null
$originalShell = $null

function Release-Reservation {
    if ([IO.File]::Exists($reservation)) {
        [IO.File]::Delete($reservation)
        [IO.Directory]::Delete($pending)
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
    $originalShell = Get-Process -Id $ShellPid -ErrorAction SilentlyContinue
    if ($originalShell -and $originalShell.Path -ne $exe) { $originalShell = $null }
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
    Verify-Publisher $package $old.publisher
    $info = [Diagnostics.FileVersionInfo]::GetVersionInfo($package)
    if ($info.ProductName -cne $identity -or $info.FileDescription -cne "Sapiens4 Setup $($old.platform)" -or $info.ProductVersion -cne $Version) { throw 'Wrong signed installer product, version or platform.' }
    [Console]::Out.WriteLine('downloaded')
    [Console]::Out.Flush()
    if ([Console]::ReadLine() -cne 'install') { throw 'The update was cancelled before installation. The previous version is available.' }
    if (-not [IO.File]::Exists($reservation)) { throw 'Installation ownership was not granted.' }
    Add-Type -AssemblyName System.IO.Compression.FileSystem
    $archive = Join-Path $PSScriptRoot 'previous.zip'
    [IO.Compression.ZipFile]::CreateFromDirectory($App,$archive)
    $saved = [IO.Compression.ZipFile]::OpenRead($archive)
    try {
        foreach ($entry in $saved.Entries) {
            $content = $entry.Open()
            try { $content.CopyTo([IO.Stream]::Null) } finally { $content.Dispose() }
        }
    } finally { $saved.Dispose() }
    $saved = [IO.File]::Open($archive,'Open','ReadWrite','None')
    try { $saved.Flush($true) } finally { $saved.Dispose() }
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
    $next = Join-Path $PSScriptRoot 'update.json'
    [IO.File]::WriteAllText($next,(@{attempt=(Split-Path $PSScriptRoot -Leaf);version=$Version;root=$Root;status='installed';recovery=$archive} | ConvertTo-Json -Compress))
    if ([IO.File]::Exists($marker)) { [IO.File]::Replace($next,$marker,$null) } else { [IO.File]::Move($next,$marker) }
    Release-Reservation
    Open-Shell
} catch {
    [IO.File]::WriteAllText($outcome,$_.Exception.Message)
    if (-not $armed) { Release-Reservation }
    exit 1
}
