param(
    [Parameter(Mandatory)][string]$PackageDirectory,
    [Parameter(Mandatory)][string]$OutputDirectory,
    [Parameter(Mandatory)][string]$Version,
    [ValidateSet('development','stable')][string]$Channel = 'development',
    [string]$CertificateThumbprint,
    [string]$TimestampUrl = 'http://timestamp.digicert.com'
)
$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../..'))
$package = (Resolve-Path -LiteralPath $PackageDirectory).Path
$output = [IO.Path]::GetFullPath($OutputDirectory)
if (Test-Path -LiteralPath (Join-Path $output 'Sapiens-Setup.exe')) { throw 'Installer output must be new' }
$pin = Get-Content -Raw -LiteralPath (Join-Path $PSScriptRoot 'windows-installer-tool.json') | ConvertFrom-Json
$cache = Join-Path $repo ".build/tools/innosetup-$($pin.version)"
New-Item -ItemType Directory -Path $cache -Force | Out-Null
$download = Join-Path $cache 'compiler-setup.exe'
if (-not (Test-Path -LiteralPath $download) -or (Get-FileHash -LiteralPath $download -Algorithm SHA256).Hash -ne $pin.sha256) {
    Invoke-WebRequest -Uri $pin.url -OutFile $download
}
if ((Get-Item -LiteralPath $download).Length -ne $pin.size -or (Get-FileHash -LiteralPath $download -Algorithm SHA256).Hash -ne $pin.sha256) { throw 'Pinned Inno Setup download failed integrity verification' }
$signature = Get-AuthenticodeSignature -LiteralPath $download
if ($signature.Status -ne 'Valid' -or $signature.SignerCertificate.Subject -notmatch 'Pyrsys B\.V\.') { throw 'Unexpected Inno Setup publisher signature' }
$compilerDirectory = Join-Path $cache 'compiler'
$compiler = Join-Path $compilerDirectory 'ISCC.exe'
$compilerHash = Join-Path $cache 'compiler-sha256.txt'
if (-not (Test-Path -LiteralPath $compiler)) {
    $process = Start-Process -FilePath $download -ArgumentList @('/VERYSILENT','/SUPPRESSMSGBOXES','/NORESTART','/SP-','/CURRENTUSER','/NOICONS','/TASKS=""',('/DIR="'+$compilerDirectory+'"')) -Wait -PassThru
    if ($process.ExitCode -ne 0 -or -not (Test-Path -LiteralPath $compiler)) { throw "Inno Setup compiler installation failed: $($process.ExitCode)" }
    (Get-FileHash -LiteralPath $compiler -Algorithm SHA256).Hash | Set-Content -LiteralPath $compilerHash
}
if (-not (Test-Path -LiteralPath $compilerHash) -or (Get-FileHash -LiteralPath $compiler -Algorithm SHA256).Hash -ne (Get-Content -Raw -LiteralPath $compilerHash).Trim()) { throw 'Cached compiler changed; use a fresh tool cache' }
$appName = if ($Channel -eq 'stable') { 'Sapiens4' } else { 'Sapiens4 Development' }
$appId = if ($Channel -eq 'stable') { 'com.poulwalcker.sapiens4' } else { 'com.poulwalcker.sapiens4.development' }
$marker = (Get-Content -Raw -LiteralPath (Join-Path $package 'sapiens-installation.id')).Trim()
if ($marker -ne $appId) { throw 'Installer channel and payload identity differ' }
$arguments = @(("/DPackageName=$appName"),("/DPackageId=$appId"),'/Qp',("/DSourceDir=$package"),("/DOutputDir=$output"),("/DAppVersion=$Version"),("/DFileVersion="+($Version -split '[-+]')[0]))
if ($CertificateThumbprint) {
    if ($CertificateThumbprint -notmatch '^[a-fA-F0-9]{40}$' -or $TimestampUrl -notmatch '^https?://[A-Za-z0-9./:_-]+$') { throw 'Invalid signing configuration' }
    $arguments += '/DPublisherSign'
    $arguments += ('/Ssapiens=signtool.exe sign /sha1 '+$CertificateThumbprint+' /fd SHA256 /tr '+$TimestampUrl+' /td SHA256 $f')
}
& $compiler @arguments (Join-Path $PSScriptRoot 'windows-installer.iss')
if ($LASTEXITCODE -ne 0) { throw 'Inno Setup compilation failed' }
$installer = Join-Path $output 'Sapiens-Setup.exe'
if ($CertificateThumbprint) {
    & signtool verify /pa /all /v $installer
    if ($LASTEXITCODE -ne 0) { throw 'Final installer signature verification failed' }
}
Write-Host "Installer $installer (Inno Setup $($pin.version))"
