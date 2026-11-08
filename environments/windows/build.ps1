param(
    [ValidateSet('win-x64')][string]$Runtime = 'win-x64',
    [string]$OutputDirectory,
    [string]$Version = '0.1.0-dev',
    [string]$ProviderDirectory,
    [string]$BlindlySource = (Join-Path $PSScriptRoot '../../submodules/blindly4'),
    [ValidateSet('msvc','gnullvm')][string]$BlindlyToolchain = 'msvc',
    [switch]$SkipBuild,
    [switch]$AllowMissingProvider,
    [string]$CertificateThumbprint = $env:SAPIENS_WINDOWS_CERT_THUMBPRINT,
    [string]$TimestampUrl = 'http://timestamp.digicert.com'
)
$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../..'))
if ($Version -notmatch '^\d+\.\d+\.\d+(-[0-9A-Za-z.-]+)?(\+[0-9A-Za-z.-]+)?$') { throw 'Version must be SemVer' }
if (-not $OutputDirectory) { $OutputDirectory = Join-Path $repo ".build/Sapiens4-Development-$Runtime" }
$destination = [IO.Path]::GetFullPath($OutputDirectory)
if (Test-Path -LiteralPath $destination) { throw 'Output must be new; refusing to mix old and new payloads' }
if ($destination -eq $repo -or $repo.StartsWith($destination + [IO.Path]::DirectorySeparatorChar)) { throw 'Output must not replace the source checkout' }
if ([IO.Path]::GetFullPath($BlindlySource) -ne (Join-Path $repo 'submodules/blindly4')) { throw 'Use the pinned Blindly4 submodule for distribution' }
Push-Location $repo
try {
    if (-not $SkipBuild) {
        $target = & ghc --print-target-platform
        if ($LASTEXITCODE -ne 0 -or $target -notmatch '^x86_64-.*mingw') { throw "Windows x64 GHC required; got $target" }
        & runghc build/Build.hs --no-provider
        if ($LASTEXITCODE -ne 0) { throw 'Haskell application build failed' }
        & (Join-Path $BlindlySource 'windows/build.ps1') -Runtime $Runtime -Toolchain $BlindlyToolchain
        if ($LASTEXITCODE -ne 0) { throw 'Blindly4 build failed' }
    }
    if (-not $ProviderDirectory -and -not $AllowMissingProvider) {
        $ProviderDirectory = Join-Path $repo '.build/codex'
        & python build/package/acquire-provider.py --platform windows-x64 --destination $ProviderDirectory --ensure
        if ($LASTEXITCODE -ne 0) { throw 'Codex provider acquisition failed' }
    }
    $payload = Join-Path $destination 'runtime'
    $manifest = Join-Path $payload 'package-manifest.json'
    $stage = @('build/package/package.py','stage','--root',$repo,'--destination',$payload,'--manifest',$manifest,
        '--version',$Version,'--platform','windows','--architecture','x64')
    if ($ProviderDirectory) { $stage += @('--provider',$ProviderDirectory) }
    if ($AllowMissingProvider) { $stage += '--allow-missing-provider' }
    & python @stage
    if ($LASTEXITCODE -ne 0) { throw 'Runtime assembly failed' }
    Copy-Item -LiteralPath (Join-Path $repo '.build/bin/sapiens-windows.exe') -Destination (Join-Path $destination 'Sapiens4.exe')
    & (Join-Path $repo 'submodules/libs4/windows/webview2.ps1') -Runtime $Runtime -Destination $destination
    if ($LASTEXITCODE -ne 0) { throw 'WebView2 loader assembly failed' }
    Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'README.md') -Destination (Join-Path $destination 'README.md')
    if ($CertificateThumbprint) {
        Get-ChildItem -LiteralPath $destination -Recurse -File | Where-Object { $_.Extension -in @('.exe','.dll') } | ForEach-Object {
            & signtool sign /sha1 $CertificateThumbprint /fd SHA256 /tr $TimestampUrl /td SHA256 $_.FullName
            if ($LASTEXITCODE -ne 0) { throw "Authenticode signing failed: $($_.FullName)" }
            & signtool verify /pa /all /v $_.FullName
            if ($LASTEXITCODE -ne 0) { throw "Authenticode verification failed: $($_.FullName)" }
        }
    }
    $signing = if ($CertificateThumbprint) { 'authenticode' } else { 'none' }
    & python build/package/package.py finalize --runtime $payload --signing $signing
    if ($LASTEXITCODE -ne 0) { throw 'Final signed runtime inventory failed' }
    $artifact = Join-Path (Split-Path $destination -Parent) "sapiens4-development-$Version-windows-x64.zip"
    if (Test-Path -LiteralPath $artifact) { throw 'Versioned artifact already exists' }
    Compress-Archive -LiteralPath $destination -DestinationPath $artifact
    & python build/package/package.py receipt --artifact $artifact --manifest $manifest --output ($artifact -replace '\.zip$','.release.json')
    if ($LASTEXITCODE -ne 0) { throw 'Release receipt failed' }
    Write-Host "Built $destination/Sapiens4.exe`nArtifact $artifact"
} finally { Pop-Location }
