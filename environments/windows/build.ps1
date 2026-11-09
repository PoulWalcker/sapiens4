param(
    [ValidateSet('win-x64')][string]$Runtime = 'win-x64',
    [string]$OutputDirectory,
    [string]$Version = '0.1.0-dev',
    [string]$ProviderDirectory,
    [string]$BlindlySource = (Join-Path $PSScriptRoot '../../submodules/blindly4'),
    [ValidateSet('msvc','gnullvm')][string]$BlindlyToolchain = 'msvc',
    [switch]$BuildOnly,
    [switch]$SkipBuild,
    [switch]$AllowMissingProvider,
    [string]$CertificateThumbprint = $env:SAPIENS_WINDOWS_CERT_THUMBPRINT,
    [string]$TimestampUrl = 'http://timestamp.digicert.com'
)
$ErrorActionPreference = 'Stop'
if ($BuildOnly -and $SkipBuild) { throw 'Choose BuildOnly or SkipBuild' }
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../..'))
if ($Version -notmatch '^\d+\.\d+\.\d+(-[0-9A-Za-z.-]+)?(\+[0-9A-Za-z.-]+)?$') { throw 'Version must be SemVer' }
if (-not $OutputDirectory) { $OutputDirectory = Join-Path $repo ".build/windows" }
$output = [IO.Path]::GetFullPath($OutputDirectory)
$destination = Join-Path $output "Sapiens4-Development-$Runtime"
if (-not $BuildOnly -and (Test-Path -LiteralPath $destination)) { throw 'Output must be new; refusing to mix old and new payloads' }
if ($destination -eq $repo -or $repo.StartsWith($destination + [IO.Path]::DirectorySeparatorChar)) { throw 'Output must not replace the source checkout' }
if ([IO.Path]::GetFullPath($BlindlySource) -ne (Join-Path $repo 'submodules/blindly4')) { throw 'Use the pinned Blindly4 submodule for distribution' }
Push-Location $repo
try {
    if (-not $SkipBuild) {
        $target = & ghc --print-target-platform
        if ($LASTEXITCODE -ne 0 -or $target -notmatch '^x86_64-.*mingw') { throw "Windows x64 GHC required; got $target" }
        if (-not (Test-Path (Join-Path $repo 'submodules/libs4/node_modules/sharp/package.json'))) {
            & npm ci --prefix (Join-Path $repo 'submodules/libs4')
            if ($LASTEXITCODE -ne 0) { throw 'Icon/UI dependency installation failed' }
        }
        & node (Join-Path $repo 'submodules/libs4/tools/svg-to-ico.cjs') (Join-Path $repo 'app/shell/favicon.svg') (Join-Path $PSScriptRoot 'AppIcon.ico')
        if ($LASTEXITCODE -ne 0) { throw 'Application icon preparation failed' }
        & node (Join-Path $repo 'submodules/libs4/tools/svg-to-ico.cjs') (Join-Path $repo 'app/shell/favicon-dark.svg') (Join-Path $PSScriptRoot 'AppIconDark.ico')
        if ($LASTEXITCODE -ne 0) { throw 'Dark icon preparation failed' }
        & runghc build/Build.hs
        if ($LASTEXITCODE -ne 0) { throw 'Haskell application build failed' }
        & (Join-Path $BlindlySource 'windows/build.ps1') -Runtime $Runtime -Toolchain $BlindlyToolchain
        if ($LASTEXITCODE -ne 0) { throw 'Blindly4 build failed' }
    }
    & (Join-Path $repo 'submodules/libs4/windows/webview2.ps1') -Runtime $Runtime -Destination (Join-Path $repo '.build/bin')
    if ($BuildOnly) { return }
    $payload = Join-Path $destination 'runtime'
    $manifest = Join-Path $payload 'package-manifest.json'
    $stage = @('build/package/package.py','stage','--root',$repo,'--destination',$payload,
        '--version',$Version,'--platform','windows')
    if ($ProviderDirectory) { $stage += @('--provider',$ProviderDirectory) }
    if ($AllowMissingProvider) { $stage += '--allow-missing-provider' }
    & python @stage
    if ($LASTEXITCODE -ne 0) { throw 'Runtime assembly failed' }
    Copy-Item -LiteralPath (Join-Path $repo '.build/bin/sapiens-windows.exe') -Destination (Join-Path $destination 'Sapiens4.exe')
    Copy-Item -LiteralPath (Join-Path $repo '.build/bin/WebView2Loader.dll'),(Join-Path $repo '.build/bin/WebView2-LICENSE.txt') -Destination $destination
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
    [IO.File]::WriteAllText((Join-Path $destination 'sapiens-installation.id'), 'com.poulwalcker.sapiens4.development')
    $artifact = Join-Path $output "sapiens4-development-$Version-windows-x64.zip"
    if (Test-Path -LiteralPath $artifact) { throw 'Versioned artifact already exists' }
    Compress-Archive -LiteralPath $destination -DestinationPath $artifact
    & (Join-Path $repo 'build/package/windows-installer.ps1') -PackageDirectory $destination -OutputDirectory $output -Version $Version -CertificateThumbprint $CertificateThumbprint -TimestampUrl $TimestampUrl
    if ($LASTEXITCODE -ne 0) { throw 'Installer assembly failed' }
    & python build/package/package.py receipt --artifact $artifact --installer (Join-Path $output 'Sapiens-Setup.exe') --manifest $manifest --output ($artifact -replace '\.zip$','.release.json')
    if ($LASTEXITCODE -ne 0) { throw 'Release receipt failed' }
    Write-Host "Built $destination/Sapiens4.exe`nArtifact $artifact"
} finally { Pop-Location }
