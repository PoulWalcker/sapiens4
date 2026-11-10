param(
    [string]$OutputDirectory,
    [string]$Version = '0.1.0-dev',
    [ValidateSet('development','stable')][string]$Channel = 'development',
    [string]$ProviderDirectory,
    [ValidateSet('msvc','gnullvm')][string]$BlindlyToolchain = 'msvc',
    [switch]$BuildOnly,
    [switch]$SkipBuild,
    [switch]$AllowMissingProvider,
    [string]$CertificateThumbprint = $env:SAPIENS_WINDOWS_CERT_THUMBPRINT,
    [string]$TimestampUrl = 'http://timestamp.digicert.com'
)
$ErrorActionPreference = 'Stop'
$Runtime = 'win-x64'
if ($BuildOnly -and $SkipBuild) { throw 'Choose BuildOnly or SkipBuild' }
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../..'))
& runghc ("-i"+(Join-Path $repo 'build/package')) (Join-Path $repo 'build/Package.hs') version $Version
if ($LASTEXITCODE -ne 0) { throw 'Version must be SemVer' }
if (-not $OutputDirectory) { $OutputDirectory = Join-Path $repo ".build/windows" }
$output = [IO.Path]::GetFullPath($OutputDirectory)
$appName = if ($Channel -eq 'stable') { 'Sapiens4' } else { 'Sapiens4 Development' }
$appId = if ($Channel -eq 'stable') { 'com.poulwalcker.sapiens4' } else { 'com.poulwalcker.sapiens4.development' }
$prefix = if ($Channel -eq 'stable') { 'sapiens4-' } else { 'sapiens4-development-' }
$destination = Join-Path $output (($appName -replace ' ', '-') + "-$Runtime")
if (-not $BuildOnly -and (Test-Path -LiteralPath $destination)) { throw 'Output must be new; refusing to mix old and new payloads' }
if ($destination -eq $repo -or $repo.StartsWith($destination + [IO.Path]::DirectorySeparatorChar)) { throw 'Output must not replace the source checkout' }
$staging = $null
$published = @()
$complete = $false
Push-Location $repo
try {
    if ($Channel -eq 'stable' -and $CertificateThumbprint) {
        $commit = & git rev-parse HEAD
        if ($LASTEXITCODE) { throw 'Production build requires a Git checkout' }
        & runghc ("-i"+(Join-Path $repo 'build/package')) (Join-Path $repo 'build/Package.hs') production-source $commit
        if ($LASTEXITCODE) { throw 'Production source must belong to current release main history' }
    }
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
        & (Join-Path $repo 'submodules/blindly4/windows/build.ps1') -Runtime $Runtime -Toolchain $BlindlyToolchain
        if ($LASTEXITCODE -ne 0) { throw 'Blindly4 build failed' }
    }
    & (Join-Path $repo 'submodules/libs4/windows/webview2.ps1') -Runtime $Runtime -Destination (Join-Path $repo '.build/bin')
    if ($BuildOnly) { return }
    $artifactName = "$prefix$Version-windows-x64.zip"
    $receiptName = $artifactName -replace '\.zip$','.release.json'
    foreach ($name in @($artifactName, $receiptName, 'Sapiens-Setup.exe')) {
        if (Test-Path -LiteralPath (Join-Path $output $name)) { throw "Output already exists: $name" }
    }
    New-Item -ItemType Directory -Path $output -Force | Out-Null
    $candidate = Join-Path $output ('.sapiens-package.' + [Guid]::NewGuid().ToString('N'))
    New-Item -ItemType Directory -Path $candidate | Out-Null
    $staging = $candidate
    $package = Join-Path $staging (Split-Path -Leaf $destination)
    $payload = Join-Path $package 'runtime'
    $manifest = Join-Path $payload 'package-manifest.json'
    $stage = @('-ibuild/package','build/Package.hs','stage','--destination',$payload,
        '--version',$Version,'--platform','windows','--channel',$Channel)
    if ($ProviderDirectory) { $stage += @('--provider',$ProviderDirectory) }
    if ($AllowMissingProvider) { $stage += '--allow-missing-provider' }
    & runghc @stage
    if ($LASTEXITCODE -ne 0) { throw 'Runtime assembly failed' }
    $packager = Join-Path $repo '.build/package-tool/sapiens-package.exe'
    Copy-Item -LiteralPath (Join-Path $repo '.build/bin/sapiens-windows.exe') -Destination (Join-Path $package 'Sapiens4.exe')
    Copy-Item -LiteralPath (Join-Path $repo '.build/bin/WebView2Loader.dll'),(Join-Path $repo '.build/bin/WebView2-LICENSE.txt') -Destination $package
    Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'README.md') -Destination (Join-Path $package 'README.md')
    if ($CertificateThumbprint) {
        Get-ChildItem -LiteralPath $package -Recurse -File | Where-Object { $_.Extension -in @('.exe','.dll') } | ForEach-Object {
            & signtool sign /sha1 $CertificateThumbprint /fd SHA256 /tr $TimestampUrl /td SHA256 $_.FullName
            if ($LASTEXITCODE -ne 0) { throw "Authenticode signing failed: $($_.FullName)" }
            & signtool verify /pa /all /v $_.FullName
            if ($LASTEXITCODE -ne 0) { throw "Authenticode verification failed: $($_.FullName)" }
        }
    }
    $signing = if ($CertificateThumbprint) { 'authenticode' } else { 'none' }
    & $packager finalize --runtime $payload --signing $signing
    if ($LASTEXITCODE -ne 0) { throw 'Final signed runtime inventory failed' }
    [IO.File]::WriteAllText((Join-Path $package 'sapiens-installation.id'), $appId)
    $artifact = Join-Path $staging $artifactName
    Compress-Archive -LiteralPath $package -DestinationPath $artifact
    & (Join-Path $repo 'build/package/windows-installer.ps1') -PackageDirectory $package -OutputDirectory $staging -Version $Version -Channel $Channel -CertificateThumbprint $CertificateThumbprint -TimestampUrl $TimestampUrl
    if ($LASTEXITCODE -ne 0) { throw 'Installer assembly failed' }
    & $packager receipt --artifact $artifact --installer (Join-Path $staging 'Sapiens-Setup.exe') --manifest $manifest --output ($artifact -replace '\.zip$','.release.json')
    if ($LASTEXITCODE -ne 0) { throw 'Release receipt failed' }
    # Publish only fully assembled, signed and verified outputs on this volume.
    [IO.Directory]::Move($package, $destination)
    $published += $destination
    foreach ($name in @($artifactName, 'Sapiens-Setup.exe', $receiptName)) {
        $target = Join-Path $output $name
        [IO.File]::Move((Join-Path $staging $name), $target)
        $published += $target
    }
    $complete = $true
    Write-Host "Built $destination/Sapiens4.exe`nArtifact $(Join-Path $output $artifactName)"
} finally {
    try {
        if (-not $complete) {
            foreach ($path in $published) { Remove-Item -LiteralPath $path -Recurse -Force }
        }
    } finally {
        try {
            if ($staging) { Remove-Item -LiteralPath $staging -Recurse -Force }
        } finally { Pop-Location }
    }
}
