param(
    [string]$OutputDirectory,
    [string]$Version = '0.1.0',
    [ValidateSet('win-x64','win-arm64')][string]$Runtime,
    [ValidateSet('msvc','gnullvm')][string]$BlindlyToolchain = 'msvc',
    [switch]$PublicRelease,
    [string]$CertificateThumbprint = $env:SAPIENS_WINDOWS_CERT_THUMBPRINT,
    [string]$TimestampUrl = 'http://timestamp.digicert.com'
)
$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../..'))
if ($Version -notmatch '^\d+\.\d+\.\d+$') { throw 'Version must be MAJOR.MINOR.PATCH' }
if ($PublicRelease -and ($CertificateThumbprint -notmatch '^[a-fA-F0-9]{40}$' -or $TimestampUrl -notmatch '^https?://[A-Za-z0-9./:_-]+$')) { throw 'Public release requires a valid signing certificate and timestamp URL' }
$target = & ghc --print-target-platform
if ($LASTEXITCODE -ne 0) { throw 'Cannot determine the GHC target' }
$ghcRuntime = switch -Regex ($target) {
    '^x86_64-.*mingw' { 'win-x64'; break }
    '^aarch64-.*mingw' { 'win-arm64'; break }
    default { throw "Windows x64 or ARM64 GHC required; got $target" }
}
if (-not $Runtime) { $Runtime = $ghcRuntime }
if ($Runtime -ne $ghcRuntime) { throw "Requested $Runtime does not match GHC target $target" }
$machine = if ($Runtime -eq 'win-arm64') { 0xaa64 } else { 0x8664 }
$installerArchitecture = if ($Runtime -eq 'win-arm64') { 'arm64' } else { 'x64compatible' }
$artifactName = "Sapiens-Setup-$Runtime.exe"
if (-not $OutputDirectory) { $OutputDirectory = Join-Path $repo ".build/$Runtime" }
$output = [IO.Path]::GetFullPath($OutputDirectory).TrimEnd('\','/')
if (Test-Path -LiteralPath $output) { throw 'Output must be a new directory' }
if ($repo -eq $output -or $repo.StartsWith($output + [IO.Path]::DirectorySeparatorChar, [StringComparison]::OrdinalIgnoreCase)) { throw 'Output must not contain the source checkout' }
$compiler = (Get-Command ISCC.exe -ErrorAction Stop).Source
# Audit every PE, including delay-import DLLs reported by compiler tooling.
function Audit-Payload($Package) {
    $files = @(Get-ChildItem -LiteralPath $Package -Recurse -File | Where-Object { $_.Extension -in @('.exe','.dll') })
    $system = @('advapi32.dll','bcrypt.dll','comctl32.dll','comdlg32.dll','crypt32.dll','d3d11.dll','dbghelp.dll','dnsapi.dll','dwmapi.dll','dxgi.dll','gdi32.dll','imm32.dll','iphlpapi.dll','kernel32.dll','msvcrt.dll','ncrypt.dll','normaliz.dll','ntdll.dll','ole32.dll','oleaut32.dll','powrprof.dll','propsys.dll','psapi.dll','rpcrt4.dll','secur32.dll','setupapi.dll','shell32.dll','shlwapi.dll','ucrtbase.dll','user32.dll','userenv.dll','uxtheme.dll','version.dll','winhttp.dll','wininet.dll','winmm.dll','winspool.drv','wintrust.dll','ws2_32.dll','wtsapi32.dll')
    foreach ($file in $files) {
        # Read the PE machine field rather than relying on tool-specific format names.
        $reader = [IO.BinaryReader]::new([IO.File]::OpenRead($file.FullName))
        try {
            if ($reader.ReadUInt16() -ne 0x5a4d) { throw "Not a PE image: $($file.FullName)" }
            $reader.BaseStream.Position = 0x3c
            $peOffset = $reader.ReadUInt32()
            $reader.BaseStream.Position = $peOffset
            if ($reader.ReadUInt32() -ne 0x4550 -or $reader.ReadUInt16() -ne $machine) { throw "Not a $Runtime PE: $($file.FullName)" }
        } finally { $reader.Dispose() }
        $headers = & objdump -p $file.FullName
        if ($LASTEXITCODE -ne 0) { throw "Cannot inspect PE imports: $($file.FullName)" }
        foreach ($line in $headers) {
            if ($line -match 'DLL Name:\s*(\S+)') {
                $dependency = $Matches[1].ToLowerInvariant()
                if ($dependency -notmatch '^(api-ms-win-|ext-ms-win-)' -and $dependency -notin $system) {
                    # Windows only searches the executable's directory for our
                    # private dependencies; a DLL elsewhere in the tree is insufficient.
                    $adjacent = Join-Path $file.DirectoryName $dependency
                    if (-not (Test-Path -LiteralPath $adjacent -PathType Leaf)) { throw "Unresolved PE dependency $dependency in $($file.FullName)" }
                }
            }
        }
    }
}
function Verify-Signature($Path) {
    & signtool verify /pa /all /v $Path
    if ($LASTEXITCODE -ne 0) { throw "Authenticode verification failed: $Path" }
}
$staging = $null
Push-Location $repo
try {
    if (-not (Test-Path (Join-Path $repo 'submodules/libs4/node_modules/sharp/package.json'))) {
        & npm ci --prefix (Join-Path $repo 'submodules/libs4')
        if ($LASTEXITCODE -ne 0) { throw 'Icon/UI dependency installation failed' }
    }
    foreach ($icon in @(@('favicon.svg','AppIcon.ico'),@('favicon-dark.svg','AppIconDark.ico'))) {
        & node (Join-Path $repo 'submodules/libs4/tools/svg-to-ico.cjs') (Join-Path $repo ('app/shell/'+$icon[0])) (Join-Path $PSScriptRoot $icon[1])
        if ($LASTEXITCODE -ne 0) { throw 'Icon preparation failed' }
    }
    & runghc build/Build.hs
    if ($LASTEXITCODE -ne 0) { throw 'Haskell application build failed' }
    $previousRustFlags = $env:RUSTFLAGS
    try {
        $env:RUSTFLAGS = "$previousRustFlags -C target-feature=+crt-static"
        & (Join-Path $repo 'submodules/blindly4/windows/build.ps1') -Runtime $Runtime -Toolchain $BlindlyToolchain
        if ($LASTEXITCODE -ne 0) { throw 'Blindly4 build failed' }
    } finally { $env:RUSTFLAGS = $previousRustFlags }
    $parent = Split-Path -Parent $output
    New-Item -ItemType Directory -Path $parent -Force | Out-Null
    $candidate = Join-Path $parent ('.sapiens-package.' + [Guid]::NewGuid().ToString('N'))
    New-Item -ItemType Directory -Path $candidate | Out-Null
    $staging = $candidate
    $package = Join-Path $staging 'Sapiens4'
    New-Item -ItemType Directory -Path $package | Out-Null
    & runghc -XOverloadedStrings -XTupleSections -icorpora/host build/Package.hs stage (Join-Path $package 'runtime')
    if ($LASTEXITCODE -ne 0) { throw 'Runtime assembly failed' }
    Copy-Item -LiteralPath (Join-Path $repo '.build/bin/sapiens-windows.exe') -Destination (Join-Path $package 'Sapiens4.exe')
    Copy-Item -LiteralPath (Join-Path $repo '.build/bin/WebView2Loader.dll'),(Join-Path $repo '.build/bin/WebView2-LICENSE.txt') -Destination $package
    [IO.File]::WriteAllText((Join-Path $package 'sapiens-installation.id'), 'com.poulwalcker.sapiens4')
    Audit-Payload $package
    if ($PublicRelease) {
        Get-ChildItem -LiteralPath $package -Recurse -File | Where-Object { $_.Extension -in @('.exe','.dll') } | ForEach-Object {
            & signtool sign /sha1 $CertificateThumbprint /fd SHA256 /tr $TimestampUrl /td SHA256 $_.FullName
            if ($LASTEXITCODE -ne 0) { throw "Authenticode signing failed: $($_.FullName)" }
            Verify-Signature $_.FullName
        }
    }
    $product = Join-Path $staging 'product'
    New-Item -ItemType Directory -Path $product | Out-Null
    $arguments = @('/DPackageName=Sapiens4','/DPackageId=com.poulwalcker.sapiens4','/Qp',("/DSourceDir=$package"),("/DOutputDir=$product"),("/DAppVersion=$Version"),("/DFileVersion=$Version"),("/DPackageArchitecture=$installerArchitecture"),("/DArtifactBaseName="+[IO.Path]::GetFileNameWithoutExtension($artifactName)))
    if ($PublicRelease) {
        $arguments += '/DPublisherSign'
        $arguments += ('/Ssapiens=signtool.exe sign /sha1 '+$CertificateThumbprint+' /fd SHA256 /tr '+$TimestampUrl+' /td SHA256 $f')
    }
    & $compiler @arguments (Join-Path $repo 'build/package/windows-installer.iss')
    if ($LASTEXITCODE -ne 0) { throw 'Inno Setup compilation failed' }
    $installer = Join-Path $product $artifactName
    if ($PublicRelease) { Verify-Signature $installer }
    $checksum = (Get-FileHash -LiteralPath $installer -Algorithm SHA256).Hash.ToLowerInvariant()
    [IO.File]::WriteAllText((Join-Path $product 'SHA256SUMS'), "$checksum  $artifactName`n")
    # Directory.Move refuses any existing destination, including a racing build.
    [IO.Directory]::Move($product, $output)
    Write-Host "Installer $(Join-Path $output $artifactName)"
} finally {
    try { if ($staging) { Remove-Item -LiteralPath $staging -Recurse -Force } }
    finally { Pop-Location }
}
