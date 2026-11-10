param([switch]$Open, [int]$Port = 4174, [string]$DataDir = (Join-Path $PSScriptRoot '../.sapiens4'), [switch]$SkipBuild)
$ErrorActionPreference = 'Stop'
Push-Location (Join-Path $PSScriptRoot '..')
try {
    if (-not (Test-Path ./submodules/libs4/web/remote-vendor.js)) {
        git submodule update --init --recursive -- submodules/libs4
        if ($LASTEXITCODE -ne 0) { throw 'Cannot initialize libs4' }
    }
    if (-not $SkipBuild) {
        if (-not (Test-Path ./submodules/blindly4/windows/Cargo.toml)) {
            git submodule update --init --recursive -- submodules/blindly4
            if ($LASTEXITCODE -ne 0) { throw 'Cannot initialize Blindly4' }
        }
        & ./submodules/blindly4/windows/build.ps1
        if ($LASTEXITCODE -ne 0) { throw 'Blindly4 build failed' }
        & runghc build/Build.hs
        if ($LASTEXITCODE -ne 0) { throw 'Haskell application build failed' }
    }
    if (-not $env:SAPIENS_CODEX_BINARY) {
        & python build/acquire-codex.py --platform windows-x64 --destination .build/codex --ensure
        if ($LASTEXITCODE -ne 0) { throw 'Codex acquisition failed' }
    }
    $arguments = @('--port', $Port, '--data-dir', $DataDir)
    if ($Open) { $arguments += '--open' }
    & ./.build/bin/sapiens4.exe @arguments
    exit $LASTEXITCODE
} finally { Pop-Location }
