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
        & ./environments/windows/build.ps1 -BuildOnly
        if ($LASTEXITCODE -ne 0) { throw 'Application compilation failed' }
    }
    $arguments = @('--port', $Port, '--data-dir', $DataDir)
    if ($Open) { $arguments += '--open' }
    & ./.build/bin/sapiens4.exe @arguments
    exit $LASTEXITCODE
} finally { Pop-Location }
