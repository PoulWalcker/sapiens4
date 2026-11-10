param([ValidateSet('key','check')][string]$Mode)
$ErrorActionPreference = 'Stop'
$root = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../..'))
$project = Join-Path $root 'submodules/libs4/haskell'
$cache = Join-Path $project '.cabal'
if ($Mode -eq 'key') {
    $inputs = [ordered]@{}
    foreach ($name in @('RUNNER_OS','RUNNER_ARCH','ImageOS','ImageVersion','MACOSX_DEPLOYMENT_TARGET')) { $inputs[$name] = [Environment]::GetEnvironmentVariable($name) }
    $inputs.image = if ($env:ImageVersion) { $env:ImageVersion } else { $env:GITHUB_RUN_ID }
    if (-not $inputs.image) { throw 'Runner image or run identity required for cache isolation' }
    $inputs.workspace = $root
    $inputs.ghc = (& ghc --info) -join "`n"
    if ($LASTEXITCODE -ne 0) { throw 'GHC identity unavailable' }
    $inputs.cabal = & cabal --numeric-version
    if ($LASTEXITCODE -ne 0) { throw 'Cabal identity unavailable' }
    foreach ($name in @('cabal.project','cabal.project.freeze','sapiens-domain.cabal')) { $inputs[$name] = (Get-FileHash -LiteralPath (Join-Path $project $name) -Algorithm SHA256).Hash }
    $hash = [Security.Cryptography.SHA256]::Create()
    try { $digest = [Convert]::ToHexString($hash.ComputeHash([Text.Encoding]::UTF8.GetBytes(($inputs | ConvertTo-Json -Compress)))).ToLowerInvariant() }
    finally { $hash.Dispose() }
    "key=cabal-deps-v2-$env:RUNNER_OS-$env:RUNNER_ARCH-$digest" >> $env:GITHUB_OUTPUT
} else {
    $databases = @(Get-ChildItem "$cache/store/*/package.db" -Directory -ErrorAction SilentlyContinue)
    $ready = $databases.Count -gt 0 -and (Test-Path "$cache/packages/hackage.haskell.org/01-index.tar" -PathType Leaf)
    $binaries = if ($env:RUNNER_OS -eq 'Windows') { @('sapiens4.exe','sapiens-windows.exe') } else { @('sapiens4','sapiens-macos') }
    foreach ($binary in $binaries) { $ready = $ready -and (Test-Path (Join-Path $root ".build/bin/$binary") -PathType Leaf) }
    $logs = foreach ($database in $databases) {
        $incoming = @(Get-ChildItem (Join-Path $database.Parent.FullName 'incoming') -ErrorAction SilentlyContinue | Where-Object { $_.PSIsContainer -or $_.Extension -ne '.lock' })
        if ($incoming.Count) { $ready = $false }
        & ghc-pkg --no-user-package-db --package-db $database.FullName check 2>&1
        if ($LASTEXITCODE -ne 0) { $ready = $false }
    }
    New-Item -ItemType Directory -Force (Join-Path $root '.build') | Out-Null
    $logs | Set-Content (Join-Path $root '.build/cabal-cache-check.log')
    "ready=$($ready.ToString().ToLowerInvariant())" >> $env:GITHUB_OUTPUT
    # A rejected cache is not a failed application build.
    exit 0
}
