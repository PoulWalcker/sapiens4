$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$libs = Join-Path $repo 'submodules/libs4'
$bin = Join-Path $libs '.cache/bin'
& npm ci --prefix $libs
if ($LASTEXITCODE -ne 0) { throw 'UI dependency installation failed' }
New-Item -ItemType Directory -Force $bin | Out-Null
Copy-Item (Join-Path $libs 'node_modules/purescript/purs.bin') (Join-Path $bin 'purs.exe') -Force
$env:PATH = $bin + ';' + $env:PATH
$env:LOCALAPPDATA = Join-Path $libs '.cache'
$env:XDG_CACHE_HOME = $env:LOCALAPPDATA
& (Join-Path $bin 'purs.exe') --version
if ($LASTEXITCODE -ne 0) { throw 'Native PureScript compiler failed' }
& node (Join-Path $libs 'node_modules/esbuild/bin/esbuild') --version
if ($LASTEXITCODE -ne 0) { throw 'esbuild failed' }
Push-Location (Join-Path $libs 'purescript')
try {
    $spago = Join-Path $libs 'node_modules/spago/bin/bundle.js'
    & node $spago install --verbose
    if ($LASTEXITCODE -ne 0) { throw 'Spago install failed' }
    $sources = & node $spago sources --json --quiet
    if ($LASTEXITCODE -ne 0 -or @($sources | ConvertFrom-Json).Count -eq 0) { throw 'Spago sources failed' }
} finally { Pop-Location }
Write-Output 'Windows UI tools passed before application compilation'
