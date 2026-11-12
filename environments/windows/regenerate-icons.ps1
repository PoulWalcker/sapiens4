$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../..'))
& npm ci --include=dev --prefix (Join-Path $repo 'submodules/libs4')
if ($LASTEXITCODE -ne 0) { throw 'Icon dependency installation failed' }
$paths = @()
foreach ($icon in @(@('favicon.svg','AppIcon.ico'),@('favicon-dark.svg','AppIconDark.ico'))) {
    $source = 'app/shell/' + $icon[0]
    $target = 'environments/windows/' + $icon[1]
    & node (Join-Path $repo 'submodules/libs4/tools/svg-to-ico.cjs') (Join-Path $repo $source) (Join-Path $repo $target)
    if ($LASTEXITCODE -ne 0) { throw 'Icon generation failed' }
    $paths += @($source, $target)
}
$hashes = foreach ($path in $paths) {
    (Get-FileHash -Algorithm SHA256 -LiteralPath (Join-Path $repo $path)).Hash.ToLowerInvariant() + '  ' + $path
}
[IO.File]::WriteAllLines((Join-Path $PSScriptRoot 'icons.sha256'), [string[]]$hashes)
