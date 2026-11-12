param(
    [Parameter(Mandatory=$true)][string]$Setup,
    [Parameter(Mandatory=$true)][string]$Evidence,
    [string]$Cases = 'A B C D E',
    [string]$Ruby = 'ruby.exe'
)
$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../..'))
$Evidence = [IO.Path]::GetFullPath($Evidence)
if (Test-Path $Evidence) { throw 'Evidence directory must be new' }
New-Item -ItemType Directory $Evidence | Out-Null
$env:D1_CLEANUP_SETUP = [IO.Path]::GetFullPath($Setup)
$env:D1_CLEANUP_EVIDENCE = $Evidence.Replace('\','/')
$env:D1_CLEANUP_CASES = $Cases
$env:D1_CLEANUP_REVISION = & git -c ('safe.directory=' + $repo.Replace('\','/')) -C $repo rev-parse HEAD
if ($LASTEXITCODE -ne 0) { throw 'Cannot record diagnostic revision' }
$env:SAPIENS_CONFIG_DIR = Join-Path $Evidence 'runner-config'
$env:CODEX_HOME = Join-Path $Evidence 'runner-codex'
$env:SAPIENS_TEST_CODEX = Join-Path $repo '.build/bin/sapiens-provider.exe'
$env:SAPIENS_TEST_BLINDLY4 = Join-Path $repo 'submodules/blindly4/.build/windows/blindly4.exe'
$ruby = (Get-Command $Ruby -ErrorAction Stop).Source
$bundle = Join-Path $repo 'submodules/libs4/ruby/bundle.rb'
$arguments = '"' + $bundle + '" exec rspec --tag windows --order defined spec/installed_distribution_spec.rb'
$p = Start-Process $ruby -ArgumentList $arguments -WorkingDirectory $repo -Wait -PassThru -RedirectStandardOutput (Join-Path $Evidence 'matrix.log') -RedirectStandardError (Join-Path $Evidence 'matrix.err')
[IO.File]::WriteAllText((Join-Path $Evidence 'matrix.exit'), [string]$p.ExitCode)
exit $p.ExitCode
