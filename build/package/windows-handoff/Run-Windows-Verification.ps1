[CmdletBinding()]
param(
    [string]$WorkBase = [IO.Path]::GetTempPath(),
    [switch]$VerifyInputsOnly,
    [switch]$RunDesktopChecks,
    [switch]$SkipResearchProbe
)
$ErrorActionPreference = 'Stop'
$bundle = $PSScriptRoot
if (-not (Test-Path -LiteralPath $WorkBase -PathType Container)) { throw 'WorkBase must be an existing writable directory.' }
$work = Join-Path $WorkBase ('sapiens4-verify-' + [Guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $work | Out-Null
$evidence = Join-Path $work 'evidence'
New-Item -ItemType Directory -Path $evidence | Out-Null
[IO.File]::WriteAllText((Join-Path $work '.sapiens-handoff-owned'), 'synthetic-only-v1')
$utf8 = New-Object Text.UTF8Encoding($false)
$steps = [ordered]@{}
foreach ($name in @('inputs','prerequisites','research_probe','ruby_dependencies','native_build','provider_api','core_rspec','desktop_rspec')) {
    $steps[$name] = [ordered]@{status='NOT_RUN'; detail='Not reached.'}
}
$summary = [ordered]@{schema=1; kind='windows-source-build-verification'; prebuilt_sapiens_binary=$false;
    status='FAILED'; source=$null; steps=$steps; tool_versions=[ordered]@{};
    environment=[ordered]@{os=[Environment]::OSVersion.VersionString; process64bit=[Environment]::Is64BitProcess;
      processArchitecture=$env:PROCESSOR_ARCHITECTURE; powershell=$PSVersionTable.PSVersion.ToString()};
    native_windows_execution='NOT_RUN'; real_auth_or_model_request=$false;
    installer='NOT_IMPLEMENTED/NOT_RUN'; publisher_signing='NOT_RUN'; manual_onboarding_ui='NOT_RUN'}
$exitCode = 1

function Safe-Text([string]$Text) {
    foreach ($pair in @(@($work,'<WORK_ROOT>'),@($bundle,'<HANDOFF_ROOT>'),
                       @($env:USERPROFILE,'<USER_PROFILE>'),@($env:LOCALAPPDATA,'<LOCAL_APPDATA>'),@($env:APPDATA,'<APPDATA>'))) {
        if ($pair[0]) { $Text = $Text.Replace($pair[0],$pair[1]).Replace($pair[0].Replace('\','/'),$pair[1]) }
    }
    return [regex]::Replace($Text, '(?im)(Authorization:\s*)[^\r\n]+', '$1<REDACTED>')
}
function Quote-Arg([string]$Value) {
    return '"' + ([regex]::Replace([regex]::Replace($Value,'(\\*)"','$1$1\"'),'(\\+)$','$1$1')) + '"'
}
function Record([string]$Name,[string]$Status,[string]$Detail) {
    $steps[$Name] = [ordered]@{status=$Status; detail=(Safe-Text $Detail)}
    Write-Host "$Name`: $Status - $Detail"
}
function Tool([string]$Name) {
    return (Get-Command $Name -CommandType Application -ErrorAction Stop | Select-Object -First 1).Source
}
function Run-Logged([string]$Name,[string]$Program,[string[]]$Arguments,[int]$Timeout=600,[hashtable]$Extra=@{}) {
    $info = New-Object Diagnostics.ProcessStartInfo
    $info.FileName=$Program; $info.Arguments=($Arguments | ForEach-Object {Quote-Arg $_}) -join ' '
    $info.WorkingDirectory=$source; $info.UseShellExecute=$false; $info.CreateNoWindow=$true
    $info.RedirectStandardOutput=$true; $info.RedirectStandardError=$true
    foreach ($key in @($info.EnvironmentVariables.Keys)) {
        if ($key -match '^(SAPIENS_|CODEX_|OPENAI_)' -or $key -match '^CARGO_REGISTRIES_.*_TOKEN$' -or
            @('NPM_TOKEN','NODE_AUTH_TOKEN','GEM_HOST_API_KEY','GH_TOKEN','GITHUB_TOKEN') -contains $key) {
            $info.EnvironmentVariables.Remove($key)
        }
    }
    foreach ($key in $isolated.Keys) { $info.EnvironmentVariables[$key]=$isolated[$key] }
    foreach ($key in $Extra.Keys) { $info.EnvironmentVariables[$key]=[string]$Extra[$key] }
    $process=New-Object Diagnostics.Process; $process.StartInfo=$info
    $rawOut=Join-Path $work ($Name+'.stdout.raw')
    $rawErr=Join-Path $work ($Name+'.stderr.raw')
    $outFile=[IO.FileStream]::new($rawOut,[IO.FileMode]::CreateNew,[IO.FileAccess]::Write,[IO.FileShare]::ReadWrite,1,$true)
    $errFile=[IO.FileStream]::new($rawErr,[IO.FileMode]::CreateNew,[IO.FileAccess]::Write,[IO.FileShare]::ReadWrite,1,$true)
    $cancel=[Threading.CancellationTokenSource]::new()
    $stdout=$null; $stderr=$null; $failure=$null; $code=$null; $output=''
    try {
        if (-not $process.Start()) { throw "$Name did not start." }
        $stdout=$process.StandardOutput.BaseStream.CopyToAsync($outFile,81920,$cancel.Token)
        $stderr=$process.StandardError.BaseStream.CopyToAsync($errFile,81920,$cancel.Token)
        if (-not $process.WaitForExit($Timeout*1000)) {
            if ($env:OS -eq 'Windows_NT') { & taskkill.exe /PID $process.Id /T /F 2>&1 | Out-Null }
            else { $process.Kill() }
            [void]$process.WaitForExit(10000)
            throw "$Name timed out; termination requested only for the owned command/tree."
        }
        $code=$process.ExitCode
        if (-not $stdout.Wait(15000) -or -not $stderr.Wait(15000)) { throw "$Name output pipes did not close; owned work root retained." }
        if ($code -ne 0) { throw "$Name exited $code; see $Name.log." }
    } catch { $failure=$_.Exception.Message }
    finally {
        $cancel.Cancel()
        foreach ($task in @($stdout,$stderr)) { if ($null -ne $task) { try { [void]$task.Wait(1000) } catch {} } }
        foreach ($stream in @($outFile,$errFile)) { try { $stream.Flush() } catch {}; $stream.Dispose() }
        $output=[IO.File]::ReadAllText($rawOut)
        $errors=[IO.File]::ReadAllText($rawErr)
        $text=$output+"`n"+$errors
        if ($failure) { $text+="`n[driver] "+$failure }
        [IO.File]::WriteAllText((Join-Path $evidence ($Name+'.log')),(Safe-Text $text),$utf8)
        # Raw files are local to this owned work root and never enter the result ZIP.
        foreach ($file in @($rawOut,$rawErr)) { try { Remove-Item -LiteralPath $file -ErrorAction Stop } catch {} }
        $process.Dispose(); $cancel.Dispose()
    }
    if ($failure) { throw $failure }
    return $output
}
function Step([string]$Name,[scriptblock]$Action) {
    try { & $Action; Record $Name 'PASS' 'Completed.' }
    catch { Record $Name 'FAIL' $_.Exception.Message; throw }
}
function Check-Inputs {
    $manifest=Get-Content -Raw -LiteralPath (Join-Path $bundle 'handoff-manifest.json') | ConvertFrom-Json
    if ($manifest.schema -ne 1 -or $manifest.kind -cne 'windows-source-build-verification' -or
        $manifest.prebuiltSapiensBinary -ne $false -or $manifest.sourceCommit -notmatch '^[a-f0-9]{40}$') {
        throw 'Unexpected handoff identity.'
    }
    $root=(Resolve-Path -LiteralPath $bundle).Path.TrimEnd([char[]]@('/','\'))
    $expected=[Collections.Generic.HashSet[string]]::new([StringComparer]::Ordinal)
    foreach ($file in $manifest.files) {
        $relative=[string]$file.path
        if ([IO.Path]::IsPathRooted($relative) -or $relative.Contains('\') -or $relative.Contains(':') -or
            $relative -match '(^|/)\.\.?(/|$)') { throw 'Unsafe handoff file path.' }
        if (-not $expected.Add($relative)) { throw 'Duplicate handoff input.' }
        $path=Join-Path $root $relative
        if (-not (Test-Path -LiteralPath $path -PathType Leaf) -or
            ((Get-Item -LiteralPath $path -Force).Attributes -band [IO.FileAttributes]::ReparsePoint) -or
            (Get-Item -LiteralPath $path -Force).Length -ne $file.size -or
            (Get-FileHash -Algorithm SHA256 -LiteralPath $path).Hash -ne $file.sha256) { throw "Handoff input mismatch: $relative" }
    }
    $observed=[Collections.Generic.HashSet[string]]::new([StringComparer]::Ordinal)
    foreach ($entry in @(Get-ChildItem -LiteralPath $root -Recurse -Force)) {
        if ($entry.Attributes -band [IO.FileAttributes]::ReparsePoint) { throw 'Handoff contains a reparse point.' }
        if (-not $entry.PSIsContainer) {
            $relative=$entry.FullName.Substring($root.Length+1).Replace('\','/')
            if ($relative -cne 'handoff-manifest.json') { $null=$observed.Add($relative) }
        }
    }
    if (-not $expected.SetEquals($observed)) { throw 'Handoff contains extra or missing files.' }
    return $manifest
}

try {
    $manifest=Check-Inputs
    Record 'inputs' 'PASS' 'Bundled source, provider, scripts and evidence hashes match.'
    $summary.source=[ordered]@{commit=$manifest.sourceCommit; submodules=$manifest.submodules; version=$manifest.applicationVersion}
    if ($VerifyInputsOnly) {
        $summary.status='INPUTS_VERIFIED_NATIVE_NOT_RUN'; $exitCode=0
    } else {
        if ($env:OS -ne 'Windows_NT' -or -not [Environment]::Is64BitProcess -or $env:PROCESSOR_ARCHITECTURE -ne 'AMD64') {
            throw 'Use x64 Windows PowerShell on the Windows test context. No native checks ran.'
        }
        $summary.native_windows_execution='Windows driver running; per-step status below is authoritative.'
        $summary['onboarding_posix_scenario']='SKIPPED_BY_EXISTING_WINDOWS_GUARD; persistence and installed-runtime scenarios remain active.'
        $source=Join-Path $work 'source'
        Copy-Item -LiteralPath (Join-Path $bundle 'source') -Destination $source -Recurse
        $cache=Join-Path $work 'isolated-build-cache'
        foreach ($name in @('cargo','npm','gem','bundle','tmp','app-config','app-data','dotnet')) {
            New-Item -ItemType Directory -Path (Join-Path $cache $name) -Force | Out-Null
        }
        $npmConfig=Join-Path $cache 'empty-npmrc'; [IO.File]::WriteAllText($npmConfig,'',$utf8)
        $gitConfig=Join-Path $cache 'empty-gitconfig'; [IO.File]::WriteAllText($gitConfig,'',$utf8)
        $isolated=@{CARGO_HOME=(Join-Path $cache 'cargo'); NPM_CONFIG_CACHE=(Join-Path $cache 'npm');
            NPM_CONFIG_USERCONFIG=$npmConfig; GEM_HOME=(Join-Path $cache 'gem'); BUNDLE_USER_HOME=(Join-Path $cache 'bundle');
            BUNDLE_FROZEN='true'; TEMP=(Join-Path $cache 'tmp'); TMP=(Join-Path $cache 'tmp');
            SAPIENS_CONFIG_DIR=(Join-Path $cache 'app-config'); SAPIENS_DATA_DIR=(Join-Path $cache 'app-data');
            DOTNET_CLI_HOME=(Join-Path $cache 'dotnet'); DOTNET_CLI_TELEMETRY_OPTOUT='1';
            GIT_CONFIG_GLOBAL=$gitConfig; GIT_CONFIG_SYSTEM='NUL'; GIT_TERMINAL_PROMPT='0'; GCM_INTERACTIVE='Never'}
        $powershell=(Get-Process -Id $PID).Path
        Step 'prerequisites' {
            foreach ($tool in @('ghc','runghc','cabal','node','npm','rustc','cargo','ruby','python','git','sh','make','windres')) {
                $null=Tool $tool
            }
            foreach ($entry in @(@('ghc','--numeric-version','9.14.1'),@('cabal','--numeric-version','3.18.2.0'),
                                  @('node','--version','v22.23.2'),@('ruby','--version','ruby 3.4.11'),
                                  @('rustc','--version','rustc 1.99.0'),@('python','--version','Python 3.'))) {
                $value=(Run-Logged ('version-'+$entry[0]) (Tool $entry[0]) @($entry[1]) 30).Trim()
                $summary.tool_versions[$entry[0]]=$value
                if (-not $value.StartsWith($entry[2],[StringComparison]::Ordinal)) { throw ('Wrong tool version: '+$entry[0]) }
            }
            $target=(Run-Logged 'ghc-target' (Tool 'ghc') @('--print-target-platform') 30).Trim()
            if ($target -notmatch '^x86_64-.*mingw') { throw 'x64 Windows GHC required.' }
            $pythonVersion=$summary.tool_versions['python'].Split(' ')[1].Split('.')
            if ([int]$pythonVersion[1] -lt 10) { throw 'Python 3.10+ required.' }
            $cabalCache=Join-Path $source 'submodules/libs4/haskell/.cabal'
            New-Item -ItemType Directory -Path $cabalCache -Force | Out-Null
            [IO.File]::WriteAllText((Join-Path $cabalCache 'config'),"repository hackage.haskell.org`n  url: https://hackage.haskell.org/`n  secure: True`n",$utf8)
        }
        if (-not $SkipResearchProbe) {
            try {
                Step 'research_probe' {
                    $output=Run-Logged 'research-probe' $powershell @('-NoProfile','-NonInteractive','-ExecutionPolicy','Bypass','-File',(Join-Path $bundle 'checks/lifecycle/Run-Probe.ps1')) 1200
                    $match=[regex]::Match($output,'(?m)^RESULT ZIP \(review then return\): (.+)\r?$')
                    if (-not $match.Success) { throw 'Research probe did not return its result ZIP.' }
                    $probeZip=[IO.Path]::GetFullPath($match.Groups[1].Value.Trim())
                    if (-not $probeZip.StartsWith($work+[IO.Path]::DirectorySeparatorChar,[StringComparison]::OrdinalIgnoreCase)) { throw 'Probe ZIP outside owned work root.' }
                    Copy-Item -LiteralPath $probeZip -Destination (Join-Path $evidence 'lifecycle-probe-results.zip')
                }
            } catch { Write-Host 'Research probe failed; retain its log and continue independent build checks.' }
            if (-not (Test-Path -LiteralPath (Join-Path $evidence 'lifecycle-probe-results.zip'))) {
                Get-ChildItem -LiteralPath (Join-Path $cache 'tmp') -Filter 'sapiens-lifecycle-probe-*-results.zip' |
                    Select-Object -First 1 | ForEach-Object { Copy-Item -LiteralPath $_.FullName -Destination (Join-Path $evidence 'lifecycle-probe-results.zip') }
            }
        }
        $ruby=Tool 'ruby'; $python=Tool 'python'
        Step 'ruby_dependencies' { $null=Run-Logged 'ruby-dependencies' $ruby @('submodules/libs4/ruby/bundle.rb','install') 900 }
        $package=Join-Path $work 'unsigned-package'
        $provider=Join-Path $bundle 'inputs/provider-package/runtime/codex'
        $buildWrapper=Join-Path $work 'Invoke-Unsigned-Build.ps1'
        [IO.File]::WriteAllText($buildWrapper,@'
param($Source,$Provider,$Output,$Version)
$ErrorActionPreference='Stop'
Set-Location -LiteralPath $Source
& (Join-Path $Source 'environments/windows/build.ps1') -Runtime win-x64 -ProviderDirectory $Provider -OutputDirectory $Output -Version $Version -CertificateThumbprint ([string]::Empty)
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
'@,$utf8)
        Step 'native_build' {
            $null=Run-Logged 'native-build' $powershell @('-NoProfile','-NonInteractive','-ExecutionPolicy','Bypass','-File',$buildWrapper,$source,$provider,$package,$manifest.applicationVersion) 7200
        }
        Step 'provider_api' {
            $apiEvidence=Join-Path $work 'd3-api-evidence'
            $null=Run-Logged 'provider-api' $powershell @('-NoProfile','-NonInteractive','-ExecutionPolicy','Bypass','-File',(Join-Path $bundle 'checks/D3_WINDOWS_ACCEPTANCE.ps1'),'-Package',$package,'-EvidenceRoot',$apiEvidence,'-ProviderStage','finalized') 600
            Copy-Item -LiteralPath (Join-Path $apiEvidence 'result.json') -Destination (Join-Path $evidence 'd3-api-result.json')
        }
        $testEnv=@{SAPIENS_TEST_CODEX=(Join-Path $source '.build/bin/sapiens-provider.exe');
            SAPIENS_TEST_PACKAGE_RUNTIME=(Join-Path $package 'runtime'); SAPIENS_TEST_WINDOWS_PACKAGE=$package}
        Step 'core_rspec' {
            $null=Run-Logged 'core-rspec' $ruby @('submodules/libs4/ruby/bundle.rb','exec','rspec','spec/onboarding_spec.rb','spec/service_spec.rb:2','spec/native_package_spec.rb') 900 $testEnv
        }
        if ($RunDesktopChecks) {
            Step 'desktop_rspec' {
                $null=Run-Logged 'desktop-rspec' $ruby @('submodules/libs4/ruby/bundle.rb','exec','rspec','spec/windows_package_spec.rb','spec/windows_startup_spec.rb') 900 $testEnv
            }
        }
        Get-ChildItem -LiteralPath $work -Filter '*.release.json' | ForEach-Object { Copy-Item -LiteralPath $_.FullName -Destination $evidence }
        Copy-Item -LiteralPath (Join-Path $package 'runtime/package-manifest.json') -Destination (Join-Path $evidence 'package-manifest.json')
        $failed=@($steps.Values | Where-Object {$_.status -eq 'FAIL'}).Count
        if ($failed) { throw 'One or more requested stages failed; see per-step results.' }
        $summary.status='AUTOMATED_CHECKS_PASSED_REMAINING_ACCEPTANCE_NOT_RUN'; $exitCode=0
    }
} catch {
    $summary['error']=Safe-Text $_.Exception.Message
    if ($steps['inputs'].status -eq 'NOT_RUN') { Record 'inputs' 'FAIL' $_.Exception.Message }
} finally {
    [IO.File]::WriteAllText((Join-Path $evidence 'results.json'),($summary|ConvertTo-Json -Depth 12),$utf8)
    [IO.File]::WriteAllText((Join-Path $evidence 'README.txt'),'Sanitized automated evidence only. Manual UI, real account/model, installer and signing acceptance are separate. Review this ZIP before sharing; keep build outputs in the printed owned work root.',$utf8)
    Add-Type -AssemblyName System.IO.Compression.FileSystem
    $resultZip=$work+'-results.zip'
    [IO.Compression.ZipFile]::CreateFromDirectory($evidence,$resultZip)
    Write-Host ('OWNED WORK ROOT (retained): '+$work)
    Write-Host ('RESULT ZIP (review then return): '+$resultZip)
    Write-Host ('STATUS: '+$summary.status)
}
exit $exitCode
