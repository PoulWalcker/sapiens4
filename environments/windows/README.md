# Sapiens4 for Windows

Run **Sapiens-Setup.exe** to install for the current user without administrator
rights. Launch **Sapiens4** (Stable) or **Sapiens4 Development** (Dev) from Start.
Files are under `%LOCALAPPDATA%\Programs\APP_NAME`; uninstall through Windows Settings.
The portable ZIP remains available: extract it whole and run **Sapiens4.exe**.
Both contain the native shell, backend, UI, Blindly, pinned Codex provider and
WebView2 loader. No Python, .NET, WSL or build tools are needed to run the app.

Microsoft Edge WebView2 Evergreen Runtime is required. If it is missing, the
window offers **Install WebView2**, **Retry** and **Cancel**. Install opens the
official Microsoft download; run that installer and choose Retry. Cancel exits.

## Build

Use x64 GHC 9.14.1, Cabal 3.18.2.0, Node 22.23.2, Rust 1.99.0 with MSVC C++
build tools, Git, a POSIX shell, GNU Make, curl and tar. Checks use Ruby 3.4.11 and Bundler 4.0.21.
Keep GHC, shell and Make paths free of spaces and use shell/Make from the same
POSIX distribution. Use x64 GHC and Node on Windows ARM. Run in an MSVC developer
shell with the pinned tools on PATH. Initialize the pinned submodules, configure
the isolated Cabal index if absent, then build from the repository root:

```powershell
git submodule sync --recursive
git submodule update --init --recursive
$cabalConfig = 'submodules/libs4/haskell/.cabal/config'
if (-not (Test-Path $cabalConfig)) {
    New-Item -ItemType Directory -Force (Split-Path $cabalConfig) | Out-Null
    "repository hackage.haskell.org`n  url: https://hackage.haskell.org/`n  secure: True`n" | Set-Content $cabalConfig
}
.\environments\windows\build.ps1 -Runtime win-x64 -Version 0.1.0-dev
.\.build\windows\Sapiens-Setup.exe
```

For an early check of the UI toolchain, run
`powershell.exe -NoProfile -File .\build\windows-ui-preflight.ps1` before the full
build. The separate process keeps its cache environment changes out of the build shell.

To capture a build log in Windows PowerShell 5, redirect through `cmd.exe`.
PowerShell's `*>` redirection can turn native-tool stderr warnings into terminating
errors under the build's `Stop` policy. Use a fresh output directory:

```powershell
New-Item -ItemType Directory -Force .build | Out-Null
cmd.exe /d /c "powershell.exe -NoProfile -File environments\windows\build.ps1 -OutputDirectory .build/windows-logged > .build/windows-build.log 2>&1"
if ($LASTEXITCODE) { throw 'Windows build failed; see .build/windows-build.log' }
```

The build downloads/checks pinned Inno Setup 7.1.1 and installs its compiler below
`.build/tools/` for the current build user. No compiler is needed by app users.
Outputs in `.build/windows/` are `Sapiens-Setup.exe`, `Sapiens4-Development-win-x64/`,
`sapiens4-development-0.1.0-dev-windows-x64.zip` and its `.release.json` receipt.
Use a new `-OutputDirectory` for another build; it contains the package directory,
Setup, ZIP and receipt. The integrated build acquires
or verifies `.build/codex`; `-ProviderDirectory` selects a prepared payload.
`-BuildOnly` compiles the backend/UI/native shell and Blindly and prepares source
icons/WebView2 without packaging or acquiring Codex. `-SkipBuild` packages existing
outputs with caller-verified provenance. Use `-BuildOnly` for desktop test preparation, including Blindly and the WebView2 loader. `-AllowMissingProvider`
is for native validation only. `-BlindlyToolchain gnullvm` selects LLVM-MinGW
instead of MSVC. The WebView2 loader is checksum-pinned. Publisher signing uses
`-CertificateThumbprint`; see the [release runbook](../../docs/release-development.md).

`-Channel stable`: `Sapiens4-win-x64/`, `sapiens4-VERSION-windows-x64.zip`; default is Dev.

## State and verification

CLI and GUI, Development and Stable all default to `%LOCALAPPDATA%\Sapiens4\data`,
with configuration in the adjacent `config` directory. `SAPIENS_DATA_DIR` and
`SAPIENS_CONFIG_DIR` override these independently; a config override does not
move default data. Use isolated overrides for manual checks.
The app owns its host, uses a dynamic loopback port,
and stops the host on close. Production update discovery is disabled.

The host inherits `CODEX_*` and `OPENAI_*`, including `CODEX_HOME`, so explicit
authentication and CLI configuration remain available. D1 does not manage accounts.
Bundled Codex remains the default; `SAPIENS_CODEX_BINARY` explicitly overrides it.

```powershell
ruby submodules/libs4/ruby/bundle.rb install
$env:SAPIENS_TEST_PACKAGE_RUNTIME = (Resolve-Path .build/windows/Sapiens4-Development-win-x64/runtime).Path
ruby submodules/libs4/ruby/bundle.rb exec rspec spec/native_package_spec.rb
$env:SAPIENS_TEST_WINDOWS_PACKAGE = (Resolve-Path .build/windows/Sapiens4-Development-win-x64).Path
ruby submodules/libs4/ruby/bundle.rb exec rspec spec/windows_package_spec.rb
```

For Stable checks: `$env:PACKAGE_CHANNEL = 'stable'`, paths use `Sapiens4-win-x64`.

The package scenario checks inventories, provider version and backend restart
with synthetic state. Desktop checks require an unlocked interactive session,
accessible controls and matching privilege levels; run them serially.

## Installer safety and remaining acceptance

The installer keeps the selected channel identity and stores data/config/auth outside
its program directory. It never runs auth, automatically stops apps, restarts
processes, or deletes external state. Quit Sapiens before repair/uninstall.
A busy owned executable or failed WMI process check refuses the operation.
This is a process snapshot, not a race-free lifecycle lock; a launch after the
check remains a limitation. Elevated uninstaller respawn is not accepted as proven.

Uninstall removes recorded application files, shortcuts and registration. Unknown
files and a small ownership marker remain so reinstall can recognize that directory.
An existing nonempty unowned directory or the data root cannot be an install target.

Recorded development checks have passed silent install/reinstall, isolated launch/health/close,
active-operation refusal, inventories and uninstall preserving selected synthetic
sentinels. This does not cover complete user state or racing launches.
Windows 11 ARM, missing-WebView2 Install/Retry/Cancel, normal-user clean-machine
and publisher-trust acceptance remain open.
WebView2 Evergreen is an external prerequisite; its download needs Internet.
Unsigned manual builds may show Windows trust warnings. No updater is installed.
