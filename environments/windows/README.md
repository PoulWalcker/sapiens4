# Sapiens4 Development for Windows

Extract the whole portable ZIP and run **Sapiens4.exe**. The x64 app includes
the native shell, owned backend, UI, Blindly, complete pinned Codex provider,
WebView2 loader and licenses. Python, .NET, WSL and build tools are builder-only.

Microsoft Edge WebView2 Evergreen Runtime is required. If it is missing, the
window offers **Install WebView2**, **Retry** and **Cancel**. Install opens the
official Microsoft download; run that installer and choose Retry. Cancel exits.

## Build

Use x64 GHC 9.14.1, Cabal 3.18.2.0, Node 22.23.2, Rust 1.99.0 with MSVC C++
build tools, Python 3.10+, Git, a POSIX shell and GNU Make. Checks use Ruby 3.4.11 and Bundler 4.0.21.
Keep GHC, shell and Make paths free of spaces. Use x64 GHC and Node on Windows ARM.
Initialize the pinned submodules, then run from the repository root:

```powershell
git config submodule.blindly4.url https://github.com/aleski-green/blindly4.git
git submodule update --init --recursive
python build/package/acquire-provider.py --platform windows-x64 --destination runtime/codex
.\environments\windows\build.ps1 -Runtime win-x64 -Version 0.1.0-dev -ProviderDirectory "$PWD/runtime/codex"
.\.build\Sapiens4-Development-win-x64\Sapiens4.exe
```

Outputs in `.build/` are `Sapiens4-Development-win-x64/`,
`sapiens4-development-0.1.0-dev-windows-x64.zip` and its `.release.json` receipt.
Use `-OutputDirectory` for another build; existing package/ZIP outputs are rejected.
`-SkipBuild` reuses binaries with caller-verified provenance. `-AllowMissingProvider`
is for native validation only. `-BlindlyToolchain gnullvm` selects LLVM-MinGW
instead of MSVC. The WebView2 loader is checksum-pinned. Publisher signing uses
`-CertificateThumbprint`; see the [release runbook](../../docs/release-development.md).

## State and verification

Data defaults to `%LOCALAPPDATA%\Sapiens4 Development\data`, with configuration
in the adjacent `config` directory. `SAPIENS_DATA_DIR` and `SAPIENS_CONFIG_DIR`
allow synthetic validation. The app owns its host, uses a dynamic loopback port,
and stops the host on close. Production update discovery is disabled.

```powershell
ruby submodules/libs4/ruby/bundle.rb install
$env:SAPIENS_TEST_PACKAGE_RUNTIME = (Resolve-Path .build/Sapiens4-Development-win-x64/runtime).Path
ruby submodules/libs4/ruby/bundle.rb exec rspec spec/native_package_spec.rb
$env:SAPIENS_TEST_WINDOWS_PACKAGE = (Resolve-Path .build/Sapiens4-Development-win-x64).Path
ruby submodules/libs4/ruby/bundle.rb exec rspec spec/windows_package_spec.rb
```

The package scenario checks inventories, provider version and backend restart
with synthetic state. Desktop checks require an unlocked interactive session,
accessible controls and matching privilege levels; run them serially.

## Required installer and acceptance work

Current packaging produces a portable ZIP. A per-user installer remains required,
including Start-menu/Explorer launch, repeat installation, and uninstall that
preserves external data/configuration. Its generated helpers and installer need
publisher signing and final inventory verification. Keep application data outside
the replaceable installation directory and retain the existing WebView2 UI.

Native launch/UI/Blindly/shutdown, WebView2 install/retry/cancel, Windows 11 ARM
x64 emulation, and downloaded signed installer acceptance remain open.
