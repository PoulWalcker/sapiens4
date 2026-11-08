# Sapiens4 Development for Windows

Extract the whole development ZIP and run **Sapiens4.exe**. The x64 payload targets
Intel/AMD PCs and Windows 11 ARM PCs with x64 app support. It contains the native
shell, Haskell host, generated UI, Blindly4, complete pinned Codex payload,
WebView2 loader and licenses. Python, .NET, WSL and build tools are not runtime
requirements.

Microsoft Edge WebView2 Evergreen Runtime is required in addition to its loader.
If missing, the native window offers **Install WebView2**, **Retry** and **Cancel**
before starting the host. Install opens Microsoft's official installer download;
run the downloaded installer and choose Retry. Cancel closes the app.

This lane currently emits a portable ZIP. Per-user installer integration and
actual Windows native/install acceptance remain required; the ZIP is not an
installer-completion claim. D3's packaged onboarding must be integrated before
user-ready authentication acceptance.

## State and shutdown

Development data lives in `%LOCALAPPDATA%\Sapiens4 Development\data`, with
configuration in the adjacent `config` directory. The host is owned by the native
app, starts on a dynamic port and receives explicit package/data/config paths.
Closing the app stops its server and confirms exit. Explicit `SAPIENS_DATA_DIR`
and `SAPIENS_CONFIG_DIR` launch overrides are available for synthetic validation.
No existing production installation or historical data is automatically adopted.
Automatic production update discovery is disabled.

## Build from source

Builders need x64 GHC 9.14.1, Cabal 3.18.2, Node.js 22.5+, Ruby 3.4 for checks,
Python 3.10+, Git, a POSIX shell, GNU Make and Rust with MSVC C++ build tools or
LLVM-MinGW. Keep GHC, shell and Make paths free of spaces; upstream dependency
tools split those paths. Use x64 GHC and Node on Windows ARM.

Initialize exact submodule pins and acquire D3's complete verified Windows x64
provider after integration. From the repository root:

```powershell
.\environments\windows\build.ps1 -Runtime win-x64 -Version 0.1.0-dev -ProviderDirectory C:\verified\codex
.\.build\Sapiens4-Development-win-x64\Sapiens4.exe
```

`-SkipBuild` reuses outputs with caller-verified provenance. `-AllowMissingProvider`
is native validation only. `-BlindlyToolchain gnullvm` selects LLVM-MinGW.
The WebView2 loader is checksum-pinned. Output directories and versioned ZIPs
must be new. Publisher signing uses `-CertificateThumbprint`; nested executable
signing finishes before provider/runtime inventories and the external ZIP receipt
are calculated. See [the release runbook](../../docs/release-development.md).

## Verification

```powershell
$env:SAPIENS_TEST_PACKAGE_RUNTIME = (Resolve-Path .build/Sapiens4-Development-win-x64/runtime).Path
ruby submodules/libs4/ruby/bundle.rb exec rspec spec/native_package_spec.rb
$env:SAPIENS_TEST_WINDOWS_PACKAGE = (Resolve-Path .build/Sapiens4-Development-win-x64).Path
ruby submodules/libs4/ruby/bundle.rb exec rspec spec/windows_package_spec.rb
```

The shared package scenario checks inventories/provider version and a real
backend restart using synthetic state. Desktop scenarios require an unlocked
interactive session; Blindly4 requires accessible controls and matching privilege
levels. Run desktop workflows serially. Actual Explorer launch, WebView2
absent/install/retry/cancel, UI/Blindly, shutdown, Windows 11 ARM x64 emulation,
installer and signed downloaded artifacts still need native acceptance. A Mac
Haskell typecheck does not establish those results.
