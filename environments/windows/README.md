# Sapiens4 for Windows

Run **Sapiens-Setup-win-x64.exe** on Windows 10/11 x64. Setup installs per user without
administrator access and creates a Start menu shortcut. No Sapiens build tools
are needed. The app uses the system WebView2 Runtime; native Install/Repair,
Retry and Cancel controls handle a missing Runtime before starting the backend.
Main-window loading failures restore these controls after closing the failed
views and stopping the owned backend; Retry starts a fresh attempt.
No browser Runtime or Codex CLI is bundled. Model execution uses your installed
Codex CLI and existing authentication. Native npm binaries are supported;
`SAPIENS_CODEX_BINARY` selects an executable explicitly.

Data and configuration live in `%LOCALAPPDATA%\Sapiens4\{data,config}`, outside the
installation by default. A fresh installation opens Welcome + Storage before
starting the backend. Start fresh initializes the empty default directory;
Use another folder opens a complete existing Data Root in place or initializes
an empty selected folder. The selection is saved only after Workspace loads.
Later launches validate and reuse it; missing, invalid or locked data returns
to the storage screen without creating replacement data. Quit stops the owned
backend. Quit before replacement or uninstall;
Setup refuses active installation processes, unowned nonempty destinations and
protected state directories, and destinations with junction/symlink parents. Uninstall preserves data, configuration and unowned
files. Process checks are preflight safeguards, not an atomic installation lock.
Uninstall removes the installed ownership marker and registration. Reinstall
into the empty installation directory; retained unowned files require a new directory.
Pre-D1 installations that stored data directly in `%LOCALAPPDATA%\Sapiens4`
are discovered and validated when no selection is saved. Data stays in place;
`SAPIENS_DATA_DIR` explicitly overrides the saved selection. The launcher and
backend share the stable profile in `runtime/package-manifest.json`.

## Build and verify

Install GHC 9.14.1 for the native target, Cabal 3.18.2, Node.js 22.5+, npm, Git, a POSIX shell,
GNU Make, stable Rust and MSVC C++ tools (or LLVM-MinGW with
`-BlindlyToolchain gnullvm`). Put GHC's `bin` and `mingw/bin`, the compiler's
`objdump`, and an installed Inno Setup 6+ `ISCC.exe` on PATH. Public builds also
need Windows SDK `signtool.exe`. Keep shell/Make tool paths free of spaces.

```powershell
git submodule update --init --recursive
.\environments\windows\build.ps1 -Runtime win-x64 -Version 0.1.0 -OutputDirectory .build\release-x64
Get-FileHash .build\release-x64\Sapiens-Setup-win-x64.exe -Algorithm SHA256
```

The output directory must be new. Only the installer and `SHA256SUMS` are
published. All packaged PE files must match the selected GHC target and resolve
imports to Windows inbox libraries or adjacent packaged DLLs. The WebView2 loader
is hash-pinned. The ordinary `runghc build/Build.hs` build also prepares this
app-local loader; packaging consumes that result.

`-Runtime win-x64` and `-Runtime win-arm64` select native payload variants. The
script rejects a mismatch with the active GHC target, passes the target to
Blindly and the loader, audits every PE machine type, and configures Inno's
architecture restrictions. Output names include `win-x64` or `win-arm64`; the
default output directory is `.build/<runtime>`. Application identity/data remain
common to both variants. Build variants serially in a checkout.

The ARM64 packaging path is configured but **not qualified**: GHC 9.14.1's
[official bindists](https://downloads.haskell.org/~ghc/9.14.1/) include Windows
x64, but no Windows ARM64 compiler. A matching native ARM64 toolchain and device
are required before claiming a working ARM64 installer. Inno retains its existing
x86 setup engine, which [runs on ARM64 through Windows emulation](https://jrsoftware.org/ishelp/topic_setup_setuparchitecture.htm); the application payload must be native ARM64.
Qualification requires a native `aarch64-*-mingw*` GHC 9.14.1 toolchain with
matching C compiler, linker and resource compiler, ARM64-capable `objdump`, all
Haskell dependencies and libsodium built for that target, a matching Rust Windows
ARM64 target, and WebView2 on a Windows ARM64 device. Build with
`-Runtime win-arm64`, retain the PE audit results, and verify installation,
startup, navigation recovery, Quit, reinstall and uninstall with retained data on
that device. An x64 build running under emulation does not qualify this target.

For a public release, set `SAPIENS_WINDOWS_CERT_THUMBPRINT` to a certificate in
the signing account's store and add `-PublicRelease`. A timestamp URL may be
supplied with `-TimestampUrl`. Builds sign and verify application binaries,
the installer and its embedded uninstaller. Local builds are unsigned.
Installer acceptance tests are deferred until the implementation is complete.

For source CLI development use `.\build\start.ps1 -Open`. For the native host,
set `SAPIENS_RUNTIME_DIR` to the checkout. `--data-dir`, `SAPIENS_DATA_DIR` and
`SAPIENS_CONFIG_DIR` retain explicit path control. Use an unlocked interactive
desktop with matching privilege levels for Blindly4 tests; run desktop tests serially.
