# Sapiens4 for Windows

Run **Sapiens-Setup.exe** on Windows 10/11 x64. Setup installs per user without
administrator access and creates a Start menu shortcut. No Sapiens build tools
are needed. The app uses the system WebView2 Runtime; native Install/Repair,
Retry and Cancel controls handle a missing Runtime before starting the backend.
No browser Runtime or Codex CLI is bundled. Model execution uses your installed
Codex CLI and existing authentication. Native npm binaries are supported;
`SAPIENS_CODEX_BINARY` selects an executable explicitly.

Data and configuration live in `%LOCALAPPDATA%\Sapiens4\{data,config}`, outside the
installation. Quit stops the owned backend. Quit before replacement or uninstall;
Setup refuses active installation processes, unowned nonempty destinations and
protected state directories, and destinations with junction/symlink parents. Uninstall preserves data, configuration and unowned
files. Process checks are preflight safeguards, not an atomic installation lock.
The ownership marker left after uninstall allows safe reinstallation.

## Build and verify

Install x64 GHC 9.14.1, Cabal 3.18.2, Node.js 22.5+, npm, Git, a POSIX shell,
GNU Make, stable Rust and MSVC C++ tools (or LLVM-MinGW with
`-BlindlyToolchain gnullvm`). Put GHC's `bin` and `mingw/bin`, the compiler's
`objdump`, and an installed Inno Setup 6+ `ISCC.exe` on PATH. Public builds also
need Windows SDK `signtool.exe`. Keep shell/Make tool paths free of spaces.

```powershell
git submodule update --init --recursive
.\environments\windows\build.ps1 -Version 0.1.0 -OutputDirectory .build\release-local
Get-FileHash .build\release-local\Sapiens-Setup.exe -Algorithm SHA256
```

The output directory must be new. Only the installer and `SHA256SUMS` are
published. All packaged PE files must be x64 and resolve imports to Windows
inbox libraries or adjacent packaged DLLs. The WebView2 loader is hash-pinned.

For a public release, set `SAPIENS_WINDOWS_CERT_THUMBPRINT` to a certificate in
the signing account's store and add `-PublicRelease`. A timestamp URL may be
supplied with `-TimestampUrl`. Builds sign and verify application binaries,
the installer and its embedded uninstaller. Local builds are unsigned.
Installer acceptance tests are deferred until the implementation is complete.

For source CLI development use `.\build\start.ps1 -Open`. For the native host,
set `SAPIENS_RUNTIME_DIR` to the checkout. `--data-dir`, `SAPIENS_DATA_DIR` and
`SAPIENS_CONFIG_DIR` retain explicit path control. Use an unlocked interactive
desktop with matching privilege levels for Blindly4 tests; run desktop tests serially.
