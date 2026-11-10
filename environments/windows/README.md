# Sapiens4 for Windows

Requires Windows 10/11 x64, or Windows 11 ARM with x64 emulation.
Run **Sapiens-Setup.exe** to install for the current user without administrator
rights. Launch **Sapiens4** or **Sapiens4 Development** from Start. Files are in
`%LOCALAPPDATA%\Programs\APP_NAME`; uninstall through Windows Settings.
Alternatively, extract the complete portable ZIP and run **Sapiens4.exe**.
No Python, .NET, WSL or build tools are required to run the app.

Microsoft Edge WebView2 Evergreen Runtime is required. If missing, choose
**Install WebView2**, run the official installer opened by the app, then **Retry**.
**Cancel** exits without starting the backend. The download needs Internet.
The app stops its owned backend when closed. See [data/config paths and tool
configuration](../../docs/reference.md#configuration).

## Build prerequisites

Use the [pinned toolchain and build commands](../../docs/release-development.md).
Install x64 GHC and Node even on Windows ARM, MSVC C++ build tools, Git, a POSIX
shell, GNU Make, curl and tar. Run in an MSVC developer shell. Keep GHC/shell/Make
paths free of spaces and use shell/Make from the same POSIX distribution. Exclude
Ruby's MSYS2/pkg-config paths from native compilation to avoid an external zlib DLL.

The build downloads and verifies pinned Inno Setup into `.build/tools/` and
bundles the checksum-pinned WebView2 loader. `-BlindlyToolchain gnullvm` selects
LLVM-MinGW instead of MSVC for Blindly. Build-only/skip-build/provider/signing
options are in the runbook.

## Troubleshooting and verification

In Windows PowerShell 5, capture logs through `cmd.exe`; `*>` can turn native
stderr warnings into terminating errors under the build's `Stop` policy:

```powershell
New-Item -ItemType Directory -Force .build | Out-Null
cmd.exe /d /c "powershell.exe -NoProfile -File environments\windows\build.ps1 -OutputDirectory .build/windows-logged > .build/windows-build.log 2>&1"
if ($LASTEXITCODE) { throw 'Windows build failed; see .build/windows-build.log' }
```

[Package/installer checks](../../docs/testing.md#package-checks) use synthetic state
and a disposable Windows account. Unsigned builds may show Windows trust warnings.

Quit the app and backend before reinstall/uninstall: Setup refuses active owned
executables and failed WMI process checks. This is a process snapshot; a subsequent
racing launch is not locked out. An unowned nonempty directory or either channel's
data root cannot be an installation target.
Uninstall removes recorded program files, shortcuts and registration; unknown
files and the ownership marker remain. Setup does not migrate/delete external
state, manage auth, stop apps or restart processes. No updater is installed.
