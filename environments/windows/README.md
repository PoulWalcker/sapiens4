# Windows

Run **Sapiens-Setup-win-x64.exe** on Windows 10/11 x64.
Install WebView2 if prompted. Choose an empty folder or a complete Data Root.
Install a model CLI and sign in, for example `codex login`.
Data/config: `%LOCALAPPDATA%\Sapiens4\{data,config}`.

## Build

Use the [source prerequisites](../../README.md), MSVC/Windows SDK, a POSIX shell,
GNU Make, objdump and Inno Setup 6+. Choose a new output directory:

```powershell
git submodule update --init --recursive
.\environments\windows\build.ps1 -Runtime win-x64 -Version 0.1.0 -OutputDirectory .build\release-x64
```

Automatic updates verify the SHA-256 digest published by GitHub for the release
asset. To sign a release, install signtool, set `SAPIENS_WINDOWS_CERT_THUMBPRINT`,
and add `-PublicRelease`.
`win-arm64` requires a matching toolchain; device qualification is pending.
Regenerate changed icons with `.\environments\windows\regenerate-icons.ps1`.
[Remote control](../../docs/remote.md) · [Tests](../../docs/testing.md)
