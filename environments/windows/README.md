# Sapiens4 for Windows

Run **Sapiens-Setup-win-x64.exe** on Windows 10/11 x64. Install WebView2 if prompted.
For model execution, install Codex CLI and run `codex login`.

Data and config default to `%LOCALAPPDATA%\Sapiens4\data` and its sibling `config`.
Choose **Use another folder** to open existing data. Quit before upgrading or uninstalling.

## Build

Use GHC 9.14.1, Cabal 3.18.2, Node.js 22.5+, npm, Git, a POSIX shell, GNU Make,
Rust with MSVC, objdump and Inno Setup 6+. Choose a new output directory.

```powershell
git submodule update --init --recursive
.\environments\windows\build.ps1 -Runtime win-x64 -Version 0.1.0 -OutputDirectory .build\release-x64
```

For signed releases, install Windows SDK signtool, set `SAPIENS_WINDOWS_CERT_THUMBPRINT`
and add `-PublicRelease`. Native ARM64 packaging requires a matching toolchain and still awaits device qualification.
