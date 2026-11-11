# Sapiens4 for Windows

Extract the whole portable package and run **Sapiens4.exe**. The `win-x64` package
runs on Intel/AMD PCs and on Windows 11 ARM PCs with x64 app support. Requires
Microsoft Edge WebView2 Evergreen Runtime. The Haskell runtime and Blindly4 are
bundled; Python, .NET, WSL and administrator access are not required.

Chat requires authenticated Codex CLI 0.156.1+. Run `codex login`; installed CLI
and native npm binaries are discovered automatically. Set `SAPIENS_CODEX_BINARY`
to a full executable path to override discovery.

New data defaults to `%LOCALAPPDATA%\Sapiens4\data` for stable packages and
`%LOCALAPPDATA%\Sapiens4 Development\data` for development packages. A stable
upgrade discovers valid baseline data directly in `%LOCALAPPDATA%\Sapiens4`
when no storage selection is saved. Chosen folders are reused in place; empty
folders can initialize new data. Closing the
app stops its server and cancels active calls. The package is unsigned and has no
automatic updater: replace the application directory while retaining the data.
Licenses ship in `license/`, `runtime/submodules/blindly4/license/` and `runtime/submodules/libs4/license/`.

## Build from source

Install GHC 9.14.1, Cabal 3.18.2, Node.js 22.5+, npm, Git, a POSIX shell,
GNU Make, and stable Rust with MSVC C++ build tools or LLVM-MinGW. Put GHC's
`bin` and `mingw/bin` directories on PATH, along with the other build tools.
Install GHC, the shell and Make in paths without spaces (or use their Windows
short paths on PATH); their dependency build tools split paths containing spaces.
Use x64 GHC and Node on ARM PCs. Add the matching Rust target, such as
`x86_64-pc-windows-msvc` or `x86_64-pc-windows-gnullvm`.

```powershell
git clone --recurse-submodules https://github.com/aleski-green/sapiens4.git
cd sapiens4
.\build\start.ps1 -Open
.\environments\windows\build.ps1 -Runtime win-x64
.\.build\Sapiens4-win-x64\Sapiens4.exe
```

`start.ps1` builds Blindly4, the Haskell host and the UI; `-SkipBuild` reuses the
existing binaries. Packaging defaults to the GHC target and rejects a mismatched
`-Runtime`. Portable packages default to `-Channel stable`. Use `-Channel development`
for isolated development storage and a separate `Sapiens4-Development-$Runtime`
output directory. Both the launcher and embedded backend read the generated
`runtime/package-manifest.json`; keep it with the package.
Native `win-arm64` packaging requires an ARM64 Windows GHC toolchain.
Use `-BlindlyToolchain gnullvm` with LLVM-MinGW. The WebView2 loader is downloaded
with a pinned checksum and selected for the executable's architecture.

For desktop development, set `SAPIENS_RUNTIME_DIR` to the repository root. Other overrides are
`SAPIENS_DATA_DIR`, `SAPIENS_BLINDLY_BINARY`, `SAPIENS_CODEX_BINARY`,
`SAPIENS_CODEX_MODEL` and `SAPIENS_CODEX_REASONING_EFFORT`.
The Haskell build regenerates icons from the favicons using the libs4 Node tool.

## Tests and desktop access

Follow [Testing](../../docs/testing.md). Set `SAPIENS_TEST_WINDOWS_PACKAGE`
to the built package directory for packaged-runtime tests; run ARM64 and x64 on
matching machines. Tests use temporary data and controlled model executables.

Blindly4 requires an unlocked interactive desktop, accessible controls and matching
privilege levels. Elevated apps and secure desktops are unsupported. Exact-value
guards must remain enabled, and desktop workflows must run serially.
