# Sapiens4 Windows source/build verification handoff

This kit contains frozen source, exact local submodule exports, the complete
pinned Windows Codex payload, and existing checks. **It does not contain a
prebuilt Sapiens4 Windows application or Setup.exe. Windows execution is NOT_RUN
until you run it on Windows.** Input and source identities are in
`handoff-manifest.json`; the ZIP digest is in the adjacent external receipt.

Use a normal-user x64 Windows test context with an unlocked desktop for desktop
checks. On Windows 11 ARM, use x64 PowerShell/toolchains under x64 emulation;
native ARM64 is not this target. Keep build-tool paths free of spaces. The driver
creates a new GUID-named directory under TEMP (or an existing writable
`-WorkBase`). It never replaces a personal app, imports auth, changes a global
toolchain, signs, publishes, or automatically installs prerequisites.

Preinstalled prerequisites: x64 GHC 9.14.1, Cabal 3.18.2.0, Node 22.23.2/npm,
Ruby 3.4.11 with Bundler 4.0.21, Python 3.10+, Rust 1.99.0/cargo with the
`x86_64-pc-windows-msvc` target and MSVC build tools, Git, sh, make and windres.
Put GHC's bin/mingw-bin tools on PATH. Build dependencies still require anonymous
public Hackage/npm/RubyGems/crates downloads; provider bytes are included offline.
The existing research probe additionally downloads its pinned isolated .NET/vpk
fixtures and needs the built-in x64 .NET Framework compiler. It installs no SDK
or application. A missing prerequisite is a recorded failure, never auto-fixed.

Extract the whole ZIP. In x64 PowerShell, from the extracted folder:

```powershell
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\Run-Windows-Verification.ps1 -RunDesktopChecks
```

`-SkipResearchProbe` leaves the existing lifecycle probe NOT_RUN for an offline
or focused build. `-VerifyInputsOnly` checks kit integrity on Windows or Mac
without compiling or executing native checks. Neither option changes a missing
acceptance into a pass. No login or real model request is performed by the driver.
Its tests use synthetic config/data and controlled model helpers; D3's existing
corrected acceptance script verifies the finalized 49-entry provider inventory.

The driver copies source to its owned work root, isolates build caches and app
state, checks tool versions, reuses the existing lifecycle probe, builds one
unsigned portable package, runs D3's API check and the existing onboarding,
persistence and package RSpec scenarios. Optional desktop scenarios run serially.
Review the printed RESULT ZIP and return that ZIP, including failures. It contains
selected tool/OS metadata, sanitized logs, per-step statuses, package receipt and
manifest—not source, binaries, personal configuration or auth directories.
The owned build/work directory is retained; its path is printed separately.

Manual missing-WebView2 Install/Retry/Cancel, native stepped UI, authorized real
sign-in/first task, installer/reinstall/uninstall, Windows 11 ARM emulation and
genuine downloaded signed-artifact acceptance remain explicitly separate where
not performed. Setup.exe/bootstrap implementation is still pending; this kit
builds the existing portable application. Signing and remote CI configuration
review are included as readiness notes, not as evidence of execution.
