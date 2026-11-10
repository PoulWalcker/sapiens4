# Sapiens4 for macOS

Requires macOS arm64 13.0+. Open `Sapiens.dmg`, drag the app to Applications and
launch it. Closing the window keeps the app running; **Quit** stops its backend.
`desktop-server.log` is in the [data directory](../../docs/reference.md#data-and-updates).
No login item or updater is installed.

## Build

Install the [pinned toolchain](../../docs/release-development.md) and Xcode Command
Line Tools (`xcode-select --install`). For clean Dev/Prod builds, signing and
publication, follow that runbook. For a direct development build:

```sh
git submodule sync --recursive
git submodule update --init --recursive
./environments/macos/build.sh --version 0.1.0-dev --output-directory .build/macos
open .build/macos/Sapiens.dmg
```

The output contains `Sapiens4 Development.app`, DMG, versioned ZIP and receipt.
Use a fresh output directory for each build. See the runbook for build-only,
skip-build and provider options; `runghc build/Build.hs` alone omits Blindly.

Application compilation fixes the deployment target at 13.0 with explicit C/linker
flags and the Cabal cache `submodules/libs4/haskell/.build/macos-13.0`. If the target
changes, change the cache directory too; environment changes alone do not invalidate
old Cabal binaries. Packaging audits the resulting binaries before signing.

The DMG build verifies the image, mounts it read-only, copies the app into a
private temporary installation and verifies its signature and runtime inventory.
See [package checks](../../docs/testing.md#package-checks) for further validation.
