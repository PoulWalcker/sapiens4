# Sapiens4 for macOS

The app bundles the native shell, owned backend, UI, Blindly, complete pinned
Codex provider and licenses. End users need macOS arm64 13.0+.

## Build

Use GHC 9.14.1, Cabal 3.18.2.0, Node 22.23.2, Rust 1.99.0,
Git and Xcode Command Line Tools. The builder and packager are Haskell; they use
the existing Cabal dependencies and platform tools. Checks use Ruby 3.4.11 and Bundler 4.0.21. For the combined Windows/local release flow, run from a clean checkout:

```sh
./environments/macos/build.sh --local dev "$(git rev-parse HEAD)" 0.1.0-dev.local1
```

This checks prerequisites and initializes pinned submodules while preserving all
compilation/download caches. Install missing tools first (GHC/Cabal via ghcup,
Node22.23.2 and Rust1.99.0 via your tool manager; `xcode-select --install` for
Xcode CLI tools). Nothing is installed globally by the command. See the
[combined release runbook](../../docs/release-development.md) for exact commit/version
matching, Prod signing and manual publication. `--local prod` builds signed/notarized
`Sapiens4.app`; Dev keeps `Sapiens4 Development.app`. macOS is not built on Actions.

For direct low-level builds, initialize the pinned submodules and run:

```sh
git submodule sync --recursive
git submodule update --init --recursive
./environments/macos/build.sh --version 0.1.0-dev --output-directory .build/macos
open .build/macos/Sapiens.dmg
```

Open `Sapiens.dmg`, drag **Sapiens4 Development** to Applications, then launch it.
Outputs in `.build/macos/` are the DMG, app, `sapiens4-development-0.1.0-dev-macos-arm64.zip`
and its `.release.json` receipt. Use `--output-directory` for another build;
existing app/DMG/ZIP outputs are rejected. The integrated build acquires or
verifies the pinned provider in `.build/codex`; use `--provider` for an already
verified payload elsewhere. Signing defaults to ad-hoc. Publisher
signing and CI policy are in the [release runbook](../../docs/release-development.md).
`--build-only` compiles the backend/UI/native shell and Blindly without packaging
or acquiring Codex. `--skip-build` packages existing outputs with caller-verified
provenance. `runghc build/Build.hs` alone does not build Blindly.
`--allow-missing-provider` supports native validation only.

Both source and package builds fix `MACOSX_DEPLOYMENT_TARGET=13.0` and pass explicit
minimum-version flags to GHC's C compiler and linker. Their Cabal build directory
is `submodules/libs4/haskell/.build/macos-13.0`; the old general-purpose cache is
not reused or deleted. Build and `list-bin` use the same directory. Changing the
declared target must also change this cache directory.

The macOS audit rejects required binaries above 13.0. Only the exact locked
Codex 0.167.0 optional voice subtree may require 14.0; its inventory fingerprint
and dependency boundary are checked. All provider files remain bundled and signed.
Actual macOS 13 launch/login/exec acceptance remains open.

## Run and verify

Stable uses `~/Library/Application Support/Sapiens4/{data,config}`.
Dev data and configuration default to `~/Library/Application Support/Sapiens4 Development/data`
and the adjacent `config` directory. Use `SAPIENS_DATA_DIR` and `SAPIENS_CONFIG_DIR`
for synthetic validation. The app starts its own host on a dynamic loopback port;
`desktop-server.log` is in the data directory. Closing the window leaves the app
running; Quit stops its host. No login item is installed.

Backend tools use system directories first, then `/opt/homebrew/bin` and
`/usr/local/bin`, then absolute entries from the inherited `PATH`. Login shell
startup scripts are not run; Finder may not inherit pyenv/conda/nvm paths.
Bundled Codex remains the default; `SAPIENS_CODEX_BINARY` explicitly overrides it.

```sh
ruby submodules/libs4/ruby/bundle.rb install
codesign --verify --deep --strict '.build/macos/Sapiens4 Development.app'
SAPIENS_TEST_PACKAGE_RUNTIME="$PWD/.build/macos/Sapiens4 Development.app/Contents/Resources/runtime" \
  ruby submodules/libs4/ruby/bundle.rb exec rspec spec/native_package_spec.rb
```

For Stable checks, set `PACKAGE_CHANNEL=stable` and use `Sapiens4.app` in those paths.

DMG creation verifies the image, mounts it read-only, copies the app to a temporary
installation path and verifies its signature/runtime inventory.
The package scenario checks final inventories, provider version, HTTP assets,
stable synthetic identity across two launches and graceful exit. Native UI,
permissions, target-OS and downloaded/notarized artifact acceptance are separate checks.
