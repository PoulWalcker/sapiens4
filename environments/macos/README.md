# Sapiens4 Development for macOS

The app bundles the native shell, owned backend, UI, Blindly, complete pinned
Codex provider and licenses. End users need macOS arm64 13.0+.

## Build

Use GHC 9.14.1, Cabal 3.18.2.0, Node 22.23.2, Rust 1.99.0, Python 3.10+,
Git and Xcode Command Line Tools. Checks use Ruby 3.4.11 and Bundler 4.0.21. Initialize the pinned
submodules, then run from the repository root:

```sh
git config submodule.blindly4.url https://github.com/aleski-green/blindly4.git
git submodule update --init --recursive
python3 build/package/acquire-provider.py --platform darwin-arm64 --destination runtime/codex
./environments/macos/build.sh --version 0.1.0-dev --provider "$PWD/runtime/codex"
open .build/macos/Sapiens.dmg
```

Open `Sapiens.dmg`, drag **Sapiens4 Development** to Applications, then launch it.
Outputs in `.build/macos/` are the DMG, app, `sapiens4-development-0.1.0-dev-macos-arm64.zip`
and its `.release.json` receipt. Use `--output-directory` for another build;
existing app/ZIP outputs are rejected. Signing defaults to ad-hoc. Publisher
signing and CI policy are in the [release runbook](../../docs/release-development.md).
`--skip-build` reuses existing binaries with caller-verified provenance.
`--allow-missing-provider` supports native validation only.

The macOS audit rejects required binaries above 13.0. Only the exact locked
Codex 0.167.0 optional voice subtree may require 14.0; its inventory fingerprint
and dependency boundary are checked. All provider files remain bundled and signed.
Actual macOS 13 launch/login/exec acceptance remains open.

## Run and verify

Data and configuration default to `~/Library/Application Support/Sapiens4 Development/data`
and the adjacent `config` directory. Use `SAPIENS_DATA_DIR` and `SAPIENS_CONFIG_DIR`
for synthetic validation. The app starts its own host on a dynamic loopback port;
`desktop-server.log` is in the data directory. Closing the window leaves the app
running; Quit stops its host. No login item is installed.

```sh
ruby submodules/libs4/ruby/bundle.rb install
codesign --verify --deep --strict '.build/macos/Sapiens4 Development.app'
SAPIENS_TEST_PACKAGE_RUNTIME="$PWD/.build/macos/Sapiens4 Development.app/Contents/Resources/runtime" \
  ruby submodules/libs4/ruby/bundle.rb exec rspec spec/native_package_spec.rb
```

DMG creation verifies the image, mounts it read-only, copies the app to a temporary
installation path and verifies its signature/runtime inventory.
The package scenario checks final inventories, provider version, HTTP assets,
stable synthetic identity across two launches and graceful exit. Native UI,
permissions, target-OS and downloaded/notarized artifact acceptance are separate checks.
