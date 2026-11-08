# Sapiens4 Development for macOS

The development bundle contains the AppKit/WebKit shell, owned Haskell host,
generated UI, prompts, Blindly4, complete pinned Codex payload and licenses.
It derives its runtime from the installed app, including paths with spaces or
Unicode. End users do not need a source checkout or build tools.

The app target remains **macOS 13.0**. An audited exception covers only the exact
locked Codex 0.167.0 voice subtree, whose helper declares 14.0 and is not entered
by Sapiens's selected login/status/exec paths. The complete provider is retained,
inventoried and signed. Packaging fingerprints its full acquired inventory and
audits the dependency boundary; unknown providers and required binaries above
13.0 fail closed. The manifest records the optional component's higher floor.
Actual macOS 13 launch/login/exec acceptance remains unrun. An explicit
`--experimental-minimum-os` still labels unsupported local experiments.

## Build a development bundle

Builders need GHC 9.14.1, Cabal 3.18.2, Node.js 22.5+, Rust, Python 3.10+, Git
and Command Line Tools. Initialize the exact submodule pins. Acquire the complete
provider with D3's acquisition script after integration, then run:

```sh
./environments/macos/build.sh --version 0.1.0-dev --provider /absolute/path/to/verified/codex
open '.build/macos/Sapiens4 Development.app'
```

The script builds native/UI/Blindly outputs and assembles a new app, versioned ZIP
and external release receipt. Output must be new; use `--output-directory` for
another assembly. `--skip-build` reuses existing outputs and requires separately
verified build provenance. `--allow-missing-provider` is limited to native
validation and cannot produce a complete release. Default signing is ad-hoc.
See [the release runbook](../../docs/release-development.md) for publisher signing.

## State and process ownership

Data defaults to `~/Library/Application Support/Sapiens4 Development/data`;
configuration defaults to the adjacent `config` directory. Explicit launch-time
`SAPIENS_DATA_DIR` and `SAPIENS_CONFIG_DIR` overrides support synthetic validation.
Packaging never imports or stamps historical data and does not discover an
existing production installation. Do not point development validation at real data.

Each app starts its own host on a dynamic loopback port. Closing the window keeps
the app running; explicit Quit stops its owned host and confirms exit. The host
log is `desktop-server.log` in the data directory. No login item is installed.
Native tabs retain links, local files, bookmarks, navigation and zoom.

## Verification

```sh
codesign --verify --deep --strict '.build/macos/Sapiens4 Development.app'
SAPIENS_TEST_PACKAGE_RUNTIME="$PWD/.build/macos/Sapiens4 Development.app/Contents/Resources/runtime" \
  ruby submodules/libs4/ruby/bundle.rb exec rspec spec/native_package_spec.rb
```

The RSpec scenario verifies final inventories and the bundled provider version,
then launches the real backend twice with fresh synthetic state and a restricted
PATH. Full native UI, permissions, macOS 13 acceptance, D3 onboarding, genuine
signing/notarization and downloaded/quarantined launch are separate acceptance
steps. Existing manual relocation/UI evidence is retained in D1_PROGRESS.md.
