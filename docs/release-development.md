# Manual distribution

Build both platforms from the same clean commit and SemVer. Use GHC 9.14.1,
Cabal 3.18.2.0, Node 22.23.2 and Rust 1.99.0, plus the platform prerequisites:
[macOS](../environments/macos/README.md), [Windows](../environments/windows/README.md).
There are no GitHub Actions builds or automatic releases.

## Build

Use fresh output directories. Builds preserve caches and refuse to overwrite
finished outputs. A failed publication rolls back only that attempt's files;
publication of the whole set is not crash-atomic.

Dev selects `development` / `Sapiens4 Development`; Prod selects `stable` /
`Sapiens4`. [Data directories and overrides](reference.md#data-and-updates) stay
separate between channels. Production source must belong to current
`aleski-green/sapiens4:main` history. Before Prod builds, refresh its local ref:

```sh
git fetch https://github.com/aleski-green/sapiens4.git +refs/heads/main:refs/remotes/release/main
```

Signed Stable builds and Prod receipt verification reject missing/stale refs,
unavailable remotes and commits outside main history. Dev and unsigned Stable
acceptance builds do not require main ancestry.

On macOS, from the chosen checkout:

```sh
COMMIT=$(git rev-parse HEAD)
VERSION=0.1.0-dev.local1
./environments/macos/build.sh --local dev "$COMMIT" "$VERSION" ".build/release/$VERSION/mac"
```

`--local` checks the clean commit and pinned tools, initializes submodules and
prepares an isolated HTTPS Cabal configuration. Missing tools must be installed
first; the command does not install global tools. For Prod, provision credentials
in the local keychain and replace the last command with:

```sh
export SAPIENS_MAC_SIGN_IDENTITY='Developer ID Application: ORGANIZATION (TEAMID)'
export SAPIENS_MAC_NOTARY_PROFILE=sapiens-release
./environments/macos/build.sh --local prod "$COMMIT" "$VERSION" ".build/release/$VERSION/mac"
```

Prod signs with hardened runtime and timestamps, notarizes and staples the app
and DMG, and verifies signatures and Gatekeeper acceptance. Dev uses ad-hoc signing.

On Windows, use the same full commit and version. After the platform setup:

```powershell
$Commit = 'FULL_COMMIT_SHA'
$Version = '0.1.0-dev.local1'
if ((git rev-parse HEAD) -ne $Commit) { throw 'Checkout commit differs' }
git submodule sync --recursive
if ($LASTEXITCODE) { throw 'Submodule sync failed' }
git submodule update --init --recursive
if ($LASTEXITCODE) { throw 'Submodule initialization failed' }
$cabalConfig = 'submodules/libs4/haskell/.cabal/config'
if (-not (Test-Path $cabalConfig)) {
    New-Item -ItemType Directory -Force (Split-Path $cabalConfig) | Out-Null
    "repository hackage.haskell.org`n  url: https://hackage.haskell.org/`n  secure: True`n" | Set-Content $cabalConfig
}
if (git status --porcelain --untracked-files=normal) { throw 'Clean checkout required' }
.\environments\windows\build.ps1 -Version $Version -OutputDirectory ".build/release/$Version/windows"
```

Dev Windows packages are unsigned. For Prod, add `-Channel stable
-CertificateThumbprint CERTIFICATE_THUMBPRINT`, with the publisher certificate in
the build user's certificate store and Windows SDK SignTool on PATH. The build
signs/verifies application EXE/DLL files; Inno signs Setup and its uninstaller.

For development and isolated validation, the direct platform builds also support:

| Purpose | macOS `build.sh` | Windows `build.ps1` |
| --- | --- | --- |
| Compile app/UI/Blindly without packaging/provider acquisition | `--build-only` | `-BuildOnly` |
| Package existing binaries; caller verifies their provenance | `--skip-build` | `-SkipBuild` |
| Use a prepared pinned Codex payload | `--provider DIR` | `-ProviderDirectory DIR` |
| Omit a missing provider for validation, never release | `--allow-missing-provider` | `-AllowMissingProvider` |

## Verify and publish

Each platform produces its application directory, installer (`Sapiens.dmg` or
`Sapiens-Setup.exe`), versioned ZIP and `.release.json` receipt. Follow
[package and installer checks](testing.md#package-checks), then manually verify
GUI startup, backend health, quit/close, absence of orphan processes/ports and
reopening the same synthetic state. Never use real user data for acceptance.

Copy Windows Setup, ZIP and receipt to `.build/release/$VERSION/windows` beside
`mac`. The receipt checker requires both platforms, the exact clean source
commit/version, complete providers, final sizes/hashes and appropriate channel
and signing. Native build hosts establish signature trust; use only their trusted
receipts. `buildId` fingerprints source/dependency/provider/toolchain inputs;
prebuilt compiler flags and binary bytes are identified by final artifact hashes.

Run verification and draft creation together so failure cannot reuse an old list:

```sh
# Prod: MODE=prod; TAG="v$VERSION". Dev:
MODE=dev
TAG="dev-$VERSION"
bash -eu -c '
  mkdir -p .build/verification
  attempt=$(mktemp -d ".build/verification/$1-$2.XXXXXXXX")
  verified="$attempt/verified-assets.txt"
  runghc -ibuild/package build/Package.hs release --mode "$3" \
    --directory ".build/release/$1" --source-commit "$2" --version "$1" --output "$verified"
  assets=()
  while IFS= read -r asset; do assets+=("$asset"); done < "$verified"
  gh release create "$4" "${assets[@]}" --repo aleski-green/sapiens4 \
    --target "$2" --draft --title "Sapiens4 $1"
' _ "$VERSION" "$COMMIT" "$MODE" "$TAG"
```

Review all six assets in the draft, then publish manually. Do not run the draft
step separately using an earlier list. Existing verification lists are never replaced.

## Provider and integrity

Platform builds acquire or verify `.build/codex` using
`build/codex-provider-lock.json`. Explicit payloads must match the complete
hash-pinned archive, notices, layout, inventory and executable permissions.
Acquisition rejects archive links/devices/unsafe paths. To prepare a provider
separately after compiling the application dependencies:

```sh
# Windows: use --platform windows-x64.
runghc -ibuild/package build/Package.hs provider --platform darwin-arm64 --destination .build/codex
```

This command verifies an existing destination without replacing it. Signing keeps
acquired provider metadata as provenance. One final `package-inventory.json`
covers every runtime file after signing, including provider bytes and metadata:

```sh
runghc -ibuild/package build/Package.hs verify --runtime /absolute/path/to/runtime
```

Source exports without Git need provenance generated from their original checkout:

```sh
runghc -ibuild/package build/Package.hs provenance --output build/source-provenance.json
```

The macOS audit enforces arm64, internal dylib resolution and the 13.0 floor.
Only the exact fingerprinted Codex optional voice subtree may require 14.0;
required binaries cannot depend on it. All provider files remain bundled/signed.
Publisher signing, downloaded-artifact trust, actual macOS 13, Windows 11 ARM,
normal-user clean-machine and missing-WebView2 acceptance still require native
results. Unsigned/ad-hoc evidence does not establish those results.
