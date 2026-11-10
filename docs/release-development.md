# Development packaging and releases

`PoulWalcker/sapiens4`: Dev uses `development` / `Sapiens4 Development` /
`com.poulwalcker.sapiens4.development`; Prod uses `stable` / `Sapiens4` /
`com.poulwalcker.sapiens4`. Desktop and packaged CLI use separate Development
and Stable data/config roots selected by the runtime's package manifest.
The source CLI keeps `<repository>/.sapiens4`. Explicit data/config overrides
are independent; there is no migration or update discovery.
macOS `--local prod` selects Stable with publisher signing. On Windows, select
`-Channel stable` and supply `-CertificateThumbprint` for publisher signing.
Low-level `--channel stable` / `-Channel stable` allows unsigned acceptance builds,
which cannot pass Prod verification. Stable ZIPs: `sapiens4-VERSION-PLATFORM-ARCH.zip`.

Targets are macOS arm64 (13.0+) and Windows x64, including Windows 11 ARM x64
emulation. See the platform instructions for requirements, build commands and checks:

- [macOS](../environments/macos/README.md)
- [Windows](../environments/windows/README.md)

## Manual builds on both platforms

Build on macOS and Windows from the same clean commit and version. GitHub Actions
workflows are removed; pushes, pull requests and tags do not build or publish packages.
Keep the existing local caches and use a new output directory for each package set.
Production releases must come from main history and pass the receipt checker in Prod mode.
Before a production build, refresh the dedicated release-main reference:

```sh
git fetch https://github.com/PoulWalcker/sapiens4.git refs/heads/main:refs/remotes/release/main
```

The signed Stable build commands and Prod receipt verification compare this ref
with the release repository's current main and require the source commit to be
its ancestor. A missing/stale ref or an unavailable remote fails explicitly;
refresh the ref and retry. The checkout need not be named `main`. Development
builds and unsigned Stable acceptance builds do not require this ancestry.

Windows uses GHC9.14.1, Cabal3.18.2.0, Node22.23.2, Rust1.99.0 and Ruby3.4.11.
Keep Ruby's MSYS2/pkg-config paths out of the native compilation environment so
Cabal does not select an external zlib DLL absent from the package. Use a separate
shell for checks. `BUNDLE_FROZEN=true` keeps the committed lockfile unchanged during
dependency installation and scenarios.

### One local macOS command

From a clean checkout of the commit selected for both platforms:

```sh
COMMIT=$(git rev-parse HEAD)
VERSION=0.1.0-dev.local1
./environments/macos/build.sh --local dev "$COMMIT" "$VERSION" ".build/release/$VERSION/mac"
```

`--local` checks macOS ARM64, Xcode CLI tools and the pinned compilers, selects
installed GHC9.14.1 without changing global ghcup settings, initializes committed
submodules and creates an isolated HTTPS Cabal configuration if absent. Missing
tools fail with a prerequisite message; it does not install global software.
Cabal, UI, Rust, provider/download and packaging caches are preserved. It invokes
the existing compilation and DMG packaging exactly once, without `--skip-build`.
Output directories must be fresh. The full commit SHA must equal HEAD and the
checkout must be clean; receipts record that identity. Use the same VERSION and
full commit SHA for Windows before combining artifacts.

Dev forces ad-hoc Mac signing (required for ARM64 execution, no publisher identity)
and unsigned Windows packages. For Prod, configure the existing local keychain:

```sh
export SAPIENS_MAC_SIGN_IDENTITY='Developer ID Application: ORGANIZATION (TEAMID)'
export SAPIENS_MAC_NOTARY_PROFILE=sapiens-release
./environments/macos/build.sh --local prod "$COMMIT" "$VERSION" ".build/release/$VERSION/mac"
```

Prod requires both variables; the existing packager signs, notarizes and staples
both the app and DMG and runs signature/Gatekeeper checks. An identity alone is
insufficient for this command. Keychain credentials are provisioned separately.

### Build Windows, combine and publish manually

Prepare the Windows toolchain described in the [Windows guide](../environments/windows/README.md).
In PowerShell, use the full commit SHA selected above:

```powershell
$Commit = 'FULL_COMMIT_SHA'
$Version = '0.1.0-dev.local1'
if ((git rev-parse HEAD) -ne $Commit) { throw 'Checkout commit differs' }
git submodule sync --recursive
if ($LASTEXITCODE) { throw 'Submodule sync failed' }
git submodule update --init --recursive
if ($LASTEXITCODE) { throw 'Submodule initialization failed' }
if (git status --porcelain --untracked-files=normal) { throw 'Clean checkout required' }
.\environments\windows\build.ps1 -Version $Version -OutputDirectory ".build/release/$Version/windows"
```

For Prod, add `-Channel stable -CertificateThumbprint CERTIFICATE_THUMBPRINT`.
Run the existing package checks from the Windows guide and manually launch the GUI
with temporary data/config paths. Confirm backend health, close the GUI, check that
its backend and port are gone, then reopen the same data and verify identity.
Copy the resulting Setup, ZIP and receipt to `.build/release/$VERSION/windows`
on the Mac alongside its `mac` directory. Do not substitute artifacts from another build.

Validate the complete set with the existing receipt checker. It requires exactly
one receipt per supported platform, both installer and ZIP, matching clean source
commit/version and final file hashes. Prod additionally requires the Stable channel and publisher signing
and the supported macOS13 floor; Dev requires Development and permits Mac ad-hoc/unsigned and Windows unsigned.
Keep the default mode `prod` when validating production packages.
The `--output` path must be new. Validation never removes an existing file;
the verified list is published atomically only after all checks succeed. Each
attempt below gets a fresh directory named with the exact version and full
source commit. Validation and draft creation stay in one fail-fast shell: a
failed validation cannot fall through to an earlier list. Do not run the draft
step separately with an old list.

This block creates a draft with all six verified assets (two installers, two
portable ZIPs and two receipts). It runs on macOS Bash3.2 as well as newer Bash:

```sh
# Prod: MODE=prod; TAG="v$VERSION". Dev:
MODE=dev
TAG="dev-$VERSION"
bash -eu -c '
  mkdir -p .build/verification
  attempt=$(mktemp -d ".build/verification/$1-$2.XXXXXXXX")
  verified="$attempt/verified-assets.txt"
  runghc -ibuild/package build/Package.hs release --mode "$3" --repository PoulWalcker/sapiens4 \
    --directory ".build/release/$1" --source-commit "$2" --version "$1" --output "$verified"
  assets=()
  while IFS= read -r asset; do assets+=("$asset"); done < "$verified"
  gh release create "$4" "${assets[@]}" --repo PoulWalcker/sapiens4 \
    --target "$2" --draft --title "Sapiens4 $1"
' _ "$VERSION" "$COMMIT" "$MODE" "$TAG"
```

Review the complete draft in GitHub, then publish it manually. Previous lists
remain untouched for reference and are never used by a new attempt.
The receipt signing labels supplement, rather than replace, native signing checks
performed on the build hosts. Do not use arbitrary untrusted receipts.

## Payload integrity

macOS compilation orchestration and packaging are Haskell (`build/Package.hs`
and `build/package/`). The shared Haskell engine owns assembly, inventories,
provenance, provider acquisition and receipts for both platforms. Windows invokes
the Haskell launcher for staging, then reuses its compiled executable for finalization
and receipts. Release-set validation uses the same Haskell executable;
package/installer scenarios run through RSpec. D1 requires no Python.

The base-only launcher runs before `Build.hs` prepares the Cabal dependencies.
It then compiles the packaging entrypoint inside the pinned Cabal environment.
`cabal exec -- runghc` also works after dependency setup, but recompiles the
packaging modules on every invocation. The cached GHC executable avoids that
cost across stage, finalize, verify and receipt commands without rebuilding the app.

The integrated platform build acquires/verifies `.build/codex` automatically.
For an explicit prepared payload, use `--provider`/`-ProviderDirectory`.
Every supplied provider is compared with the complete payload reconstructed from
the hash-pinned archive and notices, including its layout and full file inventory.
To prepare one separately (repeat-safe):

```sh
runghc -ibuild/package build/Package.hs provider --platform darwin-arm64 --destination .build/codex --ensure
```

For Windows, use the same Haskell command with `--platform windows-x64`. Both
platforms use `curl` and `tar` from the builder PATH and the same pinned lock,
archive-member inspection and final inventory validation.
Assembly rejects missing, changed or extra provider files and symlinks. It signs
nested code before writing final provider/runtime inventories. The acquired
inventory is retained as `sapiens-provider-upstream.json`.

```sh
runghc -ibuild/package build/Package.hs verify --runtime /absolute/path/to/runtime
```

Primary downloads are `Sapiens.dmg` and `Sapiens-Setup.exe`; portable ZIPs remain
available. Each platform has one `.release.json` binding both final files, their
sizes, SHA256 values and package-manifest hash. `buildId` records source, dependency, provider and toolchain
inputs; it omits caller-supplied compiler flags and prebuilt binary hashes when
using `--skip-build`. The final artifact digests identify the actual shipped bytes.
Source exports need `build/source-provenance.json`, generated in the checkout:

```sh
runghc -ibuild/package build/Package.hs provenance --output build/source-provenance.json
```

## Publisher signing

Dev builds need no signing credentials. Dev Mac builds use ad-hoc
signing; Dev Windows builds are unsigned. For a locally configured signing
identity, add these arguments to the platform build command:

- macOS: `--sign-identity 'Developer ID Application: ORGANIZATION (TEAMID)' --notary-profile PROFILE`
- Windows: `-CertificateThumbprint CERTIFICATE_THUMBPRINT` with Windows SDK SignTool on PATH.

Mac signing uses hardened runtime, timestamps, strict deep verification,
notarization, stapling and Gatekeeper assessment. Windows signs and verifies
all application EXE/DLL files before final inventories. Inno signs its generated
Setup, uninstaller and temporary copies through the configured SignTool; final
installer hashes are calculated after composition/signing.

Windows publisher certificates are provisioned in the build user's local certificate
store; select the signing certificate by thumbprint and put Windows SDK SignTool on PATH.
Mac credentials stay in the local keychain. Builds do not read GitHub Actions secrets.

## Outstanding delivery checks

The application owns the provider release policy in `build/codex-provider-lock.json`.
Acquisition and validation read this same lock; its download hashes, notices and
upstream commit are pinned independently of libs4. Distribution builds use libs4
`c0cdad5e4c9328710387d910655a1178be1fa798` and do not require an Onboarding module
or a newer shared-library revision. Onboarding registration belongs to its separate PR.

Windows Setup uses pinned Inno Setup for per-user installation only. Velopack,
update feeds, migration and recovery are separate lifecycle work. WebView2
Evergreen remains a prerequisite with native Install/Retry/Cancel guidance.
Use the real installer with synthetic state and no auth for acceptance checks.

Publisher signing/notarization, clean-machine/normal-user Windows, Windows 11 ARM,
macOS 13 and downloaded signed-artifact acceptance require recorded results.
Unsigned/ad-hoc build evidence does not establish signing, target-OS or real-account
acceptance. Use synthetic data.
