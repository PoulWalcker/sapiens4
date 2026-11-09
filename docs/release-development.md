# Development packaging and releases

Dev packages use channel `development`, name `Sapiens4 Development`, and app ID
`com.poulwalcker.sapiens4.development`. Prod packages use channel `stable`, name
`Sapiens4`, and app ID `com.poulwalcker.sapiens4`. Both use `PoulWalcker/sapiens4`.
The Stable identity belongs to this packaging flow; it does not claim or migrate
older installations with a different bundle ID. Update discovery stays disabled.

Stable defaults to `~/Library/Application Support/Sapiens4/{data,config}` on Mac
and `%LOCALAPPDATA%\Sapiens4\{data,config}` on Windows. Dev keeps its existing
`Sapiens4 Development` directories. Explicit data/config overrides still work;
no profiles are moved or deleted. Both channels can be installed alongside each other.

`--local dev` selects Development and ad-hoc signing; `--local prod` selects
Stable and requires Developer ID plus notarization. Actions `mode=prod` likewise
selects Stable and Authenticode. Low-level packagers accept `--channel stable`
(Mac) / `-Channel stable` (Windows) for local unsigned/ad-hoc acceptance builds;
these cannot pass production release verification. Portable Stable ZIPs are named
`sapiens4-VERSION-PLATFORM-ARCH.zip`; Dev names are unchanged.

Targets are macOS arm64 (13.0+) and Windows x64, including Windows 11 ARM x64
emulation. See the platform instructions for requirements, build commands and checks:

- [macOS](../environments/macos/README.md)
- [Windows](../environments/windows/README.md)

## Local macOS + Windows Actions policy

| Event | Work | Publication |
|---|---|---|
| Feature push | No native workflow | None |
| PR to main | Windows compilation only | None |
| Manual native workflow, selected ref | Windows compile once, package/verify Dev or Prod | Workflow artifacts only |
| Version tag in main history | Signed Windows packages | Workflow artifacts only |
| macOS | Build locally using the same commit/version | Manual complete release only |

There are no macOS Actions jobs and no automatic release publication job.
Windows still uses GHC9.14.1, Cabal3.18.2.0, Node22.23.2, Rust1.99.0 and
Ruby3.4.11, existing caches, installer validation and `-SkipBuild` packaging.
Production Windows runs require main ancestry and signing secrets before compilation.
Ruby is installed after native compilation: its MSYS2/pkg-config paths must not
make Cabal select an external zlib DLL absent from the package. `BUNDLE_FROZEN=true`
keeps the committed lockfile unchanged during dependency installation and scenarios.

### One local macOS command

From a clean checkout of the exact Windows workflow commit:

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
checkout must be clean; receipts record that identity. Use the same VERSION in
Windows Actions and confirm the run's `headSha` before combining artifacts.

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

For Dev, after pushing the approved commit, choose that exact ref in **Native
verification and test packages**, enter the same version and `mode=dev`.
For Prod, use a commit in main history and create/push its `vVERSION` tag first;
that tag runs the signed Stable Windows build. Build Mac locally from the same
commit/version. Use that existing tag when creating the draft, so publication
will not cause a second Windows build. A manual `mode=prod` run is also available,
but do not build manually and then push a new version tag for the same release.
No run publishes a release.
After a successful run, download its Windows artifact (replace RUN_ID):

```sh
gh run view RUN_ID --repo PoulWalcker/sapiens4 --json headSha,conclusion
# Require conclusion=success and headSha=$COMMIT before downloading.
gh run download RUN_ID --repo PoulWalcker/sapiens4 \
  --name sapiens4-development-windows-x64 --dir ".build/release/$VERSION/windows"
```

For Prod, download artifact `sapiens4-stable-windows-x64` instead.

Validate the complete set with the existing receipt checker. It requires exactly
one receipt per supported platform, both installer and ZIP, matching clean source
commit/version and final file hashes. Prod additionally requires the Stable channel and publisher signing
and the supported macOS13 floor; Dev requires Development and permits Mac ad-hoc/unsigned and Windows unsigned.
Keep the default mode `prod` when validating production packages.

```sh
runghc -ibuild/package build/Package.hs release --mode dev --repository PoulWalcker/sapiens4 \
  --directory ".build/release/$VERSION" --source-commit "$COMMIT" \
  --version "$VERSION" --output .build/verified-assets.txt
```

Only after this command succeeds, create a draft with all six verified assets
(two installers, two portable ZIPs and two receipts). This shell block runs on
macOS Bash3.2 as well as newer Bash:

```sh
# Dev: TAG="dev-$VERSION" (does not start the production tag workflow).
# Prod: TAG="v$VERSION" must already exist from the initial Windows build.
# Use --mode prod above for Prod. Run only after verification succeeds.
bash -eu -c '
  assets=()
  while IFS= read -r asset; do assets+=("$asset"); done < .build/verified-assets.txt
  gh release create "$3" "${assets[@]}" --repo PoulWalcker/sapiens4 \
    --target "$2" --draft --title "Sapiens4 $1"
' _ "$VERSION" "$COMMIT" "$TAG"
```

Review the complete draft in GitHub, then publish it manually. No credentials or
workflow can automatically publish a Windows-only production release. A failed
verification stops the process and invalidates the previous verified-assets list.
The receipt signing labels supplement, rather than replace, native signing checks
performed on the build hosts. Do not use arbitrary untrusted receipts.

## Payload integrity

macOS compilation orchestration and packaging are Haskell (`build/Package.hs`
and `build/package/`). The shared Haskell engine owns assembly, inventories,
provenance, provider acquisition and receipts for both platforms. Windows invokes
the Haskell launcher for staging, then reuses its compiled executable for finalization
and receipts. Release-set validation uses the same Haskell executable; CI cache preparation uses
PowerShell and all package/installer scenarios run through RSpec. D1 requires no Python.

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

PR and Dev builds need no signing credentials. Dev Mac builds use ad-hoc
signing; Dev Windows builds are unsigned. Prod requires publisher credentials. For a locally configured signing
identity, add these arguments to the platform build command:

- macOS: `--sign-identity 'Developer ID Application: ORGANIZATION (TEAMID)' --notary-profile PROFILE`
- Windows: `-CertificateThumbprint CERTIFICATE_THUMBPRINT` with Windows SDK SignTool on PATH.

Mac signing uses hardened runtime, timestamps, strict deep verification,
notarization, stapling and Gatekeeper assessment. Windows signs and verifies
all application EXE/DLL files before final inventories. Inno signs its generated
Setup, uninstaller and temporary copies through the configured SignTool; final
installer hashes are calculated after composition/signing.

Windows production workflows require repository secrets `WINDOWS_CERTIFICATE_PFX`
(base64) and `WINDOWS_CERTIFICATE_PASSWORD`. Missing credentials fail before
compilation; temporary certificates are removed after the job. Mac credentials
stay in the local keychain and are no longer required as Actions secrets.

## Outstanding delivery checks

The application owns the provider release policy in `build/codex-provider-lock.json`.
Acquisition and validation read this same lock; its download hashes, notices and
upstream commit are pinned independently of libs4. Distribution builds use libs4
`731223b2f573166577be9d3f095a9d3d367092a2` and do not require an Onboarding module
or a newer shared-library revision. Onboarding registration belongs to its separate PR.

Windows Setup uses pinned Inno Setup for per-user installation only. Velopack,
update feeds, migration and recovery are separate lifecycle work. WebView2
Evergreen remains a prerequisite with native Install/Retry/Cancel guidance.
CI compiler preflight uses a tiny fixture before the expensive Windows build;
the full real installer is then tested with synthetic state and no auth.

Publisher signing/notarization, clean-machine/normal-user Windows, Windows 11 ARM,
macOS 13 and downloaded signed-artifact acceptance require recorded results.
Unsigned/ad-hoc CI evidence does not establish signing, target-OS or real-account
acceptance. Use synthetic data.
