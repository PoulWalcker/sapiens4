# Development packaging and releases

Packages use `PoulWalcker/sapiens4`, channel `development`, and app ID
`com.poulwalcker.sapiens4.development`. Production update discovery is disabled.
Targets are macOS arm64 (13.0+) and Windows x64, including Windows 11 ARM x64
emulation. See the platform instructions for requirements, build commands and checks:

- [macOS](../environments/macos/README.md)
- [Windows](../environments/windows/README.md)

## CI policy

| Event | Work | Publication |
|---|---|---|
| Feature branch push | No native workflow | None |
| Pull request to main | Compile host, UI, native shell and Blindly once per platform | None |
| `vSemVer` tag in main history | Compile once, package, sign, verify both targets | GitHub Release |
| Manual `workflow_dispatch`, selected branch | Compile once, package and verify test artifacts | Workflow artifacts only |

`native-build.yml` uses GHC 9.14.1, Cabal 3.18.2.0, Node 22.23.2, Rust 1.99.0
and Ruby 3.4.11. PR jobs stop before provider acquisition and packaging. Tag and
manual jobs reuse that job's compilation through `--skip-build`/`-SkipBuild`.
`release-development.yml` validates SemVer and main ancestry, then requires both
signed receipts to match the tag, clean source commit, supported targets and
final asset hashes. Its publish job uploads to a draft and publishes the complete
release after successful upload; only that job has contents-write permission.

## Payload integrity

Acquire the complete pinned provider with `build/package/acquire-provider.py`.
Assembly rejects missing, changed or extra provider files and symlinks. It signs
nested code before writing final provider/runtime inventories. The acquired
inventory is retained as `sapiens-provider-upstream.json`.

```sh
python3 build/package/package.py verify --runtime /absolute/path/to/runtime
```

Primary downloads are `Sapiens.dmg` and `Sapiens-Setup.exe`; portable ZIPs remain
available. Each platform has one `.release.json` binding both final files, their
sizes, SHA256 values and
package-manifest hash. `buildId` records source, dependency, provider and toolchain
inputs; it omits caller-supplied compiler flags and prebuilt binary hashes when
using `--skip-build`. The final artifact digests identify the actual shipped bytes.
Source exports need `build/source-provenance.json`, generated in the checkout:

```sh
python3 build/package/package.py provenance --output build/source-provenance.json
```

## Publisher signing

PR and manual builds need no signing credentials. Manual Mac builds use ad-hoc
signing; manual Windows builds are unsigned. For a locally configured signing
identity, add these arguments to the platform build command:

- macOS: `--sign-identity 'Developer ID Application: ORGANIZATION (TEAMID)' --notary-profile PROFILE`
- Windows: `-CertificateThumbprint CERTIFICATE_THUMBPRINT` with Windows SDK SignTool on PATH.

Mac signing uses hardened runtime, timestamps, strict deep verification,
notarization, stapling and Gatekeeper assessment. Windows signs and verifies
all application EXE/DLL files before final inventories. Inno signs its generated
Setup, uninstaller and temporary copies through the configured SignTool; final
installer hashes are calculated after composition/signing.

Tag workflows require these repository secrets:

- `MAC_CERTIFICATE_P12` and `MAC_NOTARY_KEY_P8`: base64-encoded certificate and key.
- `MAC_CERTIFICATE_PASSWORD` (may be empty), `MAC_SIGN_IDENTITY`,
  `MAC_NOTARY_KEY_ID`, `MAC_NOTARY_ISSUER`.
- `WINDOWS_CERTIFICATE_PFX` (base64) and `WINDOWS_CERTIFICATE_PASSWORD`.

Missing credentials fail before compilation. Release runners remove temporary
certificate/keychain material after the job. Keep signing material out of logs.

## Outstanding delivery checks

Windows Setup uses pinned Inno Setup for per-user installation only. Velopack,
update feeds, migration and recovery are separate lifecycle work. WebView2
Evergreen remains a prerequisite with native Install/Retry/Cancel guidance.
CI compiler preflight uses a tiny fixture before the expensive Windows build;
the full real installer is then tested with synthetic state and no auth.

Publisher signing/notarization, clean-machine/normal-user Windows, Windows 11 ARM,
macOS 13 and downloaded signed-artifact acceptance require recorded results.
Local ad-hoc DMG success is not evidence of these checks. Use synthetic data.
