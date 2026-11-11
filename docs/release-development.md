# Manual release

Build on [macOS](../environments/macos/README.md) and [Windows](../environments/windows/README.md) from the same clean commit, pinned submodules and SemVer. Use fresh output directories, GHC 9.14.1, Cabal 3.18.2.0, Node 22.23.2 and Rust 1.99.0.

| Mode | macOS | Windows | Tag |
|---|---|---|---|
| dev | Ad-hoc | Unsigned | `dev-$VERSION` |
| prod | Developer ID + notarization | Authenticode | `v$VERSION` |

For prod, refresh upstream main on each build and verification host. The commit must belong to its history.

```sh
git fetch https://github.com/aleski-green/sapiens4.git +refs/heads/main:refs/remotes/release/main
```

**macOS (Bash).** For prod, set `SAPIENS_MAC_SIGN_IDENTITY` and `SAPIENS_MAC_NOTARY_PROFILE`.

```bash
MODE=dev VERSION=0.1.0-dev.local1
COMMIT=$(git rev-parse HEAD)
./environments/macos/build.sh --local "$MODE" "$COMMIT" "$VERSION" ".build/release/$VERSION/mac"
```

**Windows (PowerShell).** Set `$Commit` and `$Version` to the macOS values. Initialize submodules recursively and configure Cabal for secure HTTPS Hackage. For prod, replace the empty certificate argument with `-Channel stable -CertificateThumbprint THUMBPRINT`.

```powershell
if ((git rev-parse HEAD) -ne $Commit -or (git status --porcelain)) { throw 'Wrong or dirty checkout' }
.\environments\windows\build.ps1 -Version $Version -OutputDirectory ".build/release/$Version/windows" -CertificateThumbprint ''
```

Run [native acceptance](testing.md#d1-native-distribution-acceptance) with synthetic data. Collect the installer, ZIP and receipt from each trusted build host under `.build/release/$VERSION/{mac,windows}`. In the same Bash session, run:

```bash
(
  set -eu
  list=$(mktemp -d)/assets
  runghc -ibuild/package build/Package.hs release --mode "$MODE" --directory ".build/release/$VERSION" \
    --source-commit "$COMMIT" --version "$VERSION" --output "$list"
  assets=()
  while IFS= read -r path; do assets+=("$path"); done < "$list"
  tag="dev-$VERSION"; if [ "$MODE" = prod ]; then tag="v$VERSION"; fi
  gh release create "$tag" "${assets[@]}" --repo aleski-green/sapiens4 --target "$COMMIT" --draft
)
```

Review all six assets and publish the draft manually.

## Provider and integrity

Builds verify pinned Codex automatically. For standalone use, run `runghc -ibuild/package build/Package.hs` with one of the following commands.

```text
provider --platform darwin-arm64 --destination .build/codex
verify --runtime PATH_TO_RUNTIME
provenance --output build/source-provenance.json
```

Use `windows-x64` on Windows. Generate provenance before exporting a checkout without Git.
