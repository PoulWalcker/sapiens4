# Sapiens4 for macOS

On macOS 13+ Apple Silicon, mount `Sapiens.dmg`, copy **Sapiens4.app** to
Applications (or your own Applications folder), and open it. No Sapiens build
tools are needed. The app opens without Codex; model execution requires your
installed Codex CLI and existing `codex login`. Nothing changes Codex credentials.

Quit Sapiens4 before replacing or removing the app. Quit stops its owned backend;
closing the last window keeps the app running. Replacing, removing and reinstalling
the app retain `~/Library/Application Support/Sapiens4/{data,config}`. The backend
log is `data/desktop-server.log`. Older Development directories remain untouched.
Use `SAPIENS_DATA_DIR` and `SAPIENS_CONFIG_DIR` at launch for explicit isolation.
For example, run `SAPIENS_DATA_DIR=/path/to/data /Applications/Sapiens4.app/Contents/MacOS/Sapiens4`.

## Build and verify

Install Command Line Tools, GHC 9.14.1, Cabal 3.18.2, Node.js 22.5+, npm,
Git and stable Rust. Initialize pinned submodules recursively. From the root:

```sh
git submodule update --init --recursive
./environments/macos/build.sh 0.1.0 .build/release-local
(cd .build/release-local && shasum -a 256 -c SHA256SUMS)
```

The output directory must be new. It receives only `Sapiens.dmg` and `SHA256SUMS`.
Builds reuse compiler caches and assemble fresh temporary payloads. Local builds
are ad-hoc signed. Each Mach-O must be ARM64, support macOS 13, and resolve all
non-system dependencies inside the app; recursive relocation rejects basename
collisions. The final mounted image is copied, audited and signature-verified.

For a public release, set `SAPIENS_MAC_SIGN_IDENTITY` to a Developer ID Application
identity and `SAPIENS_MAC_NOTARY_PROFILE` to a configured `notarytool` keychain
profile, then add `--release`:

```sh
./environments/macos/build.sh 0.1.0 .build/release-public --release
```

Public builds require both credentials, sign nested code with Hardened Runtime,
notarize and staple the app and DMG, then run Gatekeeper and signature checks.
The temporary notarization ZIP is discarded. Credentials are not stored in outputs.
Installer acceptance tests are deferred until the implementation is complete.
Native browser tabs, file picking and accessibility behavior retain their existing interfaces.
