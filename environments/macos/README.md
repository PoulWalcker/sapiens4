# macOS

Requires macOS 13+ on Apple Silicon.
Mount `Sapiens.dmg`, copy **Sapiens4.app** to Applications, and open it.
Choose **Start fresh** or **Use another folder** for a complete Data Root.
Install a model CLI and sign in, for example `codex login`.
Grant Accessibility permission for computer control; chat does not require it.

## Build

Install Xcode Command Line Tools and the [source prerequisites](../../README.md).
Use a new output directory:

```sh
git submodule update --init --recursive
./environments/macos/build.sh 0.1.0 .build/release-local
(cd .build/release-local && shasum -a 256 -c SHA256SUMS)
```

Automatic updates verify the SHA-256 digest published by GitHub for the release
asset. To sign and notarize a release, add `--release` and set
`SAPIENS_MAC_SIGN_IDENTITY` and `SAPIENS_MAC_NOTARY_PROFILE`.
Data/config: `~/Library/Application Support/Sapiens4/{data,config}`.
Startup log: `config/desktop-server.log`.
[Remote control](../../docs/remote.md) · [Tests](../../docs/testing.md)
