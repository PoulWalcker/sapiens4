# Sapiens4

Local AI agents with chat, tasks, files and computer access.

- [macOS](environments/macos/README.md) · [Windows](environments/windows/README.md) · [Linux](environments/linux/README.md)
- [Remote control and pairing](docs/remote.md)
- [Models, data and configuration](docs/reference.md)
- [Tests](docs/testing.md) · [Contributing](docs/contributing.md)

## Run from source

Requires Git, GHC 9.14.1, Cabal 3.18.2, Rust, Node.js 22.5+ and npm.
Install a model CLI and sign in, for example `codex login`.

```sh
git clone --recurse-submodules https://github.com/aleski-green/sapiens4.git
cd sapiens4
./build/start.sh --open
```

Windows: `.\build\start.ps1 -Open`.
Local workspace: http://127.0.0.1:4174/workspace/.

[License](license/LICENSE.md) · [Commercial licensing](license/LICENSING.md)

App version: **v00.02.00**. The previous dev data layout is **v00.01.99**.
Persistent upgrades run through the [versioned migrations](migrations/README.md).
