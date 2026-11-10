# Sapiens4

Local conversational agents with Chat, Groups, tasks, Notes, native browser
workspaces and remote access. Codex CLI runs model calls; Blindly4 provides
computer access. Sapiens4 is built around proactivity, self-reflection and
independent judgment with human oversight.

## Start

| Platform | Setup |
| --- | --- |
| macOS | [Build and run the desktop app](environments/macos/README.md) |
| Windows | [Install and build](environments/windows/README.md) |
| Remote browser or mobile app | [Pairing and relay setup](docs/remote.md) |

For source CLI execution, install the [distribution toolchain](docs/release-development.md)
and platform prerequisites, then compile:

```sh
git clone --recurse-submodules https://github.com/PoulWalcker/sapiens4.git
cd sapiens4
runghc build/Build.hs
```

Prepare the [pinned Codex provider](docs/release-development.md#provider-and-integrity),
then run `.build/codex/bin/codex login` (`codex.exe` on Windows) and
`./build/start.sh --open` or `.\build\start.ps1 -Open`. The local UI opens at
http://127.0.0.1:4174/workspace/.

The UI uses PureScript/Halogen. Source startup builds it when needed; completed
packages need no Node installation. Third-party dependencies live in the pinned
[libs4](https://github.com/PoulWalcker/libs4) submodule.

The header switches between local corpora and saved remote connections. Selection
does not change execution: use Pause or Activate separately. On mobile, install
and open the Home Screen app before pairing, then approve the matching code on
the host. Each corpora supports one approved remote device.

## Documentation

Application source lives in `corpora/` (state and work), `app/` (the UI),
`remote/` (relay, control and protocol) and `environments/` (native integration).
Shared build tools and launchers live in `build/`; tests live in `spec/`.
Dependencies retain their own layout under `submodules/`.

- [Usage, configuration, data and API](docs/reference.md)
- [UI architecture](docs/ui.md)
- [Running behavior tests](docs/testing.md)
- [Contributing](docs/contributing.md)

Windows support was contributed through the
[fatmahalqaisi-code/sapiens4 fork](https://github.com/fatmahalqaisi-code/sapiens4).

## License

Sapiens4 is source-available under the [Sustainable Use License 1.0](license/LICENSE.md).
See [licensing and commercial services](license/LICENSING.md).
Commercial inquiries: [license@sapiens4.ai](mailto:license@sapiens4.ai).
