# Local AI onboarding

Native startup shows **Welcome → Your data** before launching the backend.
Choose **Start new** or **Open existing data**; the selected directory is saved in
`storage.json` under `SAPIENS_CONFIG_DIR`. Existing data must pass compatibility
and ownership checks. No automatic search, copy or migration is performed.
After storage selection: **AI → Computer access → Corpora (if needed) → Ready**.
The completed setup and selected Corpora survive restart; AI connections remain
available from **AI Connections**.
Claude Code, Gemini CLI and Kimi Code remain Coming soon. Browser sign-in uses
the complete authorization URL produced by Codex. One-time-code sign-in is
available after cancelling an active browser sign-in.

## Source prerequisites

This feature builds independently of the distribution/installer branch. Until
the module registration dependency is merged upstream, initialize the pinned
libs4 fork at the commit recorded by `git ls-tree HEAD submodules/libs4`.

`build/start.sh` (macOS arm64) and `build/start.ps1` (Windows x64) acquire Codex
0.167.0 into `.build/codex` before starting the host. This preparation requires
Python 3 with its standard library and network access. The lock is owned by
Sapiens at `build/codex-provider-lock.json`; archive hashes, package layout,
resources and redistribution notices are verified. Existing payloads are
verified against their inventory. No developer Codex installation is selected
implicitly.

For native source launch, prepare the provider before using the existing native
build script:

```sh
python3 build/acquire-codex.py --platform darwin-arm64 --destination .build/codex --ensure
./environments/macos/build.sh
```

On Windows use `python build/acquire-codex.py --platform windows-x64
--destination .build/codex --ensure`, then the existing Windows native build
script. `SAPIENS_CODEX_BINARY` explicitly selects an alternate complete Codex
runtime for development; source launch skips acquisition when it is set.

## Account and data safety

Login and execution share an app-owned Codex home, under `SAPIENS_CONFIG_DIR`
when configured. Inherited CODEX_* and OPENAI_* authentication variables are
removed. Disconnect and account replacement take an exclusive profile lock;
running AI work retains a shared lease. Queued work remains paused after account
replacement until **Resume AI work**. Chats and settings are retained.

AI connection and computer permission actions share `/api/host/onboarding`.
Permission checks remain separate from the action that requests Accessibility access.

Accessibility permission is optional on macOS and can be requested/rechecked
from Computer settings. Windows does not display macOS permission instructions
for its normal permission state. Automatic recovery UI triggered by a denied
computer tool call is not implemented by this extraction.

## Manual acceptance

Use an isolated config and data directory. Check first launch, real browser
sign-in, complete copied link, cancel/retry, disconnect then immediate reconnect,
change account and explicit Resume. Check restart persistence and that queued
work stays paused during account replacement. Check narrow/normal windows and
light/dark themes. Verify native focus returns after sign-in on each OS.

Existing synthetic API scenarios live in `spec/onboarding_spec.rb`; they do not
prove real OAuth, actual model output, clean-machine installation or macOS 13
compatibility. No installer, release workflow or publishing behavior is included
in this feature branch.
