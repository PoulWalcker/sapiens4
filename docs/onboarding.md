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

For source builds, provide a Codex CLI through `SAPIENS_CODEX_BINARY` or the
existing installation discovery. This branch does not download distribution
payloads. D1 owns pinned provider acquisition and bundling for installable packages.

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
