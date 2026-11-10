# Local AI onboarding

Native startup shows **Welcome → Your data** before launching the backend.
Choosing another folder opens the picker immediately. Continue applies the
selection, and cancelling the picker retains the previous selection.
Choose **Start fresh** or **Use another folder**; the selected directory is saved in
`storage.json` under `SAPIENS_CONFIG_DIR`. Existing data must pass compatibility
and ownership checks. No automatic search, copy or migration is performed.
**Start fresh** requires an empty directory. **Use another folder** requires a
complete installation and never initializes missing data. Source launch reopens
its existing selected directory unless an explicit storage mode is supplied.
After storage selection: **AI → Computer access → Corpora (if needed) → Ready**.
AI connection and computer access are optional. **Skip for now** opens the
remaining setup steps without Codex; finishing opens the dashboard and settings.
Only AI task execution requires a ready account. **Back** revisits earlier setup
decisions without losing the selected Corpora or changing account credentials.
Forward actions require an available local Corpora; permission grants and sign-in
are never required merely to explore the application.
The completed setup and selected Corpora survive restart; AI connections remain
available from **AI Connections**.
On a cold launch without a saved client selection, restore the starting Corpora
from setup. A valid client selection takes precedence after setup is complete;
later account checks never switch Corpora.
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
Initial setup completion is a separate execution requirement: authentication
alone cannot release queued, scheduled or delegated AI work in any local Corpora.
Finishing setup validates the local selection and saves completion before
resuming saved services. If resuming fails, retry Finish or reload the Corpora
list; already-open services are reused. Account-safety holds remain independent.
Finishing with an unavailable existing login leaves AI held for explicit review;
finishing without any login keeps the disconnected hold until connection succeeds.

Only the current setup schema is supported. Missing `onboarding.json` starts
setup; unreadable, corrupt or incompatible settings stop startup with a recovery
error and preserve the file. Restore valid settings from backup before restarting.

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

## Revisiting storage during setup

Back from the native AI step opens storage selection inside the existing page.
Welcome, storage and AI navigation retain the storage draft without stopping the
current backend. Keep current storage returns immediately. Selecting a folder
only records a draft; Continue applies it. Native handlers serialize folder
pickers and startup, and replies are scoped to their owning dialog generation.
An existing path that resolves to the current directory is also a no-op.
Repeating Start fresh for the current Data Root created by this running session
also reuses it; it does not initialize or overwrite it again. A different
populated directory still fails StartNew validation.

For a different directory, validate it while the current backend still owns its
lock, then stop/join that process and start the replacement. Failed validation
leaves the current backend and selection intact. If replacement startup fails
after stopping the prior process, the draft remains visible; choosing Keep
current storage and Continue reopens the previous directory. No data is moved
or deleted. Storage details show the final two path components; hovering reveals
the full path. Change is part of the existing-data card.
