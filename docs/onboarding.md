# One-time desktop setup

macOS and Windows use the same local Welcome + Storage page before starting the
backend. Start fresh uses the existing default location and requires an empty
folder. Use another folder attaches a complete Sapiens Data Root in place.
The selection is saved in `storage.json` under the installation's configuration
directory. A missing saved folder opens the picker with a recoverable error;
it never falls back to creating a replacement store.

`DataRoot.hs` supplies shared checks for both native launchers and the backend.
Preflight checks ownership, identity, database integrity, registered children
and saved agent state. Backend initialization repeats checks under its own lock,
then uses the existing SQLite and agent-state migrations. Supported SQLite
versions remain 0–8, including the version 1/2 jobs import. No new initialization
command, storage modes, copying or data migration system is introduced.

After storage opens, setup consists of **Computer Access → Ready → Complete**.
Accessibility request and Recheck remain available on macOS. Skip continues
without permission. Windows uses its existing UI Automation behavior and does
not request macOS Accessibility approval. Ready contains a short confirmation
and Open Workspace. A failed completion save leaves Ready open for retry.

Only the setup stage and schema are written to `onboarding.json`. Missing
settings start at Computer Access; interrupted setup resumes its last committed
stage. Completed earlier D3 settings retain completion even if their discarded
Corpora selection is absent. An earlier provider stage resumes at Computer
Access; an earlier Corpora stage resumes at Ready. Unreadable or invalid setup
settings retain the existing explicit startup error and are not overwritten.
An update/reinstall preserving this configuration retains completion.

Setup has no role in task admission, queue processing or Corpora restoration.
Users install and authenticate Codex externally. Existing CLI discovery,
`SAPIENS_CODEX_BINARY`, inherited `CODEX_HOME`/authentication variables and normal
execution arguments are preserved. Sapiens does not install, authenticate,
replace accounts, poll provider status or hold work for setup. Active Corpora
and the saved client selection resume through the existing registry paths.

## Runtime storage switching

Your data on Computer Access revisits the existing runtime storage picker.
Keep current storage and reselecting the current root reuse the owned process.
Repeated Start fresh also reuses a root created by this session. Picking another
folder records a draft; Continue validates it before stopping the current host.
A different nonempty folder never becomes a fresh store.

The existing replacement and rollback lifecycle remains: failed preflight keeps
the current backend running; failed replacement startup reopens the prior root;
failed rollback disconnects into the shared picker. Saved selection changes only
after replacement installation succeeds. Native handlers serialize requests and
scope replies to the requesting dialog. No files are moved or deleted.

## Build and verification

Application module registration is in `build/sapiens-domain.cabal` and
`build/cabal.project`. Dependencies and native bindings still come from upstream
libs4 at the baseline commit; no fork-only pin is needed. Build normally with
`runghc build/Build.hs`.

`spec/onboarding_spec.rb` exercises durable stages, save failures, external
credentials and AI execution/active Corpora restoration before completion.
`spec/storage_spec.rb` checks invalid, missing, partial, locked and unsupported
stores, retry, and every supported SQLite version through the real initializer.
`spec/onboarding_browser_spec.rb` contains two macOS native journeys covering
storage selection/switching/rollback, permission request/recheck/skip, interrupted
setup, completion save failure, selected Corpora/draft restoration and missing
saved-folder recovery. Permission responses use a controlled external helper;
actual OS permission grants and Windows desktop journeys require platform QA.
