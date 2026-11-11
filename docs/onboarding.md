# Minimal desktop startup

macOS and Windows use the same local Welcome + Storage page before starting the
Haskell backend:

**Welcome + Storage → Validate Storage → Start Backend → Open Workspace**

Start fresh uses the existing default directory and requires an empty folder.
Use another folder attaches a complete Sapiens Data Root in place. Nothing is
copied, reset or overwritten to make an incomplete root usable.

After a successful workspace navigation, the launcher saves the canonical root
in `storage.json` in the installation configuration directory, using the existing
durable atomic JSON writer. Later launches validate the saved root and open the
workspace directly. Missing, unreadable, invalid or locked saved storage returns
to the same pre-backend picker with an error. It never creates replacement data.
If initialization was interrupted, restore complete data from backup or select
another empty directory; partial initialization is deliberately not resumed.

`DataRoot.hs` shares validation between both launchers and the backend. Preflight
checks ownership, identity, SQLite integrity and version, registered children and
saved agent state. Backend initialization repeats validation under its exclusive
lock, then uses the existing SQLite and agent-state migrations. Supported SQLite
versions remain 0–8, including the version 1/2 jobs import. Normal Corpora creation,
selection, restoration and routing remain independent of startup selection.

Readiness and workspace navigation have bounded waits. Startup failures stop and
reap only children owned by the launcher, retaining ownership if termination
cannot be confirmed. A failed startup, navigation or selection save offers retry
through the picker. Successfully initialized data is never erased after failure;
attach it explicitly on retry. Close during startup cancels the worker before
stopping the owned backend. Storage selection is confined to the pre-backend picker. The running workspace
has no storage switching.

There are no persistent setup stages, setup completion checks, post-backend
wizard, setup API or runtime storage switching. Legacy setup files are ignored
and left untouched. Computer/Accessibility remains available through the existing
Shared computer UI and normal execution paths. Users install and authenticate
Codex externally; discovery, inherited credentials and execution are unchanged.

Device-local workspace preferences and active Corpora restoration remain outside
Corpora snapshots. macOS uses its profile-specific NSUserDefaults suite. Windows
reuses the legacy `DataRoot/WebView2/Shell` profile if no newer config profile
exists; otherwise it keeps the config profile introduced by D3. Profiles are not
moved or overwritten. Windows also mirrors theme, active Corpora and workspace
preferences to root-specific durable JSON under config, restoring them before
page scripts run on a changed loopback port. Guest browser profiles are unchanged.

Application module registration is in `build/sapiens-domain.cabal` and
`build/cabal.project`. Native launchers reuse `Types.writeJSON`; the upstream
libs4 submodule remains unchanged. Build with `runghc build/Build.hs`; see
[Testing](testing.md) for native desktop prerequisites.

## Refactor inventory

KEEP: the shared bootstrap page, `DataRoot` validation and locks, normal backend
initialization/migrations/readiness/shutdown, Corpora routing, external Codex
execution, normal Computer features, preferences and workspace restoration.

MODIFY: cold-start native ownership, navigation completion and retry, durable
storage selection, Windows profile continuity and native preference restoration.

DELETE: the post-backend wizard, persistent setup progress/completion, setup API,
PureScript storage UI, live-switch transactions, rollback and old-backend restart.

| Files | Changed or removed functions/state |
| --- | --- |
| `environments/macos/Application.hs` | `Application`, `ServerProcess`, `check`, `startServer`, `launchServer`, `stopServer`, `reload`, `installMethods`, `initialize`, `runSession`, `closeApplication`, `receiveMessage`, `saveClientPreferences`, `installClientPreferences`, `serverRunning`, `installServer`, `startSelected`; add `stopOwnedServer`, `loadBootstrap`, `recoverStartup`, `workspaceWindow`; remove `restartServer`, runtime storage replies/handlers, `disconnectStorage`, `storageFailure`, `setSetupWindow` |
| `environments/macos/Browser.hs` | `receive`: remove setup and runtime-storage callbacks |
| `environments/windows/Host.hs` | `ServerProcess`, `startHost`, `launchHost`; remove `restartHost`, `activeDataDirectory`, `activeStorageChoice`, `hostRunning` |
| `environments/windows/Shell.hs` | `desktopSession`: startup selection/install/recovery, profile choice, navigation completion/deadline, preference persistence; add `jsonString`; remove setup stages, runtime messages, previous-server replacement/rollback |
| `environments/windows/Native.hs` | add `addDocumentScript` completion barrier |
| `corpora/state/DataRoot.hs` | `saveDataDirectory` uses `Types.writeJSON`; remove legacy setup special case from `startupMessage`; validation unchanged |
| `corpora/host/HostServer.hs` | `startHostWith`, `application`, `httpApplication`: remove setup resource and GET/POST routes |
| `corpora/work/Onboarding.hs` | delete module and all setup/permission handlers |
| `build/sapiens-domain.cabal` | remove setup module; register existing `Types` for native executables and probe |
| `app/features/Dialog.purs` | `make`, `view`, `body`; remove setup title/dismissal logic, setup/storage views and D3 permission controls; restore normal Computer UI |
| `app/features/DialogEffects.purs` | `handle`: remove setup polling, storage effects, setup mutations and guarded setup dismissal |
| `app/features/Workspace.purs` | `sync`: remove setup gating and native setup messages |
| `app/shell/interaction/App.purs` | `handle`: remove setup checks and runtime-storage report dispatch |
| `app/shell/model/Action.purs` | remove setup/storage actions and dialog intent |
| `app/shell/model/Form.purs` | remove `SetupContent`, `setupValue` |
| `app/shell/model/Model.purs` | remove setup/storage ADTs, dialog draft, session fields, initial values, stage parsing and `setupRequired` |
| `app/shell/model/Update.purs` | `update`/`Entered`: remove setup-dialog retention |
| `app/shell/rendering/Html.purs` | remove unused `navigationControl` |
| `app/shell/rendering/View.purs` | remove setup gating/loading/error/retry UI |
| `app/theme/live.css` | remove wizard-only rules; keep bootstrap styles |
| `spec/support/host.rb` | initialize saved storage instead of completed setup for existing native journeys |
| `spec/windows_startup_spec.rb` | adapt missing/exiting-runtime checks to the pre-backend screen; isolate configuration |
| `docs/onboarding.md`, `docs/ui.md` | document current startup, recovery and preference behavior |

## LOC accounting

These measurements cover the minimal-flow refactor and its safe simplification
follow-up relative to the reviewed HEAD and original baseline.

Counts use `git diff --numstat` against reviewed HEAD
`05e95c48f0225c58edc77d2297a995637d8530d0` and baseline
`f108e45843ab9817e9653d8467e0a5b21209530f`. Production/build includes `app`,
`build`, `corpora`, `environments`, `remote`; tests and documentation are separate.
No production code was moved to obtain the reduction.

| Area | Refactor added | Refactor deleted | Refactor net | Final baseline net |
| --- | ---: | ---: | ---: | ---: |
| macOS | 155 | 215 | -60 | +313 |
| Windows | 150 | 176 | -26 | +322 |
| Shared/backend Haskell | 12 | 132 | -120 | +249 |
| PureScript | 19 | 239 | -220 | +0 |
| Bootstrap, shell HTML and styles | 5 | 43 | -38 | +148 |
| Build | 12 | 21 | -9 | +161 |
| **Production/build total** | **353** | **826** | **-473** | **+1,193** |

The original baseline-to-reviewed-HEAD production/build growth is reproduced as
+1,666 lines. Added process-reader ownership, bounded navigation recovery and
Windows preference preservation account for retained safety work. Platform
process startup/stop implementations and the small preference-injection scripts
remain separate because AppKit/WebKit and Win32/COM have different ownership,
threading, callback and persistence requirements. Validation and durable JSON
writing remain shared; no replacement framework or storage initialization command
was introduced.

## Executed validation and limits

The results below describe validation performed before removal of MR-added tests.
At the user’s request, the two new native startup scenarios and the new public
storage-validation scenario were removed; pre-existing tests and their necessary
startup adaptations remain. No tests were run for that removal.

On macOS ARM64, the backend, native launcher and PureScript bundles built with
GHC 9.14.1, Ruby 3.4.11 and Node 22.23.2. The Windows adapter passes a host-side
`ghc -fno-code` typecheck with GHC 9.14.1; this does not establish Windows linking,
COM ABI correctness, WebView2 profile behavior, ARM64/x64 parity or reinstall
behavior on a Windows machine.

Focused backend integration completed with 129 examples, zero failures and three
platform skips. Public storage validation completed with 10 examples and zero
failures. The two native first-run/recovery journeys passed, including fresh
startup, saved-root restart, native-folder attachment, Corpora/draft/appearance
restoration, missing/partial/corrupted/locked storage, and ignored legacy setup
settings. Controlled macOS manual fixtures passed early backend exit, the real
35-second readiness timeout, failed workspace navigation, closing the actual
startup window, and selection-persistence failure. These checks verified owned
child exit and no selected-root commit on failure; initialized data survived
persistence failure. A source-only distribution built and completed external
provider work successfully.

The user explicitly stopped both broad regression runs. They must not be resumed
without a new explicit request. Partial results at interruption were:

- Non-desktop: 184 examples, zero failures, 35 platform/environment skips.
- Desktop: 50 examples, two failures, 13 platform/environment skips. The failures
  were `spec/chat_scroll_spec.rb:18` (Accessibility focus unavailable after a
  reply) and `spec/workspace_accessibility_spec.rb:38` (timed out waiting for
  “Pause First”). Neither failure has been isolated or classified; the complete
  suite has not passed.

Physical unavailable drives, a backend that refuses all termination attempts,
and native Windows execution remain untested. Failed-stop ownership is retained
in code until an exit check confirms termination. Interrupted initialization is
rejected rather than repaired automatically. Legacy WebView2 profile directories
are preserved in place; recovery of preferences already lost by an earlier
installation is not attempted.

## Safe simplification follow-up

The next pass removes Windows launcher/probe Cabal duplication through a common
stanza, unused startup stderr buffers and their parameters, redundant environment
copying for `check-data`, and four bootstrap CSS rules already overridden by the
existing stylesheet. The macOS diagnostic log, stderr consumption, process
ownership, cleanup, timeouts, storage validation and durable persistence remain.
The existing short macOS EOF delay is retained for diagnostic-reader behavior.

Compared with the working tree immediately before this pass, production/build
changed by **25 lines added, 45 deleted: net -20**. MR growth is now **+1,193**
lines against the original baseline, down from +1,213. The complete minimal-flow
refactor now removes 473 net production/build lines relative to the reviewed HEAD.
The preference bridge and startup-state representation were kept because a larger
rewrite would add machinery and risk without established net LOC savings.

Validation was limited to GHC 9.14.1 compilation of the backend/macOS executable,
host-side Windows typechecking and a static comparison of resolved Cabal package
descriptions. Before/after descriptions matched for macOS ARM64, Windows x64,
Windows ARM64 and Linux x64, each with both desktop-flag settings. Native Windows
linking and execution remain unverified. No regression tests were started.
