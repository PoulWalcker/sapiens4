# D3 simplification implementation report

Implemented on the existing `codex/d3-clean-main` branch in its clean isolated
worktree. Both the remote PR #61 ref and local HEAD were verified as
`c086be9d3ce303bf5073d6fd35c88d68dd72bdca` before changes. The comparison baseline
is `f108e45843ab9817e9653d8467e0a5b21209530f`. No main merge or remote push was made.

## Implemented behavior

One shared pre-backend Welcome + Storage page now offers Start fresh and attaching
existing data. Computer Access is optional and leads directly to a minimal Ready
screen with Open Workspace. Durable setup state has only ComputerAccess, Ready
and Complete; writes precede UI dismissal, interrupted stages resume, and Complete
skips setup on subsequent launches. Previous D3 completion survives removal of
its Corpora choice. The storage picker remains reachable through Your data on
Computer Access, preserving the existing native replacement/rollback lifecycle.

External Codex discovery, inherited credentials and execution arguments are back
to baseline. Task admission and queues no longer depend on setup or account
holds. Corpora restoration uses the baseline registry path. SQLite versions 0–8
and the version 1/2 jobs import are restored, with shared version compatibility
and read-only preflight integrity checks.

## KEEP / MODIFY / DELETE

| Decision | Scope |
| --- | --- |
| KEEP | Shared DataRoot checks, locks, SQLite/agent-state initialization and migrations, Corpora identity/routing, readiness/shutdown and native child ownership. |
| KEEP | Runtime storage switching, validation before stopping the old host, unchanged-selection reuse, rollback and disconnected recovery. |
| KEEP | Necessary Windows process-job cleanup, WebView2 detection/loading and build fixes; macOS native browser ownership and device-local client preferences. |
| MODIFY | Welcome and Storage become one shared bootstrap screen. Setup is Computer Access → Ready → Complete, without a persisted starting Corpora. |
| MODIFY | Restore baseline CLI/environment/task and registry behavior. Restore supported SQLite migrations removed by the original PR. |
| MODIFY | Register application modules in this repository's Cabal manifest; restore upstream libs4 URL/branch and baseline gitlink. |
| DELETE | AI Connections, provider cards/icons/licenses, account-management screens/effects, browser/device-code login, logout/account switching, status polling, credential isolation, account holds/leases and setup execution gates. |
| DELETE | Corpora setup choice, summaries/advertisements, progress bars, dead Welcome/provider state, sign-in focus helper and styles. |

Pre-existing AI execution and other AI-related baseline functionality are retained.
No D1 distribution or D2 update features were added or redesigned.

## Measured LOC

Physical lines from `git diff --numstat`. Production includes app/build/corpora/
environments/remote source and Cabal metadata; it excludes tests, documentation,
SVG assets, licenses and submodule gitlink records. This reproduces the supplied
previous production figure exactly. Comments and blank source lines are included.

| Comparison | Added | Deleted | Net |
| --- | ---: | ---: | ---: |
| Reviewed PR production, baseline → c086be9 | 2,514 | 287 | +2,227 |
| Result production, baseline → local result | 1,878 | 212 | +1,666 |
| This simplification's production changes | 420 | 981 | −561 |

The reduction is **25.2%**. The result includes 166 new lines of repository-owned
Cabal registration needed to remove the fork-only pin. Tests are +436 against
baseline, versus +283 previously: this work adds +153 test lines net. Combined
production + tests is **+2,102**, down from **+2,510** (−408). The two newly added
backend spec files contain 160 lines; existing native coverage and helpers were
adapted as well. Four provider SVGs and their documentation/licenses are removed
in addition to the production-code reduction.

## Actual verification — macOS

Executed on macOS arm64 using GHC 9.10.3 and the installed Node/Ruby toolchains.

| Verification | Result |
| --- | --- |
| `runghc build/Build.hs` | Passed: Haskell backend/domain/peer/macOS executables; PureScript compile and three UI bundles. Existing compiler warnings remain. |
| onboarding/storage/launcher specs | 13 examples, 0 failures. Covers four new comprehensive E2E tests plus nine launcher contracts. |
| service/Corpora/workflow/scheduling specs, excluding `slow` | 105 examples, 0 failures. Includes actual controlled CLI execution, cancellation/descendants, queued-work restart, legacy agent state, delegation and Corpora behavior. |
| Final combined native onboarding spec | 2 examples, 0 failures; seed 29453. Both real native UI journeys pass together. |
| Isolated source-distribution spec | 2 examples, 0 failures. Builds a signed macOS launcher from copied source, leaves pre-existing data untouched and boots/completes a task using the release's assets. |
| Final process inspection | No owned onboarding launchers or test backends remained. |
| `git diff --check` | Passed. |

Total final executed examples across these runs: **122 passing**.

The new backend scenarios exercise inherited CODEX_HOME/explicit synthetic API
credentials, discovery of an external fixture CLI, ignored old account holds,
AI work and active child Corpora before completion, stage restart/back/finish,
obsolete action rejection and completion-save failure/retry. Storage scenarios
exercise missing/invalid/partial/locked/unsupported stores and all supported
pre-current SQLite versions, including actual legacy jobs imports.

Native journeys cover no backend before storage selection, first default startup,
selection persistence, unchanged-selection process reuse, invalid target rejection,
existing attachment, request/recheck/skip, interruption at permissions and Ready,
failed completion save, completed restart, selected Corpora/draft restoration,
replacement-startup rollback, locked-target retry and missing saved-folder recovery.
Permission responses are supplied by a controlled helper; the tests do not grant
real macOS Accessibility permission. Earlier development runs exposed an incorrect
AX role assertion and a folder-picker interaction timeout; the corrected final
combined run passes without retrying uncertain clicks.

## Windows and unexecuted verification

All nine Windows launcher modules were successfully typechecked with GHC
`-fno-code`, the configured package database and existing dependencies. This is
source typechecking on macOS, **not a Windows build or runtime result**.
Windows linking, WebView2/folder-picker interactions, process-job termination,
replacement/rollback and inherited authentication still require Windows QA.
The shared backend specs are platform-neutral, but were executed here on macOS.
The full slow suite, real provider authentication/model calls, actual OS permission
grants, installer/reinstall/update qualification and Linux runtime QA were not run.

## Remaining complexity and risks

Native replacement and recovery orchestration still exists separately for macOS
and Windows, as required by the instruction to preserve runtime switching rather
than redesign it. Common storage rules remain in DataRoot. Its preflight table
inventory still parallels the initializer's SQL definitions; supported database
versions now have a single shared declaration. The application Cabal manifest
owns application module lists while importing upstream dependency constraints;
future dependency changes should keep those declarations aligned.

Malformed/unreadable onboarding settings retain the existing explicit startup
failure rather than silently resetting setup. Missing settings resume Computer
Access. Unknown future schemas are rejected. No speculative recovery marker or
legacy adapter framework was introduced. SQLite read-only preflight can create
normal WAL/shared-memory sidecars; rejection tests confirm persistent data hashes
remain unchanged. The existing supported migrations can write only after the
backend acquires its own storage lock and validates the selected root.

The application changes are implemented and the macOS checks above pass. The
remaining verification gap is native Windows/platform permission/install QA.

## Changed files versus reviewed head

The table includes all 45 implementation/test/documentation/dependency paths
changed from c086be9 before adding this report. Restored baseline files therefore
appear even when absent from the final base-to-result diff. This report is the
additional documentation file; it is excluded from production/test LOC totals.

| File | Added | Deleted | Net |
| --- | ---: | ---: | ---: |
| `.gitmodules` | 2 | 2 | +0 |
| `app/features/Conversation.purs` | 1 | 1 | +0 |
| `app/features/CorporaEffects.purs` | 1 | 2 | -1 |
| `app/features/Dialog.purs` | 24 | 211 | -187 |
| `app/features/DialogEffects.js` | 0 | 1 | -1 |
| `app/features/DialogEffects.purs` | 15 | 49 | -34 |
| `app/features/Workspace.purs` | 2 | 2 | +0 |
| `app/shell/AI-LOGOS-LICENSE.txt` | 0 | 21 | -21 |
| `app/shell/AI-LOGOS.md` | 0 | 8 | -8 |
| `app/shell/ai-claude.svg` | 0 | 1 | -1 |
| `app/shell/ai-codex.svg` | 0 | 1 | -1 |
| `app/shell/ai-gemini.svg` | 0 | 1 | -1 |
| `app/shell/ai-kimi.svg` | 0 | 1 | -1 |
| `app/shell/bootstrap.html` | 8 | 35 | -27 |
| `app/shell/interaction/App.purs` | 15 | 19 | -4 |
| `app/shell/interaction/Effects.purs` | 1 | 2 | -1 |
| `app/shell/model/Action.purs` | 0 | 1 | -1 |
| `app/shell/model/Model.purs` | 22 | 11 | +11 |
| `app/shell/model/Update.purs` | 1 | 1 | +0 |
| `app/shell/rendering/View.purs` | 1 | 8 | -7 |
| `app/theme/live.css` | 3 | 68 | -65 |
| `build/Build.hs` | 2 | 2 | +0 |
| `build/cabal.project` | 3 | 0 | +3 |
| `build/sapiens-domain.cabal` | 163 | 0 | +163 |
| `corpora/host/Assets.hs` | 1 | 2 | -1 |
| `corpora/host/HostServer.hs` | 2 | 8 | -6 |
| `corpora/state/DataRoot.hs` | 8 | 2 | +6 |
| `corpora/state/Store.hs` | 3 | 1 | +2 |
| `corpora/work/Execution.hs` | 27 | 30 | -3 |
| `corpora/work/Onboarding.hs` | 67 | 332 | -265 |
| `corpora/work/Provider.hs` | 36 | 134 | -98 |
| `docs/onboarding.md` | 56 | 83 | -27 |
| `environments/macos/Application.hs` | 6 | 6 | +0 |
| `environments/macos/Browser.hs` | 0 | 6 | -6 |
| `environments/windows/Browser.hs` | 1 | 4 | -3 |
| `environments/windows/Host.hs` | 2 | 3 | -1 |
| `environments/windows/Shell.hs` | 1 | 2 | -1 |
| `remote/control/Registry.hs` | 4 | 38 | -34 |
| `spec/onboarding_browser_spec.rb` | 152 | 142 | +10 |
| `spec/onboarding_spec.rb` | 78 | 0 | +78 |
| `spec/storage_spec.rb` | 82 | 0 | +82 |
| `spec/support/host.rb` | 2 | 4 | -2 |
| `spec/support/onboarding_tools.rb` | 6 | 22 | -16 |
| `spec/support/provider.rb` | 2 | 1 | +1 |
| `submodules/libs4` | 1 | 1 | +0 |
