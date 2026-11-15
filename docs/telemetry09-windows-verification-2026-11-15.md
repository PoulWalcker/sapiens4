# Ticket 09 — actual Windows/WebView2 verification

Date: 2026-11-15 (Asia/Dubai).

Integration note: imported into `telemetry/d4` after the scope and batch-size
changes. The observations below retain their original `3b12492` / `4bea2f4`
revision boundaries; they do not verify the later scheduler heartbeat, ten-event
batch limit or revised Settings copy on Windows. Previously removed explanatory
documents remain removed; their links below point to the reviewed Git revision.

**Verdict: the requested focused Windows scenarios pass with the separate fix
`4bea2f46a6714aa4a9b79519f831d00b0fa3fa6a`. The original `3b12492` fails
fresh native configuration provisioning.** This report supersedes the initial
runner-unavailable report in this file. It does not close installer/E2E,
production collection or disclosure-policy release gates.

## Revisions, environment and isolation

- Requested repository: `https://github.com/PoulWalcker/sapiens4`;
  `git ls-remote` confirmed `telemetry/d4` at
  `3b12492913e41299b963f07759b20b3e64ee0eb0` when selected.
- Separate clone:
  `/Users/pashvel/Desktop/personal/sapiens4-worktrees/telemetry09-windows-20261115`;
  local branch `codex/telemetry09-windows-20261115`.
- Separate implementation fix: `4bea2f46a6714aa4a9b79519f831d00b0fa3fa6a`.
  The fix changes one source line. Report changes are committed separately.
- libs4: `170c2fd6936269e3245a0246787101dc9ac9310b`;
  Blindly4: `cfc28cb5bcccbd58947bf13e80b79b76062b8dc9`. Neither gitlink changed.
  Blindly4 was built from that exact pin, offline, into an owned target directory.
- Actual Windows Server 2022 Datacenter x64, build 20348 / NT 10.0.20348;
  WebView2 **157.0.4341.46**; an interactive elevated Administrator session.
  GHC **9.14.1**, installed Cabal **3.18.2.0**, Rust/Cargo **1.99.0**.
- All new Windows sources, outputs, profiles and DataRoots were confined to
  `C:\Windows\Temp\t09-3b12492-20261115`. Existing `C:\Build` dependencies were
  read without changing the shared checkout or package database.
- The actual host and ordinary WindowsMain built directly with GHC against the
  existing pinned package database, using the pinned libs4 Windows C source.
  A complete Windows Cabal build is not established by these direct builds.
- A fresh UI compilation used Node **22.23.2**, PureScript **0.15.16**, the pinned
  Spago/esbuild installation and the selected source. No UI warnings were emitted.
- Provider overrides pointed to an owned fake command that exits without a model.
  The final fake-provider invocation count is **zero**. Native child environments
  also omitted inherited API-key/token variables.

Instructions reviewed: [native UI](https://github.com/PoulWalcker/sapiens4/blob/3b12492913e41299b963f07759b20b3e64ee0eb0/docs/telemetry-native-ui.md),
[native authorization](https://github.com/PoulWalcker/sapiens4/blob/3b12492913e41299b963f07759b20b3e64ee0eb0/docs/telemetry-native-authorization.md), and the locally
available `.claude/tasks/telemetry/issues/09-native-ui.md`. That task file is
untracked in the selected commit and was not imported or edited. B9 and ticket
08 were not reopened; their earlier platform results are not counted as fresh
post-wiring evidence.

| Artifact | SHA256 |
| --- | --- |
| Fresh app.js | `227864a84990ae9f3d2d51318b6b3d0cf6b8e48a2b35d4da5dca7eee0edc9f5b` |
| Pinned native/windows.c | `49182188b17a0aa12889c1031646aafc8520182dec1fc9f8f8afe0a92e18b43a` |
| Fixed Shell.hs | `b145a5c160fac5f1a4863afdef5d2d3b1cf79440c32802d8ca7f7fe37b24099b` |
| Original ordinary desktop executable | `727c80cd4a0ada474204b014921b9536179133c4e1fd026713ecb8dc5c0a7f56` |
| Fixed ordinary desktop executable | `b684e6d243af737fc3ad70688f51edfbf4addd366ef6c3a846f5ccd47dd64fb2` |

## Reproduced defect and narrow fix

A new native launch creates the WebView2 shell profile below the configuration
root before reading consent. Ordinary directory creation assigned the config
root to `BUILTIN\Administrators` under the elevated token. This happened even
when its synthetic LOCALAPPDATA parent had already been restricted to TokenUser.
The resulting inherited DACL granted only that user, but the owner was wrong.
The shared profile authority correctly rejected it, leaving onboarding opt-in
disabled. No consent profile was created. A pre-existing private config did not
have this problem.

`15-fresh-config-repro.ps1` is the executed focused reproducer: start ordinary
WindowsMain beneath a new private LOCALAPPDATA, activate it with Blindly4, find
the real checkbox, inspect the resulting config owner/DACL, and require an
enabled checkbox. It reported `REPRO: fresh native config disables opt-in`.

The fix calls the existing `DataRoot.createPrivateDirectory configRoot` before
WebView2 can create its descendants. That helper creates missing parents and a
private, TokenUser-owned config, while leaving existing directories alone.
No new helper, dependency, route, consent operation or ACL repair is introduced.

The same fresh-launch observation against the fixed ordinary executable found
an enabled unchecked checkbox, a TokenUser owner and a protected user-only DACL,
with no telemetry profile before choice. Actual Blindly4 checkbox/Continue
interaction then reached workspace, enrolled and delivered creation plus one
open. Reopening the original unsafe fixture preserved its security descriptor,
kept the checkbox disabled and created no telemetry profile.

| Fresh-native provisioning | Original | Fixed |
| --- | --- | --- |
| New config below a private user-owned parent; real opt-in available | FAIL | PASS |
| Existing unsafe config remains unchanged and cannot authorize consent | PASS | PASS |

## Focused acceptance matrix

PASS below means observations from this Windows attempt, not inherited macOS or
08 results. Initial Settings/conflict/withdrawal observations used `3b12492` with
private pre-existing profiles. The fixed ordinary executable additionally covered
fresh provisioning, checked onboarding, notice repair, portable layout and
startup failure; final context observers were built from the fixed source. The
one-line fix does not change consent dispatch for those pre-existing profiles.

| Criterion / scenario | Result | Actual observation |
| --- | --- | --- |
| O16-A01: clean onboarding, existing storage screen, unchecked default | PASS after fix | Blindly4 found the native control; unchecked Continue reached workspace without telemetry profile, enrollment or collector HTTP. Fresh automatic config provisioning is corrected as above. |
| O16-A01: checked onboarding and Continue | PASS | Real checkbox and existing Continue action created a usable root, supported-v1 grant and one native open. No extra setup screen or injected UI action. |
| O16-A01: Settings toggle, collapsed notice and static illustration | PASS | Menu → Workspace & settings; one real checkbox; Details exposed the synthetic JSON and preview/history/in-flight/best-effort wording. The shared illustrative OS value remains `macos`; actual Windows wire/Parquet OS is `windows`. |
| O16-A01/A04: missing/invalid nested consent shown off | PASS | Missing profile was off. Unsupported nested notice 999 was shown off; a real Settings click repaired to supported v1 with a new epoch, preserving DeviceId and enrollment. No newly approved notice version was invented. |
| O16-A02: bounded result/privacy contract | PASS for inspected source and observed UI/output | The existing dedicated parser/result shape remains bounded to consent/notice/epoch, local-disable and fixed codes. Actual expanded Settings text contains synthetic IDs and no actual DeviceId, InstallationId or Basic password; native diagnostic outputs were checked separately for private enrollment values. This is not a DOM assertion or an arbitrary-JavaScript isolation claim. |
| O16-A02: malformed, mixed and forwarded native payloads | PASS | Actual trusted WebView2 callbacks carried extra fields, unsupported notice, malformed epoch, wrong choice type and mixed generic actions; no consent profile was created. Generic/window-forwarded consent-shaped input gained no authority. |
| O16-A02: retired core and obsolete generation | PASS | Retained strict consent-write events were replayed after actual legacy-core replacement and native recovery. No writer authority or profile. Actual generation changed 1→2; a reused COM pointer value compared equal in the latter observation, so pointer equality alone is not claimed to establish object identity. |
| O16-A02: stale current document | PASS | A retained workspace write was processed after the same core navigated to bootstrap, with unchanged generation and closing=false. Source/current context no longer authorized it; no profile. |
| O16-A02: closing callback | PASS | Actual WM_CLOSE replay: matching workspace Source/current, matching generation/core, closing=true; no profile. |
| O16-A02: ordinary child frames | PASS within the established trusted-bundle boundary | Actual same-origin, cross-origin, about:blank and srcdoc navigation events were cancelled; no consent profile. This establishes navigation/channel exclusion, not rejection of a message from an already loaded adversarial child. |
| O16-A02: separate guest | PASS | The real Browser.createTab guest reached `T09_GUEST_POST_RETURNED` after attempting a strict write. No guest write reached the shell authority and no profile was created. |
| O16-A02: local HTTP | PASS | Eight POST/PUT requests across four consent-shaped paths, with matching Origin and local JSON/header treatment, returned 404; no private profile was created. |
| O16-A02: encrypted Remote Control | PASS | Real host + relay + independently built Haskell peer completed encrypted pairing. The consent route returned 404; the host route returned `Remote API route is not allowed`; private consent remained absent. |
| O16-A03: one successful native open | PASS | One open on a granted existing-root restart; a new granted root produced one creation and one open. Storage selection was published and the actual bundled workspace was usable. |
| O16-A03: reload/focus/rerender/duplicate callback | PASS | Real Blindly4 reload, activation and menu rerender added no open. A separate real-WebView2 fixture posted three additional workspace-ready callbacks and still produced exactly one open for that observer launch. |
| O16-A03: bootstrap and failed startup | PASS | No event before initial unchecked Continue. A granted profile with an intentionally missing guest.js stayed on the real recovery/storage screen, displayed the missing-file error, preserved storage selection and delivered no open. |
| O16-A03: headless startup | PASS | Ordinary host without `--desktop` reached its public ready address and remained live for 14 seconds with valid grant/enrollment. No event request used that headless DeviceId. Only that owned process was terminated afterward. |
| O16-A04: first grant never backfills | PASS | Native open completed while off; later Settings grant caused enrollment but no open. Regrant and supported-notice repair likewise did not replay an already consumed open. |
| O16-A04: stale Settings revision and deliberate retry | PASS | Separate CLI writer denied the observed grant. The stale actual checkbox action showed the conflict message, preserved the external epoch and did not overwrite automatically. A subsequent deliberate UI action created a new grant epoch. |
| O16-A04: failed withdrawal / stale retry suppression | PASS | An owned read handle prevented profile replacement. UI reported failure and reporting-off; committed grant/epoch remained unchanged. Refresh continued to report sticky local-off after releasing the handle. One already-started event request received 503; no later old-epoch attempt occurred over more than 150 seconds. Explicit re-enable then deny succeeded with new epochs; denied restart remained off. |
| O16-A05: private owner/DACL through publication and restart | PASS | Native/CAS-created files have TokenUser owner, protected DACL and one explicit user FullControl ACE. Owner read succeeds; an independent restricted-token read returns Win32 Access Denied (5). Grant, deny, fresh native creation and portable profile observations preserve this property. |
| O16-A05: packaged-runtime configuration authority | PASS for the manual uninstalled layout | The same fixed executable, adjacent runtime/loader and stable manifest selected external `LOCALAPPDATA/Sapiens4/config`, not Development. Grant and restart retained the valid choice; an internal-package config override was rejected without creating a profile. No installer was built or run. |
| O16-A05: network failure leaves native use available | PASS | Cold granted launch reached a usable workspace while the owned collector fixture failed. Actual 503 occurred; the UI remained usable for Settings/failure/refresh actions. TLS/hostname validation remained enabled. |
| Real collector delivery and independent Parquet reading | PASS | Seven unique published Windows rows; accepted wire IDs equal published IDs. See below. |
| Regression suites; installer/E2E; production data/collection; paid models | NOT RUN | Explicitly excluded. No permanent test suite was added. |

The known trusted-parent limitation from 08 remains: arbitrary JavaScript already
executing in the trusted principal is not isolated from that principal. The
negative fixtures above do not relabel that limitation as solved. No broad
product-data renderer, B9 trust/fallback matrix or ticket-08 re-audit ran.

## Collector and independent reader

The actual in-repository collector ran in a separate owned macOS process with a
new private 32-byte key and new dataset. Its copied prebuilt executable and
unchanged selected collector-source hashes are recorded in
`collector-provenance.json`. An owned HTTPS tunnel/proxy forwarded synthetic
Windows enrollment/events using normal Windows certificate and hostname
validation. The proxy recorded event bodies/statuses, never Authorization or
enrollment response credentials. Outage mode generated the deliberate 503.
This verifies actual collector delivery, not production deployment readiness.

DuckDB **1.5.5**, with `hive_partitioning=false`, independently read **7 rows**:
**5 app_opened and 2 corpora_created**. Seven distinct accepted EventIds equal the
seven published EventIds. The single failed-outage EventId is absent. All rows
have Windows/native_desktop context and notice v1. All fourteen physical columns,
the app-open NULL matrix and creation request-kind NULLs were checked. Independent
PowerShell/.NET HMAC calculations matched the two native new-root public IDs
without exporting their privacy keys or raw Corpora UUIDs.

The five opens belong to: the original private-profile granted restart; the fixed
fresh granted root; the controlled duplicate-readiness launch; the stable-layout
fresh granted root; and its restart. Off-state opens, later grant/repair, failed
startup, headless startup and reload/focus did not add opens.

The first independent read saw three sealed rows while the fourth accepted event
was still in the collector's current buffer. That interim read is retained and
is not a publication failure or a final PASS. The final independent read waited
for ordinary timed publication and matched every accepted ID. No shutdown flush
or durable-202 guarantee is claimed.

## Evidence and cleanup

Sanitized evidence is retained outside Git at:

`/Users/pashvel/Desktop/personal/sapiens4-worktrees/telemetry09-windows-evidence-20261115`

Its README maps numbered development commands, exact build arguments, source and
executable hashes, actual Blindly4 targets/actions/trees, native event observations,
wire records, ACL/restricted-token results and independent Parquet output.
Probe sources and synthetic published rows are evidence; profiles, credentials,
transport tokens and private keys are removed during cleanup. No production
profile or data was used. Source/report remain in the separate clone.

Final cleanup confirms the entire owned Windows tree absent, the worker stopped,
zero remaining owned native processes/jobs, and the shared checkout/package database
present. The owned local collector, HTTPS tunnel and transfer server exited; ports
8081/18969 are closed, the transport tree/private keys are removed, and temporary UI
dependency links are removed. The final cleanup artifacts record these outcomes. No task writes were made to the original checkout
or shared Windows build; concurrent unrelated work was left in place. No push,
PR, regression run, installer, E2E or paid model call was performed.
