# Native telemetry controls and workspace opens

Ticket 09 adds the shared onboarding checkbox, native Settings toggle and
`app_opened` hook for macOS and Windows. The existing ticket-08 trusted-bundle
and product-data boundary remains binding. Windows runtime verification is
still required; implemented source is not a platform PASS.

## Explicit choice and private host channel

Onboarding starts unchecked and retains the existing storage screen and Continue
action. Settings reads the app-global choice and offers one toggle and a static,
synthetic JSON example. Missing or invalid nested consent displays off. An
insecure, corrupt or unsupported whole profile cannot be repaired by the UI.
The adjacent copy stays on one short line; a collapsed disclosure holds the
full notice and, in Settings, the static example.
The reviewed wording is "Help improve Sapiens4" with "Share usage statistics.
No messages or files."; the disclosure is labeled "Details".
The preview notice describes pseudonymous identifiers, best-effort delivery,
retained received history and the possible already-authorized in-flight request.
It explicitly leaves retention/erasure policy and real-user notice approval open.

`TelemetryControl` accepts exactly a dedicated read action or a write containing
choice, notice version 1 and the expected canonical epoch/null. Extra fields,
mixed generic actions, credentials, paths and caller authority flags are rejected.
The native adapters choose the context; JavaScript cannot select bootstrap
authority or provide an owned process handle.

Before a host exists, a verified bundled bootstrap action uses the shared profile
CAS writer. With a child host running, both native adapters use its existing
private stdin/stdout pipes. A missing workspace channel returns an error; it
never falls back to an independent writer. The macOS arbitrary-URL `--fixture`
entrypoint cannot read or mutate the local consent profile through this action.

The host's desktop stdin loop recognizes only the bounded telemetry command
envelope and the native opened notification. Ordinary `stop`, unknown input and
EOF retain their shutdown meaning. Headless startup does not run this loop.
Stdout responses have a distinct prefix and a request number, so normal startup
output is not a reply. Native readers continue draining stdout. One outstanding
request waits up to five seconds; late replies cannot satisfy a different request.
The small pipe writes are serialized independently from the response wait.

The host delegates writes to its actual `hostConsent`, preserving disable/purge
before CAS and the sticky local disabled state after failed publication. Its safe
read reports that local state too. Conflicts return the current choice/epoch and
require another deliberate action. No conflict or failure retries a choice
automatically. A channel failure reports an unconfirmed change rather than
claiming a successful withdrawal.

Only consent state/notice/epoch, a local-disabled flag and fixed result codes reach
JavaScript. InstallationId, DeviceId, Basic credentials, identity keys, filesystem
diagnostics and live event history do not. There is no new HTTP mutation route,
event intake endpoint, IPC service or dependency.

## One native open per application lifetime

Each native application retains one consumed flag across focus, reload and host
recovery. The existing storage-readiness join must observe both successful
navigation and workspace readiness and commit the selected storage path first.
Only then does native code consume the flag and notify the owned host. The host
also ignores duplicate opened notifications within its own lifetime.

Consuming the flag while reporting is off does not generate an event or retain an
open for later grant. Regrant does not repeat readiness. The existing sender
captures the current consent epoch and materializes the same closed wire catalog;
the change introduces no fifth event, final flush or persistent event spool.
Bootstrap, failed startup, headless boot, focus and UI rerender are not open hooks.

## Focused macOS observations, 2026-11-15

Actual macOS 26.6, GHC 9.10.3, the selected `build/` freeze and a freshly compiled
PureScript bundle were used. Native interaction used Blindly4, temporary private
profiles/DataRoots, a provider executable that always exits without invoking a
model, and the actual collector behind an owned localhost TLS fixture. Certificate
and hostname validation remained enabled through an explicit temporary test CA.

| Observation | Result |
| --- | --- |
| Host/macOS builds and UI build | PASS; no PureScript warnings; existing unrelated GHC warnings remain. |
| Clean onboarding initially unchecked | PASS through native accessibility; no consent profile/DeviceId existed. |
| Actual checkbox plus Continue | PASS; explicit private grant, new root and usable workspace. The fixture's explicit new path first exercises existing missing-root recovery, without a telemetry open. |
| Unchecked Continue | PASS; usable new root with no telemetry profile, privacy-key preparation, enrollment or event HTTP. |
| Settings grant/deny | PASS through actual checkbox clicks and persisted private state. |
| Stale Settings epoch versus a separate CLI writer | PASS; visible conflict, current epoch returned, no automatic overwrite, deliberate subsequent choice succeeds. |
| Failed withdrawal | PASS; an immutable disposable profile prevents publication, UI reports failure, committed grant remains unchanged, sender stays locally disabled; refresh and an explicit retry work after repair. |
| Restart | PASS; valid choice persists and a new successful launch adds one open, without another creation. |
| Reload and focus | PASS; existing launch does not produce another open. |
| Cold native launch during collector outage | PASS; workspace remains usable across all three actual 503 attempts. |
| Invalid nested notice | PASS; refreshed Settings displays off; explicit supported-v1 grant repairs the record, changes epoch and preserves IDs/enrollment. No new approved notice version is invented. |
| Native grant after an off open | PASS; automatic enrollment is observed, with no backfilled open. |
| Static JSON illustration | PASS after expanding the real disclosure control; native accessibility text contains the synthetic example and no actual private identifiers/password. |
| Local HTTP and encrypted Remote Control | PASS; eight consent-shaped local POST/PUT requests return 404, encrypted consent path returns 404, host path remains remotely prohibited, and no caller creates a consent profile. |
| Arbitrary matching-origin macOS fixture page | Reproduced against the initial 09 binding, then fixed: it created a disposable grant before the guard and cannot create one afterward. The fixture entrypoint now returns only an unavailable result. |
| Host pipe behavior | PASS; safe read/write replies, stale CAS, publication failure/sticky-off, pending retry drop, no backfill, duplicate-open suppression and ordinary stop/unknown-input shutdown observed through the real host. |

DuckDB 1.5.5 independently read four actual published Parquet rows: one synthetic
host-pipe open, one genuinely new native root and two native opens on separate
application launches. There are four distinct EventIds; accepted wire IDs equal
published IDs. All fourteen physical columns, the app-open NULL matrix and the
new root's public HMAC were checked independently. Outage attempts are not
counted as published rows. This is local synthetic publication, not deployment
readiness or durable acknowledgment of a collector 202.

The UI observations above used real accessibility actions, not injected JavaScript
or DOM assertions. Separate adversarial caller pages are negative security probes,
not substitutes for operator interaction. Ticket-08 view/frame/source/current
checks remain in place; its earlier complete context matrix is not relabeled as
a new post-wiring run. Repeat that matrix, including guest/frame and stale-current
cases, during remaining platform/feature verification.

The initial runner could not expose its window while Chrome remained fullscreen.
The symptom also occurred with the older native executable. Early debugger-only
window-presentation trials were not counted as UI acceptance. Exiting Chrome
fullscreen allowed fresh uninstrumented launches and normal Blindly4 interaction;
the original Chrome mode is restored after the probes. No presentation workaround
was added to the product.

## Remaining verification

Actual Windows WebView2 build/runtime, packaged configuration authority and
grant/deny/restart/conflict/failed-withdrawal/open-negative cases remain OPEN.
Run them against the committed implementation; ticket-08 evidence alone does not
close the new consent-write behavior. Full desktop/migration E2E remains 12;
policy approval and production collection remain separate release gates.

Sanitized focused commands, native accessibility observations, wire records and
independent Parquet observations are retained outside the repository under
`sapiens4-worktrees/telemetry09-macos-c4_g38rn`. No permanent per-ticket suite,
installer check, paid model call or production collection was added or run.
