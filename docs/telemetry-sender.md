# Shared background telemetry sender

Ticket 05 implements delivery infrastructure. Production event hooks remain in
tickets 06, 07, 09 and 10. There is no debug ingestion endpoint, new consent
route, persistent spool, final shutdown flush or extra event type.

Ticket 06 subsequently connects [root and child creation](telemetry-creation.md)
to this shared sender; the remaining event hooks retain their original tickets.

## Ownership and configuration

`HostServer.startHost` owns one telemetry queue, worker and reusable TLS manager.
Managed child hosts receive the same `Emit` function through `startHostWith` and
`Service.openService`; they create no telemetry worker or manager. Startup
failure cancels the owner. Shutdown disables capture, drops queued RAM and
cancels network/backoff work before closing the host and its children. Resource
acquisition uses exception cleanup, including bridge, registry and HTTP startup.

`SAPIENS_TELEMETRY_ENDPOINT` selects an HTTPS origin, for example a private
synthetic development collector. It accepts no user info, query, fragment or
path prefix. No production endpoint is invented: absent/invalid configuration,
unsupported OS or an unsafe profile location provides a disabled emitter while
ordinary application work remains available. Consent writes still use the
private profile authority independently of endpoint availability.

TLS explicitly permits only 1.3/1.2, with certificate validation and server-name
indication. The pinned TLS library also enables older versions by default, so
its unmodified defaults are insufficient for the telemetry contract. These
direct imports use dependencies already present in the selected libs4 freeze.

The app version comes from Cabal's generated package metadata. Runtime is
`native_desktop` for the desktop-managed host and Linux native entrypoint;
ordinary standalone host use is `headless`. Profile scope follows that runtime:
headless requires its explicit deployment-private configuration root. See
[consent storage](telemetry-consent.md) and [Corpora identity](telemetry-identity.md).

The responsibilities are deliberately small at the callers:

- `Telemetry` contains typed local intents, request kinds and the `Emit`
  capability. `CorporaRef` validates an absolute host-local path up to 4096 UTF-8
  bytes without filesystem IO. It has no JSON encoder.
- `TelemetryWire` contains materialized events and the exact batch encoder.
  Corpora events require an opaque public HMAC identifier; local paths and raw
  Corpora UUIDs cannot be encoded through it.
- `TelemetrySender` owns capture, profile refresh, enrollment, materialization,
  batching and HTTP. Callers receive no manager, queue or credentials.

## Capture and authorization

Cached off returns before UUID/time generation. Granted capture creates one
EventId and UTC timestamp and attempts an STM queue insertion. It never reads
profile/identity files, derives a key or performs HTTP. It checks that the cached
authority still matches before inserting, so a concurrent credential change
cannot insert an old capture after a purge. Overflow drops the newest intent;
a private saturating Word64 counter records overflow without adding telemetry.
An OS IO failure during capture drops the event instead of failing business work.

Each queued intent retains its captured ConsentEpoch. The worker reads the
authoritative private profile on wake, before enrollment/key work, while
collecting and immediately before each HTTP attempt. Denied, invalid, unsupported
or insecure profiles disable capture and purge queued work. A changed epoch
drops old work even if an intermediate denial was missed. Credential removal or
replacement also purges old queued/pending work. Initial enrollment may preserve
intents captured under its still-current grant. Events are never restamped.

Enrollment posts exactly `{"protocol_version":1}` without Authorization. A
successful response must have protocol version 1 and exactly the two canonical
credential fields. HTTP runs outside the profile lock. The existing
`commitEnrollmentIfEmpty` CAS commits only under the original current grant and
safe unchanged configuration root. A loser uses the currently authorized
persisted winner; uncommitted credentials never authorize an event POST.

Corpora identity preparation runs only in the worker and has a one-second
budget, shortened to the remaining collection window when applicable. A failed
or timed-out identity drops only that candidate. `IAppOpened` cannot produce a
wire event in a headless runtime. Each pending batch fixes one consent epoch,
notice, DeviceId, enrollment principal/password and encoded byte sequence.

`hostConsent` is the same-process operator adapter for the existing private
profile writer, not an HTTP endpoint. It disables capture and purges RAM before
persistence. Failure returns the profile error and keeps that process disabled
for the observed epoch. An explicit successful local retry or another process's
later consent generation reconciles it. The same worker refreshes an idle
profile within its normal cadence; no consent watcher is added.

## Finite delivery policy

| Resource or operation | Selected bound |
| --- | --- |
| Queued intents | 128, drop newest |
| Active batch | 1–20 events, each at most 1024 encoded bytes, total at most 64 KiB |
| Collection cadence / idle profile refresh | 10 seconds, using monotonic deadlines |
| Identity preparation | At most 1 second per candidate |
| Connection/TLS handshake and response headers | 2 seconds |
| Entire HTTP operation, including response body | 5 seconds |
| Response body | 4096 bytes; no decompression |
| Retry-After input | At most 64 bytes, seconds or HTTP date, delay capped at 30 seconds |
| Attempts | First attempt plus at most two retries; approximately 1/3 seconds plus bounded jitter |

The single worker holds one pending batch alongside the queue. Network retries
can delay queued work beyond the collection cadence; this is no end-to-end
delivery deadline. Already-overdue collection flushes after bounded processing
rather than opening another ten-second window.

Only `202` with the exact `accepted_volatile` status and matching event count is
accepted as an acknowledgment. It means RAM intake, not durable Parquet output.
Malformed or unexpected 2xx replies use the bounded retry policy instead of
inventing success. Transient transport failures, 429 and 5xx retry with identical
bytes/EventIds after authorization rechecks; exhausted attempts drop the batch.
Permanent payload errors drop it without retry. Redirects are never followed,
cookies are disabled and library retries of stale connections are disabled so
the worker owns the attempt limit.

`401` disables the rejected credential record and purges work. Regrant with the
same credentials does not silently re-enroll. A deliberate local enrollment
reset/replacement may supply a new principal while preserving DeviceId; this
ticket adds no reset product interface. The rejection is process-local, not a
new persisted authentication protocol; restart may encounter 401 again but
cannot silently replace persisted credentials.

The final profile check and network send are not atomic. A request already past
that check may start or arrive after withdrawal, including after regrant. No
instantaneous global cutoff or retrospective cancellation is claimed. Stop
cancels its worker instead of waiting for configured HTTP/backoff timeouts or
attempting a synchronous final flush.

## Development observations and remaining gates

This change follows `730868a` on `telemetry/d4`, retaining the selected unmerged
D3 baseline `ee3586ef00691b818ec404327e590743499da3b8` and libs4
`170c2fd6936269e3245a0246787101dc9ac9310b`. Host and macOS builds use the existing
`build/` dependency freeze. Temporary synthetic process/HTTPS/file fixtures on
macOS arm64 observed:

- Typed event variants, canonical UUID/time/HMAC fields, wire/count limits and
  absence of local paths, raw Corpora UUIDs and consent epochs from requests.
- Off-state silence, automatic enrollment, concurrent enrollment winner CAS,
  stale E1 enrollment after E2/E3 and no uncommitted credential use.
- Full queue with a blocked HTTP request: capture returns promptly and retains
  exactly the pending batch plus 128 queued intents, dropping newer candidates.
- Stable retry bytes, exhausted/accepted retries, malformed acknowledgments,
  permanent failures, redirects, oversized replies, a 30-second Retry-After cap,
  two-second connection/header and five-second total response timeouts.
- Missed intermediate denial, same-epoch principal replacement, corrupt or
  unsupported profiles, failed denial publication, later external regrant,
  401 without implicit re-enrollment, and explicit reset retaining DeviceId.
- A send paused after its last check arrives after committed denial; subsequent
  capture stays off. Stop interrupts body reads, TLS handshake and backoff.
- A contended identity lock times out without blocking capture or publishing a
  key later; other candidates survive and later new captures can prepare it.
- The actual collector first rejects the incorrect development enrollment
  shape; after correction, real enrollment and authenticated complete-batch
  `202` pass. This does not claim Parquet durability for that response.
- The normal host creates a managed child, serves both, restarts and closes
  cleanly through the existing process/HTTP fixture with synthetic providers.
- Occupied-port startup failure closes an active telemetry connection and leaves
  no retry worker while the caller continues running. Its actual ClientHello
  advertises only TLS 1.3/1.2.

No test suite or production hook is committed here. A temporary TLS fixture
injects settings for its self-signed development certificate; the application
uses normal certificate validation and has no flag to disable it.
Actual Windows/Linux execution, native consent adapters and full feature E2E
remain open in their owning tickets. The earlier Windows private-storage gate
is not closed by these macOS observations. No production collection or
deployment was performed.
