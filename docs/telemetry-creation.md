# Corpora creation capture

Ticket 06 connects the existing shared sender to genuine root and managed-child
creation. It adds two nonblocking captures and no sender, storage or event type.

`HostServer.startHostWith` captures `ICorporaCreated` only for `StartNew` with
`managed == False`, after service initialization, HTTP readiness, execution and
transcriber startup have succeeded. Failure before that point captures nothing.
The reference comes from the opened service's canonical root, not an unchecked
input path. AttachExisting, restore and ordinary headless restart are excluded.
A genuinely new headless StartNew is a creation, as specified by the ticket's
headless demonstration; it never implies `app_opened`.

`Registry.registryCreate` captures once after the child has opened, registry JSON
has been saved and its in-memory registration installed. The child inherits the
parent's emit capability. Its managed open does not run the root capture, even
if called with StartNew. Failed validation, initialization/open or registry save,
lazy AttachChild and registry reads do not capture child creation.

Both captures pass a typed local reference to the existing emitter. Config,
identity/key IO, HMAC derivation, enrollment and delivery stay in the background
sender. Disabled capture, invalid identity, outage and a full queue preserve
successful business creation. There is no synchronous flush, backfill, creation
registry or transaction spanning business storage and telemetry. A crash between
commit and capture can lose an observation; ambiguous HTTP retries can duplicate
its stable EventId, requiring read-side deduplication as already specified.

## Development observations

Baseline: `a45810deba8e777ce7acdc4705e7547c558f7c8e` on `telemetry/d4`, with the
selected D3/libs4 revisions unchanged. Host/macOS builds use the existing
`build/` freeze. Temporary public host/process/API fixtures on macOS observed:

- One root and one managed-child capture after success; the registry entry is
  present on disk when the successful child capture is observed.
- No extra capture on root reopen, lazy child attach, registry reads, copied
  root/child restore or a managed StartNew.
- Failed root and child opens on occupied loopback ports and a failed registry
  replacement leave no corresponding creation capture.
- A missing consent record creates no BI HTTP or privacy key while root and
  child remain usable. An invalid existing telemetry key drops the event without
  changing the key field or breaking the public business identity.
- A queue filled with 128 typed synthetic intents before root open drops the
  subsequent root/child creation intents while both business operations succeed.
  A collector outage/backoff likewise does not block a subsequent child create.
- The actual collector accepts one authenticated batch containing exactly two
  creation events. Its normal publisher emits an immutable Parquet file after
  the configured buffer interval. An independent PyArrow 18.1.0 reader observes
  two `corpora_created` rows, nullable `request_kind`, one shared InstallationId
  and DeviceId, and two distinct public IDs matching independent Corpora HMACs.

The fixture uses the real sender with a private synthetic HTTPS proxy and a
test-only self-signed certificate setting. Production TLS policy is unchanged.
Typed filler intents exist only in the temporary fixture; no request hook or
debug endpoint is added to the application. No test suite is committed.

Actual Windows/Linux observations for these hooks remain open; the separate
verification session is pinned to the predecessor `a45810d` and does not by
itself verify ticket 06. Native app-opened, accepted-request and pairing hooks
belong to their later tickets; full feature E2E remains ticket 12. No production
collection or deployment was performed.
