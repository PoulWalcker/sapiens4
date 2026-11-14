# Private Corpora telemetry identity

Ticket 04 prepares a migration-stable public identifier. It adds no event hooks,
sender, HTTP enrollment, consent UI or new operator command. The future sender
calls `TelemetryIdentity.preparePublicCorporaId` in the background and drops an
event when identity preparation fails; business acceptance must not call it.

## Identity and consent

The existing `corpora-identity.json` keeps version 1, its `corpora_id` UUID and
unknown fields. After a matching local grant, an absent
`telemetry_privacy_key_v1` gains 32 OS-random bytes, encoded as canonical unpadded
base64url. A present invalid value, including null, is an error: it is never
replaced automatically. Missing, corrupt, oversized or insecure identity storage
also fails telemetry preparation without repairing the business identity.

The parser uses private `LocalCorporaId` and `CorporaPrivacyKey` types. Neither
the key nor the parsed identity has a `Show` or `ToJSON` instance. The public
interface returns only an opaque `PublicCorporaId` or a fixed error constructor.
JSON objects remain at the persistence boundary so unknown fields survive.

For key `K` and canonical 16-byte UUID `U` in network byte order:

```text
M = UTF8("sapiens4.telemetry.corpora.v1\0") || U
PublicCorporaId = BASE64URL_NO_PAD(HMAC_SHA256(K, M))
```

The NUL is one byte. The resulting identifier has 43 characters. DeviceId,
InstallationId, credentials, raw UUID text and consent epoch do not participate
in the HMAC.

The synthetic comparison vector uses UUID
`550e8400-e29b-41d4-a716-446655440000` and key bytes `00` through `1f`.
Its encoded key is `AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8`.
The independently calculated public ID is
`qMv-QcdrCEzzmH3c6Z5iKliyFkj1mGtT4Y0Qca1lI4U`.

Preparation locks the configuration profile, re-reads its consent and requires
the captured granted epoch before touching Corpora storage. It then locks
`<corpora_root>/.telemetry-identity.lock` and re-reads the identity. Competing
initializers, including ones using separate configuration roots, reuse the one
committed key. Lock order is profile then identity; no network work runs inside
these locks. Withdrawal waits for an ongoing preparation to finish and prevents
later preparation under its old epoch. The sender must still check its own
consent generation before enqueue/send; that work belongs to ticket 05.

`TelemetryStorage` shares the private-file primitives previously implemented in
`TelemetryProfile`: bounded reads, owner/permission checks, exclusive locking
and closed same-directory temporary files followed by atomic replacement.
Publication rechecks the resolved Corpora root and granted epoch. Handled write,
close or rename failure leaves the previous committed identity and removes the
temporary file. This promises atomic visibility, not power-loss durability.
Existing unsafe permissions are rejected rather than silently repaired. See
[the profile storage rules](telemetry-consent.md#profile-behavior).

## Privacy boundaries

The selected revision's `Population.corporaIdentity` selects public metadata
instead of serializing the identity object. Service snapshots and Remote Control
use that projection. Registry creation writes only a new UUID/version; it does
not copy private identity into registry metadata. The inspected paths contain
no general Corpora backup/export endpoint; the existing legacy agent-state
backup copies agent state, not this identity file.

The workspace previously allowed opening or attaching the raw identity file,
which made its contents readable through local HTTP and encrypted Remote
Control. Those paths now reject private files before preview/attachment use,
including saved tabs and symlink aliases resolved at read time. Saved group
attachments are filtered before constructing a model prompt. Ordinary document
preview remains available. Native browser navigation uses the same predicate;
macOS also checks its custom file-serving path. Linux calls that Haskell
predicate from the existing browser C source, without duplicating the policy.

`corpora-identity.json`, `telemetry.json` and `.telemetry-writing-*` are reserved
basenames, wherever opened. The predicate checks both the given and resolved
path; Windows additionally normalizes trailing dots/spaces and alternate-stream
suffixes. File URL decoding preserves Unicode and rejects invalid UTF-8.
Business JSON readers replace parser diagnostics for these private files with
fixed messages: malformed JSON must not echo input fragments into HTTP or
startup logs. The ordinary public Corpora UUID remains ordinary app metadata;
only its derived HMAC belongs in telemetry payloads.

These guards protect application dataflows. They do not provide a filesystem
sandbox against an operator or a process running as the same OS user.

## Migration, backup and key loss

An intentional migration or backup must preserve UUID and key together, with
private permissions and trusted storage/transport. The complete identity file
is secret-bearing; do not attach it to messages, expose it to controlling
clients or include it in public diagnostic exports. Destination deployment
consent and credentials stay separate and start off until its operator grants.
Changing the destination DeviceId or InstallationId does not change this
Corpora's public identifier.

Copying UUID and key to two active clones makes them share a public identifier.
Restoring the same pair preserves it. Losing or replacing the key changes it.
Losing the entire field is indistinguishable from a legacy identity and may
initialize a new key under a current applicable grant. There is no registry,
historical key recovery, rotation system or clone detection. Secure backup
policy and full runtime migration verification remain separate work.

## Development evidence and open platform gates

Integration baseline is `ee3586ef00691b818ec404327e590743499da3b8` with libs4
`170c2fd6936269e3245a0246787101dc9ac9310b`; this change follows
`f660beda181911575e8c9466a70290bd94f046b8` on `telemetry/d4`.
Temporary synthetic process, file and API probes on macOS arm64 observed:

- Host and macOS builds; focused new-module compilation with warnings as errors
  except the existing `RuntimePaths` name-shadowing warning.
- Exact HMAC against an independent Python standard-library vector; unknown
  field preservation, private modes, restart stability and copied UUID+key under
  a separate granted profile; independence from InstallationId.
- No Corpora initialization without matching grant; invalid stored keys remain
  unchanged; eight processes with separate granted profiles select one key.
  Actual lock contention serializes key publication and withdrawal.
- File-size-limit write failure, unwritable parent, immutable destination and
  insecure modes leave prior bytes unchanged and clean handled temporary files.
- Real host HTTP snapshots, identity projection, file previews and attachments;
  private opens are rejected, including old tabs and changed symlink targets,
  while ordinary files remain usable. An invalid telemetry key does not prevent
  ordinary business identity/health requests.
- Existing relay/peer fixtures perform real encrypted pairing and requests:
  public identity/snapshot replies omit the key and private file operations fail.
- A reproduced malformed-JSON leak is fixed for HTTP responses and restart logs;
  diagnostics contain no synthetic secret fragment.

No new test suite is committed. These observations do not close ticket 03's
Windows private-storage gate. Actual Windows and Linux native builds/runtime
and their privacy checks remain unverified here; macOS native navigation was
built but not exercised through the GUI. Full migration and feature E2E remain
ticket 12. No production collection or deployment was performed.
