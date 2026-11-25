# LMDB storage through Append4

Sapiens embeds Append4's Haskell transaction API over vendored LMDB 0.9.36.
There is no storage service or storage timer. The host's shared 120 BPM clock
(500 ms) schedules application workers, filesystem observation and telemetry;
LMDB commits start as soon as writes are submitted.

A Corpora owns corpora.lmdb. API operations and execution transitions stage
changes inside the existing service guard. Exceptions discard staged changes.
LMDB commits synchronously before success or provider dispatch. Audio buffers
are removed after commit; interrupted cancellations finish cleanup on restart.
Paused Corpora can still commit administrative changes.

One LMDB environment contains state, receipts, expiry, audit and metadata named
databases. These namespaces share atomic commits. Agents, turns, attachments,
settings, directory relationships, pairing and registry data remain accessible
through the same KV API. Prefix reads seek into the B-tree. Only transaction
changes are staged in Haskell memory; current state and permanent history are
read from disk on demand. local/ keys remain device-bound.

Corpora retain mutation history on disk. Each accepted transaction has one
logical version; separate reversing transactions remain distinct. NoChange
rejects an exact logical round trip inside one transaction. The indexed duplicate
ledger replaces scans of past transactions. Full snapshots and history are explicit
reads, not startup or per-commit work. Disk retention of Corpora history remains
unbounded; this change does not add replication or an audit archival policy.

The relay owns relay.lmdb with CurrentOnly history policy. Expired and acknowledged
message payloads are deleted from current state without being copied into an
append-only audit journal. An expiry index supports bounded cleanup (256 index
entries per pass), on requests and once per minute. Delivery and authentication
check expiration even while cleanup has a backlog. Room-prefixed message keys
avoid scanning other rooms. LMDB reuses released pages; deletion does not promise
secure byte erasure or immediate shrinking of the database file. Address mappings
and explicit request-ID ledgers have their own lifetimes.

The map grows from 16 MiB up to a configurable 64 GiB ceiling. Growth waits for
readers and retries prepared writes only, without rerunning application actions.
Use short transactions and local filesystems. Writes stay on one bound OS thread;
read data is copied before transaction release. Default synchronous LMDB durability
is enabled. The vendored engine includes its original copyright and license notices.

Bootstrap/installer files, workspace documents, media and generated provider
artifacts remain files. Backups stop writers and copy corpora.lmdb with the
owned data tree, excluding disposable LMDB and Append4 lock files. Managed
children are discovered through a read-only offline lookup that does not
change the database or create lock files. It requires stopped writers or an
immutable backup.

App/data upgrades are versioned in `migrations/`. The previous dev layout is
`v00.01.99`; this release is `v00.02.03`. SQLite/JSON Corpora and relay data are
converted automatically before workers start, through a verified staging LMDB.
Original data files are retained. Committed migration steps and their version
markers are atomic, and future steps run sequentially. See
[the migration contract](../migrations/README.md) for coverage and recovery rules.
Unreleased custom `.append4` journals still require a separately registered reader.
A persisted history policy must match when reopening a store.

External effects still require reconciliation after an uncertain interruption;
afterCommit is not a durable job queue. Remote incomplete receipts prevent blind
replay. Low-level checks cover commit/cancellation/restart behavior, capacity
failures, indexed reads and transient page reuse. See the Append4 README and
docs/testing.md for commands.

## Complete typed keys (v00.02.03)

`Append4.KV` accepts one complete text key and one binary typed value: null,
boolean, text, exact decimal (integer coefficient and exponent), empty object or
empty array. For example, `agents/<id>/name` maps directly to a text value.
Nonempty records have no stored root, document ID, field-path column or JSON blob.
Keys escape literal slash/tilde in field names; `~iN` identifies array positions.
An empty object/array marker preserves the distinction from null and absence.

Application record serializers assemble domain/API values in memory from prefix
reads. They atomically diff scalar keys on write and remove obsolete keys.
The storage engine does not accept documents. All new audit, receipt, expiry and
metadata values also use complete scalar keys. Long keys over LMDB's limit use a
collision-checked full-key overflow index; ordinary keys are stored directly.

The versioned migration retains the previous database, converts every current
value and replayable historical state, and archives the exact original journal
as scalar keys in `prior-history`. This archive preserves original conditions,
no-op mutations and transaction identities. New audit entries operate directly
on scalar keys. Original append IDs and the duplicate-ID ledger are retained.
See `migrations/README.md` for the sequential conversion and recovery contract.
