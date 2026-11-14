# Telemetry collector

An isolated Haskell executable implementing stateless enrollment and the
ticket 02 volatile-ingest/immutable-Parquet collector. It does not change the
desktop or libs4 dependency solver.

## Run locally

From this directory:

    cabal build telemetry
    cabal run telemetry -- /absolute/private/server-key.bin /absolute/dataset

The key must be an operator-owned private regular file containing exactly 32
bytes. With only the key argument, the dataset defaults to ./telemetry-data.
The service binds to 127.0.0.1:8081. TLS termination, public IP/proxy rate
enforcement, encrypted storage, target-platform prerequisites and operational
approval remain deployment gates in ticket 11.

The native snappy and LZ4 library paths on this Mac are in the ignored
cabal.project.local. Dependency versions remain pinned by cabal.project.freeze.

## Endpoints and limits

- POST /v1/enroll: strict protocol_version=1 request, at most 1024 bytes;
  returns server-issued UUIDv4 and a stateless HMAC-derived Basic password.
- POST /v1/events: one Basic Authorization header and application/json.
  Accepts all four approved event shapes with strict typed context, UUIDs,
  public IDs, supported notice version 1, and UTC millisecond timestamps.
  Unknown field names return 400; invalid values/event scopes return 422.
- GET /healthz: 200 only while the publisher is healthy and the queue has
  capacity. Writer failure or a full queue returns 503.
- All JSON responses have Cache-Control: no-store. Errors use fixed envelopes;
  request input and exception text are not reflected or logged.

Event requests are bounded to 64 KiB, including chunked input. Each original
serialized event object is bounded to 1024 bytes, including its internal
whitespace and escapes. There must be 1–20 events. Compression and duplicate
JSON keys are rejected. Malformed JSON nesting is bounded before building
Value. Reading a request body has an independent five-second deadline.
Warp's connection inactivity timeout is ten seconds, so the body deadline can
produce the fixed 400 envelope before the connection timeout fires.

A fixed-memory global budget permits 100 enrollment/event requests per
monotonic second and returns 429 with Retry-After: 1 when exhausted.
It is separate from authentication and does not maintain an IP registry.

## Admission and publication

One STM transaction checks publisher health and writes the whole batch into
the 4096-row TBQueue. If any write cannot fit, orElse rolls back all writes
and returns 503 immediately. A 202 response has exactly:

    {"status":"accepted_volatile","accepted":2}

It acknowledges RAM admission. Pending data can be lost on failure/restart;
an ambiguous retry may create duplicate event IDs. There is no dedup registry,
disk spool or replay promise.

Rows are materialized with strict fields before entering the queue. The
writer buffer is limited to 4096 rows and an 8 MiB conservative input estimate
(2048 bytes per validated row). Encoded output is limited to 16 MiB before
writing. This bounds inputs/outputs, not total RTS/native peak RSS.

A partial buffer seals after one minute, earlier on row/input-size limits or
a change of UTC receipt date. The deadline remains attached to the first
buffered row while new rows arrive. The selected one-minute interval is below
the contract's five-minute maximum.

One process owns the dataset through a held POSIX writer lock. Files go to:

    schema=v1/events/received_date=YYYY-MM-DD/part-<utc>-<uuid>.parquet

The partition comes from server received_at. Out-of-order earlier-date rows
create another unique file in that earlier partition. The exact fourteen
physical columns are in contract order, with UTF8 strings, real optional
corpora_id/request_kind columns and TIMESTAMP(MILLIS,isAdjustedToUTC=true).
Compression is explicitly Uncompressed.

Encoding completes before a hidden same-partition temporary file is written.
The file is closed before atomic rename to a fresh checked final name.
Failures remove the unfinished temporary file where possible, mark readiness
unhealthy, and reject further admission. Existing published history is never
deleted or rewritten. Restart ignores stale hidden temporary files.
Atomic visibility is provided; power-loss durability is not claimed.

Auth.hs owns credentials, Protocol.hs owns typed validation and row mapping,
Parquet.hs owns queue/writer/publication, and Main.hs owns HTTP and lifecycle.

## Local verification: 2026-11-14, macOS arm64

Verified on Darwin arm64, GHC 9.10.3, Cabal 3.18.2.0, local filesystem.
Work was performed on telemetry/d4 with HEAD
493b2f612a72c0c7f935b5a1a9bcf47fc7688651 plus the working-tree implementation.
The deliberately selected D3 candidate is
ee3586ef00691b818ec404327e590743499da3b8; libs4 remains at
170c2fd6936269e3245a0246787101dc9ac9310b.

Manual synthetic process/HTTP/GHCi/file probes were run outside the repository.
No permanent per-function/ticket test suite was added; feature E2E automation
remains owned by ticket 12.

| Observation | Result |
| --- | --- |
| Build with -Wall -Werror; git diff --check | Passed |
| HTTP envelopes, Basic failures/duplicates, all shapes, schema, null rules, compression, known/chunked lengths, 1024/1025-byte event boundary, rate budget | 186 response checks passed |
| Default partial-buffer publication | Observed at 60.006 seconds |
| Independent DuckDB metadata/value reading and restart/ownership checks | 103 assertions; 11 admitted rows; exactly 14 physical columns, both UTC millis annotations, correct NULL matrix |
| Near-full queue: two occupied slots of three, incoming batch of two | Rejected; both prior rows and length unchanged |
| Exact remaining capacity, full/unhealthy readiness, concurrent competing batches | Passed; all-or-none admission |
| Row/input count boundary and following partial buffer | Files with 4096 and 2 rows |
| Receipt-day sequence Nov 12, Nov 13, Nov 12; client occurred_at in 2020 | Three immutable files in correct UTC receipt partitions |
| Second writer, old credentials after restart, stale temporary file, volatile pending loss | Observed; published hashes preserved |
| Real permission failure and child-process file-size quota failure | HTTP readiness and subsequent valid ingestion become 503; prior file hash unchanged; unfinished temps cleaned |
| Injected encoder/close/publication failures at the IO boundary | Admission closes; failed close/rename leaves no published or temporary part |
| Unexpected application exception | Fixed no-store 500 envelope without diagnostic text |
| Final strict-row rollback, independent final-file reader, excessive nesting | Passed |
| Slow-body deadline | Fixed no-store 400 observed at 5.004 seconds; service remained ready |

DuckDB was read with TimeZone='UTC' and hive_partitioning=false when verifying
the physical column list. Hive readers may otherwise add received_date as a
virtual partition column; the files themselves contain fourteen columns.

Actual server OS/filesystem/native-library verification, encryption-at-rest,
public TLS/IP controls, target peak-RSS evidence and Windows results have not
been inferred from this Mac run. They remain the explicit target/platform
gates in tickets 11–12.

This is an in-repository executable, not a Hackage distribution. cabal check
still reports the scaffold's missing distribution metadata (license,
synopsis/description, category and maintainer); Hackage packaging is outside
the collector acceptance criteria.
