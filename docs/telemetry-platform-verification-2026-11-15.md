# Telemetry 06/07/08 platform verification

> Follow-up: the [Windows native gate completion](telemetry-windows-native-gate-2026-11-15.md)
> closes ticket 08 at `e2cca20b211d84e2e5cf94502a16ca966356cd46`. This document
> retains the earlier cf94 verification and its OPEN verdict as historical evidence.

Actual Windows and Linux observations, 2026-11-14 UTC / 2026-11-15 Dubai.
Required baseline: `cf94f221ec1d9cd5acac8f5ee6a5c3d7af1cbe6e`.

**B9's focused HTTPS prerequisites pass on actual Windows. Windows ticket 08
remains OPEN; do not start Windows consent wiring or declare platform readiness.**
The native adapter has a reproducible access violation when opening an existing
root with a legacy WebView profile, including with the unchanged baseline
Windows entrypoint. Its cause is unknown. Obsolete-generation and wrong-current-core
rejection were not successfully isolated. The two-line child-navigation fix is
independent of this crash and does not claim to resolve it.

## Revisions and isolation

Work was performed in the separate clone
`sapiens4-worktrees/telemetry-platform-cf94-20261114`, branch
`codex/telemetry-platform-cf94`. No obsolete amended commit was merged.
Baseline host/transport/06/07 sources were built at the required SHA. The native
frame guard was committed separately as
`ca94d6303fbe1f9ba55a0665dec9e7a1f511e4df` and exercised on Windows.
Linux remains at the baseline: the fix changes only Windows `Shell.hs`.

| Dependency | Selected revision |
| --- | --- |
| blindly4 | `cfc28cb5bcccbd58947bf13e80b79b76062b8dc9` |
| libs4 | `170c2fd6936269e3245a0246787101dc9ac9310b` |
| libs4 Haskell freeze SHA256 | `16ce5ec94a977b4747ce219c041618a8ae4fca66f861e7cf454f662792031265` |
| libs4 package-lock SHA256 | `235ced3db7901c94da21782a31e2710a846d95aa6d4577cfb185aad4c08a664d` |
| Freshly built app.js SHA256 | `918e6467dd3740018b285d2283ac530b4eab05a04dab2caf94294e401cd6e4d6` |

Both platforms used the pinned sender packages: http-client 0.7.19,
http-client-tls 0.3.6.4, network 3.2.9.0, tls 1.9.0, crypton 1.0.6,
crypton-x509-system 1.6.8, aeson 2.3.2.0, async 2.2.6,
sqlite-simple 0.4.19.0 and direct-sqlite 2.3.29. The retained Linux build plan
matches every overlapping libs4 freeze version; the Windows package inventory
records the reused package database. No package store was modified.

## Platform results

Detailed acceptance tables, commands and limitations are separate:

- [Windows evidence](telemetry-platform-windows-2026-11-15.md).
- [Linux evidence](telemetry-platform-linux-2026-11-15.md).

PASS means the specific observed boundary passed. NOT RUN is not a source-review
PASS, and a row with missing subcases does not close its entire original ticket.
The most material remaining cases are stale native generations/current cores,
bootstrap query/fragment negatives, Windows product-data rendering probes,
creation with a queue already full before root startup, and existing-child migration.

The temporary `BoundaryProbe` calls the real `HostServer.startHostWith`, actual
configured sender and actual host consent operation. Its wrapper records typed
hook **invocation attempts**, including when the sender is off or drops a full
queue. These observations establish business acceptance and hook placement;
they are not counts of successful queue insertions or deliveries. Wire and
Parquet establish the delivered events separately. Faults were injected only
into disposable SQLite/files/HTTPS fixtures. Providers returned scripted output;
no model service was called.

## Independent collector and Parquet evidence

The actual `remote/telemetry` collector was built from the baseline using its
own pinned freeze (GHC 9.10.3, Cabal 3.18.2.0, wireform-parquet 0.1.0.0).
It listened on private loopback with a synthetic private deployment key and
published its normal Parquet output. A Windows HTTPS tunnel and a Linux private
TLS proxy forwarded only synthetic traffic. Each injected one initial 503;
subsequent real collector responses were 202. Collector Authorization and
enrollment secrets were excluded from retained proxy records.

DuckDB **1.5.5**, independently of the collector's Parquet writer, read the
published files after their normal publication interval:

| Platform | Rows / distinct EventIds | Creation | agent_message | group_message | group_task_run | Installation / Device |
| --- | --- | --- | --- | --- | --- | --- |
| Linux | 7 / 7 | 2 | 2 | 1 | 2 | 1 / 1 |
| Windows | 11 / 11 | 2 | 4 | 2 | 3 | 1 / 1 |

Windows includes three independently accepted encrypted requests and one accepted
real attachment request. Sending each encrypted frame again reused its saved
response and did not capture again. This is distinct from HTTP delivery retry:
the initial Windows batch was retried byte-for-byte with SHA256
`89bda578057499bf4f5c3bdc057ac52d7a836639a5736899fbda8b1015f3ada8`;
Linux's was
`1e86dcbae82caeee9d08d3eb92d8b72c3a504ec5595618d295f8904096cad07d`.
Wire EventId sets equal the published Parquet EventId sets on each platform.

The actual schema contains only 14 columns: schema_version, event_id, event_type,
received_at, occurred_at, installation_id, device_id, corpora_id, request_kind,
app_id, app_version, os, runtime_kind and notice_version. Closed wire field sets,
synthetic content/attachment/path/provider marker absence, no raw Corpora UUIDs,
and absence of app_opened were checked. Python stdlib and PowerShell HMAC-SHA256
derivations from the actual disposable root/child identities matched the public
Corpora IDs. Private keys and raw identities were not retained.

| Published file | SHA256 |
| --- | --- |
| part-20261114T195734.829Z-94c9ef57-30dc-4943-b4dc-4d9c77c24b4f.parquet | `06d489a9f7c1a6e7496282dfa525b439294183aa39ff090bcb67bbdc6137be4d` |
| part-20261114T201456.996Z-d6cdb04e-51ea-4fb2-9b07-d716e05c7876.parquet | `3754a58083c90c5bb520c89f5828bc540f91348867b9d21b61921b01c77adf08` |
| part-20261114T201933.526Z-28bcbd16-91bb-46c6-b705-1beb5be992f7.parquet | `f7f9e1f6a642a7349e6f1874e57b3e2a26eff17091251a435fed509038e38759` |

This proves actual local publication of these synthetic rows, not durable
deployment storage or target deployment readiness.

## Retained evidence and cleanup

Sanitized temporary sources, exact PowerShell/Python commands, package inventory,
build logs, native logs, timing observations, wire bodies, published Parquet and
the independent reader are retained outside the product repository at:

`/Users/pashvel/Desktop/personal/sapiens4-worktrees/telemetry-platform-evidence-cf94-20261114`

See its `README.md` and SHA256 manifest. Windows fixture transport origins/tokens
are replaced with placeholders. Restoring those placeholders requires a new
private fixture; the old URLs are not a supported replay service. No temporary
endpoint, provider or probe was added to the product.

Owned Windows temporary clones, profiles, DataRoots and TLS certificates were
removed; the cleanup reported zero owned certificates remaining. The temporary
executor exited, observed through the Google Chrome Windows console, and its
prior queue was restored. Existing `C:\Build` installations/package stores were
preserved. The OrbStack container was left running for its existing owner;
our `/work/cf94-evidence` runtime data/TLS keys were removed, with the earlier
source/build/cache preserved. The new collector, proxy and tunnel were stopped
and its private key removed. Sanitized synthetic Parquet remains for review.

No regression suite, installer, final feature E2E, paid model call, production
collection, push, PR or ticket-status change was performed. The shared checkout
was never stashed, reset or checked out, and its pre-existing changes were preserved.
