# App migrations

`v00.01.99` names the previous `dev` application (commit `7ae4825`):
Corpora SQLite schema 8, agent JSON schema 3, JSON configuration and archives,
SQLite remote receipts and relay mailboxes. `v00.02.00` is the LMDB release.
`VERSION` carries the public version; Cabal represents it numerically (currently `0.2.3`).
Installer UUIDs and per-document schema versions are different identifiers.

## Permanent rules

Every future persistent app/data transformation belongs in a new file here.
Do not add upgrade code to startup, API handlers, models or installers. Runtime
crash reconciliation and initialization of genuinely new records are not upgrades.
Keep released migration files and readers: never rewrite history to require users
to install every intermediate application release.

1. Add `V00_02_01.hs` (for example), implementing its one declared source/target
   transition, including changed settings, files, indexes and compatibility rules.
   Freeze its source/target constants; never define an old step’s target in terms
   of the moving `currentVersion`.
2. Register the transition in `Migrations`' sequential runner (transaction steps or an explicit format-publication step). Supply
   Corpora and relay actions as needed; an unchanged scope still advances its
   marker through an explicit no-op action. Update `MigrationVersion`, `VERSION`
   and the Cabal version together.
3. `MigrationPlan` validates the complete route before executing anything. Missing
   steps, ambiguous paths and downgrades fail closed. Steps run in version order,
   regardless of their registration order.
4. Each step's database changes and `local/app-version` commit in the same LMDB
   transaction. An exception leaves that step unapplied; restart resumes from the
   last committed version. Never run providers or network effects in migrations.
5. Any filesystem transformation must retain its inputs and use deterministic,
   idempotent preparation plus atomic publication. A database transaction cannot
   roll back filesystem IO. Add failure/restart tests for each such transformation.
6. Add fixtures from the previous released format, assertions about customer-visible
   records, and tests for skip-version upgrade, repeat execution, interruption,
   malformed input, unknown versions and rollback. All migration modules must be
   listed in the host and desktop Cabal components so installed builds include them.
   Existing unversioned PR91 LMDB is always interpreted as v00.02.00, even in
   later releases; fresh stores start at the current version.

## v00.01.99 -> v00.02.00

`V00_01_99.hs` reads SQLite copies, including WAL/rollback journals. It never opens
customer SQLite for writing. `V00_02_00.hs` owns all field/key conversions:
agents and state, historical turns and details, messages, attachments, workloads,
groups, routines, pulses/events, settings, directory, registry and pairing secrets,
remote receipts, archives, voice progress, note documents and relay state/indexes.
Unchanged workspaces, media and provider artifacts remain in place. Unknown fields
on imported documents are retained. Old receipt hashes retain their frozen
compatibility reader in `LegacyReceipt.hs`.

The host holds `host.lock` before conversion, before any service workers start.
Each managed Corpora goes through the same path when opened. Backup inspection
understands both old JSON registries and new LMDB registries, so unopened children
are still locked and backed up. Stop the old host/relay before upgrading.

The first storage conversion builds `corpora.lmdb.migrating` or
`relay.lmdb.migrating`. Original SQLite databases, JSON files and media are retained.
The converted file is closed, reopened and compared before same-volume publication.
A source fingerprint prevents reusing a completed staging file after the old app
has changed its inputs. Failed import never publishes the destination. Existing
PR91 LMDB without a version marker can be adopted only when no legacy database
competes with it. Mixed unversioned stores require explicit recovery, never guessing.

Old explicit relay `--db` paths ending in `.sqlite3`, `.sqlite` or `.db` are mapped
to a sibling `.lmdb`; the default discovers `relay.sqlite3`. Relay imports preserve
the AUTOINCREMENT high-water mark and rebuild expiry indexes under `CurrentOnly`.
Corpora retains history. No conversion discards acknowledged-vs-uncertain receipts.

Do not launch the old app against retained SQLite after switching to LMDB: it
cannot understand a newer app marker and would write a separate, stale data set.
Use the installer's full-tree backup/rollback to return to the old version.
Old original files are deliberately not deleted automatically.

Unreleased custom `.append4` journal files and formats predating the named baseline
are not guessed or silently converted. They remain intact and are rejected with an
explicit error. They need their own registered migration before being supported.

## Checks

From `build/`: `cabal build exe:sapiens-migration-fixture`.
Then run `python3 spec/migrations/check.py /absolute/path/to/sapiens-migration-fixture`
from the repository root. The suite uses a frozen copy of the actual dev SQL schema.

### Previous desktop launcher selection

`V00_02_00Launcher.hs` reads the v00.01.99 profile's `config.json` data
location when no modern selection exists. Explicit and modern saved selections
have priority. Missing or invalid old workspaces fail visibly instead of starting
fresh. Discovery retains the old config; the normal workspace-ready publication
writes `config/storage.json` only after successful opening. Desktop setup selects
detected content even when current terms still require acceptance.

## v00.02.00 -> v00.02.01

`V00_02_01.hs` removes `system.started` and `system.stopped` entries whose actor is
`system` and entity is `corpora` from every stored `events/` list. Other activity,
unknown fields, row IDs and order are retained. Startup and shutdown no longer
create these entries. Relay data is unchanged and explicitly advances its marker.
Malformed event lists abort the entire step; retry starts from the previous
committed version. The storage engine's historical transaction journal and retained
legacy source files are unchanged.

## v00.02.01 -> v00.02.02

`V00_02_02.hs` replaces JSON values in all LMDB namespaces with Append4's flat
binary cells. `v00.02.01` remains an explicit prerequisite, including its original
lifecycle-event cleanup. Earlier releases traverse both steps in order.

The migration preserves all current logical values, unknown/nested fields,
empty containers, nulls, arbitrary-size exact numbers, long Unicode field names,
receipts, expiry indexes, metadata, duplicate-ID ledgers and the complete ordered
mutation history. Historical append IDs, transaction IDs, conditions and deletions
are unchanged. One final audit commit records the v00.02.02 marker.

Conversion builds `<store>.v00.02.02.staging`, verifies each namespace and value,
then records the target marker. The closed original is retained as
`<store>.v00.02.01`; only the verified stage becomes the active file. A crash
between renames restores the old filename and repeats verification. Incomplete
stages are rebuilt. Malformed history, unexpected versions, map exhaustion or a
conflicting backup refuse publication. The caller holds the app migration lock
and excludes writers throughout. Do not run an older app on the retained backup.

Run `spec/migrations/flat.py FIXTURE LEGACY_WRITER` for typed round trips, original
history preservation, publication recovery, malformed-history rejection and relay
coverage. Build `Legacy.c` against the vendored LMDB for the frozen input writer.
Network/provider payloads and external integration documents still use their
specified JSON formats; they are not the LMDB representation.

`V00_02_02Files.hs` converts active storage selection, terms acceptance, telemetry
settings, Windows client preferences and Corpora identity files to a binary list
of flat path/atom pairs. Original settings files are retained with `.v00.02.01` suffixes;
the identity uses an authoritative `.typed` sidecar so the frozen SQLite source
fingerprint remains unchanged.
existing filenames remain compatible with discovery and installer contracts.
Each file is validated, backed up, atomically replaced and can be retried. Current
writers emit typed bytes; frozen JSON readers are retained only for older input.
macOS native preferences already use the platform plist format. Relay connection
exports, computer-provider snapshots and `host-control.json` are external JSON
interchange documents, as are HTTP/LLM/provider responses and installer manifests.

## v00.02.02 -> v00.02.03

`V00_02_03.hs` owns the transition to complete-key typed KV. The sequential runner
first finishes the frozen .00, .01 and .02 steps as necessary, closes that store,
then invokes .03 before opening the runtime `Append4.KV` store. A persisted newer
version is rejected before writes. All previously released migration readers stay
available. New runtime code uses `AppStorage` only for in-memory domain assembly.

The conversion writes `<store>.v00.02.03.staging`, retaining the exact closed
source as `<store>.v00.02.02`. It verifies every scalar, reconstruction of every
original record, every historical replay, and the exact original history archive
before committing the .03 marker and publishing. Duplicate or ambiguous complete
keys fail closed. A crash between renames restores the source and rebuilds the
stage. Existing conflicting backups are never overwritten. The .03 publication
lock spans the earlier steps and the final transition.

Each namespace now maps a complete key directly to a typed value. There are no
`.cells`/`.paths` tables or nonempty document roots. `prior-history` retains the
original journal as complete scalar keys; active `audit` replays scalar mutations
with the original append and transaction IDs. The version change adds one commit.
CurrentOnly relay stores remain CurrentOnly and preserve receipts and expiry.

`V00_02_03Files.hs` migrates the .02 settings/identity files to complete key/value
pairs, retaining the .02 bytes beside them. Inputs from earlier releases first
pass through the frozen .02 file step. Customer workspace and media files are
unchanged. Run `spec/migrations/single.py FIXTURE`; the older `check.py` and
`flat.py` continue exercising their frozen transitions through explicit fixture
commands, so later releases cannot silently change their target versions.
