# Local telemetry consent

The local operator command and future trusted native UI use the same
`TelemetryProfile` authority. Ticket 03 adds private consent persistence;
it does not start a sender, perform HTTP enrollment or emit events.
Native consent adapters belong to tickets 08–09.

## Operator commands

Build/run from `build/`, using the existing desktop dependency freeze:

```sh
cd build
export SAPIENS_CONFIG_DIR=/absolute/path/to/deployment-a/config
cabal run sapiens4 -- telemetry status
cabal run sapiens4 -- telemetry grant 1 none
```

`status` returns only the consent record, including its revision:

```json
{"consent":null}
```

After a successful grant, use the returned `consent_epoch` for every subsequent
action:

```sh
cabal run sapiens4 -- telemetry deny EXPECTED_EPOCH
cabal run sapiens4 -- telemetry grant 1 EXPECTED_EPOCH
```

`none` is accepted only when no valid consent epoch exists. A stale revision
returns `conflicting_consent_revision` and a failing exit status; refresh and
make a deliberate new choice. Errors contain fixed codes, not paths,
credentials or parser diagnostics. Version 1 is the current development
notice version; this does not approve a notice or authorize real-user collection.

Headless commands require an explicit, nonempty `SAPIENS_CONFIG_DIR`.
The home directory, shared default application configuration root and its
ancestors are rejected. Each deployment/operator must choose its own private
root; filesystem permissions cannot discover two deployments intentionally
configured with the same directory under the same OS account. A Corpora root,
its descendants and an explicitly configured data root are rejected.

For an explicit local desktop operation, put `--desktop` before the command:

```sh
cabal run sapiens4 -- telemetry --desktop status
```

This resolves the existing `RuntimePaths.configDirectory` authority, including
its normal OS-account desktop default. Reading missing storage creates no
profile, IDs or directory. There is no HTTP consent-write route.

## Profile behavior

`<config_root>/telemetry.json` contains `format_version`, `consent`, `device_id`
and `enrollment`. Optional fields may be absent or null. IDs are canonical
UUIDv4; the password is validated as canonical `v1.` plus base64url of 32 bytes.
Reads are bounded to 64 KiB; larger files fail closed as invalid profiles.
Credentials have private constructors and no `Show` or `ToJSON` instance.
Only a private profile encoder serializes the secret.

| Stored input | Result |
| --- | --- |
| Missing profile or consent | Off; first explicit grant may use `none`; denial is a profile no-op. |
| Valid consent and epoch | Use the stored choice; writes require its matching epoch. |
| Invalid nested consent in an otherwise supported valid profile | Off; explicit grant with `none` repairs consent and preserves valid IDs/credentials; denial leaves the file unchanged. |
| Corrupt, unsupported, unreadable or insecure whole profile | Error and off; no implicit overwrite, including with `none`. Deliberate local repair/reset is required. |

Every mutation acquires the same persistent `telemetry.lock`, re-reads the
committed profile, checks the expected revision and replaces the file using a
closed temporary file in the same directory. First grant creates DeviceId and
an epoch. Effective grant/deny or supported granted-notice changes mint a new
epoch. Matched identical writes, enrollment and restart do not. Withdrawal
retains DeviceId and enrollment; it does not delete history.

`commitEnrollmentIfEmpty` is the shared locked persistence operation for the
later sender. The caller performs HTTP outside the lock. An enrollment result
is installed only under the same still-granted epoch and unchanged resolved
configuration root. If a competing result already won, return that stored
pair after the same check. Results from an older grant are discarded.

POSIX directories/files are created as `0700`/`0600`; existing permissions are
checked rather than silently changed. Symlinks, nonregular files, extra hard
links and wrong ownership are rejected. On macOS, extended ACLs are currently
unsupported and fail closed, even when mode bits look private. Repair an ACL
deliberately; the application does not strip it. Windows uses the `Win32`
package and direct Haskell FFI for owner/DACL checks and private creation;
unknown ACE forms fail closed. Both platforms share the same consent logic.
No new C source is used.

Ticket 04 moves these filesystem primitives into shared `TelemetryStorage` and
uses the same consent lock to authorize background Corpora identity preparation.
See [private Corpora telemetry identity](telemetry-identity.md) for its lifecycle,
file-access guards and migration limitations.

Publication promises atomic visibility, not power-loss durability. A failed
write/close/rename leaves the preceding committed profile and returns an error;
the caller must remain locally disabled until reconciliation. Temporary files
are cleaned on handled failure; a process crash may leave an ignored private
temporary file. No secret is stored in the lock file.

## Verification and remaining platform gate

Selected integration baseline: deliberately chosen unmerged D3 candidate
`ee3586ef00691b818ec404327e590743499da3b8`, with libs4
`170c2fd6936269e3245a0246787101dc9ac9310b`; implementation follows collector
commit `b3196ce3705384fb652be4ddbd7a07edb9c40811` on `telemetry/d4`.

Development checks use temporary synthetic files and process/API probes outside
the repository, without a new committed test suite. Final feature E2E remains
ticket 12. Observations on macOS arm64:

- Host build and focused compiler checks for the new Haskell modules.
- Four parsing/recovery cases, strict IDs/passwords, stale and idempotent actions,
  restart/withdrawal identity retention and separate-root isolation.
- Concurrent first grants, a writer waiting on the actual process lock,
  locked re-read/CAS, cancelled waiter and complete JSON snapshots during writes.
- Racing enrollment commits, winner preservation and stale results after
  withdrawal/regrant, including a still-empty enrollment record.
- Private modes, unsafe links/FIFO/directories, unreadable files, unwritable
  parent, file-size-limit write failure and immutable-destination rename failure;
  prior committed bytes remain unchanged and temporary files are removed.
- A macOS ACL reproduction first accepted an unsafe ACL with `0700`/`0600`;
  after the fix it rejects that profile and accepts it after deliberate repair.

Actual Windows observations on 2026-11-14 used Windows Server 2022 Datacenter
x64 (10.0.20348), GHC 9.14.1 and cabal-install 3.18.2.0. The implementation
revision was `6ee25abf09f613dc4b327b8a17a8068568abe1aa`, based on ticket 03's
`f660beda181911575e8c9466a70290bd94f046b8`. All data and credentials were
temporary and synthetic; no collector or HTTP enrollment was contacted.

The original revision rejected an elevated Administrator's first grant:
`openBinaryTempFile` assigned the Administrators group as owner even though its
DACL allowed only TokenUser. Windows temporary files now use `CREATE_NEW` with
the same explicit TokenUser owner and protected private DACL as the directory
and lock. The existing owner/DACL checks remain strict. The closed file is
reopened for binary writing inside the private parent; creation and handle
acquisition remain masked and handled failures clean up the new path.

Those observations predate the ticket 04 `TelemetryStorage` extraction and the
sender work in `6aafc29`. The adaptation retains the verified Windows helper
and selects it in `TelemetryStorage.atomicWritePrivate`, shared by consent and
Corpora identity publication. It preserves `beforePublish`, privacy checks,
locking, cleanup and the `.telemetry-writing-` prefix. Results for the earlier
revision do not establish verification of the adapted shared path.

The isolated adaptation based on `6aafc29` compiles on macOS with the selected
dependency freeze. Focused synthetic process/file observations pass for grant,
shared identity publication and independent HMAC comparison, preserved unknown
fields/private modes, failed `beforePublish` rollback and temporary cleanup,
denial and stale-epoch rejection. The POSIX temporary-file operation is
unchanged. The adapted Windows consent/identity publication still requires a
fresh actual-platform observation; no regression suite is implied by these
focused checks.

Native Windows compilation and manual process/file/API observations covered:

- The real `TelemetryCommand.run` fixture and the full Haskell host entry point
  `corpora/Main.hs` (40 modules), compiled with GHC against the existing package
  database and static sodium library without changing that dependency store.
- First grant, owner/DACL checks for directory/lock/open temporary/final files,
  CAS, locked re-read, four-case recovery, identity/enrollment retention,
  configuration isolation and output redaction under both elevated Administrator
  and a temporary standard user: 44 completed observations per principal.
- Eleven additional observations per principal covered symbolic links, unsafe
  lock ACLs, competing initial grants, stale enrollment after withdrawal,
  denied-notice parsing, empty configuration and private-data cleanup.
- Actual access denial to a different principal for all four storage objects,
  including the final file after replacement. This checks ordinary access; it
  does not claim protection against an administrator exercising ACL-bypass
  privileges.
- Two concurrent writers, and independent readers during 30 replacements per
  principal: 179 complete JSON snapshots, no invalid/partial JSON or read/write
  errors. Replacement retained the private owner/DACL.
- Lock sharing failures, denied temporary creation and a destination held open
  without `FILE_SHARE_DELETE`: fixed errors, unchanged committed bytes and no
  leftover temporary files.
- A temporary, process-local fault-injection DLL used only by the fixture
  intercepted the actual CRT `_write` and `_close` of the temporary stream.
  Injected disk-full/write and close errors returned `profile_io_error`, kept the
  prior committed hash and left no temporary file. The close probe released the
  real descriptor before reporting its injected error. A separate injected
  `CloseHandle` failure covered the initial private-file creation handle.
  These are injected API failures, not physical disk or power-loss tests; no
  fault hooks or new C files are part of the product.
- The full host started with corrupt telemetry JSON and unsafe telemetry ACLs;
  its local `/api/host` returned HTTP 200. Lock and replacement failures did not
  disrupt it, and both host runs shut down with exit 0. A copy of an existing
  compiled `app.js` supplied the startup asset: this was a host/core check, not
  a UI rebuild or GUI behavior check.

The Windows installer/native GUI package and final feature E2E were not built
or run by these observations. No permanent test suite was added and no
regression suite was run. GUI consent wiring remains tickets 08–09 and final
E2E remains ticket 12. These manual observations do not establish power-loss
durability or authorize production collection/deployment.
