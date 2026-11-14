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

Actual Windows build/runtime, effective ACL access by another principal and
Windows concurrent locking/replacement/failure observations have **not** run.
They remain ticket 03's B4 completion gate. A macOS check does not close those
criteria. No production collection/deployment is performed by these checks.
