# Linux headless telemetry verification

See the [verification index](telemetry-platform-verification-2026-11-15.md) for
revisions, independent Parquet evidence and interpretation of captures.
Windows native ticket 08 is N/A here. These results do not close Windows B9 or
claim target deployment readiness.

## Runtime and commands

Actual Docker container `sapiens05-platform-5938fqr` in OrbStack; Debian 12
bookworm, aarch64, kernel `7.0.14-orbstack-00374-gbbca68e8d741`.
GHC 9.14.1, Cabal 3.16.1.0, GCC 12.2.0, libsodium-dev 1.0.18-1+deb12u1.
Fresh source `/work/cf94-source` is exactly
`cf94f221ec1d9cd5acac8f5ee6a5c3d7af1cbe6e`, with the index's gitlink pins.
The existing `/work/source`, `/work/build` and package cache were preserved.

`/work/cf94.project` selected a separate build directory and imported
`/work/client.freeze`, the existing platform-adapted freeze. Its package versions
match every overlapping libs4 pin; the complete plan/freeze are retained.

```sh
export PATH=/opt/ghc/9.14.1/bin:$PATH CABAL_DIR=/work/cabal
cabal build --offline --project-file=/work/cf94.project --builddir=/work/cf94-build exe:sapiens4 -j2 --disable-tests --disable-benchmarks
cabal build --offline --project-file=/work/cf94.project --builddir=/work/cf94-build exe:sapiens-peer -j2 --disable-tests --disable-benchmarks
```

Both built successfully. The actual host executable was
`/work/cf94-build/build/aarch64-linux/ghc-9.14.1/sapiens-domain-0.2.0.0/x/sapiens4/noopt/build/sapiens4/sapiens4`.
The temporary BoundaryProbe/RulesProvider/DirectHTTPS executables were linked
against this same package selection. Focused commands were `python3
/work/cf94-evidence/linux-{boundaries,failures,replay,https,isolation,overflow,
schedules,provenance,key-startup}.py`, plus the final direct HTTPS and allkinds
follow-ups. Exact files, build logs and commands are in the external `linux/`
evidence directory; none is a committed regression suite.

## Ticket 06

| Criterion / case | Status | Actual observation |
| --- | --- | --- |
| O08-A05 new root StartNew | PASS | Exactly one root hook after successful actual open. |
| Existing boot / AttachExisting / reopen | PASS | Zero creation hooks; registry reads add none. |
| Root/child restore and lazy attach | PASS | Copied stopped root and child state reopens without creation hooks. |
| Failed root/managed opening | PASS | Occupied actual listener port causes startup_failed with empty captures. |
| O13-A01 new managed child / failed registry persistence | PASS | One successful registered child hook; registry target replaced with directory causes 500 and no child hook. |
| O13-A02 no duplicate root / same sender | PASS | Explicit managed StartNew emits no root hook. One passed Emit, one actual sender/enrollment and shared wire/Parquet installation/device. |
| O08-A06 / O13-A04 actual rows and HMAC | PASS | Two published creation rows, own public root/child HMAC IDs, one InstallationId/DeviceId. Independent Python HMAC agrees. |
| Off/corrupt profile/outage ordinary creation | PASS | Actual root and managed child stay usable; off/corrupt profile produce zero observed BI HTTP and no privacy-key preparation. |
| Full queue child creation | PASS | Worker blocked in HTTPS body, 300 fillers vs queue bound 128; child plus all three kinds accepted in 181.75 ms. |
| Root creation with queue already full / child migration | NOT RUN | Root creation precedes queue filling; migration not independently performed. |

## Ticket 07

| Criterion / case | Status | Actual observation |
| --- | --- | --- |
| O10-A01 external turn and accepted clarification | PASS | One per committed input; wrong/duplicate clarification rejected; actual specialist Execution adds no delegated external capture. |
| O11-A01 group message | PASS | One after normal successful external group save. |
| O12-A01 / A04 task run | PASS | Two independent accepted runs add two; pending 409 adds zero. |
| Required persistence failures, all three kinds | PASS | message_inputs/groups SQLite insert triggers produce HTTP 500 before capture. |
| Later failure retains acceptance | PASS | Optional agent projection/group dispatch failure returns 500 after the accepted hook, which remains observed. |
| O10-A02 validation/routine exclusions | PASS | Empty/flow/attachment rejection and rejected clarification add none; routine acknowledgments add none. |
| Server provenance | PASS | Client actor/origin/agencyRun/external flags fail; actual provider GroupControl and GroupTaskRun execute without external group/task hooks. |
| O11-A03 / O12-A02 schedules, worker and settlement | PASS | Actual minute scheduler executes agent/group routines, run_count 1 each; zero hooks. Worker/settlement/task edit/create/revision rejection add none. |
| Actual encrypted replay, all three kinds | PASS | Real Haskell peer pairs through actual relay; first frame accepts once, repeated same encrypted frame returns saved response without new hook. |
| Off/corrupt profile/invalid key, all three kinds | PASS | Business accepts agent/group/task; no observed BI HTTP; invalid identity bytes remain unchanged. |
| Outage, all three kinds | PASS | Root + child + agent/group/task accept in 317.56 ms. |
| Overflow, all three kinds | PASS | Full-queue follow-up accepts child + agent/group/task in 181.75 ms while server body remains active. |
| O10-A03 EventIds and HTTP retry | PASS | Seven published rows have seven unique IDs; injected 503/202 batch SHA and IDs unchanged. |
| Published Parquet privacy | PASS for synthetic message/task data | Seven actual rows independently read, closed schema; no message/path/raw Corpora UUID/provider fields or markers. |
| Valid stored-attachment privacy on Linux | NOT RUN | Actual valid attachment was exercised on Windows; Linux invalid-attachment rejection is not substituted for it. |
| Missing provider/computer setup negatives on Linux | NOT RUN | Windows exercised them; Linux validation cases above do not cover these separately. |
| Exhaustive persistence/input failures | NOT RUN | Focused commit boundaries only. |

Evidence suffixes: `linux-boundary-results.json`, `linux-failure-results.json`,
`linux-replay-results.json`, `linux-provenance-results.json`,
`linux-schedule-results.json`, and `linux-*-allkinds-results.json`.
The replay script's final host-consent-route assertion encountered a fixture
KeyError after all three replay observations and ordinary consent-route 404 had
completed. That additional host-route status is NOT RUN on Linux; Windows has
the actual encrypted host-route rejection observation.

## Shared HTTPS and deployment configuration

The direct probe used real `TelemetryHTTP`, normal validation, generated
localhost test certificates and an explicit `SSL_CERT_FILE` test CA. The final
direct-only repeat retained all four observations, avoiding an earlier fixture
variable reset which had overwritten the direct rows when starting the wire
phase. TLS 1.3 was observed on Linux.

| Case | Status | Observation |
| --- | --- | --- |
| Valid localhost certificate | PASS | 89.811 ms; TCP/TLS/HTTP observed, connection closed. |
| Untrusted certificate | PASS | 6.910 ms; TCP 1, TLS failure 1, no HTTP. |
| Wrong hostname/IP | PASS | 5.392 ms; TCP 1, TLS failure 1, no HTTP. |
| Slow response body whole deadline | PASS | 5002.069 ms after successful TLS/HTTP. Immediate server closed count was still 0; this row alone is not connection-release evidence. |
| Stop cancels body and releases connection | PASS | Actual sender stopped in 25.388 ms; server subsequently recorded TCP 1 / closed 1. |
| Explicit private deployment config | PASS | Per-fixture private config used. Actual headless telemetry status rejects absent, empty and home-root configuration with insecure_profile/exit 1. |
| Off-state silence | PASS | Off and corrupt-profile allkinds fixtures each recorded zero BI HTTP, while root/child and agent/group/task accepted. |
| Headless app_opened exclusion | PASS | Explicit typed app attempt yields no app_opened in actual wire or published Parquet. |
| Windows WebView2/native authority | N/A | No Linux result is used as Windows evidence. |

Sources/results: `linux-direct-final.py`, `linux-direct-final-results.json`,
`linux-isolation-results.json`, `linux-key-startup-results.json` and allkinds
follow-ups. Temporary TLS keys/runtime profiles were removed after archiving
sanitized observations; the existing OrbStack container and caches were preserved.
