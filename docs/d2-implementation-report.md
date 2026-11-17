# D2 implementation and qualification checkpoint

2026-11-18. Branch `feat/update`; production revision `0044ac5bb1edcb3e92b924851f9273bf0e794041`.
Base: `release/nov-16` (`666a028346323dee23ccf943b9a424b7e93db126`).
Original D2 implementation: `d279ed7eea85effa791c1780b88e645e05702cb9`.

**NO-GO for integration/public release.** This is an implementation checkpoint, not a claim that the requested installed E2Es or radical LOC reduction are complete. Full signed A → B → migration → Workspace qualification is outstanding on both platforms. The ordinary Windows account is not qualified. No release branch was merged.

## Architecture actually implemented

The desktop shell discovers a release and starts an external native helper only after an explicit update action. The helper downloads while A remains available. After download, the shell asks the backend to stop safely, obtains exclusive installation ownership and atomically publishes a pending file containing the attempt ID. A download alone creates no installation gate.

Sparkle replaces the macOS bundle; Inno replaces the Windows payload. The retained helper, request, recovery commands and application recovery copies live outside the replaceable application. `/Applications` uses a stable shared installation-lock directory; private per-attempt directories retain selected Data Root/configuration paths. The public completion record contains the attempt, version, status and later Corpora identity/root hash, not the private root path.

B owns storage conversion inside the existing backend startup. It locks the selected root and registered child Corpora, closes database connections, preserves an immutable whole-tree original, publishes `Migrating`, executes existing domain conversions and durably publishes `Migrated`. Normal workers start afterward. Actual local Workspace readiness confirms the installed attempt; backend listening or migration completion alone does not.

Storage recovery is explicit. Before touching the live tree it saves all newer B data into a fresh private snapshot. `Restoring` records that snapshot and the exact participant lock set, allowing interrupted recovery to resume even if the live registry is partially restored. `Restored` prevents replay of that same original. No timeout causes automatic rollback. No automatic deletion of previous applications or data originals is implemented.

Native copies are restricted to unpublished snapshots. A measured controller-death experiment showed that macOS rsync could outlive the lock owner and continue writing. Therefore live restoration uses a small traversal in the process holding the locks, with native per-file metadata copying. Replacing this with an external live mirror would weaken exclusion.

## Deletions and retained costs

| Module/mechanism | Result |
|---|---|
| `UpdateContract`, same-format policy, `StartupFormats`, A-side preflight | Deleted; updater no longer decides storage formats. |
| Empty SQLite future-step registry | Deleted; actual supported SQL conversions remain. |
| Migration inventory, content hashes, generic reconciliation, automatic rollback | Deleted; immutable original + explicit recovery phases replace them. |
| Per-agent `legacy-state-v1/v2.json` creation | Deleted; the whole-tree original already covers these files. Existing backups are preserved. |
| Windows custom HttpClient/stream/cancellation/disposal sequence | Replaced by `Invoke-WebRequest -UseBasicParsing -MaximumRedirection 0 -TimeoutSec 3600`; digest and publisher verification remain. |
| `DataRoot` format and path validation | Partly simplified; much remains because several existing Value-based converters otherwise tolerate malformed inputs. No claim that moving these checks would save LOC. |
| `MigrationFiles` | Hybrid implementation. Exact lock identities, ACL/privacy preservation, durable publication, long paths and read-only NTFS files required retained/added code. |
| `Installation` | Retained exclusion; atomic pending file replaces pending-directory/token staging. Shared macOS installation support added. |
| Native application recovery | Added missing behavior, rather than merely refactoring existing behavior. This is the largest net growth. |

macOS retains A and installed B archives. Explicit repair uses the validated installed B when possible; damaged payload recovery exposes the retained app through Finder, which handles replacement authorization. An interrupted native replacement requires a macOS restart before repair because Sparkle's installer can outlive its controller. The guard is tested; the actual reboot/Finder replacement journey is not yet qualified.

Windows retains exact native installers, verifies their publisher/product/version/platform, and provides explicit Repair/Previous modes. Inno caches its own installer and uses its native InstallDelete section to remove obsolete package-owned runtime files. Data Roots and configuration must be outside the installation. A preproduction installation without a retained installer must be reinstalled once before this update path is available; A remains usable when preparation fails.

## R1–R8 reconciliation

| Requirement | Implementation and present evidence |
|---|---|
| R1 download/consent/exclusion | Download-before-stop and explicit handoff implemented. Updated full installed journey not yet qualified. No automatic installation. |
| R2 shutdown/no concurrent writes | Installation leases, backend stop admission and all-tree host locks retained. macOS interruption/storage checks pass; complete installed Windows interruption remains unqualified. |
| R3 B-owned storage conversion | Existing backend owns SQL/JSON conversion; updater has no SQLite/JSON format policy. Current parent/registered-child migration and explicit recovery pass on macOS and Windows Administrator; current backend/shell builds pass on both platforms. |
| R4 skipped releases | Existing supported version transformations are reused without a speculative registry. SQL 7 → 8 and agent state 1 → 3 were exercised together on macOS. This is not proof for every arbitrary historical binary/schema. |
| R5 recoverable data | Whole originals, durable receipts and explicit preservation of newer data implemented. Process-interruption checks pass on macOS. Physical power loss is NOT TESTED. |
| R6 failed update/recovery | Explicit application/data recovery implemented. macOS valid-B repair and stale/same-boot rejection pass. Windows native installer A → B → A component scenario passes. Failed signed-helper Repair/retry and damaged macOS payload replacement are NOT TESTED. |
| R7 real readiness | Local Corpora ID plus navigation and actual snapshot required. Historical macOS Workspace Ready with retained chat/HTML and a custom root passes; the latest storage preparation still needs installed UI qualification. Windows installed UI Ready is NOT TESTED. |
| R8 previous version/originals | A archives/installers and originals retained; no automatic cleanup. Installed artifacts and every retained installer were not yet exercised as complete application recovery E2Es. |

A manually built application and a custom private `.sapiens` Data Root are separate concerns: migration follows the selected root when B opens it, regardless of how B was installed. A source checkout updated by `git pull` does not itself enable a packaged Sparkle/Inno update channel. Compatibility is bounded by supported stored formats; the user's specific first-build commit has not been identified or qualified.

## Measured production LOC

Same method as the original 1,745 measurement: nonblank added/deleted diff lines against the fixed base, in `app`, `corpora`, `environments`, `build`, `remote`, and `sapiens-domain.cabal`. Documentation, temporary qualification scripts, generated output, SDKs and submodules are excluded. The existing Remote changes contribute 21 net lines to both columns; this work did not change those modules.

| | Original implementation | Current implementation |
|---|---:|---:|
| Added nonblank production lines | 2,042 | 2,303 |
| Deleted nonblank production lines | 297 | 297 |
| Net above base | **1,745** | **2,006** |

The current feature is **261 net nonblank lines larger (15%)**, not smaller. The revision-2 estimate of about 1,400 is not achieved. These are feature-diff counts, not the entire repository's line count. Lines replaced in-place are not credited as independent architectural savings.

At `b2752fe` the count was 2,025. The subsequent deletion of duplicate per-agent backups and custom HTTP plumbing removed 19 net nonblank lines. Earlier major net changes versus the original implementation included MigrationBackup −44, SQLiteSchema −16, DataRoot −10, but native macOS recovery +133, MigrationFiles +75, Windows helper +64 and Inno +42. These are before the last 19-line deletion and explain the direction, not additional savings to subtract again.

The lower estimate depended on native live mirroring being safe and on smaller recovery/permissions obligations. The live-mirror assumption failed experimentally. Filling application recovery, shared `/Applications` support and actual readiness cost more than the removed policies. Further reduction must remove demonstrably redundant mechanisms; introducing a new storage-version protocol solely to shorten validation has not been implemented.

## Executed checks and retained evidence

Temporary helpers remain outside the repository. `spec/native-mirror/qualify.py` is not production code and was removed from that repository location. No new test framework was committed.

| Check | Result / limits |
|---|---|
| macOS GHC backend and shell build | PASS after latest Haskell deletion. Host dependency/deployment warnings mean macOS 13 runtime compatibility is not established. |
| Windows GHC backend and shell build | PASS including long-path/read-only fixes, exit 0. Last platform-independent deletion was built on macOS; it has not been rebuilt on Windows. |
| UI build | PASS after local-Corpora readiness change. |
| Current Objective-C helper | PASS `clang -Wall -Werror`, link and execution. |
| PowerShell parser | PASS for latest helper; this alone is not execution qualification. |
| Windows built-in HTTPS download | PASS byte-for-byte comparison of the downloaded NuGet archive; redirect request refused with MaximumRedirection 0. Signed updater workflow NOT TESTED. |
| macOS migration + newer-data restore | PASS: parent/child, SQL/state versions, normal HTTP state, original/newer bytes, unchanged locks, replay refusal. |
| macOS kill during migration/restore | PASS: startup blocked for parent/direct child, explicit retry with damaged registry, preserved original/newer bytes. |
| Unpublished recovery preparation interruption | PASS fresh snapshot on retry after new work; does not independently prove every possible orphan-copier timing. |
| `/Applications` real B Workspace | PASS selected custom root, migrated child, retained chat/HTML, Ready, private request/public header separation. Ad-hoc qualification app, not a production release. |
| macOS current Repair with valid B | PASS archive creation, pending cleared, actual local Workspace Ready. Same-boot interrupted-installer and stale-helper guards PASS. Actual reboot and Finder replacement NOT TESTED. |
| First root created by B | PASS installed backend initialization of a hidden custom `.sapiens`, attempt-bound original before normal runtime. Onboarding UI NOT TESTED. |
| Windows actual backend data recovery | PASS current source `223196d`, Administrator: default and `--readonly` diagnostic both exit 0; parent/child migration, original/newer bytes, explicit restore, exact lock identities, replay refusal. This diagnostic does not establish API/UI readiness or signed installation. |
| Inno 6.4.3 compile + native A → B → A | PASS isolated installer component fixture: both installers cached, obsolete B runtime removed, external sentinel preserved. Payload was a non-launched fixture, not the actual application. |
| Ordinary Windows account | NOT TESTED. D2Qualification exists; user stated no password was entered. S4U task registration failed AccessDenied. No known valid login established. |
| Full installed updated A → B → migration → Ready | NOT TESTED on either platform. Previous Sparkle PoCs do not qualify this changed implementation. |

Evidence paths (local scratch is retained, not committed):

- Latest macOS storage: `/private/var/folders/97/80rddxm51750bvdn9k9fnv2w0000gn/T/d2-storage-current-3ja3mb8y`.
- macOS interruption: `/private/var/folders/97/80rddxm51750bvdn9k9fnv2w0000gn/T/d2-interruption-current-glnodd_d`.
- Failed native-controller live-copy experiment: `/private/var/folders/97/80rddxm51750bvdn9k9fnv2w0000gn/T/d2-orphan-copy-0xm85ytg/result.json`.
- Unpublished preparation: `/private/var/folders/97/80rddxm51750bvdn9k9fnv2w0000gn/T/d2-unpublished-recovery-kwnji5n2/result.json`.
- macOS real UI and current helper: `/private/var/folders/97/80rddxm51750bvdn9k9fnv2w0000gn/T/d2-ui-readiness-9in26ib1/readiness-result.json` and `repair-current-result.json`.
- First hidden root: `/private/var/folders/97/80rddxm51750bvdn9k9fnv2w0000gn/T/d2-first-root-9twq2emw/result.json`.
- Current Windows storage: `C:\Users\Administrator\AppData\Local\Temp\1\d2-current-storage-sxfi0r7l\result.json`; readonly: `d2-current-storage-srf6lx2o\result.json` under the same parent. Current build: `C:\Build\d2-native\build-223196d.log`.
- Windows Inno: `C:\Build\d2-native\inno-current\inno-qualification-b76c1047d161458c8cf30e6776d23688\result.json`.

During initial macOS UI setup, a test app was inadvertently launched without the isolated environment and opened the default user Data Root. It was stopped; the readiness binding correctly refused the wrong root. No claim is made that this default root was untouched. User data/backups were not deleted. Subsequent recorded UI checks used the verified isolated custom root.

## Quality and outstanding work

The storage phase ADT and the complementary navigation/Workspace signals make important invalid transitions explicit. Existing domain functions own transformations; installation helpers remain storage-agnostic. Resource ownership is clearer than before, especially the live-copy/lock lifetime. No daemon, separate migration executable, trial runtime or generalized migration framework was introduced.

The result still has substantial hand-written path/ACL validation and two platform-specific recovery implementations. DataRoot continues to duplicate knowledge of loosely typed domain formats. That is an unresolved maintainability cost, not a completed radical simplification.

Release gates still required: complete installed success and interrupted-recovery journeys on both platforms; actual ordinary Windows-user execution; signed Windows retained-installer Repair failure/retry; macOS damaged-payload Finder/reboot recovery; packaging/deployment qualification and supported path checks with final artifacts. CurrentUser code-signing certificate discovery on the Windows stand returned no certificate. No trust bypass or password from the chat was used.

Existing committed work is reviewable. Integration/public release remain NO-GO until these gaps are closed. The implementation task and requested reduction are not declared complete by this checkpoint.
