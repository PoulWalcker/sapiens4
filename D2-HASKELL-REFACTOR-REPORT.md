# D2 Haskell refactor

Branch `refactor/d2-haskell-quality`. Baseline `36f657435a053268d770002c3fff369de07f5225`, verified against the remote PR head before creating the worktree. Final implementation `929b458104d2fa495d2c4c956f16b03223f03e08`. This report is committed separately after that revision. [PR #77](https://github.com/aleski-green/sapiens4/pull/77)

## Findings and decisions

| Finding | Decision |
| --- | --- |
| F06 | Kept opaque UUID-backed immutable versions and attempt IDs, a checked SHA-256 type, and named Data and Config paths. Reused the canonical UUID parser for state, argv, installed paths, uninstall enumeration, and registered child directories. Generic temporary and preference nonces now come directly from the UUID library. |
| F07 | Kept typed coordinator intent, shared target and ticket checks, and pure commit and failure dispatch. State reads stay in their original lease and exception scopes. The Config claim reuses one owner comparison across the preflight and the locked reread. |
| F08 | Kept separate host request/reply, coordinator control and milestone, and desktop notice types. The host codecs share one spelling per value. Both desktop adapters name their handoff effects. The UI decodes native notifications before constructing an action. |
| F10 | Kept named inventories and a pure child-first removal list. Copy, metadata, verification, and settlement keep their established order and run with the original writer locks. Browser cache-parent preparation retains its original timing. |
| F22 | Kept a three-part numeric version, typed release feed, explicit selection errors, and pure platform-asset selection. The raw tag spelling still determines the exact expected download URL. |
| F03, F04, F11, F20, F21 | Reviewed. Native wait, join, installer and stop compensation belong to the correctness branch. Rejected shared aliases for the native ownership Boolean because they would imply stronger termination evidence than the current implementation supplies. |
| F01, F02, F05, F09, F12–F19 | Deferred functional changes. These cover Busy and Workspace readiness, admission, path exclusions, telemetry, registry work, native hashing, preferences, timeouts, uninstall ownership, and snapshot durability. F09 and F19 can affect recovery behavior and need native qualification with their respective fixes. |

The refactor preserves format-1 state keys and values, canonical UUID text, the two-element reservation array, backup slots `0` and `1`, and ordered inventory encoding including Boolean entry kinds. Existing user-data schemas carry through verbatim.

## Types and invariants

| Type | Purpose and guarantee |
| --- | --- |
| `Version`, `UpdateAttemptId` | Distinct opaque UUID values. A version cannot select a backup directory, and an attempt cannot select an immutable executable. `Previous` and `PendingUpdate` now encode both roles. |
| `Sha256` | One checked lowercase 64-digit representation for release payloads, backups, and file fingerprints. Consumers accept only a parsed or computed digest. |
| `DataReservation` | Names the installation and attempt binding in memory and gives the read and write operations an explicit value with the existing JSON array representation. |
| `UpdateRoots` | Names Data and Config at the update, snapshot, and recovery call sites. Both fields remain `FilePath`, with path identity and disjointness checked by the existing native operations. |
| `EntryKind`, `TreeEntry`, `Entry` | Replace Boolean file/directory tags and positional inventory fields. Tree inspection can describe a path and kind before hashing. |
| `Inventory`, `BackupInventory` | Inventory has named root, locks, exclusions, and entries. A backup has exactly one Data and one Config inventory. Restore no longer depends on a truncating zip to associate live trees with numbered slots. |
| `ReleaseVersion` | Exactly three nonnegative components, ordered numerically. |
| `ReleaseAsset`, `ReleaseFeed`, `Release`, `ReleaseError` | Feed and assets have domain names after JSON decode. Only a newer matching asset with the exact URL and a checked digest becomes a `Release`. Selection exposes distinct outcomes for malformed version, current version, missing asset, wrong origin, and malformed digest. |
| `HostCommand`, `HostReply` | Close the sets of correlated host commands and replies. The uncorrelated stop line stays outside the request type. The sender selects the long preparation wait by constructor. |
| `UpdateHandoff` | Names status, host calls, preference flush, and checked stop at both shell call sites. Three distinct `IO ()` actions no longer depend on their position. |
| `UpdateNotice`, `AppUpdateNotice` | Close the native status vocabulary and the UI action input. Unknown wire values are ignored before dispatch, and the callback cannot construct the UI's internal Downloading state. |
| `CoordinatorIntent`, `CoordinatorMessage`, `Milestone` | A staged attempt always supplies its target and roots. Recovery takes no staged arguments. The process exchange can send Handoff or Commit and await only the three supported milestones. Failure remains a separate signal. |

## Before and after

| Operation | Baseline | Refactor |
| --- | --- | --- |
| Installed identifier | `Version { versionName :: String }` | Opaque `Version` backed by `UUID` |
| Backup path | `FilePath -> Version -> FilePath` | `FilePath -> UpdateAttemptId -> FilePath` |
| Reservation | `FilePath -> FilePath -> Version -> IO ()` | `FilePath -> DataReservation -> IO ()` |
| Payload verification | `String -> FilePath -> IO ()` | `Sha256 -> FilePath -> IO ()` |
| Version parsing | `String -> IO [Integer]` | `Text -> Either ReleaseError ReleaseVersion` |
| Installed release stamp | `FilePath -> Version -> IO [Integer]` | `FilePath -> Version -> IO ReleaseVersion` |
| Release discovery | `Manager -> FilePath -> Version -> IO Release` with policy inline | Same interface for fetch and decode, with pure `selectRelease :: ReleaseVersion -> String -> ReleaseFeed -> Either ReleaseError Release` |
| Update offer | `updateAvailable :: IO Bool` | `updateOffer :: IO UpdateNotice` |
| Prepare/activate | `(String -> IO String) -> IO ()` | `(HostCommand -> IO (Maybe HostReply)) -> IO ()` |
| Host request transport | `MVar () -> Handle -> Chan String -> Async () -> String -> IO String` | `MVar () -> Handle -> Chan String -> Async () -> HostCommand -> IO (Maybe HostReply)` |
| Native status report | `Text -> IO ()` on macOS and `String -> IO ()` on Windows after binding shell state | `UpdateNotice -> IO ()` after binding shell state |
| Start handoff | `IO () -> (String -> IO String) -> IO () -> IO () -> FilePath -> FilePath -> IO ()` | `UpdateHandoff -> UpdateRoots -> IO ()` |
| Coordinate | `Maybe (Version, FilePath, FilePath) -> IO ()` | `CoordinatorIntent -> IO ()` |
| Await process milestone | `Int -> Candidate -> String -> IO ()` | `Int -> Candidate -> Milestone -> IO ()` |
| Validate roots | `FilePath -> FilePath -> FilePath -> IO ()` | `FilePath -> UpdateRoots -> IO ()` |
| Snapshot | `FilePath -> FilePath -> FilePath -> IO String` | `FilePath -> UpdateRoots -> IO Sha256` |
| Inventory collection | `FilePath -> FilePath -> FilePath -> FilePath -> IO [Inventory]` | `UpdateRoots -> UpdateRoots -> IO BackupInventory` |
| Tree walk | `FilePath -> [FilePath] -> [FilePath] -> IO [(FilePath, Bool)]` | `FilePath -> [FilePath] -> [FilePath] -> IO [TreeEntry]` |

Starter, coordinator argv, and uninstall call sites no longer serialize strings to Aeson merely to validate UUIDs. Pruning Previous now uses the same typed `backupPath` as snapshot and restore. Desktop calls identify which effect stops the backend and which flushes preferences.

## Pure policy and complexity

`parseReleaseVersion` and `selectRelease` work on values supplied by their file and HTTP adapters. `newTarget` and `candidateAuthorized` centralize predicates previously duplicated across the decoder, installed launch authorization, Data Root reservation, and coordinator. `commitSelection` rotates Current and Previous only on the exact observed Pending record. `afterCandidateFailure` selects relaunch, recovery, or error from the reread durable decision. `removals` decides which observed entries to delete and in which order. In recovery, both live tree observations and execution remain inside the existing writer locks. Release, process-line, host, and UI codecs are pure.

| Scoped metric | Baseline | Refactor |
| --- | ---: | ---: |
| Significant stringly typed roles | 12 | 3 |
| Ambiguous Boolean parameter roles | 5 | 5 |
| Positional domain tuple shapes | 8 | 1 |
| Standalone deterministic parsing or decision functions with an unnecessary `IO` signature | 1 | 0 |
| Duplicated predicates in the refactored paths | 3 | 0 |
| Open-export updater and native modules | 5 | 4 |
| Production source LOC across touched Haskell and PureScript files | 2,958 | 3,210 |
| Production LOC including touched Cabal configuration | 3,152 | 3,407 |
| Shared Haskell updater LOC | 703 | 929 |

The string roles counted are version ID, attempt ID, backup digest, payload digest, host verb, host reply, parent process instruction, child milestone, UI notice, URL, native root identity, and browser-to-shell verb. URL, native identity, and browser-to-shell verb remain text at their existing interfaces. The Boolean count covers lease mode, native flush mode, native publication mode, native copy metadata kind, and candidate ownership. It counts each native semantic role once. Backup entry kinds no longer use Booleans internally.

The removed tuple shapes are feed, asset, coordinator request, reservation, tree node, inventory entry, and restore join. The installation locator still returns `(FilePath, Version)`. The three deduplicated predicates are target exclusion, candidate identity plus ticket, and Config owner equality. Broad exports count the two `UpdatePlatform` modules, the two `UpdateNative` modules, and the baseline's `UpdateState`. The latter now has an explicit export list.

There are **17 new Haskell data declarations**, **two newtypes**, and **one PureScript ADT**. The new data declarations include the record that replaces the old `Entry` tuple. The seven main pure parsing and policy functions are listed above. Removed abstractions include the positional restore join, the effectful version parser, the bespoke backup hash formatter, the release asset lookup tuples, the raw-string host interface, and UUID validation via JSON round trips.

Physical LOC rose by 255 across the touched files, with 516 insertions and 261 deletions. Excluding blanks and comment-only lines, the touched Haskell and PureScript files went from 2,791 to 2,993 lines. The increase buys explicit types and codecs, named restore and handoff inputs, and smaller IO decision regions. The new logic uses the existing dependencies and updater architecture.

## Verification

| Platform or area | Evidence |
| --- | --- |
| macOS arm64 build | Clean baseline build with GHC 9.10.3. Successful builds during the implementation and final links for `sapiens4`, `sapiens-domain`, `sapiens-macos`, and `sapiens-macos-starter` with GHC 9.10.3 and 9.14.1. Cabal 3.18.2.0, deployment target 13.0, host macOS 26.6. PureScript and web bundle built successfully. Logs are under `.build/` in the worktree. |
| Existing end-to-end suite | 22/22 headless and 2/2 native desktop jobs passed on the GHC 9.10.3 build. This includes schema-2 interrupted-work migration, retention of its original backup, writer exclusion, and desktop relaunch. Test results are in `.build/headless-results.json` and `.build/desktop-results.json`. |
| macOS snapshot and restore | Native fixture produced identical baseline and refactor backup digests on the same live inputs. Both directions of cross-build restore passed. The fixture exercised file/directory shape swaps, registered child contents and lock identity, a busy child lock, tamper refusal, mode, ACL, xattr, format-1 Pending, reservation encoding, and settlement. |
| macOS handoff | Built coordinator with a controlled candidate. Pending was observable before Ready, and the native installed host accepted the correct attempt ticket and refused a stale one. Commit, Active, Current/Previous rotation and pruning passed. Pre-Ready rejection rolled back and retained the earlier Previous. A competing Config writer prevented ownership publication and candidate launch. |
| Host pipe | Both real backend builds emitted identical command and reply bytes, preserved the correlation token and spacing, ignored an unknown verb, and shut down cleanly on the uncorrelated stop line. |
| Release policy | Six assertions passed for numeric ordering, version grammar, exact raw tag URL, equal version, missing asset, wrong URL, and missing or malformed digest. |
| Windows x64 build | **PASS** on Windows Server 2022 Datacenter via Hyonix using GHC 9.14.1 and Cabal 3.18.2.0. Checked out `7be0b93f1201e9edc48bd5bb9419b8d10f03db13` from the fork and linked `sapiens4.exe`, `sapiens-domain.exe`, `sapiens-starter.exe`, and `sapiens-windows.exe`. The exact source UI bundle also built. Running the pinned npm install from `build/dependencies` resolved the initial Windows launcher error. |
| Windows native coordinator E2E | **3/3 passed** with the Windows build and WebView2. Healthy candidates committed with both Local and saved Remote selected. A broken candidate with saved Remote selected rolled back before commit. The adapted fixture checked Pending and settlement, Previous backup retention on commit, cleanup on rollback, registered child access, preserved file bytes, and an NTFS alternate data stream in all three cases. |
| Production publisher and packaging | **UNVERIFIED**. The macOS fixture used ad hoc signing for controlled relaunch. Developer ID, notarized DMG download and staging, and Windows Authenticode were not exercised. |
| Other installed cases | **UNVERIFIED** for a signed production old-to-new desktop update, power-loss durability, Active failure with a real candidate, historical installed schema-1 profiles, custom Windows ACL fixtures, and the full Windows Lean suite. |

Windows logs are in `C:\Build\d2-quality-7be0b93` on the VM. `windows-host.log`, `windows-desktop.log`, `windows-ui.log`, and `windows-e2e-copy.log` recorded successful exits. The first fixture used hard links to save space and was refused by the native path ownership check before Pending. The passing run used separate copies of the binaries and removed temporary version trees after each case. Windows verification added zero tracked production lines.

The native fixture and release verification output are in `.build/native-probe/`. One initial pre-Ready fixture that exited immediately reached the existing native unconfirmed-join path with a process-group permission error. The repeat fixture stayed alive until the coordinator terminated it and verified the rollback path. The platform join implementation is outside this patch.

## Risks and integration

The main regression surfaces are exact digest bytes, pipe framing, path comparison, and reread ordering around durable publication. The fixture results cover the first three for the exercised macOS paths and the normal Pending-to-Settled and pre-Ready rollback paths. Remaining maintenance work includes raw native identity text, native ownership and lease Booleans, desktop check/start strings, wide platform exports, and the case-sensitive backup-exclusion issue tracked under F09. Restore derives the desired directory list once with the existing file-path comparison. A faster lookup would need an equivalent platform comparison key.

A read-only merge-tree comparison against correctness commit `2a84b0e07a241d4eecc7700d0c6c5d51988948f0` found conflicts in `Update.hs`, `UpdateBackup.hs`, `UpdateState.hs`, Windows `Host.hs`, and Windows `Shell.hs`. Integrate the correctness branch first, then apply the refactor commits in the order below. Keep the correctness branch's ownership, stop, readiness, and filesystem behavior when resolving shared call sites. In particular, apply its exclusion logic inside the named backup inventories, and adapt its native ownership operations at the coordinator's existing masking and error scopes. After integration, rebuild and rerun the complete qualification on both platforms.

| Order | Commit | Change |
| ---: | --- | --- |
| 1 | `24fccd5154b11cd50a79bf5ed8389ca661bd26a3` | Select updater releases with pure typed policy |
| 2 | `5dcd2f5863ed64977dbd16fc0a0ede82dcf62f1b` | Distinguish installed versions, attempts, and SHA-256 digests |
| 3 | `1e559438cc1e0e856feaca545f6e8ba98cd43cbf` | Name backup roots and derive restore removals from inventory |
| 4 | `48a8d1842fb8b9f1c00dd7ab02733a39ed434e09` | Type update host commands and desktop notices |
| 5 | `929b458104d2fa495d2c4c956f16b03223f03e08` | Make coordinator intent and commit decisions explicit |
