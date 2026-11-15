# Windows telemetry verification

> Follow-up: the [Windows native gate completion](telemetry-windows-native-gate-2026-11-15.md)
> closes ticket 08 at `e2cca20b211d84e2e5cf94502a16ca966356cd46`. This document
> retains the earlier cf94 verification and its OPEN verdict as historical evidence.

See the [verification index](telemetry-platform-verification-2026-11-15.md) for
revision pins, evidence location, terminology and independent Parquet checks.

## Runtime and commands

Actual Windows Server 2022 Datacenter, 10.0.20348, x64, reached through Google
Chrome's Windows console. GHC 9.14.1, Cabal 3.18.2.0, clang 20.1.7 targeting
x86_64-w64-windows-gnu; WebView2 **157.0.4341.46**. Remote disposable source:
`C:\Windows\Temp\telemetry-cf94-20261114-r3\source`.

The host and focused probes were compiled directly with GHC against the existing
read-only pinned package database at
`C:\Build\d1-final-regression\submodules\libs4\haskell\.cabal\store\ghc-9.14.1-bd8b\package.db`.
The prebuilt static libsodium and WebView2Loader.dll were reused without altering
their installations. `Paths_sapiens_domain` reports version 0.2.0.0. Initial Cabal
configuration and a direct link with a redundant explicit `-lws2_32` failed
(duplicate WSASendMsg); removing that redundant fixture link flag produced the
host. A complete Windows Cabal build is **not** claimed.

Exact argument arrays and build outputs are retained in `windows/cf94-build3.ps1`,
`cf94-probes.ps1`, `cf94-key-startup.ps1`, `cf94-native-compare.ps1` and the
`received-cf94-*-build.log` files. Representative host invocation:

```powershell
$src='C:\Windows\Temp\telemetry-cf94-20261114-r3\source'
$w=Split-Path $src
$db='C:\Build\d1-final-regression\submodules\libs4\haskell\.cabal\store\ghc-9.14.1-bd8b\package.db'
ghc --make "$src\corpora\Main.hs" "-i$src\corpora" "-i$src\corpora\state" "-i$src\corpora\work" "-i$src\corpora\work\schedule" "-i$src\corpora\host" "-i$src\remote\relay" "-i$src\remote\control" "-i$src\remote\protocol" "-i$src\build" "-i$w" -package-db $db -threaded -O0 -XOverloadedStrings -XScopedTypeVariables -XTupleSections -ladvapi32 "$src\corpora\host\TelemetryWindowsIO.c" "-L$src\submodules\libs4\haskell\sodium\src\libsodium\.libs" -lsodium -outputdir "$w\host-obj" -o "$w\sapiens4.exe"
```

The successfully built raw native product uses `environments/windows/WindowsMain.hs`
and the pinned libs4 `native/windows.c`. Its SHA256 is
`49182188b17a0aa12889c1031646aafc8520182dec1fc9f8f8afe0a92e18b43a`.
The initially reused C:\Build copy differed only in line endings; normalized
contents were identical. Final frame, delayed-message and guest probes were
repeated with the pinned file and fresh native profiles.

## B9: actual Windows HTTPS

**PASS for the focused prerequisite; no transport source change was necessary.**

The original IPv4-only localhost fixture had no observed TCP. Actual backend
instrumentation resolved `localhost` as `[::1]` then `127.0.0.1`, attempted IPv6
first, received nonblocking pending `-10035` and exhausted the approximately
2003.6 ms connection budget without reaching the IPv4 listener. Changing the
fixture to listen on IPv6 loopback made the same validated backend succeed;
an IPv4 literal with the matching IP SAN also succeeded. The address-family
mismatch explains the fixture failure. Why that pending IPv6 connection did not
fail sooner and reach fallback remains unknown; no general dual-stack fallback
correctness is claimed.

The private fixture used `SSL_CERT_FILE` for its generated CA and a DNS localhost
leaf; validation and hostname checking stayed enabled. Negative trust explicitly
used an empty CA bundle. Certificates were removed afterward. Normal system
trust also succeeded through the public valid-HTTPS synthetic collector tunnel.

| Case | Result | Actual client/server observation |
| --- | --- | --- |
| Valid chain and localhost | PASS | 130.911 ms; TCP 1, successful TLS 1.2, HTTP 1, connection closed. |
| Untrusted certificate | PASS | 46.3604 ms, UnknownCa; TCP 1, TLS failure 1, no HTTP. |
| Trusted chain, wrong hostname | PASS | 27.4874 ms, NameMismatch ::1; TCP 1, TLS failure 1, no HTTP. |
| Slow response body | PASS | 5011.1874 ms whole-operation timeout after actual TLS/HTTP; connection closed. |
| Stop during body read, emitter retained | PASS | stop 0.2143 ms; server observed close within 436 ms. |
| Stop with pooled connection, emitter retained | PASS | stop 0.1386 ms; server observed close within 108 ms. |

Reproduction sources: `DirectHTTPS.hs`, `cf94-direct6.ps1`, `local-tls6.ps1`,
`cf94-network-trace.ps1`, `cf94-cancel.ps1`. Server-side observations and timings
are in `received-cf94-direct-v6-results.json`, `network-trace.json` and
`cancel-results.json`. The earlier IP fixture's accidentally trusted negative
case is superseded by the explicit-empty-trust IPv6 negative and is not a PASS.

## Ticket 06

| Criterion / case | Status | Observation / evidence suffix under windows/ |
| --- | --- | --- |
| O08-A05 genuinely new root | PASS | Exactly one hook; root-followup.json. The initial scalar PowerShell .Count assertion was faulty and is superseded by this explicit array check. |
| AttachExisting, ordinary existing-root boot | PASS | Zero creation hooks on reopen; boundary-results.json. |
| Restored root and lazy attached existing child | PASS | Zero hooks, both registry entries present; restore-results.json. |
| Failed root/managed opening | PASS | Occupied-port actual startup failure, empty hook list; key-startup-results.json. |
| O13-A01 child after registry persistence | PASS | One hook after successful public registry create; registry-save failure 500 adds none; boundary/failure-results.json. |
| O13-A02 no duplicate root hook / shared sender | PASS | Managed StartNew emits no root hook; actual root/child use the same passed Emit and one enrollment, InstallationId/DeviceId in wire/Parquet. |
| O13-A03 existing-child migration | NOT RUN | Restore and lazy attachment passed; migration was not separately performed. |
| O08-A06 / O13-A04 independent public IDs | PASS | Actual root/child Parquet rows, independent PowerShell HMAC, one InstallationId/DeviceId; wire-identity.json and independent-privacy-checks.json. |
| Actual host-created DataRoot ACL / initial publication | PASS | TokenUser owns root and identity, protected DACL with owner-only full access; identity preparation publishes a privacy key while preserving protection. Not an artificially secured root. |
| Off / invalid profile / outage creation | PASS | New root and managed child remain usable; allkinds-isolation/key-startup-results.json. |
| Full queue managed-child creation | PASS | Worker blocked on body, 300 filler attempts against bound 128; child plus three request kinds accepted in 133 ms. |
| Root startup when queue is already full | NOT RUN | Root hook precedes filling the queue in these probes. |

## Ticket 07

| Criterion / case | Status | Actual observation |
| --- | --- | --- |
| O10-A01 new external turn / clarification | PASS | One hook per independently accepted message and accepted clarification; wrong/duplicate clarification adds none. Actual specialist Execution follows assessment/delegation without another external capture. |
| O11-A01 normal external group save | PASS | One group_message hook after normal save. |
| O12-A01 / A04 external task runs | PASS | Two permitted independent runs each add one; pending rejection 409 adds none. |
| Required persistence failures | PASS | SQLite triggers reject message_inputs and groups saves: HTTP 500, no corresponding hook for all three kinds. |
| Later projection / dispatch failure | PASS | turn_details projection after agent acceptance and group downstream dispatch return 500 but retain the already accepted hook. |
| O10-A02 routine / validation / provider / setup exclusions | PASS | Routine acknowledgments, empty/invalid flow/attachment input, retired agent, missing provider and missing computer setup add none. |
| O11-A02 / O12-A03 server provenance | PASS | Client actor/origin/agencyRun/external flags cannot grant provenance. Actual provider GroupControl message and GroupTaskRun execute without group/task external captures. |
| O11-A03 / O12-A02 scheduled/internal/worker/settlement | PASS | Actual minute scheduler advanced agent and group routine run_count to 1 with zero captures; task creation/edit/revision rejection and later worker/settlement add none. |
| Real encrypted replay, all three kinds | PASS | Actual relay + Haskell peer pairing; first accepted encrypted request adds one, same encrypted frame returns identical saved response and adds zero. |
| Off/profile/key/outage/overflow, all three kinds | PASS | Follow-up allkinds fixtures each accept agent, group and task; malformed key unchanged, outage including creation 295 ms, full-queue creation plus three accepts 133 ms. |
| O10-A03 distinct IDs / unchanged HTTP retry | PASS | Independent acceptances have distinct EventIds; first seven-event 503/202 attempts have identical bytes and IDs. |
| Privacy and actual Parquet | PASS | Eleven unique Windows rows, closed schema, real attachment name/content absent, raw UUID/path/provider absent; independent DuckDB and HMAC checks. |
| Exhaustive invalid inputs and persistence fault sites | NOT RUN | Focused boundaries above were exercised; every possible filesystem/SQLite failure point was not enumerated. |

Boundary/provenance/failure/schedule/restore/wire-replay/attachment and
allkinds JSON files retain the precise assertions. Hook observations are not
equivalent to delivery; see the index for the distinction.

## Ticket 08: OPEN

The actual `Shell`, `Navigation`, `Native`, `Browser`, and `Host` paths were
reviewed. Temporary shadow copies logged real engine-created Source/current
values and unchanged predicates. Delayed messages retained the actual COM event
and re-entered the UI thread; no caller/frame metadata was invented.

| Context / rejection | Status | Actual observation |
| --- | --- | --- |
| Allowed exact bundled bootstrap / current workspace | PASS | Real callbacks accepted in both contexts with freshly built app.js. |
| Direct srcdoc/about:blank/same/cross child | PASS after fix | FrameNavigationStarting cancels all four navigations. Direct child messages did not reach the shell handler. |
| Parent handler borrowing | FAIL as an absolute isolation claim | Baseline srcdoc and initially blank child borrowed parent authority. Fix prevents srcdoc navigation, but parent-injected eval in initially blank workspace child still produces an accepted parent-attributed message. |
| Wrong bootstrap file / workspace path / origin and port / top-level about:blank | PASS for navigation rejection | Actual NavigationStarting canceled each attempted navigation. This is not a loaded-wrong-document message predicate test. |
| Deferred bootstrap Source, current workspace | PASS | Real bootstrap event processed after legitimate workspace navigation; source/current mismatch rejected. |
| Guest created by actual Browser.createTab | PASS | Guest script ran and attempted postMessage; no guest message reached Shell, no telemetry profile created. Separate core disables web messaging and has no shell callback. |
| Generic forwarded/native consent-shaped payload / window.postMessage | PASS before 09 | Real trusted generic payload accepted as generic, no consent profile created. No consent action exists yet. |
| Local HTTP including matching Origin/X-Sapiens-Local | PASS before 09 | Consent-shaped routes return 404, existing profile hash unchanged. Headers confer no operator identity. |
| Encrypted Remote Control consent route / host route | PASS before 09 | 404 or existing remote host-route error; no write authority. |
| Obsolete generation / wrong current shell core | NOT RUN | Replacement probe interrupted by access violation; guards in source do not substitute for successful runtime negatives. |
| Closing-state callback / workspace delayed mismatch | NOT RUN | Not isolated successfully. |
| Bootstrap query/fragment / independent same-host wrong-port | NOT RUN | Not separately exercised. |
| Actual malicious product-data renderer cases on Windows | NOT RUN | Fixed-bundle Memo/Workspace filtering was reviewed; macOS observations are not substituted for Windows runtime probes. |
| Narrow choice/notice/expected-epoch CAS contract | PASS for specification | Existing documented dedicated-action contract retained; no consent writer/UI/IPC was added. Real write/failure behavior belongs to 09. |
| Legacy-profile startup / core replacement | FAIL | Baseline and fixed ordinary WindowsMain executables exit 11 with generated-code access violation. Cause unknown. |

The parent-borrowing fixture already executes arbitrary trusted parent JavaScript.
It is not evidence of a reachable untrusted product-data exploit. Source review
found fixed shell assets, Memo rejecting active embeds, structural Workspace
rendering and no arbitrary window-message proxy. That distinction limits the
claim: it does not make arbitrary JavaScript in the trusted principal safe.

The minimal fix `ca94d63` registers ICoreWebView2 FrameNavigationStarting
(vtable add slot 17), cancelling child navigation in the shell/bootstrap.
It leaves separate guest navigation intact. Pinned-C follow-ups are
`received-cf94-pinned-{current,delayed,guest}-native-*`.

### Access violation reproduction

On an existing fixture root which had successfully reached workspace, create the
legacy directory recognized by the adapter, then launch the ordinary desktop
entrypoint without observer/delayed-message instrumentation:

```powershell
New-Item -ItemType Directory "$root/WebView2/Shell/EBWebView" -Force
& $productExe --runtime-dir $src --data-dir $root
```

`cf94-native-compare.ps1` builds fixed-product from ordinary WindowsMain;
`cf94-baseline-product.ps1` repeats with exact baseline Shell and ordinary
WindowsMain. Both use the pinned native C. Baseline-product reads
0xffffffffffffffff and exits 11; fixed-product reads 0x5f and exits 11.
The baseline probe entrypoint also fails. Fresh native profiles completed the
allowed/frame/guest cases. The cause and exact crash mechanism remain unknown;
neither a retained callback nor the two-line frame guard alone explains these
observations. Stack excerpts and commands are retained in the native comparison
JSON/build logs. No speculative crash fix was committed.

O14-A04 is satisfied as an uncertainty record, **not as permission to wire
Windows consent**. The native gate remains OPEN pending targeted crash and
generation/current-core verification. No JSON command was sent to the ordinary
host's shutdown stdin; only the explicitly implemented temporary probes accepted
fixture commands.
