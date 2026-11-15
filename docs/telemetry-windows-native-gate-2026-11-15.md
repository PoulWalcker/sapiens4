# Windows native authorization gate — ticket 08 completion

**Verdict: CLOSED for ticket 08 at `e2cca20b211d84e2e5cf94502a16ca966356cd46`.**
The ordinary Windows crash is reproduced, explained and fixed. The remaining
native observations below ran on actual WebView2. This closes the documented
trusted-bundle/product-data boundary, including its parent-authority limitation;
it does not establish arbitrary JavaScript isolation or complete Windows feature
readiness. Ticket 09 was not started.

## Revisions and build

- Required start: `2fea2318b93dcefdd55bd79797733426fe710d7f`.
- Fix: `e2cca20b211d84e2e5cf94502a16ca966356cd46` (two added source lines).
- Separate clone: `/Users/pashvel/Desktop/personal/sapiens4-worktrees/telemetry-native-gate-2fea-20261115`, branch `codex/telemetry-native-gate-2fea-20261115`.
- libs4: `170c2fd6936269e3245a0246787101dc9ac9310b`;
  blindly4: `cfc28cb5bcccbd58947bf13e80b79b76062b8dc9`. Neither pin changed.
- Windows Server 2022 x64, NT 10.0.20348.0; WebView2 **157.0.4341.46**;
  GHC **9.14.1**; installed Cabal **3.18.2.0**. Cabal was initially absent from
  this shell's PATH; its explicit installed path later confirmed the version.
- The real `corpora/Main.hs` host and ordinary
  `environments/windows/WindowsMain.hs` built directly with GHC. The latter used
  the pinned libs4 `native/windows.c`, not a replacement native implementation.
  **A complete Windows Cabal build was not attempted or established.**
- Windows builds read the existing package database under
  `C:\Build\d1-final-regression\submodules\libs4\haskell\.cabal\store\ghc-9.14.1-bd8b\package.db`
  and its prebuilt sodium library. Build outputs, source and profiles were confined
  to `C:\Windows\Temp\native-gate-2fea-20261115`. The baseline Windows source was
  an export of the required commit; the fixed build selected a shadow `Shell.hs`
  whose hash equals the committed file. No shared Windows checkout was changed.
- Fresh UI compilation used Node 22.23.2, PureScript 0.15.16, Spago 1.0.5 and
  esbuild 0.25.12 in the isolated local clone: 588 compiled modules and all three
  bundles. Its identical app hash to the earlier evidence is a reproducible
  rebuild result, not reuse of the old bundle.

Exact argument arrays are in `received-build-commands.json`; executable/source
hashes are in `received-guest-result.json` and `revisions.json`. Key SHA256 values:

| Input/output | SHA256 |
| --- | --- |
| Pinned native C | `49182188b17a0aa12889c1031646aafc8520182dec1fc9f8f8afe0a92e18b43a` |
| Fresh app.js | `918e6467dd3740018b285d2283ac530b4eab05a04dab2caf94294e401cd6e4d6` |
| Fixed Shell.hs | `06c3fe77b71a7e04ae2b1b84c7e6008a3b4af8da79e31d852916ac20062dd481` |
| libs4 Haskell freeze | `16ce5ec94a977b4747ce219c041618a8ae4fca66f861e7cf454f662792031265` |

The live package inventory retains http-client 0.7.19, http-client-tls 0.3.6.4,
network 3.2.9.0, tls 1.9.0, aeson 2.3.2.0, async 2.2.6,
sqlite-simple 0.4.19.0 and direct-sqlite 2.3.29.

## Reproducer and demonstrated cause

`02-valid-root-repro.ps1` first creates a complete disposable root through the
real host, observes its workspace address, and closes host stdin to stop it.
It then performs the previously reported trigger with the ordinary desktop:

```powershell
New-Item -ItemType Directory "$root/WebView2/Shell/EBWebView" -Force
& $productExe --runtime-dir $src --data-dir $root
```

The unchanged product exits **11**, reading **0x5f**. No retained-message probe
or modified callback arena is needed. An empty legacy profile directory is
sufficient; prior profile contents or corruption are not required. The earlier
attempt against an incomplete fresh folder only reached storage recovery and
is explicitly not counted as a reproduction.

LLVM symbolization of the ordinary product's crash RVA `0x1c7eb8d` resolves to
`GHC.Internal.Storable.readFunPtrOffPtr1_info`. Disassembly shows the failed
vtable-slot read, `movq (%rax,%rbx,8), %rax`. A separate temporary `Native.hs`
observer retained ordinary WindowsMain and added call-stack logging to slot 12:

```text
closeView 0x0000711000118608
method object=0x0000711000118608 slot=12 table=0xffffffffffffffff
  method, called at .../trace-source/Native.hs:108:30
  callInt, called at .../source/environments/windows/Shell.hs:111:59
Access violation ... reading 0x5f
```

That Shell line is the `WM_SETFOCUS` branch. During legacy-profile migration,
`closeView bootstrap` calls controller Close, releases its core/controller, and
then destroys its child container. Window destruction re-enters the parent
window procedure while `shellRef` still names the released view. Its focus
handler tries `MoveFocus` through controller slot 12. The observed invalid table
plus `12 * 8` explains address `0x5f` exactly. The ordinary and instrumented
failures agree; the instrumentation supplies the useful caller stack rather
than being assumed to explain the original failure.

The fix clears `shellRef` **before** closing the bootstrap view, matching the
existing detach-before-close discipline in `closeViews`. Reentrant focus,
resize and event checks consequently cannot use the retired shell. The new view
is installed by the existing `loaded` path. The [Microsoft controller reference](https://learn.microsoft.com/en-us/microsoft-edge/webview2/reference/win32/icorewebview2controller#close)
documents synchronous Close and handler release. COM ownership, callback arenas,
profile locations, guest cores, navigation guards and host shutdown are unchanged.
No exception is ignored and no profile data is deleted or reset by the fix.

| Product observation | Before | After |
| --- | --- | --- |
| Ordinary WindowsMain, valid root plus legacy directory | Exit 11, read 0x5f | Two launches reached the real bundled workspace; both closed normally with exit 0 |
| Instrumented ordinary WindowsMain | Same exit 11; useful slot/caller trace above | Separate authority observer completed replacement/recovery/closing cases with exit 0 |
| Synthetic marker inside legacy EBWebView | Present before fixed launch | Identical hash after both ordinary launches |
| Storage selection | Startup interrupted by crash | `storage.json` published after workspace readiness |

## Actual WebView2 observations

The temporary observer uses existing `WindowsProbeMain`, real views and actual
COM event objects. Retained events are AddRef'd, processed later on the UI thread,
and released. Source/current URLs come from WebView2; current-core identity,
generation and closing state come from actual product lifecycle transitions.
No JSON caller/frame/current-core metadata is substituted. The product predicates
remain unchanged. The probe defers automatic storage selection to order its
synthetic bootstrap actions, and invokes real recovery/WM_CLOSE paths.

| Remaining case | Status | Observed evidence |
| --- | --- | --- |
| Exact bundled bootstrap and current workspace | PASS | Both actual contexts pass their predicates; ordinary fixed product reaches workspace independently. |
| Message from a core no longer current | PASS | Retained bootstrap event replays after legacy replacement: generation remains 1, closing false, current core differs; receiver body is not entered. |
| Obsolete-generation message after replacement | PASS | After real recovery/begin, captured generation 1 versus live generation 2; retained event does not enter receiver body. Current core also differs; this is not a claim of an independently isolated generation-only state. |
| Closing-state callback | PASS | Actual WM_CLOSE replays a retained workspace event with closing true, same current core and matching generation; no receiver authority. |
| Retained workspace event after context change | PASS | Real recovery returns the same core to bootstrap, with generation unchanged; retained Source remains workspace, current Source is bootstrap, and both authority branches reject. |
| Bootstrap query | PASS | Real Source and current Source both end `bootstrap.html?gate=1`; predicate rejects. |
| Bootstrap fragment | PASS | Real Source and current Source both end `bootstrap.html#gate`; predicate rejects. |
| Same host, wrong port: navigation | PASS | Ordinary top-level navigation is cancelled. This alone is not message-predicate evidence. |
| Same host, wrong port: loaded-document message | PASS | Separate owned loopback document posts from its real wrong-port URL; Source/current both match that wrong port and the unchanged message predicate rejects. See diagnostic qualification below. |
| Malicious product-data embedded documents | PASS | Real Notes GET returns 409 for iframe/srcdoc, javascript iframe, object and SVG/onload inputs. |
| Accepted product-data script/attribute markup | PASS | Notes GET/control return 200; actual bundled UI renders an article and expected text. Its DOM has no script/iframe/onclick, and the script-URL link is a span without href. Click attempts produce no attacker callback. |
| Direct child frame cases | PASS for navigation/channel observation | Real same/cross-origin, about:blank and srcdoc child navigations are cancelled. Direct child posts do not reach the shell. This is not a loaded-child predicate rejection claim. |
| Parent-handler borrowing as an absolute isolation claim | FAIL | Trusted parent-injected eval in an initially empty child still produces a parent-attributed, accepted workspace message. This known limitation remains explicit. |
| Separate guest created by actual Browser.createTab | PASS | Guest title reaches GUEST_POST_RETURNED after its script attempts the channel; no guest marker reaches the shell handler and no consent profile appears. |
| Actual consent UI/writes and post-wiring negatives | NOT RUN | Ticket 09; outside this gate. |
| Full Windows Cabal build, installer and final feature E2E | NOT RUN | Outside these focused observations; no full readiness claim. |

For the loaded wrong-port case, the temporary observer permits exactly one
explicit diagnostic destination through its navigation layer, logging
`diagnosticBypass:true`. The ordinary cancellation is observed first with that
bypass off. The diagnostic changes neither message predicates nor engine
metadata; it makes the otherwise unreachable loaded-context predicate observable.
**The committed product keeps its navigation cancellation unchanged.** No
wrong-port product navigation success is claimed.

The first same-document URL fixture posted immediately after history changes.
WebView2 still reported the preceding URL, so its labels were misleading and it
never completed the lifecycle sequence. Those r1 artifacts are retained as
superseded. The final r2 fixture waits between each history update and post; its
actual Source/current values, not the marker names, establish query/fragment
rejection. The context case completed successfully in r1 and needed no repeat.

## Authority verdict and remaining scope

Parent borrowing does not protect arbitrary JavaScript already executing in the
trusted parent's principal. The verified boundary is fixed bundled code and its
product-data renderers. The real Windows malicious-data observations above,
`Memo.validateMemoMedia`, the structural `Workspace.memoNode`/`memoLink` renderer,
and separate guest cores establish that boundary for the current application.
Revisit it before adding active HTML, iframe rendering or forwarding arbitrary
window messages into the native channel.

A generic consent-shaped native message reached the normal workspace handler,
but created no telemetry profile. Forwarded window messages gained no native
consent action. Prior actual local-HTTP/header and encrypted Remote Control
negative observations remain recorded in the original Windows evidence; they
were not rerun or relabeled as fresh observations here. This fix adds no consent
branch, route or IPC, and changes none of those transports.

The existing [narrow adapter contract](telemetry-native-authorization.md#narrow-adapter-contract-for-ticket-09)
remains binding: explicit local action, choice/notice/expected epoch, the same
CAS writer, running-sender withdrawal ordering, and a safe read-only result.
Generic native messages, Host/Origin/X-Sapiens-Local and encrypted Remote Control
do not become consent authority. No fixture JSON was sent to ordinary host
stdin; closing its input was used only for its existing shutdown behavior.

O14-A01/A02 now have the missing actual Windows lifecycle/context/data evidence;
O14-A03 retains its specified adapter without implementation; O14-A04 records
the tested resolution and the surviving trusted-principal limitation. Windows
native authorization is consequently **CLOSED for 08**, while actual consent
wiring/writes remain unimplemented 09 work. B9 was not reopened and tickets
06/07 were not broadly repeated.

## Evidence and cleanup

New sanitized evidence is retained separately at:

`/Users/pashvel/Desktop/personal/sapiens4-worktrees/telemetry-native-evidence-2fea-20261115`

Its README maps commands, ordinary/observer builds, crash symbolization, logs,
observations, superseded fixtures and hashes. Earlier cf94 evidence is untouched.
Transport URLs/tokens are replaced with `<OWNED_FIXTURE_BASE>`; replay requires
a new owned transport. No credentials, private keys or user content are retained.

Windows cleanup confirms the entire owned temporary tree absent, zero owned
native processes, no wrong-port listener, and existing C:\Build/package database
preserved. The temporary worker exited, its jobs were empty, and console cleanup
returned NATIVE_GATE_CLEAN. No certificate was created. The new local tunnel and
loopback transfer service were stopped, and the task-owned link to the existing
UI dependency installation was removed. Source/build evidence remains in the
separate local clone. The shared checkout's original changes are unchanged.

No regression suite, installer check, paid model call, production profile, final
feature E2E, push or PR ran. Fix and report are separate commits. Work stops at
this gate.
