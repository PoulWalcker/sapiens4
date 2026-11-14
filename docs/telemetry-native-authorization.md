# Native consent authority review — ticket 08

Reviewed against `f5ed98937acafbac4bb794b2eec6efb3e24cb535` on
`telemetry/d4`, retaining the selected D3 baseline and libs4 pin. This ticket
reviews the boundary before ticket 09 adds consent controls. It adds no consent
endpoint, UI control, event hook, credential exposure or IPC service.

**macOS gate: PASS for ticket 08. Windows gate: OPEN after actual verification.**
The follow-up to `1f8fdf4` verifies the trusted-content boundary, blocks child
document navigation in shell/bootstrap, and exercises the actual bundled UI.
macOS consent wiring may proceed in ticket 09 under the adapter contract below;
the real consent-write/UI checks belong to 09. Windows is not closed by this result.

## Existing native contexts

| Context | Source-grounded checks and handling |
| --- | --- |
| macOS bootstrap | `bootstrapNavigation` permits only the main frame and exact expected file. `Application.receiveMessage` checks the live shell view, main frame, source and current bundled URL through `Browser.trustedShellMessage`. Only storage actions currently exist. |
| macOS workspace | `shellNavigation` rejects child document navigation. `Browser.receive` checks the live shell view, main frame, source/current origin and workspace paths. Source view/frame information comes from `WKScriptMessage`, not its JSON body. |
| macOS guest | `Browser.createTab` uses a separate configuration with no `browser` script-message handler. The shared delegate alone does not expose that handler to guests. |
| Windows bootstrap | `Shell` registers on the current shell core. Its callback checks closing state, generation and active core. Both message Source and current core Source must match `bootstrapSource`, which rejects credentials/authority, query/fragment and an incorrect file path. |
| Windows workspace | The same callback checks both Source and current Source with `Navigation.trusted`: expected scheme/authority, no credentials, allowed workspace path. Arbitrary other paths, views or origins are not authority. |
| Windows frames/guests | Registration is on `ICoreWebView2`, rather than a frame message handler. Guest views have separate cores. The Windows follow-up below records actual parent borrowing and adds shell child-navigation cancellation. |

Apple exposes the source webview and frame through
[WKScriptMessage](https://developer.apple.com/documentation/webkit/wkscriptmessage?language=objc).
Microsoft documents core `WebMessageReceived` for top-level documents and
asynchronous delivery in
[ICoreWebView2](https://learn.microsoft.com/en-us/microsoft-edge/webview2/reference/win32/icorewebview2?view=webview2-1.0.3856.49#add_webmessagereceived).
That attribution alone does not prove which script caused a call. Microsoft also
recommends checking current Source and validating specific messages in its
[WebView2 security guidance](https://learn.microsoft.com/en-us/microsoft-edge/webview2/concepts/security).

The macOS change extracts one shared predicate for bootstrap/workspace and checks
both the sending document and current loaded document. Previously the handlers
checked only the sending URL. Bootstrap now compares full NSURLs rather than
only scheme/path. Net ticket-08 implementation growth is eight nonblank source lines;
there is no new module or dependency.

## Observed limitation: parent handler borrowing

A controlled real WebKit fixture loaded a same-origin iframe. Its direct message
was rejected as a child-frame message. However, calling
`window.parent.webkit.messageHandlers.browser.postMessage(...)` from that iframe
produced a message attributed to the parent's main frame and passed the existing
workspace gate. An inline `srcdoc` frame could borrow the parent handler too.

A candidate `frame-src 'none'` policy blocked ordinary child URL loads in the
fixture but did not stop inline srcdoc script or parent-handler borrowing. This
candidate was **not shipped** as a complete remedy. Bootstrap's existing CSP is
also not evidence that all inline descendant script is isolated: it permits
inline script and its frame restriction is inherited from `default-src`.

The fix rejects child document navigation in the native shell and bootstrap
delegates. Actual WebKit navigation checks block ordinary same/cross-origin,
about:blank/srcdoc, data and blob child loads before their script runs. Separate
guest views keep their existing navigation behavior and have no shell handler.

The remaining boundary is trusted bundled code, not arbitrary JavaScript injected
into that code's principal. A deliberate top-level fixture script can still run
a `javascript:` URL in an initially empty child and borrow the parent handler;
that path does not invoke the navigation delegate. The fixture already holds the
same parent authority it is borrowing. No protection against compromised bundled
JavaScript or a modified runtime is claimed.

Source review and actual host/UI observations establish why untrusted product
data cannot request that operation: fixed asset routes supply the shell;
`Memo.validateMemoMedia` rejects iframe/object/SVG embedded content before it
reaches the renderer; `Workspace.memoNode` renders only structural tags and copies
only prefixed IDs, dropping scripts/event attributes. `memoLink` excludes script
URLs. Other text previews remain text. The app creates no iframe, srcdoc or
javascript-URL child, and its inspected native calls do not proxy arbitrary
window messages. Guest pages use independent native views with no shell channel.

This resolves the macOS trusted-content question for the current application.
Revisit this boundary before adding a new active HTML/iframe renderer. In 09,
only the explicit onboarding choice/Continue and Settings-toggle callbacks may
produce the dedicated consent action. A JSON `userInitiated` boolean, claimed
actor, generic snapshot or forwarded message is not operator authority.

## Narrow adapter contract for ticket 09

- Validate native caller/context before decoding a consent operation. Use a
  dedicated action, separate from workspace readiness, preferences, guest
  synchronization and remote dispatch. Do not grant from startup state, model
  snapshots, ordinary messages or a read of the current choice.
- Accept only explicit choice, approved notice version and expected prior
  consent epoch (canonical UUID or null when no valid epoch exists). Reject
  malformed/unknown fields and mixed generic actions; a caller cannot supply
  enrollment credentials, DeviceId, a config path or authority flags.
- Retain the same profile CAS writer. A running sender must use its existing
  `hostConsent` operation so disabling/purging precedes the write and failed
  withdrawal stays locally disabled. Conflicts require refresh and deliberate
  retry, never automatic overwrite of the newly observed choice.
- Return only `consentStatusJSON` plus a bounded action result: granted/denied
  state, notice and epoch, or a safe error/conflict. No credentials, identifiers,
  filesystem diagnostics, privacy keys or live event history reach JavaScript.
  A read-only local result grants no write authority.

Native shell and sender currently live in **different processes**. Both adapters
already own child stdin/stdout handles, but `corpora/Main.hs` currently treats
the first stdin line as shutdown and the native readers drain subsequent stdout.
Ticket 09 must explicitly reconcile that existing owned channel if it uses it
for narrow commands/results; sending JSON into the current loop would stop the
host. No new public HTTP mutation or IPC service is authorized by this review.
Before any child/sender exists, a trusted bootstrap action can use the same
profile CAS writer without bypassing a running queue owner. This is an adapter
specification, not an implemented command protocol.

## Actual macOS observations

On 2026-11-14, macOS 26.6 / WebKit 21624.4.5.11.5 / GHC 9.10.3, a temporary
hidden native probe compiled the real `Browser`,
`ObjC` and guest-view code with the existing `build/` dependency freeze. It used
real WKWebViews, engine-created script messages, private loopback fixtures and
temporary files; no fabricated frame metadata, production profile, paid provider
or GUI automation was used. Nineteen assertions included reproducing the
limitations above; that count is **not** nineteen security PASS results.

- The allowed main workspace reached the real readiness/preferences callbacks.
  A different actual view, wrong origin/port, wrong path, direct same-origin,
  cross-origin and srcdoc child messages did not. Cross-origin parent borrowing
  was blocked by the browser. A guest created through actual `createTab` had no
  browser handler. Parent borrowing from same-origin/srcdoc remained accepted.
- Actual messages were retained and processed after real navigation. A queued
  workspace message was rejected when the current origin changed; a queued
  bootstrap message was rejected after the current file changed. Exact bundled
  bootstrap passed its gate, while another file and an unexpected query failed.
- Eight public loopback POST/PUT attempts at telemetry/consent-shaped routes,
  including valid local Origin/JSON headers, returned 404. A real encrypted
  Remote Control consent-path request returned 404; a host-path request was
  rejected by the existing remote route restriction. No telemetry profile was
  created. These are pre-wiring observations and must be repeated after 09.
- `cabal build sapiens4 sapiens-macos` and `git diff --check` passed. No regression
  suite, installer, production collection or feature-completion E2E ran.

Temporary probe sources, observations and build instructions are retained outside
the repository in `sapiens4-worktrees/telemetry08-native-evidence-20261114` for
targeted review. No permanent probe or test suite is added to the product.

## macOS completion follow-up

The original controlled iframe scenario failed before the native navigation fix
and passed using the actual `shellNavigation` afterward. Twelve focused guard
assertions, six production-navigation checks and seven actual host/bundled-UI
observations passed. These cover the original loaded-child attack, source/current
checks, separate guest behavior and successful native workspace readiness.

The production bootstrap function allowed its exact file, rejected another file
and query, and prevented ordinary and srcdoc child handler calls. WebKit can
still update the current URL on an in-document fragment navigation; the current
NSURL then fails the exact bootstrap-message predicate, so that fragment cannot
authorize a native operation. This is not claimed as a guaranteed URL-bar rollback.

Actual host requests rejected embedded iframe/srcdoc/javascript-iframe/object/SVG
Notes examples with 409. A freshly built actual workspace loaded an accepted note
containing script/attribute markup, reached native readiness and emitted no
attacker callback or child navigation. Attribute filtering is also established
by the structural renderer source; no DOM assertion or synthetic user click is
claimed. The remaining fixture-only javascript-URL case described above is
recorded explicitly and is not counted as a blocked arbitrary-root-script attack.

| Criterion | macOS result |
| --- | --- |
| O14-A01 | PASS: view/frame/source/current/navigation context and explicit-action contract recorded. |
| O14-A02 | PASS within the verified fixed-bundle/data/guest boundary: source review, actual native negatives and public HTTP/encrypted-caller observations. |
| O14-A03 | PASS: narrow choice/notice/expected-epoch CAS adapter and safe read result specified; no mutable public consent API. |
| O14-A04 | PASS for macOS 08: context uncertainty resolved and scope recorded; actual consent UI/writes remain 09 checks. |

Host/macOS builds, UI build and diff whitespace checks passed. Windows transport
fixes imported independently from `a92438b` and `b144e9d` do not change this macOS
authority verdict. Completion evidence is retained separately in
`sapiens4-worktrees/telemetry08-macos-completion-evidence-20261114`.

## Required Windows observations and remaining gate

The separate verification ran at published
`cf94f221ec1d9cd5acac8f5ee6a5c3d7af1cbe6e` on Windows Server 2022,
WebView2 157.0.4341.46 and GHC 9.14.1. The
[Windows report](telemetry-platform-windows-2026-11-15.md) records the complete
PASS/FAIL/NOT RUN table and retained temporary probe sources.

Baseline actual srcdoc/initially blank child code could call
`window.parent.chrome.webview.postMessage`, producing an accepted message
attributed to the trusted parent. Direct child posts did not reach the shell.
The separate fix `ca94d63` cancels FrameNavigationStarting in shell/bootstrap.
Actual same/cross-origin, srcdoc and about:blank navigation were cancelled after
the fix; the freshly built workspace and an independently created guest worked.
A real retained bootstrap event was rejected when processed after navigation to
workspace because its source and current context differed. Generic consent-shaped
native/window messages, local HTTP and actual encrypted Remote Control did not
gain consent-write authority before 09.

The deliberately trusted parent fixture can still evaluate code in an initially
empty workspace child and borrow the parent channel. This is the same limitation
described for macOS: arbitrary JavaScript already in the trusted principal is
not isolated from that principal. Fixed-bundle/data-renderer source review found
no reachable product-data path; actual malicious renderer cases were not repeated
on Windows and are NOT RUN, not inherited macOS PASS results.

An existing-root/legacy-WebView-profile scenario causes a generated-code access
violation (exit 11), reproduced with both the uninstrumented baseline WindowsMain
and the fixed ordinary WindowsMain, using pinned native C. The cause remains
unknown. It interrupted the obsolete-generation/wrong-current-core probes;
those runtime negatives, closing-state, bootstrap query/fragment and workspace
delayed mismatch remain unproven. Do not infer their PASS from the source guards.
No speculative crash fix is included.

**Windows consent wiring remains blocked by this OPEN native verdict.** After each platform's
adapter exists, repeat disallowed-caller
checks against **real consent writes**, including generic payloads, local HTTP,
Remote Control and failed-withdrawal behavior. No ticket status file is changed.
