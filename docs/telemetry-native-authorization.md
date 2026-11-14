# Native consent authority review — ticket 08

Reviewed against `f5ed98937acafbac4bb794b2eec6efb3e24cb535` on
`telemetry/d4`, retaining the selected D3 baseline and libs4 pin. This ticket
reviews the boundary before ticket 09 adds consent controls. It adds no consent
endpoint, UI control, event hook, credential exposure or IPC service.

**B3 remains open.** The macOS review found a real limitation of main-frame
attribution and verified a narrow navigation fix. Windows runtime evidence and
targeted review of the trusted-content/explicit-action assumptions are still
required before consent mutation wiring. No whole-platform PASS is claimed.

## Existing native contexts

| Context | Source-grounded checks and handling |
| --- | --- |
| macOS bootstrap | `Application.receiveMessage` checks the live shell view, main frame, exact expected bundled file URL and the shell's current URL through `Browser.trustedShellMessage`. Only storage actions currently exist. |
| macOS workspace | `Browser.receive` checks the live shell view, main frame, source and current origin, and the allowed workspace paths. The source view/frame information comes from `WKScriptMessage`, not its JSON body. |
| macOS guest | `Browser.createTab` uses a separate configuration with no `browser` script-message handler. The shared delegate alone does not expose that handler to guests. |
| Windows bootstrap | `Shell` registers on the current shell core. Its callback checks closing state, generation and active core. Both message Source and current core Source must match `bootstrapSource`, which rejects credentials/authority, query/fragment and an incorrect file path. |
| Windows workspace | The same callback checks both Source and current Source with `Navigation.trusted`: expected scheme/authority, no credentials, allowed workspace path. Arbitrary other paths, views or origins are not authority. |
| Windows frames/guests | Registration is on `ICoreWebView2`, rather than a frame message handler. Guest views have separate cores. Actual frame/parent-handler behavior still needs the Windows probes below. |

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
only scheme/path. Net implementation growth is four nonblank source lines;
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

These are controlled adversarial documents, not a demonstrated reachable exploit
in the shipped UI. Source inspection found no app iframe renderer: guest pages
use separate native views, the workspace routes serve fixed bundled assets, and
the inspected native-message calls do not forward arbitrary window messages.
Nevertheless, main-frame metadata cannot support an unconditional claim that
every child caller is excluded. Consent writes are not wired today.

Targeted review must resolve whether the verified fixed-bundle/native-guest-only
invariant establishes the required authority, or whether stronger separation is
needed for the future consent action. It must also ground the explicit operator
action in the actual onboarding/Settings handler. A JSON boolean such as
`userInitiated`, a claimed actor, or a generic forwarded message is not proof.
Do not silently replace this review with a new authentication/IPC framework.

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

## Required Windows observations and remaining gate

The separate verification session is asked to check 06/07/08 at published
`f5ed989`. It must record actual OS/WebView2/compiler versions and runtime results
separately from this macOS review. Repeat allowed bootstrap/workspace, wrong
view/origin/path/file/query/fragment, current-navigation mismatch, stale generation,
guest, direct child and parent-handler borrowing cases, including srcdoc.
Do not infer frame isolation from the documented top-level event alone.

Before 09 is approved, resolve the trusted-content/action review above and obtain
the actual Windows verdict. After the adapter exists, repeat disallowed-caller
checks against **real consent writes**, including generic payloads, local HTTP,
Remote Control and failed-withdrawal behavior. No ticket status file is changed.
