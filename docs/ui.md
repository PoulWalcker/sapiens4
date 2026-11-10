# UI architecture

The UI is PureScript/Halogen. Views render the model; typed actions handle user
input and external events. Halogen owns rendering and subscription cleanup.

| Source | Responsibility |
| --- | --- |
| `app/shell` | Model, actions, navigation, component lifecycle, shared HTML |
| `app/features` | Conversations, tasks, workspaces, dialogs and connections |
| `app/theme` | Styles and responsive layout |

Requests carry identities and a corpora epoch so late responses cannot replace
another selection or newer data. Send completion clears only the submitted draft
and attachments; newer input survives. Failed resources require explicit retry.
Native browser reports are serialized, and corpora switching restores that
corpora's cached preferences.

The macOS shell mirrors workspace preferences and the active Corpora into a
profile-specific NSUserDefaults suite, keyed by the canonical Data Root. It
restores them before the UI starts on a new loopback origin, so a changed port
does not lose drafts. Only the trusted shell can update this device-local state;
guest pages and Corpora snapshots do not carry it.

Browser/native APIs and third-party bindings live in
`submodules/libs4/purescript/src/Libs4/Browser.purs` and its foreign implementation.
Application behavior stays here. Host payloads are JSON; navigation, actions,
dialog kinds and resource lifecycles use algebraic types. Work → Memo opens or focuses the selected Sapi’s bookmarked Notes.html in its workspace.
Closing the tab preserves its bookmark. Native guest views have no app-control bridge.

Build with GHC, Cabal, Node.js 22.5+ and npm:

```sh
git submodule update --init --recursive
runghc build/Build.hs
```

The shared `build/BuildUI.hs` helper compiles and bundles the UI. After the host has
been built, `.build/bin/sapiens4 build-ui` rebuilds only the UI.
The pinned toolchain emits `app.js`, `sw.js` and `guest.js` into `.build/web`.
Generated bundles are not committed. Source startup and packaging invoke the build;
completed packages need no compiler or npm. See [Testing](testing.md) for validation.
