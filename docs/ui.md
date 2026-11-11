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

## Desktop startup and storage

macOS and Windows open the shared `bootstrap.html` before starting the backend:
Welcome + Storage → Validate Storage → Start Backend → Workspace.
Start fresh uses the empty default directory. Use another folder initializes an
empty selected directory or attaches a complete existing Data Root in place.
Nonempty, partial and invalid roots are never replaced or repaired.

After successful workspace navigation, the launcher durably saves the canonical
path in `storage.json` under the configuration directory. Subsequent launches
validate and reopen that root. Explicit launch overrides take precedence. When
no selection exists, source macOS launches discover the baseline Launcher.plist
DataDirectory (otherwise the checkout's `.sapiens4`); stable Windows discovers
baseline `%LOCALAPPDATA%\Sapiens4`. Discovery still requires full validation,
never copies data, and never crosses stable/development defaults. Fresh installs
retain the profile-specific default. Missing, invalid or locked storage returns
to the pre-backend picker instead of creating replacement data. The running workspace
has no storage switching or persistent onboarding stages.

`DataRoot.hs` shares validation between the launchers and backend. The backend
rechecks storage under its exclusive lock before initialization or migration.
Partial initialization is rejected; restore complete data from backup or select
another empty directory. Startup failures stop owned children and preserve data;
process ownership is retained until exit is confirmed, and failed startup or
navigation does not commit a new selected path.

The native shells mirror workspace preferences, appearance and active Corpora
by canonical Data Root: macOS uses a profile-specific NSUserDefaults suite;
Windows uses durable JSON under the configuration directory, falling back to
safe defaults and logging errors if optional preferences cannot be decoded.
They restore this state before the UI starts on a new loopback origin, so a changed port does not
lose drafts. Windows reuses existing WebView2 shell profiles in place. Only the
trusted shell can update this device-local state; guest pages and Corpora
snapshots do not carry it.

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
