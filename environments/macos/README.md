# Sapiens4 for macOS

An AppKit/WebKit window with a Dock icon. Requires macOS 13+, Command Line Tools,
stable Rust, GHC 9.14.1, Cabal 3.18.2, Node.js 22.5+, npm, Git and authenticated
Codex CLI 0.156.1 or newer.

## Build and open

From the repository root:

```sh
git submodule update --init --recursive
./submodules/blindly4/macos/build.sh
./environments/macos/build.sh
open .build/macos/Sapiens4.app
```

The build records the compiled backend executable, PATH, repository location
and data directory. Keep the checkout and its build output in place.
Closing the Workspace window leaves the backend running so conversations can
finish. Quitting stops its owned backend and active calls; closing during startup
cancels startup. The app installs no login item.
Right-click the Dock icon → Options → Keep in Dock.

Fresh source builds default to `~/Library/Application Support/Sapiens4 Development/data`.
When no storage selection is saved, the launcher discovers existing baseline data
at the build's `Launcher.plist` DataDirectory (normally the checkout's `.sapiens4`)
and validates it before reuse. `SAPIENS_DATA_DIR` at launch explicitly overrides
the selection. Empty folders can initialize new data from the same storage screen.
The backend log is
`desktop-server.log` inside that directory. Builds do not move or delete data.

## Manual version changes

Desktop update checks, update controls, managed installation, and automatic
backup/rollback are removed. Build and open each new version manually. The
app is locally ad-hoc signed, not notarized or portable.

For an existing managed installation, first let active work finish, quit the old
app, and gracefully stop its backend before opening the replacement. Reuse the
exact data directory recorded by the old installation:

```sh
SAPIENS_DATA_DIR="/absolute/path/to/existing/data" ./environments/macos/build.sh
open .build/macos/Sapiens4.app
```

Keep the old data directory and backups. The new build does not read or modify
old managed releases, and an old Dock shortcut still opens the old app; pin the
new app instead. Never run two backends against the same data directory.

## Workspace and verification

Native tabs support links, local files, bookmarks, back/forward, reload and zoom.
WebKit tabs keep their own browser sessions. File selection, editing shortcuts,
external links, themes and the favicon-derived Dock icon remain available.

```sh
ruby submodules/libs4/ruby/bundle.rb exec rspec spec/distribution_spec.rb spec/workspace_accessibility_spec.rb
codesign --verify --strict .build/macos/Sapiens4.app
```

Tests use temporary data and controlled model executables; see
[coverage and platform requirements](../../docs/testing.md).
