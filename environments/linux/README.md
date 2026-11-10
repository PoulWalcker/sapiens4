# Linux development preview

Sapiens4 has a GTK/WebKit desktop and a host that can run without a display.
Desktop control currently requires **X11**. GNOME is not required, and Wayland
control is unsupported. On Ubuntu, use an available Xorg login session or an
X11 desktop such as XFCE. The driver rejects unsupported sessions rather than
attempting input through Xwayland.

## Build and run

Use the pinned toolchains and libs4 dependencies in the root README. On Ubuntu
22.04 or 24.04 amd64, install the native build prerequisites:

```sh
sudo apt-get install -y build-essential pkg-config libgtk-3-dev libwebkit2gtk-4.1-dev libjson-glib-dev libatspi2.0-dev libx11-dev at-spi2-core dbus-user-session dbus-x11 libglib2.0-bin
runghc build/Build.hs --desktop
submodules/blindly4/linux/build.sh release
SAPIENS_LINUX_BACKEND=desktop ./environments/linux/start.sh
```

Without `--desktop`, the build produces the host without the GTK executable.
The model CLI needs separate installation and authentication; tests use local
model fixtures. `SAPIENS_DATA_DIR` overrides the XDG data directory, and an
explicit `--data-dir DIRECTORY` takes precedence.

Desktop control uses AT-SPI accessibility, XRes window ownership, EWMH activation
and XTest input. PID, focus and exact-draft checks remain required for actions.
Editable AT-SPI controls support Unicode. For browser editors that lack a working
editable-text interface, explicitly set `BLINDLY4_LINUX_TEXT_INPUT=keyboard`;
this transport accepts printable ASCII up to 4096 bytes and verifies the result.
Unsupported text is rejected before clearing a draft. Clipboard access is not
implemented. See [driver behavior](../../submodules/blindly4/linux/README.md).

Native browser tabs open on selection, preserve per-Sapi cookies across restarts,
and unload expired hidden text previews. Active tabs, history and downloads are
protected from idle unloading.

## Tests

```sh
sudo apt-get install -y xvfb xauth openbox x11-utils libgtk-4-dev
sh environments/linux/qualify.sh .build/qualification-x11
```

This runs the suite using the existing `virtual.sh` Xvfb/Openbox launcher and
records revisions, binary hashes, logs and JSON results in a new directory.
Desktop suites run serially against local fixtures. For a focused run, use:

```sh
SAPIENS_TEST_LINUX_DESKTOP=1 SAPIENS_TEST_LINUX_AUTOMATION=1 \
  sh environments/linux/virtual.sh ruby submodules/libs4/ruby/bundle.rb exec rspec spec/linux_automation_spec.rb spec/linux_browser_spec.rb
```

External browser tests additionally require Chromium and Firefox and
`SAPIENS_TEST_LINUX_EXTERNAL_DESKTOP=1`. Executables can be selected with
`SAPIENS_TEST_CHROMIUM_EXECUTABLE` and `SAPIENS_TEST_FIREFOX_EXECUTABLE`.
`SAPIENS_TEST_SCALE=2 GDK_SCALE=2` enables integer scaling checks. Physical desktop,
fractional/mixed-monitor scaling and managed-browser coverage remain pending.

## Debian package

Build the desktop and release driver against Ubuntu 22.04 libraries, then run:

```sh
sh environments/linux/package.sh .build/sapiens4.deb
```

The script checks glibc compatibility and includes dependency notices. The package
contains no GNOME extension or input helper and installs no background service.
To test an extracted package, set `SAPIENS_TEST_LINUX_RUNTIME` to its `opt/sapiens4`
directory. Package installation/upgrade tests require a disposable filesystem;
see `spec/linux_package_spec.rb` for the explicit opt-in settings.
