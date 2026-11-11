# Testing

RSpec owns integration scenarios and assertions. Exercise public HTTP, processes,
files, relay traffic and native user interactions through Blindly4. Do not execute
JavaScript, inspect private application state or assert DOM/CSS implementation.
Fixtures use temporary data and controlled external model executables, never live
corpora or paid model calls. Dependencies stay in libs4.

Domain checks also run through public endpoints and saved installation fixtures:
invalid states and payloads, nonempty histories, retry cleanup, completion criteria,
and compatibility of stored data and native browser commands.

## Setup

Use GHC 9.14.1, Cabal 3.18.2, Ruby 3.4 and Node.js 22.5+:

```sh
git submodule update --init --recursive
ruby submodules/libs4/ruby/bundle.rb install
runghc build/Build.hs
ruby submodules/libs4/ruby/bundle.rb exec rspec
```

Scheduling cases use real minute pulses; `--tag '~slow'` is useful for iteration,
but run the complete suite before submitting. Source-release and platform build
checks are included. Set `SAPIENS_TEST_CABAL_CACHE` to the absolute path of an
existing `submodules/libs4/haskell/.cabal` cache to reuse dependencies in source-release tests.

## Native desktop tests

Use an unlocked desktop with accessibility permission. Run serially and avoid
interacting with fixture windows. On macOS:

```sh
./submodules/blindly4/macos/build.sh
ruby submodules/libs4/ruby/bundle.rb exec rspec
```

On Windows, use the same Haskell, Ruby and Node toolchains:

```powershell
git submodule update --init --recursive
ruby submodules/libs4/ruby/bundle.rb install
runghc build/Build.hs
$env:SAPIENS_TEST_CODEX = (Resolve-Path .build/bin/sapiens-provider.exe).Path
$env:SAPIENS_TEST_BLINDLY4 = 'C:\path\to\blindly4.exe'
ruby submodules/libs4/ruby/bundle.rb exec rspec
```

Set `SAPIENS_TEST_WINDOWS_PACKAGE` to a package built with `environments/windows/build.ps1`
to include packaged-runtime checks. Test ARM64 and x64 on matching systems.
Use `--tag desktop` for focused UI runs. Desktop tests automatically use the bundled
Blindly4 build on macOS and Windows; `SAPIENS_TEST_BLINDLY4` optionally selects a
different executable. A missing driver or a permission or interaction error fails
the test instead of silently skipping it. Unsupported platforms and Windows-only
contracts remain pending on macOS.
Failed desktop scenarios write accessible content to `tmp/desktop-failure.txt`
and a separate accessibility-tree JSON file under `.build/` for each failed example.

The Windows UI build limits the PureScript compiler to one runtime capability,
including compiler calls made by Spago. This mitigates intermittent compiler
access violations seen with the x64 compiler under Windows ARM emulation;
it does not limit the application runtime.

On Linux, build with `runghc build/Build.hs --desktop` and build the Blindly4
Linux driver. Desktop control requires X11; Wayland is unsupported. Run the
Linux and shared desktop suites on an isolated software-rendered X11 display:

```sh
sh environments/linux/qualify.sh .build/qualification-x11
```

The runner uses Xvfb and Openbox, and preserves logs and revision details in a
new report directory. Install the prerequisites in
[the Linux guide](../environments/linux/README.md). External Chromium/Firefox
checks are opt-in with `SAPIENS_TEST_LINUX_EXTERNAL_DESKTOP=1`. Tests exercise
real GTK/WebKit controls and generated computer commands against local fixtures.
Run desktop suites serially. Virtual displays do not establish physical-display
or mixed-monitor coverage.

To exercise a desktop package, extract it with `dpkg-deb --extract` and set
`SAPIENS_TEST_LINUX_RUNTIME` to the extracted `opt/sapiens4` directory before
running the same suite. Repeat in Ubuntu 22.04 and 24.04 userspaces.

Use mouse clicks to establish that controls are reachable; accessibility
invocation alone can activate offscreen controls. Rediscover moving targets and
verify editor focus and exact values. Retry only observations or rejections known
to occur before input; never replay an action after an uncertain outcome.

For an archived remote client, set `SAPIENS_TEST_LEGACY_REMOTE` to a directory
containing the relay's public shell (`index.html`, `sw.js`, and every asset listed
by that worker), then run `spec/legacy_remote_spec.rb` in the Linux desktop session.
The fixture serves those files locally, pairs through an isolated relay, and makes
shell downloads unavailable before pairing and reloading. It verifies compatibility
with a cached client without fetching production assets or using live credentials
during the test. Record the archive's URL, date and hashes alongside test results.
This is WebKit coverage of the `/ios` entrypoint, not a physical iOS installation.

## Coverage limits

The macOS and Linux fixtures run WebKit with the production browser adapter;
Windows runs MainWindow/WebView2. Remote-browser and HTTP-proxy window cases
currently require macOS or Linux Xorg. Reload tests do not establish persistence
across an actual app quit/upgrade.
Validate mobile installation, camera pairing, background/resume, offline launch,
updates, keyboard/rotation and touch behavior on real devices. Visually compare
appearance changes; behavioral tests do not prove styling equivalence.
