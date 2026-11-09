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
./environments/windows/build.ps1 -BuildOnly
$env:SAPIENS_TEST_CODEX = (Resolve-Path .build/bin/sapiens-provider.exe).Path
$env:SAPIENS_TEST_BLINDLY4 = 'C:\path\to\blindly4.exe'
ruby submodules/libs4/ruby/bundle.rb exec rspec
```

Set `SAPIENS_TEST_WINDOWS_PACKAGE` to a package built with `environments/windows/build.ps1`
to include packaged-runtime checks. Set `SAPIENS_TEST_INSTALLER` to the Setup.exe
and optionally `SAPIENS_TEST_INSTALLER_EVIDENCE` to a log directory to run the
existing install/reinstall/active-refusal/uninstall scenario through RSpec. Use a
disposable Windows account without an existing development installation. Test ARM64 and x64 on matching systems.
Use `--tag desktop` for focused UI runs. Desktop tests automatically use the bundled
Blindly4 build on macOS and Windows; `SAPIENS_TEST_BLINDLY4` optionally selects a
different executable. A missing driver or a permission or interaction error fails
the test instead of silently skipping it. Unsupported platforms and Windows-only
contracts remain pending on macOS.
Failed desktop scenarios write accessible content to `tmp/desktop-failure.txt`.

Use mouse clicks to establish that controls are reachable; accessibility
invocation alone can activate offscreen controls. Rediscover moving targets and
verify editor focus and exact values. Retry only observations or rejections known
to occur before input; never replay an action after an uncertain outcome.

## Coverage limits

The macOS fixture runs WebKit with the production browser adapter; Windows runs
MainWindow/WebView2. Remote-browser and HTTP-proxy window cases currently require
macOS. Reload tests do not establish persistence across an actual app quit/upgrade.
Validate mobile installation, camera pairing, background/resume, offline launch,
updates, keyboard/rotation and touch behavior on real devices. Visually compare
appearance changes; behavioral tests do not prove styling equivalence.
