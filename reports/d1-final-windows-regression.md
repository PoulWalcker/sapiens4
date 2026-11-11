# D1 final Windows x64 regression

Validation date: **2026-11-12, Asia/Dubai**. Process timestamps below are UTC (the preceding UTC calendar date). Execution used the existing Windows Server and its interactive desktop, not a macOS substitute or a CI-only build.

Small evidence: [observed-results transcription](evidence/d1-final-windows-observed-results.json). Raw Windows artifacts are identified below.

## Revision and isolation

- Fetched `PoulWalcker/sapiens4` source branch `dev/d1-distribution` before testing. Actual HEAD: **`6dc6cfab6b2ed0a90e4c3fc6004d02282902ce16`**, unchanged from the previously reviewed HEAD.
- Windows detached worktree: `C:/Build/d1-final-regression`; both the build and diagnostic recorded this revision.
- Pinned submodules: libs4 `1bf73bb56ffe487c4751cb73566d0f3402bc1ca4`; Blindly4 `cfc28cb5bcccbd58947bf13e80b79b76062b8dc9`.
- Report branch: `test/d1-final-regression`, based directly on that source commit. No production changes, merges, new regression suite, or new functionality.
- Existing GHC, Cabal, Node, Ruby, MSVC/Rust, and dependency caches were reused. The retained, signature-verified Inno Setup installer restored the required temporary packaging compiler. Application installations, profiles, configuration, and provider fixtures were disposable. The original QA and WebView2-investigation directories were left alone.
- Application and installer acceptance ran with `runas /trustlevel:0x20000`; the existing installed-spec and diagnostic guards rejected administrator-role membership and existing Sapiens registration. This was a restricted Basic User token in the existing Administrator desktop, not a newly provisioned standard-user account.

## Environment

| Item | Observed value |
|---|---|
| Windows | Microsoft Windows Server 2022 Datacenter, `10.0.20348` |
| Architecture | Native x64; GHC target `x86_64-unknown-mingw32` |
| WebView2 | `157.0.4341.42` |
| WebView2 executable | `C:/Program Files (x86)/Microsoft/EdgeWebView/Application/157.0.4341.42/msedgewebview2.exe` |
| Haskell / Ruby | GHC `9.14.1`; existing RubyInstaller `3.4.11-1-x64` |
| RSpec | `3.13.6` |
| Desktop control | Existing Hyonix interactive console, session **1**, `UserInteractive=True`, and the freshly built native Blindly4 |

## Build, packaging, and artifact

The actual build command was:

```powershell
./environments/windows/build.ps1 -Runtime win-x64 -Version 0.1.0 -OutputDirectory .build/d1-final-regression
```

**PASS:** native Haskell application and test helpers, PureScript/UI, native Blindly4, recursive PE machine/import audit, runtime assembly, and Inno Setup packaging. The build rejects a non-x64 GHC target and audits every staged EXE/DLL against PE machine `0x8664` and resolved adjacent/inbox imports. This is an unsigned local installer; Inno's setup engine architecture is separate from the native x64 application payload.

The first attempt used PowerShell `*>` logging and stopped when a Cabal deprecation warning on stderr became `NativeCommandError`. The same unmodified build was rerun through `cmd.exe` with native stdout/stderr file redirection and completed. This was a logging/invocation issue, not an application regression.

| Artifact | Exact Windows path |
|---|---|
| Installer | `C:/Build/d1-final-regression/.build/d1-final-regression/Sapiens-Setup-win-x64.exe` |
| Manifest | `C:/Build/d1-final-regression/.build/d1-final-regression/SHA256SUMS` |
| Build stdout / stderr | `C:/Build/d1-final-regression/.build/final-build.out`, `final-build.err` |

Installer SHA-256, independently computed and equal to `SHA256SUMS`:

```text
75594a411414652f74dc6af8dddeb3bc64839c84807d5e6fdc1c177d4d8f1a38
```

Installed independence was checked beyond the source tests. The installer supplied `C:/Build/d1-final-package/Sapiens4.exe`. After the rapid-Quit check, this installation reopened the repaired case-C data from working directory `C:/Build/d1-final-extra`, with `PATH=C:/Windows/System32;C:/Windows`, `SAPIENS_RUNTIME_DIR` and `SAPIENS_REPOSITORY` unset, and the controlled provider copied outside the checkout. The working UI was observed. Host PID **3484** used the installed executable; backend PID **3296** used `C:/Build/d1-final-package/runtime/.build/bin/sapiens4.exe`. Seven backend/WebView2 command lines matched case C's data directory. No development tool was needed on the product's PATH. The checkout remained on disk; it was not physically removed.

## Existing regression results

Used the project's `submodules/libs4/ruby/bundle.rb exec rspec` runner, not replacement tests. Required environment:

```text
SAPIENS_TEST_SETUP=C:/Build/d1-final-regression/.build/d1-final-regression/Sapiens-Setup-win-x64.exe
SAPIENS_TEST_CODEX=C:/Build/d1-final-regression/.build/bin/sapiens-provider.exe
SAPIENS_TEST_BLINDLY4=C:/Build/d1-final-regression/submodules/blindly4/.build/windows/blindly4.exe
SAPIENS_TEST_WINDOWS_PACKAGE=C:/Build/d1-final-package
BUNDLE_PATH=C:/Build/qa64-a294cde/submodules/libs4/ruby/.gems
```

Runner configuration and Codex home were redirected to disposable directories. The installed-distribution file ran first, before creating the separate package installation, because it requires no existing Sapiens registration.

```powershell
ruby submodules/libs4/ruby/bundle.rb exec rspec spec/installed_distribution_spec.rb --format json --out .build/installed.json
ruby submodules/libs4/ruby/bundle.rb exec rspec spec/windows_startup_spec.rb spec/windows_package_spec.rb --format json --out .build/windows.json
```

| File | Examples | Failures | Skips/pending | Measured seconds |
|---|---:|---:|---:|---:|
| `spec/installed_distribution_spec.rb` | 2 | 0 | 1 | 19.2036398, RSpec run duration |
| `spec/windows_startup_spec.rb` | 2 | 0 | 0 | 0.4454551, sum of example times |
| `spec/windows_package_spec.rb` | 2 | 0 | 0 | 16.2097650, sum of example times |

The two Windows files ran together: **16.6575306 seconds**, 4 examples, 0 failures, 0 pending, 0 errors outside examples. The two RSpec invocations total **35.8611704 seconds**, **6 examples, 0 failures, 1 expected skip**. The skipped example is the macOS DMG lifecycle, not Windows coverage. Installed run seed: `27321`.

The startup file uses the source companion launcher and domain helper built at the tested revision. The installed-distribution file consumes the new Setup; the package file consumes an installation of that same Setup, including its embedded runtime. No broader regression suite was started.

Raw JSON and runner logs remain at `C:/Build/d1-final-regression/.build/{installed.json,installed.log,windows.json,windows.log}`.

## Mandatory WebView2 diagnostic A–E

Executed the existing diagnostic unchanged, under the restricted token:

```powershell
./spec/support/windows_webview2_cleanup.ps1 -Setup C:/Build/d1-final-regression/.build/d1-final-regression/Sapiens-Setup-win-x64.exe -Evidence C:/Build/d1-final-cleanup -Ruby C:/Build/tools/ruby/rubyinstaller-3.4.11-1-x64/bin/ruby.exe
```

**PASS: matrix exit 0, empty `matrix.err`, all five cases completed.** Each case installed into its own `C:/Build/d1-final-cleanup/case-<letter>/install`; its profile was `case-<letter>/data/WebView2/Shell`.

| Case | Executed sequence | Host PID | Backend PID | Main WebView2 PID / parent | After exit and uninstall |
|---|---|---:|---:|---|---|
| A | Obstructed profile → Cancel | 6816 | Not started | Not started | No owned process/window; uninstall complete |
| B | Obstructed profile → Retry still obstructed → Cancel | 7560 | Not started | Not started | No owned process/window; uninstall complete |
| C | Obstructed profile → remove obstruction → Retry → working UI → Quit | 6448 | 7300 | 7104 / 6448 | No owned process/window; uninstall complete |
| D | Normal startup → working UI → Quit | 3344 | 7880 | 3064 / 3344 | No owned process/window; uninstall complete |
| E | Normal startup → Quit → reinstall → uninstall | 2460 | 6356 | 9116 / 2460 | No owned process/window; reinstall/uninstall complete |

The diagnostic observes processes five seconds after host exit and five seconds after uninstall, before any optional fixture-window cleanup. Both observations were empty for every case. No survivor error window needed closing. A/B displayed Sapiens native recovery without starting WebView2 or the backend. A/B obstruction files retained exactly `D1-A` and `D1-B`; C's obstruction was intentionally removed for recovery. Recursive observation found **0** `.sapiens4-write-check*` files.

## Process ownership and unrelated-process control

Process evidence uses **PID plus creation time, executable and command/profile path**, with observed parentage; names alone were not the ownership verdict. Per-case `processes.json` includes these identities and window titles at launch, recovery, Retry, successful startup, host exit, uninstall and final observation.

Selected primary process creation records, in UTC:

| Case | Backend PID / created | WebView2 PID / created |
|---|---|---|
| C | 7300 / `2026-11-11T22:09:40.2332710Z` | 7104 / `2026-11-11T22:09:40.3390000Z` |
| D | 7880 / `2026-11-11T22:10:02.8444540Z` | 3064 / `2026-11-11T22:10:02.9840840Z` |
| E | 6356 / `2026-11-11T22:10:27.6346010Z` | 9116 / `2026-11-11T22:10:27.7942120Z` |

Backend executable paths were under the corresponding case's installed `runtime/.build/bin/sapiens4.exe`; WebView2 used the system executable recorded above and the corresponding case profile. Full child identities are retained in the raw snapshots.

A deliberately separate source-host session, using disposable `C:/Build/d1-final-control`, remained open during all A–E cases. Its host **1152**, backend **8868**, and WebView2 PIDs **6760, 7884, 4268, 7376, 8772, 5496** all retained the **same creation time, executable path and complete command line** afterward. Thus the diagnostic/shutdown/uninstall sequence did not terminate these unrelated WebView2 processes. Only after that comparison was the owned control host closed normally, with its identity rechecked.

Control evidence: `.build/control-before.json` and `.build/after-matrix-processes.json` in the Windows worktree. The package regression intentionally terminates its own attributed guest browser to test failure handling; this is separate from the unrelated control.

## Additional positive and negative observations

| Check | Execution and result |
|---|---|
| Rapid Quit during startup | Confirmed gap in the requested existing files; performed one standalone native observation, not another regression suite. `CloseMainWindow` at **70.0727 ms after Start-Process returned** was accepted. Host **8916**, created `2026-11-11T22:20:13.4602460Z`, exited **0** within the 35-second bound. At five seconds, the Sapiens/Codex/WebView2 inventory was empty. Evidence: `C:/Build/d1-final-extra/rapid.ps1`, `rapid.json`. |
| Retry while still obstructed | Existing case B passed, then Cancel exited cleanly. |
| Retry after failed startup | Installed spec and case C passed after removing only the fixture obstruction. |
| Backend exits before readiness | Existing startup spec passed using the real domain executable that immediately rejects the host arguments. |
| Reinstall while running | Existing installed spec observed Setup refusal, exit **7**, while the recovered app continued working. |
| Unowned nonempty destination | Existing installed spec observed refusal, exit **7**, with only the original `keep.txt` remaining. |
| Restart after recovery | The final installed app reopened case C's original repaired profile and reached the working UI; the installed backend path and profile were independently observed. |
| Chat and browser resilience | Existing installed/package specs passed controlled-provider chat and guest-browser-failure responsiveness. |
| User state through uninstall | Installed spec verified database SHA-256 and configuration/unowned-file contents across reinstall/uninstall. The final manual uninstall additionally compared the recovered database and disposable data/configuration markers. |

The final manual uninstall completed (`unins000.exe` absent). The recovered database and both data/configuration markers had identical SHA-256 values before and after uninstall. The final Sapiens/Codex/WebView2 process inventory was **empty**. The temporary packaging compiler was subsequently removed; test evidence and disposable retained user data were preserved.

## Independent lifecycle review

Reviewed the fix against `d5a35bb` and its immediate lifecycle interactions after execution. Production delta: **16 insertions, 1 deletion, two files**. It adds one small profile-preparation helper and calls it at the shell/backend boundary and immediately before WebView2 creation; there is no new process supervisor or process-name-based termination layer.

| Challenge | Reviewed references and execution evidence |
|---|---|
| Cancel / Retry / Quit races | [Shell.hs:84](https://github.com/PoulWalcker/sapiens4/blob/6dc6cfab6b2ed0a90e4c3fc6004d02282902ce16/environments/windows/Shell.hs#L84), [generation/recovery:145](https://github.com/PoulWalcker/sapiens4/blob/6dc6cfab6b2ed0a90e4c3fc6004d02282902ce16/environments/windows/Shell.hs#L145), [attempt guards:218](https://github.com/PoulWalcker/sapiens4/blob/6dc6cfab6b2ed0a90e4c3fc6004d02282902ce16/environments/windows/Shell.hs#L218). A/B/C, normal Quit and rapid Quit produced no survivor. |
| COM callback ownership / teardown | [Native.hs:150](https://github.com/PoulWalcker/sapiens4/blob/6dc6cfab6b2ed0a90e4c3fc6004d02282902ce16/environments/windows/Native.hs#L150), [creation and stale-controller handling:227](https://github.com/PoulWalcker/sapiens4/blob/6dc6cfab6b2ed0a90e4c3fc6004d02282902ce16/environments/windows/Native.hs#L227), [closeView:249](https://github.com/PoulWalcker/sapiens4/blob/6dc6cfab6b2ed0a90e4c3fc6004d02282902ce16/environments/windows/Native.hs#L249), [COM/apartment ordering:29](https://github.com/PoulWalcker/sapiens4/blob/6dc6cfab6b2ed0a90e4c3fc6004d02282902ce16/environments/windows/Shell.hs#L29). These ownership mechanisms are unchanged by the fix; observed process cleanup passed. |
| Failed initialization cleanup | [Native.hs:209](https://github.com/PoulWalcker/sapiens4/blob/6dc6cfab6b2ed0a90e4c3fc6004d02282902ce16/environments/windows/Native.hs#L209), [Shell.hs:230](https://github.com/PoulWalcker/sapiens4/blob/6dc6cfab6b2ed0a90e4c3fc6004d02282902ce16/environments/windows/Shell.hs#L230), [Host.hs:119](https://github.com/PoulWalcker/sapiens4/blob/6dc6cfab6b2ed0a90e4c3fc6004d02282902ce16/environments/windows/Host.hs#L119), [Browser.hs:305](https://github.com/PoulWalcker/sapiens4/blob/6dc6cfab6b2ed0a90e4c3fc6004d02282902ce16/environments/windows/Browser.hs#L305). A/B started neither browser nor backend; C recovered; no write-check residue. |
| Preflight / creation TOCTOU | [Native.hs:220](https://github.com/PoulWalcker/sapiens4/blob/6dc6cfab6b2ed0a90e4c3fc6004d02282902ce16/environments/windows/Native.hs#L220) checks immediately before loader/environment creation, in addition to the pre-backend check. These filesystem operations are not an atomic lock against external mutation; see limits below. No new TOCTOU defect was reproduced in this run. |
| Normal startup and scope | D/E, package specs, rapid Quit, and restart on the repaired profile passed. The production change is narrowly scoped; the larger branch additions are diagnostic/tests/documentation, not production lifecycle abstractions. |

**Confirmed defects: none.** No severity /10 is assigned and no speculative finding is promoted to a release defect.

## Coverage limitations and recommendation

- One Windows Server 2022 x64 configuration and one WebView2 version. Windows 10/11 client editions, ARM64, physical-device/GPU variants, missing-WebView2 installation UX, and other security policies were not qualified here.
- Restricted Basic User execution in the existing Administrator desktop is not a separate normal-user account qualification.
- Unsigned local Setup only; Authenticode/public release signing, SmartScreen reputation, and download trust UX were not exercised.
- A–E cover the confirmed profile obstruction and observed five-second cleanup windows, not every possible WebView2 failure or an exhaustive race stress campaign. No deliberate filesystem mutation was injected into the narrow interval after preflight, and no allocator/COM-reference leak instrumentation was added.
- RSpec uses the existing controlled external provider. No real authenticated Codex/model request or long-running Windows Codex child tree was introduced. Process inventories found no unexpected Codex/provider survivors; this does not establish all busy-model shutdown scenarios.
- Installed independence was demonstrated with an external working directory, installed executable/runtime paths, external provider, cleared source overrides and restricted PATH. The source checkout was still present on disk.
- Raw logs/JSON remain on the Windows machine at the exact paths above. The small committed evidence is a transcription of the observed results, not a claim that all raw artifacts were copied off the server.

**Recommendation: GO for the tested D1 Windows x64 candidate on this Windows Server configuration.** The required process-cleanup gate passed independently of RSpec: A–E left no attributable error window or WebView2/backend survivor, and the unrelated WebView2 control remained intact. This is not a blanket Windows 10/11 or public-signing qualification.
