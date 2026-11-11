# PR #63 review and fixes

Reviewed against `PR63_review_and_fix_prompt.md` on 2026-11-11. The document was
treated as an investigation checklist; its claims were rechecked against source.
Tests were neither added nor run. Regressions and installer E2E remain deferred
by the user's instruction.

## Revisions and scope

The reviewed PR base and current upstream `main` were both
`f108e45843ab9817e9653d8467e0a5b21209530f`; the reviewed PR head was
`52e2dfbd1a96c793a1738dec4c5c118a989cea37`. Fixes are on `codex/d1-v2`, in:

- `6cc60db`: external Codex discovery and Windows native packaging targets.
- `065bf93`: cancellation after the final scheduler tick.
- `e76e11e`: responsive AppKit startup and retained backend supervision.

No additional submodule change was needed: the existing pinned `libs4`
`6d25693b7e5a6a4dd04032cfdf49867560e7c36c` already provides x64/ARM64 WebView2
loader preparation; the pinned Blindly script already accepts both targets.
Existing native build, private stdout readiness, graceful-stop protocol,
installation guards, dependency checks and signing machinery were reused.

The backend remains a separate owned process. Codex remains externally installed
and authenticated. No onboarding, authentication, permissions or workspace
selection implementation changed. D3 must use the existing directory overrides
and provider interface; directory overrides do not isolate Codex credentials.
No new CI, release framework, compatibility layer or shared lifecycle framework
was added.

## Standards

The independent standards review and re-review found **0 documented-standard
violations and 0 actionable code-smell findings**. Small platform-specific
lifecycle adapters are deliberate: a new shared abstraction would expand V2
without simplifying the native boundaries. No broad cleanup was requested.

## Spec

Independent source review confirmed A1–A5. A1 is inherited from `main`;
A2/A3/A5 are PR regressions; A4 is a revised requirement conflict rather than
evidence of a previously working ARM64 installer. B2 and B3 are established
from source ordering; B3 also applies to Windows. B1 remains an unverified
platform concern. C1 is optional and was left alone.

| Finding | Changed source | Why it works | Risk and validation limits |
|---|---|---|---|
| A1: inherited npm executable suffix | [Provider.hs:105](../corpora/work/Provider.hs#L105) | Resolves `vendor/<target>/bin/codex.exe`, matching the published v0.167.1 package. | Low. Package metadata was checked; real authenticated Windows Codex invocation was not exercised. |
| A5: `.cmd` override | [Provider.hs:112](../corpora/work/Provider.hs#L112) | Missing overrides produce no candidates. A found `.cmd` uses the resolved executable's directory; other overrides use the resolved executable itself. | Low. Source review confirms override precedence and fail-closed behavior; no mock resolver tests were added. |
| A3: direct Windows build loader | [Build.hs:67](../build/Build.hs#L67), [Windows packaging:75](../environments/windows/build.ps1#L75) | Ordinary native builds prepare the matching loader with the existing pinned script. Packaging consumes that result instead of preparing it again. | Low. Direct Windows build succeeded after the old loader was moved aside; loader was recreated. Dependency caches were reused, so this was not a fully clean checkout build. |
| A4: native Windows ARM64 target | [Windows packaging:14](../environments/windows/build.ps1#L14), [PE audit:35](../environments/windows/build.ps1#L35), [installer:10](../build/package/windows-installer.iss#L10) | Validates Runtime against the native GHC target, propagates it to Blindly, selects matching WebView2 through the common build, checks PE Machine values, and sets Inno restrictions and architecture-specific artifact names. Identity and installation policy stay common. | Partial qualification. x64 build can be checked on the available VM; native ARM64 compilation/installation needs a matching compiler and device. No ARM64 artifact is claimed. |
| A2: blocking AppKit startup | [Application.hs:54](../environments/macos/Application.hs#L54), [289](../environments/macos/Application.hs#L289), [314](../environments/macos/Application.hs#L314), [Browser.hs:50](../environments/macos/Browser.hs#L50) | Initializes native UI before starting an owned worker on the first timer tick. Publishes process ownership under `mask` immediately after spawn. Readiness/guest fetch happen off the UI thread; Ready/Failed callbacks run through the native timer. Quit cancels and joins startup before stopping its child. The stored Cocoa URL is retained across autorelease pools. | Medium. Native build/DMG passed and early Quit was manually verified with readiness delayed. Startup failure and concurrent termination combinations still need the deferred E2E work. An uncooperative child can trigger two bounded stop attempts during startup cancellation. |
| B2: final scheduler admission | [Service.hs:113](../corpora/host/Service.hs#L113) | Repeats `stopRuns` after joining the scheduler; a final tick can no longer register work after the last cancellation snapshot. Existing run registration/stop machinery remains unchanged. | Low. Ordering follows the actual scheduler lock/registration path; no forced-interleaving runtime test was run. |
| B3: failed stop loses supervision | [Application.hs:99](../environments/macos/Application.hs#L99), [Windows Host.hs:78](../environments/windows/Host.hs#L78), [127](../environments/windows/Host.hs#L127) | Disposes output readers/handles and clears the owner only after confirmed process exit. A stop failure retains the child reference and stdout/stderr supervision. Windows also publishes ownership before cancellable initialization. | Low to medium. Both native adapters compile. An unkillable-child timeout was not reproduced; the former unconditional `finally` cleanup is demonstrably removed. No data-loss claim is made. |
| B1: Hardened Runtime microphone entitlement | No speculative change | Public signing uses Hardened Runtime and no audio-input entitlement, but this alone does not demonstrate failure of this WKWebView capture path. | Unverified. Local DMG is ad-hoc signed without Hardened Runtime; a publisher-signed artifact and clean-consent voice capture are still required. |
| C1: unused path/runtime arguments | Unchanged | Core fixes do not require touching these interfaces. | Optional cleanup deferred. |

Review count: Standards **0**; Spec **7 established findings** (A1–A5/B2/B3),
with ARM64 qualification still partial; B1 is an unverified concern and C1 an
optional cleanup. No new blocking source finding arose during re-review.

## Checks actually executed

- macOS native `runghc build/Build.hs`, GHC 9.14.1: passed after the lifecycle
  changes. Build log: `/tmp/pr63-macos-build.log`.
- `environments/macos/build.sh 0.1.0 .build/pr63-review`: passed. Existing
  architecture/minimum-OS/dependency audits, nested ad-hoc signature checks,
  DMG verification and mounted-copy verification completed. A separate
  `shasum -a 256 -c SHA256SUMS` returned `Sapiens.dmg: OK`.
- Manual macOS early Quit: copied the mounted app into `.build/pr63-manual`,
  launched with explicit disposable data/config directories, and paused only
  its owned backend before readiness. The Starting window and native Quit menu
  were responsive. Quit was requested, the paused child resumed, and both app
  and backend exited; native launcher exit status was 0. No test file was made.
  CUA also relaunched an inspection instance with the default profile; that
  instance was closed, and no user data was deleted.
- Published Codex v0.167.1 registry/UNPKG metadata confirmed the Windows x64
  optional package and its `vendor/x86_64-pc-windows-msvc/bin/codex.exe` entry.
  No Codex binary was bundled, downloaded or authenticated for this check.
- Windows Server 2022 x64, GHC 9.14.1: direct `runghc build/Build.hs` at
  `e76e11e` returned 0 and recreated the 194,912-byte `WebView2Loader.dll` after
  the previous copy had been moved aside. Log: `C:\Build\pr63-direct-build.log`.
- Windows `environments/windows/build.ps1 -Runtime win-x64 -Version 0.1.0
  -OutputDirectory C:/Build/artifacts/pr63-x64` at `e76e11e` returned 0 with
  Inno Setup 7.1.1. Native payload architecture/import audits passed. The final
  unsigned `Sapiens-Setup-win-x64.exe` is 22,465,180 bytes; its SHA-256 matched
  `SHA256SUMS`. Log: `C:\Build\pr63-package.log`. This was build/artifact
  validation on Server 2022, not installation/runtime qualification on Windows
  10/11. No installer E2E was run.
- Requesting `win-arm64` with the VM's `x86_64-unknown-mingw32` GHC returned 1
  with an explicit target mismatch; no output directory was created.
- `git diff --check` passed. Existing `spec/` and `docs/testing.md` match
  `52e2dfb` exactly. No unit tests, E2E suites or regressions were run.

## Native target and signing limits

[GHC 9.14.1's official bindists](https://downloads.haskell.org/~ghc/9.14.1/)
provide Windows x64, not a native Windows ARM64 compiler. The script supports a
matching native ARM64 compiler, but the pinned Haskell/native dependencies and
runtime need actual ARM64 build/device qualification. Cross-target requests
with an x64 compiler must fail before publishing an artifact.

Inno's existing default x86 setup engine is retained. Its
[documented engine architectures](https://jrsoftware.org/ishelp/topic_setup_setuparchitecture.htm)
are x86/x64; the x86 engine runs through Windows ARM64 emulation, while the
application payload must be native ARM64. The selected
[architecture restriction](https://jrsoftware.org/ishelp/topic_setup_architecturesallowed.htm)
is `arm64` or `x64compatible`. PE payload checks use the
[documented Machine field](https://learn.microsoft.com/en-us/windows/win32/debug/pe-format),
not a tool-specific objdump format label.

The microphone concern requires a real publisher-signed Hardened Runtime app.
Apple documents the
[audio-input entitlement](https://developer.apple.com/documentation/bundleresources/entitlements/com.apple.security.device.audio-input)
for recording audio; this is supporting platform guidance, not an observed
Sapiens voice failure. Developer ID/notarization, Authenticode and clean-consent
voice capture remain unqualified. No D3 permission flow was changed.

## Size and simplification

Physical added/deleted lines, including comments and blanks, compared with
reviewed head `52e2dfb`; generated outputs and gitlinks excluded:

| Source area | Added | Removed | Net |
|---|---:|---:|---:|
| Core: Provider/Service | 7 | 3 | +4 |
| macOS native adapter | 92 | 39 | +53 |
| Windows native adapter | 8 | 2 | +6 |
| Build/packaging: Build.hs, PowerShell, Inno | 40 | 14 | +26 |
| Total production source | 147 | 58 | +89 |

Removed duplicate Windows loader preparation and unconditional failed-stop pipe
cleanup. Reused platform scripts and the existing AppKit pending-action timer;
there is no new lifecycle module, startup ADT solely for style or orchestration
framework. Documentation is counted separately from production changes (net 160 lines).

For comparison with PR #60, both branches use `f108e458` as the base and exclude
`spec/`, `docs/testing.md`, gitlinks and generated outputs. Including non-test
documentation and this report, PR #60 adds net **1,595** lines and current V2 adds
net **895** lines, a reduction of **700** lines (**43.9%**).
