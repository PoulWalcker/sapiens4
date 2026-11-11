# D1 Architecture Plan V2 — implementation report

Historical snapshot at `52e2dfb`; subsequent fixes and current limitations are in
[the PR #63 review report](pr63-review-fixes.md).

2026-11-11. Branch: `codex/d1-v2`. No merge into `main`.
Primary reference: [approved investigation and V2 plan](https://chatgpt.com/share/6af422e0-9444-83ed-8808-11e0e74c14c0).

## Revisions and scope

- Latest upstream `main`, checked at start and finish: `f108e45843ab9817e9653d8467e0a5b21209530f`.
- Reference PR #60: `f9ed02dc8626853a22eaffbc2b7423c5bfebd19a`. The new branch starts from `main`, with selected files reused; no PR merge/cherry-pick.
- Distribution only: macOS ARM64 DMG, Windows x64 per-user Setup, separate owned backend, installation guards, dependency audits, signing policies and final checksums.
- No onboarding, authentication, permission or workspace-selection changes. D3 integration requirements are in [reference.md](reference.md#d1-integration-contract-for-d3).

## Reuse and simplification

Reused PR #60's native build fixes, separate-process adapters, recursive macOS relocation/signing/DMG operations, Windows WebView2 recovery and Inno installation policy. Retained baseline Codex invocation and installed-CLI discovery, removing version ranking/probing.

Replaced manifest-dependent path decisions with fixed `Sapiens4` defaults and explicit overrides. Completed stdout/stderr reader ownership, confirmed termination, and macOS deferred Quit. Its completion timer also runs in AppKit's [modal termination loop](https://developer.apple.com/documentation/appkit/nsapplication/terminatereply/terminatelater?language=objc). Windows rejects preview WebView2 versions and exposes repair after creation failure.

Consolidated 1,197 lines across the PR's Haskell packaging wrapper/modules into 310 lines in `build/Package.hs`. Assets come directly from `Assets.allAssets`. Kept recursive dependency closure, basename collision refusal, ARM64/macOS-floor checks, nested signing and exclusive output publication. Packaged console commands prefer their own executable-relative assets over unrelated working-directory content. Windows adds a PE import audit, static Rust CRT, and guarded installer destinations, including junctions and active-process path aliases.

Not carried over: bundled Codex/download locks, channels, portable ZIP deliverables, inventory/provenance/receipt schemas, ancestry checks, release aggregation, installer-tool downloading and the separate release-development document. No new CI or shared lifecycle framework. The tiny installation ownership marker remains. Native scripts publish only one installer plus `SHA256SUMS` into a new directory.

Only `libs4` changes: `64c62dbdd54bc52cce732526f4e822c9fbc0f4ff` → `6d25693b7e5a6a4dd04032cfdf49867560e7c36c`, identical to PR #60. It registers `RuntimePaths` and removes unused regex dependencies (two files, +5/−3). The upstream URL remains unchanged; Blindly's gitlink is unchanged.

## Build and artifact verification

| Platform | Result |
|---|---|
| macOS 26.6, Apple Silicon; GHC 9.14.1 / Cabal 3.18.2 | Native build and DMG passed architecture/minimum-OS/dependency audits, nested ad-hoc signature checks, image verification, mounted-copy verification and SHA-256 verification. |
| Windows Server 2022 x64; GHC 9.14.1 / Cabal 3.18.2 / Inno Setup 7.1.1 | Native build and 22,481,346-byte Setup.exe completed; all payload PE architectures/imports passed audit and final SHA-256 matched `SHA256SUMS`. Local installer is unsigned, as expected for local signing policy. |

macOS output: `.build/d1-v2-review/{Sapiens.dmg,SHA256SUMS}`. Windows VM output: `C:\Build\artifacts\d1-v2-review\{Sapiens-Setup.exe,SHA256SUMS}`. The checksum files identify the final installer bytes.

Existing-output refusal and missing-public-signing-credential refusal were checked; the existing DMG remained unchanged. Public builds require Developer ID/notarization or Authenticode credentials. No macOS signing identity was available, so publisher signing/notarization and public Windows signing remain unqualified.

## Tests deferred by user direction

All agent-authored tests were removed. Existing `spec/` files and `docs/testing.md` match the baseline exactly. No further tests or regressions are being written or run.

Historical checks before deferral passed: 104 backend/CLI examples (0 failures, 3 platform skips), one source-release scenario, and both macOS D1 scenarios, including spaced/Unicode installation paths. The added D1 test code was subsequently removed; these observations do not constitute committed acceptance coverage. Windows installer E2E and the final regression run remain deferred.

## LOC comparison

Physical added/removed lines, including blanks/comments, from the same `f108e458` base. Excludes gitlinks/submodule contents and generated outputs; includes this report. Test work is excluded from both sides in the fair implementation comparison because it was explicitly deferred.

| Scope | PR #60 net LOC | V2 net LOC | Reduction |
|---|---:|---:|---:|
| Implementation and non-test documentation (exclude `spec/`, `docs/testing.md`) | 1,595 | 646 | 949 (59.5%) |
| All changed text, including PR #60's test work | 2,015 | 646 | 1,369 (67.9%) |

No savings are credited to CI workflows or Fixed WebView2 removal; neither was present in PR #60.

## Remaining qualification

Installer E2E and regressions are deferred. Public signing, stapling and trusted-release installation need real credentials. Repeat acceptance on clean macOS 13+ and Windows 10/11 machines without build tools; Server 2022 is not Windows 10/11 qualification. Real absent-WebView2 recovery remains unqualified. External Codex updates remain a compatibility dependency. Windows process checks remain preflight safeguards; Finder replacement requires quitting first. Existing data is preserved in place, with explicit directory overrides for legacy locations; D1 performs no migration.
