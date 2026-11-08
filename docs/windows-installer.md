# Native Windows per-user installer preparation

Prepared for the existing D1 lane, 2026-11-08. This is a concrete implementation
approach, not a generated or executed installer. The portable ZIP and Mac parser
checks do not establish Windows installation acceptance. No remote workflow,
account, release, real installation or user data was touched.

## Choice and ownership

Use the already researched **Velopack 1.2.161** C ABI/native package engine and its
per-user Setup.exe. Keep `com.poulwalcker.sapiens4.development`, display name
`Sapiens4 Development`, development channel and win-x64. Do not add MSI/NSIS,
a separate launcher service, another updater or an independent component version.
The existing R1 packaging assessment and R3 actual replacement experiment are the
basis; the Windows research fixture remains unexecuted.

The versioned package contains the existing native shell, owned backend, runtime,
complete D3 provider and notices. Velopack owns installation layout, stable launch
stub/shortcuts and eventual payload replacement. Sapiens keeps update consent,
admission/drain, full-state snapshot, migration, health and recovery ownership.
This preparation introduces no update check, download, apply or migration call.

## Smallest source slice

1. D1 adds a bootstrap call as the first action of existing
   `environments/windows/WindowsMain.hs`, before Shell/UI/host/data setup. Add
   dynamic C bindings in existing `Native.hs`, loading the exact bundled x64
   `velopack_libc_win_x64_msvc.dll` by executable-relative absolute path. Pass
   UTF-8 process arguments, call `vpkc_app_set_auto_apply_on_startup(false)`, then
   `vpkc_app_run(NULL)`. Fast installer hooks terminate before ordinary startup.
   Do not register callbacks that write corpus data or perform migrations.
2. Source/ordinary portable development can omit the engine only when no managed
   `sq.version` is present. A managed package missing the DLL/entrypoints fails
   clearly before host launch. Bundle the matching complete engine license.
3. Extend D1's existing Windows assembly to acquire/checksum-pin the C ABI asset,
   add its DLL before payload signing, and verify the native bootstrap through
   the packer's normal compatibility check. Do not reuse the research fixture's
   `--skipVeloAppCheck`; that bypass is not acceptable for the real installer.
4. Add one native installer packaging entrypoint consuming that complete,
   verified directory. Keep portable diagnostics as a separate output. Generate
   a full package and Setup.exe, with delta generation off initially. Do not run
   `vpk upload`, create release objects or activate an update feed.

The existing Windows modules can host this small dynamic adapter. **No new
Build.hs/Cabal/libs4 registration is required by this approach.** If implementation
finds a need for static linking/new module registration, main owns those shared
files and D1 must report the exact patch before edits. Main owns the combined
checkout and provider integration; D1 must not copy changes into it independently.

## Explicit choices and remaining integration decisions

| Item | Current proposal choice | Remaining work or decision |
|---|---|---|
| Installer | Velopack 1.2.161 per-user Setup.exe, x64; existing portable ZIP retained for diagnostics | Implement native bootstrap/assembly, then execute actual installation |
| Identity/channel | Existing development package ID/name/channel above | No production identity or feed promotion in this wave |
| Startup ownership | Dynamic C ABI in existing WindowsMain/Native, startup auto-apply disabled | Confirm native hook/compatibility-check behavior; no Build.hs/libs4 edit planned |
| WebView2 | Keep existing native Install/Retry/Cancel UX | Native absent/install/retry/cancel acceptance; no second prerequisite flow |
| Package replacement | Full packages, delta off initially | Main's Sapiens lifecycle/recovery transaction remains authoritative; D2 stays research-only |
| Persistent state | Existing external development data/config roots | No adoption/migration or legacy routine semantics changes |
| Publisher signing | Existing certificate-thumbprint/SignTool integration plus engine-generated artifact signing | Main/operator must supply expected publisher/profile when signing is authorized; no signing account chosen or operated here |
| Release metadata | Preserve current two-ZIP contract until explicitly extended | Main/D1 must bind actual native Setup/full-package/feed output names and final digests in one reviewed asset-set change before publication |

Windows x64 and Windows 11 ARM x64-emulation acceptance remain required checks,
not optional product-scope decisions. A VC runtime dependency is an empirical PE
import/clean-target question; configure it only if actual evidence requires it.
No further installer framework or update owner needs selection.

## Operator-ready native validation setup

Use a disposable existing Windows test context with synthetic state and an
unlocked desktop. These are prepared commands, not steps executed by this report.
The Windows checkout must be main's frozen combined source (including D3's
Windows environment/credential fixes), not the D1-only branch. Keep GHC, Make and
shell paths free of spaces. Reuse the version/toolchain setup in
[the Windows README](../environments/windows/README.md): x64 GHC 9.14.1, Cabal
3.18.2, Node 22.5+, Ruby 3.4, Python 3.10+, Rust 1.99.0/MSVC, Git, sh and Make.
Record tool versions, source SHA/patch receipt, exact submodule pins and OS/process
architecture; do not collect complete environment variables or credentials.

Transfer the existing handoff `results/assessment_v2/windows_probe/` as a whole,
and D3's complete verified `windows-codex/` directory including its metadata.
The acquired Windows archive is already retained by D3: SHA256
`60567ca940bbaf6c10501805d9909d1328179231e2d47a0deec7a1368bb21641`,
201675464 bytes. D1 independently rechecked its archive digest and all 48 upstream
file hashes on November 8. This verifies inputs, not Windows execution. Do not
copy only codex.exe. Existing package staging will reverify all provider bytes.

In x64 Windows PowerShell as a normal user, first run the existing fixture:

```powershell
$Checkout = 'C:\sapiens4-combined'
$Handoff = 'C:\sapiens4-handoff'
$Provider = 'C:\verified\windows-codex'
powershell.exe -NoProfile -ExecutionPolicy Bypass -File `
  (Join-Path $Handoff 'results\assessment_v2\windows_probe\Run-Probe.ps1')
```

Read the existing probe README and return its printed result ZIP, including
failures. It uses its own synthetic tree, pinned downloads and built-in fixture
compiler; it does not install Sapiens. It requires an AMD64/x64 process and does
not itself certify native ARM64. Do not substitute a Mac parser result for this
run or count result-ZIP presence as success. Its A/B/corrupt-package observations
remain research evidence, not production update/migration acceptance.

Then validate the combined native portable app with the already acquired provider:

```powershell
Set-Location -LiteralPath $Checkout
$Evidence = Join-Path $env:TEMP ('Sapiens D1 evidence ' + [Guid]::NewGuid())
New-Item -ItemType Directory -Path $Evidence | Out-Null
$env:SAPIENS_CONFIG_DIR = Join-Path $Evidence 'synthetic config'
$env:SAPIENS_DATA_DIR = Join-Path $Evidence 'synthetic data'
ruby submodules/libs4/ruby/bundle.rb install
if ($LASTEXITCODE -ne 0) { throw 'Ruby setup failed' }
$Package = Join-Path $Checkout ('.build\native-validation-' + [Guid]::NewGuid())
.\environments\windows\build.ps1 -Runtime win-x64 -Version 0.1.0-dev.20261108 `
  -ProviderDirectory $Provider -OutputDirectory $Package
if ($LASTEXITCODE -ne 0) { throw 'Native package build failed' }
$env:SAPIENS_TEST_PACKAGE_RUNTIME = Join-Path $Package 'runtime'
$env:SAPIENS_TEST_CODEX = Join-Path $Checkout '.build\bin\sapiens-provider.exe'
$env:SAPIENS_TEST_WINDOWS_PACKAGE = $Package
ruby submodules/libs4/ruby/bundle.rb exec rspec spec/native_package_spec.rb `
  --format documentation --out (Join-Path $Evidence 'package-rspec.txt')
if ($LASTEXITCODE -ne 0) { throw 'Installed runtime acceptance failed' }
ruby submodules/libs4/ruby/bundle.rb exec rspec spec/windows_package_spec.rb `
  spec/windows_startup_spec.rb --format documentation `
  --out (Join-Path $Evidence 'desktop-rspec.txt')
if ($LASTEXITCODE -ne 0) { throw 'Native desktop acceptance failed' }
```

These existing RSpec scenarios use synthetic state/controlled model executables.
The package scenario uses the real bundled CLI only for `--version`. Keep desktop
runs serial. Use the normal Explorer launch and existing prerequisite controls in
an appropriate fresh VM state for missing-WebView2 cases; do not remove a working
runtime from a personal machine. Do not start account login or submit a real
model request as part of these checks. Preserve outputs/logs on failure and report
which dependent steps stayed NOT_RUN; do not replace the expected behavior with
mock-success results.

**Concrete D3/main review fix before API acceptance:** the currently supplied
`D3_WINDOWS_ACCEPTANCE.ps1` hard-codes 48 provider inventory entries. D1 finalizes
48 upstream Windows content files plus `sapiens-provider-upstream.json`, so the
active final inventory has 49 entries, whether the package is unsigned or signed.
D3 should make its operator script understand the preserved upstream/final
inventory distinction and reuse `package.py verify` for final package hashes.
Do not delete metadata or bypass the count/check to make the script pass. Run that
existing script against the final `$Package` only after this fix is integrated;
it must continue to avoid login/model requests. D1 has not edited D3's script.

Only after the native bootstrap/assembler exists should the Setup command below
be exercised. Until then, installer install/reinstall/uninstall and generated
helper signing remain NOT_RUN. For every actual native run retain source/pins,
manifest/buildId, final receipts, package/public-route assertions, owned-process
exit evidence, prerequisite results and sanitized failure logs under `$Evidence`.
Record Windows 11 ARM x64-emulation separately from x64-PC results. Reuse these
existing scenarios and the probe; no second PowerShell test framework is needed.

## Fixed builder inputs and prepared command

Both are pinned official release assets already recorded in assessment v2:

| Input | SHA256 |
|---|---|
| `velopack_libc_1.2.161.zip` | `31de8f2cf05a47bd5328a9f98b0c41aec1582156f6075f1058b26941f7f17c20` |
| `vpk.1.2.161.nupkg` | `2b56ce117f803fc70c103cb423bd040e395e40370f6ff6e10818e9ff9c26a323` |

Use source commit `92d6a1c91716729d449034df5c50307dcce39493`. The builder can use
an isolated .NET runtime matching the researched vpk target; the application and
installer must not add a .NET requirement for end users. Audit the engine DLL's
actual Windows PE imports and verify clean-user startup before deciding whether
any VC runtime bootstrap is needed. No prerequisite result is inferred from Mac.

After the bootstrap exists and a native build has passed, the prepared local
unsigned assembly command is:

```powershell
& $DotnetPath $VpkDll pack `
  --packId com.poulwalcker.sapiens4.development `
  --packVersion $Version --packTitle 'Sapiens4 Development' `
  --packDir $VerifiedPackageDirectory --mainExe Sapiens4.exe `
  --runtime win-x64 --channel development --delta None --noPortable `
  --outputDir $NewInstallerOutput
if ($LASTEXITCODE -ne 0) { throw 'Native installer packaging failed' }
```

This is not ready to execute against the current native binary: the bootstrap
source slice above has not been implemented or native-tested. Do not bypass the
compatibility check to produce a seemingly successful installer.

## Layout, signing and immutable inventories

The package engine's per-user install root is separate from the existing data
root: `%LOCALAPPDATA%\com.poulwalcker.sapiens4.development\current` contains
`Sapiens4.exe` and sibling `runtime`; its parent holds Update.exe and the stable
launch stub. The selected pack title controls the stub/shortcut display name;
verify its exact generated name on Windows. Config/data stay under
`%LOCALAPPDATA%\Sapiens4 Development\config` and `data`, outside replaceable
`current`. No import, epoch stamping or production-install discovery occurs.
Uninstall must leave these external data roots intact unless a later explicit
user action requests removal.

The pinned Windows packer copies the input directory, adds sq.version,
Squirrel.exe and an execution stub, signs PE files, builds packages, then signs
Setup.exe. Preserve the final runtime inventory across that composition:

- Sign and verify the entire application/provider payload first; refresh provider
  and runtime inventories using the current D1 finalizer.
- During a genuinely signed installer build, use the pinned packer's
  `--signExclude '[\\/]runtime[\\/]'` with its reviewed signing parameters so
  already finalized runtime bytes cannot be re-signed. Sign generated engine/stub
  and Setup artifacts through the engine. Verify their expected publisher and
  timestamps; do not rely only on the engine's trusted-signature skip heuristic.
- Extract the final full package and run `package.py verify` against its runtime,
  then verify every shipped PE publisher signature, including engine-generated
  code. Any changed finalized byte rejects the candidate. If the packer mutates
  runtime content despite exclusion, stop and integrate finalization at its final
  composition boundary; never ship stale inventories.
- Compute final Setup/full-package/feed SHA256 and sizes only after all signing.
  The current two-ZIP development draft validator must gain an explicit reviewed
  Windows installer/full-package asset set before publishing those artifacts;
  do not silently substitute a Setup.exe into the old ZIP contract.

## Native acceptance sequence

On a fresh normal-user Windows context with synthetic development state, verify
one local Setup install without elevation, Start-menu/Explorer stable-stub launch,
correct installed runtime identity and six public routes, then the existing
package restart scenario against installed `current/runtime`. Exercise WebView2
absent/install/retry/cancel, guest UI and Blindly, and confirm every owned process
exits on close. Keep the existing native WebView2 UX; do not add a competing
prerequisite workflow merely for packaging.

Then verify repeat installation and uninstall without changing external synthetic
data, x64 execution on Windows 11 ARM, and actual publisher-signed downloaded
installer trust. Active-file/update/recovery scenarios consume main/D2's approved
lifecycle transaction later; ordinary installer hooks cannot substitute for it.
Machine access/configuration and signing are delivery/setup tasks, not grounds to
omit these checks. Native build/install execution, genuine signing and publication
remain unrun in this preparation.

References: [native C ABI startup](https://docs.velopack.io/getting-started/cpp),
[per-user Windows layout](https://docs.velopack.io/packaging/operating-systems/windows),
[pinned C header](https://github.com/velopack/velopack/blob/92d6a1c91716729d449034df5c50307dcce39493/src/lib-cpp/include/Velopack.h),
[pinned Windows composition](https://github.com/velopack/velopack/blob/92d6a1c91716729d449034df5c50307dcce39493/src/vpk/Velopack.Packaging.Windows/Commands/WindowsPackCommandRunner.cs),
[pinned signature handling](https://github.com/velopack/velopack/blob/92d6a1c91716729d449034df5c50307dcce39493/src/vpk/Velopack.Packaging.Windows/CodeSign.cs).
