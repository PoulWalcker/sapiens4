# D1 Windows WebView2 process cleanup

Source fetched before investigation: `dev/d1-distribution` at `d5a35bbba27293168ff31ac9023cc00f038e521f`.
Fix branch: `fix/d1-webview2-process-cleanup`.
Original QA: `test/d1-distribution-e2e` at `3be7ce28ef33059993fbb1f2ee226904c9ecaa0f`.
Investigation date: 2026-11-12 (Asia/Dubai).

## Finding

Retry is not necessary to reproduce the failure. Two independent runs of A (obstructed Shell profile, Cancel, exit, uninstall) against a newly built real Setup left the WebView2 data-directory error window and its process alive:

| Run | Host PID | Surviving WebView2 PID | WebView2 parent PID | Host exited | Uninstall completed |
|---|---:|---:|---:|---|---|
| Baseline A | 4076 | 7276 | 4076 | Yes | Yes |
| Independent A repeat | 8708 | 7748 | 8708 | Yes | Yes |

The window title was `We couldn't create the data directory`. Its text named the corresponding disposable `case-A/data/WebView2/Shell/EBWebView` path. Closing only that window released its process.

The installed, instrumented build established the failing lifecycle:

1. `CreateCoreWebView2EnvironmentWithOptions` completes successfully; the environment completion handler receives a non-null environment and HRESULT 0.
2. Controller creation starts with a child HWND. The WebView2 process shows its own data-directory error window for the obstructed profile.
3. The shell's 35-second loading deadline invokes `failStart`. This invalidates the generation, closes any completed views, and stops the owned backend.
4. The pending child is discarded by the arena sweep. The controller completion handler receives `E_ABORT` (`-2147467260`), a null controller, and `active=False`.
5. Both completion handlers reach reference count zero and are disposed. `CoUninitialize` returns, then `freeArena` unloads the loader and returns. There is no completed controller for `closeView` to close, but the separate error-window process remains alive.

The trace rules out a retained successful controller or completion-handler reference as the cause of this reproduction. The association of pending-child destruction with the abort follows from the trace order and the arena sweep implementation. WebView2's internal modal-window implementation was not debugged.

The diagnostic prefix was `[DEBUG-d1-wv2]`; it has been removed from the final application source.

## Lifecycle audit

| Area | Ownership and observed behavior |
|---|---|
| `Native.hs` environment callback | The callback's initial reference is released after the create call. The SDK retains it for completion. The environment argument is borrowed and used within the callback; the app does not store an additional environment reference. |
| Controller callback | Successful controllers are retained with `AddRef`; `get_CoreWebView2` returns the owned core reference. Stale non-null controllers receive `Close`. The observed failed callback returned null, so this branch had no controller to close. |
| `newViewWhile` / pending child | The generation predicate rejects stale completion. A pending child HWND is retained for cancellation/disposal; completed views take ownership of it. |
| `closeView` | Calls controller `Close`, releases core and controller, and destroys the child HWND through `finally` cleanup. |
| `Browser.hs` | `closeBrowser` marks the browser disposed, drains its tab map, marks tabs dead and closes each completed view. Late guest creation closes a newly completed view when the browser is disposed. The shared Native profile guard also covers guest creation. |
| `Shell.hs` | Retries create a new generation. Startup failure closes completed views and the backend. Quit stops the worker/backend, closes views on the UI thread, destroys the main window and exits the message pump. |
| `Host.hs` | The backend is owned through its process handle and pipes. Shutdown requests graceful stop, waits, and uses the existing bounded fallback for that owned backend. It does not own the WebView2 error-window process. |
| Arena / COM teardown | The message pump sweeps released callback allocations. COM is uninitialized before final arena disposal. Both completion callbacks reached zero in the traced failure. |

Microsoft documents [controller Close](https://learn.microsoft.com/en-us/microsoft-edge/webview2/reference/win32/icorewebview2controller#close) as the browser cleanup API, [STA and message-pump requirements](https://learn.microsoft.com/en-us/microsoft-edge/webview2/concepts/threading-model), and [writable user data folders](https://learn.microsoft.com/en-us/microsoft-edge/webview2/concepts/user-data-folder). In this failure, creation never yields a usable controller.

## Change

`prepareWebViewProfile` creates/checks the actual profile storage directory and opens, writes, flushes, closes and removes a uniquely named temporary probe there. It never removes the obstruction or existing profile files. A filesystem failure becomes a native Sapiens startup error with the profile path.

The shell performs this preflight before starting its backend. `newViewWhile` repeats it at the boundary immediately before acquiring the loader or registering creation callbacks, including for guest views. Retry rechecks the filesystem, so removing the obstruction permits the next attempt to start normally.

This prevents the confirmed failure from entering the WebView2 controller-creation path. It does not enumerate or terminate arbitrary WebView2 processes, alter the installed WebView2 Runtime, or delete profiles. It is not a general cancellation API for every possible WebView2 runtime failure; the filesystem can also change after a preflight check.

## Installer and process evidence

All application launches used a real installed x64 Setup in the existing Windows Server test environment, under `runas /trustlevel:0x20000`. The diagnostic rejects an elevated token and an existing Sapiens per-user installation. Each case uses its own install, data, config and Codex directories. RSpec examples and regression suites were not run.

Baseline Setup: `C:/Build/d1-webview2-cleanup/.build/baseline-setup/Sapiens-Setup-win-x64.exe`.
SHA-256, independently checked against its manifest: `b5c57f1aaa3e2a7d0619a2816f57d68881336f90a894d1bb145552b1a9931ea3`.

Instrumented Setup: `.build/trace-setup/Sapiens-Setup-win-x64.exe` in that checkout, built at `280a7bb`.
SHA-256: `a6c7b1fd27865c4f0006a6b1c1c9d35e6a2b32de4a1484c2f7171be4e4787129`.

After correcting fixture cleanup to compare process creation timestamps through the same CIM API, all five instrumented cases ran independently. The diagnostic records survivors before closing any fixture error window and stops before the next case if fixture cleanup fails.

| Case | Sequence | Host PID | Orphan PID after uninstall | Uninstall |
|---|---|---:|---:|---|
| A | Obstruction → Cancel → exit | 5520 | 3172 | Completed |
| B | Obstruction → Retry still obstructed → Cancel | 4652 | 9084 | Completed |
| C | Obstruction → remove file → Retry → successful startup → Quit | 1924 | 5820 | Completed |
| D | Successful startup → Quit | 3844 | None | Completed |
| E | Successful startup → Quit → reinstall → uninstall | 1556 | None | Completed |

The initial baseline matrix left A's recorded error window open because its cleanup guard compared CIM microsecond timestamps with .NET process timestamps of greater precision. Its B/C observations therefore were followed by the isolated instrumented matrix above. The initial evidence has been retained unchanged.

On the Windows machine, evidence roots are:

- `C:/Build/d1-cleanup-baseline-r2` — first baseline matrix.
- `C:/Build/d1-cleanup-baseline-repeat-A` — minimal independent repeat.
- `C:/Build/d1-cleanup-trace-1` — independent instrumented A–E matrix.

Each root contains `matrix.log`, `matrix.err`, `matrix.exit`, installer identity, and one directory per case. `processes.json` records timestamps, host/backend/WebView2 PIDs, parent PIDs, executable and command line, profile path and window title at launch, recovery, Retry, host exit, uninstall and fixture cleanup. `host.log`, `result.json`, Setup/reinstall/uninstall logs and survivor window trees preserve the other observations.

## Final validation

**PASS: all five installed cases, exit 0, empty diagnostic stderr.**

Tested application commit: `d7bcbf4a1c85d998bb94f2d5956d6f3286d4b4b1`.
The official Windows build/package script returned 0, including its recursive PE architecture/import audit and Inno Setup packaging. Final Setup:
`C:/Build/d1-webview2-cleanup/.build/fixed-setup/Sapiens-Setup-win-x64.exe`.
Its independently computed SHA-256 matched `.build/fixed-setup/SHA256SUMS`.
The full digest is preserved in that manifest and in the final evidence root's `identity.txt`.

Verified platform: Microsoft Windows Server 2022 Datacenter, version `10.0.20348`, 64-bit. The tested WebView2 executable was in `Microsoft/EdgeWebView/Application/157.0.4341.42`. This is local unsigned installer validation on that server configuration, not a Windows 10/11 or public-signing qualification.

| Case | Host PID | Backend PID | Main WebView2 PID / parent | Host/backend exited | WebView2 survived | Uninstall |
|---|---:|---|---|---|---|---|
| A | 72 | Not started | Not started | Host exited; no backend started | No | Completed |
| B | 8312 | Not started, including obstructed Retry | Not started | Host exited; no backend started | No | Completed |
| C | 5472 | 9172 after repair/Retry | 8796 / 5472 | Yes | No | Completed |
| D | 7536 | 6340 | 6128 / 7536 | Yes | No | Completed |
| E | 7876 | 8012 | 2884 / 7876 | Yes | No | Completed after reinstall |

All profiles were under `C:/Build/d1-cleanup-fixed-1/case-<letter>/data/WebView2/Shell`. A selected Cancel. B selected Retry while obstructed and then Cancel. C removed the obstruction, selected Retry, reached the working application UI and quit. D quit a normal session. E quit, reinstalled and uninstalled. The failing A/B paths displayed Sapiens native recovery; no WebView2 error window or browser process was created for those paths.

The post-exit and post-uninstall process snapshots contain no surviving owned processes. The diagnostic waits five seconds before each of these snapshots; host exit itself is bounded at 35 seconds. The final desktop process query also returned no `msedgewebview2.exe` instances. The A/B obstruction files still contained exactly `D1-A` and `D1-B`, and no `.sapiens4-write-check*` probe files remained.

Final evidence: `C:/Build/d1-cleanup-fixed-1`, with the same per-case JSON, process snapshots and installer/host logs described above. No regression suites were started. The temporary Inno compiler was uninstalled after validation (exit 0, `ISCC.exe` absent). The original Windows QA checkout still reported `3be7ce28ef33059993fbb1f2ee226904c9ecaa0f` after cleanup.

## Re-running the manual diagnostic

From an unlocked disposable, unelevated Windows desktop with Ruby and the repository's bundled Ruby dependencies installed:

```powershell
./spec/support/windows_webview2_cleanup.ps1 `
  -Setup C:/path/to/Sapiens-Setup-win-x64.exe `
  -Evidence C:/path/to/a-new-evidence-directory `
  -Ruby C:/path/to/ruby.exe
```

`-Cases A` selects the minimal reproduction. The default runs A–E. Exit 1 indicates an observed survivor or a diagnostic failure; inspect the per-case records and stderr to distinguish them. A successful diagnostic build alone does not establish that application process cleanup passed.

## Original QA preservation

The original local QA worktree remains at `3be7ce28ef33059993fbb1f2ee226904c9ecaa0f`. Its report and screenshots were not edited. The report `docs/pr64-distribution-qa.md` retains SHA-256 `65945878900c2c01ad8a4d19278b4ef4566e5a4c4f6b1ed7f2596d1c4e05b724`. The Windows checkout and release artifacts under `C:/Build/qa64-a294cde` remain separate from the new worktree and evidence paths.
