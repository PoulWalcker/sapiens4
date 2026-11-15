# Scheduler heartbeat

`scheduler_heartbeat` is the fifth D4 event, added after `fa9c167`. It records a
successful top-level scheduler minute pulse, at most once per rolling hour.
It is supported by desktop and headless hosts.

`HostServer` supplies `Just (emit ISchedulerHeartbeat)` only for the root host;
managed hosts receive `Nothing`. `Service.startService` forwards this optional
callback to `Execution.startExecution`. The existing scheduler owns one new
monotonic deadline, initialized to startup time plus 3,600,000 milliseconds.
After a successful active `Bph60` handler, eligibility advances that deadline to
the current monotonic time plus one hour before calling the callback.

The existing active-state gate suppresses pulses while paused. Resuming can emit
one current heartbeat at the next eligible minute pulse. A long time jump emits
one event, without replaying missed hours; restarting creates a fresh deadline.
An eligible callback advances the deadline even when consent prevents capture.
No heartbeat history is persisted. Existing pulse deadlines, frequency counters,
routine dispatch, worker ownership and the 50 ms scheduler loop are unchanged.

The existing Emit/queue/sender handles consent, epochs, IDs, timestamps and HTTPS.
The new intent materializes without a Corpora identity lookup. Its strict JSON
contains only `event_type`, `event_id` and `occurred_at`; Corpora/request fields,
even explicit nulls, are rejected. Both optional Parquet columns are NULL. The
physical fourteen-column schema, schema version, notice version and the original
four event encodings remain unchanged.

## Focused verification, macOS arm64 / GHC 9.10.3

- Host, macOS executable and standalone collector builds passed.
- One existing RSpec scheduling example passed against the current binary with
  an actual sixty-second pulse, covering overdue routines, run limits and
  paused/retired owners (one example, zero failures).
- The actual built host was exercised with a temporary process-local monotonic
  clock shim: startup/pre-hour exclusion, repeated minute suppression, an active
  managed child, paused root, resume, a large time jump, restart and opt-out all
  produced the expected bounded counts. Wall-clock timestamps stayed current.
- An actual headless sender delivered the event. A SQLite failure inside routine
  dispatch prevented heartbeat capture despite an elapsed hourly deadline.
- The current collector accepted all five desktop events together and a headless
  heartbeat; it rejected scoped/null heartbeat fields and retained the existing
  rejection of headless `app_opened`.
- DuckDB 1.5.5 independently read thirteen published rows, including six
  heartbeats across both runtimes. All heartbeat optional columns were NULL; all
  five event types, original null rules and notice version 1 matched the wire.

The collector application/publisher ran through Cabal GHCi on an isolated port
because port 8081 belonged to another session. No production port option or
clock-testing interface was added. The clock shim and drivers are temporary
verification artifacts, not production code or a new test suite.

Native Windows/Linux, actual OS suspend and multi-hour wall-clock runs were not
performed. Full feature E2E and deployment verification remain separate.
Evidence: `/Users/pashvel/Desktop/personal/sapiens4-worktrees/telemetry-scheduler-macos-2_ptpoil/`.
