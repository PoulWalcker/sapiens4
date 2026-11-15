# Accepted external requests

`request_accepted` records only `agent_message`. The capture is in
`Execution.submitTurn`, after the new turn or clarification and required message
persistence, before optional draft cleanup, projection and trimming. Routine
acknowledgments, rejected requests, internal delegation and scheduled work do
not count. Remote Control reaches the same hook through `Service.dispatch`;
a saved receipt returns its reply without repeating capture.

Group messages and task runs have no telemetry hooks. Their request kinds were
removed from the host ADT, JSON codec, collector parser and Parquet writer on
2026-11-15. The collector rejects retired request kinds, including an entire
mixed batch containing one. The four event names and fourteen physical Parquet
columns are unchanged. Previously published files are not rewritten.

Capture passes only an opaque local Corpora reference and the closed request
kind to the shared nonblocking sender. The sender owns consent, EventId/time,
identity preparation and delivery. This is best-effort RAM telemetry, not a
transactional business audit.

## Agent-only scope checks, 2026-11-15

Following `11a27a6`, the host, macOS executable and standalone collector built
with the existing dependency freezes. Focused macOS process/HTTP observations
used private temporary storage and the existing scripted provider:

- A group message and group task run returned 202 and settled, with zero new
  telemetry after a twelve-second delivery window.
- A subsequent external agent message produced one accepted-use event through
  the actual host sender and collector.
- Both retired request kinds and a mixed agent/group batch returned
  `422 invalid_event`. The complete batch is validated before admission.
- DuckDB 1.5.5 independently read the two published rows: one root creation and
  one `agent_message`, fourteen physical columns, two distinct EventIds and no
  additional rows. Wire and saved values matched.

Port 8081 belonged to another session. The checks loaded the current collector
HTTP application and real publisher through Cabal GHCi on a private alternate
loopback port; no production port option was added. Initial launcher/HTTP-status
expectation errors were corrected in temporary fixtures, without application
changes. The successful host observations were retained rather than rerun.
Windows/Linux, broad regressions and feature E2E were not rerun.

Sanitized evidence is retained locally at
`/Users/pashvel/Desktop/personal/sapiens4-worktrees/telemetry-agent-only-macos-pl6dazrz/`.
Owned host/collector/proxy processes and private profiles/keys were cleaned.

## Historical observations before the scope reduction

On 2026-11-14, macOS arm64 / GHC 9.10.3, following
`c90c51fcd9fb8b6dc069f097022f23fe5d1e1f47` on `telemetry/d4`, temporary
process/HTTP/SQLite/HTTPS fixtures compiled the actual current Host and Work
modules. Existing scripted providers supplied synthetic responses; no paid
provider or production collector was used. Host and macOS executables built
with the selected `build/` dependency freeze.

- Forty focused boundary observations covered independent agent commits,
  accepted and rejected clarification, actual internal delegation, routine
  acknowledgments, input/provider/inactive-agent rejection, normal group
  dispatch, server provenance, task creation/editing, revision/pending rejection,
  two accepted task runs and their settlement. Actual provider GroupControl
  message/task-run actions committed business work without external group
  captures. Overdue persisted agent/group schedules ran through the real minute
  pulse and added no accepted-request capture.
- SQLite failure injection at required message and group/task persistence
  prevented capture. Injection after accepted agent persistence during projection
  and after group save during downstream dispatch retained one capture despite
  the later HTTP failure.
- A real encrypted relay pairing exercised each of the three request kinds.
  Repeating each frame returned its saved response and left captures unchanged.
- With consent absent, an invalid existing privacy key, a corrupted profile,
  collector 503/backoff, and a queue filled by 300 synthetic capture attempts,
  ordinary agent/group/task requests still committed promptly. Invalid identity
  keys were not regenerated. Off/key/profile cases sent no accepted-request
  event; a root creation already authorized before fault injection may precede
  that fault.
- The real collector accepted five request events (two agent messages, one
  group message, two task runs) plus one root creation. An injected first 503
  produced a byte-identical retry. Independent HMAC calculation matched the
  public Corpora ID. PyArrow 18.1.0 read six rows from the actual published
  Parquet, including five distinct request EventIds, all three request kinds and
  one shared installation/device. Wire and saved rows contained no synthetic
  message/task content, local path, raw Corpora UUID or provider identity.

Positive identity checks used a DataRoot created by the actual host, rather than
the generic fixture's precreated directory with broader permissions. TLS used a
temporary private proxy and fixture-only self-signed certificate acceptance;
production certificate policy was unchanged. Temporary probes/readers/data were
removed; no test suite or debug endpoint was added.

Actual Windows/Linux verification of these hooks remains open. Predecessor
verification at `a45810d` does not verify ticket 07. Regression suites, GUI,
installer and feature-completion E2E were not run; final E2E remains ticket 12.
Ticket status files were not changed, and no deployment or production collection
was performed.
