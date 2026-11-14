# Accepted external requests

Ticket 07 connects the existing `IRequestAccepted` intent to three business
acceptance boundaries. It adds eleven source lines across three existing Work
modules, with no new event type, module, dependency, origin abstraction or API.

| Request kind | Capture boundary | Exclusions |
| --- | --- | --- |
| `agent_message` | `Execution.submitTurn`, after the new turn or clarification and required `Store.message` persistence, before draft cleanup, projection and trimming | Routine acknowledgment, failed validation/provider/setup/acceptance, internal delegation and scheduled work |
| `group_message` | Normal `GroupChat.submitGroup`, after `saveGroup`, before draft cleanup and downstream dispatch; actor must be server-selected `admin` | Routine acknowledgment, internal GroupControl, scheduled work and failed validation/save |
| `group_task_run` | `GroupWork.startTask`, after `saveGroup`; actor must be server-selected `admin` | Creation/editing, invalid revision, pending rejection, internal GroupTaskRun, scheduled work, worker execution and settlement |

The external Service routes select the actor themselves. Group-message JSON
cannot supply actor/origin/agencyRun flags; unknown fields are rejected. Extra
task-run fields cannot replace the actor selected by the route. Internal
GroupControl uses the actual agent identity, while scheduled work persists its
own occurrence without calling these external capture boundaries.

There is no HTTP success middleware: a routine acknowledgment can return 202
without new accepted use, and an accepted operation can later fail during
unrelated projection or dispatch. Capture follows the required business writes;
it does not imply that subsequent model work succeeds. The existing remote
receipt path returns a saved encrypted response without repeating dispatch,
business persistence or capture.

Each capture passes only the local opaque Corpora reference and closed
`RequestKind` to the shared nonblocking `Emit`. The sender owns EventId/time,
consent, private identity preparation, bounded RAM and delivery. Two independently
accepted requests create two EventIds; retrying a serialized batch preserves its
bytes and IDs. Events carry no message, attachment, task, group or agent details.
This remains best-effort RAM telemetry, not transactional business auditing:
process interruption or telemetry failure can lose an accepted-use event.

## Development observations

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
