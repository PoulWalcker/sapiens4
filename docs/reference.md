# Usage and operations

See the [README](../README.md) for startup and platform requirements.

## Work and conversations

ChiefSapi manages the team and can delegate to specialists. Specialists can refer
work to Chief; accepted handoffs retain task identity and return the responsible
Sapi's result to the original chat. **Answer Chief** continues a clarification.
Creating a Sapi or Group does not itself start work.

Personal Tasks offers Upcoming and Past, with read-only YAML specifications and
results. Scheduled routines can be created, edited, paused and resumed through
Chief. Group tasks have shared ownership, revision checks and explicit execution;
saving or restoring a task does not run it. Groups require at least two active
members and one Lead, with up to eleven active memberships per Sapi.

Each Sapi or Group has its own chat, files, Notes and browser workspace. Group
chat defaults to the Lead; explicit mentions address members. Personal and other
Groups' histories remain separate. Retirement and archival preserve saved data.

Stop terminates the runner and child commands but cannot undo completed external
actions. Never-started queued work resumes after restart; previously running work
becomes interrupted and requires explicit retry. Inspect uncertain external
outcomes before retrying. The shared computer allows one owner at a time.

### Routines

Under a Sapi's **Work → Automation → Routines**, save a title, goal and
completion criteria, then use **Run now** with optional inputs. Saving does not
start work. Each run keeps its original owner, definition revision, goal, criteria
and inputs even when the routine is edited. Work can delegate through the existing
team; its history identifies the current responsible Sapi. Open a routine to view
its latest result or history, then open an individual run for its details.
Settings edits the definition; its Schedule page separately selects an interval, a daily local time with an IANA timezone, or on-request only. Preview the next UTC run before saving. An empty timezone field uses the browser's current IANA zone and saves that concrete zone; later travel or host timezone changes do not move the schedule.

**Stop run** stops that occurrence, including delegated calls, and prevents later
results from being published. It does not stop other runs or undo completed
external actions. **Delete routine** prevents new runs and stops active occurrences;
completed history remains visible. A routine without a schedule runs on request.
Changing the schedule preserves its identity, accepted contract and prepared implementation. **Pause** stops future
timer admissions, while current work continues and **Run now** remains available.
**Resume** finds the next occurrence after resumption using the saved rule. The schedule
and the current run have separate statuses.

Minute pulses admit due work. One routine and due time identify one scheduled
occurrence; its admission and the next deadline are saved together. Downtime does
not replay every missed period. Daily rules follow the saved local calendar rather than 1440 elapsed minutes. Repeated local times use the first occurrence; nonexistent times shift forward by the transition gap. Previously started work is not automatically
repeated merely because its result is unknown after restart.

## Notes and files

Memo reads `workspaces/<id>/Notes.html`. Its wiki has `about`, `map` and `content`
sections; about/map enter model context and the model reads relevant content as
needed. Scripts and deliverables remain ordinary linked files. The renderer
allows inert markup and local images while removing executable content.

Native desktop tabs open websites, local files and bookmarks. Text/source files
render literally; HTML, PDF and media use the native browser. Closing a tab does
not delete its file. Remote clients can preview text files and control the host's
tabs, but do not stream native browser views.

## Configuration

Default model: `gpt-6-sol`, reasoning `high`; Deep work uses `xhigh`. A call's
30-minute limit is shared across its decision steps. Environment overrides:

| Variable | Purpose |
| --- | --- |
| `SAPIENS_DATA_DIR` | Data directory |
| `SAPIENS_PYTHON` | Python executable |
| `SAPIENS_CODEX_BINARY` | Codex executable |
| `SAPIENS_CODEX_MODEL` | Model |
| `SAPIENS_CODEX_REASONING_EFFORT` | Reasoning effort |
| `SAPIENS_BLINDLY_BINARY` | Blindly4 executable |

Decision templates live in `corpora/prompts/`. **Edit decision prompts** saves local
`decision-prompts/*.md` overrides for subsequent invocations; past decision
records retain the prompt they used. Context, Usage and legacy budget settings
are inactive. Global Codex configuration is unchanged.

## Data and updates

macOS/source data defaults to `.sapiens4` in the checkout; Windows packages use
`%LOCALAPPDATA%\Sapiens4`. Back up the complete data directory while the host is
stopped, including:

- `corpora.sqlite3`: agents, workloads, Groups, preferences and attachments.
- `agentpy/`: persisted chat state and archives; the legacy path remains in use.
- `workspaces/`: Notes, files and deliverables.
- `decision-prompts/`: local prompt overrides.
- `corpora-identity.json`, registry and remote credential files: identity and trust.

A database-only backup is incomplete. Protect backups as credentials and private
data. Copying a backup retains its corpora identity; it does not create an
independent corpora or synchronize simultaneous hosts. Never run two hosts against
one data directory. Install new versions manually using the platform setup guide.

The header corpora picker changes the viewed workspace; Pause and Activate control
execution separately. Drafts and layout are stored on the viewing device. Cached
remote snapshots are read-only until reconnection. See [remote setup](remote.md)
for pairing, revocation and relay deployment.

## HTTP API

The host binds to loopback. Writes require JSON and `X-Sapiens-Local: 1`;
a supplied Origin must match the loopback Host. Common entrypoints:

| Method | Path | Purpose |
| --- | --- | --- |
| GET | `/api/health`, `/api/state` | Readiness and current state |
| POST / PUT | `/api/agents`, `/api/agents/<id>` | Create/update Sapis |
| POST | `/api/agents/<id>/messages`, `/api/agents/<id>/attachments` | Submit chat or attachments |
| GET | `/api/agents/<id>/notes` | Memo and file metadata |
| GET | `/api/agents/<id>/tasks` | YAML export; `?format=json` returns task metadata |
| POST | `/api/agents/<id>/routines`, `/api/agents/<id>/routines/<routine>/<action>` | Save a routine; edit, run, stop, delete, schedule, pause or resume |
| GET | `/api/agents/<id>/decisions` | Decision records |
| POST | `/api/agents/<id>/control` | Authorized team/workspace operations |
| POST | `/api/agents/<id>/turns/<id>/retry`, `/api/agents/<id>/turns/<id>/cancel` | Explicit recovery |
| GET / PUT | `/api/decision-prompts`, `/api/decision-prompts/<node>` | Inspect/edit templates |

Group notes, messages, attachments, tasks and calls use `/api/groups/<id>`.
Updates require the relevant revision. [RSpec scenarios](../spec) exercise the
public contracts; see [Testing](testing.md) to run them.

### Routine commands

Create accepts `operation`, `title`, `goal`, and a nonempty `criteria` array.
Actions are `edit`, `run`, `stop`, `delete`, `schedule`, `pause`, and `resume`; each also requires the current
`revision`. Edit supplies the same definition fields; run accepts optional text
`inputs`; stop requires the occurrence's `run` ID. Schedule accepts an integer
`minutes` from 1 to 525600, a `schedule` object such as `{"kind":"daily","time":"09:00","timezone":"Asia/Dubai"}`, or `trigger:"manual"`. These alternatives are exclusive. Daily rules require an explicit valid IANA timezone and HH:MM time. Schedule changes preserve the routine ID and contract generation; saving the same daily rule retains its previously saved deadline. Definition and schedule
edits are separate commands, each checked against the current revision.
Ownership comes from the route,
not the body. `/api/state` exposes definitions and occurrence history in
`personal_routines`, and saved command refusals in `routine_errors`. This list
includes legacy personal interval routines, whose unspecified completion criteria
remain `null`. Its explicit `trigger` is `manual` or `interval`; legacy storage is
not rewritten merely to render this normalized view.

The existing host-control endpoint also accepts `routine_schedule_preview` with the same rule fields and an optional explicit UTC/offset `after` timestamp. It returns the next UTC deadline without changing state, admitting work or invoking a model. This hypothetical timestamp is never scheduling authority. Host-control `routine_create` and `routine_update` accept the daily `schedule` instead of `minutes`.

Reuse the same operation ID and unchanged request to recover its original response
without duplicating work, including after restart or while the corpora is paused.
Reusing an ID with changed contents returns 409; use a new operation for new inputs.
For an existing owner and valid operation ID, first refusals are saved as command outcomes; operation-ID conflicts also appear
in the existing bounded activity history. Stale definition revisions return 409.
A stopped occurrence cannot be reopened through ordinary call retry.

### Preparing a routine candidate

Accepted ordinary Routine results can automatically start preparation through the
existing dispatcher. The same observations do not create another automatic attempt
on each tick or restart. An applicable active version avoids needless preparation.
Standalone tasks may request Composer directly within their accepted task contract.

For explicit preparation, use
`POST /api/agents/<owner>/routines/<routine>/prepare` with `operation` and current
`revision`. A Routine needs an accepted ordinary source under its current contract;
the response contains a `preparation` ID.
`GET /api/agents/<owner>/routines/<routine>/preparations/<preparation>` exposes
its candidate, independent fixtures, actual checks, review and operation evidence.
`stop-preparation` accepts `operation`, `revision` and `preparation`.
An unchanged operation replay returns its original response.

The fixture-design stage may return `insufficient_evidence` or `unsuitable` with a
reason. Otherwise it establishes expected results before generation, executes the
actual code against them, and requests candidate review. Success is
`awaiting_occurrence`: preparation does not activate code or authorize a business
run. Contract changes, rejected source evidence, deletion and Stop fence promotion.
Requests and versions under the same Routine contract share their preparation
allowance; a new request ID cannot refill it or revive stopped work.

The first verified external runtime profile requires macOS ARM64 with a running
local OrbStack/Docker 29.4.0 daemon and the already-cached official
`python@sha256:ffb752e139c0a19692a43af8d8523b274222dd68eebad5d583b45c2201c6e30a`
Linux ARM64 image (Python 3.13.15). The app does not install or pull a runtime.
`SAPIENS_GENERATED_DOCKER` may specify an absolute Docker CLI path, and
`SAPIENS_GENERATED_SOCKET` a local `unix:///...` endpoint; defaults use
`~/.orbstack/bin/docker` and `~/.orbstack/run/docker.sock`.
Unavailable profiles, non-standard dependencies, or effect capabilities fail
explicitly. Other languages need their own verified profiles.

Each fixture runs without networking or host mounts, with a read-only root,
unprivileged user, dropped capabilities, and bounded memory, process count,
CPU, time, and streamed output. These are per-workload limits; they do not bound
the whole Docker VM's host memory. Durable container ownership is recorded before
start. After a host crash, restart reconciles only the matching owned containers
without replaying the attempt. If cleanup is uncertain, preparation remains
`cleanup_pending`; restore the runtime and restart the app before another
preparation. Ordinary work remains available.

### Running a prepared routine

The next authorized Run now or scheduled occurrence can claim the newest checked
candidate for a single trial. The candidate reference, accepted task and actual
inputs are pinned at admission. Other admitted runs keep their existing bindings.
A pending trial prevents a second concurrent trial, and a consumed trial never
causes an older untried candidate to be selected automatically.

The script performs the pure computation in the verified external runtime above.
Each code occurrence then uses one response-only model call to assess the original
completion criteria. The first trial also returns distinct activation-audit facts
from that same invocation; this is shared evidence, not a second independent call.
Only an accepted result and affirmative audit, with the current contract and source
feedback still applicable, activate the exact version for future admissions.
The routine screen shows the version and the run, time and reason for activation.

A failed or unknown trial retains its actual output and does not immediately launch
an ordinary replacement. A later occurrence uses a still-valid prior active version
or ordinary execution; trying another candidate requires a newer preparation.
A proven code or semantic failure suspends only the matching active version.
Interruption, unavailable runtime or unknown verification do not themselves
quarantine an active version. Generic Retry is unavailable for code-bound runs;
start a new authorized occurrence instead. Ordinary Retry is unchanged.
Owned-container uncertainty blocks further generated-code starts until runtime
restoration and app restart reconcile ownership; ordinary work remains available.

### Hybrid calculation and explanation

Preparation may produce a two-step candidate: pure code calculates structured
facts, then a response-only owner invocation produces the useful explanation.
Its original numerical and semantic criteria remain unchanged. Independent
preparation examples compare numerical results before explanation and rubric
review; final business acceptance separately checks the already-produced result.
Retaining numerical fields does not make contradictory prose acceptable, and
these checks are not a general mathematical oracle for every unseen input.

The explanation pins its requested model, reasoning effort, instruction and host
prompt-template hashes. Admission retains the exact implementation reference and
structure identity. Changed defaults cannot repin an accepted run; a changed host
prompt template fails visibly and requires new preparation. Remote model weights
are not claimed immutable. Both stages share the existing execution deadline,
Stop and recovery; no tools, external effects or automatic stage replay are enabled.

The routine result shows Calculation and Explanation progress. Explanation usage
is recorded separately from final acceptance and the initial activation audit.
Final assessment cannot replace the business explanation. A rejected calculation
input retains its reason and stops before explanation or activation, without
quarantining a valid active version. Existing single-script candidates keep their
prior behavior; fallback and effect reconciliation are separate capabilities.

### Receipt-aware feed and delivery

Routine create/edit HTTP requests can explicitly configure `externalProfile`:

```json
{
  "kind": "receipt_feed_v1",
  "taskBoundary": "content_result",
  "executionPolicy": "response_only",
  "source": {"url": "https://source.example/feed", "identity": "sales-feed", "initialCursor": "c0"},
  "delivery": {"url": "https://inbox.example/deliveries", "receiptUrl": "https://inbox.example/receipts", "identity": "daily-inbox"}
}
```

The accepted Task creates content under its unchanged criteria. Delivery is a
separate operation; Task completion does not prove delivery. The profile forces
response-only ordinary content execution and refuses delegation. Checked pure code
or the existing calculation/explanation hybrid can consume the same retained feed
input; their containers remain networkless. Configuration and preparation controls
are currently through HTTP. Changing or removing a profile advances applicability:
old versions and accepted history remain, but future runs use ordinary execution
until a candidate is prepared for the new contract/profile. Restoring old settings
does not revive an older version.

The source GET adds an opaque `cursor`. A complete response contains `schema: 1`,
matching `source` and `cursor`, `nextCursor`, `complete: true`, and `records` with
`id`/`value`. Identical repeated IDs are deduplicated; conflicting duplicate values
fail. Previously accepted IDs are omitted from new content. Failed or incomplete
reads do not advance the cursor. Accepted content, cursor progress and the exact
unsent delivery reservation are stored atomically. A cursor conflict retains the
model's proposal but records host acceptance as unresolved and sends nothing.
An accepted page without new records does not create a delivery.

The host POSTs the accepted content once. Its body binds a host-generated operation
key, consumer scope, profile hash/revision, contract hash, source observation hash,
record IDs and content to a canonical SHA256 `requestHash` (computed without the
hash field itself). A receipt must echo schema, receiver, operationKey, scope,
profileHash and requestHash, with `status: confirmed` and a nonempty receiptId.
A timeout, lost acknowledgement or interruption after starting leaves delivery
unknown. Recovery never repeats the POST. A reservation interrupted before send
remains explicitly not sent. Empty lookup results and model claims do not prove
that no external action happened.

Explicit `POST /api/agents/:owner/routines/:routine/runs/:run/reconcile-delivery`
with `{"operation":"unique-local-id"}` performs only a GET of the pinned receipt
endpoint with `operationKey`. The first exact confirmed receipt is retained;
conflicting later receipts cannot replace it. Repeating a completed local operation
returns its saved response. A pending/interrupted lookup needs a new explicit
operation for another GET. Reconciliation remains available for historical runs
after Stop, deletion or profile replacement.

Unknown deliveries or unexpected tool/protocol uncertainty block further attempts
of the same Routine, including ordinary Retry and after profile removal. Retained
pre-snapshot Routine ancestry is checked against the original owner, including
legitimate delegated calls. Standalone Tasks, Groups and unrelated routines keep
their existing policy. Profile-bound occurrences cannot generic-Retry;
a failed read without uncertain effects can be retried as a new authorized run.
Omitting externalProfile on edit preserves it; explicit null removes configuration
but preserves history and uncertainty. Profile revisions prevent old authorization
from becoming valid again after replacement and restoration of identical settings.

The immutable `receipt-feed-input-v1` input contains original `taskInputs`, the
complete `observation`, host-derived `newRecords`, and page-relevant
`previouslySeenIds`. It does not copy the entire growing source history. Candidate
identity pins the input interface and profile revision; each occurrence also pins
the observation and normalized input hashes. A malformed saved source state is
not treated as an empty history. The existing 64 KiB code input bound applies;
oversized observations are retained without silently truncating code input.

Preparation freezes independent source examples and a content interface before
code generation. Pure code produces `value.text`; hybrid delivery uses its actual
explanation. The final assessment cannot rewrite this content. Successful task
acceptance, cursor CAS, candidate activation and delivery reservation share one
transaction. Delivery begins afterward; a lost acknowledgement can leave valid
code active while the separate consumer uncertainty fence blocks another run.
Previously accepted observations can supply preparation evidence only when their
retained page, original parameters and deduplication facts agree. Derived legacy
inputs explicitly retain unknown historical-state hashes rather than invent them.

For ordinary Routine retries, new admissions retain whether an invocation was
prepared or admitted before starting its provider. Failed or interrupted unsafe
invocations with known admission and uncertain tool outcomes block the consumer;
a later successful run does not clear that evidence. Complete clean no-tool traces
and proven pre-admission failures permit Retry. Historical completed work without
evidence is not retroactively quarantined. A legacy failed attempt with no retained
start provenance cannot itself be retried, but does not automatically block all
future runs: unrecorded historical starts remain an explicit migration limitation.
An unrelated feed receipt or model assertion cannot clear arbitrary tool uncertainty.

Transport uses fixed HTTPS endpoints or exact loopback HTTP, no credentials,
environment proxies, redirects or automatic retries. URLs cannot contain userinfo,
query or fragment. Responses are streamed with a 64 KiB bound; run IO shares the
original deadline and Stop, while explicit receipt lookup has a 20-second bound.
The routine screen separates content acceptance from delivery status and offers
an explicit receipt check for unknown delivery. Polling performs no external IO.

### Opt-in managed calls

Ordinary chat submissions can explicitly send `"managed": true`. This mode is
currently available through HTTP; ordinary UI submissions retain their existing
behavior. Computer submissions, workload clarification and schedule commands do
not accept this option. Managed calls run individually in the existing scheduler.

SQLite owns a managed call's pinned input/flow, lifecycle, number of admitted
attempts (`attempts_started`, advanced before launching the provider), outcome receipts and accepted command identities. Its `state.json` turn
is a protected projection; other turns and unrelated saved fields retain their
existing file authority, including same-revision external edits. A receipt records
the call's outcome, not independent proof of an external action or task acceptance.
The attempt counter is not a token or spending budget. Delegated recipient calls
continue through the existing delegation path; they are not implicitly managed.

Use the usual HTTP cancel/retry endpoints. A file-based client may instead add
`"managed_command": {"id": "unique-operation", "action": "retry", "revision": N}`
to the managed turn, using its current `managed.revision`. `cancel` is also
supported. The worker consumes the command; reading state does not execute it.
An exact accepted command replay is harmless. Reusing its id with different
contents, stale new commands, deleting a managed turn, or changing protected
fields returns a visible conflict. Preserve the rest of the projection when
submitting a command. Repair a conflicting file from its known valid contents;
do not rewrite protected facts to reset an attempt.

Canonical transitions and their file-projection intents commit together.
Interrupted projection writes are recovered on startup or worker pulses without
starting a fresh attempt. Calls that were running at shutdown become interrupted;
retry remains explicit, and prior receipts are retained. Back up SQLite and
`agentpy/` together. Managed turns and their command/receipt history currently stay
in the active projection; archival and large-history scaling are not implemented
for this opt-in mode yet.


### Owner preparation and retained-result assessment

A proven implementation defect retains its evidence and starts a new immutable
candidate through the owner's Composer capability. Generation receives the exact
previous failed version and observations; independent fixture design receives the
observations without the source. Safe ordinary fallback can finish the user's
occurrence, but its success does not establish candidate health. A replacement
still needs an authorized occurrence and accepted result before activation.

If an otherwise established calculation lacks a valid assessment, the owner can
independently reassess its exact retained result. The original version, inputs,
criteria and receipts must remain applicable. This does not recalculate the result
or replay delivery. Missing or stale evidence, fallback and unknown effects cannot
be treated as proof of recovery.

Each regeneration case has one persisted allowance of 30 active minutes covering
preparation, checks and authorized verification. Waiting releases active capacity;
unfinished reservations remain consumed with unknown spend shown explicitly.
Restart and new candidate IDs do not refill the allowance. An explicit finite grant
can extend an exhausted eligible case; it cannot override Stop or uncertain effects.
Confirmed temporary provider failures use the existing bounded continuation policy.

The Routine view shows preparation status, failure reason, current candidate,
measured and reserved resource, and retained operation evidence. Missing token or
price information remains unknown. The state snapshot exposes `repairCases` and
the current `repair` projection; these records describe owner preparation and
assessment, not a separate agent or executor.
