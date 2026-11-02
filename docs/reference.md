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
| POST | `/api/agents/<id>/routines`, `/api/agents/<id>/routines/<routine>/<action>` | Save a manual routine; edit, run, stop or delete |
| GET | `/api/agents/<id>/decisions` | Decision records |
| POST | `/api/agents/<id>/control` | Authorized team/workspace operations |
| POST | `/api/agents/<id>/turns/<id>/retry`, `/api/agents/<id>/turns/<id>/cancel` | Explicit recovery |
| GET / PUT | `/api/decision-prompts`, `/api/decision-prompts/<node>` | Inspect/edit templates |

Group notes, messages, attachments, tasks and calls use `/api/groups/<id>`.
Updates require the relevant revision. [RSpec scenarios](../spec) exercise the
public contracts; see [Testing](testing.md) to run them.

### Routine commands

Create accepts `operation`, `title`, `goal`, and a nonempty `criteria` array.
Actions are `edit`, `run`, `stop`, and `delete`; each also requires the current
`revision`. Edit supplies the same definition fields; run accepts optional text
`inputs`; stop requires the occurrence's `run` ID. Ownership comes from the route,
not the body. `/api/state` exposes definitions and occurrence history in
`personal_routines`, and saved command refusals in `routine_errors`.

Reuse the same operation ID and unchanged request to recover its original response
without duplicating work, including after restart or while the corpora is paused.
Reusing an ID with changed contents returns 409; use a new operation for new inputs.
For an existing owner and valid operation ID, first refusals are saved as command outcomes; operation-ID conflicts also appear
in the existing bounded activity history. Stale definition revisions return 409.
A stopped occurrence cannot be reopened through ordinary call retry.

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
