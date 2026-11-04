# Prepared workflow branches

Ordinary agent work keeps its existing loop. When preparation chooses a workflow, it
produces a finite immutable structure containing isolated Python calculations,
response-only model steps, authorized agent steps, strict Boolean branches and result
steps. Isolated scripts retain their existing capability restrictions; cycles are
unavailable. Finite fork/join may run independent prepared regions in parallel.

A condition is retained data, not a prose edge evaluated implicitly. A calculation or
explicit model step produces {value:boolean,reason:text}; a branch selects its existing
true or false successor. Missing, null, malformed or unknown conditions stop the workflow.
Selectors read the accepted input, prior outputs, literals or bounded object/array
construction. A prior output must exist on every incoming path. No expression evaluator
or runtime-added structure is involved. Definition size is limited to64KiB/256nodes;
node outputs use the existing64KiB bound.

The whole candidate pins structure, sources, runtime, model policy, prompt and accepted
contract. Independent fixtures execute the actual isolated scripts; model stubs are
fixture evidence only. Independent review and the receiving Routine's own authorized
occurrence and activation audit are still required. Real occurrences call the actual
response-only provider. Selecting a branch does not create a new structure version;
changing nodes, links or implementations changes the immutable candidate identity.

Each admitted node retains its exact input, implementation/policy identity and output
hash. Branch history shows its input, Boolean result, reason and selected successor;
unvisited nodes are skipped when the selected path finishes. Stop prevents later node
admission. Restart reuses completed matching receipts; an admitted node without a complete
result stays held and is never automatically replayed. The original workflow deadline
is retained across restart. A completed workflow is not recalculated merely to assess
or display its retained result.

A fork declares its complete ordered child set and matching join before execution. Child
regions are disjoint, finite and must reach that join; nested forks are supported. Join
reads only outputs guaranteed by each required child. Failed, stopped or unknown children
block the join rather than becoming missing values or partial success.

Children use existing managed Calls, Pulse admission, owner capacity and process cleanup.
A waiting parent is durably queued and releases its execution slot and lease; it reacquires
capacity before continuing. It remains addressable by Stop. Each child has its own process
handle and checked cohort membership. Prepared regions inherit the exact root contract
and implementation. Agent Calls have a local contract derived from their pinned node;
explicit other-Sapi assignment uses the existing Chief routing permissions. Ordinary work by other owners competes for the same host capacity.

A root repair/preparation reserves its existing resource once while its fork cohort is
active. Children inherit that reservation rather than receiving fresh time. Recorded active
time is the union of active parent/child intervals; overlapping children are counted once,
and proven all-queued idle time does not consume active resource. Original business wall
deadlines still apply. Provider usage is summed separately. A known provider backoff closes
its measured segment and later uses the original remaining resource; it does not reset
completed children or preparation checks.

Stop revokes the canonical root gate and propagates to queued and live descendants.
Reservation settlement waits for confirmed live-process exit. Completed child receipts
remain visible. Restart can preserve a proven idle cohort with complete/never-admitted
children; an open or uncertain activity keeps its reservation consumed and blocks free
replay. The Routine history shows child states, waiting/blocked joins and union accounting.

A defer step selects one finite wait: a timezone-aware `at` timestamp or a nonnegative
integer `afterMilliseconds`, together with nonempty `progress` and a JSON `value`.
Its immutable `next` names an existing successor; only `value` becomes that step's
output. Relative due time is resolved once and retained with the original input,
structure and version pins. The existing Pulse wakes the same managed Call after its
due time and normal capacity admission; waiting holds neither a slot nor a computer
lease. Completed receipts are reused, and the original business wall deadline remains
unchanged. Proven idle waiting does not consume active repair/preparation time.

Before wake and successor admission, current authority, the root gate and original
deadline are checked again. Stop, deletion or revoked permission prevents continuation;
an expired or blocked wait retains a visible reason. History shows due time, saved
progress, successor and wait status. Independent preparation fixtures validate wait
inputs using explicitly simulated timing; authorized occurrences perform real waits
before activation. Waiting for declared child results uses the fork/join contract above;
external results use an `awaitResult` node with `input` and an immutable `next`.
Its selected input is exactly `{source:{workload,call},progress}`. The exact source
object must occur explicitly in the accepted input. It identifies an existing personal
root execution belonging to the same Sapi, including a separately started Task or
Workflow; partial Calls, Group results and mutable Workflow names are unavailable.
The host derives and retains the source Task identity, accepted contract hash and
implementation binding. Only an accepted whole Completed result with a successful
terminal frontier permits continuation. Delegated work retains its original root
identity. The output contains `source`, the accepted `outcome`, and `value` (the retained
runtime envelope, or null for an ordinary Task); the waiting Task still needs its own
completion assessment.

Result waits reuse the same Call, Pulse, resource, original wall deadline, Stop and
restart rules as defer. Authority and source pins are checked both at readiness and
at actual consumption after capacity admission. Failed, unresolved, unknown, stopped,
deleted, rejected or inaccessible sources leave a visible blocked reason. No source
is automatically rerun, and a blocked wait does not retarget a newer execution.
Independent fixtures use explicitly simulated `modelStubs` entries for result nodes;
they never grant access to a real source or authorize business continuation.

For an accepted ordinary personal Task, its Sapi may choose prepared execution
before business actions. Preparation checks a separate immutable candidate under
one shared 30-minute active preparation allowance for that source Task and all its
versions. A new request ID does not refill it. The original Task waits while an
internally authorized child verifies the exact candidate against its original
criteria and input; only an accepted actual result can settle the original Task.
A new version requires its own checks and actual verification. Failed or interrupted
preparation returns retained evidence to the Sapi without replaying old providers.
Task details show preparation, version, checks, verification, result and Stop.
No user Compose or Run action is required. Multi-input batches and prior business
or unknown tool activity retain ordinary execution instead of changing execution.

An instrumental step is a closed node
`{kind:"agent",instruction,criteria:["local obligation"],input:SELECTOR,next:"id"}`
with an optional `recipient:SELECTOR`. Recipient selects a nonempty Sapi ID or null
for the current owner. Its output references must dominate the node just like input
references. Recipient selection grants no authority: current owner/recipient activity,
Chief-to-Sapi or Sapi-to-Chief routing, recipient availability and existing routing
bounds are checked. A response-only content consumer cannot execute agent tools.

The agent uses its ordinary authorized Sapi execution, workspace, tool policy and
effect admission under one managed Call. Its internal provider tool cycle is bounded
by the inherited resource and original deadline. Only an Outcome may finish that
Call; it cannot compose, hand off, change the outer graph or publish root completion.
Local criteria describe an intermediate obligation within the original assignment.
The outer task retains its original accepted goal, inputs and completion criteria.

The retained output is `{reply,outcome,author}`. Only a Completed local outcome with
complete successful provider capture becomes an output. Existing script/model nodes
and selectors can consume that output or produce a strict branch predicate. Text is
not implicitly parsed into a Boolean or treated as proof of the whole task. Calls,
ordinary tool evidence and local outcomes are visible separately from node receipts.

The parent yields its slot while the agent Call is queued or running; agent Calls
use the same Pulse, owner capacity, original root budget and Stop gate as other work.
Parallel recipients share active-time union accounting and join still requires every
child. Stop or changed authority prevents acceptance and later nodes. Incomplete or
unknown effects remain held without automatic agent retry; completed outputs remain
reusable across restart. Pending agent waits are also checked when a retired owner
is excluded from ordinary admission, so refusal remains visible.

Independent preparation examples supply explicitly simulated `modelStubs` for agent
nodes. They invoke no agent tools and do not authorize business execution or activate
the candidate. The receiving consumer still requires its checked candidate and own
authorized actual occurrence/audit. The immutable candidate pins the agent instruction,
criteria and additional workflow-agent prompt; ordinary Sapi policy remains the
authority for tools rather than a permissions field in the graph.
