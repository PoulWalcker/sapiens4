# Finite repair continuation

An exhausted Routine repair case can receive an explicit additional amount of active
repair time. The shipped **Continue repair…** dialog starts at 5 minutes; edit the whole
minute amount and enter a reason. Approval queues owner preparation or retained-result assessment for the
same case. There is no automatic refill or escalation and no new fixed attempt cap.

A grant adds to the original case ceiling. It preserves prior spending, reservations,
history, contract/version pins and unknown outcomes. It does
not reopen an old provider operation, reset its deadline or retry counter, repeat a
confirmed effect, calculate a business result or authorize delivery. Unknown provider
usage remains unknown. A grant does not reconcile unknown effects.

Only exhausted active resource is eligible. Provider-attempt exhaustion with active time
remaining, interrupted/uncertain reservations, active repair work, changed instructions,
explicit Stop and unresolved consumer effects block continuation. Historical operations
remain available as evidence; the continuation uses a separate recorded operation.

**Stop repair…** stops this case's queued and active repair work through the existing
execution gates. It blocks later admission and late publication, preserving unused grants
and measured spending in history. It does not undo completed effects or stop unrelated
cases and safe ordinary work. A grant cannot override that Stop. Restart or a new business
run never creates additional resource by itself.

The existing Admin command API accepts `POST
/api/agents/:owner/routines/:routine/repair-grant` with `operation`, current `revision`,
exact `case`, positive integral `minutes` and nonempty `reason`. The grant, new waiting
phase and command receipt are committed together. Repeating identical contents with the
same operation id returns that receipt; conflicting contents fail. `stop-repair` accepts
`operation`, `revision`, `case` and `reason` for the case-wide Stop. Model host-control
has no grant command. These commands use the existing trusted local Admin boundary.

An owner can inspect an ordinary Routine with `routine_inspect(id)` and request
`routine_recover(id)` when retained unknown actions block its restoration. Recovery
queues one independent response-only review through the existing call runtime,
with a 90-second provider deadline. The tool receipt is not proof of recovery.
Inspection exposes the review's saved result. Repeated requests for identical
source evidence and review policy reuse that call, including negative or stopped
reviews; changing frequency does not provide another allowance.
An independently recorded configuration refusal before provider process creation
can resume the same call and operation after compatibility is verified. Prior
attempts remain, and only unused active review time remains available. This does
not apply to Stop, negative reviews or uncertain admission after process creation.

Only complete retained requests and terminal tool outcomes can be reviewed.
Missing, shortened or redacted action details and interrupted tails remain
blocked. A positive review must cover every observed action as a task-authorized
read. The host binds it to the exact original work and checks that evidence,
ownership, contract and Stop authority have not changed before accepting it.
It retains the original uncertainty marker and failed result, adding independent
proof for future authorized admissions. It never replays the old occurrence or
publishes its result. Registered external effects and generated-code repairs
retain their existing receipt and repair paths. Recovery is confirmed to the
user only after a new authorized report actually arrives.
