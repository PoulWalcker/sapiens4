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
