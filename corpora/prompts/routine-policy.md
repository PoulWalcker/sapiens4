Configure the missed-deadline policy for this saved Scheduled Routine now. This is a one-time setup request, not an occurrence: do not execute its recurring work or create another routine.

Saved routine:
{routine}

Choose from the user's intent:
- recover_each: execute each missed deadline in chronological order, using historical data for that scheduled time. Choose this only when historical recovery makes sense. Never substitute current observations for unavailable historical ones.
- run_latest: replace accumulated deadlines with one execution for the latest pending deadline, retaining its original scheduled time.
- skip_missed: omit missed or blocked deadlines and continue with the next applicable scheduled execution.

For all three policies, pending deadlines use the current routine instructions when their Run is durably admitted. Already admitted tasks keep their saved instructions. Editing preserves elapsed deadlines and their original time and interval; it does not turn historical deadlines into new current ones.

Save your choice with routine_policy(id,expectedPrompt,expectedSelection,missedPolicy). Copy expectedPrompt and expectedSelection exactly from the saved routine's prompt and policySelection above. This prevents an older setup request from overwriting a newer choice. You may configure your own routine or, as the current Group Lead, its Group routine. Do not change its schedule, pause state or run count.

If the user's intent is ambiguous, select a reasonable provisional policy, save it first, then explain the choice and ask a short question in the same reply. Do not wait for an answer before saving a policy. Never provisionally authorize replay of payments, messages or other external actions that cannot safely be repeated without the user's confirmation; use skip_missed until clarified. A saved provisional choice is sufficient to complete this setup request. Do not enter a retry loop if the host rejects the change: report the blocker so the existing task can be retried.
