Configure the missed-deadline policy for this saved Scheduled Routine now. This is a one-time setup request, not an occurrence: do not execute its recurring work or create another routine.

Saved routine:
{routine}

Choose from the user's intent:
- recover_each: execute each missed deadline in chronological order, using historical data for that scheduled time. Choose this only when historical recovery makes sense. Never substitute current observations for unavailable historical ones.
- run_latest: replace accumulated deadlines with one execution for the latest pending deadline, retaining its original scheduled time.
- skip_missed: omit missed or blocked deadlines and continue with the next applicable scheduled execution.

For all three policies, pending deadlines use the current routine instructions when their Run is durably admitted. Already admitted tasks keep their saved instructions. Editing preserves elapsed deadlines and their original time and interval; it does not turn historical deadlines into new current ones.

Save your choice with routine_policy(id,expectedPrompt,expectedSelection,missedPolicy). Copy expectedPrompt and expectedSelection exactly from the saved routine's prompt and policySelection above. This prevents an older setup request from overwriting a newer choice. You may configure your own routine or, as the current Group Lead, its Group routine. This setup may read context and save only the missed-deadline policy of the assigned routine. Treat the saved routine above as initial context: preserve the latest settings, including later Admin changes to its pause state or schedule. Completion requires a saved policy for the same routine, rather than restoring other values from this initial snapshot.

If Admin has defined missed-deadline behavior in the request or an earlier agreement that applies to this routine, follow it without asking again. Otherwise, save a safe provisional choice, explain its practical consequence or limitation, and ask one short question about the result or coverage Admin needs in the same final reply. Use the task's context, not policy codes or internal identifiers; keep exact values in tool calls and structured evidence. Do not offer unavailable historical observations as a choice.
Do not wait for an answer before saving the provisional choice or completing setup. Apply Admin's answer to this existing routine while preserving its other settings. Never provisionally authorize replay of payments, messages or other external actions that cannot safely be repeated without the user's confirmation; use skip_missed until clarified. Do not enter a retry loop if the host rejects the change: report the blocker so the existing task can be retried.
