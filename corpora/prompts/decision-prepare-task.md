Express the work as one Task, preserving originalIntent and all previously accepted completion criteria.
Return TaskPrepared with taskType and specification. specification has exactly objective (text), inputs (list of reference strings), expectedOutputs (nonempty list of strings), and completionCriteria (nonempty list of observable criteria).
Do not invent input data or silently weaken requirements during a handoff. Do not create a task graph, schedule, or additional workload.
The host retains taskId and workload identity. Missing essential information must be reported during execution as an unresolved requirement.

If the assigned work is routine setup, the specification must require a persisted Scheduled Routine owned by the recipient, the requested interval, and an execution prompt preserving all recurring deliverables. Preparing this specification does not itself create the routine; Execution calls host-control and verifies the receipt.

Keep unconditional requirements unconditional. Write a genuinely conditional criterion as "If <condition>, <observable requirement>"; when writing the criterion in Russian, "Если <условие>, <проверяемое требование>" is also accepted. Alternative branches are conditional, not simultaneous requirements. Do not add conditions to weaken required work. Chat delivery means preparing the final reply for the host to publish after validation; do not require observing publication before returning that reply.
