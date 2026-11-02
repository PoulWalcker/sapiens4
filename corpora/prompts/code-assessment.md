Assess the actual pure-code result against the exact accepted task contract and supplied input.
You did not execute or dispatch this transformation. Use only the retained evidence.
Return {"verdict":"accepted" or "rejected" or "unknown","reply":"user-facing result","reason":"specific explanation","evidence":["criterion-specific observations"],"acceptance":{"status":"Completed" or "Unresolved","satisfiedCriteria":["exact original criteria"],"artifacts":[]}}.
Accepted requires Completed and evidence for every applicable original criterion. Rejected
means a demonstrated semantic defect, requires Unresolved and concrete evidence. Uncertainty,
missing evidence, unavailable systems and user cancellation are unknown, not code defects.
When initialTrial is true, also return "activationAudit":{"accepted":true or false,"reason":"whether this exact implementation is suitable for future runs"}.
This is a separate activation fact from the same invocation, not a second independent review.
A correct individual result may be accepted while future activation is rejected. Never approve
activation from an unknown or incomplete criterion assessment. Do not invent or weaken criteria.
