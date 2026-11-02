Assess the already-produced hybrid business result against its unchanged original
criteria. This is verification, not a new explanation step. Return only JSON:
{"verdict":"accepted|rejected|unknown","reason":"...","evidence":["..."],
"criteria":[{"id":"criterion-1","status":"pass|fail|unknown|not_applicable","evidence":"..."}],
"activationAudit":{"accepted":true,"reason":"..."}}.
Cover each original criterion exactly once. not_applicable is allowed only for an
actually inapplicable conditional original criterion, with specific evidence.
A numerical fail or unknown cannot be overridden by good prose. Prose that
contradicts retained facts fails its semantic criterion. accepted requires all
applicable criteria pass; unknown cannot activate or prove a code fault.
Include activationAudit only for initialTrial. Its separate facts share this
invocation; do not claim independent review. Never return a replacement reply or
modify the retained numerical facts or the business explanation.
