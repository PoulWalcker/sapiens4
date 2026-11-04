Design independent held-out examples for the accepted pure task. You have not seen candidate code.
Return {"fixtures":[{"input":JSON,"expected":{"status":"ok","value":JSON}}],"reason":"coverage explanation"}.
For explicitly invalid input, expected may be {"status":"rejected","error":"invalid_input"}.
Use exact rejection codes when the accepted contract specifies them. Otherwise
use the shared error code "invalid_input" for every contract-defined invalid input;
do not invent a separate error vocabulary. This convention does not change which
inputs must be rejected or permit rejecting valid inputs.
Every example needs an independently computed expected result. Include different new inputs,
empty/boundary and invalid cases where appropriate. Do not merely repeat observed inputs.
The generator will not receive these examples or answers. If reliable expectations cannot
be established, return {"fixtures":[],"reason":"what is missing"}; do not invent an oracle.

If the original task needs exact computation followed by useful explanatory prose,
also return "rubric":[{"id":"criterion-1","kind":"numeric"},...]. The context
contains host-assigned IDs and the exact original criterion text. Classify every
original criterion once as numeric or semantic, with at least one of each. Do not
add, drop or rewrite criteria. Expected fixture values describe calculation facts,
not prose. This rubric is frozen before any candidate generation.

For a receipt-feed profile, freeze contentInterface before generation:
"script-text-v1" for pure text production, or "hybrid-explanation-v1" for useful prose
following computed facts. Every fixture input is {"taskInputs":JSON,"page":complete
receipt-feed page,"priorSeenIds":["synthetic previously seen IDs"]}. The host derives
the consumer input and newRecords; do not supply invented hashes. Pure successful
expected values are {"text":"independently computed delivery text"}. Hybrid requires
the original-criteria rubric above. Include overlap and a genuinely new observation.

If the retained evidence cannot establish a supported input/result contract, return
{"status":"insufficient_evidence","reason":"specific missing observation"} instead
of fixtures. If the accepted task cannot use the supported preparation capability,
return {"status":"unsuitable","reason":"concrete limitation"}. This is part of
this existing fixture-design decision, not a request for another suitability call.
Never weaken the original criteria or invent a successful example to proceed.
