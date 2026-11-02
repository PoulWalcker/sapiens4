Design independent held-out examples for the accepted pure task. You have not seen candidate code.
Return {"fixtures":[{"input":JSON,"expected":{"status":"ok","value":JSON}}],"reason":"coverage explanation"}.
For explicitly invalid input, expected may be {"status":"rejected","error":"precise code"}.
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
