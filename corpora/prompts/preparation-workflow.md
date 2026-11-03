
For a task with meaningful finite outer branches, you may prepare an immutable workflow
instead of a single source/reuse/explanation. Return exactly {"workflow":{"entry":"id","nodes":{...}}}.
Closed node variants:
{"kind":"script","source":"isolated Python reading JSON stdin and printing JSON","input":SELECTOR,"next":"id"}
{"kind":"model","instruction":"Return a JSON value; response-only reasoning, no tools","input":SELECTOR,"next":"id"}
{"kind":"branch","input":SELECTOR,"true":"id","false":"id"}
{"kind":"finish","input":SELECTOR}
A branch consumes exactly {"value":true_or_false,"reason":"nonempty evidence-based reason"}.
Unknown/null is unresolved, never false. Finish returns the original ok/rejected envelope.
Selectors are {"input":true}, {"output":"completed-node-id"}, {"literal":JSON},
{"object":{"field":SELECTOR}}, {"array":[SELECTOR]}. Input/output selectors may have a
strict JSON pointer such as "pointer":"/value". Outputs must exist on every incoming path.
No expressions, runtime-added nodes, cycles, parallel/defer, host files/network/effects,
packages or tool-capable model steps. At most256nodes and64KiB total definition. Pure
script logic handles calculation/formatting; the host retains each selected step receipt.
For independent workflow examples, PreparationFixtures may add modelStubs:{"node-id":JSON}
to each fixture. Define expectations/stubs independently before seeing generated source.
Stubs are fixture-only evidence; real occurrences invoke the actual response-only model.
Prefer a single implementation for simple tasks. Never combine workflow with source/reuse.
