
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
No expressions, runtime-added nodes, cycles, host files/network/effects,
packages or tool-capable model steps. At most256nodes and64KiB total definition. Pure
script logic handles calculation/formatting; the host retains each selected step receipt.
For independent workflow examples, PreparationFixtures may add modelStubs:{"node-id":JSON}
to each fixture. Define expectations/stubs independently before seeing generated source.
Stubs are fixture-only evidence; real occurrences invoke the actual response-only model.
Prefer a single implementation for simple tasks. Never combine workflow with source/reuse.

Finite same-owner fork/join is available through two additional closed variants:
{"kind":"fork","children":["left-entry","right-entry"],"join":"joined"}
{"kind":"join","fork":"fork-id","input":SELECTOR,"next":"successor"}
Each distinct child region must reach its matching join, cannot overlap another region
or finish/escape early. Nested finite forks are allowed. Join selectors can combine
outputs guaranteed by each required child; ordered array selectors retain declared order.
A waiting parent releases its slot. Each child uses the existing shared owner capacity,
original resource and Stop gates. Model nodes remain response-only and belong to this
same owner; no recipient assignment or tool permission is introduced.

Timed defer is a closed node: {"kind":"defer","input":SELECTOR,"next":"existing-node"}.
Selected input must be exactly {"at":"timestamp with UTC/offset timezone","progress":"nonempty text","value":JSON}
OR {"afterMilliseconds":nonnegative_integer,"progress":"nonempty text","value":JSON}.
The host resolves relative time once, saves due/pins, releases the slot and resumes the
same node/successor through Pulse. The original business wall deadline is never extended.
Defer's output is value; use that retained output instead of rewriting accepted inputs.
Independent fixture timers are explicitly simulated after identical format validation;
real authorized occurrences must actually wait. Fixtures never authorize business wake.
Own-child result waiting uses the declared join. No external Task/Workflow references.
