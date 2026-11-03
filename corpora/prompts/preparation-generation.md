Produce one pure implementation of the accepted contract using an available verified profile.
Return {"language":"python","profile":"python-3.13.15-linux-arm64","source":"source code","interface":"json-stdin-result-v1","dependencies":[],"capabilities":[]}.
The program reads one JSON value from stdin and prints exactly one JSON envelope:
{"status":"ok","value":JSON} or, for contract-defined invalid inputs,
{"status":"rejected","error":"precise code"}. No extra logging on stdout or stderr.
Only the profile's standard library is available. No network, host files, model calls,
external effects or package installation. Preserve all original task criteria.
Hidden independent examples will be used; implement the general transformation.

For a hybrid task requiring meaningful prose, the code may return numerical facts
and the response may additionally include "explanation":{"instruction":"..."}.
This selects exactly calculation followed by a response-only owner explanation.
The instruction consumes actual facts and original inputs; it cannot call tools,
change criteria or invent data. The host pins its model policy and checks the
independent original-criteria rubric. Omit explanation for fully scripted work.

When contentInterface is supplied for a receipt-feed profile, follow that frozen
choice exactly. Input is {schema:1,interface:"receipt-feed-input-v1",taskInputs,
observation,newRecords,previouslySeenIds}. Process host-derived newRecords;
previouslySeenIds contains only IDs relevant to this page, not all historical IDs.
For script-text-v1 return value:{text:"actual content to deliver"} and no explanation.
For hybrid-explanation-v1 return facts and an explanation instruction. The host owns
profile identity, cursor progress and delivery; do not return an inputBinding or
choose endpoints. Never use the assessor to write the business content.
