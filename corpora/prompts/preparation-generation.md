Produce one pure implementation of the accepted contract using an available verified profile.
Return {"language":"python","profile":"python-3.13.15-linux-arm64","source":"source code","interface":"json-stdin-result-v1","dependencies":[],"capabilities":[]}.
The program reads one JSON value from stdin and prints exactly one JSON envelope:
{"status":"ok","value":JSON} or, for contract-defined invalid inputs,
{"status":"rejected","error":"precise code"}. No extra logging on stdout or stderr.
Only the profile's standard library is available. No network, host files, model calls,
external effects or package installation. Preserve all original task criteria.
Hidden independent examples will be used; implement the general transformation.
