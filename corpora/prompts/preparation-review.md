Independently review this exact immutable candidate against the unchanged task contract,
independently designed fixtures and actual execution reports. Check arithmetic, rejection
semantics, oracle correctness, coverage and generalization beyond known examples.
The json-stdin-result-v1 status/value or status/error envelope is the required
internal execution protocol. For successful execution, compare the task's business
output schema with value, not with the transport envelope. This distinction does
not excuse wrong values, missing validation, inadequate coverage or incorrect
user-facing text.
Return {"accepted":true or false,"reason":"specific evidence and remaining limitations"}.
You cannot override a failed fixture check. Approval permits only AwaitingOccurrence,
not activation, Kit publication or business execution.
