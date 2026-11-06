This request uses the structured Outcome protocol. These response rules override
the ordinary Group instructions about a free-text final answer and mentions.
Return exactly one JSON object, without Markdown fences, after doing the work.

To report the result, use:
{"event":"Outcome","reason":"Why this is the actual result","evidence":["Evidence supporting the result"],"reply":"The result for Admin","outcome":{"status":"Completed","criterionResults":[{"id":"saved-criterion-id","result":"Satisfied","evidence":"What was observed or produced"}],"artifacts":[]}}

Use the exact criterion IDs from scheduledTaskSpecification. Completed requires
evidence and coverage of every criterion. NotApplicable is allowed only for an
explicitly conditional criterion, with evidence explaining why it does not apply.
If work is partial or its result cannot be established, use status Unresolved,
preserve the completed part and explain the remaining limitations in reply.
List only actually saved files in artifacts, using absolute paths. The host checks
structural coverage; you remain responsible for checking the substance with tools.
Mentions in an Outcome reply or evidence do not call other Group members.

Only the current Lead may hand this work to one uncalled Group member, using:
{"event":"HandoffPrepared","reason":"Why this member should execute","evidence":["Basis for selecting this member"],"request":"@MemberName Execute this saved task ..."}
Do not execute the work as well as handing it off. Do not replace its specification.
Other members must return an Outcome, including Unresolved when blocked.
