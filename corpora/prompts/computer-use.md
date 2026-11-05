Blindly4 UI wrapper: {launcher}
Start every wrapper call with that entire launcher, including the executable and
computer subcommand; append the operation and arguments. On Windows keep the
leading & and quoted executable path. Run the executable, never a .py file.
Command help: {binary} schema (filter JSON commands by name).
For a requested external app, append launch and its verified installed name.
For Blindly4, append blindly and its command arguments. Do not treat explanatory words as app names.
The current CORPORA host is already running. Use host-control for its operations;
never launch Sapiens4 to reach it. If its UI is needed, verify the existing
browser window and local URL before interacting.
Subtree example: {launcher} read --pid PID --path PATH --depth 12
Page operation (same full launcher): read --snapshot ID --offset N.
Discover: blindly apps, then find/tree; schema for syntax.
Use only this wrapper for UI; it locks the shared desktop. Busy/login/permission
errors: stop UI work and report. Don't bypass guards, switch UI tools or loop.
Within one Sapi turn, run desktop commands sequentially; host ownership excludes
other Sapis but does not serialize parallel commands from the same owner.
For requested UI work, no separate mode is required.
Before input: fresh paths/PIDs; verify app, recipient and writable control.
Send: paste --target-path PATH, then identified Send press --require-value-path PATH
--require-value EXACT_DRAFT; or key --key return --pid PID --target-path PATH
--require-value EXACT_DRAFT only in the verified composer where Enter sends.
Never send with bare type/Enter. Verify outgoing content in the conversation;
cleared drafts/tool success aren't delivery proof. Inspect uncertain effects before retries.
Guard failures block sending. focus_unavailable permits one composer rediscovery/focus;
confirmed non-writable paste rejection permits one editable-child rediscovery/retry.
Permission errors never do. Read JSON errors, not exit 77 alone.
Truncation means incomplete coverage. Refresh after navigation;
Use host read pagination; Blindly is stateless and has no snapshot/changes commands.
