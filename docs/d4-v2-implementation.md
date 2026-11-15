# D4 V2 implementation

Base: `4e610e56bb8832197bca6f9999473cbd40105a09`, freshly fetched from
`PoulWalcker/sapiens4:release/distribution-onboarding-provider-adapters`.
Branch: `dev/telemetry`. Worktree: `sapiens4-worktrees/telemetry-v2`.

Approved references: D4_JTBD_ARCHITECTURE_REDESIGN_V2,
D4_AUTH_IDENTITY_CONTRACT_FINAL (takes precedence for authentication), and
D4_HTTP_LIFECYCLE_SPIKE in the original checkout's `.claude/docs`.

## Plan and local commits

1. Authentication, identity and protocol: canonical Bearer codec, immutable
   private active/previous/revocation configuration, verified principals and
   one strict ten-column row model; private token persistence and random
   Corpora identity.
2. Consent and sender: fail-closed revision/CAS controls, explicit reset,
   bounded queue and retry episodes, one standard TLS manager/worker,
   nonjoining stop. No file/network work in capture.
3. Integration: trusted native/private parent pipe and local CLI, asynchronous
   settings/onboarding, executing-host hooks for all five events, headless.
4. Storage/deployment: reuse Parquet publication/ownership safety, remove
   speculative writer interfaces, configure mandatory public ingress controls
   and document coordinated auth cutover.

Each milestone gets a coherent local commit after compilation/static checks
where possible, with actual net production source/declaration LOC recorded.
Counts exclude tests, documentation, generated files, dependencies and locks;
nonblank/noncomment imports, pragmas and CPP count as source.

No automated tests are added or expanded. Final Windows/macOS manual
verification is a separate user-run release gate. No push, merge or PR.

## Milestone 1

Implemented the shared canonical 97-byte Bearer codec and 128-byte response
parser; HMAC issuance/verification, active/previous keys and denylist; private
1 MiB configuration loading; strict authenticated rows for all five events;
token-only profile, enrollment/renewal CAS, explicit reset persistence and
lazy random Corpora UUID. Reused the private filesystem primitives only.

Validation: GHC 9.10.3 `-fno-code -Wall` compiled all nine affected modules;
the public deterministic HMAC vector matched exactly in an interactive smoke
command. Host/native Cabal compilation succeeded; the initial host link
needed the existing Homebrew libsodium path supplied as a build override.
The collector HTTP executable is integrated in milestone 4.

Actual net production source versus release base: **+977 LOC**; Cabal
production declarations: **+6 LOC**. Counts include private Windows code,
which is not compiled on macOS. Ignore-file lines are separate.

Replaced/reference-only subsystems: Basic/password and DeviceId models,
Corpora privacy key/HMAC, layered wire/row model. None were imported into
this branch. No compatibility implementation and no automated tests.

Deviation: the credential codec and bounded duplicate/depth JSON guard are
small shared modules used by client and collector to keep their canonical
rules identical; they add no runtime/service boundary. No contract deviation.
