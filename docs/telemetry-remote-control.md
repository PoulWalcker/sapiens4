# Remote Control telemetry

Ticket 10 adds three Haskell source lines in `remote/control/Bridge.hs`.
`approvePairing` passes `IRemotePaired` and the executing service's opaque
Corpora reference to its existing nonblocking `serviceEmit`, after relay trust,
approval encryption and `saveConfig` succeed. Failed approval cannot reach this
hook. Invitations, pending proof, saved acceptance delivery, status reads,
connection updates and reopening a saved pairing do not call it.

The existing sender owns consent, identity preparation, EventId/time and HTTPS.
This is best-effort RAM telemetry: interruption between the business commit and
capture can lose an event. It is not a transactional pairing audit.

Forwarded requests still use `executeRemote` → `Service.dispatch` and ticket 07's
three acceptance hooks. A matching saved receipt returns its encrypted reply
before dispatch. No capture hook, sender or consent authority was added to the
Relay or controlling client. No module, dependency or event type was added.

## Focused macOS observations, 2026-11-15

The selected source follows `3b12492913e41299b963f07759b20b3e64ee0eb0` on
`telemetry/d4`. The changed Bridge source SHA-256 is
`167cb5363da5361f5c961938b3208ebcfd050ec6f233c717af038a4faea518c7`.
The actual host and macOS executable built with GHC 9.10.3 and the existing
`build/` dependency freeze; existing unrelated compiler warnings remain.

Owned temporary profiles, a real host and Relay, an independent encrypted peer,
scripted local providers, a fixture-only trusted TLS proxy and the real isolated
collector supplied these observations. No paid provider or production data was
used. CLI consent changes used the shared operator CAS authority.

| Observation | Result |
| --- | --- |
| Invitation/pending, bad proof, substituted peer, expired approval, unreachable Relay and immutable committed-file failure | PASS: zero pairing rows; failed persistence retained pending configuration |
| Successful approval after trust and committed peer | PASS: one `remote_paired` |
| Duplicate approval, resume, refresh and host restart/reconnection | PASS: no additional pairing row |
| Explicit revoke and genuinely new peer approval | PASS: a second `remote_paired` |
| Encrypted agent message, group message and task run | PASS: one `request_accepted` per kind |
| Exact encrypted receipt replay before and after host restart | PASS: same responses, no additional provider execution or accepted-request rows |
| Seven remote consent/private/traversal route attempts | PASS: rejected; private profile bytes unchanged |
| Encrypted state response | PASS: no stored Basic password or Corpora privacy key |
| Missing-consent host pairing and agent command | PASS: business success; zero enrollment/events, no profile or privacy-key creation |
| Independent saved-dataset read and Corpora HMAC calculation | PASS: DuckDB 1.5.5 read five selected rows, fourteen physical columns and five unique EventIds; all scoped IDs matched |
| Dedicated steady-state heartbeat observation, Windows/Linux execution, regressions, GUI/installer and full feature E2E | NOT RUN |

The five selected rows are two pairings and one accepted request of each kind,
under one installation/device. Wire and Parquet values match; no peer identity,
device label, request content, local path, raw Corpora UUID or privacy key enters
the selected telemetry. One additional dataset row belongs to an interrupted
fixture preparation run and is excluded by EventId, not counted as product use.
The first preparation used the generic helper's precreated `0755` Data Root;
identity preparation correctly failed closed. The successful run explicitly
selected a private `0700` fixture root. No application permission rule changed.

Sanitized commands, observations, dataset, hashes and the independent reader are
retained locally at
`/Users/pashvel/Desktop/personal/sapiens4-worktrees/telemetry10-macos-est2e65y/`.
Temporary host/peer/Relay processes and private profiles were removed. Collector
and proxy processes were stopped and their private keys removed after publication.
Ticket 10 implementation is complete; the listed unobserved platform/heartbeat
cases remain open. Comprehensive feature verification belongs to ticket 12.
