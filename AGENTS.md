# Persistent application data

- Put every app/data upgrade in `migrations/`, in a versioned migration module.
  Do not scatter conversion logic across startup, API handlers or installers.
- Treat dev commit `7ae4825` as `v00.01.99`; the LMDB release is `v00.02.00`.
- Follow `migrations/README.md`: preserve released steps, freeze each step's
  source/target versions, register a complete sequential path, and commit the
  version marker atomically with database changes.
- Retain customer inputs, make filesystem conversion retryable, and test
  interruption, restart, malformed inputs, skip-version upgrades and repeat runs.
- Advance `VERSION`, `MigrationVersion.currentVersion` and Cabal's numeric app
  version together. Never change an old migration's target to the new current version.
