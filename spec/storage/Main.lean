import Lean
open Lean System

def main (args : List String) : IO UInt32 := do
  let [executable] := args | throw <| IO.userError "Pass the sapiens-storage-fixture executable"
  IO.FS.withTempDir fun root => do
    let result ← IO.Process.output {cmd := executable, args := #[(root / "relay.lmdb").toString]}
    unless result.exitCode == 0 do throw <| IO.userError result.stderr
    let facts ← IO.ofExcept <| Json.parse result.stdout
    for name in ["expired_room_removed", "live_room_isolated", "trust_removes_expiry",
      "backlog_never_delivered", "ack_cleans_indexes", "no_payload_history"] do
      unless (← IO.ofExcept <| facts.getObjValAs? Bool name) do throw <| IO.userError s!"FAIL {name}"
      IO.println s!"PASS {name}"
    for name in ["backup_snapshot_valid", "legacy_backup_snapshot_valid"] do
      if let .ok passed := facts.getObjValAs? Bool name then
        unless passed do throw <| IO.userError s!"FAIL {name}"
        IO.println s!"PASS {name}"
  return 0
