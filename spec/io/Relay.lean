import io.Host
import Std.Async.TCP

open Lean System
namespace Spec

private def availableRelayPort : IO UInt16 := do
  let socket ← Std.Async.TCP.Socket.Server.mk
  socket.bind (.v4 ⟨.ofParts 127 0 0 1, 0⟩)
  return (← socket.getSockName).port

def withRelay (use : String → String → IO α) : IO α := IO.FS.withTempDir fun directory => do
  let root ← repoRoot
  let port ← availableRelayPort
  let url := s!"http://127.0.0.1:{port}"
  let config := directory / "connection.json"
  let child ← IO.Process.spawn {
    cmd := (binary root "sapiens4").toString, cwd := root, stdout := .piped
    env := #[("SAPIENS_RELAY_ADMIN_TOKEN", none), ("RAILWAY_ENVIRONMENT_ID", none), ("SAPIENS_RELAY_BETA", none)]
    args := #["relay", "--bind", "127.0.0.1", "--port", toString port, "--origin", url,
      "--db", (directory / "relay.lmdb").toString, "--connection-file", config.toString] }
  try
    let startup ← IO.asTask (do
      discard child.stdout.getLine
      child.stdout.getLine) .dedicated
    let ready ← untilSome "the fixture relay" do
      if ← IO.hasFinished startup then return some (← IO.ofExcept startup.get)
      return none
    IO.ofExcept <| requireThat "relay did not start" (ready.trimAscii == "Remote relay ready.")
    let connection ← IO.ofExcept <| Json.parse (← IO.FS.readFile config)
    use url (← IO.ofExcept <| stringField connection "token")
  finally
    if (← child.tryWait).isNone then
      child.kill
      discard child.wait

end Spec
