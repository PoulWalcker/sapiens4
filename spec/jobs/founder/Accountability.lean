import io.Host

open Lean
namespace Spec.Founder

def reviewFailedWorkBeforeRetry : IO Unit := IO.FS.withTempDir fun directory => do
  let prompts := directory / "fixture-prompts"
  let input := "Review the launch costs"
  let (owner, turnId, audit) ← withHost directory fun host => do
    IO.FS.createDirAll prompts
    IO.FS.writeFile (prompts / "fail-next") ""
    let owner ← host.chief
    let turnId ← host.submit owner input
    discard <| host.awaitTurn turnId "failed"
    let id ← IO.ofExcept turnId.getStr?
    let auditPath := "/api/agents/" ++ owner ++ "/turns/" ++ id ++ "/audit"
    let record ← host.api "GET" auditPath
    IO.ofExcept do
      let message ← stringField record "error"
      requireThat "provider failure missing from the audit" (message.contains "Fixture provider unavailable")
      let calls ← record.getObjValAs? (Array Json) "logs"
      let #[call] := calls | throw "Expected one failed assessment"
      fieldsMatch call (json% {role: "Assessing"})
      requireThat "the reviewed request is missing" ((← stringField call "prompt").contains input)
    return (owner, turnId, (auditPath, record))
  withHost directory fun host => do
    discard <| host.awaitTurn turnId "failed"
    let persisted ← host.api "GET" audit.1
    IO.ofExcept <| requireThat "restart changed the failed audit" (persisted == audit.2)
    IO.ofExcept <| requireThat "restart repeated the failed call" ((← prompts.readDir).size == 1)
    discard <| host.api "POST" ("/api/agents/" ++ owner ++ "/turns/" ++ (← IO.ofExcept turnId.getStr?) ++ "/retry") (json% {})
    let completed ← host.awaitTurn turnId "done"
    IO.ofExcept <| fieldsMatch completed (json% {input: $(input), output: "The review is ready."})
    IO.ofExcept <| requireThat "explicit retry should make one call" ((← prompts.readDir).size == 2)

end Spec.Founder
