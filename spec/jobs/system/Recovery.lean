import io.Host

open Lean
namespace Spec.SystemJobs

def preserveInterruptedWorkAcrossCrash : IO Unit := IO.FS.withTempDir fun directory => do
  let prompts := directory / "fixture-prompts"
  let turnId ← try
    withCrashedHost directory fun host => do
      IO.FS.createDirAll prompts
      IO.FS.writeFile (prompts / "hold") ""
      let id ← host.submit (← host.chief) "Review the proposal after a crash"
      discard <| host.awaitTurn id "running"
      discard <| untilSome "provider started" do
        return if (← prompts.readDir).size > 1 then some () else none
      return id
    finally IO.FS.removeFile (prompts / "hold")
  for _ in [0:2] do
    withHost directory fun host => do
      let recovered ← host.awaitTurn turnId "interrupted"
      IO.ofExcept do
        fieldsMatch recovered (json% {input: "Review the proposal after a crash", status: "interrupted"})
        requireThat "missing recovery explanation" (!(← stringField recovered "error").isEmpty)
      IO.ofExcept <| requireThat "restart repeated an uncertain provider call" ((← prompts.readDir).size == 1)

def rejectConcurrentStorageOwner : IO Unit :=
  IO.FS.withTempDir fun directory => withHost directory fun host => do
    let before ← host.state
    let root ← repoRoot
    let contender ← IO.Process.output {
      cmd := (binary root "sapiens4").toString
      args := #["--desktop", "--port", "0", "--data-dir", directory.toString],
      cwd := root, env := ← fixtureEnvironment directory} (some "")
    IO.ofExcept <| requireThat "a second host failed to respect the data-directory lock"
      (contender.exitCode != 0 && contender.stderr.contains "Another Sapiens4 server")
    let after ← host.state
    IO.ofExcept <| requireThat "contending host changed the population"
      (before.getObjValD "agents" == after.getObjValD "agents")
    discard <| host.completedMessage (← host.chief) "The original host still works"

def releaseExclusiveComputerAccessOnCancellation : IO Unit :=
  IO.FS.withTempDir fun directory => withHost directory fun host => do
    let chief ← host.chief
    let colleague ← host.agent "Operator"
    let acquire := json% {op: "computer_acquire"}
    discard <| host.control chief acquire 409
    let prompts := directory / "fixture-prompts"
    IO.FS.createDirAll prompts
    IO.FS.writeFile (prompts / "hold") ""
    try
      let first ← host.submit chief "Review the workspace"
      let second ← host.submit colleague "Check the workspace"
      discard <| untilSome "both active provider calls" do
        let calls := (← prompts.readDir).filter (·.fileName != "hold")
        return if calls.size == 2 then some () else none
      IO.ofExcept <| fieldsMatch (← host.control chief acquire) (json% {owner: $(chief)})
      let denied ← request host.url "POST" ("/api/agents/" ++ colleague ++ "/control") acquire
      let lease ← IO.ofExcept <| (← host.state).getObjVal? "computer"
      IO.ofExcept do
        requireThat "concurrent computer access was accepted" (denied.status == 409)
        fieldsMatch lease (json% {owner: $(chief)})
      discard <| host.api "POST" ("/api/agents/" ++ chief ++ "/turns/" ++ (← IO.ofExcept first.getStr?) ++ "/cancel") (json% {})
      discard <| untilSome "release of the cancelled turn's lease" do
        let state ← host.state
        let owner ← IO.ofExcept do (← state.getObjVal? "computer").getObjVal? "owner"
        return if owner == .null then some () else none
      IO.ofExcept <| fieldsMatch (← host.control colleague acquire) (json% {owner: $(colleague)})
      IO.FS.removeFile (prompts / "hold")
      discard <| host.awaitTurn second "done"
      let computer ← IO.ofExcept <| (← host.state).getObjVal? "computer"
      IO.ofExcept <| fieldsMatch computer (json% {owner: null})
    finally
      if ← (prompts / "hold").pathExists then IO.FS.removeFile (prompts / "hold")

end Spec.SystemJobs
