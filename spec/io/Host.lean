import pure.Privacy

open Lean System
namespace Spec

def repoRoot : IO FilePath := do
  let cwd ← IO.currentDir
  if ← (cwd / "corpora").isDir then return cwd
  if let some parent := cwd.parent then
    if ← (parent / "corpora").isDir then return parent
  throw <| IO.userError "Run lake -d spec test from the sapiens4 checkout"

def binary (root : FilePath) (name : String) : FilePath :=
  root / ".build" / "bin" / (name ++ (if Platform.isWindows then ".exe" else ""))

def untilSome (description : String) (observe : IO (Option α)) (timeoutMs := 20000)
    (pollMs : UInt32 := 75) : IO α := do
  let deadline := (← IO.monoMsNow) + timeoutMs
  repeat
    if let some value ← observe then return value
    if (← IO.monoMsNow) ≥ deadline then throw <| IO.userError s!"Timed out waiting for {description}"
    IO.sleep pollMs
  throw <| IO.userError description

structure Host where
  url : String

def fixtureEnvironment (directory : FilePath) : IO (Array (String × Option String)) := do
  let root ← repoRoot
  let provider := root / "spec/.lake/build/bin" / (if Platform.isWindows then "fixtureProvider.exe" else "fixtureProvider")
  unless ← provider.pathExists do throw <| IO.userError "Missing Lean fixture provider"
  return #[("SAPIENS_TEST_PROMPTS", (directory / "fixture-prompts").toString), ("SAPIENS_CODEX_BINARY", provider.toString),
    ("SAPIENS_REPOSITORY", (root / ".build/spec").toString), ("SAPIENS_WHISPER_URL", "http://127.0.0.1:9")]

def withHostMode (crash : Bool) (directory : FilePath) (use : Host → IO α) : IO α := do
  let root ← repoRoot
  let child ← IO.Process.spawn {
    cmd := (binary root "sapiens4").toString
    args := #["--desktop", "--port", "0", "--data-dir", directory.toString]
    cwd := root, env := ← fixtureEnvironment directory, stdin := .piped, stdout := .piped }
  try
    let greeting ← IO.asTask child.stdout.getLine .dedicated
    let line ← untilSome "host startup" do
      if ← IO.hasFinished greeting then return some (← IO.ofExcept greeting.get)
      return none
    let some address := line.trimAscii.toString.dropPrefix? "Sapiens4: http://127.0.0.1:"
      | throw <| IO.userError "Host did not publish its loopback address"
    let some port := (address.toString.splitOn "/").head!.toNat?
      | throw <| IO.userError "Invalid host port"
    let result ← use ⟨s!"http://127.0.0.1:{port}"⟩
    if crash then child.kill else
      child.stdin.putStrLn "stop"
      child.stdin.flush
    let status ← untilSome "clean host shutdown" child.tryWait
    IO.ofExcept <| requireThat "host shutdown failed" (crash || status == 0)
    return result
  catch error =>
    try
      if (← child.tryWait).isNone then
        child.kill
        discard child.wait
    catch _ => pure ()
    throw error

def withHost (directory : FilePath) (use : Host → IO α) : IO α := withHostMode false directory use
def withCrashedHost (directory : FilePath) (use : Host → IO α) : IO α := withHostMode true directory use

def request (url method path : String) (payload : Option Json := none)
    (headers : Array String := #["Content-Type: application/json", "X-Sapiens-Local: 1"]) : IO Reply := do
  let args := #["--disable", "--noproxy", "*", "--silent", "--show-error", "--max-time", "12",
    "--request", method, "--write-out", "\n%{http_code}"] ++ headers.flatMap (fun h => #["--header", h])
  let args := args ++ (if payload.isSome then #["--data-binary", "@-"] else #[]) ++ #[url ++ path]
  let output ← IO.Process.output {cmd := "curl", args} (payload.map Json.compress)
  unless output.exitCode == 0 do throw <| IO.userError s!"{method} {path}: {output.stderr}"
  let lines := output.stdout.splitOn "\n"
  let some status := lines.getLast!.toNat? | throw <| IO.userError "Missing HTTP status"
  return ⟨status, String.intercalate "\n" lines.dropLast⟩

def Host.api (host : Host) (method path : String) (body : Option Json := none)
    (expected : Nat := 200) : IO Json := do
  let reply ← request host.url method path body
  unless reply.status == expected do
    throw <| IO.userError s!"{method} {path}: expected {expected}, received {reply.status}"
  IO.ofExcept <| Json.parse reply.body

def Host.state (host : Host) := host.api "GET" "/api/state"

def Host.chief (host : Host) : IO String := do
  IO.ofExcept <| stringField (← host.state) "main_agent_id"

def Host.agent (host : Host) (name : String) : IO String := do
  let result ← host.api "POST" "/api/agents" (json% {name: $(name), role: "Research"}) 201
  IO.ofExcept <| stringField result "id"

def Host.control (host : Host) (owner : String) (command : Json) (expected := 200) :=
  host.api "POST" ("/api/agents/" ++ owner ++ "/control") command expected

def Host.submit (host : Host) (owner message : String) : IO Json := do
  let submitted ← host.api "POST" ("/api/agents/" ++ owner ++ "/messages") (json% {text: $(message)}) 202
  IO.ofExcept <| submitted.getObjVal? "id"

def Host.awaitTurn (host : Host) (turnId : Json) (expected : String) : IO Json := do
  untilSome s!"turn {turnId} to become {expected}" do
    let turns ← IO.ofExcept <| (← host.state).getObjVal? "turns"
    let turn ← IO.ofExcept <| uniqueRecord turns "id" turnId
    let status ← IO.ofExcept <| stringField turn "status"
    if status == expected then return some turn
    if ["failed", "interrupted", "cancelled", "done"].contains status then
      throw <| IO.userError s!"Turn reached {status} while waiting for {expected}"
    return none

def Host.completedMessage (host : Host) (owner message : String) : IO Json := do
  host.awaitTurn (← host.submit owner message) "done"

end Spec
