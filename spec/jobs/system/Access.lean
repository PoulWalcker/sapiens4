import io.Host

open Lean
namespace Spec.SystemJobs

def rejectUntrustedRequestsWithoutDisclosingLocalState : IO Unit :=
  IO.FS.withTempDir fun directory => withHost directory fun host => do
    let marker := "PrivateRosterMarker"
    discard <| host.agent marker
    let roster := do IO.ofExcept <| (← host.state).getObjVal? "agents"
    let before ← roster
    IO.ofExcept <| requireThat "trusted caller must see the fixture" (before.compress.contains marker)
    for write in LocalWrite.cases do
      let before ← roster
      let name := if write.origin == .absent then "NativeWriter" else "BrowserWriter"
      let reply ← request host.url "POST" "/api/agents" (json% {name: $(name), role: "Research"}) (write.headers host.url)
      let after ← roster
      if write.allowed then
        IO.ofExcept do
          requireThat "trusted write failed" (reply.status == 201)
          let created ← Json.parse reply.body
          let member ← uniqueRecord after "id" (← created.getObjVal? "id")
          fieldsMatch member (json% {name: $(name)})
      else
        IO.ofExcept <| verifyDenial (reprStr write) 403 reply before after [marker]
    let before ← roster
    for (name, path, headers, status) in [
      ("opaque origin", "/api/state", #["Origin: null"], 403),
      ("unrecognized host", "/api/state", #["Host: other.invalid"], 403),
      ("private database", "/corpora.lmdb", #[], 404)] do
      let reply ← request host.url "GET" path none headers
      IO.ofExcept <| verifyDenial name status reply before (← roster) [marker]

def keepFilePreviewsAndAttachmentsWithTheirOwner : IO Unit := IO.FS.withTempDir fun directory => do
  let marker := "Private finance review 417"
  let (owner, colleague, tabId, attachmentId) ← withHost directory fun host => do
    let owner ← host.chief
    let colleague ← host.agent "Colleague"
    let status ← host.control owner (json% {op: "status"})
    let workspace ← IO.ofExcept do stringField (← status.getObjVal? "notes") "workspace"
    let file := System.FilePath.mk workspace / "finance.txt"
    IO.FS.writeFile file marker
    let opened ← host.control owner (json% {op: "workspace_open", path: $(file.toString)})
    let tabId ← IO.ofExcept do
      let tabs ← (← opened.getObjVal? "workspace").getObjValAs? (Array Json) "tabs"
      let #[tab] := tabs | throw "expected one open finance tab"
      stringField tab "id"
    let upload ← host.api "POST" ("/api/agents/" ++ owner ++ "/attachments")
      (json% {kind: "document", name: "finance.txt", data: "UHJpdmF0ZSBmaW5hbmNlIHJldmlldyA0MTc="}) 201
    let stored ← IO.ofExcept <| stringField upload "value"
    IO.ofExcept <| requireThat "uploaded document changed" ((← IO.FS.readFile stored) == marker)
    return (owner, colleague, tabId, ← IO.ofExcept <| stringField upload "id")
  withHost directory fun host => do
    let preview := fun agent => "/api/agents/" ++ agent ++ "/browser/" ++ tabId ++ "/content"
    let owned ← host.api "GET" (preview owner)
    IO.ofExcept <| fieldsMatch owned (json% {content: $(marker)})
    let turns := do IO.ofExcept <| (← host.state).getObjVal? "turns"
    let before ← turns
    let denied ← request host.url "GET" (preview colleague)
    IO.ofExcept <| verifyDenial "another agent's file" 404 denied before (← turns) [marker]
    let denied ← request host.url "POST" ("/api/agents/" ++ colleague ++ "/messages")
      (json% {text: "Review document", attachments: [$(attachmentId)]})
    IO.ofExcept <| verifyDenial "another agent's attachment" 400 denied before (← turns) [marker]
    discard <| host.api "POST" ("/api/agents/" ++ owner ++ "/messages")
      (json% {text: "Review my document", attachments: [$(attachmentId)]}) 202
    IO.ofExcept <| fieldsMatch (← host.api "GET" (preview owner)) (json% {content: $(marker)})

def keepLocalCorporaSeparateAcrossRestart : IO Unit := IO.FS.withTempDir fun directory => do
  let marker := "Private launch notes for the second corpora"
  let (route, childChief) ← withHost directory fun host => do
    let created ← host.api "POST" "/api/host/local" (json% {name: "Studio", scope: "Release planning"}) 201
    let identity ← IO.ofExcept <| stringField created "id"
    let route := "/api/host/corpora/" ++ identity
    let child ← host.api "GET" (route ++ "/api/state")
    let childChief ← IO.ofExcept <| stringField child "main_agent_id"
    let originalChief ← host.chief
    IO.ofExcept do
      requireThat "separate corpora share the same chief" (originalChief != childChief)
      fieldsMatch (← child.getObjVal? "corpora") (json% {id: $(identity), status: "paused"})
    discard <| host.api "PUT" (route ++ "/api/preferences")
      (json% {drafts: $(Json.mkObj [(childChief, toJson marker)])})
    return (route, childChief)
  withHost directory fun host => do
    let original ← host.state
    let child ← host.api "GET" (route ++ "/api/state")
    IO.ofExcept do
      fieldsMatch (← (← child.getObjVal? "preferences").getObjVal? "drafts") (Json.mkObj [(childChief, toJson marker)])
      requireThat "private draft leaked into the original corpora" (!original.compress.contains marker)
      fieldsMatch (← original.getObjVal? "corpora") (json% {status: "active"})
      fieldsMatch (← child.getObjVal? "corpora") (json% {status: "paused"})

end Spec.SystemJobs
