import io.Host

open Lean System
namespace Spec.User

def retainModelSettingsAcrossRestart : IO Unit := IO.FS.withTempDir fun directory => do
  let defaults := json% {provider: "codex", model: "gpt-6-astra", effort: "medium"}
  let (owner, saved) ← withHost directory fun host => do
    let owner ← host.agent "Reviewer"
    let overrides := Json.mkObj [(owner, json% {provider: "codex", model: "gpt-6-luna", effort: "low"})]
    let saved ← host.api "PUT" "/api/model-settings" (json% {revision: 0, defaults: $(defaults), overrides: $(overrides)})
    for (body, status) in [
      (json% {revision: 0, defaults: $(defaults), overrides: {}}, 409),
      (json% {revision: 1, defaults: {provider: "unknown", model: "unsupported", effort: "medium"}, overrides: {}}, 400),
      (json% {revision: 1, defaults: $(defaults), overrides: {"missing-sapi": $(defaults)}}, 404)] do
      discard <| host.api "PUT" "/api/model-settings" body status
      IO.ofExcept <| requireThat "invalid save changed model settings" ((← host.api "GET" "/api/model-settings") == saved)
    return (owner, saved)
  withHost directory fun host => do
    IO.ofExcept <| requireThat "restart changed model settings" ((← host.api "GET" "/api/model-settings") == saved)
    discard <| host.completedMessage owner "Use my saved model override"
    let inherited ← host.api "PUT" "/api/model-settings" (json% {revision: 1, defaults: $(defaults), overrides: {}})
    IO.ofExcept <| fieldsMatch inherited (json% {defaults: $(defaults), overrides: {}})
    discard <| host.completedMessage owner "Use the corpora default again"

def completeWorkThroughMcp : IO Unit := IO.FS.withTempDir fun directory => do
  withHost directory fun host => do
    let owner ← host.chief
    let corpora ← IO.ofExcept <| stringField ((← host.state).getObjValD "corpora") "id"
    let replies ← mcpSession host.url #[
      json% {jsonrpc: "2.0", id: 1, method: "tools/list", params: {}},
      tool 2 "corpora",
      tool 3 "sapis" (json% {corpora: $(corpora)}),
      tool 4 "send" (json% {corpora: $(corpora), sapi: $(owner), text: "Review release readiness"}),
      tool 5 "chat" (json% {corpora: $(corpora), sapi: $(owner), limit: 1, wait: 30})]
    IO.ofExcept do
      let tools ← (replies[0]!.getObjValD "result").getObjValAs? (Array Json) "tools"
      requireThat "MCP tool catalog changed" ((← tools.mapM (fun item => stringField item "name")) == #["corpora", "sapis", "chat", "send", "lifecycle"])
      discard <| uniqueRecord (← toolBody replies[1]!) "id" (toJson corpora)
      discard <| uniqueRecord (← toolBody replies[2]!) "id" (toJson owner)
      let sent ← toolBody replies[3]!
      let turn ← uniqueRecord (← toolBody replies[4]!) "id" (← sent.getObjVal? "id")
      fieldsMatch turn (json% {status: "done"})
    let lifecycle ← mcpSession host.url #[
      tool 1 "lifecycle" (json% {corpora: $(corpora), action: "pause"}),
      tool 2 "send" (json% {corpora: $(corpora), sapi: $(owner), text: "Refuse while paused"}),
      tool 3 "lifecycle" (json% {corpora: $(corpora), action: "activate"})]
    IO.ofExcept do
      fieldsMatch (← toolBody lifecycle[0]!) (json% {status: "paused"})
      fieldsMatch (lifecycle[1]!.getObjValD "result") (json% {isError: true})
      fieldsMatch (← toolBody lifecycle[2]!) (json% {status: "active"})

end Spec.User
