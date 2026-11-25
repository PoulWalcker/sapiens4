import io.Host

open Lean
namespace Spec.SystemJobs

def eraseCancelledVoiceDraftsAcrossRestart : IO Unit := IO.FS.withTempDir fun directory => do
  let recording := "a82a45dc-8885-4965-82fe-d1ad007a9c74"
  let silentSecond := "UklGRiR9AABXQVZFZm10IBAAAAABAAEAgD4AAAB9AAACABAAZGF0YQB9" ++
    String.ofList (List.replicate 42670 'A') ++ "=="
  let piece := fun (index : Nat) => json% {recording: $(recording), index: $(index), final: true, data: $(silentSecond)}
  -- Persist an unfinished upload, then finish it after restarting the host.
  withHost directory fun host => do
    let owner ← host.chief
    let saved ← host.api "POST" ("/api/agents/" ++ owner ++ "/voice")
      (json% {recording: $(recording), index: 0, final: false, data: $(silentSecond)})
    IO.ofExcept <| fieldsMatch saved (json% {messages: []})
  let (owner, clip, file) ← withHost directory fun host => do
    let owner ← host.chief
    let path := "/api/agents/" ++ owner ++ "/voice"
    let saved ← host.api "POST" path (piece 1)
    let retried ← host.api "POST" path (piece 1)
    IO.ofExcept <| fieldsMatch retried (json% {duplicate: true})
    let messages ← IO.ofExcept <| saved.getObjValAs? (Array Json) "messages"
    let #[message] := messages | throw <| IO.userError "Expected a single voice message"
    let clip ← IO.ofExcept <| stringField message "id"
    let file := System.FilePath.mk (← IO.ofExcept <| stringField message "value")
    IO.ofExcept <| requireThat "voice data missing before cancellation" (← file.pathExists)
    let drafts := Json.mkObj [(owner, toJson [clip])]
    discard <| host.api "PUT" "/api/preferences" (json% {attachment_drafts: $(drafts)})
    let savedDrafts ← IO.ofExcept <| (← host.state).getObjVal? "attachment_drafts"
    let entries ← IO.ofExcept <| savedDrafts.getObjVal? owner
    discard <| IO.ofExcept <| uniqueRecord entries "id" (toJson clip)
    let cancelled ← host.api "POST" (path ++ "/" ++ recording ++ "/cancel") (json% {})
    IO.ofExcept <| fieldsMatch cancelled (json% {cancelled: [$(clip)]})
    return (owner, clip, file)
  withHost directory fun host => do
    let path := "/api/agents/" ++ owner
    discard <| host.api "POST" (path ++ "/voice") (piece 1) 409
    discard <| host.api "POST" (path ++ "/messages") (json% {text: "", attachments: [$(clip)]}) 409
    discard <| host.api "PUT" "/api/preferences"
      (json% {attachment_drafts: $(Json.mkObj [(owner, toJson [clip])])})
    let state ← host.state
    let existsNow ← file.pathExists
    IO.ofExcept do
      requireThat "cancelled audio remains on disk" (!existsNow)
      let drafts ← state.getObjVal? "attachment_drafts"
      requireThat "a stale device restored the draft" (!drafts.compress.contains clip)
      fieldsMatch state (json% {turns: []})

end Spec.SystemJobs
