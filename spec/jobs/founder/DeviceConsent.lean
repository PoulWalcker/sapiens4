import io.Relay
import io.CryptoWindow

open Lean
namespace Spec.Founder

private def requestPairing (authority device : Host) (qr : Json) : IO (Json × Json) := do
  let pairing ← device.api "POST" "/api/host/pair" qr 202
  let pending ← untilSome "host consent prompt" do
    let status ← authority.api "GET" "/api/remote"
    let pending ← IO.ofExcept <| status.getObjVal? "pending"
    return if pending == .null then none else some pending
  let code ← IO.ofExcept <| stringField pending "confirmation_code"
  IO.ofExcept do
    requireThat "confirmation code missing" (!code.isEmpty)
    fieldsMatch pairing (json% {code: $(code)})
  return (pairing, pending)

private def invitationFor (authority : Host) (relay admin : String) : IO Json := do
  let invitation ← authority.api "POST" "/api/remote/pair" (json% {relay: $(relay), token: $(admin)}) 201
  IO.ofExcept <| invitation.getObjVal? "qr"

private def approvePairing (authority device : Host) (pairing peer : Json) : IO Unit := do
  discard <| authority.api "POST" "/api/remote/approve" (json% {peer: $(peer)})
  let pairId ← IO.ofExcept <| stringField pairing "id"
  discard <| untilSome "approved device registration" do
    let state ← device.api "GET" ("/api/host/pair/" ++ pairId)
    return if (← IO.ofExcept <| stringField state "state") == "approved" then some () else none

def approveOneDeviceThenRevokeItsMailbox : IO Unit := withRelay fun relay admin =>
  IO.FS.withTempDir fun authorityDir => IO.FS.withTempDir fun deviceDir => do
    withCryptoHost authorityDir fun authority => do
      let marker := "UnlistedReviewer"
      discard <| authority.agent marker
      let identity ← IO.ofExcept <| stringField (← authority.api "GET" "/api/corpora") "id"
      let qr ← invitationFor authority relay admin
      let mailbox ← IO.ofExcept <| stringField qr "room"
      let token ← IO.ofExcept <| stringField qr "token"
      let secret ← IO.ofExcept <| stringField qr "secret"
      let discoveryPath := "/api/corpora/" ++ (← IO.ofExcept <| stringField qr "public_id")
      let discover ← request relay "GET" discoveryPath
      IO.ofExcept do
        requireThat "discovery should be available" (discover.status == 200)
        let publicInfo ← Json.parse discover.body
        discard <| publicInfo.getObjValAs? Float "expires"
        let keys := (← publicInfo.getObj?).toList.map Prod.fst
        requireThat "unexpected public discovery fields" (keys == ["desktop", "expires", "public_id"])
        fieldsMatch publicInfo (json% {desktop: $(← qr.getObjVal? "desktop"), public_id: $(← qr.getObjVal? "public_id")})
        requireThat "discovery leaked private data" ([marker, admin, token, secret].all (!discover.body.contains ·))
        requireThat "invitation exposed relay administration" (!qr.compress.contains admin)
      let route := "/api/host/corpora/" ++ identity
      withCryptoHost deviceDir fun device => do
        let (pairing, pending) ← requestPairing authority device qr
        let registry := do IO.ofExcept <| (← device.api "GET" "/api/host").getObjVal? "corpora"
        let before ← registry
        let denied ← request device.url "GET" (route ++ "/api/state")
        IO.ofExcept <| verifyDenial "pending device" 404 denied before (← registry) [marker, admin, token, secret]
        let peer ← IO.ofExcept <| pending.getObjVal? "peer"
        let mismatch ← authority.api "POST" "/api/remote/approve" (json% {peer: "different-pending-device"}) 409
        let unchanged ← authority.api "GET" "/api/remote"
        IO.ofExcept do
          fieldsMatch unchanged (json% {paired: false, pending: $(pending)})
          requireThat "denied approval disclosed credentials" ([admin, token, secret].all (!mismatch.compress.contains ·))
        approvePairing authority device pairing peer
        let remoteState ← device.api "GET" (route ++ "/api/state")
        IO.ofExcept do
          discard <| uniqueRecord (← remoteState.getObjVal? "agents") "name" (toJson marker)
          fieldsMatch (← remoteState.getObjVal? "corpora") (json% {id: $(identity)})
        let before ← authority.api "GET" "/api/remote"
        let refused ← request device.url "POST" (route ++ "/api/remote/revoke") (json% {})
        let after ← authority.api "GET" "/api/remote"
        IO.ofExcept do
          fieldsMatch before (json% {paired: true, invitation: null})
          requireThat "paired status disclosed credentials" ([admin, token, secret].all (!before.compress.contains ·))
          verifyDenial "remote cannot administer pairing" 403 refused (← before.getObjVal? "fingerprint")
            (← after.getObjVal? "fingerprint") [marker, admin, token, secret]
        let hidden ← request relay "GET" discoveryPath
        IO.ofExcept <| requireThat "paired device remained discoverable" (hidden.status == 404)
      withCryptoHost deviceDir fun device => do
        let saved ← device.api "GET" (route ++ "/api/corpora")
        IO.ofExcept <| fieldsMatch saved (json% {id: $(identity)})
        let revoked ← authority.api "POST" "/api/remote/revoke" (json% {})
        IO.ofExcept <| fieldsMatch revoked (json% {revoked: true, relay_deleted: true})
        let before ← authority.api "GET" "/api/corpora"
        let denied ← request relay "GET" ("/api/rooms/" ++ mailbox ++ "/messages") none #[s!"Authorization: Bearer {token}"]
        IO.ofExcept <| verifyDenial "revoked mailbox" 401 denied before (← authority.api "GET" "/api/corpora") [marker, admin, token, secret]
    withCryptoHost authorityDir fun authority => do
      IO.ofExcept <| fieldsMatch (← authority.api "GET" "/api/remote") (json% {paired: false, enabled: false, invitation: null})

def withdrawConsentBeforeTheOtherDeviceApproves : IO Unit := withRelay fun relay admin =>
  IO.FS.withTempDir fun hostDir => IO.FS.withTempDir fun deviceDir => do
    withCryptoHost hostDir fun authority => withCryptoHost deviceDir fun device => do
      let qr ← invitationFor authority relay admin
      let (pairing, pending) ← requestPairing authority device qr
      let path := "/api/host/pair/" ++ (← IO.ofExcept <| stringField pairing "id")
      let cancelled ← device.api "POST" path (json% {})
      IO.ofExcept do fieldsMatch cancelled (json% {state: "cancelled", code: $(← pairing.getObjVal? "code")})
      let peer ← IO.ofExcept <| pending.getObjVal? "peer"
      discard <| authority.api "POST" "/api/remote/approve" (json% {peer: $(peer)})
      IO.ofExcept <| requireThat "late approval replaced cancellation" ((← device.api "GET" path) == cancelled)
      IO.ofExcept <| requireThat "repeat cancellation changed the receipt" ((← device.api "POST" path (json% {})) == cancelled)
    withCryptoHost deviceDir fun device => do
      let registered ← IO.ofExcept <| (← device.api "GET" "/api/host").getObjValAs? (Array Json) "corpora"
      let locations ← IO.ofExcept <| registered.mapM (fun corpora => stringField corpora "location")
      IO.ofExcept <| requireThat "cancelled device registered a remote corpora" (locations.toList == ["Local"])

private structure Mailbox where
  relay : String
  path : String
  deviceToken : String

private def Mailbox.entries (box : Mailbox) (token : String) : IO (Array Json) := do
  let reply ← request box.relay "GET" box.path none #[s!"Authorization: Bearer {token}"]
  IO.ofExcept do
    requireThat "fixture mailbox unavailable" (reply.status == 200)
    (← Json.parse reply.body).getObjValAs? (Array Json) "messages"

private def Mailbox.awaitEnvelope (box : Mailbox) (token : String) (privateText : List String) : IO Json :=
  untilSome "one encrypted delivery" do
    let messages ← box.entries token
    if messages.isEmpty then return none
    IO.ofExcept do
      let #[message] := messages | throw "Unexpected additional mailbox delivery"
      let frame ← message.getObjVal? "frame"
      requireThat "public envelope contains additional fields"
        ((← frame.getObj?).toList.map Prod.fst == ["ciphertext", "nonce", "sender"])
      for key in ["ciphertext", "nonce", "sender"] do
        requireThat s!"missing {key}" (!(← stringField frame key).isEmpty)
      requireThat "private content visible to relay" (privateText.all (!message.compress.contains ·))
      return some frame

-- The fixture owns its phone keys. Tests never read the application's saved
-- credentials or depend on their storage representation.
private def Mailbox.acknowledge (box : Mailbox) : IO Unit := do
  for entry in ← box.entries box.deviceToken do
    let id ← IO.ofExcept <| entry.getObjVal? "id"
    let reply ← request box.relay "DELETE" (box.path ++ "/" ++ id.compress) none
      #[s!"Authorization: Bearer {box.deviceToken}"]
    IO.ofExcept <| requireThat "relay refused acknowledgement" (reply.status == 200)

private def fixturePhone (authority : Host) (browser : CryptoWindow) (qr : Json) : IO (Mailbox × Json × String) := do
  let keys ← browser (json% {operation: "keypair"})
  let config := (qr.setObjVal! "private" (keys.getObjValD "secretKey")).setObjVal! "public" (keys.getObjValD "publicKey")
  let box ← IO.ofExcept do
    return Mailbox.mk (← stringField qr "relay")
      ("/api/rooms/" ++ (← stringField qr "room") ++ "/messages") (← stringField qr "token")
  let frame ← browser (json% {operation: "request", config: $(config), body: {kind: "pair", secret: $(qr.getObjValD "secret")}})
  let sent ← request box.relay "POST" box.path frame #["Content-Type: application/json", s!"Authorization: Bearer {box.deviceToken}"]
  IO.ofExcept <| requireThat "relay refused pairing" (sent.status == 200)
  discard <| untilSome "fixture phone consent" do
    let pending := (← authority.api "GET" "/api/remote").getObjValD "pending"
    return if pending.getObjValD "peer" == keys.getObjValD "publicKey" then some () else none
  discard <| authority.api "POST" "/api/remote/approve" (json% {peer: $(keys.getObjValD "publicKey")})
  let accepted ← box.awaitEnvelope box.deviceToken []
  let plain ← browser (json% {operation: "response", config: $(config), frame: $(accepted)})
  IO.ofExcept <| fieldsMatch (plain.getObjValD "result") (json% {paired: true})
  box.acknowledge
  let identity ← IO.ofExcept <| stringField (← authority.api "GET" "/api/corpora") "id"
  return (box,config,identity)

def resumeRemoteWorkWhenBrowserReopens : IO Unit := withRelay fun relay admin =>
  IO.FS.withTempDir fun hostDir => IO.FS.withTempDir fun deviceDir =>
    withHost hostDir fun authority => withHost deviceDir fun device => withCryptoSession device fun browser => do
      let (box, config, identity) ← withCryptoWindow authority do
        fixturePhone authority browser (← invitationFor authority relay admin)
      discard <| untilSome "host browser disconnected" do
        return if (← authority.api "GET" "/api/host/crypto").getObjValD "connected" == toJson false then some () else none
      let route := "/api/agents/" ++ (← authority.chief) ++ "/messages"
      let message := "Wait for the host browser"
      let frame ← browser (json% {operation: "request", config: $(config), body: {
        kind: "request", op: "api", method: "POST", path: $(route), corpora_id: $(identity), data: {text: $(message)}}})
      let queued ← request relay "POST" box.path frame
        #["Content-Type: application/json", s!"Authorization: Bearer {box.deviceToken}"]
      IO.ofExcept <| requireThat "relay refused queued work" (queued.status == 200)
      IO.sleep 600
      let state ← authority.state
      IO.ofExcept <| requireThat "closed browser executed remote work" ((← IO.ofExcept <| state.getObjValAs? (Array Json) "turns").isEmpty)
      IO.ofExcept <| requireThat "closed browser replied to encrypted work" (← box.entries box.deviceToken).isEmpty
      withCryptoWindow authority do
        let response ← box.awaitEnvelope box.deviceToken [message, admin]
        let plain ← browser (json% {operation: "response", config: $(config), frame: $(response)})
        let receipt ← IO.ofExcept <| plain.getObjVal? "result"
        IO.ofExcept <| fieldsMatch receipt (json% {status: 202})
        let turnId ← IO.ofExcept <| (receipt.getObjValD "body").getObjVal? "id"
        let turn ← authority.awaitTurn turnId "done"
        IO.ofExcept <| fieldsMatch turn (json% {input: $(message), output: "The review is ready."})

def processRedeliveredRemoteWorkOnce : IO Unit := withRelay fun relay admin =>
  IO.FS.withTempDir fun hostDir => IO.FS.withTempDir fun deviceDir =>
    withHost deviceDir fun device => withCryptoSession device fun browser => do
      let message := "Private review 9831"
      let (box, config, identity, path) ← withCryptoHost hostDir fun authority => do
        let (box, config, identity) ← fixturePhone authority browser (← invitationFor authority relay admin)
        return (box, config, identity, "/api/agents/" ++ (← authority.chief) ++ "/messages")
      -- A browser request with a five-minute validity window survives fixture restarts.
      let frame ← browser (json% {operation: "request", config: $(config), body: {
        kind: "request", op: "api", method: "POST", path: $(path), corpora_id: $(identity), data: {text: $(message)}}})
      let enqueue := do
        let queued ← request relay "POST" box.path frame
          #["Content-Type: application/json", s!"Authorization: Bearer {box.deviceToken}"]
        IO.ofExcept <| requireThat "relay refused encrypted delivery" (queued.status == 200)
      enqueue
      let (turnId, original) ← withCryptoHost hostDir fun authority => do
        let response ← box.awaitEnvelope box.deviceToken [message, admin]
        let plain ← browser (json% {operation: "response", config: $(config), frame: $(response)})
        let result ← IO.ofExcept <| plain.getObjVal? "result"
        IO.ofExcept <| fieldsMatch result (json% {status: 202})
        let turnId ← IO.ofExcept do (← result.getObjVal? "body").getObjVal? "id"
        let turn ← authority.awaitTurn turnId "done"
        IO.ofExcept <| fieldsMatch turn (json% {input: $(message), output: "The review is ready."})
        return (turnId, response)
      box.acknowledge
      IO.ofExcept <| requireThat "original receipt was not acknowledged" (← box.entries box.deviceToken).isEmpty
      enqueue
      withCryptoHost hostDir fun authority => do
        let replayed ← box.awaitEnvelope box.deviceToken [message, "The review is ready.", admin]
        IO.ofExcept <| requireThat "redelivery did not return the stored encrypted receipt" (replayed == original)
        let turns ← IO.ofExcept <| (← authority.state).getObjValAs? (Array Json) "turns"
        let #[turn] := turns | throw <| IO.userError "Redelivery repeated a remote command"
        IO.ofExcept <| fieldsMatch turn (json% {id: $(turnId), input: $(message), status: "done"})
        IO.ofExcept <| requireThat "redelivery repeated the provider call" ((← (hostDir / "fixture-prompts").readDir).size == 1)

end Spec.Founder
