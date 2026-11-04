module Sapiens.Form where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Sapiens.Domain as X
import Foreign.Object as O
import Sapiens.Data as D

data Field = Inputs | Reason | Minutes | ReadFiles | WriteFiles | Name | Role | Manager | Mode | Description | Lead | Title | Body | Assignee | State | Value | Node | Path | Content | Scope | Message | Connection | Invitation | Code | Pending | Link | Expiration | Qr | Accepted | Archived | ShowLink
derive instance eqField :: Eq Field

type Fields =
  { inputs :: String, reason :: String, minutes :: String, readFiles :: String, writeFiles :: String, name :: String, role :: String, manager :: String, mode :: String, description :: String, lead :: String, title :: String, body :: String, assignee :: String, state :: String, value :: String, node :: String, path :: String, content :: String, scope :: String, message :: String, connection :: String, invitation :: String, code :: String, pending :: String, link :: String, expiration :: String, qr :: String, accepted :: String, archived :: String, showLink :: String, members :: Array String, revision :: Number, scanning :: Boolean, dates :: O.Object String }

decode :: Json -> Fields
decode value =
  { inputs: D.s "inputs" value, reason: D.s "reason" value, minutes: D.s "minutes" value, readFiles: D.s "readFiles" value, writeFiles: D.s "writeFiles" value, name: D.s "name" value, role: D.s "role" value, manager: D.s "manager" value, mode: D.s "mode" value, description: D.s "description" value, lead: D.s "lead" value, title: D.s "title" value, body: D.s "body" value, assignee: D.s "assignee" value, state: D.s "state" value, value: D.s "value" value, node: D.s "node" value, path: D.s "path" value, content: D.s "content" value, scope: D.s "scope" value, message: D.s "message" value, connection: D.s "connection" value, invitation: D.s "invitation" value, code: D.s "code" value, pending: D.s "pending" value, link: D.s "link" value, expiration: D.s "expiration" value, qr: D.s "qr" value, accepted: D.s "accepted" value, archived: D.s "archived" value, showLink: D.s "showLink" value, members:D.strings $ D.get "members" value, revision:D.n "revision" value, scanning:D.b "scanning" value, dates:D.objectStrings $ D.get "dates" value }

name :: Field -> String
name = case _ of
  Inputs -> "inputs"
  Reason -> "reason"
  Minutes -> "minutes"
  ReadFiles -> "readFiles"
  WriteFiles -> "writeFiles"
  Name -> "name"
  Role -> "role"
  Manager -> "manager"
  Mode -> "mode"
  Description -> "description"
  Lead -> "lead"
  Title -> "title"
  Body -> "body"
  Assignee -> "assignee"
  State -> "state"
  Value -> "value"
  Node -> "node"
  Path -> "path"
  Content -> "content"
  Scope -> "scope"
  Message -> "message"
  Connection -> "connection"
  Invitation -> "invitation"
  Code -> "code"
  Pending -> "pending"
  Link -> "link"
  Expiration -> "expiration"
  Qr -> "qr"
  Accepted -> "accepted"
  Archived -> "archived"
  ShowLink -> "showLink"

fieldValue :: Field -> Fields -> String
fieldValue field form = case field of
  Inputs -> form.inputs
  Reason -> form.reason
  Minutes -> form.minutes
  ReadFiles -> form.readFiles
  WriteFiles -> form.writeFiles
  Name -> form.name
  Role -> form.role
  Manager -> form.manager
  Mode -> form.mode
  Description -> form.description
  Lead -> form.lead
  Title -> form.title
  Body -> form.body
  Assignee -> form.assignee
  State -> form.state
  Value -> form.value
  Node -> form.node
  Path -> form.path
  Content -> form.content
  Scope -> form.scope
  Message -> form.message
  Connection -> form.connection
  Invitation -> form.invitation
  Code -> form.code
  Pending -> form.pending
  Link -> form.link
  Expiration -> form.expiration
  Qr -> form.qr
  Accepted -> form.accepted
  Archived -> form.archived
  ShowLink -> form.showLink

update :: Field -> String -> Fields -> Fields
update field text form = case field of
  Inputs -> form {inputs=text}
  Reason -> form {reason=text}
  Minutes -> form {minutes=text}
  ReadFiles -> form {readFiles=text}
  WriteFiles -> form {writeFiles=text}
  Name -> form {name=text}
  Role -> form {role=text}
  Manager -> form {manager=text}
  Mode -> form {mode=text}
  Description -> form {description=text}
  Lead -> form {lead=text}
  Title -> form {title=text}
  Body -> form {body=text}
  Assignee -> form {assignee=text}
  State -> form {state=text}
  Value -> form {value=text}
  Node -> form {node=text}
  Path -> form {path=text}
  Content -> form {content=text}
  Scope -> form {scope=text}
  Message -> form {message=text}
  Connection -> form {connection=text}
  Invitation -> form {invitation=text}
  Code -> form {code=text}
  Pending -> form {pending=text}
  Link -> form {link=text}
  Expiration -> form {expiration=text}
  Qr -> form {qr=text}
  Accepted -> form {accepted=text}
  Archived -> form {archived=text}
  ShowLink -> form {showLink=text}

member :: String -> Boolean -> Fields -> Fields
member id checked form = form {members=if checked then A.nub $ A.snoc form.members id else A.filter (_ /= id) form.members}

type Prompt = {node :: String, path :: String, content :: String}
type Relay = {id :: String, name :: String, address :: String, archived :: Boolean, available :: Boolean, beta :: Boolean}
type Relays = {servers :: Array Relay, selected :: String, revision :: Number, beta_accepted :: Boolean}
type Device = {name :: String, platform :: String, client :: String}
type Confirmation = {confirmation_code :: String, peer :: String}
type PairStatus = {available :: Boolean, paired :: Boolean, enabled :: Boolean, invitation :: X.Invitation, pending :: Maybe Confirmation, device :: Device, paired_at :: Number, last_connected_at :: Number, error :: String}

data Content = NoContent | Prompts (Array Prompt) | RelayRegistry Relays | Pairing PairStatus
derive instance eqContent :: Eq Content

prompt :: Json -> Prompt
prompt row = {node:D.s "node" row,path:D.s "path" row,content:D.s "content" row}

promptFields :: Prompt -> Fields -> Fields
promptFields p fields = fields {node=p.node,path=p.path,content=p.content}

prompts :: Content -> Array Prompt
prompts = case _ of
  Prompts rows -> rows
  _ -> []

relay :: Json -> Relay
relay row = {id:D.s "id" row,name:D.s "name" row,address:D.s "address" row,archived:D.b "archived" row,available:D.b "available" row,beta:D.b "beta" row}

relayRegistry :: Json -> Content
relayRegistry value = RelayRegistry {servers:map relay $ D.rows "servers" value,selected:D.s "selected" value,revision:D.n "revision" value,beta_accepted:D.b "beta_accepted" value}

relays :: Content -> Relays
relays = case _ of
  RelayRegistry value -> value
  _ -> {servers:[],selected:"",revision:0.0,beta_accepted:false}

pairStatus :: Json -> PairStatus
pairStatus value =
  {available:not (D.valid (D.get "available" value)) || D.b "available" value
  ,paired:D.b "paired" value,enabled:D.b "enabled" value,invitation:X.invitation $ D.get "invitation" value
  ,pending:if D.valid $ D.get "pending" value then Just {confirmation_code:D.str $ D.at ["pending","confirmation_code"] value,peer:D.str $ D.at ["pending","peer"] value} else Nothing
  ,device:{name:D.str $ D.at ["device_info","name"] value,platform:D.str $ D.at ["device_info","platform"] value,client:D.str $ D.at ["device_info","client"] value}
  ,paired_at:D.n "paired_at" value,last_connected_at:D.n "last_connected_at" value,error:D.s "error" value}

data PairView = Unavailable | Paired Device | AwaitingApproval Confirmation | Invited X.Invitation | Unpaired

pairView :: PairStatus -> PairView
pairView value
  | not value.available = Unavailable
  | value.paired = Paired value.device
  | otherwise = case value.pending of
      Just pending -> AwaitingApproval pending
      Nothing -> if value.enabled then Invited value.invitation else Unpaired

savedPrompt :: String -> String -> String -> Content -> Content
savedPrompt node path text = case _ of
  Prompts rows -> Prompts $ map (\p -> if p.node == node then p {content=text,path=path} else p) rows
  other -> other

-- Relay connection documents and mutations are opaque wire payloads. UI state
-- carries their typed identity/context, while the host validates their schema.
data Context
  = RoutineRecord Json
  | NoRecord
  | EntityRecord X.Entity
  | TaskRecord X.GroupTask
  | CorporaRecord X.Corpora Boolean
  | RelayRecord {id :: String, name :: String, connection :: Json}
  | ChangeRecord {change :: Json, saving :: Boolean, from :: String, to :: String}
  | NoticeRecord String

derive instance eqContext :: Eq Context

contextId :: Context -> String
contextId = case _ of
  EntityRecord value -> value.id
  TaskRecord value -> value.id
  CorporaRecord value _ -> value.id
  RelayRecord value -> value.id
  _ -> ""

contextName :: Context -> String
contextName = case _ of
  EntityRecord value -> value.name
  CorporaRecord value _ -> value.name
  RelayRecord value -> value.name
  _ -> ""

contextCorpora :: Context -> X.Corpora
contextCorpora = case _ of
  CorporaRecord value _ -> value
  _ -> X.corpora D.empty

contextRevision :: Context -> Number
contextRevision = case _ of
  EntityRecord value -> value.revision
  TaskRecord value -> value.revision
  CorporaRecord value _ -> value.revision
  _ -> 0.0

contextArchived :: Context -> Boolean
contextArchived = case _ of
  EntityRecord value -> value.archived
  _ -> false

contextDeleted :: Context -> Boolean
contextDeleted = case _ of
  TaskRecord value -> value.deleted
  _ -> false

contextReady :: Context -> Boolean
contextReady = case _ of
  CorporaRecord _ ready -> ready
  _ -> false

readyContext :: Context -> Context
readyContext = case _ of
  CorporaRecord value _ -> CorporaRecord value true
  other -> other

contextConnection :: Context -> Json
contextConnection = case _ of
  RelayRecord value -> value.connection
  _ -> D.null

contextChange :: Context -> {change :: Json, saving :: Boolean, from :: String, to :: String}
contextChange = case _ of
  ChangeRecord value -> value
  _ -> {change:D.empty,saving:false,from:"",to:""}

relayContext :: Json -> Context
relayContext value = RelayRecord {id:D.s "id" value,name:D.s "name" value,connection:D.get "connection" value}

fieldsFor :: Context -> Fields
fieldsFor context = case context of
  EntityRecord value -> empty {name=value.name,role=value.role,description=value.description,lead=value.lead,members=value.members,revision=value.revision}
  TaskRecord value -> empty {title=value.title,body=value.body,assignee=value.assignee,state=X.groupStateName value.state}
  RelayRecord value -> empty {name=value.name,connection=if D.valid value.connection then D.encode value.connection else ""}
  NoticeRecord message -> empty {message=message}
  _ -> empty
  where
  empty = decode D.empty

-- Routine payloads remain opaque host-validated records at this boundary.
routineRecord :: Context -> Json
routineRecord = case _ of
  RoutineRecord value -> value
  _ -> D.empty
