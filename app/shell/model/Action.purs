module Sapiens.Action where

import Prelude (class Eq)

import Sapiens.Form as F
import Sapiens.Browser as Browser
import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe)
import Sapiens.RoutineData as RD
import Sapiens.Domain as X
import Sapiens.Model (DialogKind)
import Sapiens.Navigation (Activity, Scope, Pane, Period, RoutineRoute, RoutineRef, RoutineFilter, RunRef)
import Web.Event.Event (Event)

data Action
  = Initialize
  | LoadRegistry
  | Finalize
  | Poll
  | Select String
  | SetScope Scope
  | Navigate Activity
  | DefaultWork
  | SetPeriod Period
  | OpenRoutine RoutineRoute
  | OpenRoutineHistory RoutineRef
  | RoutineBack
  | RoutineHistoryPage Boolean
  | SubmitRoutineRequest
  | BackFromRoutineChat
  | ControlRoutine RD.RoutineControl
  | RefreshRoutine
  | SelectRoutineResult RunRef (Maybe RD.ExactResultRef)
  | SetRoutineFilter RoutineFilter
  | Search String
  | ToggleSearch
  | TogglePane Pane
  | Home
  | Draft String
  | MessageKey Event
  | Send
  | RetryDocument String
  | ToggleExpanded String
  | Clarify X.Workload
  | Menu String
  | CloseMenu
  | OpenDialog DialogKind F.Context
  | CloseDialog
  | Field F.Field String
  | CheckField String Boolean
  | SubmitDialog
  | DialogCommand DialogIntent
  | Copy String String
  | Scroll
  | UserScroll
  | Resize
  | Global Browser.GlobalEvent
  | GlobalEvent String Event
  | DrainNative
  | NativeReport Browser.NativeReport
  | MentionChoice Int
  | MentionCursor
  | RefreshTheme
  | Theme
  | Persist
  | EnterCorpora String
  | Lifecycle X.LifecycleIntent
  | ShowArchived Boolean
  | Mutate String String Json Success
  | AttachmentKind X.AttachmentKind
  | RemoveAttachment String
  | Browser Browser.Command
  | Address String
  | OpenAddress
  | EditAddress
  | OpenFile
  | PrepareDraft String String
  | Feedback
  | Pair
  | ScanQR
  | ScanFrame Int
  | UploadQR
  | StopScan
  | HostPairPoll Int
  | Install
  | Submit Event Action
  | Link Event Action
  | OpenLink String
  | NoteAnchor String

data Success = Refresh | CloseAndRefresh String | SavedPrompt String

data DialogIntent
  = SuggestNames
  | CreateAgent
  | MentionGroup X.Entity
  | ResetRelay
  | InstallPrompt
  | DeleteTask
  | AddRelay
  | BackRelay
  | SelectRelay F.Relay
  | ConfirmRelay
  | EditRelay String
  | ArchiveRelay F.Relay
  | ImportRelay
  | DoneRelay
  | AcceptRelay
  | HostPair HostIntent

data HostIntent = RevokeHost | ReplaceHost | InviteHost | ApproveHost String

derive instance eqHostIntent :: Eq HostIntent
