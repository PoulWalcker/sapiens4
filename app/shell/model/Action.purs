module Sapiens.Action where

import Sapiens.Model as M
import Data.Argonaut.Core (Json)
import Sapiens.Model (DialogKind)
import Sapiens.Navigation (Activity, Scope, Pane, Period)
import Web.Event.Event (Event)

data Action
  = Initialize
  | CheckSetup
  | LoadRegistry
  | Finalize
  | Poll
  | Select String
  | SetScope Scope
  | Navigate Activity
  | DefaultWork
  | SetPeriod Period
  | Search String
  | ToggleSearch
  | TogglePane Pane
  | Home
  | Draft String
  | MessageKey Event
  | Send
  | RetryDocument String
  | ToggleExpanded String
  | Clarify Json
  | Menu String
  | CloseMenu
  | OpenDialog DialogKind Json
  | CloseDialog
  | Field String String
  | CheckField String String Boolean
  | SubmitDialog
  | DialogCommand String Json
  | Copy String String
  | Scroll
  | UserScroll
  | Resize
  | Global String Json
  | GlobalEvent String Event
  | DrainNative
  | NativeReport Json
  | MentionChoice Int
  | MentionCursor
  | RefreshTheme
  | Theme
  | Persist
  | EnterCorpora String
  | Lifecycle String
  | ShowArchived Boolean
  | Mutate String String Json Success
  | AttachmentKind String
  | RemoveAttachment String
  | Browser String Json
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
  | ProviderNavigate M.ProviderPage
  | OnboardingPoll Int
  | HostPairPoll Int
  | Install
  | Submit Event Action
  | Link Event Action
  | OpenLink String
  | NoteAnchor String

data Success = Refresh | CloseAndRefresh String | SavedPrompt String
