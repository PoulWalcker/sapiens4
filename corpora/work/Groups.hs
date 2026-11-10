{-# LANGUAGE OverloadedStrings #-}
module Groups (module GroupModel, GroupOperation(..), groupControl) where

import Types
import Data.Aeson (Value)
import Data.Text (Text)
import GroupModel
import qualified GroupChat as Chat
import qualified GroupWork as Work

data GroupOperation = GroupCreate | GroupUpdate | GroupGet | GroupMessage | GroupTaskCreate | GroupTaskUpdate | GroupTaskRun deriving (Eq, Show)
groupControl :: Service -> Text -> GroupOperation -> Value -> IO Value
groupControl s actor operation input = case operation of
 GroupCreate -> createGroup s payload actor
 GroupUpdate -> updateGroup s gid payload actor
 GroupGet -> withGroup False pure
 GroupMessage -> withGroup True (const (Chat.submitGroup s gid payload actor))
 GroupTaskCreate -> withGroup True (const (Work.createTask s gid payload actor))
 GroupTaskUpdate -> withGroup True (const (Work.updateTask s gid (at payload "task") (remove ["task"] payload) actor))
 GroupTaskRun -> withGroup True $ \_ -> do
  root <- Chat.activeRoot s gid actor
  result <- Work.startTask s gid (at payload "task") (get payload "revision") actor root
  Chat.dispatchGroups s
  pure result
 where
 payload = remove ["op","group"] input
 gid = at input "group"
 withGroup active action = do
  group <- getGroup s gid active
  authorizeGroup s group actor Member
  action group
