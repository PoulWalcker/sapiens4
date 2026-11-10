{-# LANGUAGE OverloadedStrings #-}
module Wire (response) where

import Command (Command(..), interpret)
import Data.Aeson (Value(..), object, (.=))

response :: Bool -> String -> Value
response attached text = case interpret attached text of
    Left message -> object ["error" .= message]
    Right Nothing -> Null
    Right (Just command) -> encodeCommand command

encodeCommand :: Command -> Value
encodeCommand command = case command of
    Create interval execution -> object ["minutes" .= interval, "execution" .= execution]
    Edit ident previous interval execution -> object
        ["routine_id" .= ident, "previous_minutes" .= previous, "minutes" .= interval, "execution" .= execution]
    SetPaused ident paused -> object ["routine_id" .= ident, "paused" .= paused]
