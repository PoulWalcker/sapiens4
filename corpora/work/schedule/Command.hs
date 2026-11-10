{-# LANGUAGE NoOverloadedStrings #-}
module Command (Command(..), interpret) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf)
import Text.ParserCombinators.ReadP

data Command = Create Integer String | Edit String Integer Integer String | SetPaused String Bool
    deriving (Eq, Show)

interpret :: Bool -> String -> Either String (Maybe Command)
interpret attached text = case template text of
    Nothing -> Right Nothing
    Just kind
        | attached -> Left "Put the execution instructions in the routine prompt"
        | otherwise -> case reverse (readP_to_S (templateParser kind <* eof) text) of
            (command,_) : _ -> Right (Just command)
            [] -> Left (templateError kind)

data Template = Creation | Editing | Pausing

template :: String -> Maybe Template
template text
    | "Add New Scheduled Routine:" `isPrefixOf` text = Just Creation
    | any (`isPrefixOf` text) ["Pause Schedule Riutine", "Pause Scheduled Routine", "Resume Scheduled Routine"] = Just Pausing
    | "Edit @routine_sch_" `isPrefixOf` text = Just Editing
    | otherwise = Nothing

templateParser :: Template -> ReadP Command
templateParser Creation = create
templateParser Editing = edit
templateParser Pausing = pause

templateError :: Template -> String
templateError Pausing = "Put the Scheduled Routine ID on the next line"
templateError _ = "Use the Scheduled Routine template with a whole number of minutes and an execution prompt"

space :: Char -> Bool
space c = c `elem` "\t\n\v\f\r\x1c\x1d\x1e\x1f \x85\xa0\x1680\x2028\x2029\x202f\x205f\x3000" || c >= '\x2000' && c <= '\x200a'

nextLine :: ReadP ()
nextLine = do
    whitespace <- munch1 space
    guard (last whitespace == '\n')

number :: ReadP Integer
number = read <$> munch1 (\c -> c >= '0' && c <= '9')

routineId :: ReadP String
routineId = do
    prefix <- string "routine_sch_"
    digits <- munch1 (\c -> c >= '0' && c <= '9')
    guard (length digits >= 5)
    pure (prefix ++ digits)

minutes :: ReadP Integer
minutes = number <* string " minute" <* optional (char 's')

execution :: ReadP String
execution = do
    value <- munch1 (const True)
    pure (dropWhileEnd space (dropWhile space value))

create :: ReadP Command
create = do
    _ <- string "Add New Scheduled Routine:" *> nextLine *> string "Frequency: every "
    interval <- minutes
    _ <- nextLine *> string "Execute: "
    Create interval <$> execution

edit :: ReadP Command
edit = do
    ident <- string "Edit @" *> routineId
    title <- string " ( " *> munch (/= '\n')
    guard (" )" `isSuffixOf` dropWhileEnd space title)
    nextLine
    previous <- string "Edit Frequency (prev every " *> minutes
    interval <- string "): new every " *> minutes
    _ <- nextLine *> string "Edit Execution Prompt as: "
    Edit ident previous interval <$> execution

pause :: ReadP Command
pause = do
    paused <- (True <$ (string "Pause Schedule Riutine" <|> string "Pause Scheduled Routine"))
              <|> (False <$ string "Resume Scheduled Routine")
    ident <- optional (char ':') *> nextLine *> char '@' *> routineId
    pure (SetPaused ident paused)
