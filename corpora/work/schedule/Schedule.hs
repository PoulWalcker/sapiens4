{-# LANGUAGE OverloadedStrings #-}
module Schedule
 ( Minutes(..), ScheduleRule(..), ruleFromInput, storedRule, ruleFields
 , nextDue, frequency, preview
 ) where

import Control.Monad (unless, when)
import Data.Aeson hiding (pairs)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time hiding (Minutes)
import Data.Time.Zones (TZ, LocalToUTCResult(..), localTimeToUTCFull, utcToLocalTimeTZ)
import Data.Time.Zones.All (tzByName)
import Types

newtype Minutes = Minutes { minuteCount :: Int } deriving (Eq, Show)
instance FromJSON Minutes where
 parseJSON value = do
  n <- parseJSON value
  unless (n>=1 && n<=525600) (fail "Choose 1–525600 minutes")
  pure (Minutes n)
instance ToJSON Minutes where toJSON = toJSON . minuteCount

data ScheduleRule = Interval Minutes | Daily TimeOfDay Text TZ deriving (Eq, Show)

-- New commands reject contradictory fields; stored manual records may retain
-- historical interval fields and are handled by Definition's trigger first.
ruleFromInput :: Value -> Parser (Maybe ScheduleRule)
ruleFromInput value
 | at value "trigger"=="manual" = do
    unless (get value "minutes"==Null && get value "schedule"==Null) (fail "Manual schedules cannot include minutes or a daily rule")
    pure Nothing
 | otherwise = do
    unless (at value "trigger" `elem` ["","interval"]) (fail "Unknown schedule trigger")
    Just <$> storedRule value

storedRule :: Value -> Parser ScheduleRule
storedRule value
 | get value "schedule"/=Null = do
    when (get value "minutes"/=Null) (fail "Choose either minutes or a daily schedule")
    withObject "Daily schedule" (\o -> do
     onlyFields ["kind","time","timezone"] (Object o)
     kind <- o .: "kind" :: Parser Text
     unless (kind=="daily") (fail "Only daily calendar schedules are supported")
     time <- o .: "time"
     unless (T.length time==5 && T.index time 2==':' && T.all (\c->c>='0' && c<='9') (T.take 2 time<>T.drop 3 time)) (fail "Daily time must be HH:MM")
     local <- maybe (fail "Daily time must be HH:MM") pure (parseTimeM True defaultTimeLocale "%H:%M" (T.unpack time))
     zone <- o .: "timezone"
     tz <- maybe (fail "Unknown IANA timezone") pure (tzByName (TE.encodeUtf8 zone))
     unless (not (T.null zone)) (fail "Unknown IANA timezone")
     pure (Daily local zone tz)) (get value "schedule")
 | otherwise = Interval <$> parseJSON (get value "minutes")

ruleFields :: ScheduleRule -> [(Text,Value)]
ruleFields rule = case rule of
 Interval minutes -> [("minutes",toJSON minutes)]
 Daily time zone _ -> [("schedule",obj [("kind",str "daily"),("time",str (T.pack (formatTime defaultTimeLocale "%H:%M" time))),("timezone",str zone)])]

-- Daily follows local calendar days, never 1440 elapsed minutes. Ambiguous
-- times use only the first occurrence; a nonexistent time shifts by the gap.
nextDue :: ScheduleRule -> UTCTime -> Maybe UTCTime -> UTCTime
nextDue rule after previous = case rule of
 Interval (Minutes minutes) ->
  let interval=fromIntegral minutes*60
  in case previous of
   Nothing -> addUTCTime interval after
   Just due -> addUTCTime (interval*fromIntegral (floor (diffUTCTime after due/interval)+1 :: Integer)) due
 Daily time _ tz ->
   let day=localDay (utcToLocalTimeTZ tz after)
       candidate offset = case localTimeToUTCFull tz (LocalTime (addDays offset day) time) of
        LTUUnique due _ -> due
        LTUAmbiguous first _ _ _ -> first
        LTUNone due _ -> due
       choose offset = let due=candidate offset in if due>after then due else choose (offset+1)
   in choose 0

frequency :: ScheduleRule -> Text
frequency rule = case rule of
 Interval (Minutes n) -> "every "<>T.pack (show n)<>" minutes"
 Daily time zone _ -> "daily at "<>T.pack (formatTime defaultTimeLocale "%H:%M" time)<>" "<>zone

preview :: UTCTime -> Value -> IO Value
preview clock input = do
 rule <- parseValue 400 ruleFromInput input
 after <- if get input "after"==Null then pure clock else maybe (failure 400 "Preview after must be an explicit UTC/offset timestamp") pure (parseTime (at input "after"))
 let due=(\selected->nextDue selected after Nothing) <$> rule
 pure (obj [("nextDue",maybe Null (str . timestamp) due),("frequency",maybe (str "on request") (str . frequency) rule)])
