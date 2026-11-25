-- Remove application lifecycle noise from the user-facing event history.
-- The enclosing migration transaction also commits the version marker.
module V00_02_01 (sourceVersion, targetVersion, corpora, relay) where
import qualified Append4 as A
import Control.Monad (forM_, when)
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as K
import qualified Data.Vector as V
import MigrationVersion (AppVersion(..))

sourceVersion, targetVersion :: AppVersion
sourceVersion = AppVersion "v00.02.00"
targetVersion = AppVersion "v00.02.01"

corpora :: A.Store -> IO ()
corpora store = do
 rows <- A.values store "events/"
 forM_ rows $ \(key,value) -> case value of
  Array events -> do
   let kept = V.filter (not . lifecycle) events
   when (kept /= events) $ A.putValue store key (Array kept)
  _ -> fail "Invalid event history; lifecycle cleanup was not applied"
 where
 lifecycle (Object row) = K.lookup "actor" row == Just (String "system")
  && K.lookup "entity" row == Just (String "corpora")
  && K.lookup "event" row `elem` map (Just . String) ["system.started", "system.stopped"]
 lifecycle _ = False

-- Relay data has no activity feed; explicitly advance its compatibility marker.
relay :: A.Store -> IO ()
relay _ = pure ()
