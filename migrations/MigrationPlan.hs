module MigrationPlan where
import qualified Append4 as A
import Control.Monad
import Data.Aeson
import MigrationVersion

data Migration = Migration
 { fromVersion :: AppVersion, toVersion :: AppVersion
 , applyMigration :: A.Store -> IO () }

-- Validate the entire route before running any step. Never guess across a gap,
-- branch, cycle or downgrade. Every step and its marker commit together.
plan :: [Migration] -> AppVersion -> AppVersion -> Either String [Migration]
plan steps source target = go [] source where
 go seen version
  | version == target = Right []
  | version > target = Left "Data belongs to a newer app; downgrade refused"
  | version `elem` seen = Left "Migration cycle"
  | otherwise = case filter ((==version) . fromVersion) steps of
     [step] | toVersion step > version && toVersion step <= target ->
       (step:) <$> go (version:seen) (toVersion step)
     _ -> Left "Missing, ambiguous or invalid app migration"

runMigrations :: [Migration] -> AppVersion -> AppVersion -> A.Store -> IO ()
runMigrations steps source target store = do
 route <- either fail pure (plan steps source target)
 forM_ route $ \step -> A.transaction store $ do
  saved <- A.getValue store versionKey
  unless (saved == Just (String (versionText (fromVersion step)))) $
   fail "Migration source version changed"
  applyMigration step store
  A.putValue store versionKey (String (versionText (toVersion step)))
