module Onboarding where

import Types
import qualified Computer as C
import RuntimePaths (configDirectory)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Aeson (FromJSON(..), Value(..), withObject, (.:), eitherDecodeStrict)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as B
import qualified Data.Text as T
import System.Directory
import System.Exit
import System.FilePath
import System.Info
import System.IO.Error (isDoesNotExistError)

-- Setup is a device-local UI decision, independent of Corpora and AI execution.
data SetupStep = ComputerAccess | Ready | Complete deriving (Eq)

stepName :: SetupStep -> T.Text
stepName ComputerAccess = "permissions"
stepName Ready = "ready"
stepName Complete = "complete"

instance FromJSON SetupStep where
 parseJSON = withObject "setup" $ \o -> do
  schema <- o .: "schema" :: Parser Int
  unless (schema==2) (fail "Unsupported setup schema")
  name <- o .: "step"
  -- Retain decisions made by the earlier D3 flow; discarded choices are ignored.
  case name :: T.Text of
   "provider" -> pure ComputerAccess
   "permissions" -> pure ComputerAccess
   "corpora" -> pure Ready
   "ready" -> pure Ready
   "complete" -> pure Complete
   _ -> fail "Invalid setup step"

data Onboarding = Onboarding
 { onboardingRoot :: FilePath, setupPath :: FilePath
 , setupSaved :: MVar (Bool,SetupStep) }

openOnboarding :: FilePath -> IO Onboarding
openOnboarding root = do
 path <- (</> "onboarding.json") <$> configDirectory
 saved <- (Just <$> B.readFile path) `catch` (\(e::IOException) -> if isDoesNotExistError e then pure Nothing else invalid)
 step <- maybe (pure ComputerAccess) (either (const invalid) pure . eitherDecodeStrict) saved
 Onboarding root path <$> newMVar (False,step)
 where
 invalid :: IO a
 invalid = failure 503 "Could not read onboarding.json. Restore valid setup settings from backup and restart Sapiens; saved decisions have been preserved."

-- Permission subprocesses are synchronous and bounded. Closing waits for any
-- in-flight request before the host releases its resources.
closeOnboarding :: Onboarding -> IO ()
closeOnboarding setup = modifyMVar_ (setupSaved setup) $ \(_,step) -> pure (True,step)

onboardingView :: Onboarding -> IO Value
onboardingView setup = withMVar (setupSaved setup) $ \(closed,step) -> do
 require (not closed) 503 "Sapiens is shutting down."
 view setup step

view :: Onboarding -> SetupStep -> IO Value
view setup step = do
 binary <- C.blindlyBinary (onboardingRoot setup)
 built <- doesFileExist binary
 permission <- if os/="darwin" then pure "not_required" else if not built then pure "missing" else do
  -- This PID cannot be foreground; the helper only checks AX trust.
  result <- (Just <$> C.invoke binary ["focused","--pid","2147483647"] 3) `catches`
   [Handler (\(_::IOException) -> pure Nothing),Handler (\(_::APIError) -> pure Nothing)]
  pure $ case result of
   Just (ExitFailure 77,out,_) -> case at (parseJSONText (T.pack out)) "code" of
    "focus_unavailable" -> "granted"
    "accessibility_permission_denied" -> "denied"
    _ -> "unavailable"
   _ -> "unavailable"
 pure (obj [("step",str (stepName step)),("completed",Bool (step==Complete)),("computer_permission",str permission),("computer_available",Bool built)])

onboardingAction :: Onboarding -> Value -> IO Value
onboardingAction setup input = modifyMVar (setupSaved setup) $ \(closed,current) -> do
 require (not closed) 503 "Sapiens is shutting down."
 next <- case at input "action" of
  "permissions-done" -> pure (if current==ComputerAccess then Ready else current)
  "finish" -> require (current/=ComputerAccess) 409 "Continue from Computer access first." >> pure Complete
  "back" -> require (current==Ready) 409 "Setup cannot move back from this step." >> pure ComputerAccess
  "permission" -> do
   require (os=="darwin") 400 "Accessibility approval is only required on macOS."
   binary <- C.blindlyBinary (onboardingRoot setup)
   built <- doesFileExist binary
   require built 503 "Computer helper is missing. Rebuild or reinstall Sapiens to repair it."
   (code,_,_) <- C.invoke binary ["request-permission"] 10
   require (code==ExitSuccess) 503 "Could not request Accessibility access. Open System Settings > Privacy & Security > Accessibility, then try again."
   pure current
  "refresh" -> pure current
  _ -> failure 400 "Unknown setup action"
 when (next/=current || at input "action"=="permissions-done" || at input "action"=="finish") $
  writeJSON (setupPath setup) (obj [("schema",num (2::Int)),("step",str (stepName next))])
   `catch` (\(_::IOException) -> failure 503 "Could not save setup. Check that Sapiens can write to its configuration folder, then retry.")
 value <- view setup next
 pure ((False,next),value)
