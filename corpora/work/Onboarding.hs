module Onboarding where

import Types
import qualified Provider as P
import qualified Computer as C
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson (Value(..))
import Data.Char (isAsciiUpper, isDigit)
import Data.IORef
import Data.Maybe
import qualified Data.Text as T
import System.Directory
import System.Exit
import System.Info
import System.IO
import System.Process hiding (runProcess)
import System.Timeout

-- Owned by the outer local host only; remote corpus routes cannot reach login.
data Onboarding = Onboarding FilePath (MVar (Bool,Maybe (Async ()))) (IORef Value) (IORef T.Text)

openOnboarding :: FilePath -> IO Onboarding
openOnboarding root = Onboarding root <$> newMVar (False,Nothing) <*> newIORef (loginState "idle" "") <*> newIORef "not_requested"

loginState :: T.Text -> T.Text -> Value
loginState state code = obj [("state",str state),("code",str code),("url",if T.null code then Null else str "https://auth.openai.com/codex/device")]

closeOnboarding :: Onboarding -> IO ()
closeOnboarding (Onboarding _ worker _ _) = modifyMVar_ worker $ \(_,active) -> mapM_ cancel active >> pure (True,Nothing)

-- Exit status is the supported readiness signal. Discard all diagnostics: even
-- login status may print account/API-key information. No model call is made.
providerStatus :: IO T.Text
providerStatus = do
 binary <- P.codexBinary
 case binary of
  Nothing -> pure "missing"
  Just path -> (do
   environment <- P.codexEnvironment
   result <- timeout 10000000 (readCreateProcessWithExitCode (proc path (P.codexConfig ++ ["login","status"])) {env=Just environment,detach_console=os=="mingw32",use_process_jobs=True} "")
   pure (case result of Just (ExitSuccess,_,_) -> "signed_in"; Just _ -> "sign_in_required"; Nothing -> "unavailable")) `catch` (\(_::IOException) -> pure "unavailable")

onboardingView :: Onboarding -> IO Value
onboardingView (Onboarding root _ progress permission) = do
 login <- readIORef progress
 provider <- if at login "state" == "signing_in" then pure "signing_in" else providerStatus
 binary <- C.blindlyBinary root
 built <- doesFileExist binary
 computer <- if not built then pure "missing" else if os/="darwin" then pure "not_required" else readIORef permission
 pure (obj [("schema",num (1::Int)),("provider",str provider),("login",login),("computer_permission",str computer),("computer_available",Bool built),("ready",Bool (provider=="signed_in"))])

onboardingAction :: Onboarding -> Value -> IO Value
onboardingAction onboarding@(Onboarding root worker progress permission) value = do
 case at value "action" of
  "login" -> modifyMVar_ worker $ \(closed,active) -> do
   require (not closed) 503 "Sapiens is shutting down."
   running <- maybe (pure False) (fmap isNothing . poll) active
   require (not running) 409 "Sign-in is already running. Complete it or cancel first."
   status <- providerStatus
   require (status/="signed_in") 409 "Codex is already signed in."
   binary <- P.codexBinary >>= maybe (failure 503 "Codex is missing. Reinstall Sapiens to repair its runtime.") pure
   environment <- P.codexEnvironment
   writeIORef progress (loginState "signing_in" "")
   task <- async ((deviceLogin binary environment progress `catch` (\(errorValue::SomeException) -> case fromException errorValue :: Maybe SomeAsyncException of Just _ -> throwIO errorValue; Nothing -> writeIORef progress (loginState "failed" ""))))
   pure (False,Just task)
  "open-login" -> do
   login <- readIORef progress
   require (at login "state"=="signing_in" && not (T.null (at login "code"))) 409 "Start sign-in first."
   let url="https://auth.openai.com/codex/device"
       (program,args)=if os=="darwin" then ("/usr/bin/open",[url]) else if os=="mingw32" then ("rundll32.exe",["url.dll,FileProtocolHandler",url]) else ("xdg-open",[url])
   (code,_,_) <- C.invoke program args 10
   require (code==ExitSuccess) 503 "Could not open your browser. Open https://auth.openai.com/codex/device manually."
  "cancel" -> modifyMVar_ worker $ \(closed,active) -> do
   mapM_ cancel active
   writeIORef progress (loginState "cancelled" "")
   pure (closed,Nothing)
  "permission" -> do
   require (os=="darwin") 400 "Accessibility approval is only required on macOS."
   binary <- C.blindlyBinary root
   exists <- doesFileExist binary
   require exists 503 "Computer helper is missing. Reinstall Sapiens to repair it."
   (code,out,_) <- C.invoke binary ["request-permission"] 10
   require (code==ExitSuccess) 503 "Could not request Accessibility access. Open System Settings > Privacy & Security > Accessibility, then try again."
   writeIORef permission (if boolean (get (parseJSONText (T.pack out)) "trusted") then "granted" else "denied")
  "refresh" -> pure ()
  _ -> failure 400 "Unknown setup action"
 onboardingView onboarding

deviceLogin :: FilePath -> [(String,String)] -> IORef Value -> IO ()
deviceLogin binary environment progress = bracket
 (createProcess (proc binary (P.codexConfig ++ ["login","--device-auth"])) {env=Just environment,std_in=NoStream,std_out=CreatePipe,std_err=NoStream,create_group=True,use_process_jobs=True,detach_console=os=="mingw32"})
 (\(_,output,_,process) -> do
  P.killTree process
  void (waitForProcess process)
  mapM_ (\h -> hClose h `catch` (\(_::IOException) -> pure ())) output)
 (\(_,output,_,process) -> do
  handle <- maybe (failure 500 "Sign-in output unavailable") pure output
  let consume expecting = do
       ended <- hIsEOF handle
       unless ended $ do
        line <- T.strip . T.pack . stripANSI <$> hGetLine handle
        when (expecting && T.length line>=6 && T.length line<=20 && T.all (\c -> isAsciiUpper c || isDigit c || c=='-') line) $ writeIORef progress (loginState "signing_in" line)
        consume ("Enter this one-time code" `T.isInfixOf` line)
  result <- timeout (16*60*1000000) (consume False >> waitForProcess process)
  writeIORef progress (loginState (case result of Just ExitSuccess -> "complete"; Nothing -> "expired"; _ -> "failed") ""))

stripANSI :: String -> String
stripANSI [] = []
stripANSI ('\ESC':'[':rest) = stripANSI (drop 1 (dropWhile (\c -> c<'@' || c>'~') rest))
stripANSI (c:rest) = c:stripANSI rest
