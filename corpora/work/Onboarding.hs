{-# LANGUAGE CPP #-}
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
import System.FilePath
import System.Info
import System.IO
import System.Process hiding (runProcess)
import System.Timeout
#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif

-- Owned by the outer local host only; remote corpus routes cannot reach login.
data Onboarding = Onboarding FilePath (MVar (Bool,Maybe (Async ()))) (IORef Value) (IORef T.Text) (MVar (Bool,[Async T.Text])) (FilePath,MVar Value)

openOnboarding :: FilePath -> IO Onboarding
openOnboarding root = do
 home <- P.codexHome
 let path=takeDirectory home</>"onboarding.json"
     initial=obj [("schema",num (1::Int)),("welcomed",Bool False),("completed",Bool False)]
 saved <- readJSON path initial `catch` (\(_::IOException) -> pure initial)
 state <- newMVar (if int (get saved "schema")==1 then saved else initial)
 Onboarding root <$> newMVar (False,Nothing) <*> newIORef (loginState "idle" "") <*> newIORef "not_requested" <*> newMVar (False,[]) <*> pure (path,state)

setupState :: Onboarding -> IO Value
setupState (Onboarding _ _ _ _ _ (_,state)) = readMVar state

saveSetup :: Onboarding -> (Value -> Value) -> IO ()
saveSetup (Onboarding _ _ _ _ _ (path,state)) update = modifyMVar_ state $ \current -> do
 let next=update current
 writeJSON path next `catch` (\(_::IOException) -> failure 503 "Could not save setup. Check that Sapiens can write to its configuration folder, then retry.")
 pure next

loginState :: T.Text -> T.Text -> Value
loginState state code = obj [("state",str state),("code",str code),("url",if T.null code then Null else str "https://auth.openai.com/codex/device")]

closeOnboarding :: Onboarding -> IO ()
closeOnboarding (Onboarding _ worker _ _ checks _) = do
 modifyMVar_ checks $ \(_,active) -> mapM_ cancel active >> pure (True,[])
 modifyMVar_ worker $ \(_,active) -> mapM_ cancel active >> pure (True,Nothing)

-- HTTP handlers may be abandoned when Warp stops; their subprocesses belong to
-- onboarding, which cancels and joins them before the host can exit.
checkedProviderStatus :: Onboarding -> IO T.Text
checkedProviderStatus (Onboarding _ _ _ _ checks _) = bracket acquire release wait
 where
 acquire = modifyMVar checks $ \(closed,active) -> do
  require (not closed) 503 "Sapiens is shutting down."
  task <- async providerStatus
  pure ((False,task:active),task)
 release task = do
  cancel task
  modifyMVar_ checks $ \(closed,active) -> pure (closed,filter ((/=asyncThreadId task) . asyncThreadId) active)

-- Capture the owned group before waiting can reap the parent. On Windows,
-- process jobs retain descendants until waitForProcess observes the whole job.
withOwnedProcess :: CreateProcess -> ((Maybe Handle,Maybe Handle,Maybe Handle,ProcessHandle) -> IO a) -> IO a
withOwnedProcess command run = bracket acquire release (run . fst)
 where
 acquire = do
  handles@(_,_,_,process) <- createProcess command {create_group=True,use_process_jobs=True,detach_console=os=="mingw32"}
  group <- getPid process
  pure (handles,group)
 release ((input,output,errors,process),group) = do
  P.killTree process
#ifndef mingw32_HOST_OS
  forM_ group $ \pid -> signalProcessGroup sigKILL pid `catch` (\(_::IOException) -> pure ())
#endif
  void (waitForProcess process)
  forM_ [input,output,errors] $ mapM_ (\h -> hClose h `catch` (\(_::IOException) -> pure ()))

-- Exit status is the supported readiness signal. Discard all diagnostics: even
-- login status may print account/API-key information. No model call is made.
providerStatus :: IO T.Text
providerStatus = do
 binary <- P.codexBinary
 case binary of
  Nothing -> pure "missing"
  Just path -> (do
   environment <- P.codexEnvironment
   -- No diagnostic pipes: status output can contain account/key information.
   result <- withOwnedProcess (proc path (P.codexConfig ++ ["login","status"])) {env=Just environment,std_in=NoStream,std_out=NoStream,std_err=NoStream} $ \(_,_,_,process) -> timeout 10000000 (waitForProcess process)
   stored <- maybe (pure False) (doesPathExist . (</>"auth.json")) (lookup "CODEX_HOME" environment)
   pure (case result of Just ExitSuccess -> "signed_in"; Just _ | not stored -> "sign_in_required"; _ -> "unavailable")) `catch` (\(_::IOException) -> pure "unavailable")

onboardingView :: Onboarding -> IO Value
onboardingView onboarding@(Onboarding root _ progress permission _ _) = do
 login <- readIORef progress
 provider <- if at login "state" == "signing_in" then pure "signing_in" else checkedProviderStatus onboarding
 binary <- C.blindlyBinary root
 built <- doesFileExist binary
 computer <- if not built then pure "missing" else if os/="darwin" then pure "not_required" else readIORef permission
 saved <- setupState onboarding
 let welcomed=boolean (get saved "welcomed")
     ready=provider=="signed_in"
     complete=welcomed && ready && boolean (get saved "completed")
     step=if not welcomed then "welcome" else if not ready then "sign_in" else if complete then "complete" else "permissions"
 pure (obj [("schema",num (1::Int)),("step",str step),("completed",Bool complete),("provider",str provider),("login",login),("computer_permission",str computer),("computer_available",Bool built),("ready",Bool ready)])

onboardingAction :: Onboarding -> Value -> IO Value
onboardingAction onboarding@(Onboarding root worker progress permission _ _) value = do
 case at value "action" of
  "begin" -> saveSetup onboarding (put "welcomed" (Bool True))
  "finish" -> do
   saved <- setupState onboarding
   require (boolean (get saved "welcomed")) 409 "Start setup from the welcome screen."
   status <- checkedProviderStatus onboarding
   require (status=="signed_in") 409 "Sign in to Codex before entering your workspace."
   saveSetup onboarding (put "completed" (Bool True))
  "login" -> modifyMVar_ worker $ \(closed,active) -> do
   require (not closed) 503 "Sapiens is shutting down."
   running <- maybe (pure False) (fmap isNothing . poll) active
   require (not running) 409 "Sign-in is already running. Complete it or cancel first."
   status <- checkedProviderStatus onboarding
   require (status/="signed_in") 409 "Codex is already signed in."
   require (status=="sign_in_required") 503 "Codex sign-in could not be checked. Recheck setup; your existing sign-in has been preserved."
   saved <- setupState onboarding
   require (boolean (get saved "welcomed")) 409 "Start setup from the welcome screen."
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
deviceLogin binary environment progress = withOwnedProcess
 (proc binary (P.codexConfig ++ ["login","--device-auth"])) {env=Just environment,std_in=NoStream,std_out=CreatePipe,std_err=NoStream}
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
