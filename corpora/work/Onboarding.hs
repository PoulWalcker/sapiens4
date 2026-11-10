{-# LANGUAGE CPP #-}
module Onboarding where

import Types
import qualified Provider as P
import Provider (SetupStep(..), stepName)
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
import qualified Data.UUID as UUID
import System.Directory
import System.Exit
import System.FileLock
import System.FilePath
import System.Info
import System.IO
import System.Process hiding (runProcess)
import System.Timeout
import Network.URI (parseURI,uriScheme,uriAuthority,uriRegName,uriUserInfo,uriPort,uriPath)
import qualified Network.Socket as Socket
#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif

-- Owned by the outer local host only; remote corpus routes cannot reach login.
data Onboarding = Onboarding FilePath (MVar (Bool,Maybe (Async ()))) (IORef Value) (MVar (Bool,[Async T.Text])) (FilePath,MVar (SetupStep,Maybe UUID.UUID))

openOnboarding :: FilePath -> IO Onboarding
openOnboarding root = do
 home <- P.codexHome
 let path=takeDirectory home</>"onboarding.json"
 state <- P.readSetup >>= newMVar
 stored <- doesPathExist (home</>"auth.json")
 hold <- P.profileHold
 when (not stored && T.null hold) (P.holdProfile "disconnected")
 Onboarding root <$> newMVar (False,Nothing) <*> newIORef (loginState "idle" "") <*> newMVar (False,[]) <*> pure (path,state)

setupState :: Onboarding -> IO Value
setupState (Onboarding _ _ _ _ (_,state)) = do
 (step,selected) <- readMVar state
 pure (obj [("step",str (stepName step)),("completed",Bool (step==SetupComplete)),("selected_corpora",maybe Null (str . UUID.toText) selected)])

advanceSetup :: Onboarding -> SetupStep -> SetupStep -> Maybe UUID.UUID -> IO ()
advanceSetup (Onboarding _ _ _ _ (path,state)) expected next selected = modifyMVar_ state $ \(current,previous) -> do
 require (current==expected || current==SetupComplete && next==SetupComplete && selected==previous) 409 "Setup changed. Check again before continuing."
 let starting=case selected of Just _ -> selected; Nothing -> previous
 require (next/=SetupComplete || isJust starting) 409 "Choose your starting Corpora before continuing."
 writeJSON path (obj [("schema",num (2::Int)),("step",str (stepName next)),("selected_corpora",maybe Null (str . UUID.toText) starting)])
  `catch` (\(_::IOException) -> failure 503 "Could not save setup. Check that Sapiens can write to its configuration folder, then retry.")
 pure (next,starting)

loginState :: T.Text -> T.Text -> Value
loginState state code = loginView state code (if T.null code then "" else "https://auth.openai.com/codex/device")

loginView :: T.Text -> T.Text -> T.Text -> Value
loginView state code url = obj [("state",str state),("code",str code),("url",str url)]

closeOnboarding :: Onboarding -> IO ()
closeOnboarding (Onboarding _ worker _ checks _) = do
 modifyMVar_ checks $ \(_,active) -> mapM_ cancel active >> pure (True,[])
 modifyMVar_ worker $ \(_,active) -> mapM_ cancel active >> pure (True,Nothing)

-- HTTP handlers may be abandoned when Warp stops; their subprocesses belong to
-- onboarding, which cancels and joins them before the host can exit.
checkedProviderStatus :: Onboarding -> IO T.Text
checkedProviderStatus (Onboarding _ _ _ checks _) = bracket acquire release wait
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
   result <- quietCommand path environment ["login","status"] 10000000
   stored <- maybe (pure False) (doesPathExist . (</>"auth.json")) (lookup "CODEX_HOME" environment)
   pure (case result of Just ExitSuccess -> "signed_in"; Just _ | not stored -> "sign_in_required"; _ -> "unavailable")) `catch` (\(_::IOException) -> pure "unavailable")

onboardingView :: Onboarding -> IO Value
onboardingView onboarding@(Onboarding root _ progress _ _) = do
 login <- readIORef progress
 provider <- if at login "state" `elem` ["signing_in","signing_out"] then pure (at login "state") else checkedProviderStatus onboarding
 binary <- C.blindlyBinary root
 built <- doesFileExist binary
 computer <- if not built then pure "missing" else if os/="darwin" then pure "not_required" else do
  -- Blindly checks AX trust before handling focused. This PID cannot be the
  -- foreground application, so the command never reads window contents.
  result <- (Just <$> C.invoke binary ["focused","--pid","2147483647"] 3) `catches`
   [Handler (\(_::IOException) -> pure Nothing),Handler (\(_::APIError) -> pure Nothing)]
  pure $ case result of
   Just (ExitFailure 77,out,_) -> case at (parseJSONText (T.pack out)) "code" of
    "focus_unavailable" -> "granted"
    "accessibility_permission_denied" -> "denied"
    _ -> "unavailable"
   _ -> "unavailable"
 saved <- setupState onboarding
 hold <- P.profileHold
 let ready=provider=="signed_in" && T.null hold
     complete=boolean (get saved "completed")
     step=at saved "step"
 pure (obj [("schema",num (2::Int)),("step",str step),("completed",Bool complete),("provider",str provider),("login",login),("computer_permission",str computer),("computer_available",Bool built),("ready",Bool ready),("hold",str hold),("selected_corpora",get saved "selected_corpora")])

onboardingAction :: Onboarding -> (UUID.UUID -> IO ()) -> Value -> IO Value
onboardingAction onboarding@(Onboarding root worker progress _ (_,state)) validateSelected value = do
 when (at value "action" `elem` ["back","provider-skip"]) $ do
  login <- readIORef progress
  require (at login "state" `notElem` ["signing_in","signing_out"]) 409 "Finish or cancel sign-in before changing setup steps."
 case at value "action" of
  "back" -> do
   (current,_) <- readMVar state
   previous <- case current of
    ComputerAccess -> pure AIProvider
    CorporaChoice -> pure ComputerAccess
    SetupReady -> pure (if boolean (get value "choose_corpora") then CorporaChoice else ComputerAccess)
    _ -> failure 409 "Setup cannot move back from this step."
   advanceSetup onboarding current previous Nothing
  "provider-skip" -> advanceSetup onboarding AIProvider ComputerAccess Nothing
  "provider-next" -> do
   status <- checkedProviderStatus onboarding
   require (status=="signed_in") 409 "Connect Codex before continuing."
   hold <- P.profileHold
   require (T.null hold) 409 "Finish connecting your ChatGPT account before continuing."
   -- An interrupted later step may need a new login; resume its saved decision.
   saved <- setupState onboarding
   when (at saved "step"=="provider") $ advanceSetup onboarding AIProvider ComputerAccess Nothing
  "permissions-done" -> do
   selected <- maybe (failure 400 "Choose a local Corpora before continuing.") pure (UUID.fromText (at value "selected_corpora"))
   validateSelected selected
   advanceSetup onboarding ComputerAccess (if boolean (get value "choose_corpora") then CorporaChoice else SetupReady) (Just selected)
  "corpora-done" -> do
   selected <- maybe (failure 400 "Choose a local Corpora before continuing.") pure (UUID.fromText (at value "selected_corpora"))
   validateSelected selected
   advanceSetup onboarding CorporaChoice SetupReady (Just selected)
  -- Initial completion holds admissions until the atomic save has returned.
  "finish" -> bracket
   (do
    saved <- setupState onboarding
    P.profileLock (if boolean (get saved "completed") then Shared else Exclusive)
     >>= maybe (failure 409 "AI work or sign-in is still running. Try again when it finishes.") pure)
   unlockFile $ \_ -> do
    saved <- setupState onboarding
    require (at saved "step"=="ready" || boolean (get saved "completed")) 409 "Finish the setup steps before entering Sapiens."
    selected <- maybe (failure 400 "Choose a local Corpora before continuing.") pure (UUID.fromText (at value "selected_corpora"))
    validateSelected selected
    unless (boolean (get saved "completed")) $ do
     status <- checkedProviderStatus onboarding
     hold <- P.profileHold
     -- An unavailable existing login must not release old queues merely because
     -- setup is optional. Preserve all existing account-safety holds.
     when (status/="signed_in" && T.null hold) (P.holdProfile "review")
    advanceSetup onboarding SetupReady SetupComplete (Just selected)
  action | action `elem` ["login","device-login","change-account","sign-out"] -> startAuth onboarding action
  "resume" -> bracket
   (P.profileLock Exclusive >>= maybe (failure 409 "AI work or sign-in is still running. Try again when it finishes.") pure)
   unlockFile $ \_ -> do
    status <- checkedProviderStatus onboarding
    require (status=="signed_in") 409 "Connect ChatGPT before resuming AI work."
    hold <- P.profileHold
    require (hold=="review") 409 "Finish connecting your ChatGPT account first."
    P.holdProfile ""
  "open-login" -> do
   login <- readIORef progress
   require (at login "state"=="signing_in" && not (T.null (at login "url"))) 409 "Start sign-in first."
   openLoginURL (at login "url")
  "cancel" -> modifyMVar_ worker $ \(closed,active) -> do
   current <- readIORef progress
   require (at current "state" `elem` ["signing_in","complete"]) 409 "Sign-out cannot be cancelled once it has started."
   mapM_ cancel active
   status <- checkedProviderStatus onboarding
   saved <- setupState onboarding
   when (status=="signed_in") $ bracket (P.profileLock Exclusive) (mapM_ unlockFile) $ mapM_ $ \_ ->
    P.holdProfile (if boolean (get saved "completed") then "review" else "")
   writeIORef progress (loginState (if status=="signed_in" then "complete" else "cancelled") "")
   pure (closed,Nothing)
  "permission" -> do
   require (os=="darwin") 400 "Accessibility approval is only required on macOS."
   binary <- C.blindlyBinary root
   exists <- doesFileExist binary
   require exists 503 "Computer helper is missing. Reinstall Sapiens to repair it."
   (code,_,_) <- C.invoke binary ["request-permission"] 10
   require (code==ExitSuccess) 503 "Could not request Accessibility access. Open System Settings > Privacy & Security > Accessibility, then try again."
  "refresh" -> pure ()
  _ -> failure 400 "Unknown setup action"
 onboardingView onboarding

-- Serialize all mutations with the UI worker and take an OS-wide exclusive
-- lease before touching credentials. A running task is never stopped by logout.
startAuth :: Onboarding -> T.Text -> IO ()
startAuth onboarding@(Onboarding _ worker progress _ _) action = modifyMVar_ worker $ \(closed,active) -> do
 require (not closed) 503 "Sapiens is shutting down."
 running <- maybe (pure False) (fmap isNothing . poll) active
 require (not running) 409 "Account sign-in or sign-out is already running."
 saved <- setupState onboarding
 let replacing=action `elem` ["change-account","sign-out"]
 binary <- P.codexBinary >>= maybe (failure 503 "Codex is missing. Reinstall Sapiens to repair its runtime.") pure
 environment <- P.codexEnvironment
 bracketOnError
  (P.profileLock Exclusive >>= maybe (failure 409 "Finish running tasks before changing your ChatGPT account. Another sign-in may also be in progress.") pure)
  unlockFile $ \lease -> do
   status <- checkedProviderStatus onboarding
   if replacing then require (status `elem` ["signed_in","unavailable"]) 409 "ChatGPT is already disconnected."
   else do
    require (status/="signed_in") 409 "ChatGPT is already connected."
    require (status=="sign_in_required") 503 "Could not check your existing sign-in. Recheck before continuing."
   -- Persist the hold before revocation or login can begin. A crash cannot
   -- release queued work under an uncertain or newly selected account.
   P.holdProfile (if replacing then "uncertain" else "disconnected")
   writeIORef progress (loginState (if replacing then "signing_out" else "signing_in") "")
   task <- async $ flip finally (unlockFile lease) $
    authFlow binary environment saved `catch` (\(errorValue::SomeException) -> case fromException errorValue :: Maybe SomeAsyncException of
     Just _ -> throwIO errorValue
     Nothing -> writeIORef progress (loginState "failed" ""))
   pure (False,Just task)
 where
 authFlow binary environment saved = do
  removed <- if action `elem` ["change-account","sign-out"] then do
   result <- quietCommand binary environment ["logout"] 30000000
   status <- providerStatus
   let removed=result==Just ExitSuccess && status=="sign_in_required"
   when removed (P.holdProfile "disconnected")
   pure removed
   else pure True
  if not removed then writeIORef progress (loginState "logout_failed" "")
  else if action=="sign-out" then writeIORef progress (loginState "signed_out" "")
  else do
   writeIORef progress (loginState "signing_in" "")
   outcome <- signIn binary environment progress (action=="device-login")
   status <- providerStatus
   if status=="signed_in" then do
    P.holdProfile (if boolean (get saved "completed") then "review" else "")
    writeIORef progress (loginState "complete" "")
   else writeIORef progress (loginState (if outcome=="complete" then "failed" else outcome) "")

quietCommand :: FilePath -> [(String,String)] -> [String] -> Int -> IO (Maybe ExitCode)
quietCommand binary environment args duration = withOwnedProcess
 (proc binary (P.codexConfig ++ args)) {env=Just environment,std_in=NoStream,std_out=NoStream,std_err=NoStream}
 (\(_,_,_,process) -> timeout duration (waitForProcess process))

-- Only the official HTTPS authorization URL is exposed, never CLI diagnostics.
validLoginURL :: T.Text -> Bool
validLoginURL url = T.length url<=16384 && case parseURI (T.unpack url) of
 Just uri -> uriScheme uri=="https:" && uriPath uri=="/oauth/authorize" && case uriAuthority uri of
  Just authority -> uriRegName authority=="auth.openai.com" && null (uriUserInfo authority) && null (uriPort authority)
  Nothing -> False
 Nothing -> False

openLoginURL :: T.Text -> IO ()
openLoginURL url = do
 require (validLoginURL url || url=="https://auth.openai.com/codex/device") 409 "The sign-in link is no longer available. Start again."
 let (program,args)=if os=="darwin" then ("/usr/bin/open",[T.unpack url]) else if os=="mingw32" then ("rundll32.exe",["url.dll,FileProtocolHandler",T.unpack url]) else ("xdg-open",[T.unpack url])
 (code,_,_) <- C.invoke program args 10
 require (code==ExitSuccess) 503 "Could not open your browser. Copy the sign-in link and open it in your browser."

-- Codex's browser listener can cancel an existing listener on its fixed port.
-- Refuse an occupied port before invoking it; never probe /cancel ourselves.
browserPortAvailable :: IO Bool
browserPortAvailable = Socket.withSocketsDo $
 (bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \socket -> do
   -- Match the CLI listener: a recently closed callback may still have
   -- TIME_WAIT connections. Only an active listener should block a new login.
   Socket.setSocketOption socket Socket.ReuseAddr 1
   Socket.bind socket (Socket.SockAddrInet 1455 (Socket.tupleToHostAddress (127,0,0,1)))
   Socket.listen socket 1
   pure True) `catch` (\(_::IOException) -> pure False)

signIn :: FilePath -> [(String,String)] -> IORef Value -> Bool -> IO T.Text
signIn binary environment progress device = do
 available <- if device then pure True else browserPortAvailable
 if not available then pure "browser_busy" else withOwnedProcess
  (proc binary (P.codexConfig ++ ["login"] ++ ["--device-auth" | device]))
   {env=Just environment,std_in=NoStream,std_out=if device then CreatePipe else NoStream,std_err=if device then NoStream else CreatePipe}
  (\(_,output,errors,process) -> do
   handle <- maybe (failure 500 "Sign-in output unavailable") pure (if device then output else errors)
   let consume expecting = do
        ended <- hIsEOF handle
        unless ended $ do
         line <- T.strip . T.pack . stripANSI <$> hGetLine handle
         if device then
          when (expecting && T.length line>=6 && T.length line<=20 && T.all (\c -> isAsciiUpper c || isDigit c || c=='-') line) $
           writeIORef progress (loginState "signing_in" line)
         else when (validLoginURL line) $ writeIORef progress (loginView "signing_in" "" line)
         consume ("Enter this one-time code" `T.isInfixOf` line)
       -- A browser descendant can keep the diagnostic pipe or Windows job
       -- alive after credentials have been saved. Check the same CLI/profile
       -- independently; pipe EOF is not an authentication completion signal.
       signedIn = do
        threadDelay 1000000
        status <- quietCommand binary environment ["login","status"] 10000000
        if status==Just ExitSuccess then pure ExitSuccess else signedIn
   result <- timeout (16*60*1000000) $ withAsync (consume False) $ \reader -> do
    link reader
    either id id <$> race (waitForProcess process) signedIn
   pure (case result of Just ExitSuccess -> "complete"; Nothing -> "expired"; _ -> "failed"))

stripANSI :: String -> String
stripANSI [] = []
stripANSI ('\ESC':'[':rest) = stripANSI (drop 1 (dropWhile (\c -> c<'@' || c>'~') rest))
stripANSI (c:rest) = c:stripANSI rest
