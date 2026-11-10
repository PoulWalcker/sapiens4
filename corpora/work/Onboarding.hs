{-# LANGUAGE CPP #-}
module Onboarding where

import Types
import qualified Provider as P
import Provider (SetupStep(..), stepName)
import qualified Computer as C
import Control.Applicative ((<|>))
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
data Onboarding = Onboarding
 { onboardingRoot :: FilePath
 , authWorker :: MVar (Bool,Maybe (Async ()))
 , loginProgress :: IORef LoginProgress
 , statusChecks :: MVar (Bool,[Async T.Text])
 , setupPath :: FilePath
 , setupSaved :: MVar (SetupStep,Maybe UUID.UUID)
 }

-- Only an active sign-in carries a browser link or device code.
data LoginProgress
 = LoginIdle | SigningIn { loginCode :: T.Text, loginURL :: T.Text }
 | SigningOut | LoginComplete | LoginFailed | LoginExpired | BrowserBusy
 | LogoutFailed | SignedOut | LoginCancelled
 deriving (Eq)

loginView :: LoginProgress -> Value
loginView progress = obj [("state",str state),("code",str code),("url",str url)]
 where
 (state,code,url) = case progress of
  LoginIdle -> ("idle","","")
  SigningIn deviceCode authorizationURL -> ("signing_in",deviceCode,authorizationURL)
  SigningOut -> ("signing_out","","")
  LoginComplete -> ("complete","","")
  LoginFailed -> ("failed","","")
  LoginExpired -> ("expired","","")
  BrowserBusy -> ("browser_busy","","")
  LogoutFailed -> ("logout_failed","","")
  SignedOut -> ("signed_out","","")
  LoginCancelled -> ("cancelled","","")

openOnboarding :: FilePath -> IO Onboarding
openOnboarding root = do
 home <- P.codexHome
 let path=takeDirectory home</>"onboarding.json"
 state <- P.readSetup >>= newMVar
 stored <- doesPathExist (home</>"auth.json")
 hold <- P.profileHold
 when (not stored && T.null hold) (P.holdProfile "disconnected")
 worker <- newMVar (False,Nothing)
 progress <- newIORef LoginIdle
 checks <- newMVar (False,[])
 pure Onboarding {onboardingRoot=root,authWorker=worker,loginProgress=progress,statusChecks=checks,setupPath=path,setupSaved=state}

setupState :: Onboarding -> IO Value
setupState onboarding = do
 (step,selected) <- readMVar (setupSaved onboarding)
 pure (obj [("step",str (stepName step)),("completed",Bool (step==SetupComplete)),("selected_corpora",maybe Null (str . UUID.toText) selected)])

advanceSetup :: Onboarding -> SetupStep -> SetupStep -> Maybe UUID.UUID -> IO ()
advanceSetup onboarding expected next selected = modifyMVar_ (setupSaved onboarding) $ \(current,previous) -> do
 require (current==expected || current==SetupComplete && next==SetupComplete && selected==previous) 409 "Setup changed. Check again before continuing."
 let starting=selected <|> previous
 require (next/=SetupComplete || isJust starting) 409 "Choose your starting Corpora before continuing."
 writeJSON (setupPath onboarding) (obj [("schema",num (2::Int)),("step",str (stepName next)),("selected_corpora",maybe Null (str . UUID.toText) starting)])
  `catch` (\(_::IOException) -> failure 503 "Could not save setup. Check that Sapiens can write to its configuration folder, then retry.")
 pure (next,starting)

closeOnboarding :: Onboarding -> IO ()
closeOnboarding onboarding = do
 modifyMVar_ (statusChecks onboarding) $ \(_,active) -> mapM_ cancel active >> pure (True,[])
 modifyMVar_ (authWorker onboarding) $ \(_,active) -> mapM_ cancel active >> pure (True,Nothing)

-- HTTP handlers may be abandoned when Warp stops; their subprocesses belong to
-- onboarding, which cancels and joins them before the host can exit.
checkedProviderStatus :: Onboarding -> IO T.Text
checkedProviderStatus onboarding = bracket acquire release wait
 where
 checks=statusChecks onboarding
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
   environment <- P.codexEnvironment path
   -- No diagnostic pipes: status output can contain account/key information.
   result <- quietCommand path environment ["login","status"] 10000000
   stored <- maybe (pure False) (doesPathExist . (</>"auth.json")) (lookup "CODEX_HOME" environment)
   pure (case result of Just ExitSuccess -> "signed_in"; Just _ | not stored -> "sign_in_required"; _ -> "unavailable")) `catch` (\(_::IOException) -> pure "unavailable")

onboardingView :: Onboarding -> IO Value
onboardingView onboarding = do
 login <- readIORef (loginProgress onboarding)
 provider <- case login of
  SigningIn {} -> pure "signing_in"
  SigningOut -> pure "signing_out"
  _ -> checkedProviderStatus onboarding
 binary <- C.blindlyBinary (onboardingRoot onboarding)
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
 pure (merge saved (obj [("schema",num (2::Int)),("provider",str provider),("login",loginView login),("computer_permission",str computer),("computer_available",Bool built),("ready",Bool ready),("hold",str hold)]))

onboardingAction :: Onboarding -> (UUID.UUID -> IO ()) -> Value -> IO Value
onboardingAction onboarding validateSelected value = do
 let progress=loginProgress onboarding
 when (at value "action" `elem` ["back","provider-skip"]) $ do
  login <- readIORef progress
  require (case login of SigningIn {} -> False; SigningOut -> False; _ -> True) 409 "Finish or cancel sign-in before changing setup steps."
 case at value "action" of
  "back" -> do
   current <- fst <$> readMVar (setupSaved onboarding)
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
   step <- fst <$> readMVar (setupSaved onboarding)
   when (step==AIProvider) $ advanceSetup onboarding AIProvider ComputerAccess Nothing
  "permissions-done" -> do
   selected <- selectedCorpora
   advanceSetup onboarding ComputerAccess (if boolean (get value "choose_corpora") then CorporaChoice else SetupReady) (Just selected)
  "corpora-done" -> do
   selected <- selectedCorpora
   advanceSetup onboarding CorporaChoice SetupReady (Just selected)
  -- Initial completion holds admissions until the atomic save has returned.
  "finish" -> bracket
   (do
    step <- fst <$> readMVar (setupSaved onboarding)
    P.profileLock (if step==SetupComplete then Shared else Exclusive)
     >>= maybe (failure 409 "AI work or sign-in is still running. Try again when it finishes.") pure)
   unlockFile $ \_ -> do
    step <- fst <$> readMVar (setupSaved onboarding)
    require (step==SetupReady || step==SetupComplete) 409 "Finish the setup steps before entering Sapiens."
    selected <- selectedCorpora
    unless (step==SetupComplete) $ do
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
   case login of
    SigningIn _ url | not (T.null url) -> openLoginURL url
    _ -> failure 409 "Start sign-in first."
  "cancel" -> modifyMVar_ (authWorker onboarding) $ \(closed,active) -> do
   current <- readIORef progress
   require (case current of SigningIn {} -> True; LoginComplete -> True; _ -> False) 409 "Sign-out cannot be cancelled once it has started."
   mapM_ cancel active
   status <- checkedProviderStatus onboarding
   step <- fst <$> readMVar (setupSaved onboarding)
   when (status=="signed_in") $ bracket (P.profileLock Exclusive) (mapM_ unlockFile) $ mapM_ $ \_ ->
    P.holdProfile (if step==SetupComplete then "review" else "")
   writeIORef progress (if status=="signed_in" then LoginComplete else LoginCancelled)
   pure (closed,Nothing)
  "permission" -> do
   require (os=="darwin") 400 "Accessibility approval is only required on macOS."
   binary <- C.blindlyBinary (onboardingRoot onboarding)
   exists <- doesFileExist binary
   require exists 503 "Computer helper is missing. Reinstall Sapiens to repair it."
   (code,_,_) <- C.invoke binary ["request-permission"] 10
   require (code==ExitSuccess) 503 "Could not request Accessibility access. Open System Settings > Privacy & Security > Accessibility, then try again."
  "refresh" -> pure ()
  _ -> failure 400 "Unknown setup action"
 onboardingView onboarding
 where
 selectedCorpora = do
  selected <- maybe (failure 400 "Choose a local Corpora before continuing.") pure (UUID.fromText (at value "selected_corpora"))
  validateSelected selected
  pure selected

-- Serialize all mutations with the UI worker and take an OS-wide exclusive
-- lease before touching credentials. A running task is never stopped by logout.
startAuth :: Onboarding -> T.Text -> IO ()
startAuth onboarding action = modifyMVar_ (authWorker onboarding) $ \(closed,active) -> do
 require (not closed) 503 "Sapiens is shutting down."
 running <- maybe (pure False) (fmap isNothing . poll) active
 require (not running) 409 "Account sign-in or sign-out is already running."
 step <- fst <$> readMVar (setupSaved onboarding)
 binary <- P.codexBinary >>= maybe (failure 503 "Codex is missing. Reinstall Sapiens to repair its runtime.") pure
 environment <- P.codexEnvironment binary
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
   writeIORef progress (if replacing then SigningOut else SigningIn "" "")
   task <- async $ flip finally (unlockFile lease) $
    authFlow binary environment step `catch` (\(errorValue::SomeException) -> case fromException errorValue :: Maybe SomeAsyncException of
     Just _ -> throwIO errorValue
     Nothing -> writeIORef progress LoginFailed)
   pure (False,Just task)
 where
 progress=loginProgress onboarding
 replacing=action `elem` ["change-account","sign-out"]
 authFlow binary environment step = do
  removed <- if replacing then do
   result <- quietCommand binary environment ["logout"] 30000000
   status <- providerStatus
   let removed=result==Just ExitSuccess && status=="sign_in_required"
   when removed (P.holdProfile "disconnected")
   pure removed
   else pure True
  if not removed then writeIORef progress LogoutFailed
  else if action=="sign-out" then writeIORef progress SignedOut
  else do
   writeIORef progress (SigningIn "" "")
   outcome <- signIn binary environment progress (action=="device-login")
   status <- providerStatus
   if status=="signed_in" then do
    P.holdProfile (if step==SetupComplete then "review" else "")
    writeIORef progress LoginComplete
   else writeIORef progress (if outcome==LoginComplete then LoginFailed else outcome)

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

signIn :: FilePath -> [(String,String)] -> IORef LoginProgress -> Bool -> IO LoginProgress
signIn binary environment progress device = do
 available <- if device then pure True else browserPortAvailable
 if not available then pure BrowserBusy else withOwnedProcess
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
           writeIORef progress (SigningIn line "https://auth.openai.com/codex/device")
         else when (validLoginURL line) $ writeIORef progress (SigningIn "" line)
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
   pure (case result of Just ExitSuccess -> LoginComplete; Nothing -> LoginExpired; _ -> LoginFailed))

stripANSI :: String -> String
stripANSI [] = []
stripANSI ('\ESC':'[':rest) = stripANSI (drop 1 (dropWhile (\c -> c<'@' || c>'~') rest))
stripANSI (c:rest) = c:stripANSI rest
