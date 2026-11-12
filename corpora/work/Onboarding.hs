{-# LANGUAGE CPP #-}
module Onboarding where

import Types
import qualified Provider as P
import Population (readProviders, Provider(..), Providers(..))
import qualified Computer as C
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Aeson (Value(..))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory
import System.Environment (getEnvironment)
import System.Exit
import System.Info
import System.IO
import System.Process hiding (runProcess)
import System.Timeout
#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif

data Onboarding = Onboarding FilePath (MVar (Bool,[Async T.Text]))

openOnboarding :: FilePath -> IO Onboarding
openOnboarding root = Onboarding root <$> newMVar (False,[])

closeOnboarding :: Onboarding -> IO ()
closeOnboarding (Onboarding _ checks) = modifyMVar_ checks $ \(_,active) -> mapM_ cancel active >> pure (True,[])

trackedCheck :: Onboarding -> IO T.Text -> IO T.Text
trackedCheck (Onboarding _ checks) runCheck = bracket acquire release wait
 where
 acquire = modifyMVar checks $ \(closed,active) -> do
  require (not closed) 503 "Sapiens is shutting down."
  task <- async runCheck
  pure ((False,task:active),task)
 release task = do
  cancel task
  modifyMVar_ checks $ \(closed,active) -> pure (closed,filter ((/=asyncThreadId task) . asyncThreadId) active)

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

cliStatus :: IO (Maybe FilePath) -> IO [(String,String)] -> [String] -> IO T.Text
cliStatus findBinary environmentOf args = do
 binary <- findBinary
 case binary of
  Nothing -> pure "missing"
  Just path -> (do
   environment <- environmentOf
   result <- withOwnedProcess (proc path args) {env=Just environment,std_in=NoStream,std_out=NoStream,std_err=NoStream}
    (\(_,_,_,process) -> timeout 10000000 (waitForProcess process))
   pure (case result of
    Just ExitSuccess -> "signed_in"
    Just _ -> "sign_in_required"
    Nothing -> "unavailable")) `catch` (\(_::IOException) -> pure "unavailable")

onboardingView :: Onboarding -> IO Value
onboardingView onboarding@(Onboarding root _) = do
 configured <- readProviders root
 let providers = providersList configured
 found <- mapConcurrently (\provider -> trackedCheck onboarding (cliStatus (P.providerBinary provider) getEnvironment (providerStatus provider))) providers
 pure (obj [("providers",arr ([obj [("id",str (providerName (providerId provider))),("name",str (providerLabel provider)),("status",str status),("login",str (providerLogin provider))] | (provider,status) <- zip providers found]
  ++ [obj [("id",str key),("name",str name),("status",str "coming_soon")] | (key,name) <- M.toList (providersUpcoming configured)]))])

computerView :: Onboarding -> IO Value
computerView (Onboarding root _) = do
 binary <- C.blindlyBinary root
 built <- doesFileExist binary
 computer <- if not built then pure "missing" else if os/="darwin" then pure "not_required" else do
  result <- (Just <$> C.invoke binary ["focused","--pid","2147483647"] 3) `catches`
   [Handler (\(_::IOException) -> pure Nothing),Handler (\(_::APIError) -> pure Nothing)]
  pure $ case result of
   Just (ExitFailure 77,out,_) -> case at (parseJSONText (T.pack out)) "code" of
    "focus_unavailable" -> "granted"
    "accessibility_permission_denied" -> "denied"
    _ -> "unavailable"
   _ -> "unavailable"
 pure (obj [("computer_permission",str computer),("computer_available",Bool built)])

computerAction :: Onboarding -> Value -> IO Value
computerAction onboarding@(Onboarding root _) value = do
 case at value "action" of
  "permission" -> do
   require (os=="darwin") 400 "Accessibility approval is only required on macOS."
   binary <- C.blindlyBinary root
   exists <- doesFileExist binary
   require exists 503 "Computer helper is missing. Reinstall Sapiens to repair it."
   (code,_,_) <- C.invoke binary ["request-permission"] 10
   require (code==ExitSuccess) 503 "Could not request Accessibility access. Open System Settings > Privacy & Security > Accessibility, then try again."
  "refresh" -> pure ()
  _ -> failure 400 "Unknown computer action"
 computerView onboarding
