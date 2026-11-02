{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GeneratedCode (Profile, profileJSON, available, reservation, businessReservation, runFixture, reconcile) where

import Control.Concurrent
import Control.Concurrent.Async hiding (wait)
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (isHexDigit)
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import GHC.Clock
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import qualified System.Info as Platform
import System.IO hiding (stdin,stdout,stderr)
import System.Process
import System.Timeout
import Types
import qualified Provider

-- Only this verified external profile is available today. Selection remains an
-- explicit part of the candidate, not a global language choice for all work.
data Profile = Profile FilePath Text Text deriving (Eq, Show)
image :: Text
image = "python@sha256:ffb752e139c0a19692a43af8d8523b274222dd68eebad5d583b45c2201c6e30a"
profileJSON :: Profile -> Value
profileJSON (Profile _ _ config) = obj [("profile",str "python-3.13.15-linux-arm64"),("image",str image),("imageConfig",str config),("platform",str "linux/arm64"),("hostTarget",str "macos/arm64"),("languageVersion",str "3.13.15"),("engineVersion",str "29.4.0"),("dependencies",arr []),("capabilities",arr []),("limits",obj [("memoryBytes",num 134217728),("swapBytes",num 0),("pids",num 16),("cpus",num 1),("cpuSeconds",num 3),("wallMs",num 5000),("sourceBytes",num 65536),("inputBytes",num 65536),("outputBytes",num 65536),("tmpfsBytes",num 16777216),("shmBytes",num 1048576)])]
reservation :: Profile -> Text -> Text -> Text -> Value
reservation (Profile _ socket config) owner preparation nonce = obj [("name",str ("sapiens-"<>nonce)),("nonce",str nonce),("owner",str owner),("preparation",str preparation),("socket",str socket),("imageConfig",str config),("status",str "reserved")]
businessReservation :: Profile -> Text -> Text -> Text -> Value
businessReservation profile owner workload nonce = remove ["preparation"] (mergeFields [("kind",str "occurrence"),("workload",str workload)] (reservation profile owner workload nonce))
ownershipKey :: Value -> Text
ownershipKey value=if at value "kind"=="occurrence" then "workload" else "preparation"

configuration :: IO (FilePath,Text)
configuration = do
 home <- getHomeDirectory
 docker <- fromMaybe (home </> ".orbstack/bin/docker") <$> lookupEnv "SAPIENS_GENERATED_DOCKER"
 socket <- T.pack . fromMaybe ("unix://"++home++"/.orbstack/run/docker.sock") <$> lookupEnv "SAPIENS_GENERATED_SOCKET"
 require (isAbsolute docker && "unix:///" `T.isPrefixOf` socket) 503 "Generated code requires an absolute Docker CLI and local Unix socket"
 exists <- doesFileExist docker
 require exists 503 "Verified generated-code Docker CLI is unavailable"
 pure (docker,socket)

-- Control output is bounded too: an unavailable or hostile daemon must not
-- turn a metadata probe into unbounded allocation or leave a waiting client.
control :: FilePath -> Text -> [String] -> IO (ExitCode,Text,Text)
control docker socket arguments = controlBounded docker socket arguments `catch` (\(problem::IOException)->failure 503 ("Generated-code runtime unavailable: "<>T.pack (displayException problem)))

controlBounded :: FilePath -> Text -> [String] -> IO (ExitCode,Text,Text)
controlBounded docker socket arguments = do
 let cp=(proc docker (["--host",T.unpack socket]++arguments)) {std_in=NoStream,std_out=CreatePipe,std_err=CreatePipe,env=Just [("PATH","/usr/bin:/bin")],create_group=True}
 bracket (createProcess cp) (\(_,out,err,ph)->do
   alive<-isNothing <$> getProcessExitCode ph
   when alive (Provider.killTree ph)
   void (timeout 1000000 (waitForProcess ph))
   mapM_ (\h->hClose h `catch` (\(_::IOException)->pure ())) (catMaybes [out,err])) $ \(_,out,err,ph)->do
  let readBounded Nothing=pure B.empty
      readBounded (Just h)=B.hGet h 65537
  result <- timeout 5000000 $ concurrently (concurrently (readBounded out) (readBounded err)) (waitForProcess ph)
  case result of
   Nothing -> failure 503 "Generated-code runtime control deadline exceeded"
   Just ((stdout,stderr),exitCode) -> do
    require (B.length stdout<=65536 && B.length stderr<=65536) 503 "Generated-code runtime control output exceeded its bound"
    pure (exitCode,TE.decodeUtf8With (\_ _->Just '\xfffd') stdout,TE.decodeUtf8With (\_ _->Just '\xfffd') stderr)
checked :: FilePath -> Text -> [String] -> IO Text
checked docker socket args = do
 (code,out,err)<-control docker socket args
 require (code==ExitSuccess) 503 ("Generated-code runtime unavailable: "<>T.take 2048 err)
 pure (T.strip out)
available :: IO Profile
available = do
 require (Platform.os=="darwin" && Platform.arch=="aarch64") 503 "This generated-code profile is verified only on macOS ARM64"
 (docker,socket)<-configuration
 version<-checked docker socket ["version","--format","{{.Server.Version}}"]
 require (version=="29.4.0") 503 "Generated code requires verified Docker server 29.4.0"
 caps<-checked docker socket ["info","--format","{{.CgroupVersion}} {{.MemoryLimit}} {{.SwapLimit}} {{.PidsLimit}}"]
 require (caps=="2 true true true") 503 "Generated code requires verified cgroup limits"
 raw<-checked docker socket ["image","inspect",T.unpack image,"--format","{{json .}}"]
 let descriptor=parseJSONText raw
 require (at descriptor "Architecture"=="arm64" && at descriptor "Os"=="linux") 503 "Pinned generated-code image is unavailable for linux/arm64"
 let config=at descriptor "Id"
 require ("sha256:" `T.isPrefixOf` config && T.length config==71) 503 "Pinned generated-code image has no immutable config identity"
 pure (Profile docker socket config)

data Ownership = Ownership Text Text Text Text Text Text (Maybe Text) deriving (Eq,Show)
instance FromJSON Ownership where
 parseJSON = withObject "Owned generated-code container" $ \fields -> do
  name<-fields .: "name"
  nonce<-fields .: "nonce"
  owner<-fields .: "owner"
  kind<-fields .:? "kind" .!= ("" :: Text)
  unless (kind `elem` ["","occurrence"]) (fail "Unknown container ownership kind")
  preparation<-if kind=="occurrence" then fields .: "workload" else fields .: "preparation"
  socket<-fields .: "socket"
  config<-fields .: "imageConfig"
  identity<-fields .:? "id"
  unless (T.length nonce==32 && T.all isHexDigit nonce && name=="sapiens-"<>nonce) (fail "Invalid durable container name/nonce")
  unless (all (\value->not (T.null value) && T.length value<=128) [owner,preparation]) (fail "Invalid durable container owner")
  unless ("unix:///" `T.isPrefixOf` socket && "sha256:" `T.isPrefixOf` config && T.length config==71 && T.all isHexDigit (T.drop 7 config)) (fail "Invalid durable runtime identity")
  unless (maybe True (\value->T.length value==64 && T.all isHexDigit value) identity) (fail "Invalid durable container ID")
  pure (Ownership name nonce owner preparation socket config identity)

inspectOwned :: FilePath -> Text -> Value -> IO (Maybe Value)
inspectOwned docker socket ownership = do
 void (decodeValue 409 ownership :: IO Ownership)
 let identity=at ownership "id" `nonempty` at ownership "name"
 (code,out,err)<-control docker socket ["inspect",T.unpack identity]
 if code/=ExitSuccess then do
  require ("no such object" `T.isInfixOf` T.toLower err || "no such container" `T.isInfixOf` T.toLower err) 503 "Owned container absence could not be confirmed"
  pure Nothing
 else do
  let rows=case parseJSONText out of Array xs->V.toList xs; _->[]
  descriptor<-case rows of [row]->pure row; _->failure 409 "Owned container inspection is malformed"
  let labels=get (get descriptor "Config") "Labels"
  require (at descriptor "Name"=="/"<>at ownership "name"
    && at labels ("sapiens."<>ownershipKey ownership)==at ownership (ownershipKey ownership)
    && at labels "sapiens.kind"==at ownership "kind"
    && at labels "sapiens.owner"==at ownership "owner"
    && at labels "sapiens.nonce"==at ownership "nonce"
    && at descriptor "Image"==at ownership "imageConfig"
    && (T.null (at ownership "id") || at descriptor "Id"==at ownership "id")) 409 "Container ownership does not match; unrelated container was preserved"
  pure (Just descriptor)
 where nonempty a b=if T.null a then b else a

reconcile :: Value -> IO ()
reconcile ownership = do
 (docker,socket)<-configuration
 require (socket==at ownership "socket") 503 "Owned container belongs to another runtime endpoint"
 existing<-inspectOwned docker socket ownership
 forM_ existing $ \descriptor -> do
  void (checked docker socket ["rm","--force",T.unpack (at descriptor "Id")])
  absent<-inspectOwned docker socket ownership
  require (isNothing absent) 503 "Owned container removal was not confirmed"

-- Reservation is already durable when this function is called. Every following
-- lifecycle receipt goes back to the same Workload before the next external step.
runFixture :: Profile -> Run -> Value -> Text -> Value -> (Value -> IO ()) -> IO Value
runFixture (Profile docker socket _) running reserved source input persist = do
 require (B.length (TE.encodeUtf8 source)<=65536 && BL.length (encode input)<=65536) 413 "Generated source or fixture input exceeds 64 KiB"
 let name=at reserved "name"
     arguments=["create","--name",T.unpack name,"--label",T.unpack ("sapiens."<>ownershipKey reserved<>"="<>at reserved (ownershipKey reserved)),"--label","sapiens.kind="++T.unpack (at reserved "kind"),"--label","sapiens.owner="++T.unpack (at reserved "owner"),"--label","sapiens.nonce="++T.unpack (at reserved "nonce"),"--platform","linux/arm64","--pull","never","--network","none","--read-only","--user","65534:65534","--cap-drop","ALL","--security-opt","no-new-privileges=true","--pids-limit","16","--cpus","1","--ulimit","cpu=3:3","--memory","128m","--memory-swap","128m","--tmpfs","/tmp:rw,noexec,nosuid,size=16m","--shm-size","1m","--log-driver","none","--interactive","--entrypoint","python3",T.unpack image,"-I","-c",T.unpack source]
 ownership<-newIORef reserved
 let cleanup=do
      current<-readIORef ownership
      reconcile current
      persist (put "status" (str "removed") current)
 result <- (do
  identity<-checked docker socket arguments
  require (T.length identity==64 && T.all isHexDigit identity) 503 "Generated runtime returned an invalid container identity"
  let created=mergeFields [("id",str identity),("status",str "created")] reserved
  writeIORef ownership created
  persist created
  stopped<-readIORef (runCancel running)
  require (not stopped) 409 "Preparation stopped before container start"
  present<-inspectOwned docker socket created
  require (isJust present) 503 "Owned container is absent before start"
  persist (put "status" (str "running") created)
  output<-newMVar B.empty
  overflow<-newIORef False
  began<-getMonotonicTimeNSec
  let cp=(proc docker ["--host",T.unpack socket,"start","--attach","--interactive",T.unpack identity]) {std_in=CreatePipe,std_out=CreatePipe,std_err=CreatePipe,env=Just [("PATH","/usr/bin:/bin")],create_group=True}
  bracket (createProcess cp) (\(stdin,out,err,ph)->do
    alive<-isNothing <$> getProcessExitCode ph
    when alive (Provider.killTree ph)
    void (timeout 1000000 (waitForProcess ph))
    mapM_ (\h->hClose h `catch` (\(_::IOException)->pure ())) (catMaybes [stdin,out,err])) $ \(stdin,out,err,ph)->do
   let reader h=do
        chunk<-B.hGetSome h 4096
        unless (B.null chunk) $ do
         modifyMVar_ output $ \saved->do
          let room=65536-B.length saved
          when (B.length chunk>room) (writeIORef overflow True)
          pure (saved<>B.take room chunk)
         tooMuch<-readIORef overflow
         unless tooMuch (reader h)
   let writer=forM_ stdin $ \h->(BL.hPut h (encode input) >> hClose h) `catch` (\(_::IOException)->pure ())
   withAsync writer $ \_ -> withAsync (mapConcurrently_ reader (catMaybes [out,err])) $ \readers->do
    let wait=do
         cancelled<-readIORef (runCancel running)
         exceeded<-readIORef overflow
         tick<-getMonotonicTimeNSec
         code<-getProcessExitCode ph
         if cancelled then pure "stopped" else if exceeded then pure "output_limit"
          else if tick>=runDeadline running || tick-began>=5000000000 then pure "timeout"
          else if isJust code then pure "exited" else threadDelay 10000 >> wait
    reason<-wait
    when (reason/="exited") $ void (control docker socket ["kill","--signal","KILL",T.unpack identity])
    void (timeout 1000000 (waitCatch readers))
    cancel readers
    descriptor<-inspectOwned docker socket created >>= maybe (failure 503 "Container disappeared before its result was retained") pure
    -- A dead Docker attachment is not evidence that its container exited.
    let state=get descriptor "State"
    when (boolean (get state "Running")) $ void (control docker socket ["kill","--signal","KILL",T.unpack identity])
    captured<-readMVar output
    ended<-getMonotonicTimeNSec
    pure (obj [("status",str reason),("exitCode",get state "ExitCode"),("oomKilled",get state "OOMKilled"),("wasRunning",get state "Running"),("output",str (TE.decodeUtf8With (\_ _->Just '\xfffd') captured)),("durationMs",toJSON ((ended-began) `div` 1000000)),("retainedBytes",num (B.length captured))])
  ) `finally` cleanup
 pure (put "cleanupConfirmed" (Bool True) result)
