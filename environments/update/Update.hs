{-# LANGUAGE CPP #-}
module Update (UpdateCache, UpdateBusy(..), withUpdateCache, updateAndRestartCached, updateMode, updateAvailable, updateAndRestart, updateAndRestartWithProgress, candidateReady, preparingUpdate, tryUpdate) where

import Control.Exception
import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
#ifdef UPDATE_FIXTURE
import qualified Data.ByteString.Lazy as BSL
#endif
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isSpace)
import Data.List (stripPrefix)
import Data.IORef
import Data.Maybe (isNothing)
import Text.Read (readMaybe)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Numeric (showHex)
import System.Directory hiding (executable)
import System.Environment
import System.Exit (exitFailure)
import System.FilePath
import System.IO
import System.Timeout (timeout)
import UpdateState
import UpdatePlatform
import UpdateBackup
import Installation

tryUpdate :: IO a -> IO (Either SomeException a)
tryUpdate = tryJust $ \problem -> case fromException problem :: Maybe SomeAsyncException of
 Just _ -> Nothing
 Nothing -> Just problem

candidateReady :: IO () -> IO ()
candidateReady activate = do
 preparing <- preparingUpdate
 when preparing $ do
  putStrLn "update: ready" >> hFlush stdout
  command <- getLine `catch` \(_::IOException) -> pure ""
  unless (command `elem` ["commit", ""]) (fail "Invalid Coordinator activation message")
  (root,version) <- installed
  state <- readUpdateState root
  unless (case state of Settled s -> current s == version; _ -> False) (fail "Candidate is not durably selected")
  activate
  unsetEnv "SAPIENS_UPDATE_ATTEMPT"
  (putStrLn "update: active" >> hFlush stdout) `catch` \(_::IOException) -> pure ()

installed :: IO (FilePath,Version)
installed = installation >>= maybe (fail "Updates require an installed Sapiens version") pure

updateAndRestart :: (String -> IO String) -> IO () -> IO () -> FilePath -> FilePath -> IO ()
updateAndRestart = updateAndRestartWithProgress (pure ())

updateAndRestartWithProgress :: IO () -> (String -> IO String) -> IO () -> IO () -> FilePath -> FilePath -> IO ()
updateAndRestartWithProgress preparing command flushPreferences stop dataDirectory configDirectory =
 withUpdateCache $ \cache -> updateAndRestartCached cache preparing command flushPreferences stop dataDirectory configDirectory

data Release = Release
 { releaseVersion :: [Integer]
 , releaseURL :: String
 , releaseDigest :: Maybe String
 } deriving Eq
newtype UpdateCache = UpdateCache (IORef (Maybe (Maybe Release,FilePath)))

withUpdateCache :: (UpdateCache -> IO a) -> IO a
withUpdateCache = bracket (UpdateCache <$> newIORef Nothing) clearUpdateCache

clearUpdateCache :: UpdateCache -> IO ()
clearUpdateCache (UpdateCache ref) = atomicModifyIORef' ref (\old -> (Nothing,old)) >>= mapM_ (discard . pure . snd)

data UpdateBusy = UpdateBusy deriving Show
instance Exception UpdateBusy where
 displayException _ = "Sapiens is busy. Finish accepted work before updating."

updateAndRestartCached :: UpdateCache -> IO () -> (String -> IO String) -> IO () -> IO () -> FilePath -> FilePath -> IO ()
updateAndRestartCached cache preparing command flushPreferences stop dataDirectory configDirectory = mask $ \restore -> do
 result <- try (restore attempt)
 case result of
  Left problem | Just UpdateBusy <- fromException problem -> throwIO problem
  _ -> clearUpdateCache cache >> either throwIO pure result
 where
 attempt = do
  (root,owner) <- installed
  validateRoots root dataDirectory configDirectory
  next <- requestUpdate cache root owner
  withCandidate (versionExecutable root owner) ["--update-coordinate",versionName next,dataDirectory,configDirectory] [] $ \coordinator ->
   (do
    preparing
    await 15 coordinator "update: coordinator"
    let quiet = command "quiesce" >>= \reply -> case reply of
         "quiet" -> pure ()
         "busy" -> throwIO UpdateBusy
         _ -> fail "Sapiens could not pause safely. Update cancelled."
    (do
      (quiet >> flushPreferences) `onException` void (tryUpdate (command "resume"))
      void (tryUpdate (command "app-update-initiated"))
      stop)
     `onException` removePathForcibly (versionPath root next)
    candidateOwnership coordinator True
    sendCandidate coordinator "handoff")
   `onException` candidateOwnership coordinator False

requestUpdate :: UpdateCache -> FilePath -> Version -> IO Version
requestUpdate cache@(UpdateCache ref) root owner = do
 nonce <- freshVersion
 let destination = versionPath root nonce
 validatePath (root </> "versions")
#ifdef UPDATE_FIXTURE
 fixture <- lookupEnv "SAPIENS_UPDATE_FIXTURE_PAYLOAD"
 fixtureRelease <- lookupEnv "SAPIENS_UPDATE_FIXTURE_RELEASE"
#else
 let fixture = Nothing
     fixtureRelease = Nothing
#endif
 manager <- newManager tlsManagerSettings
 release <- if isNothing fixture || not (isNothing fixtureRelease) then Just <$> discoverRelease manager root owner else pure Nothing
 existing <- readIORef ref
 payload <- case existing of
  Just (cached,path) | release == cached -> pure path
  _ -> do
   clearUpdateCache cache
   let path = root </> (versionName nonce ++ "-" ++ payloadName)
   writeIORef ref (Just (release,path))
   case (fixture,release) of
    (Just file,_) -> copyFile file path
    (Nothing,Just available) -> do
     transfer <- parseRequest (releaseURL available)
     withResponse transfer manager $ \result -> withBinaryFile path WriteMode $ \file ->
      let loop = do chunk <- brRead (responseBody result); unless (B.null chunk) (B.hPut file chunk >> loop)
      in loop
    _ -> fail "No update payload is available"
   pure path
 validatePath payload
 when (isNothing fixture) $ do
  expected <- maybe (fail "Missing update payload SHA-256") pure (release >>= releaseDigest)
  verifyPayloadDigest expected payload
 (do
  stagePayload payload destination
  forM_ release $ \available -> do
   staged <- installedVersion root nonce
   unless (staged == releaseVersion available) (fail "Update payload version differs from the release tag"))
  `onException` removePathForcibly destination
 pure nonce

verifyPayloadDigest :: String -> FilePath -> IO ()
verifyPayloadDigest expected path = do
 digest <- withBinaryFile path ReadMode $ \file ->
  let readChunks context = do
       chunk <- B.hGetSome file 65536
       if B.null chunk then pure (SHA256.finalize context)
        else readChunks (SHA256.update context chunk)
  in readChunks SHA256.init
 let hex byte = let digits = showHex byte "" in replicate (2 - length digits) '0' ++ digits
 unless (concatMap hex (B.unpack digest) == expected) (fail "Update payload SHA-256 differs from the release")

updateAvailable :: IO Bool
updateAvailable = do
 result <- tryUpdate $ do
  (root,owner) <- installed
  manager <- newManager tlsManagerSettings
  void (discoverRelease manager root owner)
 pure (either (const False) (const True) result)

installedVersion :: FilePath -> Version -> IO [Integer]
installedVersion root version = readFile (versionPath root version </> "sapiens-release.version") >>= parseReleaseVersion

discoverRelease :: Manager -> FilePath -> Version -> IO Release
discoverRelease manager root owner = do
 installedRelease <- installedVersion root owner
 let repository = "PoulWalcker/sapiens4"
     latest = do
      request <- parseRequest ("https://api.github.com/repos/" ++ repository ++ "/releases/latest")
      responseBody <$> httpLbs request {requestHeaders=[("User-Agent","Sapiens4-Updater"),("Accept","application/vnd.github+json")]} manager
#ifdef UPDATE_FIXTURE
 fixture <- lookupEnv "SAPIENS_UPDATE_FIXTURE_RELEASE"
 body <- maybe latest (fmap BSL.fromStrict . B.readFile) fixture
#else
 let fixture = Nothing :: Maybe FilePath
 body <- latest
#endif
 value <- either fail pure (eitherDecode body)
 tag <- either fail pure (parseEither (withObject "release" (\o -> o .: "tag_name")) value)
 available <- parseReleaseVersion tag
 unless (available > installedRelease) (fail "Sapiens is already up to date.")
 assets <- either fail pure (parseEither (withObject "release" (\o -> o .: "assets")) value) :: IO [Value]
 let asset = withObject "release asset" $ \o -> (,) <$> o .: "name" <*> ((,) <$> o .: "browser_download_url" <*> o .:? "digest")
 entries <- forM assets (either fail pure . parseEither asset) :: IO [(String,(String,Maybe String))]
 (url,publishedDigest) <- maybe (fail "No update payload for this platform is available in the latest release") pure (lookup payloadName entries)
 unless (url == "https://github.com/" ++ repository ++ "/releases/download/" ++ tag ++ "/" ++ payloadName) (fail "Unexpected update download origin")
 let digest = publishedDigest >>= stripPrefix "sha256:"
 when (maybe (isNothing fixture) (not . validDigest) digest) (fail "Missing or invalid update payload SHA-256")
 pure (Release available url digest)

parseReleaseVersion :: String -> IO [Integer]
parseReleaseVersion input = do
 let trimmed = reverse (dropWhile isSpace (reverse input))
     version = case trimmed of 'v':rest -> rest; _ -> trimmed
     parts value = case break (=='.') value of
      (part,'.':rest) -> part : parts rest
      (part,_) -> [part]
     fields = parts version
 let digits field = not (null field) && all (\c -> c >= '0' && c <= '9') field
 case traverse readMaybe fields of
  Just release | length fields == 3 && all digits fields -> pure release
  _ -> fail "Expected a release version in MAJOR.MINOR.PATCH format"


updateMode :: [String] -> IO Bool
updateMode args = case args of
 ["--update-coordinate",name,dataDirectory,configDirectory] -> do
  version <- either fail pure (parseEither parseJSON (toJSON name))
  putStrLn "update: coordinator" >> hFlush stdout
  command <- getLine `catch` \(_::IOException) -> pure ""
  when (command == "handoff") $ report (coordinate (Just (version,dataDirectory,configDirectory)))
  pure True
 ["--update-recover"] -> report (coordinate Nothing) >> pure True
 _ -> pure False
 where
 report action = tryUpdate action >>= either (\problem -> showUpdateError (displayException problem) >> exitFailure) pure

data Dispatch = Running | Launch Version | Recover

newtype CandidateNotJoined = CandidateNotJoined SomeException deriving Show
instance Exception CandidateNotJoined where
 displayException (CandidateNotJoined problem) =
  "The candidate process tree could not be joined. Recovery is deferred. Close Sapiens and relaunch from its installed shortcut. " ++ displayException problem

discard :: [FilePath] -> IO ()
discard paths = forM_ paths $ \path -> removePathForcibly path `catch` \(_::IOException) -> pure ()

coordinate :: Maybe (Version,FilePath,FilePath) -> IO ()
coordinate requested = do
 (root,owner) <- installed
 dispatch <- withLease (leasePath root) True $ \lease -> do
  state <- readUpdateState root
  case (state,requested) of
   (Pending pending,_) -> do
    unless (owner == current (original pending)) (fail "Recovery must run through retained original version")
    restoreSnapshot root pending (writeUpdateState root (Settled (original pending)))
    discard [versionPath root (target pending), backupPath root (attempt pending)]
    pure (Launch (current (original pending)))
   (Settled selected,Nothing) -> pure (Launch (current selected))
   (Settled selected,Just (next,dataDirectory,configDirectory)) -> do
    unless (owner == current selected && next /= owner && Just next /= (previousVersion <$> previous selected)) (fail "Update selection changed")
    result <- tryUpdate $ do
     originalData <- canonicalizePath dataDirectory
     originalConfig <- canonicalizePath configDirectory
     validateRoots root originalData originalConfig
     claimConfigRoot originalConfig
     nonce <- freshVersion
     let backups = root </> "backups"
     exists <- pathPresent backups
     unless exists (createDirectory backups)
     validatePath (versionExecutable root next)
     withRootIdentity originalData $ \dataId -> withRootIdentity originalConfig $ \configId -> do
      saved <- withRootLocks originalData $ do
       fingerprint <- snapshot (backupPath root nonce) originalData originalConfig
       reserveDataRoot originalData root nonce
       let pending = PendingUpdate selected next nonce originalData originalConfig dataId configId fingerprint
       writeUpdateState root (Pending pending)
       pure pending
      downgradeLease lease
      runCandidate root saved
    case result of
     Right () -> do
      forM_ (previous selected) $ \old ->
       discard [versionPath root (previousVersion old),root </> "backups" </> versionName (previousBackup old)]
      pure Running
     Left problem -> do
      decision <- readUpdateState root
      hPutStrLn stderr ("Sapiens update: " ++ displayException problem) `catch` \(_::IOException) -> pure ()
      case decision of
       Settled selection | current selection == next ->
        fail ("The update was committed, but activation failed. Relaunch Sapiens with its installed shortcut. " ++ displayException problem)
       Settled selection -> pure (Launch (current selection))
       Pending _ | Just (_::CandidateNotJoined) <- fromException problem -> throwIO problem
       Pending _ -> pure Recover
 case dispatch of
  Running -> pure ()
  Launch version -> launchVersion root version []
  Recover -> coordinate Nothing

runCandidate :: FilePath -> PendingUpdate -> IO ()
runCandidate root pending = withCandidate (versionExecutable root (target pending)) []
 [("SAPIENS_UPDATE_ATTEMPT",versionName (attempt pending)),("SAPIENS_DATA_DIR",dataRoot pending),("SAPIENS_CONFIG_DIR",configRoot pending)] $ \candidate -> do
  (do
   await (60 * 60) candidate "update: ready"
   alive <- candidateAlive candidate
   unless alive (fail "Candidate exited before commit")
   state <- readUpdateState root
   unless (state == Pending pending) (fail "Update decision changed before Ready")
   mask_ $ do
    writeUpdateState root (Settled (Selection (target pending) (Just (Previous (current (original pending)) (attempt pending) (backupDigest pending)))))
    candidateOwnership candidate True)
   `onException` (tryUpdate (candidateOwnership candidate False) >>= either (throwIO . CandidateNotJoined) pure)
  sendCandidate candidate "commit"
  await 60 candidate "update: active"

await :: Int -> Candidate -> String -> IO ()
await seconds candidate expected =
 timeout (seconds * 1000000) loop >>= maybe (fail ("Timed out waiting for " ++ expected)) pure
 where
 loop = do
  message <- candidateMessage candidate
  when (message == "update: failed") (fail "Candidate could not prepare the workspace")
  unless (message == expected) loop
