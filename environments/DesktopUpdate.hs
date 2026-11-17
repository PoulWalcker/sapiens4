{-# LANGUAGE OverloadedStrings #-}
module DesktopUpdate
  ( DesktopUpdate, Release, releaseVersion, withDesktopUpdate, watchReleases
  , InstallResult(..), installRelease, stopIdle, recordReply, updateError, UpdateView(..), viewScript, workerFailure, startupNotice
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseEither, Parser)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as TE
import Data.Char (isAscii, isHexDigit)
import Data.List (stripPrefix)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Version
import Text.ParserCombinators.ReadP (readP_to_S)
import Installation
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Network.URI
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Info
import System.Process
import System.Timeout

data DesktopUpdate = DesktopUpdate Installation Version String String Manager
data UpdateView = NoOffer | Offered Release | Installing Release String

viewScript :: UpdateView -> Text
viewScript view = "window.setDesktopUpdate?.(" <> TE.decodeUtf8 (L.toStrict $ encode payload) <> ");"
  where
    payload = case view of
      NoOffer -> Null
      Offered release -> object ["version" .= releaseVersion release]
      Installing _ label -> object ["label" .= label]

startupNotice :: DesktopUpdate -> IO (Maybe String)
startupNotice (DesktopUpdate owner _ _ _ _) = do
  tag <- UUID.toString <$> UUID.nextRandom
  let path = statePath owner </> "outcome.txt"
      claimed = statePath owner </> ("outcome-" ++ tag)
  exists <- tryJust (\e -> if isDoesNotExistError e then Just () else Nothing) (renameFile path claimed)
  case exists of
    Left () -> pure Nothing
    Right () -> Just . T.unpack . TE.decodeUtf8 <$> B.readFile claimed `finally` removeFile claimed

workerFailure :: DesktopUpdate -> ProcessHandle -> IO (Maybe String)
workerFailure (DesktopUpdate owner _ _ _ _) worker = do
  code <- waitForProcess worker
  case code of
    ExitSuccess -> pure Nothing
    ExitFailure n -> do
      let outcome = statePath owner </> "outcome.txt"
      recorded <- doesFileExist outcome
      Just <$> if recorded then readFile outcome else pure ("The installer exited with code " ++ show n)

data Release = Release Version UpdateContract String String deriving Eq

releaseVersion :: Release -> String
releaseVersion (Release version _ _ _) = showVersion version

versionField :: String -> Parser Version
versionField raw = case [v | (v,"") <- readP_to_S parseVersion raw, showVersion v == raw,
                             length (versionBranch v) == 3, null (versionTags v),
                             all (>=0) (versionBranch v)] of
  [v] -> pure v
  _ -> fail "Invalid desktop release version"

https :: String -> Parser String
https url = case parseURI url of
  Just uri | uriScheme uri == "https:", Just server <- uriAuthority uri,
             null (uriUserInfo server), not (null (uriRegName server)), null (uriFragment uri) -> pure url
  _ -> fail "An update URL must be HTTPS"

withDesktopUpdate :: (Maybe DesktopUpdate -> IO a) -> IO a
withDesktopUpdate action = withInstallation $ \owner -> case owner of
  Nothing -> action Nothing
  Just install -> do
    let file = runtimePath (appPath install) </> "desktop-update.json"
    configured <- doesFileExist file
    if not configured then action Nothing else do
      bytes <- B.readFile file
      (version,feed,platform) <- either fail pure (eitherDecodeStrict bytes >>= parseEither config)
      manager <- newManager tlsManagerSettings
      action (Just (DesktopUpdate install version feed platform manager))
  where
    config = withObject "installed desktop release" $ \o -> do
      platform <- o .: "platform"
      format <- o .: "format"
      contract <- o .: "contract"
      unless ((contract == PrepareData || format == sameFormatFamily) && platform == nativePlatform) $
        fail "Invalid installed update target"
      (,,) <$> (o .: "version" >>= versionField) <*> (o .: "feed" >>= https) <*> pure platform
    nativePlatform = if os == "darwin" then "macos-arm64"
                     else if arch == "aarch64" then "win-arm64" else "win-x64"

check :: DesktopUpdate -> IO (Maybe Release)
check (DesktopUpdate _ current feed platform manager) = do
  request <- parseRequest feed
  let bounded reader remaining chunks = do
        chunk <- brRead reader
        if B.null chunk then pure (B.concat (reverse chunks)) else do
          when (B.length chunk > remaining) $ fail "Update metadata exceeds 64 KiB"
          bounded reader (remaining - B.length chunk) (chunk:chunks)
  bytes <- withResponse request {redirectCount=0,responseTimeout=responseTimeoutMicro 10000000} manager $ \response -> do
    unless (statusCode (responseStatus response) == 200) $ fail "Update feed unavailable"
    bounded (responseBody response) 65536 []
  either fail pure (eitherDecodeStrict bytes >>= parseEither parseOffer)
  where
    parseOffer = withObject "desktop update feed" $ \o -> do
      product <- o .: "product"
      unless (product == ("com.sapiens4.desktop" :: Text)) $ fail "Unexpected desktop product"
      format <- o .: "format"
      version <- o .: "version" >>= versionField
      contract <- o .: "contract"
      if (contract == SameFormat && format /= sameFormatFamily) || version <= current then pure Nothing else do
        assets <- o .: "assets"
        withObject "release assets" (\targets -> do
          asset <- targets .:? (Key.fromString platform)
          traverse (withObject "release asset" $ \entry -> do
            url <- entry .: "url" >>= https
            digest <- entry .: "sha256"
            unless (length digest == 64 && all (\c -> isAscii c && isHexDigit c) digest) $ fail "Invalid release SHA-256"
            pure (Release version contract url digest)) asset) assets

watchReleases :: DesktopUpdate -> (Either String (Maybe Release) -> IO ()) -> IO ()
watchReleases update notify = forever $ do
  result <- tryJust updateError (check update)
  notify result
  replicateM_ 24 (threadDelay 3600000000)

updateError :: SomeException -> Maybe String
updateError e
  | Just err <- fromException e :: Maybe IOException = Just (displayException err)
  | Just err <- fromException e :: Maybe HttpException = Just (displayException err)
  | otherwise = Nothing

recordReply :: MVar String -> String -> IO ()
recordReply replies line = forM_ (stripPrefix "update: " line) (void . tryPutMVar replies)

stopIdle :: Handle -> MVar String -> IO ()
stopIdle input replies = do
  hPutStrLn input "stop-for-update"
  hFlush input
  reply <- timeout 10000000 (takeMVar replies)
  case reply of
    Just "stopping" -> pure ()
    Just "busy" -> fail "Local work is active. Finish it and try again."
    Just reason -> fail ("Backend refused the update: " ++ reason)
    Nothing -> fail "The backend did not acknowledge the update."

data InstallResult = SelectionChanged (Maybe Release) | WorkerStarted ProcessHandle

installRelease :: DesktopUpdate -> Release -> IO () -> Maybe FilePath -> FilePath -> IO InstallResult
installRelease update@(DesktopUpdate owner _ _ _ _) selected stop root config = do
  current <- check update
  if current == Just selected
    then WorkerStarted <$> startWorker owner selected stop root config
    else pure (SelectionChanged current)

startWorker :: Installation -> Release -> IO () -> Maybe FilePath -> FilePath -> IO ProcessHandle
startWorker owner selected@(Release _ contract url digest) stop root config = do
  stop
  let app = appPath owner
      state = statePath owner
      backend = runtimePath app </> ".build/bin" </> if os == "darwin" then "sapiens4" else "sapiens4.exe"
  when (contract == SameFormat) $ forM_ root $ \directory -> do
    (code,_,detail) <- readProcessWithExitCode backend ["--check-update-root",directory] ""
    unless (code == ExitSuccess) $ fail detail
  pid <- getCurrentPid
  attempt <- UUID.toString <$> UUID.nextRandom
  let workspace = state </> ("driver-" ++ attempt)
      arguments = [app,state,releaseVersion selected,url,maybe "" id root,config,show pid,contractName contract]
      macDriver = workspace </> "Update.app"
      powershell = "C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\powershell.exe"
      worker = workspace </> "apply.ps1"
  bracketOnError (createDirectory workspace) (const $ removeDirectoryRecursive workspace) $ \_ -> do
    command <- if os == "darwin" then do
      callProcess "/usr/bin/ditto" [app </> "Contents/Helpers/Update.app",macDriver]
      callProcess "/usr/bin/codesign" ["--verify","--deep","--strict",macDriver]
      pure (proc (macDriver </> "Contents/MacOS/Update") arguments)
     else do
      copyFile (app </> "apply.ps1") worker
      pure (proc powershell (["-NoProfile","-NonInteractive","-File",worker] ++ arguments ++ [digest]))
    beginMaintenance owner $ do
      let detail = state </> "outcome.txt"
      exists <- doesFileExist detail
      when exists (removeFile detail)
      let launch streams = do
            (_,_,_,child) <- createProcess command {std_in=streams,std_out=streams,std_err=streams,
                                                    close_fds=True,new_session=os=="darwin",create_new_console=False}
            pure child
      -- Sparkle may claim a closed standard descriptor as a guarded XPC port.
      if os == "darwin" then withFile "/dev/null" ReadWriteMode (launch . UseHandle)
                        else launch NoStream
