{-# LANGUAGE CPP #-}
module Main where

import Types
import qualified RuntimePaths as Paths
import HostServer
import Assets
import BuildUI
import Computer
import Server
import Setup
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.String
import Data.Maybe
import GHC.IO.Encoding (setLocaleEncoding)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Wai.Handler.Warp hiding (run)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Info
import System.IO
import System.Process
#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif

main :: IO ()
main = run `catches` [Handler (\(code::ExitCode) -> throwIO code), Handler (\(e::SomeException) -> hPutStrLn stderr (displayException e) >> exitFailure)]
run :: IO ()
run = do
 setLocaleEncoding utf8
 mapM_ (`hSetEncoding` utf8) [stdin,stdout,stderr]
 hSetBuffering stdout LineBuffering
 args <- getArgs
 root <- repository
 setEnv "SAPIENS_REPOSITORY" root
 case args of
  ["build-ui"] -> buildUI root
  ["control",config,payload] -> jsonCommand $ do
   value <- either (failure 400 . T.pack) pure (eitherDecode (BL.fromStrict (TE.encodeUtf8 (T.pack payload))))
   require (case value of Object _ -> True; _ -> False) 400 "Expected a JSON object"
   (code,result) <- control config value
   BL.putStrLn (encode result)
   pure (if code<200 || code>=300 then ExitFailure 1 else ExitSuccess)
  "control":_ -> jsonCommand (failure 400 "Usage: sapiens4 control CONFIG JSON")
  "computer":rest -> jsonCommand (computerMain root rest)
  "relay":rest -> relayMain root (normalized rest)
  _ -> hostMain root (normalized args)
jsonCommand :: IO ExitCode -> IO ()
jsonCommand action = do
 code <- action `catch` (\(e::SomeException) -> case fromException e :: Maybe AsyncException of
  Just _ -> throwIO e
  Nothing -> do
   let message=case fromException e of Just (APIError _ value)->value; Nothing->T.pack (displayException e)
   BL.putStrLn (encode (obj [("error",str message)]))
   pure (ExitFailure 1))
 exitWith code
repository :: IO FilePath
repository = do
 supplied <- lookupEnv "SAPIENS_REPOSITORY"
 case supplied of
  Just path -> canonicalizePath path
  Nothing -> do
   cwd <- getCurrentDirectory
   here <- doesDirectoryExist (cwd</>"app")
   executable <- getExecutablePath
   let runtime=takeDirectory (takeDirectory (takeDirectory executable))
   assets <- doesFileExist (runtime</>"app/shell/index.html")
   -- A workspace may also contain an app directory. Prefer the executable's
   -- own assets so generated console commands remain relocatable from any cwd.
   canonicalizePath (if assets then runtime else if here then cwd else runtime)
hostMain :: FilePath -> [String] -> IO ()
hostMain root args = do
 when ("--help" `elem` args || "-h" `elem` args) $ putStrLn "Usage: sapiens4 [--port PORT] [--data-dir DIRECTORY] [--open]" >> exitSuccess
 settings <- validated (hostOptions root args)
 source <- doesFileExist (root</>"corpora/Main.hs")
 defaultData <- if source then
  lookupEnv "SAPIENS_DATA_DIR" >>= maybe (pure (root</>".sapiens4")) (\path -> if null path then fail "SAPIENS_DATA_DIR must not be empty" else makeAbsolute path)
  else Paths.defaultDataDirectory root
 let directory=option "--data-dir" defaultData args
 void (asset root "/")
 void (asset root "/workspace/app.js")
 bracket (startHost directory (requestedPort settings) False) hostStop $ \host -> do
  let url="http://127.0.0.1:"<>show (hostPort host)<>"/workspace/"
  putStrLn ("Sapiens4: "<>url<>"\nUI database: "<>directory</>"corpora.sqlite3")
  finished <- newEmptyMVar
  let shutdown=void (tryPutMVar finished ())
  installShutdown shutdown
  when (desktopMode settings) (void (forkIO ((void getLine `catch` \(_::IOException)->pure ()) >> shutdown)))
  when (openWindow settings) (void (createProcess (proc (if os=="darwin" then "open" else if os=="mingw32" then "explorer" else "xdg-open") [url])))
  takeMVar finished

data HostOptions = HostOptions
 { requestedPort :: Int, hostDirectory :: FilePath, desktopMode :: Bool, openWindow :: Bool }

hostOptions :: FilePath -> [String] -> Validation HostOptions
hostOptions root args = do
 validateOptions ["--port","--data-dir"] ["--open","--desktop"] args
 check (not (null (option "--data-dir" "." args))) 400 "Data directory must not be empty"
 port <- parseIntegerOption "--port" "4174" args
 check (port>=0 && port<=65535) 400 "Port must be between 0 and 65535"
 pure (HostOptions port (option "--data-dir" (root</>".sapiens4") args) ("--desktop" `elem` args) ("--open" `elem` args))

parseIntegerOption :: String -> String -> [String] -> Validation Int
parseIntegerOption key def args = case reads (option key def args) of
 [(port,"")] -> Right port
 _ -> Left (APIError 400 ("Invalid "<>T.pack key))

relayMain :: FilePath -> [String] -> IO ()
relayMain root args = do
 when ("--help" `elem` args || "-h" `elem` args) $ putStrLn "Usage: sapiens4 relay --origin ORIGIN [--bind ADDRESS] [--port PORT] [--db FILE] [--connection-file FILE] [--beta]" >> exitSuccess
 validated (validateOptions ["--origin","--bind","--port","--db","--connection-file"] ["--beta"] args)
 bind <- option "--bind" <$> (fromMaybe "127.0.0.1" <$> lookupEnv "SAPIENS_RELAY_BIND") <*> pure args
 portDefault <- fromMaybe "4180" <$> lookupEnv "PORT"
 port <- validated (parseIntegerOption "--port" portDefault args)
 dbDefault <- fromMaybe ".sapiens4/relay.sqlite3" <$> lookupEnv "SAPIENS_RELAY_DB"
 urlEnv <- lookupEnv "SAPIENS_RELAY_ORIGIN"
 railway <- lookupEnv "RAILWAY_PUBLIC_DOMAIN"
 let database=option "--db" dbDefault args
     suppliedOrigin = listToMaybe [value | (key,value)<-zip args (drop 1 args),key=="--origin"]
     public = suppliedOrigin `mplus` urlEnv `mplus` (("https://"<>) <$> railway)
     connectionPath=option "--connection-file" (replaceExtension database "connection.json") args
 publicOrigin <- maybe (failure 400 "Relay --origin is required") pure (public >>= \value -> if null value then Nothing else Just value)
 railwayEnv <- lookupEnv "RAILWAY_ENVIRONMENT_ID"
 forM_ railwayEnv $ \_ -> do
  mount <- lookupEnv "RAILWAY_VOLUME_MOUNT_PATH"
  dbPath <- makeAbsolute database
  mountPath <- traverse makeAbsolute mount
  require (maybe False (\p -> addTrailingPathSeparator p `isPrefixOfPath` dbPath) mountPath) 400 "Attach a persistent Railway volume at /data before deploying the relay"
 admin <- maybe "" T.pack <$> lookupEnv "SAPIENS_RELAY_ADMIN_TOKEN"
 config <- provision connectionPath (T.pack publicOrigin) admin
 betaEnv <- lookupEnv "SAPIENS_RELAY_BETA"
 relay <- newRelay root database (at config "token") (at config "relay") ("--beta" `elem` args || betaEnv==Just "1")
 let settings=setPort port $ setHost (fromStringHost bind) $ setGracefulCloseTimeout1 1000 $ setInstallShutdownHandler installShutdown $ setBeforeMainLoop (putStrLn ("Relay connection file: "<>connectionPath<>"\nRemote relay ready.")) defaultSettings
 runSettings settings (relayApplication relay)
 where isPrefixOfPath prefix path=take (length prefix) path==prefix
       fromStringHost = Data.String.fromString
installShutdown :: IO () -> IO ()
installShutdown action = do
#ifndef mingw32_HOST_OS
 void (installHandler sigTERM (Catch action) Nothing)
 void (installHandler sigINT (Catch action) Nothing)
#else
 pure ()
#endif

normalized :: [String] -> [String]
normalized = concatMap (\arg -> case break (=='=') arg of (key,'=':value) | take 2 key=="--" -> [key,value]; _ -> [arg])
validateOptions :: [String] -> [String] -> [String] -> Validation ()
validateOptions values flags = check
 where
 check [] = pure ()
 check (key:rest) | key `elem` flags = check rest
 check (key:value:rest) | key `elem` values = check rest
 check (key:_) = Left (APIError 400 ("Unknown option or missing argument: "<>T.pack key))
