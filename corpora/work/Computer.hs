module Computer where

import Types
import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX
import qualified Network.HTTP.Client as H
import Network.HTTP.Types.Status
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Info
import System.IO
import System.Process
import System.Timeout

control :: FilePath -> Value -> IO (Int,Value)
control path payload = do
 config <- readJSON path empty
 run <- lookupEnv "SAPIENS_AGENCY_RUN"
 let body = maybe payload (\i -> put "agencyRun" (str (T.pack i)) payload) run
 manager <- H.newManager H.defaultManagerSettings
 req <- H.parseRequest (T.unpack (at config "url"))
 result <- H.httpLbs req {H.method="POST",H.requestHeaders=[("Content-Type","application/json"),("X-Sapiens-Local","1")],H.requestBody=H.RequestBodyLBS (encode body),H.responseTimeout=H.responseTimeoutMicro 20000000,H.redirectCount=0,H.checkResponse = \_ _ -> pure ()} manager
 value <- either (failure 500 . T.pack) pure (eitherDecode (H.responseBody result))
 pure (statusCode (H.responseStatus result),value)
blindlyBinary :: FilePath -> IO FilePath
blindlyBinary root = fromMaybe (root</>if os=="mingw32" then "submodules/blindly4/.build/windows/blindly4.exe" else "submodules/blindly4/.build/release/blindly4") <$> lookupEnv "SAPIENS_BLINDLY_BINARY"
invoke :: FilePath -> [String] -> Int -> IO (ExitCode,String,String)
invoke binary args duration = timeout (duration*1000000) (readCreateProcessWithExitCode (proc binary args) {use_process_jobs=True,detach_console=os=="mingw32"} "") >>= maybe (failure 504 "Computer operation timed out") pure
compact :: Value -> Value
compact = obj . filter (\(k,v) -> k `elem` ["path","pid","role","subrole","title","description","value","identifier","enabled","focused","selected","actions"] && v `notElem` [Null,str "",arr []]) . pairs
flatten :: Text -> Value -> [Value]
flatten path node = put "path" (str path) (compact node) : concat [flatten (if T.null path then T.pack (show i) else path<>"."<>T.pack (show i)) child | (i,child)<-zip [0::Int ..] (items node "children")]
bounded :: Text -> Int -> Text
bounded input maximumSize = case parseJSONText input of
 Null -> excerpt input
 value | get value "tree"/=Null -> jsonText (trimRows "nodes" (obj [("pid",get (get value "tree") "pid"),("nodes",arr (flatten "" (get value "tree"))),("truncated",fallback (get value "truncated") (Bool False)),("coverage",str "Bounded AX tree; depth and node limits apply")]))
 value | has value "matches" -> jsonText (trimRows "matches" (put "matches" (arr (map compact (items value "matches"))) value))
 value -> excerpt (jsonText (if has value "role" then compact value else value))
 where
 excerpt t = if T.length t <= maximumSize then t else jsonText (obj [("truncated",Bool True),("output_excerpt",str (T.take maximumSize t)),("hint",str "Partial observation only. Use find or inspect for the relevant element; do not repeat this full dump.")])
 trimRows field v = if T.length (jsonText v)<=maximumSize || null (items v field) then v else trimRows field (put "truncated" (Bool True) (put field (arr (init (items v field))) v))
option :: String -> String -> [String] -> String
option key def args = case dropWhile (/=key) args of _:value:_ -> value; _ -> def
integerOption :: String -> Int -> [String] -> IO Int
integerOption key def args = case reads (option key (show def) args) of [(n,"")] -> pure n; _ -> failure 400 (T.pack ("Invalid "<>key))
readComputer :: FilePath -> [String] -> Int -> IO Value
readComputer binary supplied maximumSize = do
 let args=concatMap (\arg -> case break (=='=') arg of (key,'=':value) | take 2 key=="--" -> [key,value]; _->[arg]) supplied
     validate []=pure ()
     validate (key:_:rest) | key `elem` ["--pid","--path","--depth","--snapshot","--offset"]=validate rest
     validate (key:_)=failure 400 ("Unknown read option or missing argument: "<>T.pack key)
 validate args
 directory <- getCurrentDirectory
 let cache=directory</>".computer-reads"; identity=option "--snapshot" "" args; path=T.pack (option "--path" "" args)
 createDirectoryIfMissing True cache
 time <- getCurrentTime
 files <- map (cache</>) . filter ((==".json").takeExtension) <$> listDirectory cache
 forM_ files $ \file -> do
  modified <- getModificationTime file
  when (diffUTCTime time modified >90) (removeFile file)
 offset <- integerOption "--offset" 0 args
 snapshot <- if not (null identity) then do
  require (length identity==32 && all (\c -> c `elem` ("0123456789abcdef"::String)) identity) 400 "Invalid snapshot"
  let saved=cache</>identity<.>"json"
  exists <- doesFileExist saved
  require exists 400 "Snapshot expired; read the current state again"
  readJSON saved empty
  else do
   pid <- integerOption "--pid" 0 args
   depth <- integerOption "--depth" 12 args
   require (pid>0 && (T.null path || all (\p -> not (T.null p) && T.all isDigit p) (T.splitOn "." path))) 400 "Read needs a positive --pid and an observed numeric --path"
   require (depth>=1 && depth<=30 && offset==0) 400 "Depth must be 1–30; paginate using --snapshot"
   let totalDepth=(if T.null path then 0 else length (T.splitOn "." path))+depth
   require (totalDepth<=64) 400 "Total depth exceeds 64"
   (code,out,err) <- invoke binary ["tree","--pid",show pid,"--depth",show totalDepth,"--max-nodes","5000"] 30
   require (code==ExitSuccess) 400 ("Blindly read failed: "<>T.take 300 (T.pack err))
   let value=parseJSONText (T.pack out)
       allRows=filter (\v -> T.null path || at v "path"==path || (path<>".") `T.isPrefixOf` at v "path") (flatten "" (get value "tree"))
       rows=filter (\v -> at v "path"==path || any (\k -> get v k `notElem` [Null,str "",arr []]) ["title","description","value","identifier","actions"]) allRows
   require (case get value "tree" of Object _->True; _->False) 400 "Blindly returned an invalid accessibility tree"
   require (not (null allRows)) 400 "Target missing from bounded tree; rediscover it. Coverage is incomplete."
   ident <- newId
   seconds <- realToFrac <$> getPOSIXTime :: IO Double
   let snapshot=obj [("snapshot",str ident),("observed_at",toJSON seconds),("pid",num pid),("path",str path),("nodes",arr rows),("truncated",fallback (get value "truncated") (Bool False)),("coverage",str "Bounded accessibility snapshot, not proof of latest/all messages; depth and 5000-node limits apply.")]
   remaining <- map (cache</>) . filter ((==".json").takeExtension) <$> listDirectory cache
   dated <- mapM (\p -> (,p) <$> getModificationTime p) remaining
   mapM_ (removeFile . snd) (drop 4 (reverse (sortOn fst dated)))
   writeJSON (cache</>T.unpack ident<.>"json") snapshot
   pure snapshot
 validated (snapshotPage maximumSize offset snapshot)

snapshotPage :: Int -> Int -> Value -> Validation Value
snapshotPage maximumSize offset snapshot = do
 let rows=items snapshot "nodes"
 check (offset>=0 && offset<=length rows) 400 "Offset outside snapshot"
 let base=merge (remove ["nodes"] snapshot) (obj [("nodes",arr []),("offset",num offset),("total",num (length rows)),("next_offset",Null)])
     add acc []=acc
     add acc (row:rest)=let updated=put "nodes" (arr (items acc "nodes"++[row])) acc in
      if T.length (jsonText updated)<=maximumSize then add updated rest else
       if not (null (items acc "nodes")) then acc else put "nodes" (arr [put "text_truncated" (Bool True) (obj [(k,case v of String s | k/="path" -> str (T.take (max 80 (maximumSize `div` 8)) s); _ -> v) | (k,v)<-pairs row])]) acc
     result=add base (drop offset rows); end=offset+length (items result "nodes")
 pure (put "next_offset" (if end<length rows then num end else Null) result)
computerMain :: FilePath -> [String] -> IO ExitCode
computerMain root args = do
 let valid=case args of
      "blindly":_->True
      "read":_->True
      ["launch",name]->not (T.null (T.strip (T.pack name))) && head name/='-' && length name<=120
      _->False
 require valid 400 "Usage: sapiens4 computer launch Application"
 when (case args of "launch":_->True; _->False) $ require (os `elem` ["darwin","mingw32"]) 400 "App launch requires macOS or Windows"
 managed <- doesFileExist "host-control.json"
 when managed $ do
  (code,result) <- control "host-control.json" (obj [("op",str "computer_acquire")])
  require (code>=200 && code<300) code (at result "error")
 writeFile ".computer-used" ""
 binary <- blindlyBinary root
 case args of
  "blindly":rest -> do
   (code,out,err) <- invoke binary rest 30
   putStrLn (T.unpack (bounded (T.pack out) 48000))
   unless (null err) (hPutStrLn stderr (take 2000 err))
   pure code
  "read":rest -> readComputer binary rest 48000 >>= BL.putStrLn . encode >> pure ExitSuccess
  ["launch",name] | not (null name) && head name/='-' && length name<=120 -> do
   if os=="mingw32" then do
    executable <- findExecutable name >>= maybe (failure 400 "Use a Windows .exe path or executable name, such as notepad.exe") pure
    require (map toLower (takeExtension executable)==".exe") 400 "Use a Windows .exe path or executable name, such as notepad.exe"
    (_,_,_,ph) <- createProcess (proc executable [])
    pid <- getPid ph
    BL.putStrLn (encode (obj [("launched",Bool True),("app",str (T.pack name)),("pid",maybe Null (num . fromIntegral) pid),("error",Null)]))
    pure ExitSuccess
   else do
    require (os=="darwin") 400 "App launch requires macOS or Windows"
    (code,_,err) <- invoke "/usr/bin/open" ["-a",name] 15
    BL.putStrLn (encode (obj [("launched",Bool (code==ExitSuccess)),("app",str (T.pack name)),("error",if null err then Null else str (T.strip (T.pack err)))]))
    pure code
  _ -> failure 400 "Usage: sapiens4 computer launch Application"

data Foreground = Foreground FilePath (Maybe UTCTime) (Maybe Text)
captureForeground :: FilePath -> FilePath -> IO Foreground
captureForeground root workdir = do
 let marker=workdir</>".computer-used"
 before <- try (getModificationTime marker)
 managed <- doesFileExist (workdir</>"host-control.json")
 captured <- try $ if not managed then pure Nothing else if os=="darwin" then do
   (code,out,_) <- invoke "osascript" ["-l","JavaScript","-e","ObjC.import('AppKit'); $.NSWorkspace.sharedWorkspace.frontmostApplication.bundleIdentifier.js"] 5
   let bundle=T.strip (T.pack out)
   pure (if code==ExitSuccess && bundle `elem` ["com.sapiens4.desktop","com.openai.codex","com.google.Chrome","com.apple.Safari","com.microsoft.edgemac","org.mozilla.firefox"] then Just bundle else Nothing)
  else if os=="mingw32" then do
   expected <- lookupEnv "SAPIENS_DESKTOP_PID"
   binary <- blindlyBinary root
   (_,out,_) <- invoke binary ["apps"] 5
   pure (listToMaybe [T.pack (show (int (get app "pid"))) | app<-items (parseJSONText (T.pack out)) "apps",boolean (get app "frontmost"), Just (show (int (get app "pid")))==expected])
  else pure Nothing
 pure (Foreground marker (either (const Nothing) Just (before::Either IOException UTCTime)) (either (const Nothing) id (captured::Either SomeException (Maybe Text))))
restoreForeground :: FilePath -> Foreground -> IO (Maybe Text)
restoreForeground root (Foreground marker before target) = (do
 exists <- doesFileExist marker
 after <- if exists then Just <$> getModificationTime marker else pure Nothing
 if after==before || after==Nothing then pure Nothing else case target of
  Nothing -> pure Nothing
  Just value -> do
   binary <- if os=="mingw32" then blindlyBinary root else pure "/usr/bin/open"
   (code,_,_) <- invoke binary (if os=="mingw32" then ["activate","--pid",T.unpack value] else ["-b",T.unpack value]) 5
   pure (if code==ExitSuccess then Nothing else Just "Could not bring CORPORA back to the foreground.")) `catch` \(_::SomeException) -> pure (Just "Could not bring CORPORA back to the foreground.")
