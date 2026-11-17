{-# LANGUAGE DeriveGeneric #-}
module MigrationBackup (withMigrationBackup, recoverBeforeOpen, checkNoPending) where

import Control.Exception
import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as ASCII
import qualified Data.ByteString.Lazy as BL
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import Installation (installed)
import MigrationFiles
import System.Directory
import System.Environment (getExecutablePath)
import System.FilePath
import System.IO
import System.IO.Error
import qualified Types

-- Relative path -> object kind, access metadata and content digest/link target.
type Inventory = M.Map FilePath (Char, String, String)
data Member = Member { folder :: FilePath, directoryId :: String, lockId :: String, corporaIdFile :: String }
  deriving (Eq, Generic, Show)
instance FromJSON Member
instance ToJSON Member
data Receipt = Receipt { operation :: String, source :: FilePath, participants :: [Member],
                         backend :: FilePath, binaryId :: String, original :: Inventory }
  deriving (Generic)
instance FromJSON Receipt
instance ToJSON Receipt

-- A child opened directly and its parent must discover the same receipt even
-- while their live registries cannot be read during restoration.
treeTop path
  | takeFileName parent == "corporas", validId (takeFileName path) = treeTop (takeDirectory parent)
  | otherwise = path
  where parent = takeDirectory path
validId s = Just s == (UUID.toString <$> UUID.fromString s)
area root = let top = treeTop root in takeDirectory top </> ("." ++ takeFileName top ++ ".sapiens-migrations")

missing e = if isDoesNotExistError e then Just () else Nothing
ensure b reason = unless b $ fail reason
noLink path = do
  resolved <- canonicalizePath path
  ensure (equalFilePath resolved path) ("Migration path was redirected: " ++ path)
  link <- pathIsSymbolicLink path
  ensure (not link) ("Migration path is a link: " ++ path)

digest path = withBinaryFile path ReadMode $ \h -> do
  bytes <- BL.hGetContents h
  let value = ASCII.unpack (Base64.encode (SHA.hashlazy bytes))
  _ <- evaluate (length value)
  pure value

member path = do
  let lock = path </> "host.lock"
      identity = path </> "corpora-identity.json"
  mapM_ noLink [path,lock,identity]
  Member path <$> fileIdentity path <*> fileIdentity lock <*> digest identity

lockPaths root paths = S.fromList [makeRelative root (p </> "host.lock") | p <- paths]

scan root locks = walk "" where
  walk rel = do
    let path = if null rel then root else root </> rel
    (kind, access) <- describeFile path
    content <- case kind of
      'f' -> digest path
      'l' -> getSymbolicLinkTarget path
      _ -> pure ""
    children <- if kind /= 'd' then pure [] else do
      names <- listDirectory path
      mapM walk [rel </> n | n <- names, S.notMember (rel </> n) locks]
    pure $ M.insert rel (kind,access,content) (M.unions children)

-- Parents precede descendants in the snapshot; directory access metadata is
-- applied after contents. Repeating an interrupted copy converges on the same tree.
copyTree from to saved live = do
  let ordered = sortOn (length . splitDirectories . fst) (M.toList saved)
      dirs = [p | (p,('d',_,_)) <- ordered]
      changed p = M.lookup p live /= M.lookup p saved
      remove p ('d',_,_) = removeDirectory (to </> p)
      remove p _ = removeFile (to </> p)
      sameKind p (kind,_,_) = case M.lookup p saved of
        Just (other,_,_) -> other == kind
        Nothing -> False
  forM_ (reverse $ sortOn (length . splitDirectories . fst) $ M.toList live) $ \(p,entry) ->
    unless (sameKind p entry || null p) $ remove p entry
  forM_ dirs $ \p -> createDirectoryIfMissing False (to </> p)
  forM_ ordered $ \(p,(kind,_,_)) -> when (kind /= 'd' && changed p) $ do
    when (maybe False (sameKind p) (M.lookup p live)) $ removeFile (to </> p)
    copyEntry False (from </> p) (to </> p)
    when (kind == 'f') $ syncEntry False (to </> p)
  forM_ (reverse dirs) $ \p -> do
    when (changed p) $ copyEntry True (from </> p) (to </> p)
    syncEntry True (to </> p)

ensureArea root = do
  let path = area root
  exists <- doesDirectoryExist path
  unless exists $ privateDirectory path `catchIOError` \e -> do
    raced <- doesDirectoryExist path
    unless raced (ioError e)
  noLink path
  syncEntry True (takeDirectory path)
  pure path

-- A receipt is discoverable only by the Pending filename. Terminal receipts
-- and uncommitted preparations never authorize a replay.
pending root = do
  let base = area root
  names <- tryJust missing (noLink base >> listDirectory base)
  case names of
    Left () -> pure []
    Right ids -> fmap concat $ forM (sort ids) $ \ident -> do
      ensure (validId ident) ("Unrecognized migration record: " ++ base </> ident)
      let dir = base </> ident
          path = dir </> "Pending"
      noLink dir
      found <- tryJust missing $ do
        noLink path
        bytes <- B.readFile path
        either (fail . (("Invalid migration record " ++ path ++ ": ") ++)) pure (eitherDecodeStrict bytes)
      case found of
        Left () -> pure []
        Right r -> do
          let paths = map folder (participants r)
              sub p = p == source r || childOf (source r) p
          ensure (operation r == ident && equalFilePath (area (source r)) base &&
                  source r `elem` paths && length paths == S.size (S.fromList paths) && all sub paths)
            ("Invalid migration participants in " ++ path)
          pure [(dir,r) | root == source r || root `elem` paths || childOf root (source r) || childOf (source r) root]
  where childOf parent path
          | path == parent = False
          | otherwise = case splitDirectories (makeRelative parent path) of
              "corporas":i:rest -> validId i && (null rest || childOf (parent </> "corporas" </> i) path)
              _ -> False

checkNoPending :: FilePath -> IO ()
checkNoPending root = do
  records <- pending root
  ensure (null records) ("Data recovery is pending for " ++ root ++ ". Reopen this Data Root.")

verified saved actual path = ensure (saved == actual) ("Migration copy verification failed: " ++ path)

closeRecord dir outcome = durableMove (dir </> "Pending") (dir </> outcome)
snapshot dir = dir </> "original"

-- Every supported writer checks Pending under the participant locks before work.
-- Recovery can replace the whole tree while the same installed binary owns it.
-- Binary rollback must restore data before admitting the previous application.
restoreOriginal dir r = do
  let root = source r
      paths = map folder (participants r)
      saved = original r
      snap = snapshot dir
      locks = lockPaths root paths
  noLink snap
  scan snap locks >>= \actual -> verified saved actual snap
  exe <- canonicalizePath =<< getExecutablePath
  idNow <- fileIdentity exe
  identities <- tryJust synchronous (mapM member paths)
  let owned = installed exe /= Nothing && exe == backend r && idNow == binaryId r &&
              either (const False) (== participants r) identities
  ensure owned ("Cannot establish exclusive recovery ownership of " ++ root ++ ". Original data verified at " ++ snap)
  live <- scan root locks
  copyTree snap root saved live
  scan root locks >>= \actual -> verified saved actual root
  closeRecord dir "RestoredClosed"

synchronous :: SomeException -> Maybe SomeException
synchronous e = case fromException e :: Maybe SomeAsyncException of
  Just _ -> Nothing
  Nothing -> Just e

recoverBeforeOpen :: FilePath -> ([FilePath] -> IO () -> IO ()) -> IO ()
recoverBeforeOpen root lock = do
  records <- pending root
  forM_ records $ \(dir,seen) -> do
    -- A replaced node must not cause lock acquisition to create files in a
    -- different tree. The same identities are checked again under the locks.
    let paths = map folder (participants seen)
    matches <- tryJust synchronous (mapM member paths)
    unless (either (const False) (== participants seen) matches) $
      fail ("Recovery participants changed at " ++ source seen ++ ". Original copy: " ++ snapshot dir)
    lock paths $ do
      active <- pending root
      forM_ [r | (d,r) <- active, d == dir] $ \r -> do
        restoreOriginal dir r
        fail ("Interrupted migration restored at " ++ source r ++ ". Reopen to retry. Backup: " ++ snapshot dir)

withMigrationBackup :: FilePath -> [FilePath] -> IO () -> IO ()
-- The caller holds all participant locks and checked Pending under those locks.
withMigrationBackup root paths convert = do
  base <- ensureArea root
  ident <- UUID.toString <$> UUID.nextRandom
  let dir = base </> ident
      snap = snapshot dir
      locks = lockPaths root paths
  privateDirectory dir
  syncEntry True base
  members <- mapM member paths
  exe <- canonicalizePath =<< getExecutablePath
  exeId <- fileIdentity exe
  saved <- scan root locks
  privateDirectory snap
  copyTree root snap saved M.empty
  scan snap locks >>= \actual -> verified saved actual snap
  scan root locks >>= \actual -> verified saved actual root
  let receipt = Receipt ident root members exe exeId saved
  Types.writeJSON (dir </> "prepared") receipt
  durableMove (dir </> "prepared") (dir </> "Pending")
  outcome <- tryJust synchronous convert
  case outcome of
    Right () -> closeRecord dir "Finalized"
    Left cause -> do
      recovered <- tryJust synchronous (restoreOriginal dir receipt)
      let detail = either (("Recovery pending: " ++) . displayException) (const "Original data restored. Reopen to retry.") recovered
      fail (displayException cause ++ " " ++ detail ++ " Backup: " ++ snap)
