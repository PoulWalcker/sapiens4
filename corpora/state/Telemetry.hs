module Telemetry
  ( CorporaRef
  , Emit
  , EventIntent (..)
  , RequestKind (..)
  , corporaDirectory
  , corporaRef
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath (isAbsolute)

newtype CorporaRef = CorporaRef FilePath

corporaRef :: FilePath -> Maybe CorporaRef
corporaRef path
  | isAbsolute path && BS.length (TE.encodeUtf8 (T.pack path)) <= 4096 = Just (CorporaRef path)
  | otherwise = Nothing

corporaDirectory :: CorporaRef -> FilePath
corporaDirectory (CorporaRef path) = path

data RequestKind = AgentMessage | GroupMessage | GroupTaskRun

data EventIntent
  = IAppOpened
  | ICorporaCreated CorporaRef
  | IRequestAccepted CorporaRef RequestKind
  | IRemotePaired CorporaRef

type Emit = EventIntent -> IO ()
