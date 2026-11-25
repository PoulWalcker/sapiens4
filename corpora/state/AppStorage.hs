-- Application/API serialization over scalar KV. This module stores no record
-- roots, path indexes or document values. The database API is Append4.KV.
module AppStorage (Store,Options(..),HistoryPolicy(..),defaultOptions,openStore,openStoreWith,closeStore,transaction,afterCommit,getValue,putValue,deleteValue,values,recordsAt,rangeValues,getOfflineValue) where
import Append4.KV (Store,Options(..),HistoryPolicy(..),defaultOptions,openStore,openStoreWith,closeStore,transaction,afterCommit)
import qualified Append4.KV as K
import qualified Append4.Key as Key
import qualified Append4.Flat as F
import Control.Monad
import Data.Aeson hiding (Options,defaultOptions)
import qualified Data.Map.Strict as M
import Data.Text(Text)
import qualified Data.Text as T

prefixKey "" = ""
prefixKey p = if "/" `T.isSuffixOf` p then Key.rootKey(T.dropEnd 1 p)<>"/" else Key.rootKey p
fieldRows store key = do
 let root=Key.rootKey key
 exact<-K.getValue store root
 rest<-K.values store (root<>"/")
 pure(M.fromList(maybe [] (\v->[(root,v)]) exact++rest))
getValue :: Store -> Text -> IO (Maybe Value)
getValue store key = transaction store (fieldRows store key >>= Key.unflatten key)
putValue :: Store -> Text -> Value -> IO ()
putValue store key value = transaction store $ do
 previous<-fieldRows store key
 let next=Key.flatten key value
 mapM_(K.deleteValue store)(M.keys(previous `M.difference` next))
 forM_(M.toList next)$ \(k,v)->when(M.lookup k previous/=Just v)(K.putValue store k v)
deleteValue :: Store -> Text -> IO ()
deleteValue store key = transaction store $ fieldRows store key >>= mapM_(K.deleteValue store).M.keys
values :: Store -> Text -> IO [(Text,Value)]
values = recordsAt 1
recordsAt :: Int -> Store -> Text -> IO [(Text,Value)]
recordsAt depth store prefix = transaction store $ do
 let start=prefixKey prefix
 rows<-K.values store start
 let roots=M.keys(M.fromList [(start<>T.intercalate "/" (take depth(T.splitOn "/"(T.drop(T.length start)k))),())|(k,_)<-rows])
 forM roots $ \root -> do
  let key=T.intercalate "/"(map Key.unescape(T.splitOn "/" root))
  value<-Key.unflatten key(M.fromList rows) >>= maybe(fail "Missing application fields") pure
  pure(key,value)
rangeValues :: Store -> Text -> Text -> Int -> IO [(Text,Value)]
rangeValues store lower upper limit = do
 rows<-K.rangeValues store (prefixKey lower)(prefixKey upper)limit
 forM rows $ \(key,atom)->do
  value<-F.unflatten(M.singleton [] atom)
  pure(key,value)
getOfflineValue :: FilePath -> Text -> IO (Maybe Value)
getOfflineValue path key = do
 rows<-K.offlineValues path (Key.rootKey key)
 Key.unflatten key(M.fromList rows)
