{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Browser (Browser, newBrowser, syncBrowser, positionGuest, closeBrowser, focusBrowser, field, string, number, boolean, palette, openBrowser) where

import Control.Exception
import Control.Monad
import Data.Aeson hiding (pairs)
import Data.Bits
import Data.IORef
import Data.Int
import Data.Maybe
import Foreign hiding (void)
import Numeric
import Domain (BrowserCommand(..), BrowserAction(..), BrowserTransition(..), browserTransition)
import Host
import Native
import qualified Navigation as Navigation
import System.FilePath
import qualified Crypto.Hash.SHA256 as SHA
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

data Tab = Tab { tabOwner :: String, tabId :: String, tabView :: View, tabSequence :: IORef String, tabLoading :: IORef Bool, tabError :: IORef String, tabFocused :: IORef Bool, tabAlive :: IORef Bool }
data Browser = Browser { arena :: Arena, window :: Window, shell :: View, host :: Host, tabs :: IORef (M.Map String Tab), latest :: IORef Value, revision :: IORef Int, syncing :: IORef Bool, disposed :: IORef Bool, shellTop :: IORef Int, guestScript :: String }

field :: String -> Value -> Value
field name (Object fields) = fromMaybe Null $ KM.lookup (K.fromString name) fields
field _ _ = Null

string :: Value -> String
string (String value) = T.unpack value
string _ = ""

boolean :: Value -> Bool
boolean (Bool value) = value
boolean _ = False

number :: Value -> Double
number (Number value) = realToFrac value
number _ = 0

pairs :: Value -> [(String, Value)]
pairs (Object values) = [(K.toString key, value) | (key, value) <- KM.toList values]
pairs _ = []

array :: Value -> [Value]
array (Array values) = V.toList values
array _ = []

palette :: String -> String -> Value -> String
palette name fallback message = let value = string $ field name message in
  case value of
    '#' : digits | length digits `elem` [3, 6], all (`elem` ("0123456789abcdefABCDEF" :: String)) digits ->
      '#' : if length digits == 3 then concatMap (replicate 2) digits else digits
    _ -> fallback

colorWord :: String -> Int32
colorWord value = case readHex (drop 1 value) of
  [(rgb, "")] -> fromIntegral ((0xff :: Word32) .|. ((rgb `shiftR` 16 .&. 255) `shiftL` 8) .|. ((rgb `shiftR` 8 .&. 255) `shiftL` 16) .|. ((rgb .&. 255) `shiftL` 24))
  _ -> -1

json :: ToJSON a => a -> String
json = T.unpack . TE.decodeUtf8 . BL.toStrict . encode

openBrowser :: Browser -> String -> IO ()
openBrowser browser url = do
  message <- readIORef (latest browser)
  report browser $ object ["owner" .= field "owner" message, "open" .= url]

newBrowser :: Arena -> Window -> View -> Host -> IORef Int -> IO Browser
newBrowser memory hwnd view owner top = do
  script <- T.unpack <$> TIO.readFile (runtimeDirectory owner </> ".build/web/guest.js")
  Browser memory hwnd view owner <$> newIORef M.empty <*> newIORef Null <*> newIORef 0 <*> newIORef False <*> newIORef False <*> pure top <*> pure script

focusBrowser :: Browser -> Window -> IO ()
focusBrowser browser previous = do
  guests <- map tabView . M.elems <$> readIORef (tabs browser)
  matches <- filterM (\view -> if container view == previous then pure True else (/= 0) <$> childWindow (container view) previous) (shell browser : guests)
  callInt (controller $ fromMaybe (shell browser) $ listToMaybe matches) 12 0

report :: Browser -> Value -> IO ()
report browser value = do
  dead <- readIORef (disposed browser)
  unless dead $ execute (arena browser) (shell browser) ("window.sapiensBrowserEvent?.(" ++ json value ++ ")") (const $ pure ())

observe :: Browser -> Tab -> IO ()
observe browser tab = do
  alive <- readIORef (tabAlive tab)
  sequenceId <- readIORef (tabSequence tab)
  when (alive && not (null sequenceId)) $ do
    let web = core (tabView tab)
    url <- getString web 4
    title <- getString web 48
    back <- (/= 0) <$> (getValue web 38 :: IO Int32)
    forward <- (/= 0) <$> (getValue web 39 :: IO Int32)
    loading <- readIORef (tabLoading tab)
    errorText <- readIORef (tabError tab)
    report browser $ object ["owner" .= tabOwner tab, "id" .= tabId tab, "seq" .= sequenceId, "url" .= url, "title" .= title, "loading" .= loading, "error" .= errorText, "can_back" .= back, "can_forward" .= forward]

applyTheme :: Browser -> Tab -> IO ()
applyTheme browser tab = do
  settings <- readIORef (latest browser)
  let dark = boolean $ field "dark" settings
      background = palette "background" (if dark then "#212121" else "#ffffff") settings
      foreground = palette "foreground" (if dark then "#ececec" else "#202020") settings
  bracket (query (core $ tabView tab) "{F75F09A8-667E-4983-88D6-C8773F315E84}") release $ \web ->
    bracket (getValue web 105) release $ \profile -> callInt profile 9 (if dark then 2 else 1)
  let value = object ["dark" .= dark, "background" .= background, "foreground" .= foreground]
  execute (arena browser) (tabView tab) (guestScript browser ++ ";window.sapiensDocumentTheme(" ++ json value ++ ")") $ \managed -> do
    alive <- readIORef (tabAlive tab)
    when alive $ bracket (query (controller $ tabView tab) "{C979903E-D4CA-4228-92EB-47EE3FA96EAB}") release $ \ctrl ->
      callInt ctrl 27 $ colorWord (if managed == "true" then background else "#ffffff")

createTab :: Browser -> String -> String -> (Tab -> IO ()) -> IO ()
createTab browser owner identity done = do
  let profile = concatMap (\byte -> let hex = showHex byte "" in replicate (2 - length hex) '0' ++ hex) $ B.unpack $ SHA.hash $ TE.encodeUtf8 $ T.pack owner
  newView (arena browser) (window browser) (dataDirectory (host browser) </> "WebView2/Guests" </> profile) $ \result -> do
    view <- either (ioError . userError) pure result
    visible view False
    dead <- readIORef (disposed browser)
    if dead then closeView view else do
      tab <- Tab owner identity view <$> newIORef "" <*> newIORef False <*> newIORef "" <*> newIORef False <*> newIORef True
      settings <- getValue (core view) 3
      callInt settings 6 0
      callInt settings 16 0
      release settings
      let on index iid = onEvent (arena browser) (core view) index iid
          notify _ = observe browser tab
      on 7 "{9ADBE429-F36D-432B-9DDC-F8881FBD76E3}" $ \args -> do
        url <- getString args 3
        base <- readIORef (origin $ host browser)
        let permitted = Navigation.guest base url
        unless permitted $ callInt args 8 1
        writeIORef (tabLoading tab) permitted
        writeIORef (tabError tab) $ if permitted then "" else "Navigation blocked: use an HTTP(S) URL or a local file."
        observe browser tab
      on 15 "{D33A35BF-1C49-4F98-93AB-006E0533FE1C}" $ \args -> do
        writeIORef (tabLoading tab) False
        success <- getValue args 3 :: IO Int32
        when (success == 0) $ (getValue args 4 :: IO Int32) >>= writeIORef (tabError tab) . webError
        observe browser tab
      on 46 "{F5F2B923-953E-4042-9F95-F3A118E1AFD4}" notify
      on 13 "{C79A420C-EFD9-4058-9295-3E8B4BCAB645}" notify
      on 44 "{D4C185FE-C81C-4989-97AF-2D3FA7AB5651}" $ \args -> do
        callInt args 6 1
        url <- getString args 3
        base <- readIORef (origin $ host browser)
        when (Navigation.guest base url) $ report browser $ object ["owner" .= owner, "open" .= url]
      on 25 "{79E0AEA4-990B-42D9-AA1D-0FCC2E5BC7F1}" $ \args -> do
        kind <- getValue args 3 :: IO Int32
        writeIORef (tabLoading tab) False
        writeIORef (tabError tab) ("Browser process failed: " ++ show kind)
        observe browser tab
      bracket (query (core view) "{9E8F0CF8-E670-4B5E-B2BC-73E061E3184C}") release $ \web ->
        onEvent (arena browser) web 64 "{4BAC7E9C-199E-49ED-87ED-249303ACF019}" $ \_ -> applyTheme browser tab
      onEvent (arena browser) (controller view) 15 "{05EA24BD-6452-4926-9014-4B82B498135D}" $ \_ -> writeIORef (tabFocused tab) True
      onEvent (arena browser) (controller view) 17 "{05EA24BD-6452-4926-9014-4B82B498135D}" $ \_ -> writeIORef (tabFocused tab) False
      modifyIORef' (tabs browser) (M.insert (owner ++ "/" ++ identity) tab)
      applyTheme browser tab
      done tab

webError :: Int32 -> String
webError code = fromMaybe (show code) $ lookup code $ zip [0..] ["Unknown", "CertificateCommonNameIsIncorrect", "CertificateExpired", "ClientCertificateContainsErrors", "CertificateRevoked", "CertificateIsInvalid", "ServerUnreachable", "Timeout", "ErrorHttpInvalidServerResponse", "ConnectionAborted", "ConnectionReset", "Disconnected", "CannotConnect", "HostNameNotResolved", "OperationCanceled", "RedirectFailed", "UnexpectedError", "ValidAuthenticationCredentialsRequired", "ValidProxyAuthenticationRequired"]

syncBrowser :: Browser -> Value -> IO ()
syncBrowser browser message = do
  dead <- readIORef (disposed browser)
  unless dead $ if boolean (field "activate" message) then do
    void $ showWindow (window browser) 9
    void $ foregroundWindow (window browser)
  else if boolean (field "pickFile" message) then do
    selected <- pickFile (window browser) (string $ field "directory" message)
    forM_ selected $ \file -> report browser $ object ["owner" .= field "owner" message, "file" .= file]
  else do
    writeIORef (latest browser) message
    modifyIORef' (revision browser) (+1)
    positionGuest browser `catch` reportException
    busy <- readIORef (syncing browser)
    unless busy $ writeIORef (syncing browser) True >> applyLatest browser

applyLatest :: Browser -> IO ()
applyLatest browser = do
  message <- readIORef (latest browser)
  version <- readIORef (revision browser)
  let desired = [(owner, string $ field "id" spec, spec) | (owner, workspace) <- pairs (field "workspaces" message), spec <- array (field "tabs" workspace)]
      finish = do
        current <- readIORef (tabs browser)
        let live = [owner ++ "/" ++ identity | (owner, identity, _) <- desired]
        forM_ (M.toList current) $ \(key, tab) -> do
          let keep = key `elem` live
              showTab = keep && boolean (field "visible" message) && key == string (field "owner" message) ++ "/" ++ string (field "active" message)
          focused <- readIORef (tabFocused tab)
          when (focused && not showTab) $ callInt (controller $ shell browser) 12 0
          if keep then visible (tabView tab) showTab else do
            writeIORef (tabAlive tab) False
            closeView (tabView tab)
            modifyIORef' (tabs browser) (M.delete key)
        positionGuest browser
        currentVersion <- readIORef (revision browser)
        if currentVersion /= version then applyLatest browser else writeIORef (syncing browser) False
      apply [] = finish
      apply ((owner, identity, spec):remaining) = do
        dead <- readIORef (disposed browser)
        unless dead $ do
          current <- readIORef (tabs browser)
          let use tab = applySpec browser tab spec >> apply remaining
          maybe (createTab browser owner identity use) (\tab -> applyTheme browser tab >> use tab) (M.lookup (owner ++ "/" ++ identity) current)
  apply desired `catch` \(errorValue :: SomeException) -> writeIORef (syncing browser) False >> windowTitle (window browser) ("Sapiens4 — " ++ displayException errorValue)

applySpec :: Browser -> Tab -> Value -> IO ()
applySpec browser tab spec = do
  when (field "zoom" spec /= Null) $ zoom (tabView tab) $ number $ field "zoom" spec
  command <- case field "command" spec of
    Null -> pure Nothing
    value -> case fromJSON value of
      Success parsed -> pure (Just parsed)
      Error reason -> ioError (userError reason)
  oldSequence <- readIORef (tabSequence tab)
  case browserTransition (T.pack oldSequence) command of
    BrowserUnchanged -> pure ()
    BrowserExecute instruction -> do
      writeIORef (tabSequence tab) (T.unpack (browserSequence instruction))
      let web = core $ tabView tab
      case browserAction instruction of
        Navigate -> do
          base <- readIORef (origin $ host browser)
          let url = string $ field "url" spec
          if Navigation.guest base url then navigate (tabView tab) url
            else writeIORef (tabError tab) "Unsupported destination" >> observe browser tab
        Reload -> call0 web 31
        Back -> (getValue web 38 :: IO Int32) >>= \allowed -> if allowed /= 0 then call0 web 40 else observe browser tab
        Forward -> (getValue web 39 :: IO Int32) >>= \allowed -> if allowed /= 0 then call0 web 41 else observe browser tab

positionGuest :: Browser -> IO ()
positionGuest browser = do
  message <- readIORef (latest browser)
  current <- readIORef (tabs browser)
  dead <- readIORef (disposed browser)
  let active = string (field "owner" message) ++ "/" ++ string (field "active" message)
      shown = boolean $ field "visible" message
  unless dead $ do
    -- Menus must hide native panes even while tab creation is pending or failed.
    forM_ (M.toList current) $ \(key, tab) -> unless (shown && key == active) $
      void $ showWindow (container $ tabView tab) 0
    when shown $ forM_ (M.lookup active current) $ \tab -> do
      dpi <- windowDpi (window browser)
      shellZoom <- getValue (controller $ shell browser) 7 :: IO Double
      top <- readIORef (shellTop browser)
      let scale = fromIntegral dpi / 96 * shellZoom
          coordinate name = truncate $ max 0 $ min 20000 $ number (field name $ field "rect" message) * scale
      bounds (tabView tab) (coordinate "x") (top + coordinate "y") (coordinate "width") (coordinate "height")
      visible (tabView tab) True

closeBrowser :: Browser -> IO ()
closeBrowser browser = do
  writeIORef (disposed browser) True
  current <- atomicModifyIORef' (tabs browser) (\value -> (M.empty, value))
  forM_ current $ \tab -> writeIORef (tabAlive tab) False >> closeView (tabView tab)
