-- | Inspired by, and borrowing from the Router contribution.
-- | https://github.com/reflex-frp/reflex-dom-contrib/blob/master/src/Reflex/Dom/Contrib/Router.hs

{-# LANGUAGE
    CPP
  , ConstraintKinds
  , FlexibleContexts
  , JavaScriptFFI
  , RankNTypes
  , RecursiveDo
  , ScopedTypeVariables
  , TypeFamilies
#-}

module Support.Legos.Router
  ( module Support.Legos.Router.AppState
  , cleanHref
  , routePath
  , goForward
  , goBack ) where

import           CGX.Prelude            hiding (on)
import           Data.Monoid                   ((<>))
import           Data.String                   (String)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           GHC.Err                       ( error )
import           GHCJS.DOM.Types               (Location(..))
import           Reflex.Dom.Core               hiding (EventName, Window, (.~))
import           GHCJS.DOM.Types               (MonadJSM)
import           GHCJS.DOM.History             (History, back, forward, pushState, replaceState)
import           GHCJS.DOM                     (currentWindow)
import           GHCJS.DOM.EventM              (on)
import           GHCJS.DOM.EventTarget         (dispatchEvent_)
import           GHCJS.DOM.Location            (getHref)
import           GHCJS.DOM.PopStateEvent
import           GHCJS.DOM.Window              (getHistory, getLocation)
import           GHCJS.DOM.WindowEventHandlers (popState)
import           GHCJS.Marshal.Pure            (pFromJSVal)
import qualified Language.Javascript.JSaddle   as JS
import qualified Text.URI                      as U
import           Reflex.Dom                    (holdDyn)

import Support.Legos.Router.AppState

-- Route a single page app according to the part of the path after pathBase
routePath :: (MonadWidget t m, AppState a)
  => Text            -- The root path. (don't include leading and trailing '/')
  -> Event t a       -- Event holding the desired route and state
  -> m (Dynamic t a) -- Path segments used for routing state
routePath pathBase evPathUpdate = do
  dyS <- holdDyn defaultState evPathUpdate
  let evPathUpdate' = toUrl <$> evPathUpdate
  url <- route' defaultState dyS pathBase evPathUpdate'
  return $ fromUrl <$> url

-- | Remove all leading forward-slashes in the given text
clean :: T.Text -> T.Text
clean = T.dropWhile (=='/')

-- | Create a popState event and attach it to the current window
dispatchEvent' :: JS.JSM ()
dispatchEvent' = do
  Just window <- currentWindow
  obj@(JS.Object o) <- JS.create
  JS.objSetPropertyByName obj ("cancelable" :: T.Text) True
  JS.objSetPropertyByName obj ("bubbles" :: T.Text) True
  JS.objSetPropertyByName obj ("view" :: T.Text) window
  event <- newPopStateEvent ("popstate" :: T.Text) $ Just $ pFromJSVal o
  dispatchEvent_ window event

-- | Get the 'GHCJS.DOM.Location.Location' of the current window (unsafe)
getLoc :: (HasJSContext m, MonadJSM m) => m Location
getLoc = do
  Just win <- currentWindow
  getLocation win

-- | Get the popState event of the current window
getPopState :: (MonadWidget t m) => m (Event t U.URI)
getPopState = do
  Just window <- currentWindow
  wrapDomEventMaybe window (`on` popState) $ do
    loc <- getLocation window
    locStr <- getHref loc
    return . U.parseURI $ locStr

-- | Convert the current URL into a URI, if possible
getURI :: (HasJSContext m, MonadJSM m) => m U.URI
getURI = do
  l <- getUrlText
  return . fromMaybe (error "No parse of window location") . U.parseURI . T.unpack $ l

-- | Get the URL text of the current window (unsafe)
getUrlText :: (HasJSContext m, MonadJSM m) => m T.Text
getUrlText = getLoc >>= getHref

-- | Navigate to the previous page in the window's history
goBack :: (HasJSContext m, MonadJSM m) => m ()
goBack = withHistory back

-- | Navigate to the next page in the window's history
goForward :: (HasJSContext m, MonadJSM m) => m ()
goForward = withHistory forward

-- | Extract the parts of the given URI, if possible
parseParts :: T.Text -> U.URI -> [T.Text]
parseParts pathBase u =
  maybe (error $ pfxErr u pathBase)
        (T.splitOn "/" . clean) .
  T.stripPrefix (clean pathBase) $
  clean . T.pack $ U.uriPath u

-- | Generate an error message for an invalid URI
pfxErr :: U.URI -> T.Text -> String
pfxErr pn pathBase =
  T.unpack $ "Encountered path (" <> (T.pack . show $ pn) <> ") without expected prefix (" <> pathBase <> ")"

-- | Manipulate and track the URL 'GHCJS.DOM.Types.Location' for dynamic routing of a widget.
-- | These sources of URL-bar change will be reflected in the output URI:
-- |   - input events to 'route'
-- |   - browser forward/cack button clicks
-- |   - forward/back javascript calls (or 'goForward'/'goBack') Haskell calls
-- |   - any URL changes followed by a popState event (external calls to pushState that don't manually fire
-- |     a popState won't be detected)
route :: (HasJSContext m, MonadWidget t m)
  => a
  -> Dynamic t a
  -> Event t (U.URI, a)
  -> m (Dynamic t (U.URI, a))
route a dyS pushTo = do
  loc0 <- getURI

  _ <- performEvent $ ffor pushTo $ \(uri, _) -> do
    let newState = T.pack . show $ uri
    withHistory' $ \h c ->
      if newState == c
      then replaceState h (0 :: Double) ("" :: T.Text) (Just newState :: Maybe T.Text)
      else pushState h (0 :: Double) ("" :: T.Text) (Just newState :: Maybe T.Text)
    JS.liftJSM dispatchEvent'

  locUpdates' <- getPopState
  let locUpdates = attachPromptlyDynWith (\s e -> (e, s)) dyS locUpdates'
  holdDyn (loc0, a) locUpdates

-- | Manipulate and track the URL 'GHCJS.DOM.Types.Location' for dynamic routing of a widget.
-- | These sources of URL-bar change will be reflected in the output URI:
-- |   - input events to 'route'
-- |   - browser forward/cack button clicks
-- |   - forward/back javascript calls (or 'goForward'/'goBack') Haskell calls
-- |   - any URL changes followed by a popState event (external calls to pushState that don't manually fire
-- |     a popState won't be detected)
route' :: (MonadWidget t m)
  => a
  -> Dynamic t a
  -> T.Text
  -> Event t (T.Text, a)
  -> m (Dynamic t ([T.Text], a))
route' dflt dyS pathBase routeUpdate = do
  rec
    rUri <- route dflt dyS urlUpdates
    let urlUpdates = attachWith encode' (current rUri) routeUpdate
  return $ decode' <$> rUri
  where
    encode' (u, _) (u', s) = (updateUrl pathBase u u', s)
    decode' (u, s) = (parseParts pathBase u, s)

-- | Combine paths to build a full URL
toPath :: T.Text -> T.Text -> T.Text
toPath pathBase dynpath =
  case clean pathBase of
    "" -> "/" <> clean dynpath
    b -> "/" <> b <> "/" <> clean dynpath

-- | Update the path in the given URI
updateUrl :: T.Text -> U.URI -> T.Text -> U.URI
updateUrl pathBase u updateParts =
  U.URI (U.uriScheme u)
        (U.uriUserInfo u)
        (U.uriRegName u)
        (U.uriPort u)
        (T.unpack $ toPath pathBase updateParts)
        (U.uriQuery u)
        (U.uriFragment u)

-- | Provide the current window's history
withHistory :: (HasJSContext m, MonadJSM m) => (History -> m a) -> m a
withHistory act = do
  Just w <- currentWindow
  h <- getHistory w
  act h

-- | Provide the current window's history and current value
withHistory' :: (HasJSContext m, MonadJSM m) => (History -> Text -> m a) -> m a
withHistory' act = do
  Just w <- currentWindow
  h <- getHistory w
  loc <- getLocation w
  c <- getHref loc
  act h c

cleanHref :: MonadWidget t m
  => Event t a
  -> m ()
cleanHref e =
  performEvent_ $ ffor e $ \_ -> do
    url <- getUrlText
    let newState = T.takeWhile (/= '?') url
    if newState == url
    then return ()
    else do
      withHistory $ \h -> replaceState h (0 :: Double) ("" :: Text) (Just newState :: Maybe Text)
      JS.liftJSM dispatchEvent'
